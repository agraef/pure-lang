#include "interpreter.hh"
#include "util.hh"
#include <sstream>
#include <stdarg.h>
#include <errno.h>
#include <sys/types.h>
#ifndef __MINGW32__
#include <sys/wait.h>
#endif
#include <regex.h>
#include <fnmatch.h>
#include <glob.h>

#include <llvm/CallingConv.h>
#include <llvm/PassManager.h>
#include <llvm/Support/CallSite.h>
#include <llvm/System/DynamicLibrary.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#ifdef HAVE_LLVM_SUPPORT_RAW_OSTREAM_H
#include <llvm/Support/raw_ostream.h>
#endif

#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Target/TargetData.h>
#include <llvm/Bitcode/ReaderWriter.h>

#include "config.h"

#ifdef HAVE_GSL
#include <gsl/gsl_version.h>
#include <gsl/gsl_matrix.h>
#endif

uint8_t interpreter::g_verbose = 0;
bool interpreter::g_interactive = false;
interpreter* interpreter::g_interp = 0;
char *interpreter::baseptr = 0;
int interpreter::stackmax = 0;
int interpreter::stackdir = 0;
int interpreter::brkflag = 0;
int interpreter::brkmask = 0;
bool interpreter::g_init = false;

static void* resolve_external(const std::string& name)
{
  // This is just to give a little more informative error message before we
  // bail out anyway.
  cout.flush();
  cerr << "error trying to resolve external: " << name << '\n';
  assert(0);
  return 0;
}

/* Check the C stack direction (pilfered from the Chicken sources). A value >0
   indicates that the stack grows upward, towards higher addresses, <0 that
   the stack grows downward, towards lower addresses, and =0 that the
   direction is not known (this shouldn't happen, though).  */

static int c_stack_dir_tester(int counter, char *baseptr)
{
  if (counter < 100) {
    return c_stack_dir_tester(counter + 1, baseptr);
  } else {
    char tester;
    return &tester - baseptr;
  }
}

static int c_stack_dir()
{
  char basechar;
  int dir = c_stack_dir_tester(0, &basechar);
  return (dir>0)?1:(dir<0)?-1:0;
}

void interpreter::debug_init()
{
  cin.clear();
  tmp_breakpoints.clear();
  debug_info.clear();
  stoplevel = 0;
  debug_skip = false;
}

void interpreter::init()
{
  if (!g_interp) g_interp = this;
  if (!g_init) {
    stackdir = c_stack_dir();
    // Preload some auxiliary dlls. First load the Pure library if we built it.
#ifdef LIBPURE
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(LIBPURE, 0);
#endif
    // Additional stuff to be loaded on some systems (e.g., Windows).
#ifdef LIBGLOB
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(LIBGLOB, 0);
#endif
#ifdef LIBREGEX
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(LIBREGEX, 0);
#endif
    g_init = true;
  }

  fptr = 0;
  sstk_sz = 0; sstk_cap = 0x10000; // 64K
  sstk = (pure_expr**)malloc(sstk_cap*sizeof(pure_expr*));
  assert(sstk);

  // Initialize the JIT.

  using namespace llvm;

  /* Accommodate the major API breakage in recent LLVM versions. This is just
     horrible, maybe we should drop support for anything older than LLVM 2.6
     in the future. */
#if LLVM26
  init_llvm_target();
  module = new Module(modname, llvm::getGlobalContext());
#else
  module = new Module(modname);
#endif
  MP = new ExistingModuleProvider(module);
#if LLVM26
  string error;
  JIT = ExecutionEngine::create(MP, false, &error,
#if FAST_JIT
#warning "You selected FAST_JIT. This isn't recommended!"
				llvm::CodeGenOpt::None,
#else
				llvm::CodeGenOpt::Aggressive,
#endif
				// bool GVsWithCode is true by default which
				// breaks freeMachineCodeForFunction, so make
				// sure to set it to false here
				false);
  if (!JIT) {
    if (error.empty()) error = "The JIT could not be created.";
    std::cerr << "** Panic: " << error << " Giving up. **\n";
    exit(1);
  }
#else
#if FAST_JIT
  JIT = ExecutionEngine::create(MP, false, 0, true);
#else
  JIT = ExecutionEngine::create(MP);
#endif
#endif
  assert(JIT);
  FPM = new FunctionPassManager(MP);

  // Set up the optimizer pipeline. Start with registering info about how the
  // target lays out data structures.
  FPM->add(new TargetData(*JIT->getTargetData()));
  // Promote allocas to registers.
  FPM->add(createPromoteMemoryToRegisterPass());
  // Do simple "peephole" optimizations and bit-twiddling optimizations.
  FPM->add(createInstructionCombiningPass());
  // Reassociate expressions.
  FPM->add(createReassociatePass());
  // Eliminate common subexpressions.
  FPM->add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  FPM->add(createCFGSimplificationPass());

  // Install a fallback mechanism to resolve references to the runtime, on
  // systems which do not allow the program to dlopen itself.
  JIT->InstallLazyFunctionCreator(resolve_external);

  // Generic pointer type. LLVM doesn't like void*, so we use a pointer to a
  // dummy struct instead. (This is a bit of a kludge. We'd rather use char*,
  // as suggested in the LLVM documentation, but we need to keep char* and
  // void* apart.)
  {
    std::vector<const Type*> elts;
    Type *VoidTy = struct_type(elts);
    module->addTypeName("void", VoidTy);
    VoidPtrTy = PointerType::get(VoidTy, 0);
  }

  // Char pointer type.
  CharPtrTy = PointerType::get(int8_type(), 0);

  // int and double pointers.
  IntPtrTy = PointerType::get(int32_type(), 0);
  DoublePtrTy = PointerType::get(double_type(), 0);

  // Complex numbers (complex double).
  {
    std::vector<const Type*> elts;
    elts.push_back(ArrayType::get(double_type(), 2));
    //elts.push_back(double_type());
    //elts.push_back(double_type());
    ComplexTy = struct_type(elts);
    ComplexPtrTy = PointerType::get(ComplexTy, 0);
  }

  // GSL matrix types. These are used to marshall GSL matrices in the C
  // interface.
  {
    std::vector<const Type*> elts;
#if SIZEOF_SIZE_T==4
    elts.push_back(int32_type());	// size1
    elts.push_back(int32_type());	// size2
    elts.push_back(int32_type());	// tda
#else
    elts.push_back(int64_type());	// size1
    elts.push_back(int64_type());	// size2
    elts.push_back(int64_type());	// tda
#endif
    elts.push_back(VoidPtrTy);		// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(int32_type());	// owner
    GSLMatrixTy = struct_type(elts);
    module->addTypeName("struct.gsl_matrix", GSLMatrixTy);
    GSLMatrixPtrTy = PointerType::get(GSLMatrixTy, 0);
  }
  {
    std::vector<const Type*> elts;
#if SIZEOF_SIZE_T==4
    elts.push_back(int32_type());	// size1
    elts.push_back(int32_type());	// size2
    elts.push_back(int32_type());	// tda
#else
    elts.push_back(int64_type());	// size1
    elts.push_back(int64_type());	// size2
    elts.push_back(int64_type());	// tda
#endif
    elts.push_back(DoublePtrTy);	// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(int32_type());	// owner
    GSLDoubleMatrixTy = struct_type(elts);
    module->addTypeName("struct.gsl_matrix_double", GSLDoubleMatrixTy);
    GSLDoubleMatrixPtrTy = PointerType::get(GSLDoubleMatrixTy, 0);
  }
  {
    std::vector<const Type*> elts;
#if SIZEOF_SIZE_T==4
    elts.push_back(int32_type());	// size1
    elts.push_back(int32_type());	// size2
    elts.push_back(int32_type());	// tda
#else
    elts.push_back(int64_type());	// size1
    elts.push_back(int64_type());	// size2
    elts.push_back(int64_type());	// tda
#endif
    elts.push_back(ComplexPtrTy);	// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(int32_type());	// owner
    GSLComplexMatrixTy = struct_type(elts);
    module->addTypeName("struct.gsl_matrix_complex", GSLComplexMatrixTy);
    GSLComplexMatrixPtrTy = PointerType::get(GSLComplexMatrixTy, 0);
  }
  {
    std::vector<const Type*> elts;
#if SIZEOF_SIZE_T==4
    elts.push_back(int32_type());	// size1
    elts.push_back(int32_type());	// size2
    elts.push_back(int32_type());	// tda
#else
    elts.push_back(int64_type());	// size1
    elts.push_back(int64_type());	// size2
    elts.push_back(int64_type());	// tda
#endif
    elts.push_back(IntPtrTy);		// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(int32_type());	// owner
    GSLIntMatrixTy = struct_type(elts);
    module->addTypeName("struct.gsl_matrix_int", GSLIntMatrixTy);
    GSLIntMatrixPtrTy = PointerType::get(GSLIntMatrixTy, 0);
  }

  // Create the expr struct type.

  /* NOTE: This is in fact just a part of the prologue of the expression data
     structure as defined by the runtime. In order to perform certain
     operations like built-in arithmetic, conditionals and expression matching
     in an efficient manner, we need to know about the layout of the relevant
     fields in memory. The runtime may add additional variants and fields of
     its own. The declarations below correspond to the following C struct:

     struct expr {
       int32_t tag; // see expr.hh, determines the variant
       uint32_t refc; // reference counter
       union {
         struct { // application
	   struct expr *x, *y;
	 };
	 int32_t i; // integer
	 double d; // double
	 char *s; // string
	 void *p; // generic pointer
       };
     };

     The different variants are implemented by the LLVM structs expr
     (application), intexpr (integer), dblexpr (double), strexpr (string) and
     ptrexpr (pointer). The expr (application) struct is considered the basic
     expression type since this is what gets used most frequently. It is cast
     to the other types (using a bitcast on a pointer) as needed. */

  {
    PATypeHolder StructTy = opaque_type();
    std::vector<const Type*> elts;
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    elts.push_back(PointerType::get(StructTy, 0));
    elts.push_back(PointerType::get(StructTy, 0));
    ExprTy = struct_type(elts);
    cast<OpaqueType>(StructTy.get())->refineAbstractTypeTo(ExprTy);
    ExprTy = cast<StructType>(StructTy.get());
    module->addTypeName("struct.expr", ExprTy);
  }
  {
    std::vector<const Type*> elts;
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    IntExprTy = struct_type(elts);
    module->addTypeName("struct.intexpr", IntExprTy);
  }
  {
    std::vector<const Type*> elts;
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    elts.push_back(double_type());
    DblExprTy = struct_type(elts);
    module->addTypeName("struct.dblexpr", DblExprTy);
  }
  {
    std::vector<const Type*> elts;
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    elts.push_back(CharPtrTy);
    StrExprTy = struct_type(elts);
    module->addTypeName("struct.strexpr", StrExprTy);
  }
  {
    std::vector<const Type*> elts;
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    elts.push_back(VoidPtrTy);
    PtrExprTy = struct_type(elts);
    module->addTypeName("struct.ptrexpr", PtrExprTy);
  }

  // Corresponding pointer types.

  ExprPtrTy = PointerType::get(ExprTy, 0);
  ExprPtrPtrTy = PointerType::get(ExprPtrTy, 0);
  IntExprPtrTy = PointerType::get(IntExprTy, 0);
  DblExprPtrTy = PointerType::get(DblExprTy, 0);
  StrExprPtrTy = PointerType::get(StrExprTy, 0);
  PtrExprPtrTy = PointerType::get(PtrExprTy, 0);

  sstkvar = global_variable
    (module, ExprPtrPtrTy, false, GlobalVariable::InternalLinkage,
     ConstantPointerNull::get(ExprPtrPtrTy),
     "$$sstk$$");
  JIT->addGlobalMapping(sstkvar, &sstk);
  fptrvar = global_variable
    (module, VoidPtrTy, false, GlobalVariable::InternalLinkage,
     ConstantPointerNull::get(VoidPtrTy),
     "$$fptr$$");
  JIT->addGlobalMapping(fptrvar, &fptr);

  // Add prototypes for the runtime interface and enter the corresponding
  // function pointers into the runtime map.

  declare_extern((void*)malloc,
		 "malloc",          "void*",  1, "size_t");
  declare_extern((void*)free,
		 "free",            "void",   1, "void*");

  declare_extern((void*)pure_clos,
		 "pure_clos",       "expr*", -7, "bool", "int", "int", "int",
		                                 "void*", "void*", "int");
  declare_extern((void*)pure_locals,
		 "pure_locals",     "expr*", -1, "int");

  declare_extern((void*)pure_call,
		 "pure_call",       "expr*",  1, "expr*");
  declare_extern((void*)pure_force,
		 "pure_force",      "expr*",  1, "expr*");
  declare_extern((void*)pure_const,
		 "pure_const",      "expr*",  1, "int");
  declare_extern((void*)pure_int,
		 "pure_int",        "expr*",  1, "int");
  declare_extern((void*)pure_int64,
		 "pure_int64",      "expr*",  1, "int64");
  declare_extern((void*)pure_bigint,
		 "pure_bigint",     "expr*",  2, "int",
		 sizeof(mp_limb_t)==8?"int64*":"int*");
  declare_extern((void*)pure_double,
		 "pure_double",     "expr*",  1, "double");
  declare_extern((void*)pure_string_dup,
		 "pure_string_dup", "expr*",  1, "char*");
  declare_extern((void*)pure_cstring_dup,
		 "pure_cstring_dup","expr*",  1, "char*");
  declare_extern((void*)pure_pointer,
		 "pure_pointer",    "expr*",  1, "void*");
  declare_extern((void*)pure_apply,
		 "pure_apply",      "expr*",  2, "expr*", "expr*");
  declare_extern((void*)pure_applc,
		 "pure_applc",      "expr*",  2, "expr*", "expr*");

  declare_extern((void*)pure_matrix_rows,
		 "pure_matrix_rows", "expr*",    -1, "int");
  declare_extern((void*)pure_matrix_columns,
		 "pure_matrix_columns", "expr*", -1, "int");
  declare_extern((void*)pure_matrix_rowsq,
		 "pure_matrix_rowsq", "expr*",    -1, "int");
  declare_extern((void*)pure_matrix_columnsq,
		 "pure_matrix_columnsq", "expr*", -1, "int");
  declare_extern((void*)pure_symbolic_matrix,
		 "pure_symbolic_matrix", "expr*", 1, "void*");
  declare_extern((void*)pure_double_matrix,
		 "pure_double_matrix", "expr*",   1, "void*");
  declare_extern((void*)pure_complex_matrix,
		 "pure_complex_matrix", "expr*",  1, "void*");
  declare_extern((void*)pure_int_matrix,
		 "pure_int_matrix", "expr*",      1, "void*");

  declare_extern((void*)matrix_from_int_array_nodup,
		 "matrix_from_int_array_nodup", "expr*", 3, "int", "int",
		 "void*");
  declare_extern((void*)matrix_from_double_array_nodup,
		 "matrix_from_double_array_nodup", "expr*", 3, "int", "int",
		 "void*");
  declare_extern((void*)matrix_from_complex_array_nodup,
		 "matrix_from_complex_array_nodup", "expr*", 3, "int", "int",
		 "void*");

  declare_extern((void*)pure_listv,
		 "pure_listv",      "expr*",  2, "size_t", "expr**");
  declare_extern((void*)pure_listv2,
		 "pure_listv2",     "expr*",  3, "size_t", "expr**", "expr*");
  declare_extern((void*)pure_tuplev,
		 "pure_tuplev",     "expr*",  2, "size_t", "expr**");

  declare_extern((void*)pure_intlistv,
		 "pure_intlistv",   "expr*",  2, "size_t", "int*");
  declare_extern((void*)pure_intlistv2,
		 "pure_intlistv2",  "expr*",  3, "size_t", "int*", "expr*");
  declare_extern((void*)pure_inttuplev,
		 "pure_inttuplev",  "expr*",  2, "size_t", "int*");

  declare_extern((void*)pure_doublelistv,
		 "pure_doublelistv", "expr*", 2, "size_t", "double*");
  declare_extern((void*)pure_doublelistv2,
		 "pure_doublelistv2","expr*", 3, "size_t", "double*", "expr*");
  declare_extern((void*)pure_doubletuplev,
		 "pure_doubletuplev","expr*", 2, "size_t", "double*");

  declare_extern((void*)pure_bigintlistv,
		 "pure_bigintlistv", "expr*", 4, "size_t",
		 sizeof(mp_limb_t)==8?"int64*":"int*", "int*", "int*");
  declare_extern((void*)pure_bigintlistv2,
		 "pure_bigintlistv2","expr*", 5, "size_t",
		 sizeof(mp_limb_t)==8?"int64*":"int*", "int*", "int*",
		 "expr*");
  declare_extern((void*)pure_biginttuplev,
		 "pure_biginttuplev","expr*", 4, "size_t",
		 sizeof(mp_limb_t)==8?"int64*":"int*", "int*", "int*");
  declare_extern((void*)pure_bigintmatrixv,
		 "pure_bigintmatrixv","expr*", 5, "size_t", "size_t",
		 sizeof(mp_limb_t)==8?"int64*":"int*", "int*", "int*");

  declare_extern((void*)pure_strlistv,
		 "pure_strlistv",   "expr*",  3, "size_t", "char*", "int*");
  declare_extern((void*)pure_strlistv2,
		 "pure_strlistv2",  "expr*",  4, "size_t", "char*", "int*",
		 "expr*");
  declare_extern((void*)pure_strtuplev,
		 "pure_strtuplev",  "expr*",  3, "size_t", "char*", "int*");
  declare_extern((void*)pure_strmatrixv,
		 "pure_strmatrixv", "expr*",  4, "size_t", "size_t",
		 "char*", "int*");

  declare_extern((void*)pure_cmp_bigint,
		 "pure_cmp_bigint", "int",    3, "expr*", "int",
		 sizeof(mp_limb_t)==8?"int64*":"int*");
  declare_extern((void*)pure_cmp_string,
		 "pure_cmp_string", "int",    2, "expr*", "char*");

  declare_extern((void*)pure_get_cstring,
		 "pure_get_cstring", "char*", 1, "expr*");
  declare_extern((void*)pure_free_cstrings,
		 "pure_free_cstrings","void", 0);
  declare_extern((void*)pure_get_bigint,
		 "pure_get_bigint",  "void*", 1, "expr*");
  declare_extern((void*)pure_get_int64,
		 "pure_get_int64",   "int64", 1, "expr*");
  declare_extern((void*)pure_get_int,
		 "pure_get_int",     "int",   1, "expr*");
  declare_extern((void*)pure_get_matrix,
		 "pure_get_matrix",  "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_data,
		 "pure_get_matrix_data", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_data_byte,
		 "pure_get_matrix_data_byte", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_data_short,
		 "pure_get_matrix_data_short", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_data_int,
		 "pure_get_matrix_data_int", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_data_float,
		 "pure_get_matrix_data_float", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_data_double,
		 "pure_get_matrix_data_double", "void*", 1, "expr*");
  declare_extern((void*)pure_free_cvectors,
		 "pure_free_cvectors","void", 0);

  declare_extern((void*)pure_catch,
		 "pure_catch",      "expr*",  2, "expr*", "expr*");
  declare_extern((void*)pure_throw,
		 "pure_throw",      "void",   1, "expr*");
  declare_extern((void*)pure_sigfpe,
		 "pure_sigfpe",     "void",   0);

  declare_extern((void*)pure_new,
		 "pure_new",        "expr*",  1, "expr*");
  declare_extern((void*)pure_free,
		 "pure_free",       "void",   1, "expr*");
  declare_extern((void*)pure_freenew,
		 "pure_freenew",    "void",   1, "expr*");
  declare_extern((void*)pure_ref,
		 "pure_ref",        "void",   1, "expr*");
  declare_extern((void*)pure_unref,
		 "pure_unref",      "void",   1, "expr*");

  declare_extern((void*)pure_new_args,
		 "pure_new_args",   "void",  -1, "int");
  declare_extern((void*)pure_free_args,
		 "pure_free_args",  "void",  -2, "expr*", "int");

  declare_extern((void*)pure_push_args,
		 "pure_push_args",  "int",   -2, "int", "int");
  declare_extern((void*)pure_pop_args,
		 "pure_pop_args",   "void",   3, "expr*", "int", "int");
  declare_extern((void*)pure_pop_tail_args,
		 "pure_pop_tail_args","void", 3, "expr*", "int", "int");

  declare_extern((void*)pure_push_arg,
		 "pure_push_arg",  "void",    1, "expr*");
  declare_extern((void*)pure_pop_arg,
		 "pure_pop_arg",   "void",    1, "expr*");
  declare_extern((void*)pure_pop_tail_arg,
		 "pure_pop_tail_arg", "void", 1, "expr*");

  declare_extern((void*)pure_debug,
		 "pure_debug",      "void",  -2, "int", "char*");
  declare_extern((void*)pure_debug_rule,
		 "pure_debug_rule", "void",   2, "void*", "void*");
  declare_extern((void*)pure_debug_redn,
		 "pure_debug_redn", "void",   3, "void*", "void*", "expr*");

  declare_extern((void*)pure_interp_main,
		 "pure_interp_main","void*",  10, "int", "void*",
		 "int", "char*", "void*", "void*", "int*", "void*",
		 "void*", "void*");
}

interpreter::interpreter()
  : verbose(0), compiling(false), interactive(false), debugging(false),
    pic(false), strip(false), restricted(false), ttymode(false),
    override(false), stats(false), temp(0), ps("> "), libdir(""),
    histfile("/.pure_history"), modname("pure"),
    nerrs(0), modno(-1), modctr(0), source_s(0), output(0),
    result(0), lastres(0), mem(0), exps(0), tmps(0), module(0), JIT(0), FPM(0),
    sstk(__sstk), stoplevel(0), debug_skip(false), fptr(__fptr)
{
  init();
}

interpreter::interpreter(int32_t nsyms, char *syms,
			 pure_expr ***vars, void **vals,
			 int32_t *arities, void **externs,
			 pure_expr ***_sstk, void **_fptr)
  : verbose(0), compiling(false), interactive(false), debugging(false),
    pic(false), strip(false), restricted(true), ttymode(false), override(false),
    stats(false), temp(0), ps("> "), libdir(""), histfile("/.pure_history"),
    modname("pure"), nerrs(0), modno(-1), modctr(0), source_s(0), output(0),
    result(0), lastres(0), mem(0), exps(0), tmps(0), module(0), JIT(0), FPM(0),
    sstk(*_sstk), stoplevel(0), debug_skip(false), fptr(*(Env**)_fptr)
{
  using namespace llvm;
  init();
  string s_syms = syms, s_externs;
  size_t p = s_syms.find("%%\n");
  if (p != string::npos) {
    s_externs = s_syms.substr(p+3);
    s_syms.erase(p);
  }
  symtab.restore(s_syms.c_str());
  // Populate the externs table (function pointers are filled in later).
  istringstream sin(s_externs);
  int f;
  string s_name, s_type;
  size_t n_args;
  while (1) {
    sin >> f >> s_name >> s_type >> n_args;
    if (sin.fail()) break;
    const Type* rettype = named_type(s_type);
    vector<const Type*> argtypes(n_args);
    for (size_t i = 0; i < n_args; i++) {
      sin >> s_type;
      argtypes[i] = named_type(s_type);
    }
    if (sin.fail() || sin.eof()) break;
    externals[f] = ExternInfo(f, s_name, rettype, argtypes, 0);
  }
  for (int32_t f = 1; f <= nsyms; f++) {
    symbol& sym = symtab.sym(f);
    size_t p = sym.s.rfind("::");
    if (p != string::npos && p > 0)
      namespaces.insert(sym.s.substr(0, p));
    pure_expr *x;
    if (!vars[f]) continue;
    if (vals[f])
      x = pure_clos(false, f, 0, arities[f], vals[f], 0, 0);
    else
      x = pure_const(f);
    GlobalVariable *u = 0;
    map<int32_t,GlobalVar>::iterator it = globalvars.find(f);
    if (it != globalvars.end()) {
      GlobalVar& v = it->second;
      u = v.v;
      globalvars.erase(it);
    }
    globalvars.insert(pair<int32_t,GlobalVar>(f, GlobalVar(vars[f])));
    it = globalvars.find(f);
    assert(it != globalvars.end());
    GlobalVar& v = it->second;
    v.v = u;
    if (!v.v) {
      v.v = global_variable
	(module, ExprPtrTy, false, GlobalVariable::InternalLinkage,
	 ConstantPointerNull::get(ExprPtrTy),
	 mkvarlabel(f));
      JIT->addGlobalMapping(v.v, &v.x);
    }
    if (v.x) pure_free(v.x); v.x = pure_new(x);
    if (externs[f]) {
      ExternInfo& info = externals[f];
      vector<const Type*> argt(info.argtypes.size(), ExprPtrTy);
      FunctionType *ft = FunctionType::get(ExprPtrTy, argt, false);
      Function *fp = Function::Create(ft, Function::InternalLinkage,
				      "$$wrap."+info.name, module);
      sys::DynamicLibrary::AddSymbol("$$wrap."+info.name, externs[f]);
      info.f = fp;
    }
  }
}

interpreter::~interpreter()
{
  // get rid of global environments and the LLVM data
  globenv.clear(); macenv.clear();
  globalfuns.clear(); globalvars.clear();
  // free the shadow stack
  free(sstk);
  // free expression memory
  pure_mem *m = mem, *n;
  while (m) {
    n = m->next;
    delete m;
    m = n;
  }
  // free the execution engine and the pass manager
#if 1
  // If this segfaults then you're probably running an older LLVM version. Get
  // LLVM 2.4 or later, or disable this line.
  if (JIT) delete JIT;
#endif
  if (FPM) delete FPM;
  // if this was the global interpreter, reset it now
  if (g_interp == this) g_interp = 0;
}

static inline void
cdf(interpreter& interp, const char* s, pure_expr *x)
{
  try {
    interp.const_defn(s, x);
  } catch (err &e) {
    std::cerr << "warning: " << e.what() << '\n';
  }
  pure_freenew(x);
}

void interpreter::init_sys_vars(const string& version,
				const string& host,
				const list<string>& argv)
{
  interpreter* s_interp = g_interp;
  g_interp = this;
  // command line arguments, system and version information
  pure_expr *args = pure_const(symtab.nil_sym().f);
  for (list<string>::const_reverse_iterator it = argv.rbegin();
       it != argv.rend(); it++) {
    pure_expr *f = pure_const(symtab.cons_sym().f);
    pure_expr *x = pure_cstring_dup(it->c_str());
    args = pure_apply(pure_new(pure_apply(pure_new(f), pure_new(x))),
		      pure_new(args));
  }
  defn("argc",		pure_int(argv.size()));
  defn("argv",		args);
  defn("compiling",	pure_int(compiling));
  defn("version",	pure_cstring_dup(version.c_str()));
  defn("sysinfo",	pure_cstring_dup(host.c_str()));
#ifdef HAVE_GSL
  defn("gsl_version",	pure_cstring_dup(gsl_version));
#endif
  // memory sizes
  interpreter& interp = *this;
  cdf(interp, "SIZEOF_BYTE",	pure_int(1));
  cdf(interp, "SIZEOF_SHORT",	pure_int(sizeof(short)));
  cdf(interp, "SIZEOF_INT",	pure_int(sizeof(int)));
  cdf(interp, "SIZEOF_LONG",	pure_int(sizeof(long)));
  cdf(interp, "SIZEOF_LONG_LONG",	pure_int(sizeof(long long)));
  cdf(interp, "SIZEOF_SIZE_T",	pure_int(sizeof(size_t)));
  cdf(interp, "SIZEOF_FLOAT",	pure_int(sizeof(float)));
  cdf(interp, "SIZEOF_DOUBLE",	pure_int(sizeof(double)));
#ifdef HAVE__COMPLEX_FLOAT
  cdf(interp, "SIZEOF_COMPLEX_FLOAT",	pure_int(sizeof(_Complex float)));
#endif
#ifdef HAVE__COMPLEX_DOUBLE
  cdf(interp, "SIZEOF_COMPLEX_DOUBLE",	pure_int(sizeof(_Complex double)));
#endif
  cdf(interp, "SIZEOF_POINTER",	pure_int(sizeof(void*)));
  g_interp = s_interp;
}

// Errors and warnings.

void
interpreter::error(const yy::location& l, const string& m)
{
  string m1 = m;
  if (m.find("bad token") != string::npos)
    m1 = "bad anonymous function or pointer value";
  nerrs++;
  if (source_s) {
    ostringstream msg;
    msg << *l.begin.filename << ", line " << l.begin.line
	<< ": " << m1 << '\n';
    errmsg += msg.str();
  } else {
    cout.flush();
    cerr << *l.begin.filename << ", line " << l.begin.line
	 << ": " << m1 << '\n';
  }
}

void
interpreter::error(const string& m)
{
  nerrs++;
  if (source_s) {
    ostringstream msg;
    msg << m << '\n';
    errmsg += msg.str();
  } else {
    cout.flush();
    cerr << m << '\n';
  }
}

void
interpreter::warning(const yy::location& l, const string& m)
{
  if (!source_s) {
    cout.flush();
    cerr << *l.begin.filename << ", line " << l.begin.line
	 << ": " << m << '\n';
  }
}

void
interpreter::warning(const string& m)
{
  if (!source_s) {
    cout.flush();
    cerr << m << '\n';
  }
}

/* Search for a source file. Absolute file names (starting with a slash) are
   taken as is. Relative pathnames are resolved using the following algorithm:
   If srcdir is nonempty, search it first, then libdir (if nonempty), then the
   current working directory. If srcdir is empty, first search the current
   directory, then libdir (if nonempty). In either case, if the resulting
   absolute pathname is a symbolic link, the destination is used instead, and
   finally the pathname is canonicalized. */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

static inline string dirname(const string& fname)
{
  size_t pos = fname.rfind('/');
  if (pos == string::npos)
    return "";
  else
    return fname.substr(0, pos+1);
}

static inline string basename(const string& fname)
{
  size_t pos = fname.rfind('/');
  if (pos == string::npos)
    return fname;
  else
    return fname.substr(pos+1);
}

static inline bool chkfile(const string& s)
{
  struct stat st;
  return !stat(s.c_str(), &st) && !S_ISDIR(st.st_mode);
}

#ifndef _WIN32
static inline bool chklink(const string& s)
{
  struct stat st;
  return !lstat(s.c_str(), &st) && S_ISLNK(st.st_mode);
}
#endif

static string unixize(const string& s)
{
  string t = s;
#ifdef _WIN32
  for (size_t i = 0, n = t.size(); i<n; i++)
    if (t[i] == '\\')
      t[i] = '/';
#endif
  return t;
}

static bool absname(const string& s)
{
  if (s.empty())
    return false;
  else
#ifdef _WIN32
    return s[0]=='/' || (s.size() >= 2 && s[1] == ':');
#else
    return s[0]=='/';
#endif
}

#define BUFSIZE 1024

static string searchdir(const string& srcdir, const string& libdir,
			const list<string>& include_dirs,
			const string& script, bool search = true)
{
  char cwd[BUFSIZE];
  if (script.empty())
    return script;
  else if (!getcwd(cwd, BUFSIZE)) {
    perror("getcwd");
    return script;
  }
  string workdir = unixize(cwd);
  if (!workdir.empty() && workdir[workdir.size()-1] != '/')
    workdir += "/";
  string fname;
  if (!absname(script)) {
    // resolve relative pathname
    if (!search) {
      fname = workdir+script;
      if (chkfile(fname)) goto found;
      fname = script;
    } else {
      fname = (srcdir.empty()?workdir:srcdir)+script;
      if (chkfile(fname)) goto found;
      for (list<string>::const_iterator dir = include_dirs.begin(),
	     end = include_dirs.end(); dir != end; dir++)
	if (!dir->empty()) {
	  fname = *dir+script;
	  if (chkfile(fname)) goto found;
	}
      if (!libdir.empty()) {
	fname = libdir+script;
	if (chkfile(fname)) goto found;
      }
      fname = script;
    }
  } else
    fname = script;
 found:
  if (!absname(fname)) fname = workdir+fname;
  char buf[BUFSIZE];
#ifndef _WIN32
  if (chklink(fname)) {
    // follow symbolic link to its destination
    int l = readlink(fname.c_str(), buf, BUFSIZE-1);
    if (l >= 0) {
      buf[l] = 0;
      string basedir = dirname(fname), linkname = buf;
      string dir = dirname(linkname), name = basename(linkname);
      if (dir.empty())
	dir = basedir;
      else if (dir[0] != '/')
	dir = basedir+dir;
      fname = dir+name;
    } else
      perror("readlink");
  }
#endif
  // canonicalize the pathname
  string dir = dirname(fname), name = basename(fname);
  if (chdir(dir.c_str())==0 && getcwd(buf, BUFSIZE)) {
    string dir = unixize(buf);
    if (!dir.empty() && dir[dir.size()-1] != '/')
      dir += "/";
    fname = dir+name;
  }
  (void)chdir(cwd);
#if DEBUG>1
  std::cerr << "search '" << script << "', found as '" << fname << "'\n";
#endif
  return fname;
}

/* Library search. */

static string searchlib(const string& srcdir, const string& libdir,
			const list<string>& library_dirs,
			const string& lib, bool search = true)
{
  char cwd[BUFSIZE];
  if (lib.empty())
    return lib;
  else if (!getcwd(cwd, BUFSIZE)) {
    perror("getcwd");
    return lib;
  }
  string workdir = unixize(cwd);
  if (!workdir.empty() && workdir[workdir.size()-1] != '/')
    workdir += "/";
  string fname;
  if (!absname(lib)) {
    // resolve relative pathname
    if (!search) {
      fname = workdir+lib;
      if (chkfile(fname)) goto found;
      fname = lib;
    } else {
      fname = (srcdir.empty()?workdir:srcdir)+lib;
      if (chkfile(fname)) goto found;
      for (list<string>::const_iterator dir = library_dirs.begin(),
	     end = library_dirs.end(); dir != end; dir++)
	if (!dir->empty()) {
	  fname = *dir+lib;
	  if (chkfile(fname)) goto found;
	}
      if (!libdir.empty()) {
	fname = libdir+lib;
	if (chkfile(fname)) goto found;
      }
      fname = lib;
    }
  } else
    fname = lib;
 found:
#if DEBUG>1
  std::cerr << "search '" << lib << "', found as '" << fname << "'\n";
#endif
  return fname;
}

// Run the interpreter on a source file, collection of source files, or on
// string data.

#ifndef DLLEXT
#define DLLEXT ".so"
#endif

pure_expr* interpreter::run(const string &_s, bool check, bool sticky)
{
  string s = unixize(_s);
  // check for library modules
  size_t p = s.find(":");
  if (p != string::npos && s.substr(0, p) == "lib") {
    if (p+1 >= s.size()) throw err("empty lib name");
    string msg, name = s.substr(p+1), dllname = name;
    // See whether we need to add the DLLEXT suffix.
    if (name.size() <= strlen(DLLEXT) ||
	name.substr(name.size()-strlen(DLLEXT)) != DLLEXT)
      dllname += DLLEXT;
    // First try the name with DLLEXT added.
    string aname = searchlib(srcdir, libdir, librarydirs, dllname);
    if (!llvm::sys::DynamicLibrary::LoadLibraryPermanently(aname.c_str(), &msg)) {
      loaded_libs.push_back(aname);
      return 0;
    }
    else if (dllname == name)
      throw err(msg);
    // Now try whether the system can resolve the library under the given name.
    aname = searchlib(srcdir, libdir, librarydirs, name);
    if (llvm::sys::DynamicLibrary::LoadLibraryPermanently(aname.c_str(), &msg))
      throw err(msg);
    loaded_libs.push_back(aname);
    return 0;
  }
  // ordinary source file
  string fname = searchdir(srcdir, libdir, includedirs, s, check);
  if (check && sources.find(fname) != sources.end())
    // already loaded, skip
    return 0;
  /* Check that the file exists. We already do that here so that errors are
     properly reported to eval/evalcmd. */
  if (!s.empty()) {
    FILE *fp;
    if ((fp = fopen(fname.c_str(), "r")))
      fclose(fp);
    else
      throw err(s+": "+strerror(errno));
  }
  // save local data
  bool l_interactive = interactive;
  string l_source = source;
  int l_nerrs = nerrs;
  uint32_t l_temp = temp;
  const char *l_source_s = source_s;
  ostream *l_output = output;
  string l_srcdir = srcdir;
  int32_t l_modno = modno;
  string *l_current_namespace = symtab.current_namespace;
  set<string> *l_search_namespaces = symtab.search_namespaces;
  // save global data
  uint8_t s_verbose = g_verbose;
  bool s_interactive = g_interactive;
  interpreter* s_interp = g_interp;
  g_verbose = verbose;
  g_interactive = interactive = interactive && s.empty();
  g_interp = this;
  // initialize
  nerrs = 0;
  source = s; declare_op = false;
  source_s = 0;
  output = 0;
  srcdir = dirname(fname);
  if (sticky)
    ; // keep the current module
  else {
    modno = modctr++;
    symtab.current_namespace = new string;
    symtab.search_namespaces = new set<string>;
  }
  errmsg.clear();
  if (check && !interactive) temp = 0;
  bool ok = lex_begin(fname);
  if (ok) {
    if (temp == 0 && !s.empty()) sources.insert(fname);
    yy::parser parser(*this);
    parser.set_debug_level((verbose&verbosity::parser) != 0);
    // parse
    if (result) pure_free(result); result = 0;
    last.clear();
    parser.parse();
    last.clear();
    // finalize
    lex_end();
  }
  // restore global data
  g_verbose = s_verbose;
  g_interactive = s_interactive;
  g_interp = s_interp;
  // restore local data
  interactive = l_interactive;
  source = l_source;
  nerrs = l_nerrs;
  temp = l_temp;
  source_s = l_source_s;
  output = l_output;
  srcdir = l_srcdir;
  modno = l_modno;
  if (!sticky) {
    delete symtab.current_namespace;
    delete symtab.search_namespaces;
    symtab.current_namespace = l_current_namespace;
    symtab.search_namespaces = l_search_namespaces;
  }
  // return last computed result, if any
  return result;
}

pure_expr* interpreter::run(const list<string> &sl, bool check, bool sticky)
{
  uint8_t s_verbose = verbose;
  // Temporarily suppress verbose output for using clause.
  if (verbose) {
    compile();
    verbose = 0;
  }
  for (list<string>::const_iterator s = sl.begin(); s != sl.end(); s++)
    run(*s, check, sticky);
  if (s_verbose) {
    compile();
    verbose = s_verbose;
  }
  return result;
}

pure_expr *interpreter::runstr(const string& s)
{
  // save local data
  bool l_compiling = compiling;
  bool l_interactive = interactive;
  string l_source = source;
  int l_nerrs = nerrs;
  const char *l_source_s = source_s;
  string l_srcdir = srcdir;
  int32_t l_modno = modno;
  string *l_current_namespace = symtab.current_namespace;
  set<string> *l_search_namespaces = symtab.search_namespaces;
  // save global data
  uint8_t s_verbose = g_verbose;
  bool s_interactive = g_interactive;
  interpreter* s_interp = g_interp;
  g_verbose = 0;
  g_interactive = interactive = false;
  g_interp = this;
  // initialize
  nerrs = 0;
  source = ""; declare_op = false;
  source_s = s.c_str();
  srcdir = "";
  modno = modctr++;
  symtab.current_namespace = new string;
  symtab.search_namespaces = new set<string>;
  errmsg.clear();
  compiling = false;
  bool ok = lex_begin();
  if (ok) {
    yy::parser parser(*this);
    // parse
    if (result) pure_free(result); result = 0;
    parser.parse();
    // finalize
    lex_end();
  }
  // restore global data
  g_verbose = s_verbose;
  g_interactive = s_interactive;
  g_interp = s_interp;
  // restore local data
  compiling = l_compiling;
  interactive = l_interactive;
  source = l_source;
  source_s = 0;
  nerrs = l_nerrs;
  source_s = l_source_s;
  srcdir = l_srcdir;
  modno = l_modno;
  delete symtab.current_namespace;
  delete symtab.search_namespaces;
  symtab.current_namespace = l_current_namespace;
  symtab.search_namespaces = l_search_namespaces;
  // return last computed result, if any
  return result;
}

// Evaluate an expression.

pure_expr *interpreter::eval(expr& x, bool keep)
{
  globals g;
  save_globals(g);
  pure_expr *e, *res = eval(x, e, keep);
  if (!res && e) pure_free(e);
  restore_globals(g);
  return res;
}

pure_expr *interpreter::eval(expr& x, pure_expr*& e, bool keep)
{
  globals g;
  save_globals(g);
  compile();
  // promote type tags and substitute macros and constants:
  env vars; expr u = csubst(macsubst(subst(vars, x)));
  compile(u);
  x = u;
  pure_expr *res = doeval(u, e, keep);
  restore_globals(g);
  return res;
}

// Define global variables.

pure_expr *interpreter::defn(expr pat, expr& x)
{
  globals g;
  save_globals(g);
  pure_expr *e, *res = defn(pat, x, e);
  if (!res && e) pure_free(e);
  restore_globals(g);
  return res;
}

pure_expr *interpreter::defn(expr pat, expr& x, pure_expr*& e)
{
  globals g;
  save_globals(g);
  compile();
  env vars;
  // promote type tags and substitute macros and constants:
  qual = true;
  expr rhs = csubst(macsubst(subst(vars, x)));
  expr lhs = bind(vars, lcsubst(pat));
  build_env(vars, lhs);
  qual = false;
  for (env::const_iterator it = vars.begin(); it != vars.end(); ++it) {
    int32_t f = it->first;
    const symbol& sym = symtab.sym(f);
    env::const_iterator jt = globenv.find(f), kt = macenv.find(f);
    if (kt != macenv.end()) {
      restore_globals(g);
      throw err("symbol '"+sym.s+"' is already defined as a macro");
    } else if (jt != globenv.end() && jt->second.t == env_info::cvar) {
      restore_globals(g);
      throw err("symbol '"+sym.s+"' is already defined as a constant");
    } else if (jt != globenv.end() && jt->second.t == env_info::fun) {
      restore_globals(g);
      throw err("symbol '"+sym.s+"' is already defined as a function");
    } else if (externals.find(f) != externals.end()) {
      restore_globals(g);
      throw err("symbol '"+sym.s+
		"' is already declared as an extern function");
    }
  }
  compile(rhs);
  x = rhs;
  pure_expr *res = dodefn(vars, lhs, rhs, e, compiling);
  if (!res) return 0;
  for (env::const_iterator it = vars.begin(); it != vars.end(); ++it) {
    int32_t f = it->first;
    pure_expr **x = &globalvars[f].x;
    assert(*x);
    globenv[f] = env_info(x, temp);
  }
  restore_globals(g);
  return res;
}

// Define global constants.

pure_expr *interpreter::const_defn(expr pat, expr& x)
{
  globals g;
  save_globals(g);
  pure_expr *e, *res = const_defn(pat, x, e);
  if (!res && e) pure_free(e);
  restore_globals(g);
  return res;
}

static inline bool is_cons(interpreter& interp,
			   pure_expr *x, pure_expr*& y, pure_expr*& z)
{
  if (x->tag == EXPR::APP && x->data.x[0]->tag == EXPR::APP &&
      x->data.x[0]->data.x[0]->tag == interp.symtab.cons_sym().f) {
    y = x->data.x[0]->data.x[1];
    z = x->data.x[1];
    return true;
  } else
    return false;
}

static bool is_list2(interpreter& interp,
		     pure_expr *x, size_t& size,
		     pure_expr**& elems, pure_expr*& tl)
{
  assert(x);
  pure_expr *u = x, *y, *z;
  size = 0;
  while (is_cons(interp, u, y, z)) {
    size++;
    u = z;
  }
  if (size == 0) return false;
  tl = u;
  elems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  assert(elems);
  size_t i = 0;
  u = x;
  while (is_cons(interp, u, y, z)) {
    elems[i++] = y;
    u = z;
  }
  return true;
}

static inline bool is_pair(interpreter& interp,
			   pure_expr *x, pure_expr*& y, pure_expr*& z)
{
  if (x->tag == EXPR::APP && x->data.x[0]->tag == EXPR::APP &&
      x->data.x[0]->data.x[0]->tag == interp.symtab.pair_sym().f) {
    y = x->data.x[0]->data.x[1];
    z = x->data.x[1];
    return true;
  } else
    return false;
}

static bool is_tuple(interpreter& interp,
		     pure_expr *x, size_t& size,
		     pure_expr**& elems)
{
  assert(x);
  pure_expr *u = x, *y, *z;
  size = 0;
  while (is_pair(interp, u, y, z)) {
    size++;
    u = z;
  }
  if (size == 0) return false;
  size++;
  elems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  assert(elems);
  size_t i = 0;
  u = x;
  while (is_pair(interp, u, y, z)) {
    elems[i++] = y;
    u = z;
  }
  elems[i++] = u;
  return true;
}

expr interpreter::pure_expr_to_expr(pure_expr *x)
{
  char test;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax)
    throw err("expression too deep in constant definition");
  switch (x->tag) {
  case EXPR::APP: {
    size_t size;
    pure_expr **elems, *tl;
    if (is_list2(*this, x, size, elems, tl)) {
      /* Optimize the list case, so that we don't run out of stack space. */
      expr x = pure_expr_to_expr(tl);
      while (size > 0)
	x = expr::cons(pure_expr_to_expr(elems[--size]), x);
      free(elems);
      return x;
    } else if (is_tuple(*this, x, size, elems)) {
      /* Optimize the tuple case. */
      expr x = pure_expr_to_expr(elems[--size]);
      while (size > 0) {
	expr y = pure_expr_to_expr(elems[--size]);
	x = expr::pair(y, x);
      }
      free(elems);
      return x;
    } else
      return expr(pure_expr_to_expr(x->data.x[0]),
		  pure_expr_to_expr(x->data.x[1]));
  }
  case EXPR::INT:
    return expr(EXPR::INT, x->data.i);
  case EXPR::BIGINT: {
    // The expr constructor globbers its mpz_t argument, so take a copy.
    mpz_t z;
    mpz_init_set(z, x->data.z);
    return expr(EXPR::BIGINT, z);
  }
  case EXPR::DBL:
    return expr(EXPR::DBL, x->data.d);
  case EXPR::STR:
    return expr(EXPR::STR, strdup(x->data.s));
  case EXPR::PTR:
    if (x->data.p == 0)
      return expr(EXPR::PTR, x->data.p);
    else
      /* A non-null pointer isn't representable at compile time, so we wrap it
	 up in a global variable. */
      return wrap_expr(x);
  case EXPR::MATRIX: {
    if (x->data.mat.p) {
      gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
      exprll *xs = new exprll;
      for (size_t i = 0; i < m->size1; i++) {
	xs->push_back(exprl());
	exprl& ys = xs->back();
	for (size_t j = 0; j < m->size2; j++) {
	  ys.push_back(pure_expr_to_expr(m->data[i * m->tda + j]));
	}
      }
      return expr(EXPR::MATRIX, xs);
    } else
      return expr(EXPR::MATRIX, new exprll);
  }
  case EXPR::DMATRIX: {
#ifdef HAVE_GSL
    if (x->data.mat.p) {
      gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
      exprll *xs = new exprll;
      for (size_t i = 0; i < m->size1; i++) {
	xs->push_back(exprl());
	exprl& ys = xs->back();
	for (size_t j = 0; j < m->size2; j++) {
	  ys.push_back(expr(EXPR::DBL, m->data[i * m->tda + j]));
	}
      }
      return expr(EXPR::MATRIX, xs);
    } else
      return expr(EXPR::MATRIX, new exprll);
#else
    throw err("GSL matrices not supported in this implementation");
    return expr(EXPR::MATRIX, new exprll);
#endif
  }
  case EXPR::IMATRIX: {
#ifdef HAVE_GSL
    if (x->data.mat.p) {
      gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
      exprll *xs = new exprll;
      for (size_t i = 0; i < m->size1; i++) {
	xs->push_back(exprl());
	exprl& ys = xs->back();
	for (size_t j = 0; j < m->size2; j++) {
	  ys.push_back(expr(EXPR::INT, m->data[i * m->tda + j]));
	}
      }
      return expr(EXPR::MATRIX, xs);
    } else
      return expr(EXPR::MATRIX, new exprll);
#else
    throw err("GSL matrices not supported in this implementation");
    return expr(EXPR::MATRIX, new exprll);
#endif
  }
  case EXPR::CMATRIX: {
#ifdef HAVE_GSL
    if (x->data.mat.p) {
      gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
      exprll *xs = new exprll;
      symbol &rect = symtab.complex_rect_sym();
      expr f = rect.x;
      for (size_t i = 0; i < m->size1; i++) {
	xs->push_back(exprl());
	exprl& ys = xs->back();
	for (size_t j = 0; j < m->size2; j++) {
	  expr u = expr(EXPR::DBL, m->data[2*(i * m->tda + j)]);
	  expr v = expr(EXPR::DBL, m->data[2*(i * m->tda + j) + 1]);
	  ys.push_back(expr(f, u, v));
	}
      }
      return expr(EXPR::MATRIX, xs);
    } else
      return expr(EXPR::MATRIX, new exprll);
#else
    throw err("GSL matrices not supported in this implementation");
    return expr(EXPR::MATRIX, new exprll);
#endif
  }
  default:
    assert(x->tag >= 0);
    if (x->data.clos && x->data.clos->local)
      /* A local closure isn't representable at compile time, so we wrap it up
	 in a global variable. */
      return wrap_expr(x);
    else
      return expr(x->tag);
  }
}

static expr subterm(expr x, const path& p)
{
  for (size_t i = 0, n = p.len(); i < n; i++) {
    assert(x.tag() == EXPR::APP);
    x = p[i]?x.xval2():x.xval1();
  }
  return x;
}

pure_expr *interpreter::const_defn(expr pat, expr& x, pure_expr*& e)
{
  globals g;
  save_globals(g);
  compile();
  env vars;
  // promote type tags and substitute macros and constants:
  qual = true;
  expr rhs = csubst(macsubst(subst(vars, x)));
  expr lhs = bind(vars, lcsubst(pat));
  build_env(vars, lhs);
  qual = false;
  for (env::const_iterator it = vars.begin(); it != vars.end(); ++it) {
    int32_t f = it->first;
    const symbol& sym = symtab.sym(f);
    env::const_iterator jt = globenv.find(f), kt = macenv.find(f);
    if (kt != macenv.end()) {
      restore_globals(g);
      throw err("symbol '"+sym.s+"' is already defined as a macro");
    } else if (jt != globenv.end() && jt->second.t == env_info::cvar) {
      restore_globals(g);
      throw err("symbol '"+sym.s+"' is already defined as a constant");
    } else if (jt != globenv.end() && jt->second.t == env_info::fvar) {
      restore_globals(g);
      throw err("symbol '"+sym.s+"' is already defined as a variable");
    } else if (jt != globenv.end() && jt->second.t == env_info::fun) {
      restore_globals(g);
      throw err("symbol '"+sym.s+"' is already defined as a function");
    } else if (externals.find(f) != externals.end()) {
      restore_globals(g);
      throw err("symbol '"+sym.s+
		"' is already declared as an extern function");
    }
  }
  compile(rhs);
  x = rhs;
  // We *don't* want to keep the generated code here. This means that
  // batch-compiled programs shouldn't rely on any side-effects of the code
  // executed to compute a constant value.
  pure_expr *res = doeval(rhs, e, false);
  if (!res) return 0;
  // convert the result back to a compile time expression
  expr u = pure_expr_to_expr(res);
  // match against the left-hand side
  matcher m(rule(lhs, rhs));
  if (m.match(u)) {
    // bind variables accordingly
    for (env::const_iterator it = vars.begin(); it != vars.end(); ++it) {
      assert(it->second.t == env_info::lvar && it->second.p);
      int32_t f = it->first;
      expr v = subterm(u, *it->second.p);
      globenv[f] = env_info(v, temp);
    }
  } else {
    pure_freenew(res);
    res = 0;
  }
  restore_globals(g);
  return res;
}

void interpreter::const_defn(const char *varname, pure_expr *x)
{
  symbol& sym = symtab.checksym(varname);
  const_defn(sym.f, x);
}

void interpreter::const_defn(int32_t tag, pure_expr *x)
{
  assert(tag > 0 && x);
  globals g;
  save_globals(g);
  symbol& sym = symtab.sym(tag);
  env::const_iterator jt = globenv.find(tag), kt = macenv.find(tag);
  if (kt != macenv.end()) {
    restore_globals(g);
    throw err("symbol '"+sym.s+"' is already defined as a macro");
  } else if (jt != globenv.end() && jt->second.t == env_info::cvar) {
    restore_globals(g);
    throw err("symbol '"+sym.s+"' is already defined as a constant");
  } else if (jt != globenv.end() && jt->second.t == env_info::fvar) {
    restore_globals(g);
    throw err("symbol '"+sym.s+"' is already defined as a variable");
  } else if (jt != globenv.end() && jt->second.t == env_info::fun) {
    restore_globals(g);
    throw err("symbol '"+sym.s+"' is already defined as a function");
  } else if (externals.find(tag) != externals.end()) {
    restore_globals(g);
    throw err("symbol '"+sym.s+
	      "' is already declared as an extern function");
  }
  // convert the value to a compile time expression
  expr u = pure_expr_to_expr(x);
  // bind the variable
  globenv[tag] = env_info(u, temp);
  restore_globals(g);
}

// Process pending fundefs.

void interpreter::mark_dirty(int32_t f)
{
  env::iterator e = globenv.find(f);
  if (e != globenv.end()) {
    // mark this closure for recompilation
    env_info& info = e->second;
    if (info.m) {
      delete info.m; info.m = 0;
    }
    dirty.insert(f);
  }
}

#if DEBUG>1
void print_map(ostream& os, const Env *e)
{
  static size_t indent = 0;
  string blanks(indent, ' ');
  interpreter& interp = *interpreter::g_interp;
  os << blanks << ((e->tag>0)?interp.symtab.sym(e->tag).s:"anonymous")
     << " (" << (void*)e << ") {\n" << blanks << "XMAP:\n";
  list<VarInfo>::const_iterator xi;
  for (xi = e->xtab.begin(); xi != e->xtab.end(); xi++) {
    const VarInfo& x = *xi;
    assert(x.vtag > 0);
    os << blanks << "  " << interp.symtab.sym(x.vtag).s << " (#" << x.v << ") "
       << (uint32_t)x.idx << ":";
    const path &p = x.p;
    for (size_t i = 0; i < p.len(); i++) os << p[i];
    os << '\n';
  }
  for (size_t i = 0, n = e->fmap.m.size(); i < n; i++) {
    os << blanks << "FMAP #" << i << ":\n";
    indent += 2;
    EnvMap::const_iterator fi;
    for (fi = e->fmap.m[i]->begin(); fi != e->fmap.m[i]->end(); fi++) {
      const Env& e = *fi->second;
      print_map(os, &e);
    }
    indent -= 2;
  }
  os << blanks << "}\n";
}
#endif

void interpreter::compile()
{
  using namespace llvm;
  if (!dirty.empty()) {
    // there are some fundefs in the global environment waiting to be
    // recompiled, do it now
    for (funset::const_iterator f = dirty.begin(); f != dirty.end(); f++) {
      env::iterator e = globenv.find(*f);
      if (e != globenv.end()) {
	int32_t ftag = e->first;
	env_info& info = e->second;
	info.m = new matcher(*info.rules, info.argc+1);
	if (verbose&verbosity::code) std::cout << *info.m << '\n';
	// regenerate LLVM code (prolog)
	Env& f = globalfuns[ftag] = Env(ftag, info, false, false);
#if DEBUG>1
	print_map(std::cerr, &f);
#endif
	push("compile", &f);
	globalfuns[ftag].f = fun_prolog(symtab.sym(ftag).s);
	pop(&f);
      }
    }
    for (funset::const_iterator f = dirty.begin(); f != dirty.end(); f++) {
      env::iterator e = globenv.find(*f);
      if (e != globenv.end()) {
	int32_t ftag = e->first;
	env_info& info = e->second;
	// regenerate LLVM code (body)
	Env& f = globalfuns[ftag];
	push("compile", &f);
	fun_body(info.m);
	pop(&f);
	// compile to native code (always use the C-callable stub here)
	assert(!f.fp); f.fp = JIT->getPointerToFunction(f.h);
#if DEBUG>1
	std::cerr << "JIT " << f.f->getNameStr() << " -> " << f.fp << '\n';
#endif
	// do a direct call to the runtime to create the fbox and cache it in
	// a global variable
	pure_expr *fv = pure_clos(false, f.tag, f.getkey(), f.n, f.fp, 0, 0);
	GlobalVar& v = globalvars[f.tag];
	if (!v.v) {
	  v.v = global_variable
	    (module, ExprPtrTy, false, GlobalVariable::InternalLinkage,
	     ConstantPointerNull::get(ExprPtrTy),
	     mkvarlabel(f.tag));
	  JIT->addGlobalMapping(v.v, &v.x);
	}
	if (v.x) pure_free(v.x); v.x = pure_new(fv);
#if DEBUG>1
	std::cerr << "global " << &v.x << " (== "
		  << JIT->getPointerToGlobal(v.v) << ") -> "
		  << (void*)fv << '\n';
#endif
      }
    }
    dirty.clear();
    clear_cache();
  }
}

// Semantic routines.

// parse a toplevel function application, return arg count and head symbol

uint32_t count_args(expr x, int32_t& f)
{
  expr y, z;
  uint32_t count = 0;
  while (x.is_app(y, z)) ++count, x = y;
  f = x.tag();
  return count;
}

uint32_t count_args(expr x, expr& f)
{
  expr y, z;
  uint32_t count = 0;
  while (x.is_app(y, z)) ++count, x = y;
  f = x;
  return count;
}

// build a local variable environment from an already processed pattern

void interpreter::build_env(env& vars, expr x)
{
  assert(!x.is_null());
  if (x.astag() > 0) {
    const symbol& sym = symtab.sym(x.astag());
    if (sym.s != "_") vars[sym.f] = env_info(0, x.aspath());
  }
  switch (x.tag()) {
  case EXPR::VAR: {
    const symbol& sym = symtab.sym(x.vtag());
    if (sym.s != "_") vars[sym.f] = env_info(x.ttag(), x.vpath());
    break;
  }
  case EXPR::APP:
    build_env(vars, x.xval1());
    build_env(vars, x.xval2());
    break;
  default:
    break;
  }
}

void interpreter::compile(expr x)
{
  if (x.is_null()) return;
  switch (x.tag()) {
  case EXPR::MATRIX: {
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++)
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	compile(*ys);
      }
    break;
  }
  case EXPR::APP: {
    exprl xs;
    expr tl;
    /* Optimize the list and tuple cases so that we don't run out of stack
       space here. */
    if (x.is_list2(xs, tl)) {
      for (exprl::iterator it = xs.begin(), end = xs.end(); it != end; it++)
	compile(*it);
      compile(tl);
    } else if (x.is_tuple(xs)) {
      for (exprl::iterator it = xs.begin(), end = xs.end(); it != end; it++)
	compile(*it);
    } else {
      compile(x.xval1());
      compile(x.xval2());
    }
    break;
  }
  case EXPR::COND:
    compile(x.xval1());
    compile(x.xval2());
    compile(x.xval3());
    break;
  case EXPR::COND1:
    compile(x.xval1());
    compile(x.xval2());
    break;
  case EXPR::LAMBDA: {
    matcher *&m = x.pm();
    assert(m == 0);
    m = new matcher(rule(x.xval1(), x.xval2()));
    compile(x.xval2());
    break;
  }
  case EXPR::CASE: {
    compile(x.xval());
    matcher *&m = x.pm();
    assert(m == 0);
    for (rulel::iterator r = x.rules()->begin();
	 r != x.rules()->end(); r++) {
      compile(r->rhs); compile(r->qual);
    }
    m = new matcher(*x.rules());
    break;
  }
  case EXPR::WHEN: {
    compile(x.xval());
    matcher *&m = x.pm();
    assert(m == 0);
    rulel *rl = x.rules();
    m = new matcher[rl->size()];
    rulel::const_iterator r;
    size_t i;
    for (i = 0, r = rl->begin(); r != rl->end(); r++, i++) {
      compile(r->rhs);
      m[i].make(*r);
    }
    break;
  }
  case EXPR::WITH: {
    compile(x.xval());
    env *e = x.fenv();
    env::iterator p;
    for (p = e->begin(); p != e->end(); p++) {
      env_info& info = p->second;
      assert(info.m == 0);
      for (rulel::iterator r = info.rules->begin();
	   r != info.rules->end(); r++) {
	compile(r->rhs); compile(r->qual);
      }
      info.m = new matcher(*info.rules, info.argc+1);
    }
    break;
  }
  default:
    break;
  }
}

void interpreter::using_namespaces(list<string> *ids)
{
  symtab.search_namespaces->clear();
  if (ids) {
    for (list<string>::iterator it = ids->begin(), end = ids->end();
	 it != end; it++) {
      namespaces.insert(*it);
      symtab.search_namespaces->insert(*it);
    }
    delete ids;
  }
}

void interpreter::declare(bool priv, prec_t prec, fix_t fix, list<string> *ids)
{
  for (list<string>::const_iterator it = ids->begin();
       it != ids->end(); ++it) {
    if (it->find("::") != string::npos) {
      string id = *it;
      delete ids;
      throw err("qualified symbol '"+id+"' not permitted in declaration");
    }
    string id = make_qualid(*it), absid = make_absid(*it);
    symbol* sym = symtab.lookup(absid);
    if (sym) {
      // crosscheck declarations
      if (sym->priv != priv) {
	delete ids;
	throw err("symbol '"+id+"' already declared "+
		  (sym->priv?"'private'":"'public'"));
      } else if (sym->prec != prec || sym->fix != fix ||
		 (fix == outfix && sym->g==0)) {
	/* We explicitly permit 'nonfix' redeclarations here, to support
	   'const nonfix' symbols on the lhs of rules. Note that this
	   actually permits a 'nonfix' redeclaration of *any* symbol unless
	   it's already been declared as an operator, but this is actually
	   only useful with 'const' symbols. */
	if (fix == nonfix && sym->fix != outfix && sym->prec == PREC_MAX)
	  sym->fix = nonfix;
	else {
	  delete ids;
	  throw err("symbol '"+id+"' already declared with different fixity");
	}
      } else if (fix == outfix) {
	list<string>::const_iterator jt = ++it;
	if (jt == ids->end()) {
	  delete ids;
	  throw err("right symbol missing in outfix declaration");
	}
	string id2 = make_qualid(*it), absid2 = make_absid(*it);
	symbol* sym2 = symtab.lookup(absid2);
	if (!sym2 || sym->g != sym2->f) {
	  delete ids;
	  throw err("right outfix symbol '"+id2+"' doesn't match existing declaration");
	}
	it = jt;
      }
    } else if (fix == outfix) {
      // determine the matching right symbol
      list<string>::const_iterator jt = ++it;
      if (jt == ids->end()) {
	delete ids;
	throw err("right symbol missing in outfix declaration");
      }
      string id2 = make_qualid(*it), absid2 = make_absid(*it);
      symbol* sym2 = symtab.lookup(absid2);
      if (sym2) {
	delete ids;
	if (sym2->g != 0)
	  throw err("symbol '"+id2+"' already declared with different fixity");
	else
	  throw err("left outfix symbol '"+id+"' doesn't match existing declaration");
      }
      sym = symtab.sym(absid, prec, fix, priv);
      sym2 = symtab.sym(absid2, prec, fix, priv);
      assert(sym && sym2);
      sym->g = sym2->f;
      it = jt;
    } else
      symtab.sym(absid, prec, fix, priv);
  }
  delete ids;
}

void interpreter::exec(expr *x)
{
  last.clear();
  if (result) pure_free(result); result = 0;
  // Keep a copy of the original expression, so that we can give proper
  // diagnostics below.
  expr y = *x;
  pure_expr *e, *res = eval(*x, e, compiling);
  if ((verbose&verbosity::defs) != 0) cout << *x << ";\n";
  if (!res) {
    ostringstream msg;
    if (e) {
      msg << "unhandled exception '" << e << "' while evaluating '"
	  << y << "'";
      pure_free(e);
    } else
      msg << "unhandled exception while evaluating '" << y << "'";
    throw err(msg.str());
  }
  result = pure_new(res);
  delete x;
  if (interactive) {
    if (lastres) pure_free(lastres);
    lastres = pure_new(result);
    cout << result << '\n';
    if (stats)
      cout << ((double)clocks)/(double)CLOCKS_PER_SEC << "s\n";
  }
}

void interpreter::define(rule *r)
{
  last.clear();
  // Keep a copy of the original rule, so that we can give proper
  // diagnostics below.
  expr lhs = r->lhs, rhs = r->rhs;
  pure_expr *e, *res = defn(r->lhs, r->rhs, e);
  if ((verbose&verbosity::defs) != 0)
    cout << "let " << r->lhs << " = " << r->rhs << ";\n";
  if (!res) {
    ostringstream msg;
    if (e) {
      msg << "unhandled exception '" << e << "' while evaluating '"
	  << "let " << lhs << " = " << rhs << "'";
      pure_free(e);
    } else
      msg << "failed match while evaluating '"
	  << "let " << lhs << " = " << rhs << "'";
    throw err(msg.str());
  }
  delete r;
  pure_freenew(res);
  if (interactive && stats)
    cout << ((double)clocks)/(double)CLOCKS_PER_SEC << "s\n";
}

void interpreter::define_const(rule *r)
{
  last.clear();
  // Keep a copy of the original rule, so that we can give proper
  // diagnostics below.
  expr lhs = r->lhs, rhs = r->rhs;
  pure_expr *e, *res = const_defn(r->lhs, r->rhs, e);
  if ((verbose&verbosity::defs) != 0)
    cout << "const " << r->lhs << " = " << r->rhs << ";\n";
  if (!res) {
    ostringstream msg;
    if (e) {
      msg << "unhandled exception '" << e << "' while evaluating '"
	  << "const " << lhs << " = " << rhs << "'";
      pure_free(e);
    } else
      msg << "failed match while evaluating '"
	  << "const " << lhs << " = " << rhs << "'";
    throw err(msg.str());
  }
  delete r;
  pure_freenew(res);
  if (interactive && stats)
    cout << ((double)clocks)/(double)CLOCKS_PER_SEC << "s\n";
}

void interpreter::clearsym(int32_t f)
{
  // Check whether this symbol was already compiled; in that case
  // patch up the global variable table to replace it with a cbox.
  map<int32_t,GlobalVar>::iterator v = globalvars.find(f);
  if (v != globalvars.end()) {
    pure_expr *cv = pure_const(f);
    if (v->second.x) pure_free(v->second.x);
    v->second.x = pure_new(cv);
  }
  map<int32_t,Env>::iterator g = globalfuns.find(f);
  if (g != globalfuns.end()) {
    llvm::Function *f = g->second.f, *h = g->second.h;
    assert(f && h);
    globalfuns.erase(g);
    if (h != f) h->dropAllReferences();
    f->dropAllReferences();
    if (h != f) h->eraseFromParent();
    f->eraseFromParent();
  }
}

void interpreter::clear(int32_t f)
{
  if (f > 0) {
    env::iterator it = globenv.find(f);
    if (it != globenv.end()) {
      symbol& sym = symtab.sym(f);
      // get rid of temporary 'nonfix' designation of variable and constant
      // symbols
      if ((it->second.t == env_info::cvar || it->second.t == env_info::fvar) &&
	  sym.prec == PREC_MAX && sym.fix == nonfix)
	sym.fix = infix;
      globenv.erase(it);
      clearsym(f);
    }
  } else if (f == 0 && temp > 0) {
    // purge all temporary symbols
    for (env::iterator it = globenv.begin(); it != globenv.end(); ) {
      env::iterator jt = it; ++it;
      int32_t f = jt->first;
      env_info& info = jt->second;
      if (info.temp >= temp) {
	symbol& sym = symtab.sym(f);
	if ((info.t == env_info::cvar || info.t == env_info::fvar) &&
	    sym.prec == PREC_MAX && sym.fix == nonfix)
	  sym.fix = infix;
	globenv.erase(jt);
	clearsym(f);
      } else if (info.t == env_info::fun) {
	// purge temporary rules for non-temporary functions
	bool d = false;
	rulel& r = *info.rules;
	for (rulel::iterator it = r.begin(); it != r.end(); )
	  if (it->temp >= temp) {
	    d = true;
	    it = r.erase(it);
	  } else
	    ++it;
	if (d) {
	  assert(!r.empty());
	  mark_dirty(f);
	}
      }
    }
    for (env::iterator it = macenv.begin(); it != macenv.end(); ) {
      env::iterator jt = it; ++it;
      env_info& info = jt->second;
      if (info.temp >= temp)
	macenv.erase(jt);
      else {
	// purge temporary rules for non-temporary macros
	bool d = false;
	rulel& r = *info.rules;
	for (rulel::iterator it = r.begin(); it != r.end(); )
	  if (it->temp >= temp) {
	    d = true;
	    it = r.erase(it);
	  } else
	    ++it;
	if (d) {
	  assert(!r.empty());
	  if (info.m) {
	    delete info.m;
	    info.m = 0;
	  }
	}
      }
    }
    if (temp > 1) --temp;
  }
}

void interpreter::clear_mac(int32_t f)
{
  assert(f > 0);
  env::iterator it = macenv.find(f);
  if (it != macenv.end())
    macenv.erase(it);
}

void interpreter::clear_rules(int32_t f, uint32_t level)
{
  assert(f > 0);
  env::iterator it = globenv.find(f);
  if (it != globenv.end()) {
    env_info& e = it->second;
    rulel& r = *e.rules;
    bool d = false;
    for (rulel::iterator it = r.begin(); it != r.end(); )
      if (it->temp >= level) {
	d = true;
	it = r.erase(it);
      } else
	++it;
    if (d) {
      assert(!r.empty());
      mark_dirty(f);
    }
  }
}

void interpreter::clear_mac_rules(int32_t f, uint32_t level)
{
  assert(f > 0);
  env::iterator it = macenv.find(f);
  if (it != macenv.end()) {
    env_info& e = it->second;
    rulel& r = *e.rules;
    bool d = false;
    for (rulel::iterator it = r.begin(); it != r.end(); )
      if (it->temp >= level) {
	d = true;
	it = r.erase(it);
      } else
	++it;
    if (d) {
      assert(!r.empty());
      if (e.m) {
	delete e.m;
	e.m = 0;
      }
    }
  }
}

rulel *interpreter::default_lhs(exprl &l, rulel *rl)
{
  assert(!rl->empty());
  rule& r = rl->front();
  if (r.lhs.is_null()) {
    // empty lhs, repeat the ones from the previous rule
    assert(rl->size() == 1);
    if (l.empty()) {
      delete rl;
      throw err("error in rule (missing left-hand side)");
    } else {
      expr rhs = r.rhs, qual = r.qual;
      rl->clear();
      for (exprl::iterator i = l.begin(), end = l.end(); i != end; i++)
	rl->push_back(rule(*i, rhs, qual));
    }
  } else {
    l.clear();
    for (rulel::iterator i = rl->begin(), end = rl->end(); i != end; i++)
      l.push_back(i->lhs);
  }
  return rl;
}

void interpreter::add_rules(rulel &rl, rulel *r, bool b)
{
  for (rulel::iterator ri = r->begin(), end = r->end(); ri != end; ri++)
    add_rule(rl, *ri, b);
  delete r;
}

void interpreter::add_rules(env &e, rulel *r, bool toplevel)
{
  for (rulel::iterator ri = r->begin(), end = r->end(); ri != end; ri++)
    add_rule(e, *ri, toplevel);
  delete r;
}

void interpreter::add_rule(rulel &rl, rule &r, bool b)
{
  assert(!r.lhs.is_null());
  closure(r, b);
  rl.push_back(r);
}

void interpreter::add_rule(env &e, rule &r, bool toplevel)
{
  assert(!r.lhs.is_null());
  closure(r, false);
  if (toplevel) {
    // substitute macros and constants:
    expr u = expr(r.lhs),
      v = expr(csubst(macsubst(r.rhs))),
      w = expr(csubst(macsubst(r.qual)));
    r = rule(u, v, w);
    compile(r.rhs);
    compile(r.qual);
  }
  expr fx; uint32_t argc = count_args(r.lhs, fx);
  int32_t f = fx.tag();
  if (f <= 0)
    throw err("error in function definition (missing head symbol)");
  else if (!toplevel && (fx.flags()&EXPR::QUAL))
    throw err("error in local function definition (qualified head symbol)");
  env::iterator it = e.find(f);
  const symbol& sym = symtab.sym(f);
  if (it != e.end()) {
    if (it->second.t == env_info::cvar)
      throw err("symbol '"+sym.s+"' is already defined as a constant");
    else if (it->second.t == env_info::fvar)
      throw err("symbol '"+sym.s+"' is already defined as a variable");
    else if (it->second.argc != argc) {
      ostringstream msg;
      msg << "function '" << sym.s
	  << "' was previously defined with " << it->second.argc << " args";
      throw err(msg.str());
    }
  } else if (toplevel) {
    map<int32_t,ExternInfo>::const_iterator it = externals.find(f);
    if (it != externals.end() && it->second.argtypes.size() != argc) {
      ostringstream msg;
      msg << "function '" << sym.s
	  << "' was previously declared as an external with "
	  << it->second.argtypes.size() << " args";
      throw err(msg.str());
    }
  }
  env_info &info = e[f];
  if (info.t == env_info::none)
    info = env_info(argc, rulel(), toplevel?temp:0);
  assert(info.argc == argc);
  if (toplevel) {
    r.temp = temp;
    if (override) {
      rulel::iterator p = info.rules->begin();
      for (; p != info.rules->end() && p->temp >= temp; p++) ;
      info.rules->insert(p, r);
    } else
      info.rules->push_back(r);
  } else {
    r.temp = 0;
    info.rules->push_back(r);
  }
  if (toplevel && (verbose&verbosity::defs) != 0) cout << r << ";\n";
  if (toplevel) mark_dirty(f);
}

void interpreter::add_simple_rule(rulel &rl, rule *r)
{
  assert(!r->lhs.is_null());
  rl.push_back(*r);
  delete r;
}

void interpreter::add_macro_rule(rule *r)
{
  assert(!r->lhs.is_null() && r->qual.is_null() && !r->rhs.is_guarded());
  closure(*r, false);
  int32_t f; uint32_t argc = count_args(r->lhs, f);
  if (f <= 0)
    throw err("error in macro definition (missing head symbol)");
  env::iterator it = macenv.find(f), jt = globenv.find(f);
  const symbol& sym = symtab.sym(f);
  if (jt != globenv.end()) {
    if (jt->second.t == env_info::cvar)
      throw err("symbol '"+sym.s+"' is already defined as a constant");
    else if (jt->second.t == env_info::fvar)
      throw err("symbol '"+sym.s+"' is already defined as a variable");
  } else if (it != macenv.end()) {
    if (it->second.argc != argc) {
      ostringstream msg;
      msg << "macro '" << sym.s
	  << "' was previously defined with " << it->second.argc << " args";
      throw err(msg.str());
    }
  }
  env_info &info = macenv[f];
  if (info.t == env_info::none)
    info = env_info(argc, rulel(), temp);
  assert(info.argc == argc);
  r->temp = temp;
  if (override) {
    rulel::iterator p = info.rules->begin();
    for (; p != info.rules->end() && p->temp >= temp; p++) ;
    info.rules->insert(p, *r);
  } else
    info.rules->push_back(*r);
  if ((verbose&verbosity::defs) != 0) cout << "def " << *r << ";\n";
  if (info.m) {
    // this will be recomputed the next time the macro is needed
    delete info.m;
    info.m = 0;
  }
  delete r;
}

void interpreter::closure(expr& l, expr& r, bool b)
{
  env vars;
  expr u = bind(vars, lcsubst(l), b), v = subst(vars, r);
  l = u; r = v;
}

void interpreter::closure(rule& r, bool b)
{
  env vars;
  expr u = expr(bind(vars, lcsubst(r.lhs), b)),
    v = expr(subst(vars, r.rhs)),
    w = expr(subst(vars, r.qual));
  r = rule(u, v, w);
}

expr interpreter::bind(env& vars, expr x, bool b, path p)
{
  assert(!x.is_null());
  expr y;
  switch (x.tag()) {
  case EXPR::VAR: {
    // previously bound variable (successor rule)
    const symbol& sym = symtab.sym(x.vtag());
    if (sym.s != "_") { // '_' = anonymous variable
      assert(p == x.vpath());
      vars[sym.f] = env_info(x.ttag(), p);
    }
    y = x;
    break;
  }
  // constants:
  case EXPR::FVAR:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::WRAP:
    y = x;
    break;
  // application:
  case EXPR::APP: {
    if (p.len() >= MAXDEPTH)
      throw err("error in pattern (nesting too deep)");
    expr u = bind(vars, x.xval1(), b, path(p, 0)),
      v = bind(vars, x.xval2(), 1, path(p, 1));
    y = expr(u, v);
    break;
  }
  // these must not occur on the lhs:
  case EXPR::MATRIX:
    throw err("matrix expression not permitted in pattern");
    break;
  case EXPR::LAMBDA:
    throw err("lambda expression not permitted in pattern");
    break;
  case EXPR::COND:
    throw err("conditional expression not permitted in pattern");
    break;
  case EXPR::CASE:
    throw err("case expression not permitted in pattern");
    break;
  case EXPR::WHEN:
    throw err("when expression not permitted in pattern");
    break;
  case EXPR::WITH:
    throw err("with expression not permitted in pattern");
    break;
  default:
    assert(x.tag() > 0);
    const symbol& sym = symtab.sym(x.tag());
    if ((!qual && (x.flags()&EXPR::QUAL)) ||
	(sym.s != "_" &&
	 (sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix ||
	  (p.len() == 0 && !b) || (p.len() > 0 && p.last() == 0)))) {
      // constant or constructor
      if (x.ttag() != 0)
	throw err("error in pattern (misplaced type tag)");
      y = x;
    } else {
      env::iterator it = vars.find(sym.f);
      if (sym.s != "_") { // '_' = anonymous variable
	if (it != vars.end())
	  throw err("error in pattern (repeated variable '"+sym.s+"')");
	vars[sym.f] = env_info(x.ttag(), p);
      }
      y = expr(EXPR::VAR, sym.f, 0, x.ttag(), p);
    }
    break;
  }
  // check for "as" patterns
  if (x.astag() > 0) {
    const symbol& sym = symtab.sym(x.astag());
    if (sym.s != "_") {
      if ((!qual && (x.flags()&EXPR::ASQUAL)) ||
	  sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix)
	throw err("error in  \"as\" pattern (bad variable symbol)");
      // Unless we're doing a pattern binding, subterms at the spine of a
      // function application won't be available at runtime, so we forbid
      // placing an "as" pattern there.
      if (!b)
	throw err("error in pattern (misplaced variable '"+sym.s+"')");
      env::iterator it = vars.find(sym.f);
      if (it != vars.end()) {
	throw err("error in pattern (repeated variable '"+sym.s+"')");
      }
      vars[sym.f] = env_info(0, p);
      y.set_astag(x.astag());
      y.set_aspath(p);
    }
  }
  return y;
}

void interpreter::promote_ttags(expr f, expr x, expr u)
{
  if (u.ttag() == EXPR::INT) {
    // unary int operations
    if (f.tag() == symtab.neg_sym().f || f.tag() == symtab.not_sym().f ||
	f.tag() == symtab.bitnot_sym().f)
      x.set_ttag(EXPR::INT);
  } else if (u.ttag() == EXPR::DBL) {
    // unary double operations
    if (f.tag() == symtab.neg_sym().f) {
      x.set_ttag(EXPR::DBL);
    }
  }
}

void interpreter::promote_ttags(expr f, expr x, expr u, expr v)
{
  if (((u.ttag() == EXPR::INT || u.ttag() == EXPR::DBL) &&
       (v.ttag() == EXPR::INT || v.ttag() == EXPR::DBL))) {
    if (u.ttag() == EXPR::INT && v.ttag() == EXPR::INT) {
      // binary int operations
      if (f.tag() == symtab.or_sym().f ||
	  f.tag() == symtab.and_sym().f ||
	  f.tag() == symtab.bitor_sym().f ||
	  f.tag() == symtab.bitand_sym().f ||
	  f.tag() == symtab.shl_sym().f ||
	  f.tag() == symtab.shr_sym().f ||
	  f.tag() == symtab.less_sym().f ||
	  f.tag() == symtab.greater_sym().f ||
	  f.tag() == symtab.lesseq_sym().f ||
	  f.tag() == symtab.greatereq_sym().f ||
	  f.tag() == symtab.equal_sym().f ||
	  f.tag() == symtab.notequal_sym().f ||
	  f.tag() == symtab.plus_sym().f ||
	  f.tag() == symtab.minus_sym().f ||
	  f.tag() == symtab.mult_sym().f ||
	  f.tag() == symtab.div_sym().f ||
	  f.tag() == symtab.mod_sym().f) {
	x.set_ttag(EXPR::INT);
      } else if (f.tag() == symtab.fdiv_sym().f) {
	x.set_ttag(EXPR::DBL);
      }
    } else {
      // binary int/double operations
      if (f.tag() == symtab.less_sym().f ||
	  f.tag() == symtab.greater_sym().f ||
	  f.tag() == symtab.lesseq_sym().f ||
	  f.tag() == symtab.greatereq_sym().f ||
	  f.tag() == symtab.equal_sym().f ||
	  f.tag() == symtab.notequal_sym().f) {
	x.set_ttag(EXPR::INT);
      } else if (f.tag() == symtab.plus_sym().f ||
		 f.tag() == symtab.minus_sym().f ||
		 f.tag() == symtab.mult_sym().f ||
		 f.tag() == symtab.fdiv_sym().f) {
	x.set_ttag(EXPR::DBL);
      }
    }
  } else if (f.tag() == symtab.or_sym().f || f.tag() == symtab.and_sym().f)
    /* These two get special treatment, because they have to be evaluated in
       short-circuit mode. Operand types will be checked at runtime if
       necessary. */
    x.set_ttag(EXPR::INT);
}

expr interpreter::subst(const env& vars, expr x, uint8_t idx)
{
  if (x.is_null()) return x;
  if (x.astag() > 0)
    throw err("error in expression (misplaced \"as\" pattern or type tag)");
  switch (x.tag()) {
  // constants:
  case EXPR::VAR:
  case EXPR::FVAR:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::WRAP:
    return x;
  // matrix:
  case EXPR::MATRIX: {
    exprll *us = new exprll;
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      us->push_back(exprl());
      exprl& vs = us->back();
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	vs.push_back(subst(vars, *ys, idx));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP:
    if (x.xval1().tag() == symtab.amp_sym().f) {
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = subst(vars, x.xval2(), idx);
      return expr(symtab.amp_sym().x, v);
    } else if (x.xval1().tag() == EXPR::APP &&
	       x.xval1().xval1().tag() == symtab.catch_sym().f) {
      expr u = subst(vars, x.xval1().xval2(), idx);
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = subst(vars, x.xval2(), idx);
      return expr(symtab.catch_sym().x, u, v);
    } else {
      expr u = subst(vars, x.xval1(), idx),
	v = subst(vars, x.xval2(), idx);
      return expr(u, v);
    }
  // conditionals:
  case EXPR::COND: {
    expr u = subst(vars, x.xval1(), idx),
      v = subst(vars, x.xval2(), idx),
      w = subst(vars, x.xval3(), idx);
    return expr::cond(u, v, w);
  }
  case EXPR::COND1: {
    expr u = subst(vars, x.xval1(), idx),
      v = subst(vars, x.xval2(), idx);
    return expr::cond1(u, v);
  }
  // nested closures:
  case EXPR::LAMBDA: {
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    expr u = x.xval1(), v = subst(vars, x.xval2(), idx);
    return expr::lambda(u, v);
  }
  case EXPR::CASE: {
    expr u = subst(vars, x.xval(), idx);
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs,	v = subst(vars, it->rhs, idx),
	w = subst(vars, it->qual, idx);
      s->push_back(rule(u, v, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    // This is slightly more involved, since a 'when' expression with k rules
    // actually introduces k different levels of bindings:
    // x when l1 = r1; ...; lk = rk end  ===
    // @@0: case r1 of l1 = ... @@k-1: case rk of lk = @@k:x end ... end
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = subst(vars, it->rhs, idx);
      s->push_back(rule(u, v));
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
    }
    expr u = subst(vars, x.xval(), idx);
    return expr::when(u, s);
  }
  case EXPR::WITH: {
    expr u = subst(vars, x.xval(), idx);
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    const env *e = x.fenv();
    env *f = new env;
    for (env::const_iterator it = e->begin(); it != e->end(); ++it) {
      int32_t g = it->first;
      const env_info& info = it->second;
      const rulel *r = info.rules;
      rulel s;
      for (rulel::const_iterator jt = r->begin(); jt != r->end(); ++jt) {
	expr u = jt->lhs, v = subst(vars, jt->rhs, idx),
	  w = subst(vars, jt->qual, idx);
	s.push_back(rule(u, v, w));
      }
      (*f)[g] = env_info(info.argc, s, info.temp);
    }
    return expr::with(u, f);
  }
  default:
    assert(x.tag() > 0);
    const symbol& sym = symtab.sym(x.tag());
    env::const_iterator it = vars.find(sym.f);
    if (sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix ||
	it == vars.end() || (!qual && (x.flags()&EXPR::QUAL))) {
      // not a bound variable
      if (x.ttag() != 0)
	throw err("error in expression (misplaced type tag)");
      return x;
    }
    const env_info& info = it->second;
    return expr(EXPR::VAR, sym.f, idx, info.ttag, *info.p);
  }
}

expr interpreter::fsubst(const env& funs, expr x, uint8_t idx)
{
  if (x.is_null()) return x;
  switch (x.tag()) {
  // constants:
  case EXPR::VAR:
  case EXPR::FVAR:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::WRAP:
    return x;
  // matrix:
  case EXPR::MATRIX: {
    exprll *us = new exprll;
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      us->push_back(exprl());
      exprl& vs = us->back();
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	vs.push_back(fsubst(funs, *ys, idx));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP:
    if (x.xval1().tag() == symtab.amp_sym().f) {
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = fsubst(funs, x.xval2(), idx);
      return expr(symtab.amp_sym().x, v);
    } else if (x.xval1().tag() == EXPR::APP &&
	       x.xval1().xval1().tag() == symtab.catch_sym().f) {
      expr u = fsubst(funs, x.xval1().xval2(), idx);
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = fsubst(funs, x.xval2(), idx);
      return expr(symtab.catch_sym().x, u, v);
    } else {
      expr u = fsubst(funs, x.xval1(), idx),
	v = fsubst(funs, x.xval2(), idx);
      return expr(u, v);
    }
  // conditionals:
  case EXPR::COND: {
    expr u = fsubst(funs, x.xval1(), idx),
      v = fsubst(funs, x.xval2(), idx),
      w = fsubst(funs, x.xval3(), idx);
    return expr::cond(u, v, w);
  }
  case EXPR::COND1: {
    expr u = fsubst(funs, x.xval1(), idx),
      v = fsubst(funs, x.xval2(), idx);
    return expr::cond1(u, v);
  }
  // nested closures:
  case EXPR::LAMBDA: {
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    expr u = x.xval1(), v = fsubst(funs, x.xval2(), idx);
    return expr::lambda(u, v);
  }
  case EXPR::CASE: {
    expr u = fsubst(funs, x.xval(), idx);
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs,	v = fsubst(funs, it->rhs, idx),
	w = fsubst(funs, it->qual, idx);
      s->push_back(rule(u, v, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = fsubst(funs, it->rhs, idx);
      s->push_back(rule(u, v));
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
    }
    expr u = fsubst(funs, x.xval(), idx);
    return expr::when(u, s);
  }
  case EXPR::WITH: {
    expr u = fsubst(funs, x.xval(), idx);
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    const env *e = x.fenv();
    env *f = new env;
    for (env::const_iterator it = e->begin(); it != e->end(); ++it) {
      int32_t g = it->first;
      const env_info& info = it->second;
      const rulel *r = info.rules;
      rulel s;
      for (rulel::const_iterator jt = r->begin(); jt != r->end(); ++jt) {
	expr u = jt->lhs, v = fsubst(funs, jt->rhs, idx),
	  w = fsubst(funs, jt->qual, idx);
	s.push_back(rule(u, v, w));
      }
      (*f)[g] = env_info(info.argc, s, info.temp);
    }
    return expr::with(u, f);
  }
  default:
    assert(x.tag() > 0);
    const symbol& sym = symtab.sym(x.tag());
    env::const_iterator it = funs.find(sym.f);
    if (it == funs.end() || (!qual && (x.flags()&EXPR::QUAL)))
      // not a locally bound function
      return x;
    else
      return expr(EXPR::FVAR, sym.f, idx);
  }
}

expr interpreter::csubst(expr x, bool quote)
{
  if (x.is_null()) return x;
  switch (x.tag()) {
  // constants:
  case EXPR::VAR:
  case EXPR::FVAR:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::WRAP:
    return x;
  // matrix:
  case EXPR::MATRIX: {
    exprll *us = new exprll;
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      us->push_back(exprl());
      exprl& vs = us->back();
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	vs.push_back(csubst(*ys, quote));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP:
    if (quote) {
      expr u = csubst(x.xval1(), true),
	v = csubst(x.xval2(), true);
      expr w = expr(u, v);
      // promote type tags
      expr f; uint32_t n = count_args(w, f);
      if (n == 1)
	promote_ttags(f, w, w.xval2());
      else if (n == 2)
	promote_ttags(f, w, w.xval1().xval2(), w.xval2());
      return w;
    } else if (x.xval1().tag() == symtab.amp_sym().f) {
      expr v = csubst(x.xval2());
      return expr(symtab.amp_sym().x, v);
    } else if (x.xval1().tag() == EXPR::APP &&
	       x.xval1().xval1().tag() == symtab.catch_sym().f) {
      expr u = csubst(x.xval1().xval2()),
	v = csubst(x.xval2());
      return expr(symtab.catch_sym().x, u, v);
    } else {
      expr u = csubst(x.xval1()),
	v = csubst(x.xval2(), is_quote(u.tag()));
      expr w = expr(u, v);
      // promote type tags
      expr f; uint32_t n = count_args(w, f);
      if (n == 1)
	promote_ttags(f, w, w.xval2());
      else if (n == 2)
	promote_ttags(f, w, w.xval1().xval2(), w.xval2());
      return w;
    }
  // conditionals:
  case EXPR::COND: {
    expr u = csubst(x.xval1()),
      v = csubst(x.xval2()),
      w = csubst(x.xval3());
    return expr::cond(u, v, w);
  }
  case EXPR::COND1: {
    expr u = csubst(x.xval1()),
      v = csubst(x.xval2());
    return expr::cond1(u, v);
  }
  // nested closures:
  case EXPR::LAMBDA: {
    expr u = x.xval1(), v = csubst(x.xval2());
    return expr::lambda(u, v);
  }
  case EXPR::CASE: {
    expr u = csubst(x.xval());
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs,	v = csubst(it->rhs),
	w = csubst(it->qual);
      s->push_back(rule(u, v, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = csubst(it->rhs);
      s->push_back(rule(u, v));
    }
    expr u = csubst(x.xval());
    return expr::when(u, s);
  }
  case EXPR::WITH: {
    expr u = csubst(x.xval());
    const env *e = x.fenv();
    env *f = new env;
    for (env::const_iterator it = e->begin(); it != e->end(); ++it) {
      int32_t g = it->first;
      const env_info& info = it->second;
      const rulel *r = info.rules;
      rulel s;
      for (rulel::const_iterator jt = r->begin(); jt != r->end(); ++jt) {
	expr u = jt->lhs, v = csubst(jt->rhs),
	  w = csubst(jt->qual);
	s.push_back(rule(u, v, w));
      }
      (*f)[g] = env_info(info.argc, s, info.temp);
    }
    return expr::with(u, f);
  }
  default:
    assert(x.tag() > 0);
    if (quote)
      return x;
    else {
      const symbol& sym = symtab.sym(x.tag());
      env::const_iterator it = globenv.find(sym.f);
      if (it != globenv.end() && it->second.t == env_info::cvar)
	// substitute constant value
	return *it->second.cval;
      else
	return x;
    }
  }
}

/* This is a trimmed-down version of csubst() for performing replacements of
   nonfix const symbols in patterns. */

expr interpreter::lcsubst(expr x)
{
  if (x.is_null()) return x;
  switch (x.tag()) {
  // constants:
  case EXPR::VAR:
  case EXPR::FVAR:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::WRAP:
    return x;
  // these must not occur on the lhs:
  case EXPR::MATRIX:
    throw err("matrix expression not permitted in pattern");
    break;
  case EXPR::LAMBDA:
    throw err("lambda expression not permitted in pattern");
    break;
  case EXPR::COND:
    throw err("conditional expression not permitted in pattern");
    break;
  case EXPR::CASE:
    throw err("case expression not permitted in pattern");
    break;
  case EXPR::WHEN:
    throw err("when expression not permitted in pattern");
    break;
  case EXPR::WITH:
    throw err("with expression not permitted in pattern");
    break;
  // application:
  case EXPR::APP: {
    expr u = lcsubst(x.xval1()),
      v = lcsubst(x.xval2());
    expr w = expr(u, v);
    w.set_astag(x.astag());
    return w;
  }
  default:
    assert(x.tag() > 0);
    const symbol& sym = symtab.sym(x.tag());
    if (sym.fix == nonfix) {
      env::const_iterator it = globenv.find(sym.f);
      if (it != globenv.end() && it->second.t == env_info::cvar) {
	/* FIXME: This breaks when the same constant is used with different
	   "as" patterns in the same pattern. This is hard to fix right now,
	   and it doesn't really seem to be worth the effort. Oh well,
	   hopefully noone will ever notice. */
	it->second.cval->set_astag(x.astag());
	// substitute constant value
	return *it->second.cval;
      } else
	return x;
    } else
      return x;
  }
}

/* Perform simple macro substitutions on a compile time expression. Does
   applicative-order (depth-first) evaluation using the defined macro
   substitution rules (which are simple, unconditional term rewriting
   rules). Everything else but macro applications is considered constant
   here. When we match a macro call, we perform the corresponding reduction
   and evaluate the result recursively.

   Note that in contrast to compiled rewriting rules this is essentially a
   little term rewriting interpreter here, so it's kind of slow compared to
   compiled code, but for macro substitution it should be good enough. (We
   can't use compiled code here, since the runtime expression data structure
   cannot represent special kinds of expressions like anonymous closures, with
   and when clauses, etc.) */

expr interpreter::macsubst(expr x, bool quote)
{
  char test;
  if (x.is_null()) return x;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax)
    throw err("recursion too deep in macro expansion");
  switch (x.tag()) {
  // constants:
  case EXPR::VAR:
  case EXPR::FVAR:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::WRAP:
    return x;
  // matrix:
  case EXPR::MATRIX: {
    exprll *us = new exprll;
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      us->push_back(exprl());
      exprl& vs = us->back();
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	vs.push_back(macsubst(*ys, quote));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP: {
    expr u = macsubst(x.xval1(), quote),
      v = macsubst(x.xval2(), quote || is_quote(u.tag()));
    expr w = expr(u, v);
    return quote?w:macval(w);
  }
  // conditionals:
  case EXPR::COND: {
    expr u = macsubst(x.xval1()),
      v = macsubst(x.xval2()),
      w = macsubst(x.xval3());
    return expr::cond(u, v, w);
  }
  case EXPR::COND1: {
    expr u = macsubst(x.xval1()),
      v = macsubst(x.xval2());
    return expr::cond1(u, v);
  }
  // nested closures:
  case EXPR::LAMBDA: {
    expr u = x.xval1(), v = macsubst(x.xval2());
    return expr::lambda(u, v);
  }
  case EXPR::CASE: {
    expr u = macsubst(x.xval());
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs,	v = macsubst(it->rhs),
	w = macsubst(it->qual);
      s->push_back(rule(u, v, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = macsubst(it->rhs);
      s->push_back(rule(u, v));
    }
    expr u = macsubst(x.xval());
    return expr::when(u, s);
  }
  case EXPR::WITH: {
    expr u = macsubst(x.xval());
    const env *e = x.fenv();
    env *f = new env;
    for (env::const_iterator it = e->begin(); it != e->end(); ++it) {
      int32_t g = it->first;
      const env_info& info = it->second;
      const rulel *r = info.rules;
      rulel s;
      for (rulel::const_iterator jt = r->begin(); jt != r->end(); ++jt) {
	expr u = jt->lhs, v = macsubst(jt->rhs),
	  w = macsubst(jt->qual);
	s.push_back(rule(u, v, w));
      }
      (*f)[g] = env_info(info.argc, s, info.temp);
    }
    return expr::with(u, f);
  }
  default:
    assert(x.tag() > 0);
    return quote?x:macval(x);
  }
}

/* Perform a single macro reduction step. */

expr interpreter::varsubst(expr x, uint8_t offs, uint8_t idx)
{
  char test;
  if (x.is_null()) return x;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax)
    throw err("recursion too deep in macro expansion");
  switch (x.tag()) {
  case EXPR::VAR:
  case EXPR::FVAR:
    if (x.vidx() < idx)
      /* reference to local environment inside the substituted value; skip */
      return x;
    if (((uint32_t)x.vidx()) + offs > 0xff)
      throw err("error in expression (too many nested closures)");
    if (x.tag() == EXPR::VAR)
      return expr(EXPR::VAR, x.vtag(), x.vidx()+offs, x.ttag(), x.vpath());
    else
      return expr(EXPR::FVAR, x.vtag(), x.vidx()+offs);
  // constants:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::WRAP:
    return x;
  // matrix:
  case EXPR::MATRIX: {
    exprll *us = new exprll;
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      us->push_back(exprl());
      exprl& vs = us->back();
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	vs.push_back(varsubst(*ys, offs, idx));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP: {
    if (x.xval1().tag() == symtab.amp_sym().f) {
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = varsubst(x.xval2(), offs, idx);
      return expr(symtab.amp_sym().x, v);
    } else if (x.xval1().tag() == EXPR::APP &&
	       x.xval1().xval1().tag() == symtab.catch_sym().f) {
      expr u = varsubst(x.xval1().xval2(), offs, idx);
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = varsubst(x.xval2(), offs, idx);
      return expr(symtab.catch_sym().x, u, v);
    } else {
      expr u = varsubst(x.xval1(), offs, idx),
	v = varsubst(x.xval2(), offs, idx);
      return expr(u, v);
    }
  }
  // conditionals:
  case EXPR::COND: {
    expr u = varsubst(x.xval1(), offs, idx),
      v = varsubst(x.xval2(), offs, idx),
      w = varsubst(x.xval3(), offs, idx);
    return expr::cond(u, v, w);
  }
  case EXPR::COND1: {
    expr u = varsubst(x.xval1(), offs, idx),
      v = varsubst(x.xval2(), offs, idx);
    return expr::cond1(u, v);
  }
  // nested closures:
  case EXPR::LAMBDA: {
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    expr u = x.xval1(), v = varsubst(x.xval2(), offs, idx);
    return expr::lambda(u, v);
  }
  case EXPR::CASE: {
    expr u = varsubst(x.xval(), offs, idx);
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs,	v = varsubst(it->rhs, offs, idx),
	w = varsubst(it->qual, offs, idx);
      s->push_back(rule(u, v, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = varsubst(it->rhs, offs, idx);
      s->push_back(rule(u, v));
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
    }
    expr u = varsubst(x.xval(), offs, idx);
    return expr::when(u, s);
  }
  case EXPR::WITH: {
    expr u = varsubst(x.xval(), offs, idx);
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    const env *e = x.fenv();
    env *f = new env;
    for (env::const_iterator it = e->begin(); it != e->end(); ++it) {
      int32_t g = it->first;
      const env_info& info = it->second;
      const rulel *r = info.rules;
      rulel s;
      for (rulel::const_iterator jt = r->begin(); jt != r->end(); ++jt) {
	expr u = jt->lhs, v = varsubst(jt->rhs, offs, idx),
	  w = varsubst(jt->qual, offs, idx);
	s.push_back(rule(u, v, w));
      }
      (*f)[g] = env_info(info.argc, s, info.temp);
    }
    return expr::with(u, f);
  }
  default:
    assert(x.tag() > 0);
    return x;
  }
}

expr interpreter::macred(expr x, expr y, uint8_t idx)
{
  char test;
  if (y.is_null()) return y;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax)
    throw err("recursion too deep in macro expansion");
  switch (y.tag()) {
  // constants:
  case EXPR::FVAR:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::WRAP:
    return y;
  // lhs variable
  case EXPR::VAR:
    if (y.vidx() == idx) {
      /* Substitute the macro variables, which are the lhs values whose idx
	 match the current idx. We also have to translate deBruijn indices
	 inside the substituted value which refer to bindings outside the
	 macro parameter, to account for local environments inside the macro
	 definition. */
      expr v = varsubst(subterm(x, y.vpath()), idx);
#if DEBUG>1
      std::cerr << "macro var: " << y << " = " << v
		<< " (" << (uint32_t)idx << ")" << '\n';
#endif
      return v;
    } else
      return y;
  // matrix:
  case EXPR::MATRIX: {
    exprll *us = new exprll;
    for (exprll::iterator xs = y.xvals()->begin(), end = y.xvals()->end();
	 xs != end; xs++) {
      us->push_back(exprl());
      exprl& vs = us->back();
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	vs.push_back(macred(x, *ys, idx));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP:
    if (y.xval1().tag() == symtab.amp_sym().f) {
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = macred(x, y.xval2(), idx);
      return expr(symtab.amp_sym().x, v);
    } else if (y.xval1().tag() == EXPR::APP &&
	       y.xval1().xval1().tag() == symtab.catch_sym().f) {
      expr u = macred(x, y.xval1().xval2(), idx);
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = macred(x, y.xval2(), idx);
      return expr(symtab.catch_sym().x, u, v);
    } else {
      expr u = macred(x, y.xval1(), idx),
	v = macred(x, y.xval2(), idx);
      if (u.tag() == symtab.amp_sym().f ||
	  (u.tag() == EXPR::APP &&
	   u.xval1().tag() == symtab.catch_sym().f)) {
	/* A catch clause or thunk was created through macro
	   substitution. Translate the deBruijn indices in the body
	   accordingly. */
	expr w = varsubst(v, 1);
	return expr(u, w);
      } else
	return expr(u, v);
    }
  // conditionals:
  case EXPR::COND: {
    expr u = macred(x, y.xval1(), idx),
      v = macred(x, y.xval2(), idx),
      w = macred(x, y.xval3(), idx);
    return expr::cond(u, v, w);
  }
  case EXPR::COND1: {
    expr u = macred(x, y.xval1(), idx),
      v = macred(x, y.xval2(), idx);
    return expr::cond1(u, v);
  }
  // nested closures:
  case EXPR::LAMBDA: {
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    expr u = y.xval1(), v = macred(x, y.xval2(), idx);
    return expr::lambda(u, v);
  }
  case EXPR::CASE: {
    expr u = macred(x, y.xval(), idx);
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    const rulel *r = y.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs,	v = macred(x, it->rhs, idx),
	w = macred(x, it->qual, idx);
      s->push_back(rule(u, v, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = y.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = macred(x, it->rhs, idx);
      s->push_back(rule(u, v));
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
    }
    expr u = macred(x, y.xval(), idx);
    return expr::when(u, s);
  }
  case EXPR::WITH: {
    expr u = macred(x, y.xval(), idx);
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    const env *e = y.fenv();
    env *f = new env;
    for (env::const_iterator it = e->begin(); it != e->end(); ++it) {
      int32_t g = it->first;
      const env_info& info = it->second;
      const rulel *r = info.rules;
      rulel s;
      for (rulel::const_iterator jt = r->begin(); jt != r->end(); ++jt) {
	expr u = jt->lhs, v = macred(x, jt->rhs, idx),
	  w = macred(x, jt->qual, idx);
	s.push_back(rule(u, v, w));
      }
      (*f)[g] = env_info(info.argc, s, info.temp);
    }
    return expr::with(u, f);
  }
  default:
    assert(y.tag() > 0);
    return y;
  }
}

/* Evaluate a macro call. */

static exprl get_args(expr x)
{
  expr y, z;
  exprl xs;
  while (x.is_app(y, z)) xs.push_front(z), x = y;
  return xs;
}

expr interpreter::macval(expr x)
{
  char test;
  if (x.is_null()) return x;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax)
    throw err("recursion too deep in macro expansion");
  int32_t f; uint32_t argc = count_args(x, f);
  if (f <= 0) return x;
  env::iterator it = macenv.find(f);
  if (it == macenv.end()) return x;
  env_info &info = it->second;
  if (argc != info.argc) return x;
  if (!info.m)
    info.m = new matcher(*info.rules, info.argc+1);
  assert(info.m);
  exprl args = get_args(x);
  assert(args.size() == argc);
  state *st = info.m->match(args);
  if (st) {
    assert(!st->r.empty());
    expr y = macred(x, info.m->r[st->r.front()].rhs);
#if DEBUG>1
    std::cerr << "macro expansion: " << x << " -> " << y << '\n';
#endif
    return macsubst(y);
  }
  return x;
}

expr interpreter::uminop(expr op, expr x)
{
  // handle special case of a numeric argument
  if (x.tag() == EXPR::BIGINT && (x.flags()&EXPR::OVF) &&
      mpz_cmp_ui(x.zval(), 0x80000000U) == 0)
    // The negated int 0x80000000 can actually be represented as a machine int
    // value, we convert it back on the fly here.
    return expr(EXPR::INT, (int32_t)-0x80000000);
  else if (x.tag() == EXPR::INT)
    return expr(EXPR::INT, -x.ival());
  else if (x.tag() == EXPR::DBL)
    return expr(EXPR::DBL, -x.dval());
  else // otherwise fall back to an explicit application
    return expr(op, x);
}

expr *interpreter::mklsect(expr *x, expr *y)
{
  expr *u = new expr(*x, *y);
  delete x; delete y;
  return u;
}

expr *interpreter::mkrsect(expr *x, expr *y)
{
  expr *u = new expr(symtab.flip_sym().x, *x, *y);
  delete x; delete y;
  return u;
}

expr *interpreter::mkexpr(expr *x, expr *y)
{
  expr *u = new expr(*x, *y);
  delete x; delete y;
  return u;
}

expr *interpreter::mkexpr(expr *x, expr *y, expr *z)
{
  expr *u = new expr(*x, *y, *z);
  delete x; delete y; delete z;
  return u;
}

expr *interpreter::mksym_expr(string *s, int32_t tag)
{
  expr *x;
  const symbol &sym = symtab.checksym(*s);
  if (tag == 0)
    if (*s == "_")
      // Return a new instance here, since the anonymous variable may have
      // multiple occurrences on the lhs.
      x = new expr(sym.f);
    else if (s->find("::") != string::npos) {
      // Return a new qualified instance here, so that we don't mistake this
      // for a lhs variable.
      x = new expr(sym.f);
      x->flags() |= EXPR::QUAL;
    } else
      x = new expr(sym.x);
  else if (sym.f <= 0 || sym.prec < PREC_MAX ||
	   sym.fix == nonfix || sym.fix == outfix)
    throw err("error in expression (misplaced type tag)");
  else if (tag > 0)
    if (*s == "_")
      x = new expr(expr(tag), expr(symtab.sym("::_")->f));
    else
      return mkas_expr(s, new expr(expr(tag), expr(symtab.sym("::_")->f)));
  else {
    x = new expr(sym.f);
    if (s->find("::") != string::npos) x->flags() |= EXPR::QUAL;
    // record type tag:
    x->set_ttag(tag);
  }
  delete s;
  return x;
}

expr *interpreter::mkas_expr(string *s, expr *x)
{
  const symbol &sym = symtab.checksym(*s);
  if (sym.f <= 0 || sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix)
    throw err("error in  \"as\" pattern (bad variable symbol)");
  if (x->tag() > 0) {
    // Avoid globbering cached function symbols.
    expr *y = new expr(x->tag());
    delete x;
    x = y;
  }
  x->set_astag(sym.f);
  if (s->find("::") != string::npos) x->flags() |= EXPR::ASQUAL;
  delete s;
  return x;
}

expr *interpreter::mksimple_expr(OpStack *in)
{
  list<OpEntry>::iterator act = in->stk.begin(), end = in->stk.end();
  expr ret = parse_simple(act, end, 0);
  delete in;
  return new expr(ret);
}

static string fixity_str(fix_t fix)
{
  switch (fix) {
  case infix: return "infix";
  case infixl: return "infixl";
  case infixr: return "infixr";
  case prefix: return "prefix";
  case postfix: return "postfix";
  case outfix: return "outfix";
  case nonfix: return "nonfix";
  default: return "<bad fixity value>";
  }
}

expr interpreter::parse_simple(list<OpEntry>::iterator& act,
			       list<OpEntry>::iterator end,
			       prec_t min)
{
  /* Operator precedence parser for the Pure expression syntax. We use a
     variation of Dijkstra's "shunting yard" algorithm here, modified to deal
     with prefix and postfix symbols. This lets us determine the proper
     parsing of a list of primary expressions and declared operator symbols on
     the fly. */
#if 0
  std::cerr << "unparsed input:";
  for (list<OpEntry>::iterator it = act; it != end; ++it)
    std::cerr << " " << it->x;
  std::cerr << endl;
#endif
  OpStack out;
  while (act != end) {
    if (act->is_op) {
      expr x = act->x;
      int32_t f = x.tag();
      fix_t fix = symtab.sym(f).fix;
      prec_t prec = symtab.sym(f).prec;
      if (out.stk.empty() || out.stk.back().is_op) {
	// we expect a prefix operator here
	if (fix != prefix && f != symtab.minus_sym().f) {
	  throw err("syntax error, unexpected "+fixity_str(fix)+
		    " operator '"+symtab.sym(f).s+"'");
	}
	// an operand must follow here
	if (++act == end) {
	  throw err("syntax error, expected operand after prefix operator '"+
		    symtab.sym(f).s+"'");
	}
	expr y = parse_simple(act, end, nprec(prec, prefix));
	if (x.tag() == symtab.minus_sym().f)
	  y = uminop(symtab.neg_sym().x, y);
	else
	  y = expr(x, y);
	out.push_arg(y);
	continue;
      }
      // an operand most be on the top of the output stack now
      assert(!out.stk.empty() && !out.stk.back().is_op);
      prec_t p = nprec(prec, fix);
      if (p < min) break;
      expr *y = out.last_op();
      while (y) {
	symbol& sym = symtab.sym(y->tag());
	prec_t q = nprec(sym.prec, sym.fix);
	if (q > p || (q == p && fix == infixl)) {
	  expr b = out.stk.back().x; out.pop();
	  expr f = out.stk.back().x; out.pop();
	  expr a = out.stk.back().x; out.pop();
	  a = expr(f, a, b);
	  out.push_arg(a);
	  y = out.last_op();
	} else
	  break;
      }
      if (fix == infix && y) {
	symbol& sym = symtab.sym(y->tag());
	prec_t q = nprec(sym.prec, sym.fix);
	if (p == q) {
	  throw err("syntax error, unexpected infix operator '"+
		    symtab.sym(f).s+"'");
	}
      }
      if (fix == infix || fix == infixl || fix == infixr) {
	// process infix operator
	out.push_op(x);
	act++;
      } else if (fix == postfix) {
	// process postfix operator
	expr y = out.stk.back().x; out.pop();
	x = expr(x, y);
	out.push_arg(x);
	act++;
      } else {
	throw err("syntax error, unexpected "+fixity_str(fix)+
		  " operator '"+symtab.sym(f).s+"'");
      }
    } else {
      /* process a primary or application */
      expr x = act->x;
      while (++act != end && !act->is_op)
	x = expr(x, act->x);
      out.push_arg(x);
    }
  }
  expr *y = out.last_op();
  while (y) {
    if (out.stk.back().is_op) {
      throw err("syntax error, expected operand after infix operator '"+
		symtab.sym(y->tag()).s+"'");
    }
    expr b = out.stk.back().x; out.pop();
    expr f = out.stk.back().x; out.pop();
    expr a = out.stk.back().x; out.pop();
    a = expr(f, a, b);
    out.push_arg(a);
    y = out.last_op();
  }
#if 0
  std::cerr << "parsed output:";
  for (list<OpEntry>::iterator it = out.stk.begin(), end = out.stk.end();
       it != end; ++it)
    std::cerr << " " << it->x;
  std::cerr << endl;
#endif
  assert(out.stk.size() == 1);
  return out.stk.back().x;
}

expr *interpreter::mkcond_expr(expr *x, expr *y, expr *z)
{
  expr *u = new expr(expr::cond(*x, *y, *z));
  delete x; delete y; delete z;
  return u;
}

expr *interpreter::mkcond1_expr(expr *x, expr *y)
{
  expr *u = new expr(expr::cond1(*x, *y));
  delete x; delete y;
  return u;
}

static inline expr lambda_expr(interpreter& interp, expr arg, expr body)
{
  interp.closure(arg, body);
  return expr::lambda(arg, body);
}

static expr lambda_expr(interpreter& interp, 
			exprl::iterator it, exprl::iterator end, expr body)
{
  if (it == end)
    return body;
  else {
    expr arg = *it;
    return lambda_expr(interp, arg, lambda_expr(interp, ++it, end, body));
  }
}

expr *interpreter::mklambda_expr(exprl *args, expr *body)
{
  assert(!args->empty());
  expr *x;
  try {
    x = new expr(lambda_expr(*this, args->begin(), args->end(), *body));
  } catch (err &e) {
    delete args; delete body;
    throw e;
  }
  delete args; delete body;
  return x;
}

expr *interpreter::mkcase_expr(expr *x, rulel *r)
{
  expr *u;
  if (r->empty()) {
    u = new expr(); delete r;
  } else
    u = new expr(expr::cases(*x, r));
  delete x;
  return u;
}

expr *interpreter::mkwhen_expr(expr *x, rulel *r)
{
  if (r->empty()) {
    delete x; delete r;
    return new expr();
  }
  expr u = *x;
  delete x;
  // x when l1 = r1; ...; lk = rk end  ===
  // case r1 of l1 = ... case rk of lk = x end ... end
  if (r->size() > 0x100) {
    delete r;
    throw err("error in expression (too many nested closures)");
  }
  rulel *s = new rulel;
  uint8_t idx = 0;
  for (rulel::reverse_iterator it = r->rbegin();
       it != r->rend(); ++it, ++idx) {
    env vars;
    expr v = bind(vars, lcsubst(it->lhs)), w = it->rhs;
    u = subst(vars, u, idx);
    uint8_t jdx = 0;
    for (rulel::iterator jt = s->begin(); jt != s->end(); ++jdx, ++jt) {
      expr v = jt->lhs, w = subst(vars, jt->rhs, jdx);
      *jt = rule(v, w);
    }
    s->push_front(rule(v, w));
  }
  delete r;
  return new expr(expr::when(u, s));
}

expr *interpreter::mkwith_expr(expr *x, env *e)
{
  expr *u;
  if (e->empty()) {
    delete x; delete e;
    u = new expr();
  } else {
    expr v = fsubst(*e, *x);
    delete x;
    for (env::iterator it = e->begin(); it != e->end(); ++it) {
      env_info& info = it->second;
      rulel *r = info.rules;
      for (rulel::iterator jt = r->begin(); jt != r->end(); ++jt) {
	expr rhs = fsubst(*e, jt->rhs, 1), qual = fsubst(*e, jt->qual, 1);
	*jt = rule(jt->lhs, rhs, qual);
      }
    }
    u = new expr(expr::with(v, e));
  }
  return u;
}

exprl *interpreter::mkrow_exprl(expr *x)
{
  exprl *xs = new exprl;
  if (!x->is_pair() || !x->is_tuplel(*xs))
    xs->push_back(*x);
  delete x;
  return xs;
}

expr *interpreter::mklist_expr(expr *x)
{
  expr *u;
  exprl xs;
  if (x->is_pair() && x->is_tuplel(xs))
    u = new expr(expr::list(xs));
  else
    u = new expr(expr::cons(*x, expr::nil()));
  delete x;
  return u;
}

expr interpreter::mklistcomp_expr(expr x, comp_clause_list::iterator cs,
				  comp_clause_list::iterator end)
{
  if (cs == end)
    return expr::cons(x, expr::nil());
  else {
    comp_clause_list::iterator next_cs = cs;
    ++next_cs;
    comp_clause& c = *cs;
    if (c.second.is_null()) {
      expr p = c.first;
      return expr::cond(p, mklistcomp_expr(x, next_cs, end), expr::nil());
    } else if (next_cs == end) {
      expr pat = c.first, arg = c.second;
      closure(pat, x);
      return expr(symtab.listmap_sym().x, expr::lambda(pat, x), arg);
    } else {
      expr pat = c.first, body = mklistcomp_expr(x, next_cs, end),
	arg = c.second;
      closure(pat, body);
      return expr(symtab.catmap_sym().x, expr::lambda(pat, body), arg);
    }
  }
}

expr *interpreter::mklistcomp_expr(expr *x, comp_clause_list *cs)
{
  expr y = mklistcomp_expr(*x, cs->begin(), cs->end());
  delete x; delete cs;
  return new expr(y);
}

expr interpreter::mkmatcomp_expr(expr x, size_t n,
				 comp_clause_list::iterator cs,
				 comp_clause_list::iterator end)
{
  if (cs == end) {
    exprll *xs = new exprll(1, exprl(1, x));
    return expr(EXPR::MATRIX, xs);
  } else {
    comp_clause_list::iterator next_cs = cs;
    ++next_cs;
    comp_clause& c = *cs;
    if (c.second.is_null()) {
      expr p = c.first;
      return expr::cond(p, mkmatcomp_expr(x, n, next_cs, end),
			expr(EXPR::MATRIX, new exprll));
    } else if (next_cs == end) {
      expr pat = c.first, arg = c.second;
      closure(pat, x);
      expr f = (n&1)?symtab.colmap_sym().x:symtab.rowmap_sym().x;
      return expr(f, expr::lambda(pat, x), arg);
    } else {
      expr pat = c.first, body = mkmatcomp_expr(x, n-1, next_cs, end),
	arg = c.second;
      closure(pat, body);
      expr f = (n&1)?symtab.colcatmap_sym().x:symtab.rowcatmap_sym().x;
      return expr(f, expr::lambda(pat, body), arg);
    }
  }
}

expr *interpreter::mkmatcomp_expr(expr *x, comp_clause_list *cs)
{
  size_t n = 0;
  for (comp_clause_list::iterator it = cs->begin(), end = cs->end();
       it != end; it++) {
    comp_clause& c = *it;
    if (!c.second.is_null()) n++;
  }
  expr y = mkmatcomp_expr(*x, n, cs->begin(), cs->end());
  delete x; delete cs;
  return new expr(y);
}

// Code generation.

#define Dbl(d)		ConstantFP::get(interpreter::double_type(), d)
#define Bool(i)		ConstantInt::get(interpreter::int1_type(), i)
#define Char(i)		ConstantInt::get(interpreter::int8_type(), i)
#define UInt(i)		ConstantInt::get(interpreter::int32_type(), i)
#define SInt(i)		ConstantInt::get(interpreter::int32_type(), (uint64_t)i, true)
#define UInt64(i)	ConstantInt::get(interpreter::int64_type(), i)
#define SInt64(i)	ConstantInt::get(interpreter::int64_type(), (uint64_t)i, true)
#if SIZEOF_SIZE_T==4
#define SizeInt(i)	UInt(i)
#else
#define SizeInt(i)	UInt64(i)
#endif
#define False		Bool(0)
#define True		Bool(1)
#define Zero		UInt(0)
#define One		UInt(1)
#define Two		UInt(2)
#define Three		UInt(3)
#define RefcFldIndex	One
#define ValFldIndex	Two
#define ValFld2Index	Three
#define SubFldIndex(i)	UInt(i+2)
#define NullPtr		ConstantPointerNull::get(VoidPtrTy)
#define NullExprPtr	ConstantPointerNull::get(ExprPtrTy)
#define NullExprPtrPtr	ConstantPointerNull::get(ExprPtrPtrTy)

const char *interpreter::mklabel(const char *name, uint32_t i)
{
  char lab[128];
  sprintf(lab, "%s%u", name, i);
  char *s = strdup(lab);
  cache.push_back(s);
  return s;
}

const char *interpreter::mklabel(const char *name, uint32_t i, uint32_t j)
{
  char lab[128];
  sprintf(lab, "%s%u.%u", name, i, j);
  char *s = strdup(lab);
  cache.push_back(s);
  return s;
}

const char *interpreter::mkvarlabel(int32_t tag)
{
  assert(tag > 0);
  const symbol& sym = symtab.sym(tag);
  string lab;
  if (sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix)
    if (sym.fix == outfix && sym.g)
      lab = "$("+sym.s+" "+symtab.sym(sym.g).s+")";
    else
      lab = "$("+sym.s+")";
  else
    lab = "$"+sym.s;
  char *s = strdup(lab.c_str());
  cache.push_back(s);
  return s;
}

void interpreter::clear_cache()
{
  for (list<char*>::iterator s = cache.begin(); s != cache.end(); s++)
    free(*s);
  cache.clear();
}

string interpreter::make_qualid(const string& id)
{
  if (!symtab.current_namespace->empty())
    return (*symtab.current_namespace)+"::"+id;
  else
    return id;
}

string interpreter::make_absid(const string& id)
{
  if (!symtab.current_namespace->empty())
    return "::"+(*symtab.current_namespace)+"::"+id;
  else
    return "::"+id;
}

using namespace llvm;

#include <iostream>
#include <fstream>
#include <sstream>
#include <time.h>
#include <llvm/TypeSymbolTable.h>

static inline bool is_c_sym(const string& name)
{
  return name=="main" || sys::DynamicLibrary::SearchForAddressOfSymbol(name);
}

static inline string mkvarsym(const string& name)
{
  bool have_c_sym = is_c_sym(name);
  if (have_c_sym)
    return "$$pure."+name;
  else
    return name;
}

static inline bool is_init(const string& name)
{
  return name.compare(0, 6, "$$init") == 0 &&
    name.find_first_not_of("0123456789", 6) == string::npos;
}

static inline bool is_tmpvar(const string& name, string& label)
{
  if (name.compare(0, 8, "$$tmpvar") == 0) {
    label = name.substr(8);
    size_t pos = label.find_last_not_of("0123456789");
    if (pos != string::npos) label.erase(pos+1);
    return true;
  } else
    return false;
}

static string& quote(string& s)
{
  size_t p = 0, q;
  while ((q = s.find_first_of(" \t", p)) != string::npos) {
    s.insert(q, 1, '\\');
    p = q+2;
  }
  return s;
}

#ifdef __MINGW32__
#include <process.h>
#define WIFEXITED(w)   (((w) & 0XFFFFFF00) == 0)
#define WEXITSTATUS(w) (w)
#endif

#define DEBUG_USED 0
#define DEBUG_UNUSED 0

/* LLVM >= 2.6 raw_ostream compatibility. This is a mess. */

// Use this to force the raw_ostream interface with LLVM <= 2.5.
//#define RAW_STREAM 1
#if !RAW_STREAM
#define RAW_STREAM LLVM26
#else
#ifndef HAVE_LLVM_SUPPORT_RAW_OSTREAM_H
// Only use raw_ostream if we have it.
#undef RAW_STREAM
#endif
#endif

#if RAW_STREAM
#if LLVM26
#define ostream_error(os) os.has_error()
#define ostream_clear_error(os) os.clear_error()
#else
// LLVM <= 2.5 doesn't have these methods.
#define ostream_error(os) (0)
#define ostream_clear_error(os) 
#endif
#else
// !RAW_STREAM: Use the simple old std::ostream interface.
#define ostream_error(os) os.fail()
#define ostream_clear_error(os) os.clear()
#endif

#if NEW_OSTREAM // LLVM >= 2.7 takes an enumeration as the last parameter
#define new_raw_fd_ostream(s,binary,msg) new llvm::raw_fd_ostream(s,msg,(binary)?llvm::raw_fd_ostream::F_Binary:0)
#else
#if LLVM26 // LLVM 2.6 takes two flags (Binary, Force)
#define new_raw_fd_ostream(s,binary,msg) new llvm::raw_fd_ostream(s,binary,1,msg)
#else // LLVM 2.5 and earlier only have the Binary flag
#define new_raw_fd_ostream(s,binary,msg) new llvm::raw_fd_ostream(s,binary,msg)
#endif
#endif

void interpreter::check_used(set<Function*>& used,
			     map<GlobalVariable*,Function*>& varmap)
{
  /* We start out by collecting all immediate dependencies where a function f
     calls or refers to another function g. Note that there are several ways
     that this can happen:

     - via a direct call (call site)

     - via a function pointer (constant)

     - via an indirect reference (global variable)

     Thus we need to analyze all uses of a function and its associated global
     variable. At the same time we also determine all the roots in the
     dependency graph (initialization code and what's in 'used' initially). */
  set<Function*> roots = used;
#if DEBUG_USED||DEBUG_UNUSED
  map<Function*, set<Function*> > callers;
#endif
  map<Function*, set<Function*> > callees;
  // Scan the global variable table for function pointers.
  for (map<int32_t,GlobalVar>::iterator it = globalvars.begin();
       it != globalvars.end(); ++it) {
    int32_t fno = it->first;
    GlobalVariable* v = it->second.v;
    Function *f = 0;
    map<int32_t,Env>::iterator jt = globalfuns.find(fno);
    if (jt != globalfuns.end()) {
      Env& e = jt->second;
      f = e.h;
    }
    if (!f) {
      map<int32_t,ExternInfo>::iterator kt = externals.find(fno);
      if (kt != externals.end()) {
	ExternInfo& info = kt->second;
	f = info.f;
      }
    }
    if (f) {
      varmap[v] = f;
      for (Value::use_iterator it = v->use_begin(), end = v->use_end();
	   it != end; it++) {
	if (Instruction *inst = dyn_cast<Instruction>(*it)) {
	  Function *g = inst->getParent()->getParent();
	  // Indirect reference through a variable. Note that we're not
	  // interested in loops in the dependency graph, so we check that the
	  // caller is different from the callee.
	  if (g && g != f) {
#if 0
	    std::cout << g->getNameStr() << " calls " << f->getNameStr()
		      << " via var " << v->getNameStr() << '\n';
#endif
#if DEBUG_USED||DEBUG_UNUSED
	    callers[f].insert(g);
#endif
	    callees[g].insert(f);
	  }
	}
      }
    }
  }
  // Next scan the table of all functions for direct uses.
  for (Module::iterator it = module->begin(), end = module->end();
       it != end; ++it) {
    Function *f = &*it;
    if (is_init(f->getName())) {
      // This is a root.
      roots.insert(f);
    } else if (f->hasNUsesOrMore(1)) {
      // Look for uses of the function.
      for (Value::use_iterator it = f->use_begin(), end = f->use_end(); 
	   it != end; it++) {
	if (Instruction *inst = dyn_cast<Instruction>(*it)) {
	  Function *g = inst->getParent()->getParent();
	  // This is a direct call.
	  if (g && g != f) {
#if 0
	    std::cout << g->getNameStr() << " calls " << f->getNameStr() << '\n';
#endif
#if DEBUG_USED||DEBUG_UNUSED
	    callers[f].insert(g);
#endif
	    callees[g].insert(f);
	  }
	} else if (Constant *c = dyn_cast<Constant>(*it)) {
	  // A function pointer. Check its uses.
	  for (Value::use_iterator jt = c->use_begin(), end = c->use_end(); 
	       jt != end; jt++) {
	    if (Instruction *inst = dyn_cast<Instruction>(*jt)) {
	      // This is a function that refers to f via a pointer.
	      Function *g = inst->getParent()->getParent();
	      if (g && g != f) {
#if 0
		std::cout << g->getNameStr() << " calls " << f->getNameStr()
			  << " via cst " << c->getNameStr() << '\n';
#endif
#if DEBUG_USED||DEBUG_UNUSED
		callers[f].insert(g);
#endif
		callees[g].insert(f);
	      }
	    }
	  }
	}
      }
    }
  }
  /* Next we determine the functions which can be reached from the roots of
     the dependency graph (transitive closure). */
  set<Function*> marked = roots;
  used = roots;
  while (!marked.empty()) {
    set<Function*> marked1;
    for (set<Function*>::iterator it = marked.begin(), end = marked.end();
	 it != end; it++) {
      Function *f = *it;
      for (set<Function*>::iterator jt = callees[f].begin(),
	     end = callees[f].end(); jt != end; jt++) {
	Function *g = *jt;
	if (used.find(g) == used.end()) {
	  marked1.insert(g);
	  used.insert(g);
	}
      }
    }
    marked = marked1;
  }
#if DEBUG_USED
  // Debugging: Print the list of all used functions on stdout.
  for (set<Function*>::iterator it = used.begin(), end = used.end();
       it != end; it++) {
    Function *f = *it;
    std::cout << "** used function: " << f->getNameStr() << " callers:";
    for (set<Function*>::iterator jt = callers[f].begin(),
	   end = callers[f].end(); jt != end; jt++) {
      Function *g = *jt;
      std::cout << " " << g->getNameStr();
    }
    std::cout << " callees:";
    for (set<Function*>::iterator jt = callees[f].begin(),
	   end = callees[f].end(); jt != end; jt++) {
      Function *g = *jt;
      std::cout << " " << g->getNameStr();
    }
    std::cout << '\n';
  }
#endif
#if DEBUG_UNUSED
  // Debugging: Print the list of all unused functions on stdout.
  for (Module::iterator it = module->begin(), end = module->end();
       it != end; ++it) {
    Function *f = &*it;
    if (used.find(f) == used.end()) {
      std::cout << "** unused function: " << f->getNameStr() << " callers:";
      for (set<Function*>::iterator jt = callers[f].begin(),
	     end = callers[f].end(); jt != end; jt++) {
	Function *g = *jt;
	std::cout << " " << g->getNameStr();
      }
      std::cout << " callees:";
      for (set<Function*>::iterator jt = callees[f].begin(),
	     end = callees[f].end(); jt != end; jt++) {
	Function *g = *jt;
	std::cout << " " << g->getNameStr();
      }
      std::cout << '\n';
    }
  }
#endif
}

int interpreter::compiler(string out, list<string> libnames)
{
  /* We allow either '-' or *.ll to indicate an LLVM assembler file. In the
     former case, output is written to stdout, which is useful if the output
     is to be processed by other LLVM utilities in a pipe. Other recognized
     extensions for the ouput file are .bc (LLVM bitcode) as well as .s and .o
     (native assembler and object code). In all other cases, output code is
     written to a temporary .bc file which is then passed to llc+opt+gcc to
     create an object file, which is linked with a minimal 'main' to create an
     executable. */
  out = unixize(out);
  string target = out;
  size_t p = target.find_last_of("/.");
  string ext = (p!=string::npos && target[p]=='.')?target.substr(p):"";
  bool file_target = target!="-", bc_target = ext==".bc";
  if (file_target && !bc_target && ext != ".ll") {
    target += ".bc"; bc_target = true;
  }
  /* Everything is already compiled at this point, so all we have to do here
     is to emit the code. We also prepare a main entry point, void
     __pure_main__ (int argc, char **argv), which initializes the interpreter
     so that a minimal runtime environment is available. This function is to
     be called by the main() or other initialization code of the standalone
     module. It takes two arguments, the argc and argv of the interpreter. */
  setlocale(LC_ALL, "C");
#if RAW_STREAM
  // As of LLVM 2.7 (svn), these need to be wrapped up in a raw_ostream.
  string error;
  // Note: raw_fd_ostream already handles "-".
  llvm::raw_ostream *codep = new_raw_fd_ostream(target.c_str(),bc_target,error);
  if (!error.empty()) {
    std::cerr << "Error opening " << target << '\n';
    exit(1);
  }
  llvm::raw_ostream &code = *codep;
#else
  std::ostream *codep =
    bc_target?
    new std::ofstream(target.c_str(), ios_base::out | ios_base::binary):
    file_target?
    new std::ofstream(target.c_str(), ios_base::out):
    &std::cout;
  std::ostream &code = *codep;
#endif
  if (!file_target) out = target = "<stdout>";
  if (ostream_error(code)) {
    std::cerr << "Error opening " << target << '\n';
    exit(1);
  }
  /* Verify the global variables. This includes the special $$sstk$$ (shadow
     stack) and $$fptr$$ (environment pointer) globals, as well as all the
     expression pointers for the global symbols of the Pure program. Here we
     do a first scan of the global variables, checking for any unresolvable
     constant values (wrapped pointers or local closures). */
  int nerrs = 0;
  for (Module::global_iterator it = module->global_begin(),
	 end = module->global_end(); it != end; ++it) {
    GlobalVariable &v = *it;
    if (!v.hasName()) continue;
    string label;
    if (is_tmpvar(v.getName(), label)) {
      // We can't handle these, sorry.
      std::cerr << "'" << label << "' is not a constant value" << '\n';
      nerrs++;
    }
  }
  if (nerrs > 0) {
    unlink(target.c_str());
    std::cerr << 
"** Your program contains one or more constants referring to run time\n\
data such as pointers and closures. Changing the offending constants\n\
to variables should fix this. **\n";
    exit(1);
  }
  // Set the module data layout and triple for the target. FIXME: Maybe we
  // should allow overriding these, but the user can also use the LLVM
  // toolchain to cross-compile for different architectures.
  string layout = JIT->getTargetData()->getStringRepresentation(),
    triple = HOST;
  module->setDataLayout(layout);
  module->setTargetTriple(triple);
  Function *initfun = module->getFunction("pure_interp_main");
  Function *freefun = module->getFunction("pure_freenew");
  // Eliminate unused functions.
  set<Function*> used = always_used; used.insert(initfun);
  map<GlobalVariable*,Function*> varmap;
  if (strip) check_used(used, varmap);
  // Remove unused globals.
  list<GlobalVariable*> var_to_be_deleted;
  for (Module::global_iterator it = module->global_begin(),
	 end = module->global_end(); it != end; ) {
    GlobalVariable &v = *(it++);
    if (!v.hasName()) {
      var_to_be_deleted.push_back(&v);
      continue;
    }
    map<GlobalVariable*,Function*>::iterator jt = varmap.find(&v);
    if (jt != varmap.end()) {
      // Strip variables holding unused function pointers.
      Function *f = jt->second;
      if (strip && used.find(f) == used.end())
	var_to_be_deleted.push_back(&v);
    }
  }
  // Remove unused functions.
  list<Function*> fun_to_be_deleted;
  for (Module::iterator it = module->begin(), end = module->end();
       it != end; ) {
    Function &f = *(it++);
    if (strip && used.find(&f) == used.end()) {
      f.dropAllReferences();
      fun_to_be_deleted.push_back(&f);
    }
  }
  for (list<GlobalVariable*>::iterator it = var_to_be_deleted.begin(),
	 end = var_to_be_deleted.end(); it != end; it++) {
    (*it)->eraseFromParent();
  }
  var_to_be_deleted.clear();
  for (list<Function*>::iterator it = fun_to_be_deleted.begin(),
	 end = fun_to_be_deleted.end(); it != end; it++) {
    (*it)->eraseFromParent();
  }
  fun_to_be_deleted.clear();
  // Finally build the main function with all the initialization code.
  vector<const Type*> argt;
  argt.push_back(int32_type());
  argt.push_back(VoidPtrTy);
  FunctionType *ft = FunctionType::get(void_type(), argt, false);
  Function *main = Function::Create(ft, Function::ExternalLinkage,
				    "__pure_main__", module);
  BasicBlock *bb = basic_block("entry", main);
#ifdef LLVM26
  Builder b(llvm::getGlobalContext());
#else
  Builder b;
#endif
  b.SetInsertPoint(bb);
  /* To make at least simple evals work, we have to dump the information in
     the symbol and external tables so that they can be reconstructed at
     runtime. We collect this information as a string, one line per symbol and
     external table entry. The two sections are separated by '%%' on a line by
     itself. The string data is hard-coded into the binary module. */
  string dump;
  symtab.dump(dump);
  // Create a number of other tables which hold the pointers to global
  // variables, corresponding Pure functions and their arities, and the
  // pointers to Pure wrappers for externals.
  int32_t n = symtab.nsyms();
  vector<Constant*> vars(n+1, NullExprPtrPtr);
  vector<Constant*> vals(n+1, NullPtr);
  vector<Constant*> arity(n+1, Zero);
  vector<Constant*> externs(n+1, NullPtr);
  // Populate the tables.
  ostringstream sout;
  sout << "%%\n";
  Value *idx[2] = { Zero, Zero };
  for (map<int32_t,GlobalVar>::iterator it = globalvars.begin();
       it != globalvars.end(); ++it) {
    int32_t f = it->first;
    GlobalVar& v = it->second;
    if (v.v && v.x) {
      map<int32_t,Env>::iterator jt = globalfuns.find(f);
      if (jt != globalfuns.end()) {
	Env& e = jt->second;
	if (!strip || used.find(e.h) != used.end()) {
	  vals[f] = ConstantExpr::getPointerCast(e.h, VoidPtrTy);
	  arity[f] = SInt(e.n);
	  vars[f] = v.v;
	}
      } else {
	map<int32_t,ExternInfo>::iterator kt = externals.find(f);
	if (kt != externals.end()) {
	  ExternInfo& info = kt->second;
	  if (!strip || used.find(info.f) != used.end()) {
	    externs[f] = ConstantExpr::getPointerCast(info.f, VoidPtrTy);
	    sout << info.tag << " " << info.name << " " << type_name(info.type)
		 << " " << info.argtypes.size();
	    for (size_t i = 0; i < info.argtypes.size(); i++)
	      sout << " " << type_name(info.argtypes[i]);
	    sout << '\n';
	    vars[f] = v.v;
	  }
	} else
	  vars[f] = v.v;
      }
    }
  }
  dump += sout.str();
  // Dump the symbol and external tables.
  const char *dump_s = dump.c_str();
  GlobalVariable *syms = global_variable
    (module, ArrayType::get(int8_type(), strlen(dump_s)+1), true,
     GlobalVariable::InternalLinkage, constant_char_array(dump_s),
     "$$syms$$");
  // This holds the pointers to the global variables
  GlobalVariable *vvars = global_variable
    (module, ArrayType::get(ExprPtrPtrTy, n+1), true,
     GlobalVariable::InternalLinkage,
     ConstantArray::get(ArrayType::get(ExprPtrPtrTy, n+1), vars),
     "$$vars$$");
  // This holds all the corresponding values. Each value is a pointer which
  // can either be null (indicating an unbound symbol), or a pointer to a
  // function (indicating a global named function).
  GlobalVariable *vvals = global_variable
    (module, ArrayType::get(VoidPtrTy, n+1), true,
     GlobalVariable::InternalLinkage,
     ConstantArray::get(ArrayType::get(VoidPtrTy, n+1), vals),
     "$$vals$$");
  // This holds all the arities of the functions.
  GlobalVariable *varity = global_variable
    (module, ArrayType::get(int32_type(), n+1), true,
     GlobalVariable::InternalLinkage,
     ConstantArray::get(ArrayType::get(int32_type(), n+1), arity),
     "$$arity$$");
  // This holds all the externals.
  GlobalVariable *vexterns = global_variable
    (module, ArrayType::get(VoidPtrTy, n+1), true,
     GlobalVariable::InternalLinkage,
     ConstantArray::get(ArrayType::get(VoidPtrTy, n+1), externs),
     "$$externs$$");
  // Call pure_interp_main() in the runtime to create an interpreter instance.
  vector<Value*> args;
  Function::arg_iterator a = main->arg_begin();
  idx[1] = Zero;
  args.push_back(a++);
  args.push_back(a++);
  args.push_back(SInt(n));
  args.push_back(b.CreateGEP(syms, idx, idx+2));
  args.push_back(b.CreateBitCast(b.CreateGEP(vvars, idx, idx+2), VoidPtrTy));
  args.push_back(b.CreateBitCast(b.CreateGEP(vvals, idx, idx+2), VoidPtrTy));
  args.push_back(b.CreateGEP(varity, idx, idx+2));
  args.push_back(b.CreateBitCast(b.CreateGEP(vexterns, idx, idx+2),
				 VoidPtrTy));
  args.push_back(b.CreateBitCast(sstkvar, VoidPtrTy));
  args.push_back(b.CreateBitCast(fptrvar, VoidPtrTy));
  b.CreateCall(initfun, args.begin(), args.end());
  // Execute the initialization code of the Pure program (global expressions
  // and variable definitions).
  for (Module::iterator it = module->begin(), end = module->end();
       it != end; ++it) {
    Function *f = &*it;
    if (f != main && is_init(f->getName())) {
      CallInst* v = b.CreateCall(f);
      b.CreateCall(freefun, v);
    }
  }
  b.CreateRet(0);
  // Emit output code (either LLVM assembler or bitcode).
  if (bc_target) {
    WriteBitcodeToFile(module, code);
  } else {
    // Print a module header showing some useful information.
    time_t t; time(&t);
    code << "; Generated by Pure " << PACKAGE_VERSION << " (LLVM "
	 << LLVM_VERSION << ") " << ctime(&t);
    module->print(code, 0);
  }
  if (ostream_error(code)) {
    std::cerr << "Error writing " << target << '\n';
    exit(1);
  }
  ostream_clear_error(code);
#if RAW_STREAM
  delete codep;
#else
  if (codep != &std::cout) delete codep;
#endif
  // Compile and link, if requested.
  if (target != out) {
    assert(bc_target);
    bool vflag = (verbose&verbosity::compiler) != 0;
    string libs;
    set<string> libset;
    for (list<string>::iterator it = loaded_libs.begin();
	 it != loaded_libs.end(); ++it) {
      if (libset.find(*it) == libset.end()) {
	libs += " "+quote(*it);
	libset.insert(*it);
      }
    }
    for (list<string>::iterator it = libnames.begin();
	 it != libnames.end(); ++it)
      libs += " -l"+quote(*it);
    /* Call llc (and opt) to create a native assembler file which can then be
       passed to gcc to handle assembly and linkage (if requested). */
    string asmfile = (ext==".s")?out:out+".s";
    string cmd = "opt -f -std-compile-opts "+quote(target)+
      " | llc -f "+string(pic?"-relocation-model=pic ":"")+
      "-o "+quote(asmfile);
    if (vflag) std::cerr << cmd << '\n';
    int status = system(cmd.c_str());
    unlink(target.c_str());
    if (WIFEXITED(status) && WEXITSTATUS(status) == 0 && ext!=".s") {
      // Assemble.
      string obj = (ext==".o")?out:out+".o";
      cmd = "gcc -c "+quote(asmfile)+" -o "+quote(obj);
      if (vflag) std::cerr << cmd << '\n';
      status = system(cmd.c_str());
      unlink(asmfile.c_str());
      if (WIFEXITED(status) && WEXITSTATUS(status) == 0) {
	// Link.
	string linkopts = quote(obj)+libs+
#ifdef __MINGW32__
	  /* Link some extra libs and beef up the stack size on Windows. */
	  " -Wl,--stack=0x800000 -lregex -lglob -lreadline"+
#endif
	  " -lpure";
	if (ext != ".o") {
	  // Link.
	  cmd = "g++ -o "+quote(out)+" "+quote(libdir)+"pure_main.o "+linkopts;
	  if (vflag) std::cerr << cmd << '\n';
	  status = system(cmd.c_str());
	  unlink(obj.c_str());
	} else if (vflag)
	  std::cerr << "Link with: g++ " << linkopts << '\n';
      }
    }
    if (WIFEXITED(status)) status = WEXITSTATUS(status);
    return status;
  } else
    return 0;
}

void interpreter::defn(const char *varname, pure_expr *x)
{
  symbol& sym = symtab.checksym(varname);
  defn(sym.f, x);
}

void interpreter::defn(int32_t tag, pure_expr *x)
{
  assert(tag > 0);
  globals g;
  save_globals(g);
  symbol& sym = symtab.sym(tag);
  env::const_iterator jt = globenv.find(tag), kt = macenv.find(tag);
  if (kt != macenv.end()) {
    restore_globals(g);
    throw err("symbol '"+sym.s+"' is already defined as a macro");
  } else if (jt != globenv.end() && jt->second.t == env_info::cvar) {
    restore_globals(g);
    throw err("symbol '"+sym.s+"' is already defined as a constant");
  } else if (jt != globenv.end() && jt->second.t == env_info::fun) {
    restore_globals(g);
    throw err("symbol '"+sym.s+"' is already defined as a function");
  } else if (externals.find(tag) != externals.end()) {
    restore_globals(g);
    throw err("symbol '"+sym.s+
	      "' is already declared as an extern function");
  }
  GlobalVar& v = globalvars[tag];
  if (!v.v) {
    if (sym.priv)
      v.v = global_variable
	(module, ExprPtrTy, false, GlobalVariable::InternalLinkage,
	 NullExprPtr, "$$private."+sym.s);
    else
      v.v = global_variable
	(module, ExprPtrTy, false, GlobalVariable::ExternalLinkage,
	 NullExprPtr, mkvarsym(sym.s));
    JIT->addGlobalMapping(v.v, &v.x);
  }
  if (v.x) pure_free(v.x); v.x = pure_new(x);
  globenv[tag] = env_info(&v.x, temp);
  restore_globals(g);
}

ostream &operator<< (ostream& os, const ExternInfo& info)
{
  interpreter& interp = *interpreter::g_interp;
  assert(info.tag > 0);
  const symbol& sym = interp.symtab.sym(info.tag);
  os << "extern " << interp.type_name(info.type) << " " << info.name << "(";
  size_t n = info.argtypes.size();
  for (size_t i = 0; i < n; i++) {
    if (i > 0) os << ", ";
    os << interp.type_name(info.argtypes[i]);
  }
  os << ")";
  if (sym.s != info.name) os << " = " << sym.s;
  return os;
}

FMap& FMap::operator= (const FMap& f)
{
  clear(); m.resize(f.m.size());
  for (size_t i = 0, n = f.m.size(); i < n; i++)
    m[i] = new EnvMap(*f.m[i]);
  root = f.root; pred = f.pred; succ = f.succ;
  idx = f.idx; lastidx = f.lastidx;
  return *this;
}

void FMap::clear()
{
  set<Env*> e;
  for (size_t i = 0, n = m.size(); i < n; i++) {
    for (EnvMap::iterator it = m[i]->begin(), end = m[i]->end();
	 it != end; it++)
      e.insert(it->second);
    delete m[i];
  }
  for (set<Env*>::iterator it = e.begin(), end = e.end();
       it != end; it++)
    delete *it;
  m.clear(); root.clear(); pred.clear(); succ.clear();
  idx = 0; lastidx = -1;
}

void FMap::first()
{
  idx = 0; lastidx = -1;
  // reset child environments
  set<Env*> e;
  for (size_t i = 0, n = m.size(); i < n; i++)
    for (EnvMap::iterator it = m[i]->begin(), end = m[i]->end();
	 it != end; it++)
      e.insert(it->second);
  for (set<Env*>::iterator it = e.begin(), end = e.end();
       it != end; it++)
    (*it)->fmap.first();
}

void FMap::next()
{
  assert(pred[idx] < 0);
  if (succ[idx] >= 0)
    idx = succ[idx];
  else {
    // create a new root
    size_t n = root.size(), k = m.size();
    root.resize(n+1);
    m.resize(k+1); pred.resize(k+1); succ.resize(k+1);
    root[n] = succ[idx] = k;
    pred[k] = succ[k] = -1;
    m[k] = new EnvMap;
    idx = k;
  }
  lastidx = -1;
}

void FMap::select(size_t n)
{
  if (root.size() > 1) {
    assert(n < root.size());
    idx = root[n];
  } else
    idx = 0;
  lastidx = -1;
}

void FMap::push()
{
  if (lastidx >= 0) {
    assert(pred[lastidx] == idx);
    if (succ[lastidx] >= 0)
      idx = succ[lastidx];
    else {
      // create a new child, link from the previous one
      size_t k = m.size();
      m.resize(k+1); pred.resize(k+1); succ.resize(k+1);
      pred[k] = idx; succ[k] = -1;
      m[k] = new EnvMap(*m[idx]);
      succ[lastidx] = k;
      idx = k;
    }
  } else if (++idx >= (int32_t)m.size()) {
    // the first child always immediately follows its parent
    assert(idx == (int32_t)m.size());
    m.resize(idx+1); pred.resize(idx+1); succ.resize(idx+1);
    pred[idx] = idx-1; succ[idx] = -1;
    m[idx] = new EnvMap(*m[idx-1]);
  }
  lastidx = -1;
}

void FMap::pop()
{
  assert(pred[idx] >= 0);
  lastidx = idx; idx = pred[idx];
}

Env& Env::operator= (const Env& e)
{
  if (f) {
    // already initialized; we only allow this for global function definitions
    assert(!local && !parent && e.n == n && e.tag == tag && b == e.b &&
	   !e.local && !e.parent);
    clear();
  } else {
    // uninitialized environment; simply copy everything
    tag = e.tag; name = e.name; n = e.n; f = e.f; h = e.h; fp = e.fp;
    args = e.args; envs = e.envs;
    b = e.b; local = e.local; parent = e.parent;
  }
  fmap = e.fmap; xmap = e.xmap; xtab = e.xtab; prop = e.prop; m = e.m;
  if (e.descr) descr = e.descr;
  key = e.key;
  return *this;
}

void Env::clear()
{
  static list<Function*> to_be_deleted;
  if (!f) return; // not initialized
  if (rp) delete rp;
  interpreter& interp = *interpreter::g_interp;
  if (local) {
    // purge local functions
#if DEBUG>2
    std::cerr << "clearing local '" << name << "'\n";
#endif
    if (h != f) interp.JIT->freeMachineCodeForFunction(h);
    interp.JIT->freeMachineCodeForFunction(f);
    f->dropAllReferences(); if (h != f) h->dropAllReferences();
    fp = 0;
    fmap.clear();
    to_be_deleted.push_back(f); if (h != f) to_be_deleted.push_back(h);
  } else {
#if DEBUG>2
    std::cerr << "clearing global '" << name << "'\n";
#endif
    // The code of anonymous globals (doeval, dodefn) is taken care of
    // elsewhere, we must not collect that here.
    if (!is_init(name)) {
      // named global, get rid of the machine code
      if (h != f) interp.JIT->freeMachineCodeForFunction(h);
      interp.JIT->freeMachineCodeForFunction(f);
      // only delete the body, this keeps existing references intact
      f->deleteBody();
    }
    fp = 0;
    // delete all nested environments and reinitialize other body-related data
    fmap.clear(); xmap.clear(); xtab.clear(); prop.clear(); m = 0;
    // now that all references have been removed, delete the function pointers
    for (list<Function*>::iterator fi = to_be_deleted.begin();
	 fi != to_be_deleted.end(); fi++) {
      (*fi)->eraseFromParent();
    }
    to_be_deleted.clear();
  }
}

CallInst *Env::CreateCall(Function *f, const vector<Value*>& args)
{
#if DEBUG
  // do some sanity checks
  bool ok = true;
  Function::arg_iterator a = f->arg_begin();
  vector<Value*>::const_iterator b = args.begin();
  for (size_t i = 0; a != f->arg_end() && b != args.end(); i++, a++, b++) {
    Value* c = *b;
    if (a->getType() != c->getType()) {
      std::cerr << "** argument mismatch!\n";
      std::cerr << "function parameter #" << i << ": "; a->dump();
      std::cerr << "provided argument  #" << i << ": "; c->dump();
      ok = false;
    }
  }
  if (ok && a != f->arg_end()) {
    std::cerr << "** missing arguments!\n";
    ok = false;
  }
  if (ok && b != args.end() && !f->isVarArg()) {
    std::cerr << "** extra arguments!\n";
    ok = false;
  }
  if (!ok) {
    std::cerr << "** calling function: " << f->getNameStr() << '\n';
    f->dump();
    assert(0 && "bad function call");
  }
#endif
  CallInst* v = builder.CreateCall(f, args.begin(), args.end());
  v->setCallingConv(f->getCallingConv());
  return v;
}

ReturnInst *Env::CreateRet(Value *v, const rule *rp)
{
  interpreter& interp = *interpreter::g_interp;
  if (rp) interp.debug_redn(rp, v);
  ReturnInst *ret = builder.CreateRet(v);
  Instruction *pi = ret;
  Function *free_fun = interp.module->getFunction("pure_pop_args");
  Function *free1_fun = interp.module->getFunction("pure_pop_arg");
  if (!rp && isa<CallInst>(v)) {
    CallInst* c = cast<CallInst>(v);
    // Check whether the call is actually subject to tail call elimination (as
    // determined by the calling convention).
    if (c->getCallingConv() == CallingConv::Fast) {
      c->setTailCall();
      // Check for a tail call situation (previous instruction must be a call
      // to pure_push_args()).
      BasicBlock::iterator it(c);
      if (it != c->getParent()->begin()) {
	--it;
	if (isa<CallInst>(it)) {
	  CallInst* c1 = cast<CallInst>(it);
	  if (c1->getCalledFunction() ==
	      interp.module->getFunction("pure_push_arg")) {
	    free_fun = interp.module->getFunction("pure_pop_tail_args");
	    free1_fun = interp.module->getFunction("pure_pop_tail_arg");
	  } else if (c1->getCalledFunction() ==
		     interp.module->getFunction("pure_push_args")) {
	    free_fun = interp.module->getFunction("pure_pop_tail_args");
	    free1_fun = interp.module->getFunction("pure_pop_tail_arg");
	    /* Patch up this call to correct the offset of the environment. */
#ifdef LLVM26
	    CallInst *c2 = c1->clone(llvm::getGlobalContext());
#else
	    CallInst *c2 = c1->clone();
#endif
	    c1->getParent()->getInstList().insert(c1, c2);
#ifdef NEW_BUILDER
	    Value *v = BinaryOperator::CreateSub(c2, UInt(n+m+1), "", c1);
#else
	    Value *v = BinaryOperator::createSub(c2, UInt(n+m+1), "", c1);
#endif
	    BasicBlock::iterator ii(c1);
	    ReplaceInstWithValue(c1->getParent()->getInstList(), ii, v);
	  }
	}
      }
      pi = c;
    }
  }
  // We must garbage-collect args and environment here, immediately before the
  // call (if any), or the return instruction otherwise.
  if (n == 1 && m == 0) {
    vector<Value*> myargs;
    if (pi == ret)
      myargs.push_back(v);
    else
      myargs.push_back(ConstantPointerNull::get(interp.ExprPtrTy));
    CallInst::Create(free1_fun, myargs.begin(), myargs.end(), "", pi);
  } else if (n+m != 0) {
    vector<Value*> myargs;
    if (pi == ret)
      myargs.push_back(v);
    else
      myargs.push_back(ConstantPointerNull::get(interp.ExprPtrTy));
    myargs.push_back(UInt(n));
    myargs.push_back(UInt(m));
    CallInst::Create(free_fun, myargs.begin(), myargs.end(), "", pi);
  }
  return ret;
}

// XXXFIXME: this should be TLD
uint32_t Env::act_key = 0;
EnvStack Env::envstk;
set<Env*> Env::props;

void Env::push(const char *msg)
{
  envstk.push_front(this);
#if DEBUG>1
  interpreter& interp = *interpreter::g_interp;
  std::cerr << "push (" << msg << ") " << this << " -> "
	    << (tag>0?interp.symtab.sym(tag).s:"<<anonymous>>")
	    << '\n';
#endif
}

void Env::pop()
{
  assert(envstk.front() == this);
  envstk.pop_front();
#if DEBUG>1
  interpreter& interp = *interpreter::g_interp;
  std::cerr << "pop " << this << " -> "
	    << (tag>0?interp.symtab.sym(tag).s:"<<anonymous>>")
	    << '\n';
#endif
}

void Env::build_map(expr x)
{
  // build the maps for a (rhs) expression
  assert(!x.is_null());
  switch (x.tag()) {
  case EXPR::FVAR: {
    // Function call site; if it's a local function, we'll have to propogate
    // the environment of that function to the current scope.
    assert(x.vtag() > 0);
    if (x.vidx() == 0 || x.vtag() == tag) break;
    Env *fenv = this;
    // First locate the function environment; the de Bruijn index tells us
    // where it's at.
    EnvStack::iterator ei = envstk.begin();
    uint32_t i = x.vidx();
    while (i-- > 0) {
      assert(ei != envstk.end());
      fenv = *ei++;
    }
    assert(fenv->fmap.act().find(x.vtag()) != fenv->fmap.act().end());
    fenv = fenv->fmap.act()[x.vtag()];
    if (!fenv->local) break;
    // fenv now points to the environment of the (local) function
    assert(fenv != this && fenv->tag == x.vtag());
    // As the map of the function environment might not be instantiated yet,
    // just leave a propagation link there for now, it will be processed
    // later.
    if (fenv->prop.find(this) == fenv->prop.end()) {
#if DEBUG>1
    {
      interpreter& interp = *interpreter::g_interp;
      string name = tag>0?interp.symtab.sym(tag).s:"<<anonymous>>";
      EnvStack::iterator ei = envstk.begin();
      while (ei != envstk.end()) {
	Env *fenv = *ei++;
	name = (fenv->tag>0?interp.symtab.sym(fenv->tag).s:"<<anonymous>>") +
	  "." + name;
      }
      std::cerr << name << ": call " << interp.symtab.sym(x.vtag()).s
		<< ":" << (unsigned)x.vidx() << '\n';
    }
#endif
      fenv->prop[this] = x.vidx();
      props.insert(fenv);
    } else
      assert(fenv->prop[this] == x.vidx());
    break;
  }
  case EXPR::VAR:
    if (x.vidx() > 0) {
      // reference to a local variable outside the current environment, which
      // needs to be captured when building a closure, cf. fbox()
      int32_t tag = x.vtag();
      uint8_t idx = x.vidx();
      path p = x.vpath();
      map<xmap_key,uint32_t>::const_iterator e = xmap.find(xmap_key(tag, idx));
      if (e == xmap.end()) {
	uint32_t v = m++;
	xmap[xmap_key(tag, idx)] = v;
	// record the new local and its properties, so that we can generate
	// code to capture the variable when boxing the function
	xtab.push_back(VarInfo(v, tag, idx, p));
#if DEBUG
      } else {
	// sanity checking; the idx and path of each given environment
	// reference should be uniquely determined by the variable symbol
	uint32_t v = e->second;
	list<VarInfo>::const_iterator info = xtab.begin();
	for (; v > 0; --v) ++info;
	assert(tag == info->vtag && idx == info->idx && p == info->p);
#endif
      }
    }
    break;
  case EXPR::MATRIX:
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++)
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	build_map(*ys);
      }
    break;
  case EXPR::APP: {
    expr f; uint32_t n = count_args(x, f);
    interpreter& interp = *interpreter::g_interp;
    exprl xs;
    expr tl;
    if (n == 1 && f.tag() == interp.symtab.amp_sym().f) {
      expr y = x.xval2();
      push("&");
      Env* eptr = fmap.act()[-x.hash()] = new Env(0, 0, 0, y, true, true);
      Env& e = *eptr;
      e.build_map(y); e.promote_map();
      pop();
    } else if (n == 2 && f.tag() == interp.symtab.catch_sym().f) {
      expr h = x.xval1().xval2(), y = x.xval2();
      push("catch");
      Env* eptr = fmap.act()[-x.hash()] = new Env(0, 0, 0, y, true, true);
      Env& e = *eptr;
      e.build_map(y); e.promote_map();
      pop();
      build_map(h);
    } else if (x.is_list2(xs, tl)) {
      /* Optimize the list case so that we don't run out of stack here. */
      for (exprl::iterator it = xs.begin(), end = xs.end(); it != end; it++)
	build_map(*it);
      build_map(tl);
    } else if (x.is_tuple(xs)) {
      /* Optimize the tuple case so that we don't run out of stack here. */
      for (exprl::iterator it = xs.begin(), end = xs.end(); it != end; it++)
	build_map(*it);
    } else {
      build_map(x.xval1());
      build_map(x.xval2());
    }
    break;
  }
  case EXPR::COND:
    build_map(x.xval1());
    build_map(x.xval2());
    build_map(x.xval3());
    break;
  case EXPR::COND1:
    build_map(x.xval1());
    build_map(x.xval2());
    break;
  case EXPR::LAMBDA: {
    push("lambda");
    Env* eptr = fmap.act()[-x.hash()] =
      new Env(0, 0, 1, x.xval2(), true, true);
    Env& e = *eptr;
    e.build_map(x.xval2()); e.promote_map();
    pop();
    break;
  }
  case EXPR::CASE: {
    push("case");
    Env* eptr = fmap.act()[-x.hash()] =
      new Env(0, "case", 1, x.xval(), true, true);
    Env& e = *eptr;
    e.build_map(*x.rules()); e.promote_map();
    pop();
    build_map(x.xval());
    break;
  }
  case EXPR::WHEN:
    assert(!x.rules()->empty());
    build_map(x.xval(), x.rules()->begin(), x.rules()->end());
    break;
  case EXPR::WITH: {
    env *fe = x.fenv();
    fmap.push();
    push("with");
    // First enter all the environments into the fmap.
    for (env::const_iterator p = fe->begin(); p != fe->end(); p++) {
      int32_t ftag = p->first;
      const env_info& info = p->second;
      fmap.act()[ftag] = new Env(ftag, info, false, true);
    }
    // Now recursively build the maps for the child environments.
    for (env::const_iterator p = fe->begin(); p != fe->end(); p++) {
      int32_t ftag = p->first;
      const env_info& info = p->second;
      Env& e = *fmap.act()[ftag];
      e.build_map(info); e.promote_map();
    }
    pop();
    build_map(x.xval());
    fmap.pop();
    break;
  }
  default:
    break;
  }
}

void Env::build_map(expr x, rulel::const_iterator r, rulel::const_iterator end)
{
  // build the maps for a 'when' expression (cf. when_codegen())
  // x = subject expression to be evaluated in the context of the bindings
  // r = current pattern binding rule
  // end = end of rule list
  if (r == end)
    build_map(x);
  else {
    rulel::const_iterator s = r;
    expr y = (++s == end)?x:s->rhs;
    push("when");
    Env* eptr = fmap.act()[-y.hash()] = new Env(0, "when", 1, y, true, true);
    Env& e = *eptr;
    e.build_map(x, s, end); e.promote_map();
    pop();
    build_map(r->rhs);
  }
}

void Env::build_map(const rulel& rl)
{
  // build the maps for the rh sides in a 'case' expression
  for (rulel::const_iterator r = rl.begin(); r != rl.end(); r++) {
    build_map(r->rhs);
    if (!r->qual.is_null()) build_map(r->qual);
  }
}

void Env::build_map(const env_info& info)
{
  // build the maps for a global function definition
  assert(info.t == env_info::fun);
  // we need a separate submap for each rule
  rulel::const_iterator r = info.rules->begin(), end = info.rules->end();
  while (r != end) {
    build_map(r->rhs);
    if (!r->qual.is_null()) build_map(r->qual);
    if (++r != end) fmap.next();
  }
#if DEBUG>1
  if (!local) print_map(std::cerr, this);
#endif
}

void Env::promote_map()
{
  for (list<VarInfo>::iterator x = xtab.begin(); x != xtab.end(); x++) {
    // promote a non-local reference to outer environments
    VarInfo& info = *x;
    Env *f = parent;
    uint32_t i = info.idx;
    assert(i > 0);
    for (; --i > 0; f = f->parent) {
      assert(f);
      assert(f->local);
#if DEBUG>1
      {
	interpreter& interp = *interpreter::g_interp;
	assert(info.vtag>0);
	std::cerr << "promoting " << interp.symtab.sym(info.vtag).s
		  << " (idx " << (uint32_t)info.idx << ")"
		  << " from "
		  << (tag>0?interp.symtab.sym(tag).s:"<<anonymous>>")
		  << " (offset " << info.idx-i << ") "
		  << " to "
		  << (f->tag>0?interp.symtab.sym(f->tag).s:"<<anonymous>>")
		  << '\n';
      }
#endif
      int32_t tag = info.vtag;
      uint8_t idx = i;
      path p = info.p;
      map<xmap_key,uint32_t>::const_iterator e =
	f->xmap.find(xmap_key(tag, idx));
      if (e == f->xmap.end()) {
	uint32_t v = f->m++;
	f->xmap[xmap_key(tag, idx)] = v;
	f->xtab.push_back(VarInfo(v, tag, idx, p));
#if DEBUG>0
      } else {
	uint32_t v = e->second;
	list<VarInfo>::const_iterator info = f->xtab.begin();
	for (; v > 0; --v) ++info;
	assert(tag == info->vtag && idx == info->idx && p == info->p);
#endif
      }
    }
  }
}

void Env::propagate_maps()
{
  size_t count = props.size();
  // Repeatedly pass over the props set to propagate environments until no
  // more changes happened in the previous pass.
  while (count > 0) {
    count = 0;
    for (set<Env*>::iterator e = props.begin(); e != props.end(); e++)
      count += (*e)->propagate_map();
  }
  props.clear();
}

size_t Env::propagate_map()
{
  if (prop.empty() || xtab.empty()) return 0;
  size_t total = 0;
  map<Env*,uint8_t>::iterator ep = prop.begin();
  for (; ep != prop.end(); ep++) {
    Env& e = *ep->first;
    uint8_t idx = ep->second;
    assert(idx>0);
    uint32_t offs = idx-1;
    // Promote the environment of a local function to a call site, and from
    // there to the call site's parents.
    size_t count = 0;
    for (list<VarInfo>::iterator x = xtab.begin(); x != xtab.end(); x++) {
      VarInfo& info = *x;
#if DEBUG>1
      {
	interpreter& interp = *interpreter::g_interp;
	assert(info.vtag>0);
	std::cerr << "propagating " << interp.symtab.sym(info.vtag).s
		  << " (idx " << (uint32_t)info.idx << ")"
		  << " from "
		  << (tag>0?interp.symtab.sym(tag).s:"<<anonymous>>")
		  << " (offset " << offs << ") "
		  << " to "
		  << (e.tag>0?interp.symtab.sym(e.tag).s:"<<anonymous>>")
		  << '\n';
      }
#endif
      int32_t tag = info.vtag;
      uint8_t idx = info.idx+offs;
      path p = info.p;
      map<xmap_key,uint32_t>::const_iterator ei =
	e.xmap.find(xmap_key(tag, idx));
      if (ei == e.xmap.end()) {
	uint32_t v = e.m++;
	e.xmap[xmap_key(tag, idx)] = v;
	e.xtab.push_back(VarInfo(v, tag, idx, p));
	count++;
#if DEBUG>0
      } else {
	uint32_t v = ei->second;
	list<VarInfo>::const_iterator info = e.xtab.begin();
	for (; v > 0; --v) ++info;
	assert(tag == info->vtag && idx == info->idx && p == info->p);
#endif
      }
    }
    if (count>0) e.promote_map();
#if DEBUG>1
    {
      Env *f = &e;
      while (f->parent) f = f->parent;
      assert(!f->local);
      print_map(std::cerr, f);
    }
#endif
    total += count;
  }
  return total;
}

void interpreter::push(const char *msg, Env *e)
{
  envstk.push_front(e);
#if DEBUG>1
  interpreter& interp = *interpreter::g_interp;
  std::cerr << "push (" << msg << ") " << e << " -> "
	    << (e->tag>0?interp.symtab.sym(e->tag).s:"<<anonymous>>")
	    << '\n';
#endif
}

void interpreter::pop(Env *e)
{
  assert(envstk.front() == e);
  envstk.pop_front();
#if DEBUG>1
  interpreter& interp = *interpreter::g_interp;
  std::cerr << "pop (" << e << ") -> "
	    << (e->tag>0?interp.symtab.sym(e->tag).s:"<<anonymous>>")
	    << '\n';
#endif
}

Env *interpreter::find_stacked(int32_t tag)
{
  list<Env*>::iterator e = envstk.begin();
  while (e != envstk.end() && (*e)->tag != tag) ++e;
  if (e != envstk.end()) {
    assert((*e)->tag == tag);
    return *e;
  } else
    return 0;
}

const Type *interpreter::named_type(string name)
{
  if (name == "void")
    return void_type();
  else if (name == "bool")
    return int1_type();
  else if (name == "char" || name == "int8")
    return int8_type();
  else if (name == "short" || name == "int16")
    return int16_type();
  else if (name == "int" || name == "int32")
    return int32_type();
  else if (name == "int64")
    return int64_type();
  else if (name == "long")
#if SIZEOF_LONG==4
    return int32_type();
#else
#if SIZEOF_LONG!=8
#error "Unknown size of long type."
#endif
    return int64_type();
#endif
  else if (name == "size_t")
#if SIZEOF_SIZE_T==4
    return int32_type();
#else
#if SIZEOF_SIZE_T!=8
#error "Unknown size of size_t type."
#endif
    return int64_type();
#endif
  else if (name == "float")
    return float_type();
  else if (name == "double")
    return double_type();
  else if (name == "char*" || name == "int8*")
    return CharPtrTy;
  else if (name == "short*" || name == "int16*")
    return PointerType::get(int16_type(), 0);
  else if (name == "int*" || name == "int32*")
    return PointerType::get(int32_type(), 0);
  else if (name == "int64*")
    return PointerType::get(int64_type(), 0);
  else if (name == "long*")
#if SIZEOF_LONG==4
    return PointerType::get(int32_type(), 0);
#else
    return PointerType::get(int64_type(), 0);
#endif
  else if (name == "size_t*")
#if SIZEOF_SIZE_T==4
    return PointerType::get(int32_type(), 0);
#else
    return PointerType::get(int64_type(), 0);
#endif
  else if (name == "float*")
    return PointerType::get(float_type(), 0);
  else if (name == "double*")
    return PointerType::get(double_type(), 0);
  else if (name == "expr*")
    return ExprPtrTy;
  else if (name == "expr**")
    return ExprPtrPtrTy;
  else if (name == "matrix*")
    return GSLMatrixPtrTy;
  else if (name == "dmatrix*")
    return GSLDoubleMatrixPtrTy;
  else if (name == "cmatrix*")
    return GSLComplexMatrixPtrTy;
  else if (name == "imatrix*")
    return GSLIntMatrixPtrTy;
  else if (name == "void*")
    return VoidPtrTy;
  else if (name.size() > 0 && name[name.size()-1] == '*')
    // all other pointer types effectively treated as void*
    return VoidPtrTy;
  else
    throw err("unknown C type '"+name+"'");
}

const char *interpreter::type_name(const Type *type)
{
  if (type == void_type())
    return "void";
  else if (type == int1_type())
    return "bool";
  else if (type == int8_type())
    return "char";
  else if (type == int16_type())
    return "short";
  else if (type == int32_type())
    /* We render this type as 'int' here, which should be the right thing in
       most cases. Unfortunately, if we are on a 32 bit system. we have no way
       of knowing whether the type was originally specified as 'long' instead,
       but time will hopefully remedy this issue. */
    return "int";
  else if (type == int64_type())
#if SIZEOF_LONG==8
    /* We render this type as 'long' here, which should be the right thing in
       most cases. Again, we have no way of knowing whether the type was
       originally specified as 'int64' instead, but time will hopefully remedy
       this issue. */
    return "long";
#else
    return "int64";
#endif
  else if (type == float_type())
    return "float";
  else if (type == double_type())
    return "double";
  else if (type == CharPtrTy)
    return "char*";
  else if (type == PointerType::get(int16_type(), 0))
    return "short*";
  else if (type == PointerType::get(int32_type(), 0))
    return "int*";
  else if (type == PointerType::get(int64_type(), 0))
#if SIZEOF_LONG==8
    return "long*";
#else
    return "int64*";
#endif
  else if (type == PointerType::get(float_type(), 0))
    return "float*";
  else if (type == PointerType::get(double_type(), 0))
    return "double*";
  else if (type == ExprPtrTy)
    return "expr*";
  else if (type == ExprPtrPtrTy)
    return "expr**";
  else if (type == GSLMatrixPtrTy)
    return "matrix*";
  else if (type == GSLDoubleMatrixPtrTy)
    return "dmatrix*";
  else if (type == GSLComplexMatrixPtrTy)
    return "cmatrix*";
  else if (type == GSLIntMatrixPtrTy)
    return "imatrix*";
  else if (type == VoidPtrTy)
    return "void*";
  else if (type->getTypeID() == Type::PointerTyID)
    return "<unknown C pointer type>";
  else
    return "<unknown C type>";
}

Function *interpreter::declare_extern(void *fp, string name, string restype,
				      int n, ...)
{
  va_list ap;
  bool varargs = n<0;
  if (varargs) n = -n;
  list<string> argtypes;
  va_start(ap, n);
  for (int i = 0; i < n; i++) {
    const char *s = va_arg(ap, char*);
    argtypes.push_back(s);
  }
  va_end(ap);
  return declare_extern(-1, name, restype, argtypes, varargs, fp);
}

Function *interpreter::declare_extern(int priv, string name, string restype,
				      const list<string>& argtypes,
				      bool varargs, void *fp,
				      string asname)
{
  // translate type names to LLVM types
  size_t n = argtypes.size();
  const Type* type = named_type(restype);
  vector<const Type*> argt(n);
  list<string>::const_iterator atype = argtypes.begin();
  for (size_t i = 0; i < n; i++, atype++) {
    argt[i] = named_type(*atype);
    // sanity check
    if (argt[i] == void_type())
      throw err("'void' not permitted as an argument type");
  }
  if (fp) {
    // These are for internal use only (runtime calls).
    Function *f = module->getFunction(name);
    if (f) {
      assert(sys::DynamicLibrary::SearchForAddressOfSymbol(name) == fp);
      return f;
    }
    // The function declaration hasn't been assembled yet. Do it now.
    FunctionType *ft = FunctionType::get(type, argt, varargs);
    f = Function::Create(ft, Function::ExternalLinkage, name, module);
    // Enter a fixed association into the dynamic linker table. This ensures
    // that even if the runtime functions can't be resolved via dlopening
    // the interpreter executable (e.g., if the interpreter was linked
    // without -rdynamic), the interpreter will still find them.
    sys::DynamicLibrary::AddSymbol(name, fp);
    always_used.insert(f);
    return f;
  }
  // External C function visible in the Pure program. No varargs are allowed
  // here for now. Also, we have to translate some of the parameter types
  // (expr** becomes void*, int32_t gets promoted to int64_t if the default
  // int type of the target platform has 64 bit).
  assert(!varargs);
  if (type == ExprPtrPtrTy)
    type = VoidPtrTy;
  else if (type == int32_type() && sizeof(int) > 4)
    type = int64_type();
  for (size_t i = 0; i < n; i++, atype++)
    if (argt[i] == ExprPtrPtrTy)
      argt[i] = VoidPtrTy;
    else if (argt[i] == int32_type() && sizeof(int) > 4)
      argt[i] = int64_type();
  if (asname.empty()) asname = name;
  string asid = make_qualid(asname), absasid = make_absid(asname);
  symbol* _sym = symtab.lookup(absasid);
  /* If the symbol is already declared and we have a public/private specifier,
     make sure that they match up. */
  if (_sym) {
    if (priv >= 0 && _sym->priv != priv)
      throw err("symbol '"+asid+"' already declared "+
		(_sym->priv?"'private'":"'public'"));
  } else {
    _sym = symtab.sym(absasid);
    if (priv >= 0)
      _sym->priv = priv;
  }
  assert(_sym);
  symbol& sym = *_sym;
  if (globenv.find(sym.f) != globenv.end() &&
      externals.find(sym.f) == externals.end())
    // There already is a Pure function or global variable for this symbol.
    // This is an error (unless the symbol is already declared as an external).
    throw err("symbol '"+asname+"' is already defined as a Pure "+
	      ((globenv[sym.f].t == env_info::fun) ? "function" :
	       (globenv[sym.f].t == env_info::fvar) ? "variable" :
	       (globenv[sym.f].t == env_info::cvar) ? "constant" :
	       "gizmo" /* this can't happen, or at least it shouldn't ;-) */));
  else if (globalvars.find(sym.f) != globalvars.end() &&
	   externals.find(sym.f) == externals.end())
    // The symbol isn't defined as a Pure function, global variable or
    // external, but an unbound symbol of this name already exists. This may
    // be an error, but we can't be sure. Since there's currently no way to
    // patch up old uses of the symbol, let's at least warn the user about
    // these.
    warning("warning: external '"+asname+"' shadows previous undefined use of this symbol");
  // Create the function type and check for an existing declaration of the
  // external.
  FunctionType *ft = FunctionType::get(type, argt, false);
  Function *g = module->getFunction(name);
  const FunctionType *gt = g?g->getFunctionType():0;
  // Check whether we already have an external declaration for this symbol.
  map<int32_t,ExternInfo>::const_iterator it = externals.find(sym.f);
  // Handle the case that the C function was imported through a dll *after*
  // the definition of a Pure function of the same name. In this case the C
  // function won't be accessible in the Pure program at all.
  if (it == externals.end() && g && !g->isDeclaration())
    throw err("symbol '"+name+"' is already defined as a Pure function");
  if (it == externals.end() && g) {
    // Cross-check with a builtin declaration.
    assert(g->isDeclaration() && gt);
    if (gt != ft) {
      bool ok = gt->getReturnType()==type && gt->getNumParams()==n &&
	gt->isVarArg()==varargs;
      for (size_t i = 0; ok && i < n; i++) {
	// In Pure, we allow void* to be passed for a char*, to bypass the
	// automatic marshalling from Pure to C strings. Oh well.
	ok = gt->getParamType(i)==argt[i] ||
	  (gt->getParamType(i)==CharPtrTy &&
	   argt[i] == VoidPtrTy);
      }
      if (!ok) {
	// Give some reasonable diagnostic. gt itself shows as LLVM assembler
	// when printed, so instead we manufacture an ExternInfo for gt which
	// supposedly is more informative and hopefully looks nicer to the
	// Pure programmer. ;-)
	size_t n = gt->getNumParams();
	vector<const Type*> argt(n);
	for (size_t i = 0; i < n; i++)
	  argt[i] = gt->getParamType(i);
	ExternInfo info(sym.f, name, gt->getReturnType(), argt, g);
	ostringstream msg;
	msg << "declaration of extern function '" << name
	    << "' does not match builtin declaration: " << info;
	throw err(msg.str());
      }
    }
  }
  if (it != externals.end()) {
    // already declared, check that declarations match
    const ExternInfo& info = it->second;
    if (type != info.type || argt != info.argtypes) {
      ostringstream msg;
      msg << "declaration of extern function '" << name
	  << "' does not match previous declaration: " << info;
      throw err(msg.str());
    }
    return info.f;
  }
  // Check that the external function actually exists by searching the program
  // and resident libraries.
  if (!sys::DynamicLibrary::SearchForAddressOfSymbol(name))
    throw err("external symbol '"+name+"' cannot be found");
  // If we come here, we have a new external symbol for which we create a
  // declaration (if needed), as well as a Pure wrapper function which is
  // entered into the externals table.
  if (!g) {
    gt = ft;
    g = Function::Create(gt, Function::ExternalLinkage, name, module);
    Function::arg_iterator a = g->arg_begin();
    for (size_t i = 0; a != g->arg_end(); ++a, ++i)
      a->setName(mklabel("arg", i));
  }
  assert(gt);
  // Create a little wrapper function which checks arguments, unboxes them,
  // calls the external function, and finally boxes the result. If the
  // arguments fail to match, provide a default value (cbox) which may be
  // patched up later with a Pure function. Note that this function is known
  // to Pure under the given alias name which may be different from the real
  // external name, so that the external name may be reused for a Pure
  // function (usually a wrapper function replacing the C external in Pure
  // programs).
  vector<const Type*> argt2(n, ExprPtrTy);
  FunctionType *ft2 = FunctionType::get(ExprPtrTy, argt2, false);
  Function *f = Function::Create(ft2, Function::InternalLinkage,
				 "$$wrap."+asid, module);
  vector<Value*> args(n), unboxed(n);
  Function::arg_iterator a = f->arg_begin();
  for (size_t i = 0; a != f->arg_end(); ++a, ++i) {
    a->setName(mklabel("arg", i)); args[i] = a;
  }
#ifdef LLVM26
  Builder b(llvm::getGlobalContext());
#else
  Builder b;
#endif
  BasicBlock *bb = basic_block("entry", f),
    *noretbb = basic_block("noret"),
    *failedbb = basic_block("failed");
  b.SetInsertPoint(bb);
  // unbox arguments
  bool temps = false, vtemps = false;
  for (size_t i = 0; i < n; i++) {
    Value *x = args[i];
    // check for thunks which must be forced
    if (argt[i] != ExprPtrTy) {
      // do a quick check on the tag value
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      Value *checkv = b.CreateICmpEQ(tagv, Zero, "check");
      BasicBlock *forcebb = basic_block("force");
      BasicBlock *skipbb = basic_block("skip");
      b.CreateCondBr(checkv, forcebb, skipbb);
      f->getBasicBlockList().push_back(forcebb);
      b.SetInsertPoint(forcebb);
      b.CreateCall(module->getFunction("pure_force"), x);
      b.CreateBr(skipbb);
      f->getBasicBlockList().push_back(skipbb);
      b.SetInsertPoint(skipbb);
    }
    if (argt[i] == int1_type()) {
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(EXPR::INT), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      Value *pv = b.CreateBitCast(x, IntExprPtrTy, "intexpr");
      idx[1] = ValFldIndex;
      Value *iv = b.CreateLoad(b.CreateGEP(pv, idx, idx+2), "intval");
      unboxed[i] = b.CreateICmpNE(iv, Zero);
    } else if (argt[i] == int8_type()) {
      /* We allow either ints or bigints to be passed for C integers. */
      BasicBlock *intbb = basic_block("int");
      BasicBlock *mpzbb = basic_block("mpz");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 2);
      sw->addCase(SInt(EXPR::INT), intbb);
      sw->addCase(SInt(EXPR::BIGINT), mpzbb);
      f->getBasicBlockList().push_back(intbb);
      b.SetInsertPoint(intbb);
      Value *pv = b.CreateBitCast(x, IntExprPtrTy, "intexpr");
      idx[1] = ValFldIndex;
      Value *intv = b.CreateLoad(b.CreateGEP(pv, idx, idx+2), "intval");
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(mpzbb);
      b.SetInsertPoint(mpzbb);
      // Handle the case of a bigint (mpz_t -> int).
      Value *mpzv = b.CreateCall(module->getFunction("pure_get_int"), x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = b.CreatePHI(int32_type());
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = b.CreateTrunc(phi, int8_type());
    } else if (argt[i] == int16_type()) {
      BasicBlock *intbb = basic_block("int");
      BasicBlock *mpzbb = basic_block("mpz");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 2);
      sw->addCase(SInt(EXPR::INT), intbb);
      sw->addCase(SInt(EXPR::BIGINT), mpzbb);
      f->getBasicBlockList().push_back(intbb);
      b.SetInsertPoint(intbb);
      Value *pv = b.CreateBitCast(x, IntExprPtrTy, "intexpr");
      idx[1] = ValFldIndex;
      Value *intv = b.CreateLoad(b.CreateGEP(pv, idx, idx+2), "intval");
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(mpzbb);
      b.SetInsertPoint(mpzbb);
      // Handle the case of a bigint (mpz_t -> int).
      Value *mpzv = b.CreateCall(module->getFunction("pure_get_int"), x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = b.CreatePHI(int32_type());
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = b.CreateTrunc(phi, int16_type());
    } else if (argt[i] == int32_type()) {
      BasicBlock *intbb = basic_block("int");
      BasicBlock *mpzbb = basic_block("mpz");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 2);
      sw->addCase(SInt(EXPR::INT), intbb);
      sw->addCase(SInt(EXPR::BIGINT), mpzbb);
      f->getBasicBlockList().push_back(intbb);
      b.SetInsertPoint(intbb);
      Value *pv = b.CreateBitCast(x, IntExprPtrTy, "intexpr");
      idx[1] = ValFldIndex;
      Value *intv = b.CreateLoad(b.CreateGEP(pv, idx, idx+2), "intval");
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(mpzbb);
      b.SetInsertPoint(mpzbb);
      // Handle the case of a bigint (mpz_t -> int).
      Value *mpzv = b.CreateCall(module->getFunction("pure_get_int"), x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = b.CreatePHI(int32_type());
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = phi;
    } else if (argt[i] == int64_type()) {
      BasicBlock *intbb = basic_block("int");
      BasicBlock *mpzbb = basic_block("mpz");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 2);
      sw->addCase(SInt(EXPR::INT), intbb);
      sw->addCase(SInt(EXPR::BIGINT), mpzbb);
      f->getBasicBlockList().push_back(intbb);
      b.SetInsertPoint(intbb);
      Value *pv = b.CreateBitCast(x, IntExprPtrTy, "intexpr");
      idx[1] = ValFldIndex;
      Value *intv = b.CreateLoad(b.CreateGEP(pv, idx, idx+2), "intval");
      intv = b.CreateSExt(intv, int64_type());
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(mpzbb);
      b.SetInsertPoint(mpzbb);
      // Handle the case of a bigint (mpz_t -> long).
      Value *mpzv = b.CreateCall(module->getFunction("pure_get_int64"), x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = b.CreatePHI(int64_type());
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = phi;
    } else if (argt[i] == float_type()) {
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(EXPR::DBL), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      Value *pv = b.CreateBitCast(x, DblExprPtrTy, "dblexpr");
      idx[1] = ValFldIndex;
      Value *dv = b.CreateLoad(b.CreateGEP(pv, idx, idx+2), "dblval");
      unboxed[i] = b.CreateFPTrunc(dv, float_type());
    } else if (argt[i] == double_type()) {
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(EXPR::DBL), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      Value *pv = b.CreateBitCast(x, DblExprPtrTy, "dblexpr");
      idx[1] = ValFldIndex;
      Value *dv = b.CreateLoad(b.CreateGEP(pv, idx, idx+2), "dblval");
      unboxed[i] = dv;
    } else if (argt[i] == CharPtrTy) {
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(EXPR::STR), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      Value *sv = b.CreateCall(module->getFunction("pure_get_cstring"), x);
      unboxed[i] = sv; temps = true;
    } else if (argt[i] == PointerType::get(int64_type(), 0)) {
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(EXPR::PTR), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      Value *pv = b.CreateBitCast(x, PtrExprPtrTy, "ptrexpr");
      idx[1] = ValFldIndex;
      Value *ptrv = b.CreateLoad(b.CreateGEP(pv, idx, idx+2), "ptrval");
      unboxed[i] = b.CreateBitCast(ptrv, argt[i]);
    } else if (argt[i] == PointerType::get(int16_type(), 0) ||
	       argt[i] == PointerType::get(int32_type(), 0) ||
	       argt[i] == PointerType::get(double_type(), 0) ||
	       argt[i] == PointerType::get(float_type(), 0)) {
      /* These get special treatment, because we also allow numeric matrices
	 to be passed as an integer or floating point vector here. */
      BasicBlock *ptrbb = basic_block("ptr");
      BasicBlock *matrixbb = basic_block("matrix");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 4);
      Function *get_fun = module->getFunction
	((argt[i] == PointerType::get(int16_type(), 0)) ?
	 "pure_get_matrix_data_short" :
	 (argt[i] == PointerType::get(int32_type(), 0)) ?
	 "pure_get_matrix_data_int" :
	 (argt[i] == PointerType::get(float_type(), 0)) ?
	 "pure_get_matrix_data_float" :
	 "pure_get_matrix_data_double");
      sw->addCase(SInt(EXPR::PTR), ptrbb);
      sw->addCase(SInt(EXPR::DMATRIX), matrixbb);
      sw->addCase(SInt(EXPR::CMATRIX), matrixbb);
      sw->addCase(SInt(EXPR::IMATRIX), matrixbb);
      f->getBasicBlockList().push_back(ptrbb);
      b.SetInsertPoint(ptrbb);
      Value *pv = b.CreateBitCast(x, PtrExprPtrTy, "ptrexpr");
      idx[1] = ValFldIndex;
      Value *ptrv = b.CreateLoad(b.CreateGEP(pv, idx, idx+2), "ptrval");
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(matrixbb);
      b.SetInsertPoint(matrixbb);
      Value *matrixv = b.CreateCall(get_fun, x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = b.CreatePHI(VoidPtrTy);
      phi->addIncoming(ptrv, ptrbb);
      phi->addIncoming(matrixv, matrixbb);
      unboxed[i] = b.CreateBitCast(phi, argt[i]); vtemps = true;
    } else if (argt[i] == GSLMatrixPtrTy ||
	       argt[i] == GSLDoubleMatrixPtrTy ||
	       argt[i] == GSLComplexMatrixPtrTy ||
	       argt[i] == GSLIntMatrixPtrTy) {
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      int32_t ttag = -99;
      if (argt[i] == GSLMatrixPtrTy)
	ttag = EXPR::MATRIX;
      else if (argt[i] == GSLDoubleMatrixPtrTy)
	ttag = EXPR::DMATRIX;
      else if (argt[i] == GSLComplexMatrixPtrTy)
	ttag = EXPR::CMATRIX;
      else if (argt[i] == GSLIntMatrixPtrTy)
	ttag = EXPR::IMATRIX;
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(ttag), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      Value *matv = b.CreateCall(module->getFunction("pure_get_matrix"), x);
      unboxed[i] = b.CreateBitCast(matv, argt[i]);
    } else if (argt[i] == ExprPtrTy) {
      // passed through
      unboxed[i] = x;
    } else if (argt[i] == VoidPtrTy) {
      BasicBlock *ptrbb = basic_block("ptr");
      BasicBlock *mpzbb = basic_block("mpz");
      BasicBlock *matrixbb = basic_block("matrix");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 7);
      /* We also allow bigints, strings and matrices to be passed as a void*
	 here. The first case lets you use GMP routines directly in Pure if
	 you declare the mpz_t params as void*. The second case allows a
	 string to be passed without implicitly converting it to the system
	 encoding first. The third case allows the raw data of a matrix to be
	 passed. Note that in all cases a direct pointer to the data will be
	 passed, which enables mutation of the data; if this isn't desired
	 then you should copy the data first. */
      sw->addCase(SInt(EXPR::PTR), ptrbb);
      sw->addCase(SInt(EXPR::STR), ptrbb);
      sw->addCase(SInt(EXPR::BIGINT), mpzbb);
      sw->addCase(SInt(EXPR::MATRIX), matrixbb);
      sw->addCase(SInt(EXPR::DMATRIX), matrixbb);
      sw->addCase(SInt(EXPR::CMATRIX), matrixbb);
      sw->addCase(SInt(EXPR::IMATRIX), matrixbb);
      f->getBasicBlockList().push_back(ptrbb);
      b.SetInsertPoint(ptrbb);
      // The following will work with both pointer and string expressions.
      Value *pv = b.CreateBitCast(x, PtrExprPtrTy, "ptrexpr");
      idx[1] = ValFldIndex;
      Value *ptrv = b.CreateLoad(b.CreateGEP(pv, idx, idx+2), "ptrval");
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(mpzbb);
      b.SetInsertPoint(mpzbb);
      // Handle the case of a bigint (mpz_t -> void*).
      Value *mpzv = b.CreateCall(module->getFunction("pure_get_bigint"), x);
      b.CreateBr(okbb);
      // Handle the case of a matrix (gsl_matrix_xyz* -> void*).
      f->getBasicBlockList().push_back(matrixbb);
      b.SetInsertPoint(matrixbb);
      Value *matrixv =
	b.CreateCall(module->getFunction("pure_get_matrix_data"), x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = b.CreatePHI(VoidPtrTy);
      phi->addIncoming(ptrv, ptrbb);
      phi->addIncoming(mpzv, mpzbb);
      phi->addIncoming(matrixv, matrixbb);
      unboxed[i] = phi;
      if (gt->getParamType(i)==CharPtrTy)
	// An external builtin already has this parameter declared as char*.
	// We allow void* to be passed anyway, so just cast it to char* to
	// make the LLVM typechecker happy.
	unboxed[i] = b.CreateBitCast(unboxed[i], CharPtrTy);
    } else
      assert(0 && "invalid C type");
  }
  // For the debugger we create a little dummy environment which provides the
  // information about the external function that we need.
  Env *e = 0;
  if (debugging) {
    e = new Env(sym.f, n);
    Function *f = module->getFunction("pure_debug_rule");
    assert(f);
    vector<Value*> args;
    args.push_back(constptr(e));
    args.push_back(constptr(0));
    b.CreateCall(f, args.begin(), args.end());
  }
  // call the function
  Value* u = b.CreateCall(g, unboxed.begin(), unboxed.end());
  // free temporaries
  if (temps) b.CreateCall(module->getFunction("pure_free_cstrings"));
  if (vtemps) b.CreateCall(module->getFunction("pure_free_cvectors"));
  // box the result
  if (type == void_type())
    u = b.CreateCall(module->getFunction("pure_const"),
		     SInt(symtab.void_sym().f));
  else if (type == int1_type())
    u = b.CreateCall(module->getFunction("pure_int"),
		     b.CreateZExt(u, int32_type()));
  else if (type == int8_type())
    u = b.CreateCall(module->getFunction("pure_int"),
		     b.CreateSExt(u, int32_type()));
  else if (type == int16_type())
    u = b.CreateCall(module->getFunction("pure_int"),
		     b.CreateSExt(u, int32_type()));
  else if (type == int32_type())
    u = b.CreateCall(module->getFunction("pure_int"), u);
  else if (type == int64_type())
    u = b.CreateCall(module->getFunction("pure_int64"), u);
  else if (type == float_type())
    u = b.CreateCall(module->getFunction("pure_double"),
		     b.CreateFPExt(u, double_type()));
  else if (type == double_type())
    u = b.CreateCall(module->getFunction("pure_double"), u);
  else if (type == CharPtrTy)
    u = b.CreateCall(module->getFunction("pure_cstring_dup"), u);
  else if (type == PointerType::get(int16_type(), 0) ||
	   type == PointerType::get(int32_type(), 0) ||
	   type == PointerType::get(int64_type(), 0) ||
	   type == PointerType::get(float_type(), 0) ||
	   type == PointerType::get(double_type(), 0))
    u = b.CreateCall(module->getFunction("pure_pointer"),
		     b.CreateBitCast(u, VoidPtrTy));
  else if (type == GSLMatrixPtrTy)
    u = b.CreateCall(module->getFunction("pure_symbolic_matrix"),
		     b.CreateBitCast(u, VoidPtrTy));
  else if (type == GSLDoubleMatrixPtrTy)
    u = b.CreateCall(module->getFunction("pure_double_matrix"),
		     b.CreateBitCast(u, VoidPtrTy));
  else if (type == GSLComplexMatrixPtrTy)
    u = b.CreateCall(module->getFunction("pure_complex_matrix"),
		     b.CreateBitCast(u, VoidPtrTy));
  else if (type == GSLIntMatrixPtrTy)
    u = b.CreateCall(module->getFunction("pure_int_matrix"),
		     b.CreateBitCast(u, VoidPtrTy));
  else if (type == ExprPtrTy) {
    // check that we actually got a valid pointer; otherwise the call failed
    BasicBlock *okbb = basic_block("ok");
    b.CreateCondBr
      (b.CreateICmpNE(u, NullExprPtr, "cmp"), okbb, noretbb);
    f->getBasicBlockList().push_back(okbb);
    b.SetInsertPoint(okbb);
    // value is passed through
  } else if (type == VoidPtrTy)
    u = b.CreateCall(module->getFunction("pure_pointer"), u);
  else
    assert(0 && "invalid C type");
  if (debugging) {
    Function *f = module->getFunction("pure_debug_redn");
    assert(f);
    vector<Value*> args;
    args.push_back(constptr(e));
    args.push_back(constptr(0));
    args.push_back(u);
    b.CreateCall(f, args.begin(), args.end());
  }
  // free arguments (we do that here so that the arguments don't get freed
  // before we know that we don't need them anymore)
  if (n > 0) {
    vector<Value*> freeargs(3);
    freeargs[0] = u;
    freeargs[1] = UInt(n);
    freeargs[2] = Zero;
    b.CreateCall(module->getFunction("pure_pop_args"),
		 freeargs.begin(), freeargs.end());
  }
  b.CreateRet(u);
  // The call failed. Provide a default value.
  f->getBasicBlockList().push_back(noretbb);
  b.SetInsertPoint(noretbb);
  if (debugging) {
    Function *f = module->getFunction("pure_debug_redn");
    assert(f);
    vector<Value*> args;
    args.push_back(constptr(e));
    args.push_back(constptr(0));
    args.push_back(NullExprPtr);
    b.CreateCall(f, args.begin(), args.end());
  }
  b.CreateBr(failedbb);
  f->getBasicBlockList().push_back(failedbb);
  b.SetInsertPoint(failedbb);
  // free temporaries
  if (temps) b.CreateCall(module->getFunction("pure_free_cstrings"));
  if (vtemps) b.CreateCall(module->getFunction("pure_free_cvectors"));
  // As default, create a cbox for the function symbol and apply that to our
  // parameters. The cbox may be patched up later to become a Pure function.
  // In effect, this allows an external function to be augmented with Pure
  // equations, but note that the external C function will always be tried
  // first.
  pure_expr *cv = pure_const(sym.f);
  assert(JIT);
  GlobalVar& v = globalvars[sym.f];
  if (!v.v) {
    v.v = global_variable
      (module, ExprPtrTy, false, GlobalVariable::InternalLinkage, NullExprPtr,
       mkvarlabel(sym.f));
    JIT->addGlobalMapping(v.v, &v.x);
  }
  if (v.x) pure_free(v.x); v.x = pure_new(cv);
  Value *defaultv = b.CreateLoad(v.v);
  vector<Value*> myargs(2);
  for (size_t i = 0; i < n; ++i) {
    myargs[0] = b.CreateCall(module->getFunction("pure_new"), defaultv);
    myargs[1] = b.CreateCall(module->getFunction("pure_new"), args[i]);
    defaultv = b.CreateCall(module->getFunction("pure_apply"),
			    myargs.begin(), myargs.end());
  }
  if (n > 0) {
    vector<Value*> freeargs(3);
    freeargs[0] = defaultv;
    freeargs[1] = UInt(n);
    freeargs[2] = Zero;
    b.CreateCall(module->getFunction("pure_pop_args"),
		 freeargs.begin(), freeargs.end());
  }
  b.CreateRet(defaultv);
  verifyFunction(*f);
  if (FPM) FPM->run(*f);
  if (verbose&verbosity::dump) {
#if RAW_STREAM
    raw_stdout_ostream out;
#else
    ostream& out = std::cout;
#endif
    f->print(out);
  }
  externals[sym.f] = ExternInfo(sym.f, name, type, argt, f);
  return f;
}

Value *interpreter::envptr(Env *f)
{
  if (!fptr)
    return NullPtr;
  else
    return act_builder().CreateLoad(fptrvar);
}

Value *interpreter::constptr(const void *p)
{
  if (!p)
    return NullPtr;
  else
#if SIZEOF_VOID_P==8
    return ConstantExpr::getIntToPtr(UInt64((uint64_t)p), VoidPtrTy);
#else
    return ConstantExpr::getIntToPtr(UInt((uint32_t)p), VoidPtrTy);
#endif
}

expr interpreter::wrap_expr(pure_expr *x)
{
  ostringstream label;
  label << x;
  GlobalVar *v = new GlobalVar;
  v->v = global_variable
    (module, ExprPtrTy, false, llvm::GlobalVariable::InternalLinkage,
     llvm::ConstantPointerNull::get(ExprPtrTy), "$$tmpvar"+label.str());
  v->x = pure_new(x);
  JIT->addGlobalMapping(v->v, &v->x);
  return expr(EXPR::WRAP, v);
}

pure_expr *interpreter::const_value(expr x, bool quote)
{
  switch (x.tag()) {
  // constants:
  case EXPR::INT:
    return pure_int(x.ival());
  case EXPR::BIGINT:
    return pure_mpz(x.zval());
  case EXPR::DBL:
    return pure_double(x.dval());
  case EXPR::STR:
    return pure_string_dup(x.sval());
  case EXPR::PTR:
    return pure_pointer(x.pval());
  case EXPR::WRAP:
    if (x.pval()) {
      GlobalVar *v = (GlobalVar*)x.pval();
      return v->x;
    } else
      return 0;
  case EXPR::MATRIX:
    return const_matrix_value(x, quote);
  case EXPR::APP:
    if (quote) {
      pure_expr *f = 0, *y = 0;
      if ((f = const_value(x.xval1(), quote)) &&
	  (y = const_value(x.xval2(), quote))) {
	pure_new_args(2, f, y);
	return pure_applc(f, y);
      } else {
	if (f) pure_freenew(f);
	return 0;
      }
    } else {
      exprl xs;
      expr tl;
      if (x.is_list2(xs, tl) || x.is_tuple(xs)) {
	// lists and tuples
	size_t i, n = xs.size();
	pure_expr *u = 0;
	if (!x.is_pair() && tl.tag() != symtab.nil_sym().f) {
	  u = const_value(tl);
	  if (u == 0) return 0;
	}
	pure_expr **xv = (pure_expr**)malloc(n*sizeof(pure_expr*));
	if (!xv) return 0;
	exprl::iterator it = xs.begin(), end = xs.end();
	for (i = 0; it != end; i++, it++)
	  if ((xv[i] = const_value(*it)) == 0) {
	    if (u != 0)
	      pure_freenew(u);
	    for (size_t j = 0; j < i; j++)
	      pure_freenew(xv[j]);
	    free(xv);
	    return 0;
	  }
	pure_expr *res;
	if (u == 0)
	  res = (x.is_pair()?pure_tuplev:pure_listv)(n, xv);
	else
	  res = pure_listv2(n, xv, u);
	free(xv);
	return res;
      } else
	return const_app_value(x);
    }
  default: {
    if (x.tag() > 0) {
      if (quote)
	return pure_const(x.tag());
      else {
	if (x.tag() == symtab.locals_sym().f ||
	    externals.find(x.tag()) != externals.end())
	  return 0;
	map<int32_t,GlobalVar>::iterator v = globalvars.find(x.tag());
	if (v != globalvars.end()) {
	  // bound variable
	  pure_expr *y = v->second.x;
	  // check if we got a closure subject to evaluation
	  if (y->tag >= 0 && y->data.clos && y->data.clos->n == 0)
	    return 0;
	  else
	    return y;
	} else {
	  // unbound symbol
	  assert(globenv.find(x.tag()) == globenv.end() && "bad global");
	  return pure_const(x.tag());
	}
      }
    } else
      return 0;
  }
  }
}

pure_expr *interpreter::const_matrix_value(expr x, bool quote)
{
  size_t n = x.xvals()->size(), m = 0, i = 0, j = 0;
  pure_expr **us = new pure_expr*[n], **vs = 0, *ret;
  assert(us);
  for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
       xs != end; xs++, i++) {
    m = xs->size(); j = 0; vs = new pure_expr*[m];
    assert(vs);
    for (exprl::iterator ys = xs->begin(), end = xs->end();
	 ys != end; ys++, j++) {
      vs[j] = const_value(*ys, quote);
      if (!vs[j]) goto err;
    }
    us[i] = (quote?pure_matrix_columnsvq:pure_matrix_columnsv)(m, vs);
    if (!us[i]) goto err;
    delete[] vs;
  }
  ret = (quote?pure_matrix_rowsvq:pure_matrix_rowsv)(n, us);
  delete[] us;
  return ret;
 err:
  // bail out
  for (size_t k = 0; k < j; k++) pure_freenew(vs[k]);
  if (vs) delete[] vs;
  for (size_t k = 0; k < i; k++) pure_freenew(us[k]);
  if (us) delete[] us;
  return 0;
}

pure_expr *interpreter::const_app_value(expr x)
{
  if (x.tag() == EXPR::APP) {
    if (is_quote(x.xval1().tag()))
      return const_value(x.xval2(), true);
    else {
      pure_expr *f = 0, *y = 0;
      if ((f = const_app_value(x.xval1())) && (y = const_value(x.xval2())))
	return pure_app(f, y);
      else {
	if (f) pure_freenew(f);
	return 0;
      }
    }
  } else if (x.tag() > 0) {
    if (externals.find(x.tag()) != externals.end())
      return 0;
    map<int32_t,GlobalVar>::iterator v = globalvars.find(x.tag());
    if (v != globalvars.end()) {
      // bound variable
      pure_expr *y = v->second.x;
      // walk down the spine, if any
      while (y->tag == EXPR::APP) y = y->data.x[0];
      // check if we got a closure subject to evaluation
      if (y->tag >= 0 && y->data.clos)
	return 0;
      else
	// not a callable closure, so it must be a constructor term
	return v->second.x;
    } else {
      // unbound symbol
      assert(globenv.find(x.tag()) == globenv.end() && "bad global");
      return pure_const(x.tag());
    }
  } else
    return 0;
}

pure_expr *interpreter::doeval(expr x, pure_expr*& e, bool keep)
{
  char test;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax) {
    e = pure_const(symtab.segfault_sym().f);
    return 0;
  }
  e = 0;
  clock_t t0 = clock();
  pure_expr *res = 0;
  if (!keep) {
    // First check whether the value is actually a constant, then we can skip
    // the compilation step. (We only do this if we're not batch-compiling,
    // since in a batch compilation we need the generated code.)
    res = const_value(x);
    if (interactive && stats) clocks = clock()-t0;
    if (res) return res;
  }
  // Create an anonymous function to call in order to evaluate the target
  // expression.
  /* NOTE: The environment is allocated dynamically, so that its child
     environments survive for the entire lifetime of any embedded closures,
     which might still be called at a later time. */
  Env *save_fptr = fptr;
  fptr = new Env(0, 0, 0, x, false);
  Env &f = *fptr;
  push("doeval", &f);
  fun_prolog("$$init");
#if DEBUG>1
  ostringstream msg;
  msg << "doeval: " << x;
  debug(msg.str().c_str());
#endif
  f.CreateRet(codegen(x));
  fun_finish();
  pop(&f);
  // JIT and execute the function. Note that we need to do this even in a
  // batch compilation, since subsequent const definitions and resulting code
  // may depend on the outcome of this computation.
  f.fp = JIT->getPointerToFunction(f.f);
  assert(f.fp);
  t0 = clock();
  res = pure_invoke(f.fp, &e);
  if (interactive && stats) clocks = clock()-t0;
  // Get rid of our anonymous function.
  JIT->freeMachineCodeForFunction(f.f);
  if (!keep) {
    f.f->eraseFromParent();
    /* XXXFIXME: We should really keep track of references to the environment
       and collect it when it's no longer used. But this causes various
       issues, especially with thunks and when LLVM is compiled with
       --enable-expensive-checks, so it seems safest to just leak some memory
       here. */
  }
  fptr = save_fptr;
  if (estk.empty()) {
    // collect garbage
    pure_expr *t = tmps;
    while (t) {
      pure_expr *next = t->xp;
      if (t != res) pure_freenew(t);
      t = next;
    }
  }
  // NOTE: Result (if any) is to be freed by the caller.
  return res;
}

static pure_expr *pure_subterm(pure_expr *x, const path& p)
{
  for (size_t i = 0, n = p.len(); i < n; i++) {
    assert(x->tag == EXPR::APP);
    x = x->data.x[p[i]?1:0];
  }
  return x;
}

pure_expr *interpreter::dodefn(env vars, expr lhs, expr rhs, pure_expr*& e,
			       bool keep)
{
  char test;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax) {
    e = pure_const(symtab.segfault_sym().f);
    return 0;
  }
  e = 0;
  clock_t t0 = clock();
  pure_expr *res = 0;
  if (!keep) {
    // First check whether the value is actually a constant, then we can skip
    // the compilation step. (We only do this if we're not batch-compiling,
    // since in a batch compilation we need the generated code.)
    res = const_value(rhs);
    if (res) {
      matcher m(rule(lhs, rhs));
      if (m.match(res)) {
	// Bind the variables.
	for (env::const_iterator it = vars.begin(); it != vars.end(); ++it) {
	  int32_t tag = it->first;
	  const env_info& info = it->second;
	  assert(info.t == env_info::lvar && info.p);
	  // find the subterm at info.p
	  pure_expr *x = pure_subterm(res, *info.p);
	  // store the value in a global variable of the same name
	  const symbol& sym = symtab.sym(tag);
	  GlobalVar& v = globalvars[tag];
	  if (!v.v) {
	    if (sym.priv)
	      v.v = global_variable
		(module, ExprPtrTy, false, GlobalVariable::InternalLinkage,
		 NullExprPtr, "$$private."+sym.s);
	    else
	      v.v = global_variable
		(module, ExprPtrTy, false, GlobalVariable::ExternalLinkage,
		 NullExprPtr, mkvarsym(sym.s));
	    JIT->addGlobalMapping(v.v, &v.x);
	  }
	  if (v.x) pure_free(v.x);
	  v.x = pure_new(x);
	}
      } else {
	// Failed match, bail out.
	pure_freenew(res);
	res = e = 0;
      }
      if (interactive && stats) clocks = clock()-t0;
      return res;
    }
  }
  // Create an anonymous function to call in order to evaluate the rhs
  // expression, match against the lhs and bind variables in lhs accordingly.
  Env *save_fptr = fptr;
  fptr = new Env(0, 0, 0, rhs, false);
  Env &f = *fptr;
  push("dodefn", &f);
  fun_prolog("$$init");
#if DEBUG>1
  ostringstream msg;
  msg << "dodef: " << lhs << "=" << rhs;
  debug(msg.str().c_str());
#endif
  // compute the matchee
  Value *arg = codegen(rhs);
  // emit the matching code
  BasicBlock *matchedbb = basic_block("matched");
  BasicBlock *failedbb = basic_block("failed");
  matcher m(rule(lhs, rhs));
  if (verbose&verbosity::code) std::cout << m << '\n';
  state *start = m.start;
  simple_match(arg, start, matchedbb, failedbb);
  // matched => emit code for binding the variables
  f.f->getBasicBlockList().push_back(matchedbb);
  f.builder.SetInsertPoint(matchedbb);
  list<pure_expr*> cache;
  for (env::const_iterator it = vars.begin(); it != vars.end(); ++it) {
    int32_t tag = it->first;
    const env_info& info = it->second;
    assert(info.t == env_info::lvar && info.p);
    // walk the arg value to find the subterm at info.p
    Value *x = arg;
    path& p = *info.p;
    size_t n = p.len();
    for (size_t i = 0; i < n; i++)
      x = f.CreateLoadGEP(x, Zero, SubFldIndex(p[i]), mklabel("x", i, p[i]+1));
    // store the value in a global variable of the same name
    const symbol& sym = symtab.sym(tag);
    GlobalVar& v = globalvars[tag];
    if (!v.v) {
      if (sym.priv)
	v.v = global_variable
	  (module, ExprPtrTy, false, GlobalVariable::InternalLinkage,
	   NullExprPtr, "$$private."+sym.s);
      else
	v.v = global_variable
	  (module, ExprPtrTy, false, GlobalVariable::ExternalLinkage,
	   NullExprPtr, mkvarsym(sym.s));
      JIT->addGlobalMapping(v.v, &v.x);
    }
    /* Cache any old value so that we can free it later. Note that it is not
       safe to do so right away, because the value may be reused in one of the
       current assignments. */
    if (v.x) cache.push_back(v.x);
    call("pure_new", x);
#if DEBUG>2
    ostringstream msg;
    msg << "dodef: " << sym.s << " := %p";
    debug(msg.str().c_str(), x);
#endif
    f.builder.CreateStore(x, v.v);
  }
  // return the matchee to indicate success
  f.builder.CreateRet(arg);
  // failed => throw an exception
  f.f->getBasicBlockList().push_back(failedbb);
  f.builder.SetInsertPoint(failedbb);
  unwind();
  fun_finish();
  pop(&f);
  // JIT and execute the function. Note that we need to do this even in a
  // batch compilation, since subsequent const definitions and resulting code
  // may depend on the outcome of this computation.
  f.fp = JIT->getPointerToFunction(f.f);
  assert(f.fp);
  t0 = clock();
  res = pure_invoke(f.fp, &e);
  if (interactive && stats) clocks = clock()-t0;
  // Get rid of our anonymous function.
  JIT->freeMachineCodeForFunction(f.f);
  if (!keep) {
    f.f->eraseFromParent();
    /* XXXFIXME: We should really keep track of references to the environment
       and collect it when it's no longer used. But this causes various
       issues, especially with thunks and when LLVM is compiled with
       --enable-expensive-checks, so it seems safest to just leak some memory
       here. */
  }
  fptr = save_fptr;
  if (res) {
    // Get rid of any old values now.
    for (list<pure_expr*>::iterator it = cache.begin(), end = cache.end();
	 it != end; ++it)
      pure_free(*it);
  } else {
    // We caught an exception, clean up the mess.
    for (env::const_iterator it = vars.begin(); it != vars.end(); ++it) {
      int32_t tag = it->first;
      GlobalVar& v = globalvars[tag];
      if (!v.x) {
	JIT->updateGlobalMapping(v.v, 0);
	v.v->eraseFromParent();
	globalvars.erase(tag);
      }
    }
  }
  if (estk.empty()) {
    // collect garbage
    pure_expr *t = tmps;
    while (t) {
      pure_expr *next = t->xp;
      if (t != res) pure_freenew(t);
      t = next;
    }
  }
  // NOTE: Result (if any) is to be freed by the caller.
  return res;
}

static rulel *copy_rulel(rulel::const_iterator r,
			 rulel::const_iterator end)
{
  rulel *rl = new rulel;
  while (r != end) {
    rl->push_back(rule(r->lhs, r->rhs, r->qual));
    r++;
  }
  return rl;
}

Value *interpreter::when_codegen(expr x, matcher *m,
				 rulel::const_iterator r,
				 rulel::const_iterator end,
				 rule *rp)
// x = subject expression to be evaluated in the context of the bindings
// m = matching automaton for current rule
// r = current pattern binding rule
// end = end of rule list
{
  if (r == end) {
    toplevel_codegen(x, rp);
    return 0;
  } else {
    Env& act = act_env();
    rulel::const_iterator s = r;
    expr y = (++s == end)?x:s->rhs;
    assert(act.fmap.act().find(-y.hash()) != act.fmap.act().end());
    Env& e = *act.fmap.act()[-y.hash()];
    push("when", &e);
    fun_prolog("anonymous");
    BasicBlock *bodybb = basic_block("body");
    BasicBlock *matchedbb = basic_block("matched");
    BasicBlock *failedbb = basic_block("failed");
    e.builder.CreateBr(bodybb);
    e.f->getBasicBlockList().push_back(bodybb);
    e.builder.SetInsertPoint(bodybb);
    Value *arg = e.args[0];
    // emit the matching code
    state *start = m->start;
    if (debugging) debug_rule(0);
    simple_match(arg, start, matchedbb, failedbb);
    // matched => emit code for the reduct
    e.f->getBasicBlockList().push_back(matchedbb);
    e.builder.SetInsertPoint(matchedbb);
    const rule& rr = m->r[0];
    if (debugging) {
      expr y = (s==end)?x:expr::when(x, copy_rulel(s, end));
      e.rp = new rule(rr.lhs, y);
      debug_rule(e.rp);
    }
    Value *v = when_codegen(x, m+1, s, end, e.rp);
    if (v) e.CreateRet(v, e.rp);
    // failed => throw an exception
    e.f->getBasicBlockList().push_back(failedbb);
    e.builder.SetInsertPoint(failedbb);
    if (debugging) debug_redn(0);
    unwind(symtab.failed_match_sym().f);
    fun_finish();
    pop(&e);
    return funcall(&e, codegen(r->rhs));
  }
}

Value *interpreter::get_int(expr x)
{
  Env& e = act_env();
  if (x.ttag() == EXPR::INT || x.ttag() == EXPR::DBL) {
    if (x.tag() == EXPR::APP) {
      // embedded int/float operation, recursively generate unboxed value
      Value *u = builtin_codegen(x);
      if (x.ttag() == EXPR::INT)
	return u;
      else
	return e.builder.CreateFPToSI(u, int32_type());
    } else if (x.tag() == EXPR::INT)
      // integer constant, return "as is"
      return SInt(x.ival());
    else if (x.tag() == EXPR::DBL)
      // double constant, return "as is", promoted to int
      return SInt((int32_t)x.dval());
    else if (x.ttag() == EXPR::INT) {
      // int variable, needs unboxing
      assert(x.tag() == EXPR::VAR);
      Value *u = codegen(x);
      Value *p = e.builder.CreateBitCast(u, IntExprPtrTy, "intexpr");
      Value *v = e.CreateLoadGEP(p, Zero, ValFldIndex, "intval");
#if 0
      // collect the temporary, it's not needed any more
      call("pure_freenew", u);
#endif
      return v;
    } else {
      // double variable, needs unboxing and int conversion
      assert(x.tag() == EXPR::VAR && x.ttag() == EXPR::DBL);
      Value *u = codegen(x);
      Value *p = e.builder.CreateBitCast(u, DblExprPtrTy, "dblexpr");
      Value *v = e.CreateLoadGEP(p, Zero, ValFldIndex, "dblval");
      v = e.builder.CreateFPToSI(v, int32_type());
#if 0
      // collect the temporary, it's not needed any more
      call("pure_freenew", u);
#endif
      return v;
    }
  } else {
    // typeless expression; we have to check the computed value at runtime
    Value *u = codegen(x);
    // check that it's actually an integer
    // NOTE: we don't want to promote double values to int here
    verify_tag(u, EXPR::INT);
    // get the value
    Value *p = e.builder.CreateBitCast(u, IntExprPtrTy, "intexpr");
    Value *v = e.CreateLoadGEP(p, Zero, ValFldIndex, "intval");
    // collect the temporary, it's not needed any more
    call("pure_freenew", u);
    return v;
  }
}

Value *interpreter::get_double(expr x)
{
  Env& e = act_env();
  if (x.ttag() == EXPR::INT || x.ttag() == EXPR::DBL) {
    if (x.tag() == EXPR::APP) {
      // embedded int/float operation, recursively generate unboxed value
      Value *u = builtin_codegen(x);
      if (x.ttag() == EXPR::INT)
	return e.builder.CreateSIToFP(u, double_type());
      else
	return u;
    } else if (x.tag() == EXPR::INT)
      // integer constant, return "as is", promoted to double
      return Dbl((double)x.ival());
    else if (x.tag() == EXPR::DBL)
      // double constant, return "as is"
      return Dbl(x.dval());
    else if (x.ttag() == EXPR::INT) {
      // int variable, needs unboxing and double conversion
      assert(x.tag() == EXPR::VAR);
      Value *u = codegen(x);
      Value *p = e.builder.CreateBitCast(u, IntExprPtrTy, "intexpr");
      Value *v = e.CreateLoadGEP(p, Zero, ValFldIndex, "intval");
      v = e.builder.CreateSIToFP(v, double_type());
#if 0
      // collect the temporary, it's not needed any more
      call("pure_freenew", u);
#endif
      return v;
    } else {
      // double variable, needs unboxing
      assert(x.tag() == EXPR::VAR && x.ttag() == EXPR::DBL);
      Value *u = codegen(x);
      Value *p = e.builder.CreateBitCast(u, DblExprPtrTy, "dblexpr");
      Value *v = e.CreateLoadGEP(p, Zero, ValFldIndex, "dblval");
#if 0
      // collect the temporary, it's not needed any more
      call("pure_freenew", u);
#endif
      return v;
    }
  } else {
    // typeless expression; we have to check the computed value at runtime
    Value *u = codegen(x);
    // check that it's actually a double
    // NOTE: we don't want to promote int values to double here
    verify_tag(u, EXPR::DBL);
    // get the value
    Value *p = e.builder.CreateBitCast(u, DblExprPtrTy, "dblexpr");
    Value *v = e.CreateLoadGEP(p, Zero, ValFldIndex, "dblval");
    // collect the temporary, it's not needed any more
    call("pure_freenew", u);
    return v;
  }
}

Value *interpreter::builtin_codegen(expr x)
{
  // handle special cases which should be inlined for efficiency: mixed
  // arithmetic, comparisons, logical ops using unboxed integer and floating
  // point values
  Builder& b = act_builder();
  expr f; uint32_t n = count_args(x, f);
  assert((n == 1 || n == 2) && "error in type checker");
  if (n == 1 && x.ttag() == EXPR::INT) {
    // unary int operations
    Value *u = get_int(x.xval2());
    if (f.tag() == symtab.neg_sym().f)
      return b.CreateSub(Zero, u);
    else if (f.tag() == symtab.not_sym().f)
      return b.CreateZExt
	(b.CreateICmpEQ(Zero, u, "cmp"), int32_type());
    else if (f.tag() == symtab.bitnot_sym().f)
      return b.CreateXor(UInt(0xffffffff), u);
    else {
      assert(0 && "error in type checker");
      return 0;
    }
  } else if (n == 1 && x.ttag() == EXPR::DBL) {
    // unary double operations
    Value *u = get_double(x.xval2());
    if (f.tag() == symtab.neg_sym().f)
      return b.CreateSub(Dbl(0.0), u);
    else {
      assert(0 && "error in type checker");
      return 0;
    }
  } else if (n == 2 && x.ttag() == EXPR::INT &&
	     x.xval1().xval2().ttag() != EXPR::DBL &&
	     x.xval2().ttag() != EXPR::DBL) {
    // binary int operations
    Value *u = get_int(x.xval1().xval2());
    // these two need special treatment (short-circuit evaluation)
    if (f.tag() == symtab.or_sym().f) {
      Env& e = act_env();
      Value *condv = b.CreateICmpNE(u, Zero, "cond");
      BasicBlock *iftruebb = b.GetInsertBlock();
      BasicBlock *iffalsebb = basic_block("iffalse");
      BasicBlock *endbb = basic_block("end");
      b.CreateCondBr(condv, endbb, iffalsebb);
      e.f->getBasicBlockList().push_back(iffalsebb);
      b.SetInsertPoint(iffalsebb);
      Value *v = get_int(x.xval2());
#if DEBUG
      if (u->getType() != v->getType()) {
	std::cerr << "** operand mismatch!\n";
	std::cerr << "operator:      " << symtab.sym(f.tag()).s << '\n';
	std::cerr << "left operand:  "; u->dump();
	std::cerr << "right operand: "; v->dump();
	assert(0 && "operand mismatch");
      }
#endif
      Value *condv2 = b.CreateICmpNE(v, Zero, "cond");
      b.CreateBr(endbb);
      iffalsebb = b.GetInsertBlock();
      e.f->getBasicBlockList().push_back(endbb);
      b.SetInsertPoint(endbb);
      PHINode *phi = b.CreatePHI(int1_type(), "fi");
      phi->addIncoming(condv, iftruebb);
      phi->addIncoming(condv2, iffalsebb);
      return b.CreateZExt(phi, int32_type());
    } else if (f.tag() == symtab.and_sym().f) {
      Env& e = act_env();
      Value *condv = b.CreateICmpNE(u, Zero, "cond");
      BasicBlock *iffalsebb = b.GetInsertBlock();
      BasicBlock *iftruebb = basic_block("iftrue");
      BasicBlock *endbb = basic_block("end");
      b.CreateCondBr(condv, iftruebb, endbb);
      e.f->getBasicBlockList().push_back(iftruebb);
      b.SetInsertPoint(iftruebb);
      Value *v = get_int(x.xval2());
#if DEBUG
      if (u->getType() != v->getType()) {
	std::cerr << "** operand mismatch!\n";
	std::cerr << "operator:      " << symtab.sym(f.tag()).s << '\n';
	std::cerr << "left operand:  "; u->dump();
	std::cerr << "right operand: "; v->dump();
	assert(0 && "operand mismatch");
      }
#endif
      Value *condv2 = b.CreateICmpNE(v, Zero, "cond");
      b.CreateBr(endbb);
      iftruebb = b.GetInsertBlock();
      e.f->getBasicBlockList().push_back(endbb);
      b.SetInsertPoint(endbb);
      PHINode *phi = b.CreatePHI(int1_type(), "fi");
      phi->addIncoming(condv, iffalsebb);
      phi->addIncoming(condv2, iftruebb);
      return b.CreateZExt(phi, int32_type());
    } else {
      Value *v = get_int(x.xval2());
#if DEBUG
      if (u->getType() != v->getType()) {
	std::cerr << "** operand mismatch!\n";
	std::cerr << "operator:      " << symtab.sym(f.tag()).s << '\n';
	std::cerr << "left operand:  "; u->dump();
	std::cerr << "right operand: "; v->dump();
	assert(0 && "operand mismatch");
      }
#endif
      if (f.tag() == symtab.bitor_sym().f)
	return b.CreateOr(u, v);
      else if (f.tag() == symtab.bitand_sym().f)
	return b.CreateAnd(u, v);
      else if (f.tag() == symtab.shl_sym().f) {
	// result of shl is undefined if u>=#bits, return 0 in that case
	BasicBlock *okbb = basic_block("ok");
	BasicBlock *zerobb = b.GetInsertBlock();
	BasicBlock *endbb = basic_block("end");
	Value *cmp = b.CreateICmpULT(v, UInt(32));
	b.CreateCondBr(cmp, okbb, endbb);
	act_env().f->getBasicBlockList().push_back(okbb);
	b.SetInsertPoint(okbb);
	Value *ok = b.CreateShl(u, v);
	b.CreateBr(endbb);
	act_env().f->getBasicBlockList().push_back(endbb);
	b.SetInsertPoint(endbb);
	PHINode *phi = b.CreatePHI(int32_type());
	phi->addIncoming(ok, okbb);
	phi->addIncoming(Zero, zerobb);
	return phi;
      } else if (f.tag() == symtab.shr_sym().f)
	return b.CreateAShr(u, v);
      else if (f.tag() == symtab.less_sym().f)
	return b.CreateZExt
	  (b.CreateICmpSLT(u, v), int32_type());
      else if (f.tag() == symtab.greater_sym().f)
	return b.CreateZExt
	  (b.CreateICmpSGT(u, v), int32_type());
      else if (f.tag() == symtab.lesseq_sym().f)
	return b.CreateZExt
	  (b.CreateICmpSLE(u, v), int32_type());
      else if (f.tag() == symtab.greatereq_sym().f)
	return b.CreateZExt
	  (b.CreateICmpSGE(u, v), int32_type());
      else if (f.tag() == symtab.equal_sym().f)
	return b.CreateZExt
	  (b.CreateICmpEQ(u, v), int32_type());
      else if (f.tag() == symtab.notequal_sym().f)
	return b.CreateZExt
	  (b.CreateICmpNE(u, v), int32_type());
      else if (f.tag() == symtab.plus_sym().f)
	return b.CreateAdd(u, v);
      else if (f.tag() == symtab.minus_sym().f)
	return b.CreateSub(u, v);
      else if (f.tag() == symtab.mult_sym().f)
	return b.CreateMul(u, v);
      else if (f.tag() == symtab.div_sym().f) {
	// catch division by zero
	if (x.xval2().tag() == EXPR::INT && x.xval2().ival() == 0) {
	  b.CreateCall(module->getFunction("pure_sigfpe"));
	  return v;
	} else {
	  BasicBlock *okbb = basic_block("ok");
	  BasicBlock *errbb = basic_block("err");
	  Value *cmp = b.CreateICmpEQ(v, Zero);
	  b.CreateCondBr(cmp, errbb, okbb);
	  act_env().f->getBasicBlockList().push_back(errbb);
	  b.SetInsertPoint(errbb);
	  b.CreateCall(module->getFunction("pure_sigfpe"));
	  b.CreateRet(NullExprPtr);
	  act_env().f->getBasicBlockList().push_back(okbb);
	  b.SetInsertPoint(okbb);
	  return b.CreateSDiv(u, v);
	}
      } else if (f.tag() == symtab.mod_sym().f) {
	// catch division by zero
	if (x.xval2().tag() == EXPR::INT && x.xval2().ival() == 0) {
	  b.CreateCall(module->getFunction("pure_sigfpe"));
	  return v;
	} else {
	  BasicBlock *okbb = basic_block("ok");
	  BasicBlock *errbb = basic_block("err");
	  Value *cmp = b.CreateICmpEQ(v, Zero);
	  b.CreateCondBr(cmp, errbb, okbb);
	  act_env().f->getBasicBlockList().push_back(errbb);
	  b.SetInsertPoint(errbb);
	  b.CreateCall(module->getFunction("pure_sigfpe"));
	  b.CreateRet(NullExprPtr);
	  act_env().f->getBasicBlockList().push_back(okbb);
	  b.SetInsertPoint(okbb);
	  return b.CreateSRem(u, v);
	}
      } else {
	assert(0 && "error in type checker");
	return 0;
      }
    }
  } else {
    // binary int/double operations
    Value *u = get_double(x.xval1().xval2()),
      *v = get_double(x.xval2());
#if DEBUG
    if (u->getType() != v->getType()) {
      std::cerr << "** operand mismatch!\n";
      std::cerr << "operator:      " << symtab.sym(f.tag()).s << '\n';
      std::cerr << "left operand:  "; u->dump();
      std::cerr << "right operand: "; v->dump();
      assert(0 && "operand mismatch");
    }
#endif
    if (f.tag() == symtab.less_sym().f)
      return b.CreateZExt
	(b.CreateFCmpOLT(u, v), int32_type());
    else if (f.tag() == symtab.greater_sym().f)
      return b.CreateZExt
	(b.CreateFCmpOGT(u, v), int32_type());
    else if (f.tag() == symtab.lesseq_sym().f)
      return b.CreateZExt
	(b.CreateFCmpOLE(u, v), int32_type());
    else if (f.tag() == symtab.greatereq_sym().f)
      return b.CreateZExt
	(b.CreateFCmpOGE(u, v), int32_type());
    else if (f.tag() == symtab.equal_sym().f)
      return b.CreateZExt
	(b.CreateFCmpOEQ(u, v), int32_type());
    else if (f.tag() == symtab.notequal_sym().f)
      return b.CreateZExt
	(b.CreateFCmpONE(u, v), int32_type());
    else if (f.tag() == symtab.plus_sym().f)
      return b.CreateAdd(u, v);
    else if (f.tag() == symtab.minus_sym().f)
      return b.CreateSub(u, v);
    else if (f.tag() == symtab.mult_sym().f)
      return b.CreateMul(u, v);
    else if (f.tag() == symtab.fdiv_sym().f)
      return b.CreateFDiv(u, v);
    else {
      assert(0 && "error in type checker");
      return 0;
    }
  }
}

Value *interpreter::external_funcall(int32_t tag, uint32_t n, expr x)
{
  // check for a saturated external function call
  map<int32_t,ExternInfo>::const_iterator it = externals.find(tag);
  if (it == externals.end()) return 0;
  const ExternInfo& info = it->second;
  if (info.argtypes.size() != n) return 0;
  // bingo! saturated call
  // collect arguments
  size_t i = 0;
  expr u, v;
  vector<expr> args(n);
  while (x.is_app(u, v)) {
    args[n-++i] = v; x = u;
  }
  vector<Value*> argv(n);
  if (n>0) {
    for (i = 0; i < n; i++)
      argv[i] = codegen(args[i]);
    if (n == 1)
      act_env().CreateCall(module->getFunction("pure_push_arg"), argv);
    else {
      vector<Value*> argv1;
      argv1.push_back(UInt(n));
      argv1.push_back(Zero);
      argv1.insert(argv1.end(), argv.begin(), argv.end());
      act_env().CreateCall(module->getFunction("pure_push_args"), argv1);
    }
  }
  return act_env().CreateCall(info.f, argv);
}

Value *interpreter::funcall(Env *f, uint32_t n, expr x)
{
  // check for a saturated global function call
  if (f->n == n) {
    // bingo! saturated call
    assert(f->m == 0);
    vector<Value*> y(n);
    vector<Value*> z;
    // collect arguments
    size_t i = 0;
    expr u, v;
    vector<expr> args(n);
    while (x.is_app(u, v)) {
      args[n-++i] = v; x = u;
    }
    for (i = 0; i < n; i++)
      y[i] = codegen(args[i]);
    // no environment here
    return fcall(*f, y, z);
  } else
    return 0;
}

Value *interpreter::funcall(Env *f, Value *x)
{
  // same as above, but for a local anonymous closure which takes a single
  // argument already codegen'ed
  assert(f->n == 1);
  assert(f->m == f->xtab.size());
  vector<Value*> y(1);
  vector<Value*> z(f->m);
  // collect arguments
  y[0] = x;
  // collect the environment
  list<VarInfo>::iterator info;
  size_t i;
  for (i = 0, info = f->xtab.begin(); info != f->xtab.end();
       i++, info++)
    z[i] = vref(info->vtag, info->idx-1, info->p);
  return fcall(*f, y, z);
}

Value *interpreter::funcall(int32_t tag, uint8_t idx, uint32_t n, expr x)
{
  // check for a saturated local function call
  Env *f;
  int offs = idx-1;
  if (idx == 0) {
    // function in current environment ('with'-bound)
    f = act_env().fmap.act()[tag];
  } else {
    // function in an outer environment, the de Bruijn index idx tells us
    // where on the current environment stack it's at
    EnvStack::iterator e = envstk.begin();
    size_t i = idx;
    for (; i > 0; e++, i--) assert(e != envstk.end());
    // look up the function in the environment
    f = (*e)->fmap.act()[tag];
  }
  if (f->n == n) {
    // bingo! saturated call
    assert(f->m == f->xtab.size());
    vector<Value*> y(f->n);
    vector<Value*> z(f->m);
    // collect arguments
    size_t i = 0;
    expr u, v;
    vector<expr> args(n);
    while (x.is_app(u, v)) {
      args[n-++i] = v; x = u;
    }
    for (i = 0; i < n; i++)
      y[i] = codegen(args[i]);
    // collect the environment
    list<VarInfo>::iterator info;
    for (i = 0, info = f->xtab.begin(); info != f->xtab.end(); i++, info++)
      z[i] = vref(info->vtag, info->idx+offs, info->p);
    return fcall(*f, y, z);
  } else
    return 0;
}

/* Experimental support for tail-recursive logical operators (&& and ||). This
   works, but is inherently broken (e.g., 0||-1 might return either -1 or 1,
   depending on whether the code is TCO'ed or not). Never use this. */
#define TAILOPS 0

void interpreter::toplevel_codegen(expr x, const rule *rp)
{
  if (x.is_null()) {
    /* Result of failed guard. This can only occur at the toplevel. */
    act_env().CreateRet(NullExprPtr, rp);
    return;
  }
#if USE_FASTCC
  if (x.tag() == EXPR::COND) {
    toplevel_cond(x.xval1(), x.xval2(), x.xval3(), rp);
    return;
  }
  if (x.tag() == EXPR::COND1) {
    toplevel_cond(x.xval1(), x.xval2(), expr(), rp);
    return;
  }
  Env& e = act_env();
#if TAILOPS
  Builder& b = act_builder();
  expr f; uint32_t n = count_args(x, f);
  if (n == 2 && x.ttag() == EXPR::INT &&
      x.xval1().xval2().ttag() != EXPR::DBL &&
      x.xval2().ttag() != EXPR::DBL) {
    if (f.tag() == symtab.or_sym().f) {
      Value *u = get_int(x.xval1().xval2());
      Value *condv = b.CreateICmpNE(u, Zero, "cond");
      BasicBlock *iftruebb = basic_block("iftrue");
      BasicBlock *iffalsebb = basic_block("iffalse");
      b.CreateCondBr(condv, iftruebb, iffalsebb);
      e.f->getBasicBlockList().push_back(iftruebb);
      b.SetInsertPoint(iftruebb);
      e.CreateRet(ibox(One), rp);
      e.f->getBasicBlockList().push_back(iffalsebb);
      b.SetInsertPoint(iffalsebb);
      toplevel_codegen(x.xval2(), rp);
    } else if (f.tag() == symtab.and_sym().f) {
      Value *u = get_int(x.xval1().xval2());
      Value *condv = b.CreateICmpNE(u, Zero, "cond");
      BasicBlock *iftruebb = basic_block("iftrue");
      BasicBlock *iffalsebb = basic_block("iffalse");
      b.CreateCondBr(condv, iftruebb, iffalsebb);
      e.f->getBasicBlockList().push_back(iffalsebb);
      b.SetInsertPoint(iffalsebb);
      e.CreateRet(ibox(Zero), rp);
      e.f->getBasicBlockList().push_back(iftruebb);
      b.SetInsertPoint(iftruebb);
      toplevel_codegen(x.xval2(), rp);
    } else
      e.CreateRet(codegen(x), rp);
  } else
#endif
    e.CreateRet(codegen(x), rp);
#else
  act_env().CreateRet(codegen(x), rp);
#endif
}

static int32_t const_vect(exprl xs)
{
  if (xs.empty()) return EXPR::INT;
  int32_t t = xs.begin()->tag();
  if (t != EXPR::INT && t != EXPR::BIGINT && t != EXPR::DBL && t != EXPR::STR)
    return 0;
  for (exprl::iterator it = xs.begin(), end = xs.end(); it != end; it++)
    if (it->tag() != t)
      return 0;
  return t;
}

/* Alternative code for list, tuple and matrix values, which considerably
   speeds up compilation for larger aggregates. See the comments at the
   beginning of interpreter.hh for details. */

Value *interpreter::list_codegen(expr x)
{
#if LIST_OPT>0
  exprl xs;
  expr tl;
  if ((x.is_list2(xs, tl) || x.is_tuple(xs)) &&
      xs.size() >= LIST_OPT) {
    Value *v;
    size_t i = 0, n = xs.size();
    int32_t ttag = const_vect(xs);
    if (ttag==EXPR::INT || ttag==EXPR::DBL) {
      /* Optimize the case of lists and tuples of ints or doubles. These
	 can be coded directly as array constants. */
      const char * tuplev_fun = ttag==EXPR::INT?"pure_inttuplev":
	"pure_doubletuplev";
      const char * listv_fun = ttag==EXPR::INT?"pure_intlistv":
	"pure_doublelistv";
      const char * listv2_fun = ttag==EXPR::INT?"pure_intlistv2":
	"pure_doublelistv2";
      vector<Constant*> c(n);
      if (ttag==EXPR::INT)
	for (exprl::iterator it = xs.begin(), end = xs.end(); it != end;
	     it++)
	  c[i++] = SInt(it->ival());
      else
	for (exprl::iterator it = xs.begin(), end = xs.end(); it != end;
	     it++)
	  c[i++] = Dbl(it->dval());
      Value *p;
      if (ttag==EXPR::INT) {
	Constant *a = ConstantArray::get
	  (ArrayType::get(int32_type(), n), c);
	GlobalVariable *w = global_variable
	  (module, ArrayType::get(int32_type(), n), true,
	   GlobalVariable::InternalLinkage, a, "$$intv");
	p = act_env().CreateGEP(w, Zero, Zero);
      } else {
	Constant *a = ConstantArray::get
	  (ArrayType::get(double_type(), n), c);
	GlobalVariable *w = global_variable
	  (module, ArrayType::get(double_type(), n), true,
	   GlobalVariable::InternalLinkage, a, "$$doublev");
	p = act_env().CreateGEP(w, Zero, Zero);
      }
      Value *u = 0;
      if (!x.is_pair() && tl.tag() != symtab.nil_sym().f)
	u = codegen(tl);
      vector<Value*> args;
      args.push_back(SizeInt(n));
      args.push_back(p);
      if (u == 0)
	v = act_env().CreateCall
	  (module->getFunction(x.is_pair()?tuplev_fun:listv_fun), args);
      else {
	args.push_back(u);
	v = act_env().CreateCall(module->getFunction(listv2_fun), args);
      }
      return v;
    } else if (ttag==EXPR::BIGINT) {
      /* Optimize the case of bigint lists and tuples. These are coded
	 as a collection of array constants containing the limbs of all
	 elements, as well as offsets into the limb array and the bigint
	 sizes with signs. */
      const char * tuplev_fun = "pure_biginttuplev";
      const char * listv_fun = "pure_bigintlistv";
      const char * listv2_fun = "pure_bigintlistv2";
      vector<Constant*> c, offs(n), sz(n);
      size_t k = 0;
      for (exprl::iterator it = xs.begin(), end = xs.end(); it != end;
	   it++) {
	const mpz_t& z = it->zval();
	size_t m = (size_t)(z->_mp_size>=0 ? z->_mp_size : -z->_mp_size);
	sz[i] = SInt((int32_t)z->_mp_size);
	offs[i] = UInt((uint32_t)k);
	vector<Constant*> u(m);
	if (sizeof(mp_limb_t) == 8)
	  for (size_t j = 0; j < m; j++) u[j] = UInt64(z->_mp_d[j]);
	else
	  for (size_t j = 0; j < m; j++) u[j] = UInt(z->_mp_d[j]);
	c.insert(c.end(), u.begin(), u.end());
	i++; k += m;
      }
      Constant *a = ConstantArray::get
	(ArrayType::get((sizeof(mp_limb_t) == 8)?int64_type()
			:int32_type(), k), c);
      GlobalVariable *w = global_variable
	(module, ArrayType::get((sizeof(mp_limb_t) == 8)?int64_type()
				:int32_type(), k), true,
	 GlobalVariable::InternalLinkage, a, "$$bigintv");
      Constant *offs_a = ConstantArray::get
	(ArrayType::get(int32_type(), n), offs);
      GlobalVariable *offs_w = global_variable
	(module, ArrayType::get(int32_type(), n), true,
	 GlobalVariable::InternalLinkage, offs_a, "$$bigintv_offs");
      Constant *sz_a = ConstantArray::get
	(ArrayType::get(int32_type(), n), sz);
      GlobalVariable *sz_w = global_variable
	(module, ArrayType::get(int32_type(), n), true,
	 GlobalVariable::InternalLinkage, sz_a, "$$bigintv_sz");
      Value *p = act_env().CreateGEP(w, Zero, Zero);
      Value *offs_p = act_env().CreateGEP(offs_w, Zero, Zero);
      Value *sz_p = act_env().CreateGEP(sz_w, Zero, Zero);
      Value *u = 0;
      if (!x.is_pair() && tl.tag() != symtab.nil_sym().f)
	u = codegen(tl);
      vector<Value*> args;
      args.push_back(SizeInt(n));
      args.push_back(p);
      args.push_back(offs_p);
      args.push_back(sz_p);
      if (u == 0)
	v = act_env().CreateCall
	  (module->getFunction(x.is_pair()?tuplev_fun:listv_fun), args);
      else {
	args.push_back(u);
	v = act_env().CreateCall(module->getFunction(listv2_fun), args);
      }
      return v;
    } else if (ttag==EXPR::STR) {
      /* Optimize the case of string lists and tuples. These are coded as a
	 single char array containing all (0-terminated) strings, together
	 with an offset table. */
      const char * tuplev_fun = "pure_strtuplev";
      const char * listv_fun = "pure_strlistv";
      const char * listv2_fun = "pure_strlistv2";
      vector<Constant*> c, offs(n);
      size_t k = 0;
      for (exprl::iterator it = xs.begin(), end = xs.end(); it != end;
	   it++) {
	const char *s = it->sval();
	size_t m = strlen(s)+1;
	offs[i] = UInt((uint32_t)k);
	vector<Constant*> u(m);
	for (size_t j = 0; j < m; j++) u[j] = Char(s[j]);
	c.insert(c.end(), u.begin(), u.end());
	i++; k += m;
      }
      Constant *a = ConstantArray::get
	(ArrayType::get(int8_type(), k), c);
      GlobalVariable *w = global_variable
	(module, ArrayType::get(int8_type(), k), true,
	 GlobalVariable::InternalLinkage, a, "$$strv");
      Constant *offs_a = ConstantArray::get
	(ArrayType::get(int32_type(), n), offs);
      GlobalVariable *offs_w = global_variable
	(module, ArrayType::get(int32_type(), n), true,
	 GlobalVariable::InternalLinkage, offs_a, "$$strv_offs");
      Value *p = act_env().CreateGEP(w, Zero, Zero);
      Value *offs_p = act_env().CreateGEP(offs_w, Zero, Zero);
      Value *u = 0;
      if (!x.is_pair() && tl.tag() != symtab.nil_sym().f)
	u = codegen(tl);
      vector<Value*> args;
      args.push_back(SizeInt(n));
      args.push_back(p);
      args.push_back(offs_p);
      if (u == 0)
	v = act_env().CreateCall
	  (module->getFunction(x.is_pair()?tuplev_fun:listv_fun), args);
      else {
	args.push_back(u);
	v = act_env().CreateCall(module->getFunction(listv2_fun), args);
      }
      return v;
    }
    Value *p = act_builder().CreateCall
      (module->getFunction("malloc"), SizeInt(n*sizeof(pure_expr*)));
    Value *a = act_builder().CreateBitCast(p, ExprPtrPtrTy);
    for (exprl::iterator it = xs.begin(), end = xs.end(); it != end;
	 it++) {
      Value *v = codegen(*it);
      Value *idx[1];
      idx[0] = UInt(i++);
      act_builder().CreateStore
	(v, act_builder().CreateGEP(a, idx, idx+1));
    }
    Value *u = 0;
    if (!x.is_pair() && tl.tag() != symtab.nil_sym().f)
      u = codegen(tl);
    vector<Value*> args;
    args.push_back(SizeInt(n));
    args.push_back(a);
    if (u == 0)
      v = act_env().CreateCall
	(module->getFunction(x.is_pair()?"pure_tuplev":"pure_listv"),
	 args);
    else {
      args.push_back(u);
      v = act_env().CreateCall
	(module->getFunction("pure_listv2"), args);
    }
    act_builder().CreateCall(module->getFunction("free"), p);
    return v;
  } else
#endif
    return 0;
}

static bool is_complex(interpreter& interp, expr x, double& a, double& b)
{
  if (x.tag() != EXPR::APP) return false;
  expr u = x.xval1(), v = x.xval2();
  if (u.tag() == EXPR::APP) {
    expr f = u.xval1();
    symbol &rect = interp.symtab.complex_rect_sym(),
      &polar = interp.symtab.complex_polar_sym();
    if (f.tag() != rect.f && f.tag() != polar.f)
      return false;
    u = u.xval2();
    switch (u.tag()) {
    case EXPR::INT:
      a = (double)u.ival();
      break;
    case EXPR::DBL:
      a = u.dval();
      break;
    default:
      return false;
    }
    switch (v.tag()) {
    case EXPR::INT:
      b = (double)v.ival();
      break;
    case EXPR::DBL:
      b = v.dval();
      break;
    default:
      return false;
    }
    if (f.tag() == polar.f) {
      double r = a, t = b;
      a = r*cos(t); b = r*sin(t);
    }
    return true;
  } else
    return false;
}

static inline int32_t matrix_tag(interpreter& interp, expr x)
{
  switch (x.tag()) {
  case EXPR::INT: return EXPR::IMATRIX;
  case EXPR::DBL: return EXPR::DMATRIX;
  case EXPR::BIGINT: return EXPR::BIGINT;
  case EXPR::STR: return EXPR::STR;
  default: {
    double a, b;
    if (is_complex(interp, x, a, b))
      return EXPR::CMATRIX;
    else
      return 0;
  }
  }
}

static bool is_const_matrix(interpreter& interp,
			    exprll xs, int32_t& t, size_t& n, size_t& m)
{
#if LIST_OPT>0
  // We don't allow empty matrices here.
  if (xs.empty() || xs.front().empty()) return false;
  n = xs.size(); m = xs.front().size();
  // Also skip small matrices.
  if (n*m < LIST_OPT) return false;
  t = matrix_tag(interp, xs.front().front());
  if (t == 0) return false;
  for (exprll::iterator it = xs.begin(), end = xs.end();
       it != end; it++) {
    if (it->size() != m) return false;
    for (exprl::iterator jt = it->begin(), end = it->end();
	 jt != end; jt++) {
      if (matrix_tag(interp, *jt) != t)
	return false;
    }
  }
  return true;
#else
  return false;
#endif
}

Value *interpreter::matrix_codegen(expr x)
{
#if LIST_OPT>0
  exprll& xs = *x.xvals();
  int32_t ttag = 0;
  size_t n = 0, m = 0;
  if (is_const_matrix(*this, xs, ttag, n, m)) {
    Value *v;
    size_t i = 0;
    if (ttag==EXPR::IMATRIX || ttag==EXPR::DMATRIX || ttag==EXPR::CMATRIX) {
      /* Optimize the case of numeric matrices. These can be coded directly as
	 array constants. */
      const char *matrix_fun =
	ttag==EXPR::IMATRIX?"matrix_from_int_array_nodup":
	ttag==EXPR::DMATRIX?"matrix_from_double_array_nodup":
	"matrix_from_complex_array_nodup";
      size_t N = n*m;
      if (ttag==EXPR::CMATRIX) N *= 2;
      vector<Constant*> c(N);
      if (ttag==EXPR::IMATRIX)
	for (exprll::iterator it = xs.begin(), end = xs.end(); it != end; it++)
	  for (exprl::iterator jt = it->begin(), end = it->end();
	       jt != end; jt++)
	    c[i++] = SInt(jt->ival());
      else if (ttag==EXPR::DMATRIX)
	for (exprll::iterator it = xs.begin(), end = xs.end(); it != end; it++)
	  for (exprl::iterator jt = it->begin(), end = it->end();
	       jt != end; jt++)
	    c[i++] = Dbl(jt->dval());
      else
	for (exprll::iterator it = xs.begin(), end = xs.end(); it != end; it++)
	  for (exprl::iterator jt = it->begin(), end = it->end();
	       jt != end; jt++) {
	    double a, b;
	    if (is_complex(*this, *jt, a, b)) {
	      c[i++] = Dbl(a);
	      c[i++] = Dbl(b);
	    } else {
	      assert(0 && "This can't happen!");
	    }
	  }
      Value *p;
      if (ttag==EXPR::IMATRIX) {
	Constant *a = ConstantArray::get
	  (ArrayType::get(int32_type(), N), c);
	GlobalVariable *w = global_variable
	  (module, ArrayType::get(int32_type(), N), true,
	   GlobalVariable::InternalLinkage, a, "$$intv");
	p = act_env().CreateGEP(w, Zero, Zero);
      } else {
	Constant *a = ConstantArray::get
	  (ArrayType::get(double_type(), N), c);
	GlobalVariable *w = global_variable
	  (module, ArrayType::get(double_type(), N), true,
	   GlobalVariable::InternalLinkage, a, "$$doublev");
	p = act_env().CreateGEP(w, Zero, Zero);
      }
      vector<Value*> args;
      args.push_back(SInt(n));
      args.push_back(SInt(m));
      args.push_back(act_builder().CreateBitCast(p, VoidPtrTy));
      v = act_env().CreateCall(module->getFunction(matrix_fun), args);
      return v;
    } else if (ttag==EXPR::BIGINT) {
      /* Optimize the case of bigint matrices. These are coded as a collection
	 of array constants containing the limbs of all elements, as well as
	 offsets into the limb array and the bigint sizes with signs. */
      size_t N = n*m;
      vector<Constant*> c, offs(N), sz(N);
      size_t k = 0;
      for (exprll::iterator it = xs.begin(), end = xs.end(); it != end; it++)
	for (exprl::iterator jt = it->begin(), end = it->end();
	     jt != end; jt++) {
	const mpz_t& z = jt->zval();
	size_t m = (size_t)(z->_mp_size>=0 ? z->_mp_size : -z->_mp_size);
	sz[i] = SInt((int32_t)z->_mp_size);
	offs[i] = UInt((uint32_t)k);
	vector<Constant*> u(m);
	if (sizeof(mp_limb_t) == 8)
	  for (size_t j = 0; j < m; j++) u[j] = UInt64(z->_mp_d[j]);
	else
	  for (size_t j = 0; j < m; j++) u[j] = UInt(z->_mp_d[j]);
	c.insert(c.end(), u.begin(), u.end());
	i++; k += m;
      }
      Constant *a = ConstantArray::get
	(ArrayType::get((sizeof(mp_limb_t) == 8)?int64_type()
			:int32_type(), k), c);
      GlobalVariable *w = global_variable
	(module, ArrayType::get((sizeof(mp_limb_t) == 8)?int64_type()
				:int32_type(), k), true,
	 GlobalVariable::InternalLinkage, a, "$$bigintv");
      Constant *offs_a = ConstantArray::get
	(ArrayType::get(int32_type(), N), offs);
      GlobalVariable *offs_w = global_variable
	(module, ArrayType::get(int32_type(), N), true,
	 GlobalVariable::InternalLinkage, offs_a, "$$bigintv_offs");
      Constant *sz_a = ConstantArray::get
	(ArrayType::get(int32_type(), N), sz);
      GlobalVariable *sz_w = global_variable
	(module, ArrayType::get(int32_type(), N), true,
	 GlobalVariable::InternalLinkage, sz_a, "$$bigintv_sz");
      Value *p = act_env().CreateGEP(w, Zero, Zero);
      Value *offs_p = act_env().CreateGEP(offs_w, Zero, Zero);
      Value *sz_p = act_env().CreateGEP(sz_w, Zero, Zero);
      vector<Value*> args;
      args.push_back(SizeInt(n));
      args.push_back(SizeInt(m));
      args.push_back(p);
      args.push_back(offs_p);
      args.push_back(sz_p);
      v = act_env().CreateCall
	(module->getFunction("pure_bigintmatrixv"), args);
      return v;
    } else if (ttag==EXPR::STR) {
      /* Optimize the case of string lists and tuples. These are coded as a
	 single char array containing all (0-terminated) strings, together
	 with an offset table. */
      size_t N = n*m;
      vector<Constant*> c, offs(N);
      size_t k = 0;
      for (exprll::iterator it = xs.begin(), end = xs.end(); it != end; it++)
	for (exprl::iterator jt = it->begin(), end = it->end();
	     jt != end; jt++) {
	const char *s = jt->sval();
	size_t m = strlen(s)+1;
	offs[i] = UInt((uint32_t)k);
	vector<Constant*> u(m);
	for (size_t j = 0; j < m; j++) u[j] = Char(s[j]);
	c.insert(c.end(), u.begin(), u.end());
	i++; k += m;
      }
      Constant *a = ConstantArray::get
	(ArrayType::get(int8_type(), k), c);
      GlobalVariable *w = global_variable
	(module, ArrayType::get(int8_type(), k), true,
	 GlobalVariable::InternalLinkage, a, "$$strv");
      Constant *offs_a = ConstantArray::get
	(ArrayType::get(int32_type(), N), offs);
      GlobalVariable *offs_w = global_variable
	(module, ArrayType::get(int32_type(), N), true,
	 GlobalVariable::InternalLinkage, offs_a, "$$strv_offs");
      Value *p = act_env().CreateGEP(w, Zero, Zero);
      Value *offs_p = act_env().CreateGEP(offs_w, Zero, Zero);
      vector<Value*> args;
      args.push_back(SizeInt(n));
      args.push_back(SizeInt(m));
      args.push_back(p);
      args.push_back(offs_p);
      v = act_env().CreateCall(module->getFunction("pure_strmatrixv"), args);
      return v;
    } else
      return 0;
  } else
#endif
    return 0;
}

Value *interpreter::codegen(expr x, bool quote)
{
  if (x.is_null()) return NullExprPtr;
  switch (x.tag()) {
  // local variable:
  case EXPR::VAR:
    return vref(x.vtag(), x.vidx(), x.vpath());
  // local function:
  case EXPR::FVAR:
    return fref(x.vtag(), x.vidx());
  // constants:
  case EXPR::INT:
    return ibox(x.ival());
  case EXPR::BIGINT:
    return zbox(x.zval());
  case EXPR::DBL:
    return dbox(x.dval());
  case EXPR::STR:
    return sbox(x.sval());
  case EXPR::PTR:
    // We only need to support null pointers here, other constant pointer
    // values are handled in the EXPR::WRAP case below.
    assert(x.pval() == 0);
    return pbox(x.pval());
  case EXPR::WRAP: {
    assert(x.pval());
    GlobalVar *v = (GlobalVar*)x.pval();
    return act_builder().CreateLoad(v->v);
  }
  // matrix:
  case EXPR::MATRIX: {
#if LIST_OPT>0
    // special code for matrix constants
    Value *w = matrix_codegen(x);
    if (w) return w;
#endif
    Function *row_fun = 0, *col_fun = 0;
    if (quote) {
      row_fun = module->getFunction("pure_matrix_rowsq");
      col_fun = module->getFunction("pure_matrix_columnsq");
    } else {
      row_fun = module->getFunction("pure_matrix_rows");
      col_fun = module->getFunction("pure_matrix_columns");
    }
    size_t n = x.xvals()->size(), i = 1;
    vector<Value*> us(n+1);
    us[0] = UInt(n);
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++, i++) {
      size_t m = xs->size(), j = 1;
      vector<Value*> vs(m+1);
      vs[0] = UInt(m);
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++, j++) {
	vs[j] = codegen(*ys, quote);
      }
      us[i] =
	act_env().CreateCall(col_fun, vs);
    }
    return act_env().CreateCall(row_fun, us);
  }
  // application:
  case EXPR::APP:
    if (quote) {
      // quoted function application
      Value *u = codegen(x.xval1(), true), *v = codegen(x.xval2(), true);
      return applc(u, v);
    } else if (x.ttag() != 0) {
      // inlined unboxed builtin int/float operations
      assert(x.ttag() == EXPR::INT || x.ttag() == EXPR::DBL);
      if (x.ttag() == EXPR::INT)
	return ibox(builtin_codegen(x));
      else
	return dbox(builtin_codegen(x));
    } else {
      /* Optimization of saturated applications. It is safe to emit a direct
	 call for these if the head of the application is a symbol denoting
	 either an external C function, a local function, or a global function
	 which is called recursively (i.e., inside its own definition). Note
	 that if a global symbol is applied outside its own definition, we
	 *always* have to box it even if it is currently known to be a
	 function, since the definition may change at any time in an
	 interactive session. */
      expr f; uint32_t n = count_args(x, f);
      Value *v; Env *e;
      if (f.tag() == EXPR::FVAR && (v = funcall(f.vtag(), f.vidx(), n, x)))
	// local function call
	return v;
      else if (f.tag() > 0 && (v = external_funcall(f.tag(), n, x)))
	// external function call
	return v;
      else if (f.tag() > 0 && (e = find_stacked(f.tag())) &&
	       (v = funcall(e, n, x)))
	// recursive call to a global function
	return v;
      else if (n == 2 && f.tag() == symtab.seq_sym().f) {
	// sequence operator
	Value *u = codegen(x.xval1().xval2());
	act_builder().CreateCall(module->getFunction("pure_freenew"), u);
	return codegen(x.xval2());
      } else if (n == 1 && is_quote(f.tag())) {
	// quoted subterm
	return codegen(x.xval2(), true);
      } else if (n == 1 && f.tag() == symtab.amp_sym().f) {
	// create a thunk (parameterless anonymous closure)
	expr y = x.xval2();
	Env& act = act_env();
	assert(act.fmap.act().find(-x.hash()) != act.fmap.act().end());
	Env& e = *act.fmap.act()[-x.hash()];
	push("&", &e);
	fun_prolog("anonymous");
	e.CreateRet(codegen(y));
	fun_finish();
	pop(&e);
	Value *body = fbox(e);
	return body;
      } else if (n == 2 && f.tag() == symtab.catch_sym().f) {
	// catch an exception; create a little anonymous closure to be called
	// through pure_catch()
	expr h = x.xval1().xval2(), y = x.xval2();
	Env& act = act_env();
	assert(act.fmap.act().find(-x.hash()) != act.fmap.act().end());
	Env& e = *act.fmap.act()[-x.hash()];
	push("catch", &e);
	fun_prolog("anonymous");
	e.CreateRet(codegen(y));
	fun_finish();
	pop(&e);
	Value *handler = codegen(h), *body = fbox(e);
	vector<Value*> argv;
	argv.push_back(Two);
	argv.push_back(handler);
	argv.push_back(body);
	act_env().CreateCall(module->getFunction("pure_new_args"), argv);
	return call("pure_catch", handler, body);
      } else {
#if LIST_OPT>0
	// special code for lists and tuples
	Value *w = list_codegen(x);
	if (w) return w;
#endif
	// ordinary function application
	Value *u = codegen(x.xval1()), *v = codegen(x.xval2());
	return apply(u, v);
      }
    }
  // conditional:
  case EXPR::COND:
    return cond(x.xval1(), x.xval2(), x.xval3());
  case EXPR::COND1:
    return cond(x.xval1(), x.xval2(), expr());
  // anonymous closure:
  case EXPR::LAMBDA: {
    Env& act = act_env();
    assert(act.fmap.act().find(-x.hash()) != act.fmap.act().end());
    Env& e = *act.fmap.act()[-x.hash()];
    push("lambda", &e);
    fun("anonymous", x.pm(), true);
    pop(&e);
    return fbox(e);
  }
  // other nested environments:
  case EXPR::CASE: {
    // case expression: treated like an anonymous closure (see the lambda case
    // above) which gets applied to the subject term to be matched
    Env& act = act_env();
    assert(act.fmap.act().find(-x.hash()) != act.fmap.act().end());
    Env& e = *act.fmap.act()[-x.hash()];
    push("case", &e);
    fun("anonymous", x.pm(), true);
    pop(&e);
    return funcall(&e, codegen(x.xval()));
  }
  case EXPR::WHEN: {
    // when expression: this is essentially a nested case expression
    // x when y1 = z1; ...; yn = zn end ==>
    // case z1 of y1 = ... case zn of yn = x end ... end ==>
    // (\y1->...((\yn->x) zn)...) z1
    // we do this translation here on the fly (cf. when_codegen() above)
    return when_codegen(x.xval(), x.pm(),
			x.rules()->begin(), x.rules()->end());
  }
  case EXPR::WITH: {
    // collection of locally bound closures; this is similar to the 'case'
    // expression, but has a nontrivial function environment around it inside
    // of which the subject term is evaluated
    env *fe = x.fenv();
    env::const_iterator p;
    Env& act = act_env();
    // first create all function entry points, so that we properly handle
    // mutually recursive definitions
    act.fmap.push();
    for (p = fe->begin(); p != fe->end(); p++) {
      int32_t ftag = p->first;
      assert(act.fmap.act().find(ftag) != act.fmap.act().end());
      Env& e = *act.fmap.act()[ftag];
      push("with", &e);
      act.fmap.act()[ftag]->f = fun_prolog(symtab.sym(ftag).s);
      pop(&e);
    }
    for (p = fe->begin(); p != fe->end(); p++) {
      int32_t ftag = p->first;
      const env_info& info = p->second;
      Env& e = *act.fmap.act()[ftag];
      push("with", &e);
      fun_body(info.m);
      pop(&e);
    }
    Value *v = codegen(x.xval());
    act.fmap.pop();
    return v;
  }
  default: {
    assert(x.tag() > 0);
    // quoted symbols
    if (quote)
      return act_builder().CreateCall
	(module->getFunction("pure_const"), SInt(x.tag()));
    // check for a call to the '__locals__' builtin
    if (x.tag() == symtab.locals_sym().f) {
      // enumerate all local functions visible in the current environment
      vector<Value*> argv;
      set<int32_t> done;
      size_t n = 0;
      argv.push_back(Zero);
      for (Env *e = &act_env(); e; e = e->parent) {
	EnvMap& m = e->fmap.act();
	for (EnvMap::iterator it = m.begin(), end = m.end(); it != end; ++it) {
	  int32_t fno = it->first;
	  if (fno <= 0 || done.find(fno) != done.end()) continue;
	  Env& f = *it->second;
	  argv.push_back(SInt(fno));
	  argv.push_back(fbox(f));
	  done.insert(fno); n++;
	}
      }
      argv[0] = UInt(n);
      return act_builder().CreateCall
	(module->getFunction("pure_locals"), argv.begin(), argv.end());
    }
    // check for a parameterless global (or external) function call
    Value *u; Env *e;
    if ((u = external_funcall(x.tag(), 0, x)))
      // external function call
      return u;
    else if ((e = find_stacked(x.tag())) && (u = funcall(e, 0, x)))
      // recursive call of a global function
      return u;
    // check for an external with parameters
    map<int32_t,ExternInfo>::const_iterator it = externals.find(x.tag());
    if (it != externals.end()) {
      const ExternInfo& info = it->second;
      vector<Value*> env;
      // build an fbox for the external
      return call("pure_clos", false, x.tag(), 0, info.f,
		  NullPtr, info.argtypes.size(), env);
    }
    // check for an existing global variable
    map<int32_t,GlobalVar>::iterator v = globalvars.find(x.tag());
    if (v != globalvars.end()) {
      Value *u = act_builder().CreateLoad(v->second.v);
#if DEBUG>2
      string msg = "codegen: global "+symtab.sym(x.tag()).s+" -> %p -> %p";
      debug(msg.c_str(), v->second.v, u);
#endif
      return call(u);
    }
    // not bound yet, return a cbox
    return call(cbox(x.tag()));
  }
  }
}

// Function boxes. These also take care of capturing the environment so that
// it can be passed as extra parameters when the function actually gets
// invoked.

Value *interpreter::fbox(Env& f)
{
  assert(f.f);
  // build extra parameters for the captured environment
  assert(f.m == f.xtab.size());
  vector<Value*> x(f.m);
  list<VarInfo>::iterator info;
  size_t i;
  for (i = 0, info = f.xtab.begin(); info != f.xtab.end(); i++, info++)
    x[i] = vref(info->vtag, info->idx-1, info->p);
  // Check for a parameterless anonymous closure (presumably catch body),
  // these calls *must* be deferred.
  if (f.n == 0 && (!f.local || f.tag > 0))
    // parameterless function, emit a direct call
    return fcall(f, x);
  else {
    // create a boxed closure
    if (f.m == 1)
      act_env().CreateCall(module->getFunction("pure_new"), x);
    else {
      vector<Value*> newargs;
      newargs.push_back(UInt(f.m));
      newargs.insert(newargs.end(), x.begin(), x.end());
      act_env().CreateCall(module->getFunction("pure_new_args"), newargs);
    }
    return call("pure_clos", f.local, f.tag, f.getkey(), f.h,
		envptr(&f), f.n, x);
  }
}

// Constant boxes. These values are cached in global variables, so that only a
// single instance of each symbol exists, and we can easily patch up the
// definition if the symbol becomes a defined function later.

Value *interpreter::cbox(int32_t tag)
{
  pure_expr *cv = pure_const(tag);
  assert(JIT);
  GlobalVar& v = globalvars[tag];
  if (!v.v) {
    v.v = global_variable
      (module, ExprPtrTy, false, GlobalVariable::InternalLinkage,
       NullExprPtr, mkvarlabel(tag));
    JIT->addGlobalMapping(v.v, &v.x);
  }
  if (v.x) pure_free(v.x); v.x = pure_new(cv);
  return act_builder().CreateLoad(v.v);
}

// Execute a parameterless function.

Value *interpreter::call(Value *x)
{
  return call("pure_call", x);
}

// Execute a function application.

Value *interpreter::apply(Value *x, Value *y)
{
  call("pure_new_args", Two, x, y);
  return call("pure_apply", x, y);
}

// Construct a literal application.

Value *interpreter::applc(Value *x, Value *y)
{
  call("pure_new_args", Two, x, y);
  return call("pure_applc", x, y);
}

// Conditionals.

Value *interpreter::cond(expr x, expr y, expr z)
{
  Env& f = act_env();
  assert(f.f!=0);
  // emit the code for x
  Value *iv = 0;
  if (x.ttag() == EXPR::INT)
    // optimize the case that x is an ::int (constant or application)
    iv = get_int(x);
  else if (x.ttag() != 0) {
    // wrong type of constant; raise an exception
    // XXXTODO: we might want to optionally invoke the debugger here
    unwind(symtab.failed_cond_sym().f, false);
    iv = Zero;
  } else
    // typeless expression, will be checked at runtime
    iv = get_int(x);
  // emit the condition (turn the previous result into a flag)
  Value *condv = f.builder.CreateICmpNE(iv, Zero, "cond");
  // create the basic blocks for the branches
  BasicBlock *thenbb = basic_block("then");
  BasicBlock *elsebb = basic_block("else");
  BasicBlock *endbb = basic_block("end");
  // create the branch instruction and emit the 'then' block
  f.builder.CreateCondBr(condv, thenbb, elsebb);
  f.f->getBasicBlockList().push_back(thenbb);
  f.builder.SetInsertPoint(thenbb);
  Value *thenv = codegen(y);
  f.builder.CreateBr(endbb);
  // current block might have changed, update thenbb for the phi
  thenbb = f.builder.GetInsertBlock();
  // emit the 'else' block
  f.f->getBasicBlockList().push_back(elsebb);
  f.builder.SetInsertPoint(elsebb);
  Value *elsev = codegen(z);
  f.builder.CreateBr(endbb);
  // current block might have changed, update elsebb for the phi
  elsebb = f.builder.GetInsertBlock();
  // emit the 'end' block and the phi node
  f.f->getBasicBlockList().push_back(endbb);
  f.builder.SetInsertPoint(endbb);
  PHINode *phi = f.builder.CreatePHI(ExprPtrTy, "fi");
  phi->addIncoming(thenv, thenbb);
  phi->addIncoming(elsev, elsebb);
  return phi;
}

void interpreter::toplevel_cond(expr x, expr y, expr z, const rule *rp)
{
  // emit tail-recursive code for a toplevel if-then-else
  Env& f = act_env();
  assert(f.f!=0);
  // emit the code for x
  Value *iv = 0;
  if (x.ttag() == EXPR::INT)
    // optimize the case that x is an ::int (constant or application)
    iv = get_int(x);
  else if (x.ttag() != 0) {
    // wrong type of constant; raise an exception
    // XXXTODO: we might want to optionally invoke the debugger here
    unwind(symtab.failed_cond_sym().f, false);
    iv = Zero;
  } else
    // typeless expression, will be checked at runtime
    iv = get_int(x);
  // emit the condition (turn the previous result into a flag)
  Value *condv = f.builder.CreateICmpNE(iv, Zero, "cond");
  // create the basic blocks for the branches
  BasicBlock *thenbb = basic_block("then");
  BasicBlock *elsebb = basic_block("else");
  // create the branch instruction and emit the 'then' block
  f.builder.CreateCondBr(condv, thenbb, elsebb);
  f.f->getBasicBlockList().push_back(thenbb);
  f.builder.SetInsertPoint(thenbb);
  toplevel_codegen(y, rp);
  // emit the 'else' block
  f.f->getBasicBlockList().push_back(elsebb);
  f.builder.SetInsertPoint(elsebb);
  toplevel_codegen(z, rp);
}

// Other value boxes. These just call primitives in the runtime which take
// care of constructing these values.

Value *interpreter::ibox(Value *i)
{
  return call("pure_int", i);
}

Value *interpreter::ibox(int32_t i)
{
  return call("pure_int", i);
}

Value *interpreter::zbox(const mpz_t& z)
{
  return call("pure_bigint", z);
}

Value *interpreter::dbox(Value *d)
{
  return call("pure_double", d);
}

Value *interpreter::dbox(double d)
{
  return call("pure_double", d);
}

Value *interpreter::sbox(const char *s)
{
  return call("pure_string_dup", s);
}

Value *interpreter::pbox(void *p)
{
  return call("pure_pointer", p);
}

// Variable access.

static uint32_t argno(uint32_t n, path &p)
{
  uint32_t m = p.len(), k = n-1;
  size_t i;
  for (i = 0; i < m && p[i] == 0; i++) {
    assert(k>0); --k;
  }
  assert(i < m && p[i] == 1);
  path q(m-++i);
  for (size_t j = 0; i < m; i++, j++) q.set(j, p[i]);
  p = q;
  return k;
}

Value *interpreter::vref(int32_t tag, path p)
{
  // local arg reference
  Env &e = act_env();
  uint32_t k = 0;
  if (e.b)
    // pattern binding
    assert(e.n==1);
  else
    k = argno(e.n, p);
  Value *v = e.args[k];
  size_t n = p.len();
  for (size_t i = 0; i < n; i++)
    v = e.CreateLoadGEP(v, Zero, SubFldIndex(p[i]), mklabel("x", i, p[i]+1));
  return v;
}

Value *interpreter::vref(int32_t tag, uint32_t v)
{
  // environment proxy
  Env &e = act_env();
  Value *sstkptr = e.builder.CreateLoad(sstkvar);
  return e.CreateLoadGEP(sstkptr, e.builder.CreateAdd(e.envs, UInt(v)));
}

Value *interpreter::vref(int32_t tag, uint8_t idx, path p)
{
  if (idx == 0)
    // idx==0 => local reference
    return vref(tag, p);
  else {
    // idx>0 => non-local, return the local proxy
#if DEBUG>2
    std::cerr << act_env().name << ": looking for " << symtab.sym(tag).s
	      << ":" << (unsigned)idx << '\n';
#endif
    assert(act_env().xmap.find(xmap_key(tag, idx)) != act_env().xmap.end());
    return vref(tag, act_env().xmap[xmap_key(tag, idx)]);
  }
}

Value *interpreter::fref(int32_t tag, uint8_t idx)
{
  // local function reference; box the function as a value on the fly
  assert(!envstk.empty());
  if (idx == 0) {
    // function in current environment ('with'-bound)
    Env& f = *act_env().fmap.act()[tag];
    return fbox(f);
  }
  // If we come here, the function is defined in an outer environment. Locate
  // the function, the de Bruijn index idx tells us where on the current
  // environment stack it's at.
  EnvStack::iterator e = envstk.begin();
  size_t i = idx;
  for (; i > 0; e++, i--) assert(e != envstk.end());
  // look up the function in the environment
  Env& f = *(*e)->fmap.act()[tag];
  assert(f.f);
  // Now create the closure. This is essentially just like fbox(), but we are
  // called inside a nested environment here, and hence the de Bruijn indices
  // need to be translated accordingly.
  assert(f.m == f.xtab.size());
  vector<Value*> x(f.m);
  list<VarInfo>::iterator info;
  for (i = 0, info = f.xtab.begin(); info != f.xtab.end(); i++, info++)
    x[i] = vref(info->vtag, info->idx+idx-1, info->p);
  if (f.n == 0)
    // parameterless function, emit a direct call
    return fcall(f, x);
  else {
    // create a boxed closure
    if (f.m == 1)
      act_env().CreateCall(module->getFunction("pure_new"), x);
    else {
      vector<Value*> newargs;
      newargs.push_back(UInt(f.m));
      newargs.insert(newargs.end(), x.begin(), x.end());
      act_env().CreateCall(module->getFunction("pure_new_args"), newargs);
    }
    return call("pure_clos", f.local, f.tag, f.getkey(), f.h,
		envptr(&f), f.n, x);
  }
}

// Function calls.

Value *interpreter::fcall(Env &f, vector<Value*>& args, vector<Value*>& env)
{
  Env& e = act_env();
  // direct call of a function, with parameters
  assert(f.f);
  size_t n = args.size(), m = env.size();
  // count references to parameters and create the environment parameter
  assert(f.local || m == 0);
  Value *argv = 0;
  if (n == 1 && m == 0)
    e.CreateCall(module->getFunction("pure_push_arg"), args);
  else if (n+m > 0) {
    vector<Value*> args1;
    args1.push_back(UInt(n));
    args1.push_back(UInt(m));
    args1.insert(args1.end(), args.begin(), args.end());
    args1.insert(args1.end(), env.begin(), env.end());
    argv = e.CreateCall(module->getFunction("pure_push_args"), args1);
  }
  // pass the environment as the first parameter, if applicable
  vector<Value*> x;
  if (m>0) x.push_back(argv);
  // pass the function parameters
  x.insert(x.end(), args.begin(), args.end());
  // create the call
  return e.CreateCall(f.f, x);
}

Value *interpreter::call(string name, bool local, int32_t tag, uint32_t key,
			 Function *f, Value *e, uint32_t argc,
			 vector<Value*>& x)
{
  // call to create an fbox (closure) for the given function with the given
  // number of parameters and the given extra arguments
  Function *g = module->getFunction(name);
  assert(g);
  vector<Value*> args;
  // local flag
  args.push_back(Bool(local));
  // tag
  args.push_back(SInt(tag));
  // key
  args.push_back(UInt(key));
  // argc
  args.push_back(SInt(argc));
  // function pointer
  args.push_back(act_builder().CreateBitCast(f, VoidPtrTy));
  // environment pointer
  args.push_back(e);
  // captured environment (varargs)
  args.push_back(SInt(x.size()));
  args.insert(args.end(), x.begin(), x.end());
  return act_env().CreateCall(g, args);
}

// Calls into the runtime system.

Value *interpreter::call(string name, Value *x)
{
  Function *f = module->getFunction(name);
  assert(f);
  vector<Value*> args;
  args.push_back(x);
  return act_env().CreateCall(f, args);
}

Value *interpreter::call(string name, Value *x, Value *y)
{
  Function *f = module->getFunction(name);
  assert(f);
  vector<Value*> args;
  args.push_back(x);
  args.push_back(y);
  return act_env().CreateCall(f, args);
}

Value *interpreter::call(string name, Value *x, Value *y, Value *z)
{
  Function *f = module->getFunction(name);
  assert(f);
  vector<Value*> args;
  args.push_back(x);
  args.push_back(y);
  args.push_back(z);
  return act_env().CreateCall(f, args);
}

Value *interpreter::call(string name, Value *x, Value *y, Value *z, Value *t)
{
  Function *f = module->getFunction(name);
  assert(f);
  vector<Value*> args;
  args.push_back(x);
  args.push_back(y);
  args.push_back(z);
  args.push_back(t);
  return act_env().CreateCall(f, args);
}

Value *interpreter::call(string name, int32_t i)
{
  return call(name, SInt(i));
}

Value *interpreter::call(string name, double d)
{
  return call(name, Dbl(d));
}

Value *interpreter::call(string name, const char *s)
{
  Env& e = act_env();
  GlobalVariable *v = global_variable
    (module, ArrayType::get(int8_type(), strlen(s)+1), true,
     GlobalVariable::InternalLinkage, constant_char_array(s),
     "$$str");
  // "cast" the char array to a char*
  Value *p = e.CreateGEP(v, Zero, Zero);
  return call(name, p);
}

Value *interpreter::call(string name, void *p)
{
  assert(p==0);
  return call(name, ConstantPointerNull::get(VoidPtrTy));
}

Value *interpreter::call(string name, Value *x, const char *s)
{
  Env& e = act_env();
  GlobalVariable *v = global_variable
    (module, ArrayType::get(int8_type(), strlen(s)+1), true,
     GlobalVariable::InternalLinkage, constant_char_array(s),
     "$$str");
  // "cast" the char array to a char*
  Value *p = e.CreateGEP(v, Zero, Zero);
  return call(name, x, p);
}

Value *interpreter::call(string name, const mpz_t& z)
{
  Value *sz, *ptr;
  make_bigint(z, sz, ptr);
  return call(name, sz, ptr);
}

Value *interpreter::call(string name, Value *x, const mpz_t& z)
{
  Value *sz, *ptr;
  make_bigint(z, sz, ptr);
  return call(name, x, sz, ptr);
}

// create a bigint (mpz_t) constant

void interpreter::make_bigint(const mpz_t& z, Value*& sz, Value*& ptr)
{
  // first arg: signed int combining sign with number of limbs
  /* NOTE: This may loose leading size bits on systems where GMP is compiled
     to support mpz's with more than 0x7fffffff limbs, but this shouldn't
     really be a problem for the forseeable future... ;-) */
  sz = SInt((int32_t)z->_mp_size);
  /* We're a bit lazy in that we only support 32 and 64 bit limbs here, but
     that should probably work on most if not all systems where GMP is
     available. */
  Env& e = act_env();
  GlobalVariable *v;
  if (sizeof(mp_limb_t) == 8) {
    // 64 bit limbs
    // second arg: array of unsigned long ints (least significant limb first)
    size_t n = (size_t)(z->_mp_size>=0 ? z->_mp_size : -z->_mp_size);
    vector<Constant*> u(n);
    for (size_t i = 0; i < n; i++) u[i] = UInt64(z->_mp_d[i]);
    Constant *limbs = ConstantArray::get(ArrayType::get(int64_type(), n), u);
    v = global_variable
      (module, ArrayType::get(int64_type(), n), true,
       GlobalVariable::InternalLinkage, limbs, "$$limbs");
  } else {
    // assume 32 bit limbs
    assert(sizeof(mp_limb_t) == 4);
    // second arg: array of unsigned ints (least significant limb first)
    size_t n = (size_t)(z->_mp_size>=0 ? z->_mp_size : -z->_mp_size);
    vector<Constant*> u(n);
    for (size_t i = 0; i < n; i++) u[i] = UInt(z->_mp_d[i]);
    Constant *limbs = ConstantArray::get(ArrayType::get(int32_type(), n), u);
    v = global_variable
      (module, ArrayType::get(int32_type(), n), true,
       GlobalVariable::InternalLinkage, limbs, "$$limbs");
  }
  // "cast" the int array to a int*
  ptr = e.CreateGEP(v, Zero, Zero);
}

// Debugger calls.

Value *interpreter::debug_rule(const rule *r)
{
  Env* e = &act_env();
  Function *f = module->getFunction("pure_debug_rule");
  assert(f);
  vector<Value*> args;
  args.push_back(constptr(e));
  args.push_back(constptr(r));
  return e->CreateCall(f, args);
}

Value *interpreter::debug_redn(const rule *r, Value *v)
{
  Env* e = &act_env();
  Function *f = module->getFunction("pure_debug_redn");
  assert(f);
  if (!v) v = NullExprPtr;
  vector<Value*> args;
  args.push_back(constptr(e));
  args.push_back(constptr(r));
  args.push_back(v);
  return e->CreateCall(f, args);
}

Value *interpreter::debug(const char *format)
{
  Function *f = module->getFunction("pure_debug");
  assert(f);
  Env& e = act_env();
  GlobalVariable *v = global_variable
    (module, ArrayType::get(int8_type(), strlen(format)+1), true,
     GlobalVariable::InternalLinkage, constant_char_array(format),
     "$$str");
  // "cast" the char array to a char*
  Value *p = e.CreateGEP(v, Zero, Zero);
  vector<Value*> args;
  args.push_back(SInt(e.tag));
  args.push_back(p);
  return e.CreateCall(f, args);
}

Value *interpreter::debug(const char *format, Value *x)
{
  Function *f = module->getFunction("pure_debug");
  assert(f);
  Env& e = act_env();
  GlobalVariable *v = global_variable
    (module, ArrayType::get(int8_type(), strlen(format)+1), true,
     GlobalVariable::InternalLinkage, constant_char_array(format),
     "$$str");
  // "cast" the char array to a char*
  Value *p = e.CreateGEP(v, Zero, Zero);
  vector<Value*> args;
  args.push_back(SInt(e.tag));
  args.push_back(p);
  args.push_back(x);
  return e.CreateCall(f, args);
}

Value *interpreter::debug(const char *format, Value *x, Value *y)
{
  Function *f = module->getFunction("pure_debug");
  assert(f);
  Env& e = act_env();
  GlobalVariable *v = global_variable
    (module, ArrayType::get(int8_type(), strlen(format)+1), true,
     GlobalVariable::InternalLinkage, constant_char_array(format),
     "$$str");
  // "cast" the char array to a char*
  Value *p = e.CreateGEP(v, Zero, Zero);
  vector<Value*> args;
  args.push_back(SInt(e.tag));
  args.push_back(p);
  args.push_back(x);
  args.push_back(y);
  return e.CreateCall(f, args);
}

Value *interpreter::debug(const char *format, Value *x, Value *y, Value *z)
{
  Function *f = module->getFunction("pure_debug");
  assert(f);
  Env& e = act_env();
  GlobalVariable *v = global_variable
    (module, ArrayType::get(int8_type(), strlen(format)+1), true,
     GlobalVariable::InternalLinkage, constant_char_array(format),
     "$$str");
  // "cast" the char array to a char*
  Value *p = e.CreateGEP(v, Zero, Zero);
  vector<Value*> args;
  args.push_back(SInt(e.tag));
  args.push_back(p);
  args.push_back(x);
  args.push_back(y);
  args.push_back(z);
  return e.CreateCall(f, args);
}

void interpreter::unwind(int32_t tag, bool terminate)
{
  Function *f = module->getFunction("pure_throw");
  assert(f);
  vector<Value*> args;
  if (tag > 0)
    args.push_back(cbox(tag));
  else
    args.push_back(NullExprPtr);
  Env& e = act_env();
  e.CreateCall(f, args);
  if (terminate)
    // add a ret instruction to terminate the current block
    e.builder.CreateRet(NullExprPtr);
}

// Create a function.

Function *interpreter::fun(string name, matcher *pm, bool nodefault)
{
  Function *f = fun_prolog(name);
  fun_body(pm, nodefault);
  return f;
}

Function *interpreter::fun_prolog(string name)
{
  Env& f = act_env();
  if (f.f==0) {
    // argument types
    vector<const Type*> argt(f.n, ExprPtrTy);
    assert(f.m == 0 || f.local);
    if (f.m > 0) argt.insert(argt.begin(), int32_type());
    // function type
    FunctionType *ft = FunctionType::get(ExprPtrTy, argt, false);
    /* Mangle local function names so that they're easier to identify; as a
       side-effect, this should also ensure that we always get the proper
       names for external functions. */
    bool have_c_func = false;
    if (f.local) {
      EnvStack::iterator e = envstk.begin();
      if (++e != envstk.end()) name = (*e)->name + "." + name;
    } else
      /* Check that we do not accidentally override a C function of the same
	 name. */
      have_c_func = is_c_sym(name);
    f.name = name;
    /* Linkage type and calling convention. For each Pure function (no matter
       whether global or local) we create a C-callable function in LLVM
       IR. (This is necessary also for local functions, since these might need
       to be called from the runtime.) In the case of a global function, this
       function is also externally visible, *unless* the function symbol is
       private or there's already a callable C function of that name (in which
       case we also mangle the name to prevent name collisions). However, in
       order to enable tail call elimination (on platforms where LLVM supports
       this), suitable functions are internally implemented using the fast
       calling convention (if enabled, which it is by default). In this case,
       the C-callable function is just a stub calling the internal
       function. */
    Function::LinkageTypes scope = Function::ExternalLinkage;
    CallingConv::ID cc = CallingConv::C;
    if (f.local ||
	// global Pure functions use internal linkage if they would shadow a C
	// function:
	have_c_func ||
	// anonymous and private functions and operators use internal linkage,
	// too:
	f.tag == 0 || symtab.sym(f.tag).priv ||
	symtab.sym(f.tag).prec < PREC_MAX || symtab.sym(f.tag).fix == outfix)
      scope = Function::InternalLinkage;
#if USE_FASTCC
    if (!is_init(name)) cc = CallingConv::Fast;
#endif
    string pure_name = name;
    /* Mangle operator names. */
    if (f.tag > 0 &&
	(symtab.sym(f.tag).prec < PREC_MAX || symtab.sym(f.tag).fix == outfix))
      pure_name = "("+pure_name+")";
    /* Mangle the name of the C-callable wrapper if it's private, or would
       shadow another C function. */
    if (f.tag > 0 && symtab.sym(f.tag).priv)
      pure_name = "$$private."+pure_name;
    else if (have_c_func)
      pure_name = "$$pure."+pure_name;
    if (cc == CallingConv::Fast) {
      // create the function
      f.f = Function::Create(ft, Function::InternalLinkage,
			 "$$fastcc."+name, module);
      assert(f.f); f.f->setCallingConv(cc);
      // create the C-callable stub
      f.h = Function::Create(ft, scope, pure_name, module); assert(f.h);
    } else {
      // no need for a separate stub
      f.f = Function::Create(ft, scope, pure_name, module); assert(f.f);
      f.h = f.f;
    }
    /* Give names to the arguments, and provide direct access to these by
       means of the env and args fields. */
    Function::arg_iterator a = f.f->arg_begin();
    if (f.m > 0) { a->setName("env"); f.envs = a++; }
    for (size_t i = 0; a != f.f->arg_end(); ++a, ++i) {
      a->setName(mklabel("arg", i));
      f.args[i] = a;
    }
    if (f.h != f.f) {
      /* Assign parameter names in the stub, too. */
      size_t n = f.n+(f.m>0?1:0);
      vector<Value*> myargs(n);
      size_t i0 = 0; a = f.h->arg_begin();
      if (f.m > 0) { a->setName("env"); myargs[i0++] = a++; }
      for (size_t i = 0; a != f.h->arg_end(); ++a, ++i) {
	a->setName(mklabel("arg", i));
	myargs[i0+i] = a;
      }
      /* Create the body of the stub. This is just a call to the internal
	 function, passing through all arguments including the environment. */
      BasicBlock *bb = basic_block("entry", f.h);
      f.builder.SetInsertPoint(bb);
      CallInst* v = f.builder.CreateCall(f.f, myargs.begin(), myargs.end());
      v->setCallingConv(cc);
      if (cc == CallingConv::Fast) v->setTailCall();
      f.builder.CreateRet(v);
      // validate the generated code, checking for consistency
      verifyFunction(*f.h);
      // optimize
      if (FPM) FPM->run(*f.h);
      // show output code, if requested
      if (verbose&verbosity::dump) {
#if RAW_STREAM
	raw_stdout_ostream out;
#else
	ostream& out = std::cout;
#endif
	f.h->print(out);
      }
    }
  }
#if DEBUG>1
  std::cerr << "PROLOG FUNCTION " << f.name << '\n';
#endif
  // create a new basic block to start insertion into
  BasicBlock *bb = basic_block("entry", f.f);
  f.builder.SetInsertPoint(bb);
#if DEBUG>1
  if (!is_init(f.name)) { ostringstream msg;
    msg << "entry " << f.name;
    debug(msg.str().c_str()); }
#endif
  // return the function just created
  return f.f;
}

void interpreter::fun_body(matcher *pm, bool nodefault)
{
  Env& f = act_env();
  assert(f.f!=0);
#if DEBUG>1
  std::cerr << "BODY FUNCTION " << f.name << '\n';
#endif
  BasicBlock *bodybb = basic_block("body");
  f.builder.CreateBr(bodybb);
  f.f->getBasicBlockList().push_back(bodybb);
  f.builder.SetInsertPoint(bodybb);
#if DEBUG>1
  if (!is_init(f.name)) { ostringstream msg;
    msg << "body " << f.name;
    debug(msg.str().c_str()); }
#endif
  BasicBlock *failedbb = basic_block("failed");
  // emit the matching code
  if (debugging && !is_init(f.name)) debug_rule(0);
  complex_match(pm, failedbb);
  // emit code for a failed match
  f.f->getBasicBlockList().push_back(failedbb);
  f.builder.SetInsertPoint(failedbb);
  if (debugging && !is_init(f.name)) debug_redn(0);
  if (nodefault) {
    // failed match is fatal, throw an exception
#if DEBUG>1
  if (!is_init(f.name)) { ostringstream msg;
    msg << "failed " << f.name << ", exception";
    debug(msg.str().c_str()); }
#endif
    unwind(symtab.failed_match_sym().f);
  } else {
#if DEBUG>1
  if (!is_init(f.name)) { ostringstream msg;
    msg << "failed " << f.name << ", default value";
    debug(msg.str().c_str()); }
#endif
    // failed match is non-fatal, instead we return a "thunk" (literal fbox)
    // of ourself applied to our arguments as the result
    vector<Value*> x(f.m);
    Value *sstkptr = f.builder.CreateLoad(sstkvar);
    for (size_t i = 0; i < f.m; i++) {
      x[i] = f.CreateLoadGEP(sstkptr, f.builder.CreateAdd(f.envs, UInt(i)));
      assert(x[i]->getType() == ExprPtrTy);
    }
    if (f.m == 1)
      act_env().CreateCall(module->getFunction("pure_new"), x);
    else {
      vector<Value*> newargs;
      newargs.push_back(UInt(f.m));
      newargs.insert(newargs.end(), x.begin(), x.end());
      act_env().CreateCall(module->getFunction("pure_new_args"), newargs);
    }
    Value *defaultv;
    map<int32_t,ExternInfo>::const_iterator it;
    if (!f.local && (it = externals.find(f.tag)) != externals.end()) {
      /* Patch up a failed call chained from an external. In this case we must
	 return an fbox for the external instead of ourself. */
      const ExternInfo& info = it->second;
      vector<Value*> env;
      assert(f.m==0 && info.argtypes.size() == f.n);
      defaultv = call("pure_clos", false, f.tag, 0, info.f, NullPtr, f.n, x);
    } else
      defaultv =
	call("pure_clos", f.local, f.tag, f.getkey(), f.h, envptr(&f), f.n, x);
    for (size_t i = 0; i < f.n; ++i) {
      Value *arg = f.args[i];
      assert(arg->getType() == ExprPtrTy);
      defaultv = applc(defaultv, arg);
    }
    f.CreateRet(defaultv);
  }
  fun_finish();
}

void interpreter::fun_finish()
{
  Env& f = act_env();
  assert(f.f!=0);
  // validate the generated code, checking for consistency
  verifyFunction(*f.f);
  // optimize
  if (FPM) FPM->run(*f.f);
  // show output code, if requested
  if (verbose&verbosity::dump)  {
#if RAW_STREAM
    raw_stdout_ostream out;
#else
    ostream& out = std::cout;
#endif
    f.f->print(out);
  }
#if DEBUG>1
  std::cerr << "END BODY FUNCTION " << f.name << '\n';
#endif
}

// Helper function to emit special code.

void interpreter::unwind_iffalse(Value *v)
{
  // throw an exception if v == false
  Env& f = act_env();
  assert(f.f!=0);
  BasicBlock *errbb = basic_block("err");
  BasicBlock *okbb = basic_block("ok");
  f.builder.CreateCondBr(v, okbb, errbb);
  f.f->getBasicBlockList().push_back(errbb);
  f.builder.SetInsertPoint(errbb);
  unwind(symtab.failed_cond_sym().f);
  f.f->getBasicBlockList().push_back(okbb);
  f.builder.SetInsertPoint(okbb);
}

void interpreter::unwind_iftrue(Value *v)
{
  // throw an exception if v == true
  Env& f = act_env();
  assert(f.f!=0);
  BasicBlock *errbb = basic_block("err");
  BasicBlock *okbb = basic_block("ok");
  f.builder.CreateCondBr(v, errbb, okbb);
  f.f->getBasicBlockList().push_back(errbb);
  f.builder.SetInsertPoint(errbb);
  unwind(symtab.failed_cond_sym().f);
  f.f->getBasicBlockList().push_back(okbb);
  f.builder.SetInsertPoint(okbb);
}

Value *interpreter::check_tag(Value *v, int32_t tag)
{
  // check that the given expression value has the given tag, return true if
  // so and false otherwise
  assert(v->getType() == ExprPtrTy);
  Value *tagv = act_env().CreateLoadGEP(v, Zero, Zero, "tag");
  return act_builder().CreateICmpEQ(tagv, SInt(tag));
}

void interpreter::verify_tag(Value *v, int32_t tag)
{
  // check that the given expression value has the given tag, throw an
  // exception otherwise
  return unwind_iffalse(check_tag(v, tag));
}

// Emit matching code.

/* First the simplified pattern matching code for a matching automaton which
   is a trie, i.e., which has only one transition in each non-final state.
   Takes the value of the expression to be matched as argument and branches to
   either the matched or the failed bb, depending on whether the match
   suceeded or not (note that the code for the reduct must be handled by the
   caller). This case is heavily used by the anonymous closures which handle
   pattern-matching lambdas and singleton pattern bindings in 'when' clauses,
   so it makes sense to optimize for it. */

void interpreter::simple_match(Value *x, state*& s,
			       BasicBlock *matchedbb, BasicBlock *failedbb)
{
  assert(x->getType() == ExprPtrTy && s->tr.size() == 1);
  const trans& t = *s->tr.begin();
  Env& f = act_env();
  assert(f.f!=0);
#if DEBUG>1
  if (!is_init(f.name)) { ostringstream msg;
    msg << "simple match " << f.name;
    debug(msg.str().c_str()); }
#endif
  Value *tagv = 0;
  // check for thunks which must be forced
  if (t.tag != EXPR::VAR || t.ttag != 0) {
    // do a quick check on the tag value
    tagv = f.CreateLoadGEP(x, Zero, Zero, "tag");
    Value *checkv = f.builder.CreateICmpEQ(tagv, Zero, "check");
    BasicBlock *forcebb = basic_block("force");
    BasicBlock *skipbb = basic_block("skip");
    f.builder.CreateCondBr(checkv, forcebb, skipbb);
    f.f->getBasicBlockList().push_back(forcebb);
    f.builder.SetInsertPoint(forcebb);
    call("pure_force", x);
    f.builder.CreateBr(skipbb);
    f.f->getBasicBlockList().push_back(skipbb);
    f.builder.SetInsertPoint(skipbb);
    tagv = 0;
  }
  // match the current symbol
  switch (t.tag) {
  case EXPR::VAR:
    if (t.ttag == 0) // untyped variable => always match
      f.builder.CreateBr(matchedbb);
    else {
      // typed variable, must match type tag against value
      if (!tagv) tagv = f.CreateLoadGEP(x, Zero, Zero, "tag");
      if (t.ttag == EXPR::MATRIX) {
	// this can denote any type of matrix, mask the subtype nibble
	Value *tagv1 = f.builder.CreateAnd(tagv, UInt(0xfffffff0));
	f.builder.CreateCondBr
	  (f.builder.CreateICmpEQ(tagv1, SInt(t.ttag)), matchedbb, failedbb);
      } else
	f.builder.CreateCondBr
	  (f.builder.CreateICmpEQ(tagv, SInt(t.ttag)), matchedbb, failedbb);
    }
    s = t.st;
    break;
  case EXPR::INT:
  case EXPR::DBL: {
    // first check the tag
    BasicBlock *okbb = basic_block("ok");
    if (!tagv) tagv = f.CreateLoadGEP(x, Zero, Zero, "tag");
    f.builder.CreateCondBr
      (f.builder.CreateICmpEQ(tagv, SInt(t.tag), "cmp"), okbb, failedbb);
    // next check the values (we inline these for max performance)
    f.f->getBasicBlockList().push_back(okbb);
    f.builder.SetInsertPoint(okbb);
    Value *cmpv;
    if (t.tag == EXPR::INT) {
      Value *pv = f.builder.CreateBitCast(x, IntExprPtrTy, "intexpr");
      Value *iv = f.CreateLoadGEP(pv, Zero, ValFldIndex, "intval");
      cmpv = f.builder.CreateICmpEQ(iv, SInt(t.i), "cmp");
    } else {
      Value *pv = f.builder.CreateBitCast(x, DblExprPtrTy, "dblexpr");
      Value *dv = f.CreateLoadGEP(pv, Zero, ValFldIndex, "dblval");
      cmpv = f.builder.CreateFCmpOEQ(dv, Dbl(t.d), "cmp");
    }
    f.builder.CreateCondBr(cmpv, matchedbb, failedbb);
    s = t.st;
    break;
  }
  case EXPR::BIGINT:
  case EXPR::STR: {
    // first do a quick check on the tag so that we may avoid an expensive
    // call if the tags don't match
    BasicBlock *okbb = basic_block("ok");
    if (!tagv) tagv = f.CreateLoadGEP(x, Zero, Zero, "tag");
    f.builder.CreateCondBr
      (f.builder.CreateICmpEQ(tagv, SInt(t.tag), "cmp"), okbb, failedbb);
    // next check the values (like above, but we have to call the runtime for
    // these)
    f.f->getBasicBlockList().push_back(okbb);
    f.builder.SetInsertPoint(okbb);
    Value *cmpv;
    if (t.tag == EXPR::BIGINT)
      cmpv = call("pure_cmp_bigint", x, t.z);
    else
      cmpv = call("pure_cmp_string", x, t.s);
    cmpv = f.builder.CreateICmpEQ(cmpv, Zero, "cmp");
    f.builder.CreateCondBr(cmpv, matchedbb, failedbb);
    s = t.st;
    break;
  }
  case EXPR::PTR:
  case EXPR::WRAP:
  case EXPR::MATRIX:
    //assert(0 && "not implemented");
    // We silently fail on these.
    f.builder.CreateBr(failedbb);
    s = t.st;
    break;
  case EXPR::APP: {
    // first match the tag...
    BasicBlock *ok1bb = basic_block("arg1");
    BasicBlock *ok2bb = basic_block("arg2");
    if (!tagv) tagv = f.CreateLoadGEP(x, Zero, Zero, "tag");
    f.builder.CreateCondBr
      (f.builder.CreateICmpEQ(tagv, SInt(t.tag)), ok1bb, failedbb);
    s = t.st;
    // next match the first subterm...
    f.f->getBasicBlockList().push_back(ok1bb);
    f.builder.SetInsertPoint(ok1bb);
    Value *x1 = f.CreateLoadGEP(x, Zero, ValFldIndex, "x1");
    simple_match(x1, s, ok2bb, failedbb);
    // and finally the second subterm...
    f.f->getBasicBlockList().push_back(ok2bb);
    f.builder.SetInsertPoint(ok2bb);
    Value *x2 = f.CreateLoadGEP(x, Zero, ValFld2Index, "x2");
    simple_match(x2, s, matchedbb, failedbb);
    break;
  }
  default:
    assert(t.tag > 0);
    // just do a quick check on the tag
    if (!tagv) tagv = f.CreateLoadGEP(x, Zero, Zero, "tag");
    f.builder.CreateCondBr
      (f.builder.CreateICmpEQ(tagv, SInt(t.tag)), matchedbb, failedbb);
    s = t.st;
    break;
  }
}

/* Next the algorithm which generates the full-blown matching code for a
   general matching automaton. This is a bit involved, as we have to recurse
   into both the list of arguments to be matched and the tree structure of the
   automaton (including the list of different guarded rules to be tried in
   each final state of the automaton). The resulting code is a decision tree
   for the automaton. At the leaves of the tree we either branch to the global
   "failed" block which handles a failed match in the caller, or return the
   value of a reduct of a matched rule. */

/* The following is just the wrapper routine which decides whether to do a
   complex or simple matcher and sets up the needed environment. */

void interpreter::complex_match(matcher *pm, BasicBlock *failedbb)
{
  Env& f = act_env();
  assert(f.f!=0);
  // Check to see if this is just a trie for a single unguarded
  // pattern-binding rule, then we can employ the simple matcher instead.
  if (f.n == 1 && f.b && pm->r.size() == 1 && pm->r[0].qual.is_null() &&
      !pm->r[0].rhs.is_guarded()) {
    Value *arg = f.args[0];
    // emit the matching code
    BasicBlock *matchedbb = basic_block("matched");
    state *start = pm->start;
    simple_match(arg, start, matchedbb, failedbb);
    // matched => emit code for the reduct, and return the result
    f.f->getBasicBlockList().push_back(matchedbb);
    f.builder.SetInsertPoint(matchedbb);
    const rule* rp = 0;
    if (debugging && !is_init(f.name)) {
      const rule& rr = pm->r[0];
      rp = &rr;
      debug_rule(rp);
    }
#if DEBUG>1
    if (!is_init(f.name)) { ostringstream msg;
      msg << "exit " << f.name << ", result: " << pm->r[0].rhs;
      debug(msg.str().c_str()); }
#endif
    toplevel_codegen(pm->r[0].rhs, rp);
  } else {
    // build the initial stack of expressions to be matched
    list<Value*>xs;
    for (uint32_t i = 0; i < f.n; i++) xs.push_back(f.args[i]);
    // emit the matching code
    set<rulem> reduced;
    if (xs.empty())
      // nothing to match
      try_rules(pm, pm->start, failedbb, reduced);
    else
      complex_match(pm, xs, pm->start, failedbb, reduced);
    // It is often an error (although not strictly forbidden) if there are any
    // rules left which will never be reduced, so warn about these.
    for (rulem r = 0; r < pm->r.size(); r++)
      if (reduced.find(r) == reduced.end()) {
	const rule& rr = pm->r[r];
	ostringstream msg;
	msg << "warning: rule never reduced: " << rr << ";";
	warning(msg.str());
      }
  }
}

// helper macros to set up for the next state

#define next_state(t)						\
  do {								\
    state *s = t->st;						\
    list<Value*> ys = xs; ys.pop_front();			\
    if (ys.empty())						\
      try_rules(pm, s, failedbb, reduced);			\
    else							\
      complex_match(pm, ys, s, failedbb, reduced);		\
  } while (0)

// same as above, but handles the case of an application where we recurse into
// subterms

#define next_state2(t)						\
  do {								\
    state *s = t->st;						\
    list<Value*> ys = xs; ys.pop_front();			\
    Value *x1 = f.CreateLoadGEP(x, Zero, ValFldIndex, "x1");	\
    Value *x2 = f.CreateLoadGEP(x, Zero, ValFld2Index, "x2");	\
    ys.push_front(x2); ys.push_front(x1);			\
    complex_match(pm, ys, s, failedbb, reduced);		\
  } while (0)

/* This is the core of the decision tree construction algorithm. It emits code
   for matching a single expression starting from a given state, then recurses
   to do the rest. The algorithm maintains an explicit stack of expression
   values waiting to be processed (implemented as a list). Initially, this is
   just the list of argument values of the closure, onto which we push new
   subexpressions when matching applications and from which we pop expressions
   which have been matched. */

struct trans_info {
  trans *t;
  BasicBlock *bb;
  trans_info(trans *_t, BasicBlock *_bb) : t(_t), bb(_bb) {}
};

struct trans_list_info {
  trans *t;
  BasicBlock *bb;
  list<trans_info> tlist;
  trans_list_info() : t(0), bb(0) {}
};

typedef map<int32_t,trans_list_info> trans_map;

void interpreter::complex_match(matcher *pm, const list<Value*>& xs, state *s,
				BasicBlock *failedbb, set<rulem>& reduced)
{
  Env& f = act_env();
  assert(!xs.empty());
  Value* x = xs.front();
  assert(x->getType() == ExprPtrTy);
  // start a new block for this state (this is just for purposes of
  // readability, we don't actually need this as a label to branch to)
  BasicBlock *statebb = basic_block(mklabel("state", s->s));
  f.builder.CreateBr(statebb);
  f.f->getBasicBlockList().push_back(statebb);
  f.builder.SetInsertPoint(statebb);
#if DEBUG>1
  if (!is_init(f.name)) { ostringstream msg;
    msg << "complex match " << f.name << ", state " << s->s;
    debug(msg.str().c_str()); }
#endif
  // blocks for retrying with default transitions after a failed match
  BasicBlock *retrybb = basic_block(mklabel("retry.state", s->s));
  BasicBlock *defaultbb = basic_block(mklabel("default.state", s->s));
  // first check for a literal match
  size_t i, n = s->tr.size(), m = 0;
  transl::iterator t0 = s->tr.begin();
  bool must_force = false;
  while (t0 != s->tr.end() && t0->tag == EXPR::VAR) {
    if (t0->ttag != 0) must_force = true;
    t0++; m++;
  }
  must_force = must_force || t0 != s->tr.end();
  Value *tagv = 0;
  // check for thunks which must be forced
  if (must_force) {
    // do a quick check on the tag value
    tagv = f.CreateLoadGEP(x, Zero, Zero, "tag");
    Value *checkv = f.builder.CreateICmpEQ(tagv, Zero, "check");
    BasicBlock *forcebb = basic_block("force");
    BasicBlock *skipbb = basic_block("skip");
    f.builder.CreateCondBr(checkv, forcebb, skipbb);
    f.f->getBasicBlockList().push_back(forcebb);
    f.builder.SetInsertPoint(forcebb);
    call("pure_force", x);
    f.builder.CreateBr(skipbb);
    f.f->getBasicBlockList().push_back(skipbb);
    f.builder.SetInsertPoint(skipbb);
    tagv = 0;
  }
  if (t0 != s->tr.end()) {
    assert(n > m);
    // get the tag value
    if (!tagv) tagv = f.CreateLoadGEP(x, Zero, Zero, "tag");
    // set up the switch instruction branching over the different tags
    SwitchInst *sw = f.builder.CreateSwitch(tagv, retrybb, n-m);
    /* NOTE: For constant transitions there may be multiple transitions under
       the same constant tag here. Therefore we must collect different
       transitions for the same builtin type under the same target label, then
       do a second pass on the actual constants for each type. To these ends,
       instead of a simple list of branches we actually have to maintain a map
       where each entry points to a list of transitions. */
    trans_map tmap;
    transl::iterator t;
    for (t = t0; t != s->tr.end(); t++) {
      // first create the block for this specific transition
      BasicBlock *bb = basic_block(mklabel("trans.state", s->s, t->st->s));
      if (t->tag == EXPR::APP || t->tag > 0) {
	// transition on a function symbol; in this case there's only a single
	// transition, to which we simply assign the label just generated
	assert(!tmap[t->tag].bb);
	tmap[t->tag].bb = bb;
	tmap[t->tag].t = &*t;
	sw->addCase(SInt(t->tag), bb);
      } else {
	// transition on a constant, add it to the corresponding list
	tmap[t->tag].tlist.push_back(trans_info(&*t, bb));
	if (!tmap[t->tag].bb) {
	  // no outer label has been generated yet, do it now and add the
	  // target to the outer switch
	  tmap[t->tag].bb =
	    basic_block(mklabel("begin.state", s->s, -t->tag));
	  sw->addCase(SInt(t->tag), tmap[t->tag].bb);
	}
      }
    }
    // now generate code for all transitions
    for (trans_map::iterator ti = tmap.begin(); ti != tmap.end(); ti++) {
      int32_t tag = ti->first;
      trans_list_info& info = ti->second;
      f.f->getBasicBlockList().push_back(info.bb);
      f.builder.SetInsertPoint(info.bb);
      if (tag == EXPR::APP || tag > 0) {
	// singleton transition on a function symbol
	assert(info.bb && info.tlist.empty());
	if (tag == EXPR::APP) {
	  // recurse into subterms
	  next_state2(info.t);
	  f.builder.SetInsertPoint(statebb);
	} else
	  next_state(info.t);
      } else {
	// outer label for a list of constants of a given type; iterate over
	// all alternatives to resolve these in turn
	assert(tag < 0 && info.bb && !info.tlist.empty());
	for (list<trans_info>::iterator l = info.tlist.begin();
	     l != info.tlist.end(); l++) {
	  list<trans_info>::iterator k = l; k++;
	  BasicBlock *okbb = l->bb;
	  BasicBlock *trynextbb =
	    basic_block(mklabel("next.state", s->s, -tag));
	  switch (tag) {
	  case EXPR::INT:
	  case EXPR::DBL: {
	    // we can inline these
	    Value *cmpv;
	    if (tag == EXPR::INT) {
	      Value *pv = f.builder.CreateBitCast(x, IntExprPtrTy, "intexpr");
	      Value *iv = f.CreateLoadGEP(pv, Zero, ValFldIndex, "intval");
	      cmpv = f.builder.CreateICmpEQ(iv, SInt(l->t->i), "cmp");
	    } else {
	      Value *pv = f.builder.CreateBitCast(x, DblExprPtrTy, "dblexpr");
	      Value *dv = f.CreateLoadGEP(pv, Zero, ValFldIndex, "dblval");
	      cmpv = f.builder.CreateFCmpOEQ(dv, Dbl(l->t->d), "cmp");
	    }
	    f.builder.CreateCondBr(cmpv, okbb, trynextbb);
	    f.f->getBasicBlockList().push_back(okbb);
	    f.builder.SetInsertPoint(okbb);
	    next_state(l->t);
	    f.f->getBasicBlockList().push_back(trynextbb);
	    f.builder.SetInsertPoint(trynextbb);
	    if (k == info.tlist.end())
	      f.builder.CreateBr(retrybb);
	    break;
	  }
	  case EXPR::BIGINT:
	  case EXPR::STR: {
	    // call the runtime for these
	    Value *cmpv;
	    if (tag == EXPR::BIGINT)
	      cmpv = call("pure_cmp_bigint", x, l->t->z);
	    else
	      cmpv = call("pure_cmp_string", x, l->t->s);
	    cmpv = f.builder.CreateICmpEQ(cmpv, Zero, "cmp");
	    f.builder.CreateCondBr(cmpv, okbb, trynextbb);
	    f.f->getBasicBlockList().push_back(okbb);
	    f.builder.SetInsertPoint(okbb);
	    next_state(l->t);
	    f.f->getBasicBlockList().push_back(trynextbb);
	    f.builder.SetInsertPoint(trynextbb);
	    if (k == info.tlist.end())
	      f.builder.CreateBr(retrybb);
	    break;
	  }
	  default:
	    //assert(0 && "not implemented");
	    // We silently let everything else fail.
	    f.builder.CreateBr(trynextbb);
	    f.f->getBasicBlockList().push_back(trynextbb);
	    f.builder.SetInsertPoint(trynextbb);
	    if (k == info.tlist.end())
	      f.builder.CreateBr(retrybb);
	    break;
	  }
	}
      }
    }
  } else
    f.builder.CreateBr(retrybb);
  // retrybb => literal match failed, check for a typed variable match
  f.f->getBasicBlockList().push_back(retrybb);
  f.builder.SetInsertPoint(retrybb);
  t0 = s->tr.begin();
  transl::iterator t1 = t0;
  if (t1->tag == EXPR::VAR && t1->ttag == 0) t1++;
  if (t1 != s->tr.end() && t1->tag == EXPR::VAR) {
    // get the tag value
    if (!tagv) tagv = f.CreateLoadGEP(x, Zero, Zero, "tag");
    // set up the switch instruction branching over the different type tags
    SwitchInst *sw = f.builder.CreateSwitch(tagv, defaultbb);
    vector<BasicBlock*> vtransbb;
    transl::iterator t;
    for (t = t1, i = 0; t != s->tr.end() && t->tag == EXPR::VAR; t++, i++) {
      vtransbb.push_back
	(basic_block(mklabel("trans.state", s->s, t->st->s)));
      sw->addCase(SInt(t->ttag), vtransbb[i]);
      if (t->ttag == EXPR::MATRIX) {
	// this can denote any type of matrix, add the other possible cases
	sw->addCase(SInt(EXPR::DMATRIX), vtransbb[i]);
	sw->addCase(SInt(EXPR::CMATRIX), vtransbb[i]);
	sw->addCase(SInt(EXPR::IMATRIX), vtransbb[i]);
      }
    }
    // now handle the transitions on the different type tags
    for (t = t1, i = 0; t != s->tr.end() && t->tag == EXPR::VAR; t++, i++) {
      f.f->getBasicBlockList().push_back(vtransbb[i]);
      f.builder.SetInsertPoint(vtransbb[i]);
      next_state(t);
    }
  } else
    f.builder.CreateBr(defaultbb);
  // defaultbb => both literal and type variable matches failed, check for the
  // default transition
  f.f->getBasicBlockList().push_back(defaultbb);
  f.builder.SetInsertPoint(defaultbb);
  if (t0->tag == EXPR::VAR && t0->ttag == 0)
    next_state(t0);
  else // nothing matched in this state, bail out
    f.builder.CreateBr(failedbb);
}

/* Finally, the part of the algorithm which emits code for the rule list in a
   final state. Here we have to consider each matched rule in turn, emit code
   for the guard (if any) and execute the code for the first rule with a guard
   returning true. */

void interpreter::try_rules(matcher *pm, state *s, BasicBlock *failedbb,
			    set<rulem>& reduced)
{
  Env& f = act_env();
  assert(s->tr.empty()); // we're in a final state here
  const rulev& rules = pm->r;
  assert(f.fmap.root.size() == 1 || f.fmap.root.size() == rules.size());
  const ruleml& rl = s->r;
  ruleml::const_iterator r = rl.begin();
  assert(r != rl.end());
  assert(f.fmap.idx == 0);
  BasicBlock* rulebb = basic_block(mklabel("rule.state", s->s, rl.front()));
  f.builder.CreateBr(rulebb);
  while (r != rl.end()) {
    const rule& rr = rules[*r];
    reduced.insert(*r);
    f.fmap.select(*r);
    f.f->getBasicBlockList().push_back(rulebb);
    f.builder.SetInsertPoint(rulebb);
    if (debugging && !is_init(f.name)) debug_rule(&rr);
#if DEBUG>1
    if (!is_init(f.name)) { ostringstream msg;
      msg << "complex match " << f.name << ", trying rule #" << *r;
      debug(msg.str().c_str()); }
#endif
    if (rr.qual.is_null() && !rr.rhs.is_guarded()) {
      // rule always matches, generate code for the reduct and bail out
      const rule *rp = 0;
      if (debugging && !is_init(f.name)) rp = &rr;
#if DEBUG>1
      if (!is_init(f.name)) { ostringstream msg;
	msg << "exit " << f.name << ", result: " << rr.rhs;
	debug(msg.str().c_str()); }
#endif
      toplevel_codegen(rr.rhs, rp);
      break;
    }
    Value *retv = 0, *condv = 0;
    if (!rr.qual.is_null()) {
      // check the guard
      Value *iv = 0;
      if (rr.qual.ttag() == EXPR::INT)
	// optimize the case that the guard is an ::int (constant or
	// application)
	iv = get_int(rr.qual);
      else if (rr.qual.ttag() != 0) {
	// wrong type of constant; raise an exception
	// XXXTODO: we might want to optionally invoke the debugger here
	unwind(symtab.failed_cond_sym().f);
#if DEBUG>1
	if (!is_init(f.name)) { ostringstream msg;
	  msg << "exit " << f.name << ", bad guard (exception)";
	  debug(msg.str().c_str()); }
#endif
	break;
      } else
	// typeless expression, will be checked at runtime
	iv = get_int(rr.qual);
      // emit the condition (turn the previous result into a flag)
      condv = f.builder.CreateICmpNE(iv, Zero, "cond");
    } else {
      assert(rr.rhs.is_guarded());
      // guard wrapped in a closure; generate code for the rhs and to check
      // for null results
      retv = codegen(rr.rhs);
      condv = f.builder.CreateICmpNE(retv, NullExprPtr, "cond");
    }
    assert(condv != 0);
    BasicBlock *okbb = basic_block("ok");
    // determine the next rule block ('failed' if none)
    BasicBlock *nextbb;
    if (++r != rl.end())
      nextbb = basic_block(mklabel("rule.state", s->s, *r));
    else
      nextbb = failedbb;
    f.builder.CreateCondBr(condv, okbb, nextbb);
    // ok => guard succeeded, return the reduct, otherwise we fall through
    // to the next rule (if any), or bail out with failure
    f.f->getBasicBlockList().push_back(okbb);
    f.builder.SetInsertPoint(okbb);
    const rule *rp = 0;
    if (debugging && !is_init(f.name)) rp = &rr;
#if DEBUG>1
    if (!is_init(f.name)) { ostringstream msg;
      msg << "exit " << f.name << ", result: " << rr.rhs;
      debug(msg.str().c_str()); }
#endif
    if (retv) {
      if (f.n+f.m != 0) {
	// do cleanup
	Function *free_fun = module->getFunction("pure_pop_args");
	f.builder.CreateCall3(free_fun, retv, UInt(f.n), UInt(f.m));
      }
      if (rp) debug_redn(rp, retv);
      f.builder.CreateRet(retv);
    } else
      toplevel_codegen(rr.rhs, rp);
    rulebb = nextbb;
  }
  f.fmap.first();
}

/* Make sure to make this the very last thing in this file. TargetSelect.h
   pulls in LLVM's config.h file which may stomp on our own config
   settings! */

#if LLVM26
#include <llvm/Target/TargetSelect.h>

void interpreter::init_llvm_target()
{
  llvm::InitializeNativeTarget();
}
#endif
