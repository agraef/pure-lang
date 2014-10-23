
/* Copyright (c) 2008-2013 by Albert Graef <Dr.Graef@t-online.de>.

   This file is part of the Pure runtime.

   The Pure runtime is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at your
   option) any later version.

   Pure is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
   more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
#pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif
#ifdef __MINGW32__
#include <malloc.h>
#endif

#include "interpreter.hh"
#include "util.hh"
#include <sstream>
#include <stdarg.h>
#include <errno.h>
#include <sys/types.h>
#ifndef __MINGW32__
#include <sys/wait.h>
#endif
#if USE_PCRE
#include <pcreposix.h>
#else
#include <regex.h>
#endif
#include <fnmatch.h>
#include <glob.h>

#if LLVM33
#include <llvm/IR/CallingConv.h>
#else
#include <llvm/CallingConv.h>
#endif
#include <llvm/PassManager.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/MemoryBuffer.h>

#include "config.h"

#ifdef HAVE_LLVM_SUPPORT_DYNAMICLIBRARY_H
#include <llvm/Support/DynamicLibrary.h>
#else
#include <llvm/System/DynamicLibrary.h>
#endif
#ifdef HAVE_LLVM_SUPPORT_RAW_OSTREAM_H
#include <llvm/Support/raw_ostream.h>
#endif
// LLVM 3.5
#ifdef HAVE_LLVM_IR_CALLSITE_H
#include <llvm/IR/CallSite.h>
#else
#include <llvm/Support/CallSite.h>
#endif
#ifdef HAVE_LLVM_LINKER_LINKER_H
#include <llvm/Linker/Linker.h>
#else
#include <llvm/Linker.h>
#endif

#include "gsl_structs.h"

uint8_t interpreter::g_verbose = 0;
bool interpreter::g_interactive = false;
interpreter* interpreter::g_interp = 0;
char *interpreter::baseptr = 0;
// provide a reasonable default for the stack size (8192K - 128K for
// interpreter and runtime)
int interpreter::stackmax = (8192-128)*1024;
int interpreter::stackdir = 0;
int interpreter::brkflag = 0;
int interpreter::brkmask = 0;
bool interpreter::g_init = false;

map<uint32_t, void (*)(void*)> interpreter::locals_destroy_cb;

static void* resolve_external(const std::string& name)
{
  /* If we come here, the dynamic loader has already tried everything to
     resolve the function, so instead we just print an error message and
     return a dummy function which raises a Pure exception when called. In any
     case that's better than aborting the program (which is what the JIT will
     do when we return NULL here). */
  cout.flush();
  cerr << "error trying to resolve external: "
       << (name.compare(0, 2, "$$") == 0?"<<anonymous>>":name) << '\n';
  return (void*)pure_unresolved;
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
  stoplevel = 0; tracelevel = -1;
  debug_skip = false;
}

void interpreter::init_jit_mode()
{
#if LLVM27
  JIT->DisableLazyCompilation(eager_jit);
#else
  eager_jit = false;
#endif
}

#if defined(__MINGW32__) && defined(LIBREGEX) && !__MINGW64__
// Support for legacy mingw32 systems which still have libgnurx-0.dll.
// FIXME: We should rather use the latest GNU regex package from the
// MinGW project instead, as we already do in the mingw64 port (cf.
// http://kemovitra.blogspot.de/2009/07/mingw-porting-gnu-regex-to-windows.html)
// then this kludge can go away.
#undef LIBREGEX
#define LIBREGEX "libgnurx-0.dll"
#endif

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

  xsym_prefix = 0;

  source_options[HOST] = true;
  codegen_options.insert(pair<string,bool*>("interactive", &interactive_mode));
  codegen_options.insert(pair<string,bool*>("debugging", &debugging));
  codegen_options.insert(pair<string,bool*>("symbolic", &symbolic));
  codegen_options.insert(pair<string,bool*>("checks", &checks));
  codegen_options.insert(pair<string,bool*>("const", &consts));
  codegen_options.insert(pair<string,bool*>("fold", &folding));
  codegen_options.insert(pair<string,bool*>("tc", &use_fastcc));
  codegen_options.insert(pair<string,bool*>("warn", &compat));
  codegen_options.insert(pair<string,bool*>("warn2", &compat2));
#if USE_BIGINT_PRAGMA
  codegen_options.insert(pair<string,bool*>("bigint", &bigints));
#endif
  readonly_options.insert("interactive");
  readonly_options.insert("debugging");

  __baseptr_save = 0;
  nwrapped = 0; fptr = 0; __fptr_save = 0;
  sstk_sz = 0; sstk_cap = 0x10000; // 64K
  sstk = (pure_expr**)malloc(sstk_cap*sizeof(pure_expr*));
  __sstk_save = 0;
  assert(sstk);

  ap = (pure_aframe*)malloc(ASTACKSZ*sizeof(pure_aframe));
  assert(ap); aplist.push_back(ap);
  abp = ap; aep = ap+ASTACKSZ; afreep = 0;

  // Initialize the JIT.

  using namespace llvm;

#if !LLVM27
  // LLVM 2.6 and earlier always do lazy JITing, so this flag *must* be false.
  eager_jit = false;
#endif

  /* Accommodate the major API breakage in recent LLVM versions. This is just
     horrible, maybe we should drop support for anything older than LLVM 2.6
     in the future. */
#if LLVM26
  init_llvm_target();
  module = new Module(modname, llvm::getGlobalContext());
#else
  module = new Module(modname);
#endif
#if !LLVM27
  MP = new ExistingModuleProvider(module);
#endif
#if LLVM31
  llvm::EngineBuilder factory(module);
  factory.setEngineKind(llvm::EngineKind::JIT);
  factory.setAllocateGVsWithCode(false);
#if USE_FASTCC || FAST_JIT
  llvm::TargetOptions Opts;
#if USE_FASTCC
  Opts.GuaranteedTailCallOpt = true;
#endif
#if FAST_JIT
#warning "You selected FAST_JIT. This isn't recommended!"
  Opts.EnableFastISel = true;
#endif
  factory.setTargetOptions(Opts);
#endif
  JIT = factory.create();
#else // LLVM 3.0 or earlier
#if LLVM26
  string error;
#if LLVM27
  JIT = ExecutionEngine::create(module, false, &error,
#else
  JIT = ExecutionEngine::create(MP, false, &error,
#endif
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
#if LLVM27
  /* LLVM 2.7 and later: Enable lazy compilation if requested. (With earlier
     LLVM versions, JITing is always done lazily, so the eager_jit flag is
     effectively ignored and this call isn't needed.) */
  if (!eager_jit) JIT->DisableLazyCompilation(false);
#endif
#else // LLVM 2.5 and earlier
#if FAST_JIT
  JIT = ExecutionEngine::create(MP, false, 0, true);
#else
  JIT = ExecutionEngine::create(MP);
#endif
#endif // LLVM 2.5 and earlier
#endif // LLVM 3.0 or earlier
  assert(JIT);
#if LLVM27
  FPM = new FunctionPassManager(module);
#else
  FPM = new FunctionPassManager(MP);
#endif

  // Set up the optimizer pipeline. Start with registering info about how the
  // target lays out data structures.
#if LLVM35
  module->setDataLayout(JIT->getDataLayout());
#else
  FPM->add(new TargetData(*JIT->getTargetData()));
#endif
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
#if LLVM31
  // It seems that this is needed for LLVM 3.1 and later.
  FPM->doInitialization();
#endif

  // Install a fallback mechanism to resolve references to the runtime, on
  // systems which do not allow the program to dlopen itself.
  JIT->InstallLazyFunctionCreator(resolve_external);

  // Generic pointer type. LLVM doesn't like void*, so we use a pointer to a
  // dummy struct instead. (This is a bit of a kludge. We'd rather use char*,
  // as suggested in the LLVM documentation, but we need to keep char* and
  // void* apart.)
  {
    std::vector<llvm_const_Type*> elts;
    Type *VoidTy = struct_type("void", elts);
    VoidPtrTy = PointerType::get(VoidTy, 0);
  }

  // Char pointer type.
  CharPtrTy = PointerType::get(int8_type(), 0);

  // int and double pointers.
  IntPtrTy = PointerType::get(int32_type(), 0);
  DoublePtrTy = PointerType::get(double_type(), 0);

  // Complex numbers (complex double).
  {
    std::vector<llvm_const_Type*> elts;
    elts.push_back(ArrayType::get(double_type(), 2));
    ComplexTy = struct_type(elts);
    ComplexPtrTy = PointerType::get(ComplexTy, 0);
  }

  // GSL-compatible matrix types. These are used to marshall GSL matrices in
  // the C interface.
  {
    std::vector<llvm_const_Type*> elts;
    elts.push_back(size_t_type());	// size1
    elts.push_back(size_t_type());	// size2
    elts.push_back(size_t_type());	// tda
    elts.push_back(VoidPtrTy);		// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(int32_type());	// owner
    GSLMatrixTy = struct_type("struct.__gsl__matrix", elts);
    GSLMatrixPtrTy = PointerType::get(GSLMatrixTy, 0);
  }
  {
    std::vector<llvm_const_Type*> elts;
    elts.push_back(size_t_type());	// size1
    elts.push_back(size_t_type());	// size2
    elts.push_back(size_t_type());	// tda
    elts.push_back(DoublePtrTy);	// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(int32_type());	// owner
    GSLDoubleMatrixTy = struct_type("struct.__gsl__matrix_double", elts);
    GSLDoubleMatrixPtrTy = PointerType::get(GSLDoubleMatrixTy, 0);
  }
  {
    std::vector<llvm_const_Type*> elts;
    elts.push_back(size_t_type());	// size1
    elts.push_back(size_t_type());	// size2
    elts.push_back(size_t_type());	// tda
    elts.push_back(ComplexPtrTy);	// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(int32_type());	// owner
    GSLComplexMatrixTy = struct_type("struct.__gsl__matrix_complex", elts);
    GSLComplexMatrixPtrTy = PointerType::get(GSLComplexMatrixTy, 0);
  }
  {
    std::vector<llvm_const_Type*> elts;
    elts.push_back(size_t_type());	// size1
    elts.push_back(size_t_type());	// size2
    elts.push_back(size_t_type());	// tda
    elts.push_back(IntPtrTy);		// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(int32_type());	// owner
    GSLIntMatrixTy = struct_type("struct.__gsl__matrix_int", elts);
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
#ifdef LLVM30
    // LLVM 3.0 named structs make recursive types quite straightforward.
    ExprTy = llvm::StructType::create
      (llvm::getGlobalContext(), "struct.__pure__expr");
    std::vector<llvm_const_Type*> elts;
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    elts.push_back(PointerType::get(ExprTy, 0));
    elts.push_back(PointerType::get(ExprTy, 0));
    llvm::ArrayRef<llvm::Type*> myelts = elts;
    ExprTy->setBody(myelts);
#else
    // Recursive struct, the old way.
    PATypeHolder StructTy = opaque_type();
    std::vector<llvm_const_Type*> elts;
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    elts.push_back(PointerType::get(StructTy, 0));
    elts.push_back(PointerType::get(StructTy, 0));
    ExprTy = struct_type(elts);
    cast<OpaqueType>(StructTy.get())->refineAbstractTypeTo(ExprTy);
    ExprTy = cast<StructType>(StructTy.get());
    module->addTypeName("struct.__pure__expr", ExprTy);
#endif
  }
  {
    std::vector<llvm_const_Type*> elts;
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    IntExprTy = struct_type("struct.__pure__intexpr", elts);
  }
  {
    std::vector<llvm_const_Type*> elts;
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    elts.push_back(double_type());
    DblExprTy = struct_type("struct.__pure__dblexpr", elts);
  }
  {
    std::vector<llvm_const_Type*> elts;
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    elts.push_back(CharPtrTy);
    StrExprTy = struct_type("struct.__pure__strexpr", elts);
  }
  {
    std::vector<llvm_const_Type*> elts;
    elts.push_back(int32_type());
    elts.push_back(int32_type());
    elts.push_back(VoidPtrTy);
    PtrExprTy = struct_type("struct.__pure__ptrexpr", elts);
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
  /* KLUDGE: This is needed on FC12 Rawhide to correctly resolve the strcmp()
     function from the C library. (For some reason, dlsym() returns the wrong
     address for this function, dynamic linker bug?) */
  declare_extern((void*)strcmp,
		 "strcmp",          "int",    2, "void*", "void*");

  /* This is needed to do equality checks for non-linearities (Pure 0.40+) and
     custom type checking (Pure 0.47+). */
  declare_extern((void*)same,
		 "same",            "bool",   2, "expr*", "expr*");
  declare_extern((void*)pure_typecheck,
		 "pure_typecheck",  "bool",   2, "int", "expr*");
  declare_extern((void*)pure_safe_typecheck,
		 "pure_safe_typecheck","bool",2, "int", "expr*");

  declare_extern((void*)pure_clos,
		 "pure_clos",       "expr*", -7, "bool", "int", "int", "int",
		                                 "void*", "void*", "int");

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

  declare_extern((void*)matrix_check,
		 "matrix_check",    "bool",   3, "expr*", "int", "int");
  declare_extern((void*)matrix_elem_at2,
		 "matrix_elem_at2", "expr*",  3, "expr*", "int", "int");

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

  declare_extern((void*)pure_listvq,
		 "pure_listvq",     "expr*",  2, "size_t", "expr**");
  declare_extern((void*)pure_listv2q,
		 "pure_listv2q",    "expr*",  3, "size_t", "expr**", "expr*");
  declare_extern((void*)pure_tuplevq,
		 "pure_tuplevq",    "expr*",  2, "size_t", "expr**");

  declare_extern((void*)pure_intlistvq,
		 "pure_intlistvq",  "expr*",  2, "size_t", "int*");
  declare_extern((void*)pure_intlistv2q,
		 "pure_intlistv2q", "expr*",  3, "size_t", "int*", "expr*");
  declare_extern((void*)pure_inttuplevq,
		 "pure_inttuplevq", "expr*",  2, "size_t", "int*");

  declare_extern((void*)pure_doublelistvq,
		 "pure_doublelistvq","expr*", 2, "size_t", "double*");
  declare_extern((void*)pure_doublelistv2q,
		 "pure_doublelistv2q","expr*", 3, "size_t", "double*", "expr*");
  declare_extern((void*)pure_doubletuplevq,
		 "pure_doubletuplevq","expr*", 2, "size_t", "double*");

  declare_extern((void*)pure_bigintlistvq,
		 "pure_bigintlistvq","expr*", 4, "size_t",
		 sizeof(mp_limb_t)==8?"int64*":"int*", "int*", "int*");
  declare_extern((void*)pure_bigintlistv2q,
		 "pure_bigintlistv2q","expr*", 5, "size_t",
		 sizeof(mp_limb_t)==8?"int64*":"int*", "int*", "int*",
		 "expr*");
  declare_extern((void*)pure_biginttuplevq,
		 "pure_biginttuplevq","expr*", 4, "size_t",
		 sizeof(mp_limb_t)==8?"int64*":"int*", "int*", "int*");

  declare_extern((void*)pure_strlistvq,
		 "pure_strlistvq",  "expr*",  3, "size_t", "char*", "int*");
  declare_extern((void*)pure_strlistv2q,
		 "pure_strlistv2q", "expr*",  4, "size_t", "char*", "int*",
		 "expr*");
  declare_extern((void*)pure_strtuplevq,
		 "pure_strtuplevq", "expr*",  3, "size_t", "char*", "int*");

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
  declare_extern((void*)pure_get_matrix_data_int64,
		 "pure_get_matrix_data_int64", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_data_float,
		 "pure_get_matrix_data_float", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_data_double,
		 "pure_get_matrix_data_double", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_vector_void,
		 "pure_get_matrix_vector_void", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_vector_char,
		 "pure_get_matrix_vector_char", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_vector_byte,
		 "pure_get_matrix_vector_byte", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_vector_short,
		 "pure_get_matrix_vector_short", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_vector_int,
		 "pure_get_matrix_vector_int", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_vector_int64,
		 "pure_get_matrix_vector_int64", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_vector_float,
		 "pure_get_matrix_vector_float", "void*", 1, "expr*");
  declare_extern((void*)pure_get_matrix_vector_double,
		 "pure_get_matrix_vector_double", "void*", 1, "expr*");
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

  declare_extern((void*)pure_checks,
		 "pure_checks",    "void",   0);

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

  declare_extern((void*)pure_sentry,
		 "pure_sentry",     "expr*",  2, "expr*", "expr*");
  declare_extern((void*)pure_tag,
		 "pure_tag",        "expr*",  2, "int", "expr*");
  declare_extern((void*)pure_check_tag,
		 "pure_check_tag",  "bool",   2, "int", "expr*");
  declare_extern((void*)pure_add_rtti,
		 "pure_add_rtti",   "void",   2, "char*", "int");
  declare_extern((void*)pure_add_rtty,
		 "pure_add_rtty",   "void",   3, "int", "int", "void*");

  declare_extern((void*)faust_float_ui,
		 "faust_float_ui",  "void*",  0);
  declare_extern((void*)faust_double_ui,
		 "faust_double_ui", "void*",  0);
  declare_extern((void*)faust_free_ui,
		 "faust_free_ui", "void",     1, "void*");
  declare_extern((void*)faust_make_info,
		 "faust_make_info","expr*",   4, "int", "int", "void*",
		 "char*");
  declare_extern((void*)faust_new_metadata,
		 "faust_new_metadata", "void*", 0);
  declare_extern((void*)faust_free_metadata,
		 "faust_free_metadata", "void", 1, "void*");
  declare_extern((void*)faust_make_metadata,
		 "faust_make_metadata","expr*", 1, "void*");
  declare_extern((void*)faust_add_rtti,
		 "faust_add_rtti", "void",    3, "char*", "int", "bool");
}

interpreter::interpreter(int _argc, char **_argv)
    : argc(_argc), argv(_argv),
    verbose(0), compat(false), compat2(false), compiling(false),
    eager_jit(false), interactive(false), debugging(false), texmacs(false),
    symbolic(true), checks(true), folding(true), consts(true),
    bigints(false), use_fastcc(true),
    pic(false), strip(true), restricted(false), ttymode(false),
    override(false),
    stats(false), stats_mem(false), temp(0),  ps("> "), libdir(""),
    histfile("/.pure_history"), modname("pure"),
    interactive_mode(false), escape_mode(0),
    source_level(0), skip_level(0), last_tag(0), logging(false),
    nerrs(0), modno(-1), modctr(0), source_s(0), output(0),
    result(0), lastres(0), mem(0), exps(0), tmps(0), freectr(0),
    specials_only(false), module(0),
    JIT(0), FPM(0), astk(0), sstk(__sstk),
    stoplevel(0), tracelevel(-1), debug_skip(false), trace_skip(false),
    fptr(__fptr), tags(0), line(0), column(0), tags_init(false),
    declare_op(false)
{
  init();
}

interpreter::interpreter(int32_t nsyms, char *syms,
			 pure_expr ***vars, void **vals,
			 int32_t *arities, void **externs,
			 pure_expr ***_sstk, void **_fptr)
  : argc(0), argv(0),
    verbose(0), compat(false), compat2(false), compiling(false),
    eager_jit(false), interactive(false), debugging(false), texmacs(false),
    symbolic(true), checks(true), folding(true), consts(true),
    bigints(false), use_fastcc(true),
    pic(false), strip(true), restricted(true), ttymode(false), override(false),
    stats(false), stats_mem(false), temp(0), ps("> "), libdir(""),
    histfile("/.pure_history"), modname("pure"),
    interactive_mode(false), escape_mode(0),
    source_level(0), skip_level(0),
    /* NOTE: We use a different range of pointer tags here, so that tags
       generated at compile time won't conflict with those generated at run
       time. If we start out with 0x7fffffff, the first tag generated at run
       time will become the smallest negative number in the 32 bit range. */
    last_tag(0x7fffffff), logging(false),
    nerrs(0), modno(-1), modctr(0), source_s(0), output(0),
    result(0), lastres(0), mem(0), exps(0), tmps(0), freectr(0),
    specials_only(false), module(0),
    JIT(0), FPM(0), astk(0), sstk(*_sstk),
    stoplevel(0), tracelevel(-1), debug_skip(false), trace_skip(false),
    fptr(*(Env**)_fptr), tags(0), line(0), column(0), tags_init(false),
    declare_op(false)
{
  using namespace llvm;
  init();
  // In a batch-compiled module, there is only a single global instance of the
  // fptr and sstk variables. When switching interpreters, these values have
  // to be saved somewhere.
  __fptr_save = (Env**)malloc(sizeof(Env*));
  __sstk_save = (pure_expr***)malloc(sizeof(pure_expr**));
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
    llvm_const_Type* rettype = named_type(s_type);
    vector<llvm_const_Type*> argtypes(n_args);
    for (size_t i = 0; i < n_args; i++) {
      sin >> s_type;
      argtypes[i] = named_type(s_type);
    }
    if (sin.fail() || sin.eof()) break;
    externals[f] = ExternInfo(f, s_name, rettype, argtypes, 0);
  }
  for (int32_t f = 1; f <= nsyms; f++) {
    symbol& sym = symtab.sym(f);
    size_t p = symsplit(sym.s);
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
      vector<llvm_const_Type*> argt(info.argtypes.size(), ExprPtrTy);
      FunctionType *ft = func_type(ExprPtrTy, argt, false);
      Function *fp = Function::Create(ft, Function::InternalLinkage,
				      "$$wrap."+info.name, module);
      sys::DynamicLibrary::AddSymbol("$$wrap."+info.name, externs[f]);
      info.f = fp;
    }
  }
}

interpreter::~interpreter()
{
  // get rid of interpreter-local storage
  for (map<uint32_t,void*>::iterator it = locals.begin(), end = locals.end();
       it != end; ++it) {
    uint32_t key = it->first;
    void *ptr = it->second;
    if (!ptr) continue;
    map<uint32_t, void (*)(void*)>::iterator
      jt = interpreter::locals_destroy_cb.find(key);
    if (jt != interpreter::locals_destroy_cb.end()) {
      void (*destroy)(void*) = jt->second;
      if (destroy) destroy(ptr);
    }
  }
  // get rid of global environments and the LLVM data
  globenv.clear(); typeenv.clear(); macenv.clear();
  globalfuns.clear(); globaltypes.clear(); globalvars.clear();
  // free the shadow stack
  if (g_interp != this && __sstk_save)
    free(*__sstk_save);
  else
    free(sstk);
  // free the activation stack
  for (list<pure_aframe*>::iterator it = aplist.begin(); it != aplist.end();
       ++it) free(*it);
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
  if (FPM) {
#if LLVM31
    // It seems that this is needed for LLVM 3.1 and later.
    FPM->doFinalization();
#endif
    delete FPM;
  }
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

void interpreter::swap_interpreters(interpreter *interp)
{
  if (g_interp != interp) {
    if (g_interp) g_interp->save_context();
    g_interp = interp;
    if (interp) interp->restore_context();
  }
}

void interpreter::init_sys_vars(const string& version,
				const string& host,
				const list<string>& argv)
{
  interpreter* s_interp = g_interp;
  swap_interpreters(this);
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
  swap_interpreters(s_interp);
}

// Activation stack.

pure_aframe *interpreter::push_aframe(size_t sz)
{
  pure_aframe *a = get_aframe();
  assert(a); a->e = 0; a->sz = sz;
  a->fp = 0; a->n = a->m = a->count = 0; a->argv = 0;
  a->prev = astk; astk = a;
  return a;
}

void interpreter::pop_aframe()
{
  pure_aframe *a = astk;
  assert(a);
  astk = a->prev;
  free_aframe(a);
}

// Errors and warnings.

void
interpreter::error(const yy::location& l, const string& m)
{
  string m1 = m;
  if (m.find("bad token") != string::npos)
    m1 = "bad anonymous function or pointer value";
  nerrs++;
  if (logging || source_s) {
    ostringstream msg;
    msg << *l.begin.filename << ", line " << l.begin.line
	<< ": " << m1 << '\n';
    errmsg += msg.str();
    errpos.push_back(errinfo(*l.begin.filename, l.begin.line, l.begin.column,
			     l.end.line, l.end.column, msg.str()));
  } else {
    /* KLUDGE: We might be called in circumstances (interpreter embedded in a
       C application) where the standard I/O streams cout and cerr aren't
       properly initialized yet. Therefore use plain C stdio here, instead of
       the corresponding C++ routines. */
#if 0
    cout.flush();
    cerr << *l.begin.filename << ", line " << l.begin.line
	 << ": " << m1 << '\n';
#else
    fflush(stdout);
    fprintf(stderr, "%s, line %u: %s\n", l.begin.filename->c_str(),
	    l.begin.line, m1.c_str());
    fflush(stderr);
#endif
  }
}

void
interpreter::error(const string& m)
{
  nerrs++;
  if (logging || source_s) {
    ostringstream msg;
    msg << m << '\n';
    errmsg += msg.str();
    errpos.push_back(msg.str());
  } else {
    cout.flush();
    cerr << m << '\n';
  }
}

void
interpreter::warning(const yy::location& l, const string& m)
{
  if (logging) {
    ostringstream msg;
    msg << *l.begin.filename << ", line " << l.begin.line
	<< ": " << m << '\n';
    errmsg += msg.str();
    errpos.push_back(errinfo(*l.begin.filename, l.begin.line, l.begin.column,
			     l.end.line, l.end.column, msg.str()));
  } else if (!source_s) {
    cout.flush();
    cerr << *l.begin.filename << ", line " << l.begin.line
	 << ": " << m << '\n';
  }
}

void
interpreter::warning(const string& m)
{
  if (logging) {
    ostringstream msg;
    msg << m << '\n';
    errmsg += msg.str();
    errpos.push_back(msg.str());
  } else if (!source_s) {
    cout.flush();
    cerr << m << '\n';
  }
}

// Report memory usage.

void interpreter::mem_usage(size_t &used, size_t &free)
{
  mem_usage(used);
  free = freectr;
  used -= free;
}

void interpreter::mem_usage(size_t &total)
{
  total = 0;
  if (!mem) return;
  total = mem->p-mem->x;
  pure_mem *m = mem->next;
  while (m) {
    total += MEMSIZE;
    m = m->next;
  }
}

// Evaluation statistics.

void interpreter::begin_stats()
{
  if (interactive && stats) {
    clocks = clock();
    if (stats_mem) {
      mem_usage(memsize);
      old_memctr = memctr = freectr;
    }
  }
}

void interpreter::end_stats()
{
  if (interactive && stats) {
    clocks = clock()-clocks;
    if (stats_mem) {
      size_t new_memsize;
      mem_usage(new_memsize);
      /* We either have made new allocations, in which case the freelist must
	 have gone empty at some point (memctr == 0), or all used expression
	 memory came from the freelist. In either case the maximum amount of
	 used memory at any one point is given by the new total amount of
	 memory minus the old total, plus the difference between old and
	 smallest size of the freelist. */
#if 0
      // FIXME: Disable these checks for now, as these figures may not be 100%
      // accurate if "stats" got invoked through evalcmd.
      assert(new_memsize >= memsize && memctr <= old_memctr);
      assert(new_memsize <= memsize || memctr == 0);
      memsize = new_memsize-memsize+old_memctr-memctr;
#else
      if (new_memsize >= memsize && memctr <= old_memctr)
	memsize = new_memsize-memsize+old_memctr-memctr;
      else
	memsize = 0;
#endif
    }
  }
}

void interpreter::report_stats()
{
  if (interactive && stats) {
    cout << ((double)clocks)/(double)CLOCKS_PER_SEC << "s";
    if (stats_mem)
      cout << ", " << memsize << " cells";
    cout << endl;
  }
}

/* Search for a source file. Absolute file names (starting with a slash) are
   taken as is. Otherwise, the 'search' flag determines the way that the
   search is to be performed. If it is zero, only the current working
   directory is searched. If it is nonzero, then the directories in
   include_dirs are searched, followed by libdir (if nonempty), and finally
   the current working directory. In addition, if 'search' is 1, the "current"
   directory (either srcdir or, if srcdir is empty, the current working
   directory) is searched first. In either case, if the resulting absolute
   pathname is a symbolic link, the destination is used instead, and finally
   the pathname is canonicalized. */

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

#define BUFSIZE 1024

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

static string searchdir(const string& srcdir, const string& libdir,
			const list<string>& include_dirs,
			const string& script, int search = 1)
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
  string fname = script;
  if (!absname(script)) {
    // resolve relative pathname
    if (!search) {
      fname = workdir+script;
    } else {
      if (search == 1) {
	fname = (srcdir.empty()?workdir:srcdir)+script;
	if (chkfile(fname)) goto found;
      }
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
    }
  }
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
  if (chdir(cwd)) perror("chdir");
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

// Source options (Pure 0.49+).

#include <ctype.h>
#include <fnmatch.h>
#include <algorithm>

bool interpreter::is_enabled(const string& optname)
{
  map<string,bool*>::iterator jt = codegen_options.find(optname);
  if (jt != codegen_options.end()) return *jt->second;
  map<string,bool>::iterator it = source_options.find(optname);
  if (it != source_options.end()) return it->second;
  // Check the environment for a default.
  string envname = optname;
  std::transform(envname.begin(), envname.end(), envname.begin(), ::toupper);
  envname.insert(0, "PURE_OPTION_");
  const char *val = getenv(envname.c_str());
  bool flag = !val || atoi(val) != 0;
  if (!val) {
    bool glob = optname.find_first_of("*?[]") != string::npos;
    if (glob) {
      // The symbol is actually a glob pattern, query the host triplet.
      flag = fnmatch(optname.c_str(), HOST, 0)==0;
      // We don't cache the result here.
      return flag;
    }
    // Predefined options.
    if (optname == "compiled") // batch-compiled
      flag = compiling;
    else if (optname.compare(0, strlen("version-"), "version-") == 0) {
      // Version check. The rest of the option name has the format
      // major.minor[+-]. '+' or '-' means that the version must be at least
      // or at most the given version, respectively; otherwise we're looking
      // for exactly the given version.
      string version = PACKAGE_VERSION;
      string vers = optname.substr(strlen("version-"));
      int act_major = 0, act_minor = 0, want_major = 0, want_minor = 0;
      char mode = 0;
      int res1 = sscanf(version.c_str(), "%d.%d", &act_major, &act_minor);
      int res2 = sscanf(vers.c_str(), "%d.%d%[+-]",
			&want_major, &want_minor, &mode);
      if (res1 == 2 && res2 >= 2) {
	if (mode == '+')
	  flag = act_major > want_major ||
	    (act_major == want_major && act_minor >= want_minor);
	else if (mode == '-')
	  flag = act_major < want_major ||
	    (act_major == want_major && act_minor <= want_minor);
	else
	  flag = act_major == want_major && act_minor == want_minor;
      } else
	flag = 0;
    } else
      // default value; not cached
      return flag;
  }
  // Cache the value.
  source_options[optname] = flag;
  return flag;
}

bool interpreter::is_defined(const string& optname)
{
  map<string,bool*>::iterator jt = codegen_options.find(optname);
  if (jt != codegen_options.end()) return true;
  map<string,bool>::iterator it = source_options.find(optname);
  if (it != source_options.end()) return true;
  // Check the environment for a default.
  string envname = optname;
  std::transform(envname.begin(), envname.end(), envname.begin(), ::toupper);
  envname.insert(0, "PURE_OPTION_");
  if (getenv(envname.c_str()) != NULL) return true;
  // Predefined options.
  bool glob = optname.find_first_of("*?[]") != string::npos;
  if (glob || optname == "compiled" ||
      optname.compare(0, strlen("version-"), "version-") == 0)
    return true;
  else
    return false;
}

void interpreter::enable(const string& optname, bool flag)
{
  map<string,bool*>::iterator it = codegen_options.find(optname);
  if (it != codegen_options.end()) {
    if (readonly_options.find(optname) == readonly_options.end())
      *it->second = flag;
    else
      warning("warning: option '"+optname+"' is read-only");
  } else
    source_options[optname] = flag;
}

// Scoped namespaces (Pure 0.43+).

void interpreter::push_namespace(string *ns, int32_t bracket)
{
  string parent = *symtab.current_namespace;
  map< string, set<int32_t> > search_namespaces = *symtab.search_namespaces;
  active_namespaces.push_front(nsinfo(parent, search_namespaces));
  set_namespace(ns, bracket);
}

void interpreter::pop_namespace()
{
  assert(!active_namespaces.empty());
  *symtab.current_namespace = active_namespaces.front().parent;
  *symtab.search_namespaces = active_namespaces.front().search_namespaces;
  active_namespaces.pop_front();
}

void interpreter::set_namespace(string *ns, int32_t bracket)
{
  size_t k = symsplit(*ns);
  if (k != string::npos &&
      namespaces.find(ns->substr(0, k)) == namespaces.end()) {
    symtab.current_namespace->clear();
    throw err("unknown namespace '"+ns->substr(0, k)+"'");
  } else {
    if (!ns->empty()) namespaces.insert(*ns);
    delete symtab.current_namespace;
    symtab.current_namespace = ns;
    if (bracket > 0) {
      symbol& sym = symtab.sym(bracket);
      assert(sym.g); // must be an outfix symbol
      // set the special namespace attribute of the brackets
      if (sym.ns) delete sym.ns;
      sym.ns = new string(*ns);
    }
  }
}

void interpreter::clear_namespace(int32_t bracket)
{
  symtab.current_namespace->clear();
  if (bracket > 0) {
    symbol& sym = symtab.sym(bracket);
    assert(sym.g); // must be an outfix symbol
    if (sym.ns) {
      // revoke the special namespace attribute of the brackets
      delete sym.ns;
      sym.ns = 0;
    }
  }
}

// Ctags/etags support.

uint32_t count_args(expr x, int32_t& f);

void interpreter::add_tags(rulel *rl)
{
  set<int32_t> syms;
  for (rulel::iterator i = rl->begin(), end = rl->end(); i != end; i++) {
    expr x = i->lhs;
    int32_t f;
    count_args(x, f);
    if (f > 0 && syms.find(f) == syms.end()) {
      symbol& sym = symtab.sym(f);
      add_tag(sym.s, srcabs, line, column);
      syms.insert(f);
    }
  }
}

void interpreter::add_tags(rule *r)
{
  if (r->lhs.is_null()) return;
  expr x = r->lhs;
  int32_t f;
  count_args(x, f);
  if (f > 0) {
    symbol& sym = symtab.sym(f);
    add_tag(sym.s, srcabs, line, column);
  }
}

void interpreter::add_tags(expr pat)
{
  env vars; vinfo vi;
  qual = true;
  expr lhs = bind(vars, vi, lcsubst(pat));
  build_env(vars, lhs);
  qual = false;
  for (env::const_iterator it = vars.begin(); it != vars.end(); ++it) {
    int32_t f = it->first;
    const symbol& sym = symtab.sym(f);
    add_tag(sym.s, srcabs, line, column);
  }
}

void interpreter::add_tags(const string& id, const string& asid)
{
  string name = asid.empty()?id:asid;
  string absid = make_absid(name);
  symbol* sym = symtab.lookup(absid);
  if (sym) add_tag(sym->s, srcabs, line, column);
}

void interpreter::add_tags(list<string> *ids)
{
  if (!ids) return;
  for (list<string>::iterator it = ids->begin(), end = ids->end();
       it != end; it++) {
    string name = *it;
    string absid = make_absid(name);
    symbol* sym = symtab.lookup(absid);
    if (sym) add_tag(sym->s, srcabs, line, column);
  }
}

static string tagfile(const string& tagsdir, const string& name)
{
  if (name.empty())
    return name;
  else if (strncmp(tagsdir.c_str(), name.c_str(), tagsdir.size()) == 0)
    return name.substr(tagsdir.size());
  else
    return name;
}

void interpreter::init_tags()
{
  if (tags_init) return;
  char cwdbuf[BUFSIZE];
  string cwd;
  if (getcwd(cwdbuf, BUFSIZE))
    cwd = cwdbuf;
  else {
    perror("getcwd");
    cwd = "";
  }
  cwd = unixize(cwd);
  if (!cwd.empty() && cwd[cwd.size()-1] != '/') cwd += "/";
  if (tagsfile.empty()) {
    if (tags == 2)
      tagsfile = "TAGS";
    else
      tagsfile = "tags";
    tagsdir = cwd;
  } else {
    tagsfile = unixize(tagsfile);
    string s = "";
    list<string> dirs;
    tagsfile = searchdir(s, s, dirs, tagsfile, 0);
    tagsdir = dirname(tagsfile);
  }
  tags_init = true;
}

void interpreter::add_tag(const string& tagname, const string& filename,
			  unsigned int line, unsigned int column)
{
  if (filename.empty()) return;
  init_tags();
  string name = tagfile(tagsdir, filename);
  tag_list[name].push_back(TagInfo(tagname, line, column));
}

#include <iostream>
#include <fstream>
#include <sys/types.h>
#include <sys/stat.h>

struct LineInfo {
  size_t offs;
  char *s;
  LineInfo() : offs(0), s(0) {}
  LineInfo(size_t _offs, char *_s) : offs(_offs), s(_s) {}
};

static char *readfile(const char *name, map<unsigned,LineInfo>& lines)
{
  struct stat st;
  if (stat(name, &st)) {
    perror("stat");
    return 0;
  }
  size_t size = st.st_size;
  FILE *fp = fopen(name, "rb");
  if (!fp) return 0;
  char *buf = (char*)malloc(size+1);
  if (!buf) {
    fclose(fp);
    return 0;
  }
  size = fread(buf, 1, size, fp);
  fclose(fp);
  buf[size] = 0;
  char *p = buf, *q;
  unsigned line = 1;
  size_t offs = 0;
  lines.clear(); lines[line] = LineInfo(offs, p);
  while ((q = strchr(p, '\n'))) {
    /* Pure scripts are encoded in UTF-8 no matter what the current system
       encoding is, so we compute offsets in terms of UTF-8 multibytes here.
       NOTE: All the documentation on the etags file format that I could find
       indicates that Emacs wants byte offsets here, but that doesn't seem to
       be true; the UTF-8 multibyte offsets appear to work fine. */
    *q++ = 0; offs += u8strlen(p)+1;
    lines[++line] = LineInfo(offs, q);
    p = q;
  }
  return buf;
}

struct CtagInfo {
  const char *tag, *file;
  unsigned line;
  CtagInfo(const char *t, const char *f, unsigned l)
    : tag(t), file(f), line(l) {}
};

bool ctag_cmp(const CtagInfo& x, const CtagInfo& y)
{
  int ret = strcmp(x.tag, y.tag);
  if (ret != 0)
    return ret<0;
  else
    return false;
}

void interpreter::print_tags()
{
  init_tags();
  if (chdir(tagsdir.c_str())) perror("chdir");
  if (tags == 2) {
    ofstream out(tagsfile.c_str());
    for (list<string>::const_iterator it = tag_files.begin(),
	   end = tag_files.end(); it != end; it++) {
      const string& filename = *it;
      const list<TagInfo>& tags = tag_list[filename];
      if (tags.empty()) continue;
      ostringstream sout;
      map<unsigned,LineInfo> lines;
      char *text = readfile(filename.c_str(), lines);
      if (!text) continue;
      for (list<TagInfo>::const_iterator it = tags.begin(), end = tags.end();
	   it != end; it++) {
	const TagInfo& info = *it;
	map<unsigned,LineInfo>::iterator tt = lines.find(info.line);
	if (tt == lines.end()) break;
	char *act = tt->second.s;
	size_t offs = tt->second.offs;
	// try to find the text leading up to the tag
	string s(act);
	string t = info.tag;
	size_t k = s.find(t);
	unsigned line = info.line;
	if (k == string::npos && (k = symsplit(info.tag)) != string::npos) {
	  // qualified name, may be used unqualified here
	  t = info.tag.substr(k+2);
	  k = s.find(t);
	} else if (k == string::npos && info.tag == "neg") {
	  // synonym for unary -
	  k = s.find("-");
	  if (k != string::npos) t = "-";
	}
	if (k == string::npos) {
	  // Still couldn't find the tag, look for it on a continuation line.
	  list<TagInfo>::const_iterator jt = it; jt++;
	  while (jt != end && jt->line == info.line &&
		 jt->column == info.column) ++jt;
	  if (jt != end) {
	    unsigned next_line = jt->line;
	    for (unsigned l = 1; line+l <= next_line; l++) {
	      s = lines[line+l].s;
	      k = s.find(t);
	      if (k != string::npos) {
		line += l;
		offs = lines[line].offs;
		break;
	      }
	    }
	  }
	}
	if (k == string::npos) continue; // give up
	if (k > 0 || t != info.tag)
	  s = s.substr(0, k+t.size());
	else
	  s.clear();
	if (s.empty())
	  sout << info.tag << "\x7f" << line << "," << offs << endl;
	else
	  sout << s << "\x7f" << info.tag << "\x01" << line << ","
	       << offs << endl;
      }
      free(text);
      out << "\f\n" << filename << "," << sout.str().size() << endl
	  << sout.str();
    }
  } else if (tags == 1) {
    size_t n_entries = 0;
    for (list<string>::const_iterator it = tag_files.begin(),
	   end = tag_files.end(); it != end; it++) {
      const string& filename = *it;
      const list<TagInfo>& tags = tag_list[filename];
      n_entries += tags.size();
    }
    vector<CtagInfo> ctags; ctags.reserve(n_entries);
    for (list<string>::const_iterator it = tag_files.begin(),
	   end = tag_files.end(); it != end; it++) {
      const string& filename = *it;
      const list<TagInfo>& tags = tag_list[filename];
      if (tags.empty()) continue;
      for (list<TagInfo>::const_iterator it = tags.begin(), end = tags.end();
	   it != end; it++) {
	const TagInfo& info = *it;
	ctags.push_back(CtagInfo(info.tag.c_str(), filename.c_str(),
				 info.line));
      }
    }
    stable_sort(ctags.begin(), ctags.end(), ctag_cmp);
    ofstream out(tagsfile.c_str());
    for (size_t i = 0; i < n_entries; i++) {
      const CtagInfo& info = ctags[i];
      out << info.tag << "\t" << info.file << "\t" << info.line << endl;
    }
  }
}

// Run the interpreter on a source file, collection of source files, or on
// string data.

#ifndef DLLEXT
#define DLLEXT ".so"
#endif
#ifndef DSPEXT
// These are dsp modules which reside in bitcode files created with the Faust
// compiler.
#define DSPEXT ".bc"
#endif
#ifndef BCEXT
// Standard LLVM bitcode modules.
#define BCEXT ".bc"
#endif
#ifndef PUREEXT
#define PUREEXT ".pure"
#endif

static string strip_modname(const string& name)
{
  string modname = name;
  size_t p = modname.rfind(".");
  if (p != string::npos) modname.erase(p);
  p = modname.find_last_of("/\\:");
  if (p != string::npos) modname.erase(0, p+1);
  return modname;
}

static string strip_filename(const string& name)
{
  string fname = name;
  size_t p = fname.find_last_of("/\\:");
  if (p != string::npos) fname.erase(0, p+1);
  return fname;
}

static void dsp_errmsg(string name, string* msg)
{
  // Give more elaborate diagnostics for failed attempts to load a Faust DSP.
  if (!msg) return;
  name = strip_filename(name);
  if (!msg->empty())
    *msg = name+": "+*msg;
  else
    *msg = name+": Error linking dsp file";
}

// LLVM provides methods to do this, but they're not portable across LLVM
// versions, so we do our own.
static llvm::MemoryBuffer *get_membuf(const char *name, string *msg)
{
  using namespace llvm;
  FILE *fp = fopen(name, "rb");
  if (!fp) {
    if (msg) *msg = strerror(errno);
    return 0;
  }
  struct stat st;
  if (fstat(fileno(fp), &st)) {
    if (msg) *msg = strerror(errno);
    fclose(fp);
    return 0;
  }
  size_t size = st.st_size;
  MemoryBuffer *buf = MemoryBuffer::getNewMemBuffer(size, name);
  if (!buf) {
    if (msg) *msg = "Not enough memory";
    fclose(fp);
    return 0;
  }
  if (fread(const_cast<char*>(buf->getBufferStart()), size, 1, fp) < size &&
      ferror(fp)) {
    if (msg) *msg = strerror(errno);
    fclose(fp);
    delete buf;
    return 0;
  }
  fclose(fp);
  return buf;
}

#if HAVE_DECL_LLVM__PARSEBITCODEFILE
// We have parseBitcodeFile(), this is in LLVM 3.5 and later. Must emulate
// ParseBitcodeFile.
static llvm::Module *ParseBitcodeFile(llvm::MemoryBuffer *Buffer,
				      llvm::LLVMContext& Context,
				      std::string *ErrMsg)
{
  using namespace llvm;
  ErrorOr<Module *> ModuleOrErr = parseBitcodeFile(Buffer, Context);
  if (error_code EC = ModuleOrErr.getError()) {
    if (ErrMsg) *ErrMsg = EC.message();
    return 0;
  } else
  return ModuleOrErr.get();
}
#endif

bool interpreter::LoadFaustDSP(bool priv, const char *name, string *msg,
			       const char *modnm)
{
  using namespace llvm;
  // Determine the basename of the Faust module. This will be used to mangle
  // the Faust functions and give the namespace of the Faust functions in Pure
  // land (so for easy access it's better if the basename is a valid Pure
  // identifier).
  string modname = modnm?string(modnm):strip_modname(name);
  // Keep track of module timestamps (modification times). Note that in
  // difference to the general bitcode interface we allow the module to be
  // reloaded here, if it has been modified since the last load.
  time_t mtime = 0;
  struct stat st;
  if (!stat(name, &st)) mtime = st.st_mtime;
  // Check whether the module has already been loaded.
  bool loaded = loaded_dsps.find(modname) != loaded_dsps.end();
  bool declared = loaded &&
    loaded_dsps[modname].declared(*symtab.current_namespace);
  bool modified = !loaded || loaded_dsps[modname].t < mtime;
  if (loaded) {
    // Module already loaded. Do some consistency checks.
    if (declared &&
	loaded_dsps[modname].priv[*symtab.current_namespace] != priv) {
      string scope = (!priv)?"private":"public";
      if (msg)
	*msg = "Module was previously '" + scope + "' in this namespace";
      dsp_errmsg(name, msg);
      return false;
    }
    // Check whether there's anything to do.
    if (declared && !modified) return true;
  }
  MemoryBuffer *buf = get_membuf(name, msg);
  if (!buf) {
    dsp_errmsg(name, msg);
    return false;
  }
  Module *M = ParseBitcodeFile(buf,
#ifdef LLVM26
			       getGlobalContext(),
#endif
			       msg);
  delete buf;
  if (!M) {
    dsp_errmsg(name, msg);
    return false;
  }
  // Determine the Faust module classname suffix (this used to be 'llvm', but
  // is now 'mydsp' by default and can be set with the -cn option).
  string classname = "_llvm";
  string buildui = "buildUserInterface";
  size_t len = buildui.length();
  bool found = false;
  for (Module::iterator it = M->begin(), end = M->end(); it != end; ) {
    Function &f = *(it++);
    string name = f.getName();
    if (name.compare(0, len, buildui) == 0) {
      classname = name.substr(len);
      found = true;
      break;
    }
  }
  if (!found) {
    // This doesn't look like a valid Faust bitcode module, bail out.
    if (msg) *msg = "Not a valid dsp file";
    dsp_errmsg(name, msg);
    delete M;
    return false;
  }
  // Check whether getSampleRate is available.
  bool have_getSampleRate = M->getFunction("getSampleRate"+classname) != 0;
  // Figure out whether our dsp uses float or double values.
  Function *compute = M->getFunction("compute"+classname);
  llvm_const_Type *type = compute->getFunctionType()->getParamType(2);
  bool is_double = type ==
    PointerType::get(PointerType::get(double_type(), 0), 0);
  if (loaded && modified) {
    // Do some more checking to make sure that the programmer didn't suddenly
    // change his mind about the precision of floating point data (-double
    // vs. -single). Note that we can't allow sudden changes in the ABI since
    // we only patch up function pointers when reloading a Faust module, which
    // would render existing wrappers invalid if the ABI was changed on the fly.
    if (loaded_dsps[modname].dbl != is_double) {
      string prec = (!is_double)?"double":"single";
      if (msg)
	*msg = "Module was previously compiled for " + prec + " precision";
      dsp_errmsg(name, msg);
      delete M;
      return false;
    }
  }
  // Fix up the target layout and triple set by the Faust compiler, in case
  // the dsp module was created on a different platform. (FIXME: We assume
  // that the Faust code itself is platform-agnostic.)
  string layout = JIT->getTargetData()->getStringRepresentation(),
    triple = HOST;
  M->setDataLayout(layout); M->setTargetTriple(triple);
  // Mangle the global names of the Faust module since they are usually the
  // same for every module. XXXFIXME: Currently we leave the type names alone
  // and rely on the linker to make them unique instead. This works, but may
  // produce weird names when emitting LLVM assembler code in the batch
  // compiler. In the future we may want to mangle those, too, if only for
  // cosmetic purposes.
  list<string> funs, aux_funs, vars;
  // Mangle the function names.
  for (Module::iterator it = M->begin(), end = M->end(); it != end; ++it) {
    Function &f = *it;
    string name = f.getName();
    // We always force external linkage here in order to avoid the automatic
    // renaming that the linker does for internal symbols.
    f.setLinkage(Function::ExternalLinkage);
    // Faust interface routines are stropped with the classname suffix.
    size_t p = name.rfind(classname);
    if (p != string::npos && p+classname.length() == name.length()) {
      string fname = name.substr(0, p);
      f.setName("$$faust$"+modname+"$"+fname);
      funs.push_back(fname);
    } else if (!f.isDeclaration()) {
      // Internal Faust function. Mangle the names of these as well, so that
      // the batch compiler recognizes them (and doesn't accidentally strip
      // them).
      f.setName("$$__faust__$"+modname+"$"+name);
      aux_funs.push_back(name);
    }
  }
  // Mangle the variable names.
  for (Module::global_iterator it = M->global_begin(), end = M->global_end();
       it != end; ++it) {
    GlobalVariable &v = *it;
    if (!v.hasName()) continue;
    string name = v.getName();
    string vname = "$$__faust__$"+modname+"$"+name;
    v.setLinkage(GlobalVariable::ExternalLinkage);
    vars.push_back(name);
    v.setName(vname);
  }
  if (loaded && modified) {
    // Get rid of all globals of the old module.
    bcdata_t& data = loaded_dsps[modname];
    list<Function*>& funptrs = data.funptrs;
    list<GlobalVariable*>& varptrs = data.varptrs;
    for (list<Function*>::iterator f = funptrs.begin();
	 f != funptrs.end(); ++f) {
      string fname = (*f)->getName();
      (*f)->dropAllReferences();
      JIT->freeMachineCodeForFunction(*f);
    }
    for (list<GlobalVariable*>::iterator v = varptrs.begin();
	 v != varptrs.end(); ++v) {
      string vname = (*v)->getName();
      (*v)->dropAllReferences();
      // XXXFIXME: Do we have to free the pointer returned by
      // updateGlobalMapping() here?
      JIT->updateGlobalMapping(*v, 0);
    }
    for (list<Function*>::iterator f = funptrs.begin();
	 f != funptrs.end(); ++f) (*f)->eraseFromParent();
    for (list<GlobalVariable*>::iterator v = varptrs.begin();
	 v != varptrs.end(); ++v) (*v)->eraseFromParent();
  }
  // Link the mangled module into the Pure module. This only needs to be done
  // if the module was modified.
  if (modified && Linker::LinkModules(module, M,
#ifdef LLVM30
				      Linker::DestroySource,
#endif
				      msg)) {
    delete M;
    dsp_errmsg(name, msg);
    return false;
  }
  delete M;
  // Add some convenience functions.
  list<string> myfuns;
  myfuns.push_back("newinit");
  myfuns.push_back("info");
  if (modified) {
    // The newinit function calls new then init, yielding a properly
    // initialized dsp instance. It takes one i32 argument, the samplerate.
    {
      Function *newfun = module->getFunction("$$faust$"+modname+"$new");
      Function *initfun = module->getFunction("$$faust$"+modname+"$init");
      llvm_const_Type *dsp_ty = newfun->getReturnType();
      vector<llvm_const_Type*> argt(1, int32_type());
      FunctionType *ft = func_type(dsp_ty, argt, false);
      Function *f = Function::Create(ft, Function::ExternalLinkage,
				     "$$faust$"+modname+"$newinit", module);
      BasicBlock *bb = basic_block("entry", f);
#ifdef LLVM26
      Builder b(getGlobalContext());
#else
      Builder b;
#endif
      b.SetInsertPoint(bb);
      // Call new.
      vector<Value*> args;
      Value *v = b.CreateCall(newfun, mkargs(args));
      // Check for null pointer results.
      BasicBlock *okbb = basic_block("ok"), *skipbb = basic_block("skip");
      b.CreateCondBr
	(b.CreateICmpNE
	 (v, ConstantPointerNull::get(dyn_cast<PointerType>(dsp_ty)), "cmp"),
	 okbb, skipbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      // Call init.
      args.push_back(v);
      Function::arg_iterator a = f->arg_begin();
      args.push_back(a);
      b.CreateCall(initfun, mkargs(args));
      b.CreateBr(skipbb);
      // Return the result.
      f->getBasicBlockList().push_back(skipbb);
      b.SetInsertPoint(skipbb);
      b.CreateRet(v);
    }
    // The info function takes a dsp as parameter and returns a triple with
    // the number of inputs and outputs and the UI description, in the same
    // format as the faust_info function in the pure-faust module.
    {
      // This is the Faust function to initialize the UI data structure.
      Function *buildUserInterface = module->getFunction
	("$$faust$"+modname+"$buildUserInterface");
      // Type of the above; the first argument gives the dsp type, the second
      // one the UI type.
      llvm_const_FunctionType *ht = buildUserInterface->getFunctionType();
      llvm_const_Type *dsp_type = ht->getParamType(0);
      llvm_const_Type *ui_type = ht->getParamType(1);
      // Create the call interface of our convenience function.
      vector<llvm_const_Type*> argt(1, dsp_type);
      FunctionType *ft = func_type(ExprPtrTy, argt, false);
      Function *f = Function::Create(ft, Function::ExternalLinkage,
				     "$$faust$"+modname+"$info", module);
      BasicBlock *bb = basic_block("entry", f);
#ifdef LLVM26
      Builder b(getGlobalContext());
#else
      Builder b;
#endif
      b.SetInsertPoint(bb);
      // Call getNumInputs and getNumOutputs to obtain the number of input and
      // output channels.
      Function *getNumInputs =
	module->getFunction("$$faust$"+modname+"$getNumInputs");
      Function *getNumOutputs =
	module->getFunction("$$faust$"+modname+"$getNumOutputs");
      vector<Value*> args;
      Function::arg_iterator a = f->arg_begin();
      llvm_const_FunctionType *gt = getNumInputs->getFunctionType();
      // In some revisions getNumInputs and getNumOutputs are parameterless
      // functions; avoid a failed assertion for these.
      if (gt->getNumParams() > 0)
	args.push_back(b.CreateBitCast(a, gt->getParamType(0)));
      Value *n_in = b.CreateCall(getNumInputs, mkargs(args));
      Value *n_out = b.CreateCall(getNumOutputs, mkargs(args));
      // Call the runtime function to create the internal UI data structure.
      Function *uifun = module->getFunction
	(is_double?"faust_double_ui":"faust_float_ui");
      args.clear();
      Value *v = b.CreateCall(uifun, mkargs(args));
      // Call the Faust function to initialize the UI data structure. Note
      // that we need to cast the second void* argument to the proper pointer
      // type expected by the buildUserInterface routine.
      args.push_back(a);
      args.push_back(b.CreateBitCast(v, ui_type));
      b.CreateCall(buildUserInterface, mkargs(args));
      // Construct the info tuple.
      Function *infofun = module->getFunction("faust_make_info");
      // Pass the module name so that faust_make_info knows about the dsp name.
      GlobalVariable *w = global_variable
	(module, ArrayType::get(int8_type(), modname.size()+1), true,
	 GlobalVariable::InternalLinkage, constant_char_array(modname.c_str()),
	 "$$faust_str");
      // "cast" the char array to a char*
      Value *idx[2] = { ConstantInt::get(interpreter::int32_type(), 0),
			ConstantInt::get(interpreter::int32_type(), 0) };
      Value *p = b.CreateGEP(w, mkidxs(idx, idx+2));
      args.clear();
      args.push_back(n_in);
      args.push_back(n_out);
      args.push_back(v);
      args.push_back(p);
      Value *u = b.CreateCall(infofun, mkargs(args));
      // Get rid of the internal UI data structure.
      Function *freefun = module->getFunction("faust_free_ui");
      args.clear();
      args.push_back(v);
      b.CreateCall(freefun, mkargs(args));
      // Return the result.
      b.CreateRet(u);
    }
    // The meta function takes no parameter and returns a list with the global
    // and static metadata of a dsp class, in the same format as the
    // faust_meta function in the pure-faust module. Note that older Faust2
    // versions (before git rev. 2ecd0a40) don't support this, so this
    // function is only added if this feature is actually available.
    Function *metadata = module->getFunction("$$faust$"+modname+"$metadata");
    if (metadata) {
      myfuns.push_back("meta");
      // Type of the above; the first argument gives the metadata type.
      llvm_const_FunctionType *ht = metadata->getFunctionType();
      llvm_const_Type *meta_type = ht->getParamType(0);
      // Create the call interface of our convenience function.
      vector<llvm_const_Type*> argt;
      FunctionType *ft = func_type(ExprPtrTy, argt, false);
      Function *f = Function::Create(ft, Function::ExternalLinkage,
				     "$$faust$"+modname+"$meta", module);
      BasicBlock *bb = basic_block("entry", f);
#ifdef LLVM26
      Builder b(getGlobalContext());
#else
      Builder b;
#endif
      b.SetInsertPoint(bb);
      // Call the runtime function to create the internal meta data structure.
      Function *newfun = module->getFunction("faust_new_metadata");
      vector<Value*> args;
      Value *v = b.CreateCall(newfun, mkargs(args));
      // Call the Faust function to initialize the meta data structure. Note
      // that we need to cast the void* argument to the proper pointer type
      // expected by the metadata routine.
      args.push_back(b.CreateBitCast(v, meta_type));
      b.CreateCall(metadata, mkargs(args));
      // Construct the metadata list.
      Function *makefun = module->getFunction("faust_make_metadata");
      args.clear();
      args.push_back(v);
      Value *u = b.CreateCall(makefun, mkargs(args));
      // Get rid of the internal meta data structure.
      Function *freefun = module->getFunction("faust_free_metadata");
      args.clear();
      args.push_back(v);
      b.CreateCall(freefun, mkargs(args));
      // Return the result.
      b.CreateRet(u);
    }
    // getSampleRate is already implemented in the latest faust2 versions.
    // On older faust2 versions we emulate it if possible.
    GlobalVariable *sr = have_getSampleRate?0:
      module->getNamedGlobal("$$__faust__$"+modname+"fSamplingFreq");
    if (sr) {
      // The getSampleRate function takes a dsp as parameter and returns its
      // sample rate.
      myfuns.push_back("getSampleRate");
      Function *newfun = module->getFunction("$$faust$"+modname+"$new");
      llvm_const_Type *dsp_ty = newfun->getReturnType();
      vector<llvm_const_Type*> argt(1, dsp_ty);
      FunctionType *ft = func_type(int32_type(), argt, false);
      Function *f = Function::Create(ft, Function::ExternalLinkage,
				     "$$faust$"+modname+"$getSampleRate",
				     module);
      BasicBlock *bb = basic_block("entry", f);
#ifdef LLVM26
      Builder b(getGlobalContext());
#else
      Builder b;
#endif
      b.SetInsertPoint(bb);
      Value *v = b.CreateLoad(sr);
      b.CreateRet(v);
    }
  }
  funs.insert(funs.end(), myfuns.begin(), myfuns.end());
  // Record the newly created function and variable pointers. These are to be
  // stored in the module table so that we can remove them later when the
  // module gets reloaded.
  list<Function*> funptrs;
  list<GlobalVariable*> varptrs;
  for (list<string>::iterator f = funs.begin(); f != funs.end(); ++f) {
    string fname = "$$faust$"+modname+"$"+*f;
    Function *g = module->getFunction(fname);
    assert(g);
    funptrs.push_back(g);
  }
  for (list<string>::iterator f = aux_funs.begin(); f != aux_funs.end(); ++f) {
    string fname = "$$__faust__$"+modname+"$"+*f;
    Function *g = module->getFunction(fname);
    assert(g);
    funptrs.push_back(g);
  }
  for (list<string>::iterator v = vars.begin(); v != vars.end(); ++v) {
    string vname = "$$__faust__$"+modname+"$"+*v;
    GlobalVariable *u = module->getGlobalVariable(vname);
    assert(u);
    varptrs.push_back(u);
  }
  // Create the namespace if necessary.
  if (symtab.current_namespace->empty())
    namespaces.insert(modname);
  else
    namespaces.insert(*symtab.current_namespace+"::"+modname);
  // Update the module data. This must be done here so that the type tag is
  // initialized when generating the wrappers.
  loaded_dsps[modname].declare(*symtab.current_namespace, priv);
  bcmap::iterator mod = loaded_dsps.find(modname);
  assert(mod != loaded_dsps.end());
  mod->second.t = mtime;
  mod->second.dbl = is_double;
  mod->second.funptrs = funptrs;
  mod->second.varptrs = varptrs;
  if (mod->second.tag == 0) mod->second.tag = pure_make_tag();
  dsp_mods[mod->second.tag] = mod;
#if 0 // debugging
  for (list<string>::iterator v = vars.begin(); v != vars.end(); ++v) {
    string vname = "$$__faust__$"+modname+"$"+*v;
    GlobalVariable *u = module->getGlobalVariable(vname);
    assert(u);
    void *p = JIT->getPointerToGlobal(u);
    fprintf(stderr, ">>> var %s = %p\n", vname.c_str(), p);
    u->dump();
  }
#endif
  // Create wrappers.
  for (list<string>::iterator it = funs.begin(), end = funs.end();
       it != end; ++it) {
    string fname = "$$faust$"+modname+"$"+*it;
    Function *f = module->getFunction(fname);
    assert(f);
    verifyFunction(*f);
    if (FPM) FPM->run(*f);
    // The name under which the function is accessible in Pure.
    string asname = modname+"::"+*it;
    // The function type.
    llvm_const_FunctionType *ft = f->getFunctionType();
    llvm_const_Type* rest = ft->getReturnType();
    size_t n = ft->getNumParams();
    vector<llvm_const_Type*> argt(n);
    for (size_t i = 0; i < n; i++) argt[i] = ft->getParamType(i);
    string restype = dsptype_name(rest);
    list<string> argtypes;
    for (size_t i = 0; i < n; i++) argtypes.push_back(dsptype_name(argt[i]));
    if (loaded && modified) {
      /* There's no need to actually regenerate the wrapper, we only have to
         patch up the function pointer here. */
      GlobalVariable *v = module->getNamedGlobal("$"+fname);
      if (v) {
	void **fp = (void**)JIT->getPointerToGlobal(v);
	assert(fp);
	*fp = JIT->getPointerToFunction(f);
      } else {
	/* The variable may not actually exist in the JIT yet if we're being
	   called in a batch-compiled program which has the same dsp module
	   already linked into it. In this case we fix up the symbol table so
	   that the wrapper and its variable get recreated below. */
	assert(!declared);
	symbol *sym = symtab.sym(asname);
	assert(sym);
	externals.erase(sym->f);
	globalvars.erase(sym->f);
      }
    }
    if (!declared) {
      // Manufacture an extern declaration for the function so that it can be
      // called in Pure land.
      declare_extern(priv, fname, restype, argtypes, false, 0, asname, false);
      // Always require this function, so that it can be called in
      // batch-compiled scripts.
      symbol *sym = symtab.sym(asname);
      assert(sym);
      required.push_back(sym->f);
    }
#if 0 // debugging
    void * p = JIT->getPointerToFunction(f);
    fprintf(stderr, ">>> fun %s = %p\n", fname.c_str(), p);
    symbol *sym = symtab.sym(asname);
    if (!sym) continue;
    ExternInfo info(sym->f, fname, rest, argt, f);
    cerr << "\n" << info << ";\n";
    f->dump();
#endif
  }
  return true;
}

static void bc_errmsg(string name, string* msg)
{
  if (!msg) return;
  name = strip_filename(name);
  if (!msg->empty())
    *msg = name+": "+*msg;
  else
    *msg = name+": Error linking bitcode file";
}

bool interpreter::LoadBitcode(bool priv, const char *name, string *msg)
{
  using namespace llvm;
  string modname = strip_modname(name);
  bool loaded = loaded_bcs.find(modname) != loaded_bcs.end();
  bool declared = loaded &&
    loaded_bcs[modname].declared(*symtab.current_namespace);
  if (loaded) {
    // Module already loaded. Do some consistency checks.
    if (declared &&
	loaded_bcs[modname].priv[*symtab.current_namespace] != priv) {
      string scope = (!priv)?"private":"public";
      if (msg)
	*msg = "Module was previously '" + scope + "' in this namespace";
      bc_errmsg(name, msg);
      return false;
    }
    // Check whether there's anything to do.
    if (declared) return true;
  }
  MemoryBuffer *buf = get_membuf(name, msg);
  if (!buf) {
    bc_errmsg(name, msg);
    return false;
  }
  Module *M = ParseBitcodeFile(buf,
#ifdef LLVM26
			       getGlobalContext(),
#endif
			       msg);
  delete buf;
  if (!M) {
    bc_errmsg(name, msg);
    return false;
  }
  // Check the target layout and triple of the module against our target and
  // give diagnostics in case of a mismatch. NOTE: Currently we ignore
  // mismatches in the target triple and just assume that bitcode files are ok
  // if the data layouts match. Not sure whether this assumption is always
  // valid.
  string layout = JIT->getTargetData()->getStringRepresentation(),
    triple = HOST;
  // We only give diagnostics on first load, to prevent a cascade of error
  // messages.
#if 0
  if (!loaded && !M->getTargetTriple().empty() &&
      M->getTargetTriple() != triple) {
    if (msg)
      *msg = "Mismatch in target architecture '"+M->getTargetTriple()+"'";
    bc_errmsg(name, msg);
    return false;
  }
#endif
#if LLVM35
  if (!loaded && !M->getDataLayoutStr().empty() && M->getDataLayoutStr() != layout) {
#else
  if (!loaded && !M->getDataLayout().empty() && M->getDataLayout() != layout) {
#endif
    // Clang 2.9 has some minor mismatches with the JIT data layout (bug?),
    // which are irrelevant for our purposes, so for the time being we just
    // check endianness and pointer sizes here.
    const TargetData &jit_dl = *JIT->getTargetData(), mod_dl = TargetData(M);
    if (jit_dl.isLittleEndian() != mod_dl.isLittleEndian() ||
	jit_dl.getPointerSize() != mod_dl.getPointerSize()) {
      if (msg)
	*msg = "Mismatch in data layout '"+
#if LLVM35
	  M->getDataLayoutStr()
#else
	  M->getDataLayout()
#endif
	  +"'";
      bc_errmsg(name, msg);
      return false;
    }
  }
  M->setDataLayout(layout); M->setTargetTriple(triple);
  // Build a list of the external functions of the module so that we can wrap
  // them later.
  list<string> funs;
  for (Module::iterator it = M->begin(), end = M->end(); it != end; ) {
    Function &f = *(it++);
    if (!f.isDeclaration() &&
	f.getLinkage() == Function::ExternalLinkage) {
      funs.push_back(f.getName());
    }
  }
  // Link the bitcode module into the Pure module. This only needs to be done
  // if the module wasnd't loaded before.
  if (!loaded && Linker::LinkModules(module, M,
#ifdef LLVM30
				     Linker::DestroySource,
#endif
				     msg)) {
    delete M;
    bc_errmsg(name, msg);
    return false;
  }
  delete M;
  // Create wrappers.
  for (list<string>::iterator it = funs.begin(), end = funs.end();
       it != end; ++it) {
    string fname = *it;
    Function *f = module->getFunction(fname);
    assert(f);
    verifyFunction(*f);
    if (FPM) FPM->run(*f);
    // The name under which the function is accessible in Pure.
    string asname = fname;
    // The function type.
    llvm_const_FunctionType *ft = f->getFunctionType();
    llvm_const_Type* rest = ft->getReturnType();
    const bool varargs = ft->isVarArg();
    size_t n = ft->getNumParams();
    vector<llvm_const_Type*> argt(n);
    for (size_t i = 0; i < n; i++) argt[i] = ft->getParamType(i);
    string restype = bctype_name(rest);
    list<string> argtypes;
    bool ok = true;
    if (!loaded) {
      // Check the result type for compatibility with Pure.
      ok = restype != "<unknown C type>";
      for (size_t i = 0; i < n; i++) {
	string argtype = bctype_name(argt[i]);
	// Check the argument type.
	if (argtype == "<unknown C type>") {
	  ok = false;
	  break;
	}
	argtypes.push_back(argtype);
      }
    } else {
      // Module has been loaded before, so assume that we're ok.
      for (size_t i = 0; i < n; i++) {
	string argtype = bctype_name(argt[i]);
	argtypes.push_back(argtype);
      }
    }
    if (ok) {
      // Manufacture an extern declaration for the function so that it can be
      // called in Pure land.
      declare_extern(priv, fname, restype, argtypes, varargs, 0, asname, false);
#if 0 // debugging
      symbol *sym = symtab.sym(asname);
      if (!sym) continue;
      ExternInfo info(sym->f, fname, rest, argt, f, varargs);
      cerr << "\n" << info << ";\n";
      f->dump();
#endif
    } else {
      // Bad argument or result type (probably a struct-by-val). Print a
      // warning in such cases.
      symbol *sym = symtab.sym(asname);
      if (!sym) continue;
      ExternInfo info(sym->f, fname, rest, argt, f, varargs);
      ostringstream msg;
      msg << strip_filename(name) << ": warning: extern function '" << fname
	  << "' with bad prototype: " << info;
      warning(msg.str());
    }
  }
  loaded_bcs[modname].declare(*symtab.current_namespace, priv);
  return true;
}

/* Compile a bit of inline code on the fly. This is done by invoking a
   suitable compiler and loading the resulting bitcode. By default, C code is
   assumed. Other languages can be selected by placing a corresponding tag
   into the first line of the code (right behind the opening bracket):

   - "-*- c -*-" (or "-*- C -*-", case is insignificant in the language label)
     selects the C language, which is also the default. The command with which
     the compiler is to be invoked can be set with the PURE_CC environment
     variable, by default this is clang. The necessary options to switch
     the compiler to bitcode output are supplied automatically.

   - "-*- c++ -*-" selects C++ (PURE_CXX environment variable, clang++ by
     default).

   - "-*- fortran -*-" selects Fortran (PURE_FC environment variable,
     gfortran -fplugin=dragonegg by default). Optionally, the fortran tag may
     be followed by a two-digit sequence denoting the desired Fortran standard
     (as of this writing, gfortran recognizes the Fortran 90, 95, 03 and 08
     standards). If this is omitted, the default is old-style (fixed form)
     Fortran.

   - "-*- ats -*-" selects ATS (PURE_ATSCC environment variable, patscc by
     default). Contributed by Barry Schwartz <sortsmill@crudfactory.com>, see
     http://www.ats-lang.org/.

   - "-*- dsp:name -*-" selects Faust (PURE_FAUST environment variable, faust
     by default), where 'name' denotes the name of the Faust dsp, which is
     used as the namespace for the dsp interface functions.

   The language tag itself is removed from the code before it is submitted for
   compilation. */

static string lang_tag(string &code, string &modname)
{
  size_t p = code.find_first_not_of(" \t\r\n"), q = 0;
  modname.clear();
  if (p == string::npos || code.compare(p, 3, "-*-") != 0) return "c";
  p += 3;
  p = code.find_first_not_of(" \t", p);
  if (p == string::npos) return "c";
  q = p; p = code.find("-*-", p);
  if (p == string::npos) return "c";
  string tag = code.substr(q, p-q);
  code.erase(0, p+3);
  p = tag.find(":");
  if (p != string::npos) {
    modname = tag.substr(p+1);
    tag.erase(p);
    p = modname.find_last_not_of(" \t");
    if (p == string::npos)
      modname.clear();
    else
      modname.erase(p+1);
  }
  p = tag.find_last_not_of(" \t");
  if (p == string::npos)
    tag.clear();
  else
    tag.erase(p+1);
  for (size_t i = 0; i < tag.size(); i++)
    tag[i] = tolower(tag[i]);
  return tag;
}

#ifdef __MINGW32__
#include <process.h>
#define WIFEXITED(w)   (((w) & 0XFFFFFF00) == 0)
#define WEXITSTATUS(w) (w)
#endif

// On some systems the LLVM toolchain gets installed into special directories
// so that multiple LLVM versions can coexist on the same system. We take care
// of that here.

#ifndef TOOL_PREFIX
#define TOOL_PREFIX ""
#endif

static string tool_prefix = TOOL_PREFIX;

#ifndef EXEEXT
#define EXEEXT ""
#endif

void interpreter::inline_code(bool priv, string &code)
{
  // Get the language tag and configure accordingly.
  string modname, tag = lang_tag(code, modname), ext = "", optargs = "";
  string intermediate_ext = "";
  const char *env, *drv, *args;
  char *asmargs = 0;
  bool remove_intermediate = false;
  // Check to see where we can find clang (used for C, C++ and ATS). If it's
  // not under the tool prefix then assume that it's somewhere on the PATH.
  string clang = tool_prefix + "clang";
  if (!chkfile(clang+EXEEXT)) clang = "clang";
  string clangpp = clang+"++";
  if (tag == "c") {
    static char *cc = NULL;
    if (!cc) cc = strdup(clang.c_str());
    env = "PURE_CC";
    drv = cc;
    args = " -x c -emit-llvm -c ";
  } else if (tag == "c++") {
    static char *cc = NULL;
    if (!cc) cc = strdup(clangpp.c_str());
    env = "PURE_CXX";
    drv = cc;
    args = " -x c++ -emit-llvm -c ";
  } else if (tag.compare(0, 7, "fortran") == 0) {
    string std = tag.substr(7);
    if (!std.empty() &&
	std != "90" && std != "95" && std != "03" && std != "08")
      throw err("unknown Fortran dialect in inline code (try one of 90, 95, 03, 08)");
    env = "PURE_FC"; drv = "gfortran -fplugin=dragonegg";
    // gfortran doesn't understand -x, so we have to do some trickery with
    // filename extensions instead.
    args = " -emit-llvm -c "; ext = ".f"+std;
  } else if (tag == "ats") {
    env = "PURE_ATSCC";
    // The default command is for ATS2-Postiats, which uses gcc as its C
    // compiler by default. We prefer to use clang instead (set by means of
    // the PATSCCOMP environment variable). If you have a working dragonegg
    // plugin installed, you can still override this by setting the PURE_ATSCC
    // variable to 'patscc -fplugin=dragonegg'. NOTE: In any case you should
    // set the PATSHOME environment variable as explained in the ATS2
    // installation instructions.
    drv = "patscc";
    args = " -emit-llvm -c ";
    ext = ".dats";
    intermediate_ext = "_dats.c";
    remove_intermediate = true;
    // We only override patscc's default C compiler if neither PURE_ATSCC nor
    // PATSCCOMP has been set, so that the user still has full control over
    // which AST and C compilers will be used.
    if (!getenv("PURE_ATSCC") && !getenv("PATSCCOMP")) {
      static char *patsccomp = NULL;
      if (!patsccomp) {
	string ccomp = string("PATSCCOMP=") + clang + " -emit-llvm -std=c99 -D_XOPEN_SOURCE -I${PATSHOME} -I${PATSHOME}/ccomp/runtime";
	patsccomp = strdup(ccomp.c_str());
	putenv(patsccomp);
      }
      args = " -c ";
    }
  } else if (tag == "dsp") {
    env = "PURE_FAUST"; drv = "faust -double";
    args = " -lang llvm ";
    const char *opt = getenv("FAUST_OPT");
    if (opt) optargs = string((!*opt||isspace(*opt))?"":" ")+opt;
    if (modname.empty())
      throw err("missing Faust module name in inline code (try dsp:name)");
    // Mangle the module name so that it can be supplied as the Faust class
    // name (-cn).
    string name = modname;
    for (size_t i = 0, n = name.size(); i < n; i++)
      if ((i==0 && !isalpha(name[i])) || !isalnum(name[i]))
	name[i] = '_';
    optargs = " -cn "+name+optargs;
  } else {
    throw err("bad tag '"+tag+
	      "' in inline code (try one of c, fortran, ats, dsp:name)");
  }
  // LLVM tools used in the build process.
  string llvm_as = (chkfile(libdir+"/llvm-as"+EXEEXT)?
		    libdir:tool_prefix)+"llvm-as";
  // Create a temporary file holding the code.
  size_t n = code.size();
  string src = modname;
  if (src.empty()) {
    src = source.empty()?"stdin":source;
    static unsigned count = 0;
    char *buf = (char*)alloca(source.size()+10);
    sprintf(buf, "%s%u", src.c_str(), count++);
    src = buf;
  }
  string tmpl = src+".XXXXXX";
  char *fnm = (char*)malloc(tmpl.size()+1);
  strcpy(fnm, tmpl.c_str());
  int fd = mkstemp(fnm);
  string nm = fnm;
  if (fd<0) goto err;
  if (write(fd, code.c_str(), n) < (ssize_t)n) {
    close(fd);
    unlink(fnm);
    goto err;
  }
  code.clear();
  close(fd);
  if (!ext.empty()) {
    // Add the given filename extension so that the compiler knows what kind
    // of input it gets.
    nm += ext;
    if (rename(fnm, nm.c_str())) {
      unlink(fnm);
      goto err;
    }
  }
  {
    // Invoke the compiler.
    const char *pure_cc = getenv(env);
    if (!pure_cc) pure_cc = drv;
    // Quick and dirty hack to make inlining work with dragonegg, which at
    // present can't generate bitcode files directly, so llvm-as must be
    // used. FIXME: This currently works only if the plugin is really named
    // "dragonegg" and is named explicitly on the command line.
    string ext = ".bc";
    if (strstr(pure_cc, "dragonegg")) {
      ext = ".ll";
      asmargs = strdup(args);
      const char *t = "-emit-llvm -c";
      char *s = strstr(asmargs, t);
      if (s) strncpy(s, "-flto      -S", strlen(t));
      args = asmargs;
    }
    string fname = nm, bcname = string(fnm)+ext, bcname2 = string(fnm)+".bc",
      cmd = string(pure_cc)+args+fname+optargs+" -o "+bcname;
    const char *bcnm = bcname.c_str(), *bcnm2 = bcname2.c_str();
    bool vflag = (verbose&verbosity::compiler) != 0;
    if (vflag) std::cerr << cmd << '\n';
    int status = system(cmd.c_str());
    unlink(nm.c_str());
    if (remove_intermediate) {
      string intermediate_name = string(fnm)+intermediate_ext;
      unlink(intermediate_name.c_str());
    }
    if (asmargs) free(asmargs);
    if (WIFEXITED(status) && WEXITSTATUS(status) == 0) {
      if (ext == ".ll") {
	// Invoke llvm-as to translate intermediate assembler to real bitcode.
	string cmd = llvm_as+" "+bcname+" -o "+bcname2;
	if (vflag) std::cerr << cmd << '\n';
	status = system(cmd.c_str());
	if (WIFEXITED(status) && WEXITSTATUS(status) == 0) {
	  unlink(bcnm);
	  bcnm = bcnm2;
	} else {
	  unlink(bcnm);
	  unlink(bcname2.c_str());
	  goto err;
	}
      }
      string msg;
      // Load the resulting bitcode.
      if (tag == "dsp") {
	// Faust bitcode loader
	if (!LoadFaustDSP(priv, bcnm, &msg, modname.c_str())) {
	  unlink(bcnm);
	  throw err(msg);
	}
      } else {
	// generic bitcode loader
	if (!LoadBitcode(priv, bcnm, &msg)) {
	  unlink(bcnm);
	  throw err(msg);
	}
      }
    } else {
      unlink(bcnm);
      goto err;
    }
    unlink(bcnm);
  }
  return;
 err:
  // general error
  throw err("error compiling inline code");
}

pure_expr* interpreter::run(int priv, const string &_s,
			    bool check, bool sticky)
{
  string s = unixize(_s);
  // check for library modules
  size_t p = s.find(":");
  if (p != string::npos && s.substr(0, p) == "lib") {
    if (priv>=0) {
      string scope = priv?"private":"public";
      throw err("invalid '"+scope+"' specifier");
    }
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
  if (p != string::npos && s.substr(0, p) == "bc") {
    if (p+1 >= s.size()) throw err("empty bitcode file name");
    string msg, name = s.substr(p+1), bcname = name;
    // See whether we need to add the BCEXT suffix.
    if (name.size() <= strlen(BCEXT) ||
	name.substr(name.size()-strlen(BCEXT)) != BCEXT)
      bcname += BCEXT;
    string aname = searchlib(srcdir, libdir, librarydirs, bcname);
    if (!LoadBitcode(priv==1, aname.c_str(), &msg))
      throw err(msg);
    return 0;
  }
  if (p != string::npos && s.substr(0, p) == "dsp") {
    if (p+1 >= s.size()) throw err("empty dsp name");
    string msg, name = s.substr(p+1), dspname = name;
    // See whether we need to add the DSPEXT suffix.
    if (name.size() <= strlen(DSPEXT) ||
	name.substr(name.size()-strlen(DSPEXT)) != DSPEXT)
      dspname += DSPEXT;
    string aname = searchlib(srcdir, libdir, librarydirs, dspname);
    if (!LoadFaustDSP(priv==1, aname.c_str(), &msg))
      throw err(msg);
    return 0;
  }
  // ordinary source file
  if (priv>=0) {
    string scope = priv?"private":"public";
    throw err("invalid '"+scope+"' specifier");
  }
  string name = s, fname = s;
  if (!s.empty()) {
    int flag = check;
    if (p != string::npos && s.substr(0, p) == "sys") {
      if (p+1 >= s.size()) throw err("empty script name");
      name = s.substr(p+1); flag <<= 1; // skip search of current directory
    }
    // See whether we need to add the PUREEXT suffix.
    string purename = name;
    if (name.size() <= strlen(PUREEXT) ||
	name.substr(name.size()-strlen(PUREEXT)) != PUREEXT)
      purename += PUREEXT;
    // First try the name with PUREEXT added.
    fname = searchdir(srcdir, libdir, includedirs, purename, flag);
    if (check && sources.find(fname) != sources.end())
      // already loaded, skip
      return 0;
    FILE *fp = fopen(fname.c_str(), "r");
    if (purename != name) {
      if (!fp) {
	// Try the real name.
	fname = searchdir(srcdir, libdir, includedirs, name, flag);
	if (check && sources.find(fname) != sources.end())
	  // already loaded, skip
	  return 0;
	fp = fopen(fname.c_str(), "r");
      } else
	name = purename;
    }
    /* Check that the file exists. We already do that here so that errors are
       properly reported to eval/evalcmd. */
    if (fp)
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
  string l_srcdir = srcdir, l_srcabs = srcabs;
  int32_t l_modno = modno;
  string *l_current_namespace = symtab.current_namespace;
  map< string, set<int32_t> > *l_search_namespaces = symtab.search_namespaces;
  bool l_symbolic = symbolic, l_checks = checks, l_folding = folding,
    l_consts = consts, l_bigints = bigints, l_use_fastcc = use_fastcc;
  bool l_source_level = source_level, l_skip_level = skip_level;
  bitset<64> l_else_stack = else_stack;
  // save global data
  uint8_t s_verbose = g_verbose;
  bool s_interactive = g_interactive;
  interpreter* s_interp = g_interp;
  g_verbose = verbose;
  g_interactive = interactive = interactive && s.empty();
  swap_interpreters(this);
  // initialize
  nerrs = 0;
  source_level = skip_level = 0;
  source = name; declare_op = false;
  source_s = 0;
  output = 0;
  srcdir = dirname(fname); srcabs = fname;
  if (tags) {
    init_tags();
    tag_files.push_back(tagfile(tagsdir, fname));
  }
  if (sticky)
    ; // keep the current module
  else {
    modno = modctr++;
    symtab.current_namespace = new string;
    symtab.search_namespaces = new map< string, set<int32_t> >;
  }
  errmsg.clear(); errpos.clear();
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
  swap_interpreters(s_interp);
  // restore local data
  interactive = l_interactive;
  source = l_source;
  source_level = l_source_level;
  skip_level = l_skip_level;
  else_stack = l_else_stack;
  nerrs = l_nerrs;
  temp = l_temp;
  source_s = l_source_s;
  output = l_output;
  srcdir = l_srcdir; srcabs = l_srcabs;
  modno = l_modno;
  if (!sticky) {
    delete symtab.current_namespace;
    delete symtab.search_namespaces;
    symtab.current_namespace = l_current_namespace;
    symtab.search_namespaces = l_search_namespaces;
    if (symbolic != l_symbolic || checks != l_checks ||
	use_fastcc != l_use_fastcc) compile();
    symbolic = l_symbolic; checks = l_checks; folding = l_folding;
    consts = l_consts; bigints = l_bigints; use_fastcc = l_use_fastcc;
  }
  // return last computed result, if any
  return result;
}

pure_expr* interpreter::run(int priv, const list<string> &sl,
			    bool check, bool sticky)
{
  uint8_t s_verbose = verbose;
  // Temporarily suppress verbose output for using clause.
  if (verbose) {
    compile();
    verbose = 0;
  }
  for (list<string>::const_iterator s = sl.begin(); s != sl.end(); s++)
    run(priv, *s, check, sticky);
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
  string l_srcdir = srcdir, l_srcabs = srcabs;
  int32_t l_modno = modno;
  string *l_current_namespace = symtab.current_namespace;
  map< string, set<int32_t> > *l_search_namespaces = symtab.search_namespaces;
  bool l_symbolic = symbolic, l_checks = checks, l_folding = folding,
    l_consts = consts, l_bigints = bigints, l_use_fastcc = use_fastcc;
  bool l_source_level = source_level, l_skip_level = skip_level;
  bitset<64> l_else_stack = else_stack;
  // save global data
  uint8_t s_verbose = g_verbose;
  bool s_interactive = g_interactive;
  interpreter* s_interp = g_interp;
  g_verbose = 0;
  g_interactive = interactive = false;
  swap_interpreters(this);
  // initialize
  nerrs = 0;
  source_level = skip_level = 0;
  source = ""; declare_op = false;
  source_s = s.c_str();
  srcdir = ""; srcabs = "";
  modno = modctr++;
  symtab.current_namespace = new string;
  symtab.search_namespaces = new map< string, set<int32_t> >;
  errmsg.clear(); errpos.clear();
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
  swap_interpreters(s_interp);
  // restore local data
  compiling = l_compiling;
  interactive = l_interactive;
  source = l_source;
  source_s = 0;
  source_level = l_source_level;
  skip_level = l_skip_level;
  else_stack = l_else_stack;
  nerrs = l_nerrs;
  source_s = l_source_s;
  srcdir = l_srcdir; srcabs = l_srcabs;
  modno = l_modno;
  delete symtab.current_namespace;
  delete symtab.search_namespaces;
  symtab.current_namespace = l_current_namespace;
  symtab.search_namespaces = l_search_namespaces;
  if (symbolic != l_symbolic || checks != l_checks ||
      use_fastcc != l_use_fastcc) compile();
  symbolic = l_symbolic; checks = l_checks; folding = l_folding;
  consts = l_consts; bigints = l_bigints; use_fastcc = l_use_fastcc;
  // return last computed result, if any
  return result;
}

pure_expr *interpreter::parsestr(const string& s)
{
  // save local data
  bool l_compiling = compiling;
  bool l_interactive = interactive;
  string l_source = source;
  int l_nerrs = nerrs;
  const char *l_source_s = source_s;
  string l_srcdir = srcdir, l_srcabs = srcabs;
  int32_t l_modno = modno;
  string *l_current_namespace = symtab.current_namespace;
  map< string, set<int32_t> > *l_search_namespaces = symtab.search_namespaces;
  bool l_symbolic = symbolic, l_checks = checks, l_folding = folding,
    l_consts = consts, l_bigints = bigints, l_use_fastcc = use_fastcc;
  bool l_source_level = source_level, l_skip_level = skip_level;
  bitset<64> l_else_stack = else_stack;
  // save global data
  uint8_t s_verbose = g_verbose;
  bool s_interactive = g_interactive;
  interpreter* s_interp = g_interp;
  g_verbose = 0;
  g_interactive = interactive = false;
  swap_interpreters(this);
  // initialize
  nerrs = 0;
  source_level = skip_level = 0;
  source = ""; declare_op = false;
  string s1 = "\007"+s;
  source_s = s1.c_str();
  srcdir = ""; srcabs = "";
  modno = modctr++;
  symtab.current_namespace = new string;
  symtab.search_namespaces = new map< string, set<int32_t> >;
  errmsg.clear(); errpos.clear();
  compiling = false;
  bool ok = lex_begin("", true);
  if (ok) {
    yy::parser parser(*this);
    // parse
    if (result) pure_free(result); result = 0;
    parser.parse();
    // finalize
    lex_end();
    // Fix up the error locations. The initial BEL character messes up the
    // column positions in the first line. Also, for some mysterious reason
    // the lexer may sometimes report a position past the length of the source
    // string. We fix these glitches on the fly below.
    size_t nlines = 1, lastpos = 0, pos;
    // count lines in the source string and find the start of the last line
    while ((pos = s.find('\n', lastpos)) != string::npos) {
      nlines++;
      lastpos = pos+1;
    }
    size_t lastlen = strlen(s.c_str()+lastpos);
    for (list<errinfo>::iterator it = errpos.begin(), end = errpos.end();
	 it != end; ++it) {
      if (it->line1 == 1 && it->col1 > 0) it->col1--;
      if (it->line2 == 1 && it->col2 > 0) it->col2--;
      if (it->line1 == (int)nlines && it->col1 > (int)lastlen+1)
	it->col1 = lastlen+1;
      if (it->line2 == (int)nlines && it->col2 > (int)lastlen+1)
	it->col2 = lastlen+1;
    }
  }
  // restore global data
  g_verbose = s_verbose;
  g_interactive = s_interactive;
  swap_interpreters(s_interp);
  // restore local data
  compiling = l_compiling;
  interactive = l_interactive;
  source = l_source;
  source_s = 0;
  source_level = l_source_level;
  skip_level = l_skip_level;
  else_stack = l_else_stack;
  nerrs = l_nerrs;
  source_s = l_source_s;
  srcdir = l_srcdir; srcabs = l_srcabs;
  modno = l_modno;
  delete symtab.current_namespace;
  delete symtab.search_namespaces;
  symtab.current_namespace = l_current_namespace;
  symtab.search_namespaces = l_search_namespaces;
  if (symbolic != l_symbolic || checks != l_checks ||
      use_fastcc != l_use_fastcc) compile();
  symbolic = l_symbolic; checks = l_checks; folding = l_folding;
  consts = l_consts; bigints = l_bigints; use_fastcc = l_use_fastcc;
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
  env vars; expr u = csubst(macsubst(0, subst(vars, rsubst(x))));
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
  env vars; vinfo vi;
  // promote type tags and substitute macros and constants:
  qual = true;
  expr rhs = csubst(macsubst(0, subst(vars, rsubst(x))));
  expr lhs = bind(vars, vi, lcsubst(pat));
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
  pure_expr *res = dodefn(vars, vi, lhs, rhs, e, compiling);
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

expr interpreter::pure_expr_to_expr(pure_expr *x, bool check)
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
      expr x = pure_expr_to_expr(tl, check);
      while (size > 0)
	x = expr::cons(pure_expr_to_expr(elems[--size], check), x);
      free(elems);
      return x;
    } else if (is_tuple(*this, x, size, elems)) {
      /* Optimize the tuple case. */
      expr x = pure_expr_to_expr(elems[--size], check);
      while (size > 0) {
	expr y = pure_expr_to_expr(elems[--size], check);
	x = expr::pair(y, x);
      }
      free(elems);
      return x;
    } else
      return expr(pure_expr_to_expr(x->data.x[0], check),
		  pure_expr_to_expr(x->data.x[1], check));
  }
  case EXPR::INT:
    return expr(EXPR::INT, x->data.i);
  case EXPR::BIGINT: {
    // The expr constructor clobbers its mpz_t argument, so take a copy.
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
      return wrap_expr(x, check);
  case EXPR::MATRIX: {
    if (x->data.mat.p) {
      gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
      exprll *xs = new exprll;
      for (size_t i = 0; i < m->size1; i++) {
	xs->push_back(exprl());
	exprl& ys = xs->back();
	for (size_t j = 0; j < m->size2; j++) {
	  ys.push_back(pure_expr_to_expr(m->data[i * m->tda + j], check));
	}
      }
      return expr(EXPR::MATRIX, xs);
    } else
      return expr(EXPR::MATRIX, new exprll);
  }
  case EXPR::DMATRIX: {
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
  }
  case EXPR::IMATRIX: {
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
  }
  case EXPR::CMATRIX: {
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
  }
  default:
    assert(x->tag >= 0);
    if (x->data.clos && (compiling || x->data.clos->local))
      /* A local closure isn't representable at compile time, so we wrap it up
	 in a global variable. We also do this for global closures when
	 batch-compiling. */
      return wrap_expr(x, check);
    else {
      assert(x->tag > 0);
      expr y = expr(x->tag);
#if 0
      symbol& sym = symtab.sym(x->tag);
      if (sym.s.find("::") != string::npos)
	y.flags() |= EXPR::QUAL;
#endif
      return y;
    }
  }
}

static uint32_t argidx(const path &p, size_t &i)
{
  size_t i0 = i;
  while (i < p.len() && p[i]) ++i;
  assert(i < p.len() && !p[i]);
  return (i++)-i0;
}

static expr subterm(expr x, const path& p)
{
  for (size_t i = 0, n = p.len(); i < n; ) {
    if (p.msk(i)) {
      // matrix path
      assert(x.tag() == EXPR::MATRIX);
      exprll *xs = x.xvals();
      uint32_t r = argidx(p, i), c = argidx(p, i);
      exprll::iterator it = xs->begin();
      while (r > 0) {
	assert(it != xs->end());
	++it; --r;
      }
      exprl::iterator jt = it->begin();
      while (c > 0) {
	assert(jt != it->end());
	++jt; --c;
      }
      x = *jt;
    } else {
      assert(x.tag() == EXPR::APP);
      x = p[i++]?x.xval2():x.xval1();
    }
  }
  return x;
}

static inline pure_expr *make_complex2(symbol& rect, double a, double b)
{
  return pure_appl(pure_symbol(rect.f), 2, pure_double(a), pure_double(b));
}

static inline pure_expr *make_complex(double a, double b)
{
  interpreter& interp = *interpreter::g_interp;
  symbol &rect = interp.symtab.complex_rect_sym();
  return make_complex2(rect, a, b);
}

static pure_expr *pure_subterm(pure_expr *x, const path& p)
{
  pure_expr *tmp = 0;
  for (size_t i = 0, n = p.len(); i < n; ) {
    if (p.msk(i)) {
      // matrix path
      uint32_t r = argidx(p, i), c = argidx(p, i);
      switch (x->tag) {
      case EXPR::MATRIX: {
	gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
	assert(r < m->size1 && c < m->size2);
	x = m->data[r * m->tda + c];
	break;
      }
      case EXPR::DMATRIX: {
	gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
	assert(r < m->size1 && c < m->size2);
	x = pure_double(m->data[r * m->tda + c]);
	break;
      }
      case EXPR::IMATRIX: {
	gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
	assert(r < m->size1 && c < m->size2);
	x = pure_int(m->data[r * m->tda + c]);
	break;
      }
      case EXPR::CMATRIX: {
	gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
	assert(r < m->size1 && c < m->size2);
	size_t k = 2*(r * m->tda + c);
	x = make_complex(m->data[k], m->data[k+1]);
	if (i < p.len()) tmp = x;
	break;
      }
      default:
	assert(0);
	break;
      }
    } else {
      assert(x->tag == EXPR::APP);
      x = x->data.x[p[i++]?1:0];
    }
  }
  if (tmp) {
    pure_new(x); pure_freenew(tmp); pure_unref(x);
  }
  return x;
}

static bool is_scalar(expr x)
{
  switch (x.tag()) {
  // constants:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR: // should PTR and WRAP really be treated as scalars?
  case EXPR::WRAP:
    return true;
  default:
    return false;
  }
}

pure_expr *interpreter::const_defn(expr pat, expr& x, pure_expr*& e)
{
  using namespace llvm;
  globals g;
  save_globals(g);
  compile();
  env vars; vinfo vi;
  // promote type tags and substitute macros and constants:
  qual = true;
  expr rhs = csubst(macsubst(0, subst(vars, rsubst(x))));
  expr lhs = bind(vars, vi, lcsubst(pat));
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
  nwrapped = 0;
  pure_expr *res = doeval(rhs, e, false);
  if (!res) return 0;
  // convert the result back to a compile time expression
  expr u = pure_expr_to_expr(res, compiling);
  // match against the left-hand side
  matcher m(rule(lhs, rhs));
  if (m.match(u)) {
    // verify type guards
    for (vguardl::const_iterator it = vi.guards.begin();
	 it != vi.guards.end(); ++it) {
      pure_expr *x = pure_subterm(res, it->p);
      bool rc = pure_safe_typecheck(it->ttag, x);
      if (x != res) pure_freenew(x);
      if (!rc) goto nomatch;
    }
    // verify non-linearities
    for (veqnl::const_iterator it = vi.eqns.begin();
	 it != vi.eqns.end(); ++it) {
      pure_expr *x = pure_subterm(res, it->p), *y = pure_subterm(res, it->q);
      bool rc = same(x, y);
      pure_new(x); pure_new(y);
      pure_free(x); pure_free(y);
      if (!rc) goto nomatch;
    }
    // bind variables accordingly
    for (env::const_iterator it = vars.begin(); it != vars.end(); ++it) {
      assert(it->second.t == env_info::lvar && it->second.p);
      int32_t f = it->first;
      expr v = subterm(u, *it->second.p);
      globenv[f] = env_info(v, temp);
      if (!::is_scalar(v) || (compiling && (nwrapped||!consts))) {
	/* As of Pure 0.38, we only inline scalar constants. Aggregate values
	   are cached in a read-only variable for better efficiency. As of
	   Pure 0.44, we actually do this for all variables bound in the
	   definition, if we're batch compiling and the result contains
	   wrapped runtime data, or if the programmer specified the --noconst
	   option. (This case is then handled further below.) */
	symbol& sym = symtab.sym(f);
	pure_expr *x = pure_subterm(res, *it->second.p);
	/* Create a new entry in the globalvars table. */
	map<int32_t,GlobalVar>::iterator it = globalvars.find(f);
	GlobalVariable *oldv = 0;
	if (it != globalvars.end()) {
	  /* We already have a global for this symbol. Make sure that the
	     binding persists after removing the globalvars entry, since
	     previous definitions may still depend on it. */
	  oldv = it->second.v;
	  pure_expr **x = new pure_expr*;
	  *x = it->second.x;
	  globalvars.erase(it);
	  // Erasing the binding from globalvars doesn't actually affect the
	  // LLVM mapping, so there's no need to reinstate it.
	  //JIT->addGlobalMapping(oldv, x);
	}
	pure_expr **xp = new pure_expr*; *xp = 0;
	globalvars.insert(pair<int32_t,GlobalVar>(f, GlobalVar(xp)));
	it = globalvars.find(f);
	assert(it != globalvars.end());
	GlobalVar& gv = it->second;
	gv.x = pure_new(x);
	gv.v = global_variable
	  (module, ExprPtrTy, false, GlobalVariable::InternalLinkage,
	   ConstantPointerNull::get(ExprPtrTy), "$$const."+sym.s);
	JIT->addGlobalMapping(gv.v, &gv.x);
	/* Also record the value in the globenv entry, so that the frontend
	   knows that the value of this constant has been cached. */
	globenv[f].cval_var = gv.x;
	/* In batch compilation we need to record the shadowed variable (which
	   might still be referred to in earlier definitions). */
	if (compiling) globenv[f].cval_v = oldv;
	if (compiling && !(nwrapped||!consts)) {
	  /* In batch-compiled scripts we also need to generate some
	     initialization code for the constant value. (But note that the
	     case of wrapped runtime data or --noconst, which needs a full
	     initialization at runtime, is handled below.) */
	  Env *save_fptr = fptr;
	  fptr = new Env(0, 0, 0, rhs, false); fptr->refc = 1;
	  Env &e = *fptr;
	  push("const_defn", &e);
	  fun_prolog("$$init");
	  /* Generate code for the rhs. Note that we generate a quoted
	     (literal) value here, because we don't want the value (which
	     presumably is in weak normal form already, but might contain
	     quoted subterms) to be reevaluated when constructing it. */
	  Value *x = codegen(v, true);
	  // store the value in the global variable
	  call("pure_new", x);
	  e.builder.CreateStore(x, gv.v);
	  // return the value to indicate success
	  e.builder.CreateRet(x);
	  fun_finish();
	  pop(&e);
	  fptr = save_fptr;
	}
      }
    }
    if (compiling && (nwrapped||!consts)) {
      /* To handle consts with --noconst or wrapped runtime objects in a batch
	 compilation, we need to generate the appropriate runtime
	 initialization code. This is done here in the same fashion as for an
	 ordinary variable (cf. dodefn). */
      Env *save_fptr = fptr;
      fptr = new Env(0, 0, 0, rhs, false); fptr->refc = 1;
      Env &f = *fptr;
      push("const_defn", &f);
      fun_prolog("$$init");
      // compute the matchee
      Value *arg = codegen(rhs);
      // emit the matching code
      BasicBlock *matchedbb = basic_block("matched");
      BasicBlock *failedbb = basic_block("failed");
      matcher m(rule(lhs, rhs));
      state *start = m.start;
      simple_match(arg, start, matchedbb, failedbb);
      // matched => emit code for binding the variables
      f.f->getBasicBlockList().push_back(matchedbb);
      f.builder.SetInsertPoint(matchedbb);
      if (!vi.guards.empty()) {
	// verify guards
	for (vguardl::const_iterator it = vi.guards.begin(),
	       end = vi.guards.end(); it != end; ++it) {
	  BasicBlock *checkedbb = basic_block("typechecked");
	  vector<Value*> args(2);
	  args[0] = ConstantInt::get(interpreter::int32_type(),
				     (uint64_t)it->ttag, true);
	  args[1] = vref(arg, it->p);
	  Value *check =
	    f.builder.CreateCall(module->getFunction("pure_safe_typecheck"),
				 mkargs(args));
	  f.builder.CreateCondBr(check, checkedbb, failedbb);
	  f.f->getBasicBlockList().push_back(checkedbb);
	  f.builder.SetInsertPoint(checkedbb);
	}
      }
      if (!vi.eqns.empty()) {
	// check non-linearities
	for (veqnl::const_iterator it = vi.eqns.begin(), end = vi.eqns.end();
	     it != end; ++it) {
	  BasicBlock *checkedbb = basic_block("checked");
	  vector<Value*> args(2);
	  args[0] = vref(arg, it->p);
	  args[1] = vref(arg, it->q);
	  Value *check = f.builder.CreateCall(module->getFunction("same"),
					      mkargs(args));
	  f.builder.CreateCondBr(check, checkedbb, failedbb);
	  f.f->getBasicBlockList().push_back(checkedbb);
	  f.builder.SetInsertPoint(checkedbb);
	}
      }
      for (env::const_iterator it = vars.begin(); it != vars.end(); ++it) {
	int32_t tag = it->first;
	const env_info& info = it->second;
	assert(info.t == env_info::lvar && info.p);
	// walk the arg value to find the subterm at info.p
	path& p = *info.p;
	Value *x = vref(arg, p);
	/* Store the value in a global variable. In difference to dodefn,
	   these variables will always be new and uninitialized, since we just
	   created them in the compile time binding code above. */
	GlobalVar& v = globalvars[tag];
	assert(v.v);
	call("pure_new", x);
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
      fptr = save_fptr;
    }
  } else {
  nomatch:
    pure_freenew(res);
    res = 0;
  }
  restore_globals(g);
  return res;
}

void interpreter::const_defn(const char *varname, pure_expr *x)
{
  symbol& sym = symtab.checksym(varname);
  sym.unresolved = false;
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
  expr u = pure_expr_to_expr(x, compiling);
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
    // also mark all interfaces as dirty which are associated with this
    // function
    map< int32_t, set<int32_t> >::iterator it = fun_types.find(f);
    if (it != fun_types.end())
      for (set<int32_t>::iterator jt = it->second.begin();
	   jt != it->second.end(); ++jt)
	mark_dirty_type(*jt);
  }
}

void interpreter::mark_dirty_type(int32_t f)
{
  env::iterator e = typeenv.find(f);
  if (e != typeenv.end() && e->second.t != env_info::none) {
    // mark this closure for recompilation
    env_info& info = e->second;
    if (info.m) {
      delete info.m; info.m = 0;
    }
    if (info.rxs) {
      delete info.rxs; info.rxs = 0;
    }
    if (info.mxs) {
      delete info.mxs; info.mxs = 0;
    }
    dirty_types.insert(f);
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
  if (!dirty.empty() || !dirty_types.empty()) {
#if DEBUG>1
    // Check for recursive invocations. This is always bad.
    static bool recursive = false;
    assert(!recursive);
    recursive = true;
#endif
    // there are some fundefs in the global environment waiting to be
    // recompiled, do it now
    set<int> to_be_jited;
    list<pure_expr*> to_be_freed;
    // Do the type predicates first, as the functions may need them.
    for (funset::const_iterator f = dirty_types.begin();
	 f != dirty_types.end(); f++) {
      env::iterator e = typeenv.find(*f);
      if (e != typeenv.end() && e->second.t != env_info::none) {
	int32_t ftag = e->first;
	env_info& info = e->second;
	if (!info.rules->empty())
	  info.m = new matcher(*info.rules, info.argc+1);
	if ((info.rxs = compile_interface(typeenv, ftag)))
	  info.mxs = new matcher(*info.rxs, info.argc+1);
	assert(!info.rxs || !info.rxs->empty());
	assert((!info.rxs && !info.mxs) ||
	       (info.rxs && info.mxs && info.argc==1));
	if (verbose&verbosity::code) {
	  const symbol& sym = symtab.sym(ftag);
	  if (info.mxs && !info.rxs->empty())
	    std::cout << "interface " << sym.s << " " << *info.mxs << '\n';
	  if (info.m && !info.rules->empty())
	    std::cout << "type " << sym.s << " " << *info.m << '\n';
	}
	if (!info.m && !info.mxs) {
	  // If we still have an LLVM function for the type, get rid of it now.
	  map<int32_t,Env>::iterator g = globaltypes.find(ftag);
	  if (g != globaltypes.end()) {
	    llvm::Function *f = g->second.f, *h = g->second.h;
	    assert(f && h);
	    globaltypes.erase(g);
	    if (h != f) h->dropAllReferences();
	    f->dropAllReferences();
	    if (h != f) h->eraseFromParent();
	    f->eraseFromParent();
	  }
	  // Reset the runtime type information.
	  pure_add_rtty(ftag, 0, 0);
	  // no rules and no interface; skip
	  continue;
	}
	// regenerate LLVM code (prolog)
	Env& f = globaltypes[ftag] = Env(ftag, info, false, false);
#if DEBUG>1
	print_map(std::cerr, &f);
#endif
	push("compile", &f);
	globaltypes[ftag].f = fun_prolog("$$type."+symtab.sym(ftag).s);
	pop(&f);
      }
    }
    for (funset::const_iterator f = dirty_types.begin();
	 f != dirty_types.end(); f++) {
      env::iterator e = typeenv.find(*f);
      if (e != typeenv.end() && e->second.t != env_info::none) {
	int32_t ftag = e->first;
	env_info& info = e->second;
	if (!info.m && !info.mxs) continue; // no rules and no interface; skip
	// regenerate LLVM code (body)
	Env& f = globaltypes[ftag];
	push("compile", &f);
	fun_body(info.m, info.mxs);
	pop(&f);
	// Always run the JIT on these right away and set up the runtime type
	// information. These functions are called indirectly through the
	// runtime, and we don't know when or where that will be.
	if (f.f != f.h) JIT->getPointerToFunction(f.f);
	void *fp = JIT->getPointerToFunction(f.h);
	pure_add_rtty(ftag, f.n, fp);
#if DEBUG>1
	std::cerr << "JIT " << f.f->getName().str() << " -> " << fp << '\n';
#endif
      }
    }
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
	fun_body(info.m, 0, set_defined_sym(ftag));
	pop(&f);
	if (eager.find(ftag) != eager.end())
	  to_be_jited.insert(ftag);
#if DEFER_GLOBALS
	// defer JIT until the function is called somewhere
	void *fp = 0;
#else
	// run the JIT now (always use the C-callable stub here)
	if (f.f != f.h) JIT->getPointerToFunction(f.f);
	void *fp = JIT->getPointerToFunction(f.h);
#if DEBUG>1
	std::cerr << "JIT " << f.f->getName().str() << " -> " << fp << '\n';
#endif
#endif
	// do a direct call to the runtime to create the fbox and cache it in
	// a global variable
	pure_expr *fv = pure_clos(false, f.tag, f.getkey(), f.n, fp, 0, 0);
	GlobalVar& v = globalvars[f.tag];
	if (!v.v) {
	  v.v = global_variable
	    (module, ExprPtrTy, false, GlobalVariable::InternalLinkage,
	     ConstantPointerNull::get(ExprPtrTy),
	     mkvarlabel(f.tag));
	  JIT->addGlobalMapping(v.v, &v.x);
	}
	/* It's not safe to free any old value v.x right here, as it might
	   have a sentry to execute which in turn might cause the compiler to
	   be invoked recursively. So we defer this until we're finished. */
	if (v.x) to_be_freed.push_back(v.x); v.x = pure_new(fv);
#if DEBUG>1
	std::cerr << "global " << &v.x << " (== "
		  << JIT->getPointerToGlobal(v.v) << ") -> "
		  << (void*)fv << '\n';
#endif
      }
    }
    if (!to_be_jited.empty())
      // eager compilation
      jit_now(to_be_jited, true);
    dirty.clear();
    dirty_types.clear();
    clear_cache();
#if DEBUG>1
    recursive = false;
#endif
    for (list<pure_expr*>::iterator it = to_be_freed.begin();
	 it != to_be_freed.end(); ++it)
      pure_free(*it);
  }
}

void interpreter::jit_now(const set<int> fnos, bool recurse)
{
  if (!recurse) {
    bool done = fnos.empty();
    for (set<int>::const_iterator it = fnos.begin(), end = fnos.end();
	 !done && it != end; ++it) {
      int fno = *it;
      done = dirty.find(fno) != dirty.end() && eager.find(fno) != eager.end();
    }
    compile();
    if (done) return;
  }
  if (fnos.empty()) {
    // Force compilation of everything. This is slow.
    for (llvm::Module::iterator it = module->begin(), end = module->end();
	 it != end; ++it) {
      llvm::Function *fn = &*it;
      JIT->getPointerToFunction(fn);
    }
  } else {
    /* TODO: We should cache the results of analyzing the call graph and reuse
       these results as much as possible. But the LLVM JIT seems to take the
       lion share of the compilation time anyway, so this isn't really worth
       the effort until the JIT gets much faster. */
    set<llvm::Function*> used;
    map<llvm::GlobalVariable*,llvm::Function*> varmap;
    for (set<int>::const_iterator it = fnos.begin(), end = fnos.end();
	 it != end; ++it) {
      int fno = *it;
      map<int32_t,Env>::iterator g = globalfuns.find(fno);
      if (g != globalfuns.end()) {
	llvm::Function *f = g->second.f, *h = g->second.h;
	if (f) used.insert(f);
	if (h) used.insert(h);
      }
    }
    check_used(used, varmap);
    // Force compilation of the given functions and everything they might use.
    for (llvm::Module::iterator it = module->begin(), end = module->end();
	 it != end; ++it) {
      llvm::Function *fn = &*it;
      if (used.find(fn) != used.end())
	JIT->getPointerToFunction(fn);
    }
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

exprl get_args(expr x, int32_t& f)
{
  expr y, z;
  exprl xs;
  while (x.is_app(y, z)) xs.push_front(z), x = y;
  f = x.tag();
  return xs;
}

exprl get_args(expr x, expr& f)
{
  expr y, z;
  exprl xs;
  while (x.is_app(y, z)) xs.push_front(z), x = y;
  f = x;
  return xs;
}

int32_t headsym(expr x)
{
  int32_t f;
  count_args(x, f);
  return f;
}

// build a local variable environment from an already processed pattern

void interpreter::build_env(env& vars, expr x)
{
  assert(!x.is_null());
  if (x.astag() > 0) {
    const symbol& sym = symtab.sym(x.astag());
    if (sym.f != symtab.anon_sym) vars[sym.f] = env_info(0, x.aspath());
  }
  switch (x.tag()) {
  case EXPR::VAR: {
    const symbol& sym = symtab.sym(x.vtag());
    if (sym.f != symtab.anon_sym) vars[sym.f] = env_info(x.ttag(), x.vpath());
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
    m = new matcher(x.lrule(), x.largs()->size()+1);
    compile(x.lrule().rhs);
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

void interpreter::using_namespaces(list< pair< string, list<int32_t> > > *items)
{
  symtab.search_namespaces->clear();
  if (items) {
    for (list< pair< string, list<int32_t> > >::iterator
	   it = items->begin(), end = items->end(); it != end; it++) {
      if (!it->first.empty()) namespaces.insert(it->first);
      map< string, set<int32_t> >::iterator jt =
	symtab.search_namespaces->find(it->first);
      if (jt == symtab.search_namespaces->end()) {
	(*symtab.search_namespaces)[it->first].clear();
	jt = symtab.search_namespaces->find(it->first);
      }
      assert(jt != symtab.search_namespaces->end());
      for (list<int32_t>::iterator kt = it->second.begin();
	   kt != it->second.end(); ++kt) {
	const symbol& sym = symtab.sym(*kt);
	if (sym.fix == outfix) {
	  if (sym.g == 0)
	    throw err("left symbol missing for right outfix symbol '"+
		      sym.s+"'");
	  list<int32_t>::iterator lt = ++kt;
	  if (lt == it->second.end())
	    throw err("right symbol missing for left outfix symbol '"+
		      sym.s+"'");
	  const symbol& sym2 = symtab.sym(*lt);
	  if (sym.g != sym2.f)
	    throw err("right symbol '"+
		      sym2.s+"' doesn't match left outfix symbol '"+sym.s+"'");
	  jt->second.insert(sym.f);
	  jt->second.insert(sym.g);
	} else {
	  jt->second.insert(sym.f);
	}
      }
    }
    delete items;
  }
}

void interpreter::declare(bool priv, prec_t prec, fix_t fix, list<string> *ids)
{
  for (list<string>::const_iterator it = ids->begin();
       it != ids->end(); ++it) {
    string id0 = *it;
    size_t k = symsplit(id0);
    if (k != string::npos) {
      string qual = id0.substr(0, k), id = id0.substr(k+2);
      if (qual.compare(0, 2, "::") == 0) qual.erase(0, 2);
      if (qual == *symtab.current_namespace)
	id0 = id;
      else
	throw err("qualified symbol '"+id0+"' has wrong namespace");
    }
    string id = make_qualid(id0), absid = make_absid(id0);
    symbol* sym = symtab.lookup(absid);
    if (sym) {
      // Make sure that the symbol is marked as resolved.
      sym->unresolved = false;
      // crosscheck declarations
      if (sym->priv != priv) {
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
	  throw err("symbol '"+id+"' already declared with different fixity");
	}
      } else if (fix == outfix) {
	list<string>::const_iterator jt = ++it;
	if (jt == ids->end()) {
	  throw err("right symbol missing in outfix declaration");
	}
	string id0 = *it;
	size_t k = symsplit(id0);
	if (k != string::npos) {
	  string qual = id0.substr(0, k), id = id0.substr(k+2);
	  if (qual.compare(0, 2, "::") == 0) qual.erase(0, 2);
	  if (qual == *symtab.current_namespace)
	    id0 = id;
	  else
	    throw err("qualified symbol '"+id0+"' has wrong namespace");
	}
	string id2 = make_qualid(id0), absid2 = make_absid(id0);
	symbol* sym2 = symtab.lookup(absid2);
	if (!sym2 || sym->g != sym2->f) {
	  throw err("right outfix symbol '"+id2+"' doesn't match existing declaration");
	}
	it = jt;
      }
    } else if (fix == outfix) {
      // determine the matching right symbol
      list<string>::const_iterator jt = ++it;
      if (jt == ids->end()) {
	throw err("right symbol missing in outfix declaration");
      }
      string id0 = *it;
      size_t k = symsplit(id0);
      if (k != string::npos) {
	string qual = id0.substr(0, k), id = id0.substr(k+2);
	if (qual.compare(0, 2, "::") == 0) qual.erase(0, 2);
	if (qual == *symtab.current_namespace)
	  id0 = id;
	else
	  throw err("qualified symbol '"+id0+"' has wrong namespace");
      }
      string id2 = make_qualid(id0), absid2 = make_absid(id0);
      symbol* sym2 = symtab.lookup(absid2);
      if (sym2) {
	if (sym2->g != 0)
	  throw err("symbol '"+id2+"' already declared with different fixity");
	else
	  throw err("left outfix symbol '"+id+"' doesn't match existing declaration");
      } else if (absid == absid2)
	throw err("left and right symbol in outfix declaration must be distinct");
      sym = symtab.sym(absid, prec, fix, priv);
      sym2 = symtab.sym(absid2, prec, fix, priv);
      assert(sym && sym2);
      sym->g = sym2->f;
      it = jt;
    } else
      symtab.sym(absid, prec, fix, priv);
  }
}

void interpreter::exec(expr *x)
{
  last.clear(); checkfuns(*x);
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
  } else
    bt.clear();
  result = pure_new(res);
  delete x;
  if (interactive) {
    if (lastres) pure_free(lastres);
    lastres = pure_new(result);
    static bool more_init = false;
    static const char *more;
    if (!more_init) {
      more = getenv("PURE_LESS");
      more_init = true;
    }
    /* If PURE_LESS is set then we pipe the printed expression through it. */
    FILE *fp;
    if (!texmacs && more && *more && isatty(fileno(stdin)) &&
	(fp = popen(more, "w"))) {
      ostringstream sout;
      sout << result << '\n';
      fputs(sout.str().c_str(), fp);
      pclose(fp);
    } else {
      if (texmacs) {
	/* Temporarily replace __show__ with __texmacs__ if it exists, to make
	   it possible to define a custom unparsing for TeXmacs. */
	const symbol *sym = symtab.lookup("::__texmacs__");
	int32_t f = sym?sym->f:0, g = symtab.__show__sym;
	map<int32_t,GlobalVar>::iterator it;
	if (f > 0 &&
	    !((it = globalvars.find(f)) != globalvars.end() &&
	      it->second.x && it->second.x->tag >= 0 &&
	      it->second.x->data.clos))
	  f = 0;
	if (f > 0) symtab.__show__sym = f;
	string s;
	{
	  ostringstream out;
	  out << result;
	  s = out.str();
	}
	// Excessive amounts of output make TeXmacs barf, so cut it down to
	// some reasonable size.
	size_t maxlen = 10000; // this should be plenty
	if (s.length() > maxlen) {
	  s.erase(maxlen-3);
	  s += "...";
	}
	cout << TEXMACS_BEGIN << s << '\n' << TEXMACS_END;
	if (f > 0) symtab.__show__sym = g;
      } else
	cout << result << '\n';
    }
    report_stats();
  }
}

void interpreter::parse(expr *x)
{
  last.clear(); checkfuns(*x);
  if (result) pure_free(result); result = 0;
  pure_expr *res = const_value(rsubst(*x), true);
  if (!res) throw err("syntax error");
  result = pure_new(res);
  delete x;
  if (interactive) {
    if (lastres) pure_free(lastres);
    lastres = pure_new(result);
    cout << result << '\n';
  }
}

void interpreter::define(rule *r)
{
  last.clear();
  checkvars(r->lhs); checkfuns(r->rhs);
  if (nerrs > 0) {
    delete r; return;
  } else if (tags) {
    add_tags(r->lhs); delete r; return;
  }
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
  } else
    bt.clear();
  delete r;
  pure_freenew(res);
  report_stats();
}

void interpreter::define_const(rule *r)
{
  last.clear();
  checkvars(r->lhs); checkfuns(r->rhs);
  if (nerrs > 0) {
    delete r; return;
  } else if (tags) {
    add_tags(r->lhs); delete r; return;
  }
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
  } else
    bt.clear();
  delete r;
  pure_freenew(res);
  report_stats();
}

static string mkvarsym(const string& name);

void interpreter::clearsym(int32_t f)
{
  using namespace llvm;
  // Check whether this symbol was already compiled; in that case
  // patch up the global variable table to replace it with a cbox.
  map<int32_t,GlobalVar>::iterator v = globalvars.find(f);
  if (v != globalvars.end()) {
    env::iterator it = globenv.find(f);
    if (it != globenv.end() &&
	it->second.t == env_info::cvar && it->second.cval_var) {
      /* This is a constant value cached in a read-only variable. We need to
	 keep that variable, so create a new one in its place. */
      globalvars.erase(v);
      pure_expr **xp = new pure_expr*; *xp = 0;
      globalvars.insert(pair<int32_t,GlobalVar>(f, GlobalVar(xp)));
      v = globalvars.find(f);
      assert(v != globalvars.end());
      symbol& sym = symtab.sym(f);
      v->second.v = global_variable
	(module, ExprPtrTy, false, GlobalVariable::InternalLinkage,
	 ConstantPointerNull::get(ExprPtrTy), mkvarsym(sym.s));
      JIT->addGlobalMapping(v->second.v, &v->second.x);
    }
    /* Check whether this is actually an external which has the --defined
       pragma. In this case the cbox is reset to NULL so that the wrapper
       function knows that we want an exception rather than a normal form. */
    bool defined_external = externals.find(f) != externals.end() &&
      set_defined_sym(f);
    pure_expr *cv = defined_external? 0 : pure_new(pure_const(f));
    if (v->second.x) pure_free(v->second.x);
    v->second.x = cv;
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
  // Update dependent interface types.
  map< int32_t, set<int32_t> >::iterator it = fun_types.find(f);
  if (it != fun_types.end())
    for (set<int32_t>::iterator jt = it->second.begin();
	 jt != it->second.end(); ++jt)
      mark_dirty_type(*jt);
}

void interpreter::cleartypesym(int32_t f)
{
  using namespace llvm;
  map<int32_t,Env>::iterator g = globaltypes.find(f);
  if (g != globaltypes.end()) {
    llvm::Function *f = g->second.f, *h = g->second.h;
    assert(f && h);
    globaltypes.erase(g);
    if (h != f) h->dropAllReferences();
    f->dropAllReferences();
    if (h != f) h->eraseFromParent();
    f->eraseFromParent();
  }
  // Reset the runtime type information.
  pure_add_rtty(f, 0, 0);
  // Update the fun_types table.
  env::iterator it = typeenv.find(f);
  if (it == typeenv.end()) return;
  env_info &info = it->second;
  if (!info.xs) return;
  for (exprl::iterator it = info.xs->begin(); it != info.xs->end(); ++it) {
    expr fx; count_args(*it, fx);
    int32_t g = fx.tag();
    assert(g>0);
    fun_types[g].erase(f);
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
      clearsym(f);
      globenv.erase(it);
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
	clearsym(f);
	globenv.erase(jt);
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
    for (env::iterator it = typeenv.begin(); it != typeenv.end(); ) {
      env::iterator jt = it; ++it;
      int32_t f = jt->first;
      env_info& info = jt->second;
      if (info.t == env_info::none) {
	typeenv.erase(jt);
	continue;
      }
      if (info.temp >= temp) {
	cleartypesym(f);
	typeenv.erase(jt);
      } else {
	// purge temporary rules for non-temporary types
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
	  mark_dirty_type(f);
	  if (info.m) {
	    delete info.m;
	    info.m = 0;
	  }
	  if (info.rxs) {
	    delete info.rxs;
	    info.rxs = 0;
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

void interpreter::clear_type(int32_t f)
{
  assert(f > 0);
  env::iterator it = typeenv.find(f);
  if (it != typeenv.end()) {
    if (it->second.t != env_info::none) cleartypesym(f);
    typeenv.erase(it);
  }
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

void interpreter::clear_type_rules(int32_t f, uint32_t level)
{
  assert(f > 0);
  env::iterator it = typeenv.find(f);
  if (it != typeenv.end() && it->second.t != env_info::none) {
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
      mark_dirty_type(f);
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

void interpreter::add_rules(env &e, rulel *r, bool headless, bool toplevel)
{
  for (rulel::iterator ri = r->begin(), end = r->end(); ri != end; ri++)
    add_rule(e, *ri, toplevel);
  if (tags && toplevel && !headless) add_tags(r);
  delete r;
}

void interpreter::add_rule(rulel &rl, rule &r, bool b)
{
  assert(!r.lhs.is_null());
  closure(r, b);
  rl.push_back(r);
}

void interpreter::add_rule(env &e, rule &r, bool toplevel, bool check)
{
  assert(!r.lhs.is_null());
  if (check || !toplevel) {
    closure(r, false);
    if (toplevel) {
      // substitute macros and constants:
      checkfuns(true, &r); if (nerrs > 0) return;
      int32_t h = headsym(r.lhs);
      expr u = expr(r.lhs),
	v = expr(csubst(macsubst(h, r.rhs))),
	w = expr(csubst(macsubst(h, r.qual)));
      r = rule(u, v, r.vi, w);
      compile(r.rhs);
      compile(r.qual);
    }
  } else {
    env vars; vinfo vi;
    int32_t h = headsym(r.lhs);
    expr u = expr(bind(vars, vi, lcsubst(r.lhs), false)),
      v = expr(csubst(subst(vars, macsubst(h, rsubst(r.rhs))))),
      w = expr(csubst(subst(vars, macsubst(h, rsubst(r.qual)))));
    r = rule(u, v, vi, w);
    compile(r.rhs);
    compile(r.qual);
  }
  expr fx; uint32_t argc = count_args(r.lhs, fx);
  int32_t f = fx.tag();
  if (f <= 0)
    throw err("error in function definition (missing head symbol)");
  else if (!toplevel && (fx.flags()&EXPR::QUAL))
    throw err("error in local function definition (qualified head symbol)");
  fx.flags() |= toplevel?EXPR::GLOBAL:EXPR::LOCAL;
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

void interpreter::add_rule_at(env &e, rule &r, int32_t g,
			      rulel::iterator& p)
{
  assert(!r.lhs.is_null());
  env vars; vinfo vi;
  int32_t h = headsym(r.lhs);
  expr u = expr(bind(vars, vi, lcsubst(r.lhs), false)),
    v = expr(csubst(subst(vars, macsubst(h, rsubst(r.rhs))))),
    w = expr(csubst(subst(vars, macsubst(h, rsubst(r.qual)))));
  r = rule(u, v, vi, w);
  compile(r.rhs);
  compile(r.qual);
  expr fx; uint32_t argc = count_args(r.lhs, fx);
  int32_t f = fx.tag();
  if (f <= 0)
    throw err("error in function definition (missing head symbol)");
  else if (f != g)
    throw err("error in function definition (wrong head symbol)");
  fx.flags() |= EXPR::GLOBAL;
  env::iterator it = e.find(f);
  const symbol& sym = symtab.sym(f);
  if (it == e.end())
    throw err("error in function definition (no existing rule)");
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
  env_info &info = e[f];
  assert(info.t == env_info::fun);
  assert(info.argc == argc);
  r.temp = temp;
  p = info.rules->insert(p, r); p++;
  if ((verbose&verbosity::defs) != 0) cout << r << ";\n";
  mark_dirty(f);
}

void interpreter::add_type_rules(env &e, rulel *r)
{
  for (rulel::iterator ri = r->begin(), end = r->end(); ri != end; ri++)
    add_type_rule(e, *ri);
  if (tags) add_tags(r);
  delete r;
}

void interpreter::add_type_rule(env &e, rule &r, bool check)
{
  assert(!r.lhs.is_null());
  if (check) {
    closure(r, false);
    // substitute macros and constants:
    checkfuns(false, &r); if (nerrs > 0) return;
    int32_t h = headsym(r.lhs);
    expr u = expr(r.lhs),
      v = expr(csubst(macsubst(h, r.rhs))),
      w = expr(csubst(macsubst(h, r.qual)));
    r = rule(u, v, r.vi, w);
    compile(r.rhs);
    compile(r.qual);
  } else {
    env vars; vinfo vi;
    int32_t h = headsym(r.lhs);
    expr u = expr(bind(vars, vi, lcsubst(r.lhs), false)),
      v = expr(csubst(subst(vars, macsubst(h, rsubst(r.rhs))))),
      w = expr(csubst(subst(vars, macsubst(h, rsubst(r.qual)))));
    r = rule(u, v, vi, w);
    compile(r.rhs);
    compile(r.qual);
  }
  expr fx; uint32_t argc = count_args(r.lhs, fx);
  int32_t f = fx.tag();
  if (f <= 0)
    throw err("error in type definition (missing head symbol)");
  else if (argc > 1)
    throw err("error in type definition (too many arguments)");
  fx.flags() |= EXPR::GLOBAL;
  env::iterator it = e.find(f);
  const symbol& sym = symtab.sym(f);
  if (it != e.end() && it->second.t != env_info::none) {
    if (it->second.argc != argc) {
      ostringstream msg;
      msg << "type predicate '" << sym.s
	  << "' was previously defined with " << it->second.argc << " args";
      throw err(msg.str());
    }
  }
  env_info &info = e[f];
  if (info.t == env_info::none)
    info = env_info(argc, rulel(), temp);
  assert(info.argc == argc);
  r.temp = temp;
  if (override) {
    rulel::iterator p = info.rules->begin();
    for (; p != info.rules->end() && p->temp >= temp; p++) ;
    info.rules->insert(p, r);
  } else
    info.rules->push_back(r);
  if ((verbose&verbosity::defs) != 0) {
    int32_t i;
    if (r.lhs.is_app() && r.rhs.is_int(i) && i==1) {
      cout << "type ";
      printx(cout, r.lhs, true);
      cout << ";\n";
    } else
      cout << "type " << r << ";\n";
  }
  mark_dirty_type(f);
}

void interpreter::add_type_rule_at(env &e, rule &r, int32_t g,
				   rulel::iterator& p)
{
  assert(!r.lhs.is_null());
  env vars; vinfo vi;
  int32_t h = headsym(r.lhs);
  expr u = expr(bind(vars, vi, lcsubst(r.lhs), false)),
    v = expr(csubst(subst(vars, macsubst(h, rsubst(r.rhs))))),
    w = expr(csubst(subst(vars, macsubst(h, rsubst(r.qual)))));
  r = rule(u, v, vi, w);
  compile(r.rhs);
  compile(r.qual);
  expr fx; uint32_t argc = count_args(r.lhs, fx);
  int32_t f = fx.tag();
  if (f <= 0)
    throw err("error in type definition (missing head symbol)");
  else if (f != g)
    throw err("error in type definition (wrong head symbol)");
  else if (argc > 1)
    throw err("error in type definition (too many arguments)");
  fx.flags() |= EXPR::GLOBAL;
  env::iterator it = e.find(f);
  const symbol& sym = symtab.sym(f);
  if (it == e.end() || it->second.t == env_info::none)
    throw err("error in type definition (no existing rule)");
  if (it->second.argc != argc) {
    ostringstream msg;
    msg << "type predicate '" << sym.s
	<< "' was previously defined with " << it->second.argc << " args";
    throw err(msg.str());
  }
  env_info &info = e[f];
  assert(info.t == env_info::fun);
  assert(info.argc == argc);
  r.temp = temp;
  p = info.rules->insert(p, r); p++;
  if ((verbose&verbosity::defs) != 0) {
    int32_t i;
    if (r.lhs.is_app() && r.rhs.is_int(i) && i==1) {
      cout << "type ";
      printx(cout, r.lhs, true);
      cout << ";\n";
    } else
      cout << "type " << r << ";\n";
  }
  mark_dirty_type(f);
}

/* Quick and dirty checks to see whether two compile time expressions are
   equal or equivalent (i.e. equal up to the renaming of variables). NOTE:
   This only works with simple expressions, which is all we really need
   here. If x or y contains any special constructs (with, when, etc.), we err
   on the safe side, declaring the expressions to be non-equivalent. */

static bool equal(map<int32_t, int32_t>& xttag, map<int32_t, int32_t>& yttag,
		  int32_t anon_tag, expr x, expr y)
{
  if (x == y) return true;
  if (x.tag() != y.tag()) return false;
  switch (x.tag()) {
  case EXPR::VAR: {
    // Handle the case of anonymous variables.
    int32_t xtag = (x.vtag() == anon_tag)?x.ttag():xttag[x.vtag()];
    int32_t ytag = (y.vtag() == anon_tag)?y.ttag():yttag[y.vtag()];
    return x.vtag() == y.vtag() && xtag == ytag;
  }
  // constants:
  case EXPR::FVAR:
    return x.vtag() == y.vtag();
  case EXPR::INT:
    return x.ival() == y.ival();
  case EXPR::BIGINT:
    return mpz_cmp(x.zval(), y.zval()) == 0;
  case EXPR::DBL:
    return x.dval() == y.dval();
  case EXPR::STR:
    return strcmp(x.sval(), y.sval()) == 0;
  // application:
  case EXPR::APP:
    return
      equal(xttag, yttag, anon_tag, x.xval1(), y.xval1()) &&
      equal(xttag, yttag, anon_tag, x.xval2(), y.xval2());
  // matrix (Pure 0.47+):
  case EXPR::MATRIX: {
    exprll *xs = x.xvals(), *ys = y.xvals();
    size_t nx = xs->size(), mx = xs->empty()?0:xs->front().size();
    size_t ny = ys->size(), my = ys->empty()?0:ys->front().size();
    if (nx != ny || mx != my) return false;
    for (exprll::const_iterator it = xs->begin(), jt = ys->begin();
	 it != xs->end(); ++it, ++jt) {
      assert(jt != ys->end());
      for (exprl::const_iterator it2 = it->begin(), jt2 = jt->begin();
	   it2 != it->end(); ++it2, ++jt2) {
	assert(jt2 != jt->end());
	if (!equal(xttag, yttag, anon_tag, *it2, *jt2))
	  return false;
      }
    }
    return true;
  }
  // these must not occur on the lhs:
  case EXPR::PTR:
  case EXPR::WRAP:
  case EXPR::LAMBDA:
  case EXPR::COND:
  case EXPR::CASE:
  case EXPR::WHEN:
  case EXPR::WITH:
    return false;
  default:
    assert(x.tag() > 0);
    return true;
  }
}

static bool equiv(map<int32_t, int32_t>& xvars, map<int32_t, int32_t>& yvars,
		  map<int32_t, int32_t>& xttag, map<int32_t, int32_t>& yttag,
		  int32_t anon_tag, expr x, expr y)
{
  if (x == y) return true;
  if (x.tag() != y.tag()) return false;
  switch (x.tag()) {
  case EXPR::VAR: {
    // Handle the case of anonymous variables.
    int32_t xtag = (x.vtag() == anon_tag)?x.ttag():xttag[x.vtag()];
    int32_t ytag = (y.vtag() == anon_tag)?y.ttag():yttag[y.vtag()];
    if (xtag != ytag) return false;
    if (x.vtag() != anon_tag) {
      map<int32_t, int32_t>::iterator it = xvars.find(x.vtag());
      if (it != xvars.end())
	return it->second == y.vtag();
      else {
	xvars[x.vtag()]= y.vtag();
	return true;
      }
    }
    if (y.vtag() != anon_tag) {
      map<int32_t, int32_t>::iterator it = yvars.find(y.vtag());
      if (it != yvars.end())
	return it->second == x.vtag();
      else {
	yvars[y.vtag()]= x.vtag();
	return true;
      }
    }
  }
  // constants:
  case EXPR::FVAR:
    return x.vtag() == y.vtag();
  case EXPR::INT:
    return x.ival() == y.ival();
  case EXPR::BIGINT:
    return mpz_cmp(x.zval(), y.zval()) == 0;
  case EXPR::DBL:
    return x.dval() == y.dval();
  case EXPR::STR:
    return strcmp(x.sval(), y.sval()) == 0;
  // application:
  case EXPR::APP:
    return
      equiv(xvars, yvars, xttag, yttag, anon_tag, x.xval1(), y.xval1()) &&
      equiv(xvars, yvars, xttag, yttag, anon_tag, x.xval2(), y.xval2());
  // matrix (Pure 0.47+):
  case EXPR::MATRIX: {
    exprll *xs = x.xvals(), *ys = y.xvals();
    size_t nx = xs->size(), mx = xs->empty()?0:xs->front().size();
    size_t ny = ys->size(), my = ys->empty()?0:ys->front().size();
    if (nx != ny || mx != my) return false;
    for (exprll::const_iterator it = xs->begin(), jt = ys->begin();
	 it != xs->end(); ++it, ++jt) {
      assert(jt != ys->end());
      for (exprl::const_iterator it2 = it->begin(), jt2 = jt->begin();
	   it2 != it->end(); ++it2, ++jt2) {
	assert(jt2 != jt->end());
	if (!equiv(xvars, yvars, xttag, yttag, anon_tag, *it2, *jt2))
	  return false;
      }
    }
    return true;
  }
  // these must not occur on the lhs:
  case EXPR::PTR:
  case EXPR::WRAP:
  case EXPR::LAMBDA:
  case EXPR::COND:
  case EXPR::CASE:
  case EXPR::WHEN:
  case EXPR::WITH:
    return false;
  default:
    assert(x.tag() > 0);
    return true;
  }
}

static void get_ttags(map<int32_t, int32_t>& ttag, int32_t anon_tag, expr x)
{
  switch (x.tag()) {
  case EXPR::VAR: {
    if (x.vtag() != anon_tag) {
      map<int32_t, int32_t>::iterator it = ttag.find(x.vtag());
      if (it == ttag.end() || it->second == 0)
	ttag[x.vtag()] = x.ttag();
    }
    break;
  }
  case EXPR::APP:
    get_ttags(ttag, anon_tag, x.xval1());
    get_ttags(ttag, anon_tag, x.xval2());
    break;
  case EXPR::MATRIX: {
    exprll *xs = x.xvals();
    for (exprll::const_iterator it = xs->begin(); it != xs->end(); ++it) {
      for (exprl::const_iterator jt = it->begin(); jt != it->end(); ++jt) {
	get_ttags(ttag, anon_tag, *jt);
      }
    }
    break;
  }
  default:
    break;
  }
}

static inline bool equal(int32_t anon_tag, expr x, expr y)
{
  map<int32_t, int32_t> xttag, yttag;
  get_ttags(xttag, anon_tag, x); get_ttags(yttag, anon_tag, y);
  return equal(xttag, yttag, anon_tag, x, y);
}

static inline bool equiv(int32_t anon_tag, expr x, expr y)
{
  map<int32_t, int32_t> xvars, yvars;
  map<int32_t, int32_t> xttag, yttag;
  get_ttags(xttag, anon_tag, x); get_ttags(yttag, anon_tag, y);
  return equiv(xvars, yvars, xttag, yttag, anon_tag, x, y);
}

/* Matches the subject term y against the pattern x, which can both be terms
   with variables. The xttag and yttag variables should be set to the type
   tags of named variables on entry. On exit, if the subject term matches,
   xvars is set to the matching substitution; this only holds the values for
   named variables. Also, xsubst is set to the list of terms bound to (both
   named or anonymous) variables of the interface type given by iface_tag. */

static bool match(map<int32_t, expr>& xvars, exprl& xsubst,
		  map<int32_t, int32_t>& xttag, map<int32_t, int32_t>& yttag,
		  int32_t anon_tag, int32_t iface_tag,
		  expr x, expr y)
{
  if (x == y) return true;
  if (x.tag() == EXPR::VAR) {
    int32_t xtag = -1, ytag = -1;
    xtag = (x.vtag() == anon_tag)?x.ttag():xttag[x.vtag()];
    if (y.tag() == EXPR::VAR)
      ytag = (y.vtag() == anon_tag)?y.ttag():yttag[y.vtag()];
    // The interface tag matches everything here, just as if the corresponding
    // variable was unqualified.
    if (xtag == iface_tag) xtag = 0;
    if (xtag) {
      // A proper type tag *must* be matched literally. It won't match any
      // non-variable term nor any different type tag.
      if (xtag != ytag) return false;
    }
    if (x.vtag() != anon_tag) {
      map<int32_t, expr>::iterator it = xvars.find(x.vtag());
      if (it != xvars.end()) {
	// Variable is already bound (this may happen if the term is
	// non-linear), check that the bindings match.
	if (!equal(anon_tag, it->second, y))
	  return false;
      } else {
	// Record a binding for this variable.
	xvars[x.vtag()]= y;
	if (xttag[x.vtag()] == iface_tag)
	  xsubst.push_back(y);
      }
    } else if (x.ttag() == iface_tag) {
      // Anonymous variable, simply record the binding.
      xsubst.push_back(y);
    }
    return true;
  }
  if (x.tag() != y.tag()) return false;
  switch (x.tag()) {
  // constants:
  case EXPR::FVAR:
    if (x.vtag() != y.vtag()) return false;
    break;
  case EXPR::INT:
    if (x.ival() != y.ival()) return false;
    break;
  case EXPR::BIGINT:
    if (mpz_cmp(x.zval(), y.zval()) != 0) return false;
    break;
  case EXPR::DBL:
    if (x.dval() != y.dval()) return false;
    break;
  case EXPR::STR:
    if (strcmp(x.sval(), y.sval()) != 0) return false;
    break;
  // application:
  case EXPR::APP:
    if (!match(xvars, xsubst, xttag, yttag,
	       anon_tag, iface_tag, x.xval1(), y.xval1()) ||
	!match(xvars, xsubst, xttag, yttag,
	       anon_tag, iface_tag, x.xval2(), y.xval2()))
      return false;
    break;
  // matrix (Pure 0.47+):
  case EXPR::MATRIX: {
    exprll *xs = x.xvals(), *ys = y.xvals();
    size_t nx = xs->size(), mx = xs->empty()?0:xs->front().size();
    size_t ny = ys->size(), my = ys->empty()?0:ys->front().size();
    if (nx != ny || mx != my) return false;
    for (exprll::const_iterator it = xs->begin(), jt = ys->begin();
	 it != xs->end(); ++it, ++jt) {
      assert(jt != ys->end());
      for (exprl::const_iterator it2 = it->begin(), jt2 = jt->begin();
	   it2 != it->end(); ++it2, ++jt2) {
	assert(jt2 != jt->end());
	if (!match(xvars, xsubst, xttag, yttag,
		   anon_tag, iface_tag, *it2, *jt2))
	  return false;
      }
    }
    break;
  }
  // these must not occur on the lhs:
  case EXPR::PTR:
  case EXPR::WRAP:
  case EXPR::LAMBDA:
  case EXPR::COND:
  case EXPR::CASE:
  case EXPR::WHEN:
  case EXPR::WITH:
    return false;
  default:
    assert(x.tag() > 0);
    break;
  }
  if (x.astag() > 0) {
    // Bind variables in "as" patterns. We need to do this to properly handle
    // non-linearities.
    if (x.astag() != anon_tag) {
      map<int32_t, expr>::iterator it = xvars.find(x.astag());
      if (it != xvars.end()) {
	// Variable is already bound (this may happen if the term is
	// non-linear), check that the bindings match.
	if (!equal(anon_tag, it->second, y))
	  return false;
      } else {
	// Record a binding for this variable.
	xvars[x.astag()]= y;
      }
    }
  }
  return true;
}

void interpreter::add_interface_rule(env &e, int32_t tag, expr& x, bool check)
{
  env::iterator it = e.find(tag);
  if (it != e.end() && it->second.t != env_info::none) {
    if (it->second.argc != 1) {
      const symbol& sym = symtab.sym(tag);
      ostringstream msg;
      msg << "type predicate '" << sym.s
	  << "' was previously defined with " << it->second.argc << " args";
      throw err(msg.str());
    }
  }
  env_info &info = e[tag];
  if (info.t == env_info::none)
    info = env_info(1, rulel(), temp);
  assert(info.argc == 1);
  if (!info.xs) info.xs = new exprl;
  env vars; vinfo vi;
  expr y = expr(bind(vars, vi, lcsubst(x), false));
  if (check) {
    rule r(y, expr(EXPR::INT, 1));
    checkfuns(true, &r);
    if (nerrs > 0) {
      if (info.xs->empty()) {
	delete info.xs; info.xs = 0;
      }
      return;
    }
  }
  expr fx; count_args(y, fx);
  int32_t f = fx.tag();
  if (f <= 0) {
    if (info.xs->empty()) {
      delete info.xs; info.xs = 0;
    }
    throw err("error in interface declaration (missing head symbol)");
  }
  fx.flags() |= EXPR::GLOBAL;
  // Check to see whether we got a duplicate.
  for (exprl::iterator it = info.xs->begin(); it != info.xs->end(); ++it)
    if (equiv(symtab.anon_sym, *it, y)) return;
  info.xs->push_back(y);
  if (check && compat) {
    if (!info.compat) info.compat = new exprset;
    info.compat->insert(y);
  }
}

void interpreter::add_interface_rule_at(env &e, int32_t tag, expr& x,
					exprl::iterator& p)
{
  env::iterator it = e.find(tag);
  if (it != e.end() && it->second.t != env_info::none) {
    if (it->second.argc != 1) {
      const symbol& sym = symtab.sym(tag);
      ostringstream msg;
      msg << "type predicate '" << sym.s
	  << "' was previously defined with " << it->second.argc << " args";
      throw err(msg.str());
    }
  }
  env_info &info = e[tag];
  if (info.t == env_info::none)
    info = env_info(1, rulel(), temp);
  assert(info.argc == 1);
  if (!info.xs) info.xs = new exprl;
  env vars; vinfo vi;
  expr y = expr(bind(vars, vi, lcsubst(x), false));
  expr fx; count_args(y, fx);
  int32_t f = fx.tag();
  if (f <= 0) {
    if (info.xs->empty()) {
      delete info.xs; info.xs = 0;
    }
    throw err("error in interface declaration (missing head symbol)");
  }
  fx.flags() |= EXPR::GLOBAL;
  p = info.xs->insert(p, y); p++;
}

static expr interface_subst(int32_t tag, int32_t tag2, expr x)
{
  expr y;
  switch (x.tag()) {
  case EXPR::VAR:
    if (x.ttag() == tag2)
      y = expr(EXPR::VAR, x.vtag(), 0, tag, x.vpath());
    else
      return x;
    break;
  case EXPR::APP:
    y = expr(interface_subst(tag, tag2, x.xval1()),
	     interface_subst(tag, tag2, x.xval2()));
    break;
  case EXPR::MATRIX: {
    exprll *xs = x.xvals();
    exprll *ys = new exprll;
    for (exprll::const_iterator it = xs->begin(); it != xs->end(); ++it) {
      ys->push_back(exprl());
      exprl& zs = ys->back();
      for (exprl::const_iterator jt = it->begin(); jt != it->end(); ++it) {
	expr u = interface_subst(tag, tag2, *jt);
	zs.push_back(u);
      }
    }
    y = expr(EXPR::MATRIX, ys);
    break;
  }
  default:
    return x;
  }
  if (x.astag() > 0) {
    y.set_astag(x.astag());
    y.set_aspath(x.aspath());
  }
  return y;
}

int interpreter::add_sub_interface(env &e, int32_t tag, int32_t iface)
{
  env::iterator it = e.find(iface);
  if (it == e.end() || it->second.t == env_info::none || !it->second.xs) {
    const symbol& sym = symtab.sym(iface);
    throw err("unknown interface type '"+sym.s+"'");
  }
  exprl& xs = *it->second.xs;
  it = e.find(tag);
  if (it != e.end() && it->second.t != env_info::none) {
    if (it->second.argc != 1) {
      const symbol& sym = symtab.sym(tag);
      ostringstream msg;
      msg << "type predicate '" << sym.s
	  << "' was previously defined with " << it->second.argc << " args";
      throw err(msg.str());
    }
  }
  env_info &info = e[tag];
  if (info.t == env_info::none)
    info = env_info(1, rulel(), temp);
  assert(info.argc == 1);
  if (!info.xs) info.xs = new exprl;
  for (exprl::iterator x = xs.begin(); x != xs.end(); ++x) {
    expr u = interface_subst(tag, iface, *x);
    bool have = false;
    for (exprl::iterator it = info.xs->begin(); it != info.xs->end(); ++it)
      if (equiv(symtab.anon_sym, *it, u)) {
	have = true; break;
      }
    if (have) continue;
    info.xs->push_back(u);
    if (compat) {
      if (!info.compat) info.compat = new exprset;
      info.compat->insert(u);
    }
  }
  return xs.size();
}

void interpreter::finalize_interface_rules(env &e, int32_t tag, size_t count,
					   exprl::iterator *p)
{
  env::iterator it = e.find(tag);
  if (it == e.end() || it->second.t == env_info::none) {
    if ((verbose&verbosity::defs) != 0) {
      const symbol& sym = symtab.sym(tag);
      cout << "interface " << sym.s << " with\nend;\n";
    }
    // empty interface matches everything
    env_info &info = e[tag];
    info = env_info(1, rulel(), temp);
    info.xs = new exprl;
    mark_dirty_type(tag);
    return;
  }
  env_info &info = it->second;
  if (info.xs && p) {
    if (*p == info.xs->end())
      p = 0;
    else
      for (size_t i = 0; i < count; ++i) --*p;
  }
  if ((verbose&verbosity::defs) != 0) {
    const symbol& sym = symtab.sym(tag);
    cout << "interface " << sym.s << " with\n";
    if (info.xs) {
      exprl::iterator it;
      if (p)
	it = *p;
      else {
	it = info.xs->begin();
	size_t sz = info.xs->size();
	for (; sz > count && it != info.xs->end(); ++it) --sz;
      }
      size_t n = count;
      for (; n-- > 0 && it != info.xs->end(); ++it) {
	cout << "  ";
	printx(cout, *it, true);
	cout << ";\n";
      }
    }
    cout << "end;\n";
  }
  if (count > 0) {
    mark_dirty_type(tag);
    // Keep track of the functions used by this interface, so that the type
    // gets invalidated automatically when one of them becomes dirty.
    if (info.xs) {
      exprl::iterator it;
      if (p)
	it = *p;
      else {
	it = info.xs->begin();
	size_t sz = info.xs->size();
	for (; sz > count && it != info.xs->end(); ++it) --sz;
      }
      size_t n = count;
      for (; n-- > 0 && it != info.xs->end(); ++it) {
	expr fx; count_args(*it, fx);
	int32_t f = fx.tag();
	assert(f>0);
	fun_types[f].insert(tag);
      }
    }
  }
}

/* Shortcut to rebind the variables in a lhs expression which already went
   through bind(). This updates the paths to their current values, and
   recomputes the nonlinearities and type checks. */

static expr rebind(env& vars, vinfo& vi, int32_t anon_tag,
		   expr x, path p = path())
{
  expr y;
  switch (x.tag()) {
  case EXPR::VAR:
    if (x.vtag() != anon_tag) {
      env::iterator it = vars.find(x.vtag());
      if (it != vars.end()) {
	env_info& info = it->second;
	// non-linearity, record an equality
	vi.eqns.push_back(veqn(x.vtag(), *info.p, p));
      } else
	vars[x.vtag()] = env_info(x.ttag(), p);
    }
    if (x.ttag() > 0)
      // record a type guard
      vi.guards.push_back(vguard(x.vtag(), x.ttag(), p));
    y = expr(EXPR::VAR, x.vtag(), 0, x.ttag(), p);
    break;
  case EXPR::APP:
    y = expr(rebind(vars, vi, anon_tag, x.xval1(), path(p, 0)),
	     rebind(vars, vi, anon_tag, x.xval2(), path(p, 1)));
    break;
  case EXPR::MATRIX: {
    exprll *xs = x.xvals();
    size_t n = xs->size(), m = xs->empty()?0:xs->front().size();
    // Encode subterm paths inside a matrix. Check bind() to see how this
    // magic works.
    exprll *ys = new exprll;
    path pi(p); size_t l = p.len();
    for (exprll::const_iterator it = xs->begin(); n>0;) {
      ys->push_back(exprl());
      exprl& zs = ys->back();
      path pj(pi, 0); pj.setmsk(l, 1);
      for (exprl::const_iterator jt = it->begin(); m>0;) {
	expr u = rebind(vars, vi, anon_tag, *jt, path(pj, 0));
	zs.push_back(u);
	if (++jt != it->end()) pj += 1; else break;
      }
      if (++it != xs->end()) pi += 1; else break;
    }
    y = expr(EXPR::MATRIX, ys);
    break;
  }
  default:
    if (x.astag() > 0) {
      // We have to update an "as" binding. Copy the original expression so
      // that we don't clobber its data.
      switch (x.tag()) {
      case EXPR::FVAR:
	y = expr(EXPR::FVAR, x.vtag());
	break;
      case EXPR::INT:
	y = expr(EXPR::INT, x.ival());
	break;
      case EXPR::BIGINT: {
	mpz_t z;
	mpz_init_set(z, x.zval());
	y = expr(EXPR::BIGINT, z);
	break;
      }
      case EXPR::DBL:
	y = expr(EXPR::DBL, x.dval());
	break;
      case EXPR::STR:
	y = expr(EXPR::STR, strdup(x.sval()));
	break;
      // these should have been expanded away already:
      case EXPR::PTR:
      case EXPR::WRAP:
      case EXPR::LAMBDA:
      case EXPR::COND:
      case EXPR::CASE:
      case EXPR::WHEN:
      case EXPR::WITH:
	return x;
      default:
	assert(x.tag() > 0);
	y = expr(x.tag());
	break;
      }
    } else
      return x;
    break;
  }
  if (x.astag() > 0) {
    y.set_astag(x.astag());
    y.set_aspath(p);
  }
  return y;
}

/* Compilation algorithm for interface definitions. This extracts the relevant
   patterns from the program source, so the interface type becomes the type
   defined by those patterns.

   The algorithm looks for the left-hand sides of equations matching the
   interface patterns. For each matching left-hand side, the substitutions for
   variables in the interface patterns which are tagged with the interface
   type give the candidate patterns which may become part of the type.

   The candidate patterns are grouped by the interface functions which they
   belong to. If P1, ..., Pn are the corresponding pattern sets for the
   interface functions f1, ..., fn, and X1, ..., Xn are the corresponding term
   sets (i.e., Xi is the set of all instances of any of the patterns in Pi),
   then, in theory, the interface type is the intersection of X1, ..., Xn.

   Unfortunately, the number of patterns to describe this intersection may
   well be exponential in size. Essentially, we'd have to unify each
   combination of patterns pi from Pi, i=1, ..., n, which simply isn't
   practical, even for interface definitions involving just a moderate number
   of functions.

   Hence the following approach is taken: We eliminate from each Pi those
   patterns pi which don't match at least one pattern pj in Pj, for each
   j!=i. This way we may loose some patterns (we always err on the safe side,
   so that the programmer can be confident that members of the type work with
   all interface functions), but the method seems to work quite well in
   practice. (In fact, it also has the nice advantage that we can warn about
   potentially missing rules for eliminated patterns in some of the interface
   operations.)

   Note that in pathological cases the method sketched out above may in fact
   not just loose some, but most (or at least some important) possible
   patterns. In such a case you'll just have to bite the bullet and write the
   definitions of the interface operations in a way that makes them easier to
   handle. To help with this, run the script with warnings on (-w option,
   --warn pragma); the compiler will then inform you about the eliminated
   patterns and the interface functions which might need some tweaking. */

rulel *interpreter::compile_interface(env &e, int32_t tag)
{
  env::iterator it = e.find(tag);
  if (it == e.end() || it->second.t == env_info::none || !it->second.xs)
    return 0;
  // The list of candidate patterns constructed by the algorithm.
  rulel *rl = new rulel;
  if (it->second.xs->empty()) {
    // empty interface, produce a rule that matches everything
    expr lhs = expr(expr(tag), expr(symtab.anon_sym)),
      rhs = expr(EXPR::INT, 1);
    rule r(lhs, rhs); closure(r, false);
    rl->push_back(r);
    return rl;
  }
  // The list of interface patterns.
  exprl& xs = *it->second.xs;
  /* Record patterns for each function in the interface, and "skip" sets which
     indicate which functions are associated with the candidate patterns rl
     (the latter is a list of sets in the same order as rl). */
  map<int32_t, exprl> patterns;
  list< set<int32_t> > skip;
  /* We try to give fairly extensive diagnostics here (all in the form of
     warnings), and want to be as accurate as possible and selective in that
     we only produce the diagnostics requested by the programmer. Since the
     interface compiler may be invoked at any time, at the compiler's
     discretion, the current settings of the warning options when we're
     invoked aren't really relevant. Instead, the frontend records the warning
     options in effect when the interface definition was parsed on a per-rule
     basis. Here we translate this to a set of interface functions for which
     diagnostics are to be produced. */
  exprset emptyset, &checked = it->second.compat?*it->second.compat:emptyset;
  bool first = true;
  bool warnings = !checked.empty();
  bool nomatch = false;
  set<int32_t> warned, hints;
  /* Pass #1: Collect all candidate patterns. *******************************/
  for (exprl::iterator it = xs.begin(); it != xs.end(); ++it) {
    // Analyze the interface pattern.
    int32_t f;
    exprl args = get_args(*it, f);
    if (f <= 0) continue;
    bool warn = checked.find(*it) != checked.end();
    // Additional diagnostics for patterns eliminated from an interface in
    // pass #2 because some interface operation doesn't implement them.
    if (warn) hints.insert(f);
    // "Defined functions" are always considered complete, and thus don't
    // place any additional restrictions on their arguments. That's why we
    // don't have to consider them in pass #2 of the algorithm.
    bool complete = set_defined_sym(f);
    // Check to see whether we have a matching function. NOTE: We currently
    // require that the number of arguments must match exactly. Maybe we
    // should also allow argc<n?
    size_t n = args.size();
    env::iterator kt = globenv.find(f);
    if (kt == globenv.end() || kt->second.t != env_info::fun ||
	kt->second.argc != n) {
      // No matching function, so the interface can't be matched either.
      nomatch = true;
      if (first && warnings) {
	// Do a quick check on all expression patterns to see if any of them
	// could be matched at all. If this isn't the case, then the interface
	// is abstract, i.e., it hasn't been implemented yet. We don't want to
	// warn about these.
	warnings = false;
	for (exprl::iterator it = xs.begin(); it != xs.end(); ++it) {
	  int32_t f;
	  exprl args = get_args(*it, f);
	  if (f <= 0) continue;
	  env::iterator kt = globenv.find(f);
	  if (kt == globenv.end() || kt->second.t != env_info::fun)
	    continue;
	  warnings = true; break;
	}
      }
      // If we're not warning about these symbols, we might as well bail out
      // now. Otherwise, we keep at it, to give proper diagnostics for all
      // functions required by the interface.
      if (!warnings) {
	delete rl;
	return 0;
      } else if (warn) {
	if (warned.find(f) == warned.end()) {
	  if (first) {
	    const symbol& sym = symtab.sym(tag);
	    warning("warning: interface '"+sym.s+"' is incomplete");
	    first = false;
	  }
	  const symbol& fsym = symtab.sym(f);
	  bool mismatch = kt != globenv.end() &&
	    kt->second.t == env_info::fun && kt->second.argc != n;
	  ostringstream msg;
	  if (mismatch) {
	    msg << "function '" << fsym.s << "' was declared with " << n
		<< " but defined with " << kt->second.argc << " args";
	  } else
	    msg << "no matching rule for function '" << fsym.s << "'";
	  warning("warning: "+msg.str());
	  warned.insert(f);
	}
	continue;
      } else
	continue;
    }
    // To find all the patterns for the interface type, match our pattern
    // against the left-hand sides of the function's rules.
    map<int32_t, int32_t> xttag;
    // these only need to be computed once
    get_ttags(xttag, symtab.anon_sym, *it);
    int count = 0;
    for (rulel::iterator jt = kt->second.rules->begin();
	 jt != kt->second.rules->end(); ++jt) {
      exprl xsubst;
      map<int32_t, expr> xvars;
      map<int32_t, int32_t> yttag;
      get_ttags(yttag, symtab.anon_sym, jt->lhs);
      if (match(xvars, xsubst, xttag, yttag,
		symtab.anon_sym, tag, *it, jt->lhs)) {
	if (xsubst.empty()) {
	  // This pattern doesn't contain the type tag and doesn't place any
	  // restrictions on the interface. Nothing to see here, move along.
	  count++; continue;
	}
	// We got a match, pass through all terms bound to variables tagged
	// with the interface type; these give the patterns we're looking for.
	for (exprl::iterator v = xsubst.begin(); v != xsubst.end(); ++v) {
	  if (v->tag() == EXPR::VAR) {
	    /* Check for singleton variables without a type tag (or the
	       interface type tag), which just match everything and thus don't
	       place any restrictions on the interface. */
	    int32_t vtag = (v->vtag() == symtab.anon_sym)?v->ttag():
	      yttag[v->vtag()];
	    if (vtag == 0 || vtag == tag) {
	      count++; continue;
	    }
	  }
	  count++;
	  if (!complete) patterns[f].push_back(*v);
	  // We got a real pattern. Compare it against the patterns we
	  // already have, to eliminate duplicates. XXXFIXME: This needs
	  // quadratic time, later we may want to figure out something faster.
	  bool have = false;
	  list< set<int32_t> >::reverse_iterator s = skip.rbegin();
	  for (rulel::reverse_iterator r = rl->rbegin();
	       !have && r != rl->rend(); ++r, ++s) {
	    if (equiv(symtab.anon_sym, *v, r->lhs.xval2())) {
	      have = true; s->insert(f);
	    }
	  }
	  // Skip this pattern if we already have it.
	  if (have) continue;
	  // Add a rule for a new pattern.
	  env vars; vinfo vi;
	  expr lhs = rebind(vars, vi, symtab.anon_sym, expr(expr(tag), *v)),
	    rhs = expr(EXPR::INT, 1);
	  rule r(lhs, rhs, vi);
	  rl->push_back(r);
	  // Initialize the "skip" set of functions this pattern belongs to.
	  skip.push_back(set<int32_t>());
	  skip.back().insert(f);
	}
      }
    }
    if (count == 0) {
      // We didn't find any rule that matches the requested function, so the
      // entire interface fails to match.
      nomatch = true;
      if (!warnings) {
	delete rl;
	return 0;
      } else if (warn) {
	if (warned.find(f) == warned.end()) {
	  if (first) {
	    const symbol& sym = symtab.sym(tag);
	    warning("warning: interface '"+sym.s+"' is incomplete");
	    first = false;
	  }
	  const symbol& fsym = symtab.sym(f);
	  warning("warning: no matching rule for function '"+fsym.s+"'");
	  warned.insert(f);
	}
      }
    }
  }
  if (nomatch) {
    delete rl;
    return 0;
  }
  /* Pass #2: Eliminate candidate patterns. *********************************/
  if (!rl->empty()) {
    /* Make another pass over the patterns we collected, and remove all
       patterns which aren't implemented by *all* of the functions. */
    bool first = true, warn = warnings && !hints.empty();
    list< set<int32_t> >::iterator s = skip.begin();
    for (rulel::iterator r = rl->begin(); r != rl->end(); ++s) {
      expr x = r->lhs.xval2();
      map<int32_t, int32_t> xttag;
      get_ttags(xttag, symtab.anon_sym, x);
      bool good = true;
      for (map<int32_t, exprl>::iterator p = patterns.begin();
	   p != patterns.end(); ++p) {
	int32_t f = p->first;
	// Check whether the pattern actually belongs to this function. In
	// this case it will always be matched, so we may skip ahead.
	if (s->find(f) != s->end()) continue;
	exprl& ys = p->second;
	bool ok = false;
	for (exprl::iterator y = ys.begin(); !ok && y != ys.end(); ++y) {
	  exprl ysubst;
	  map<int32_t, expr> yvars;
	  map<int32_t, int32_t> yttag;
	  get_ttags(yttag, symtab.anon_sym, *y);
	  ok = match(yvars, ysubst, yttag, xttag, symtab.anon_sym, tag, *y, x);
	}
	if (!ok) {
	  if (warn && hints.find(f) != hints.end()) {
	    if (first) {
	      const symbol& sym = symtab.sym(tag);
	      warning
		("warning: interface '"+sym.s+"' may be incomplete");
	      first = false;
	    }
	    /* NOTE: This may generate spurious warnings about patterns which
	       weren't intended to be part of the interface in the first
	       place, but just happen to be implemented by some interface
	       function. The compiler can't really know, but you can disable
	       such warnings with --nowarn on a per-function basis. */
	    const symbol& fsym = symtab.sym(f);
	    ostringstream msg;
	    msg << "warning: function '" << fsym.s
		<< "' might lack a rule for '";
	    printx(msg, x, true);
	    msg << "'";
	    warning(msg.str());
	  }
	  good = false;
	  // We may as well break out of the loop now. But if warnings are
	  // enabled, we keep going to give diagnostics about the other
	  // interface functions.
	  if (!warn) break;
	}
      }
      rulel::iterator r1 = r; ++r;
      if (!good) rl->erase(r1);
    }
  }
  /* Pass #3: Eliminate superflous candidate patterns. **********************/
  if (!rl->empty()) {
    /* Make a final pass over the patterns we collected, and remove all
       patterns which are subsumed by other patterns, so that we only keep the
       most general patterns. */
    for (rulel::iterator r = rl->begin(); r != rl->end(); ) {
      expr x = r->lhs.xval2();
      map<int32_t, int32_t> xttag;
      get_ttags(xttag, symtab.anon_sym, x);
      bool have = false;
      for (rulel::iterator r2 = rl->begin(); r2 != rl->end(); ++r2) {
	if (r2 == r) continue;
	expr y = r2->lhs.xval2();
	map<int32_t, int32_t> yttag;
	map<int32_t, expr> yvars;
	exprl ysubst;
	get_ttags(yttag, symtab.anon_sym, y);
	if (match(yvars, ysubst, yttag, xttag, symtab.anon_sym, tag, y, x)) {
	  have = true; break;
	}
      }
      rulel::iterator r1 = r; ++r;
      if (have) rl->erase(r1);
    }
  }
  if (rl->empty()) {
    delete rl;
    rl = 0;
  }
  return rl;
}

void interpreter::add_simple_rule(rulel &rl, rule *r)
{
  assert(!r->lhs.is_null());
  rl.push_back(*r);
  delete r;
}

void interpreter::add_macro_rules(rulel *r)
{
  for (rulel::iterator ri = r->begin(), end = r->end(); ri != end; ri++)
    add_macro_rule(*ri);
  if (tags) add_tags(r);
  delete r;
}

void interpreter::add_macro_rule(rule& r, bool check)
{
  assert(!r.lhs.is_null() && r.qual.is_null() && !r.rhs.is_guarded());
  last.clear(); closure(r, false);
  if (check) {
    checkfuns(true, &r);
    if (nerrs > 0) return;
  }
  expr fx; uint32_t argc = count_args(r.lhs, fx);
  int32_t f = fx.tag();
  if (f <= 0)
    throw err("error in macro definition (missing head symbol)");
  fx.flags() |= EXPR::GLOBAL;
  env::iterator it = macenv.find(f), jt = globenv.find(f);
  const symbol& sym = symtab.sym(f);
  if (jt != globenv.end()) {
    if (jt->second.t == env_info::cvar)
      throw err("symbol '"+sym.s+"' is already defined as a constant");
    else if (jt->second.t == env_info::fvar)
      throw err("symbol '"+sym.s+"' is already defined as a variable");
  }
  if (it != macenv.end()) {
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
  r.temp = temp;
  if (override) {
    rulel::iterator p = info.rules->begin();
    for (; p != info.rules->end() && p->temp >= temp; p++) ;
    info.rules->insert(p, r);
  } else
    info.rules->push_back(r);
  if ((verbose&verbosity::defs) != 0) cout << "def " << r << ";\n";
  if (info.m) {
    // this will be recomputed the next time the macro is needed
    delete info.m;
    info.m = 0;
  }
}

void interpreter::add_macro_rule_at(rule& r, int32_t g, rulel::iterator& p)
{
  assert(!r.lhs.is_null() && r.qual.is_null() && !r.rhs.is_guarded());
  last.clear(); closure(r, false);
  expr fx; uint32_t argc = count_args(r.lhs, fx);
  int32_t f = fx.tag();
  if (f <= 0)
    throw err("error in macro definition (missing head symbol)");
  else if (f != g)
    throw err("error in macro definition (wrong head symbol)");
  fx.flags() |= EXPR::GLOBAL;
  env::iterator it = macenv.find(f), jt = globenv.find(f);
  const symbol& sym = symtab.sym(f);
  if (jt != globenv.end()) {
    if (jt->second.t == env_info::cvar)
      throw err("symbol '"+sym.s+"' is already defined as a constant");
    else if (jt->second.t == env_info::fvar)
      throw err("symbol '"+sym.s+"' is already defined as a variable");
  }
  if (it == macenv.end())
    throw err("error in macro definition (no existing rule)");
  if (it->second.argc != argc) {
    ostringstream msg;
    msg << "macro '" << sym.s
	<< "' was previously defined with " << it->second.argc << " args";
    throw err(msg.str());
  }
  env_info &info = macenv[f];
  assert(info.t == env_info::fun);
  assert(info.argc == argc);
  r.temp = temp;
  p = info.rules->insert(p, r); p++;
  if ((verbose&verbosity::defs) != 0) cout << "def " << r << ";\n";
  if (info.m) {
    // this will be recomputed the next time the macro is needed
    delete info.m;
    info.m = 0;
  }
}

void interpreter::closure(rule& r, bool b)
{
  env vars; vinfo vi;
  expr u = expr(bind(vars, vi, lcsubst(r.lhs), b)),
    v = expr(subst(vars, rsubst(r.rhs))),
    w = expr(subst(vars, rsubst(r.qual)));
  r = rule(u, v, vi, w);
}

static bool check_occurrences(const veqnl& eqns, int32_t vtag, const path& p)
{
  for (veqnl::const_iterator it = eqns.begin(), end = eqns.end();
       it != end; ++it)
    if (it->tag == vtag && p <= it->q)
      return true;
  return false;
}

static inline int32_t ttag_sym(int32_t ttag)
{
  interpreter& interp = *interpreter::g_interp;
  switch (ttag) {
  case EXPR::INT:
    return interp.symtab.int_sym().f;
  case EXPR::BIGINT:
    return interp.symtab.bigint_sym().f;
  case EXPR::DBL:
    return interp.symtab.double_sym().f;
  case EXPR::STR:
    return interp.symtab.string_sym().f;
  case EXPR::PTR:
    return interp.symtab.pointer_sym().f;
  case EXPR::MATRIX:
    return interp.symtab.matrix_sym().f;
  default:
    assert(ttag != 0);
    return ttag;
  }
}

string interpreter::ttag_msg(int32_t tag)
{
  if (tag <= 0) {
    // built-in type tag
    tag = ttag_sym(tag);
    if (tag > 0)
      return "type tag '"+symtab.sym(tag).s+"'";
    else // this shouldn't happen
      return "type tag <unknown>";
  }
#if 0
  // check for a user-defined type tag
  env::iterator e = typeenv.find(tag);
  if (e == typeenv.end())
    // this doesn't appear to be a valid type tag
    return "type tag '"+symtab.sym(tag).s+"', or bad qualified symbol";
#endif
  return "type tag '"+symtab.sym(tag).s+"'";
}

/* Rectify unqualified variable symbols. We always resolve these in the
   current namespace, to avoid any discrepancies due to the use of namespace
   brackets. Note that this might well yield a different symbol if the symbol
   was shadowed by a namespace bracket when it was parsed. */

int32_t interpreter::rectify(int32_t f, bool b)
{
  const symbol& sym = symtab.sym(f);
  if (b || sym.f == symtab.anon_sym ||
      sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix)
    return f;
  string id = sym.s;
  size_t p = symsplit(id);
  if (p != string::npos) id.erase(0, p+2);
  const symbol &sym2 = symtab.checksym(id);
  return sym2.f;
}

/* Bind variable symbols in a pattern. NOTE: The a flag indicates whether
   we're operating inside an application or a toplevel context; this is always
   true except for the immediate subterms of a matrix pattern. (You should
   never have to set this flag explicitly outside of the bind() routine
   itself.) The b flag indicates a pattern binding, in which case an ordinary
   symbol always denotes a variable, unless we're at the head symbol of an
   application (in which case the a flag must be true). The b flag
   automagically becomes true in the arguments of an application (if it isn't
   already set by the caller anyway). The b flag is also consulted to
   determine whether an "as" variable is permitted at the current position
   inside the term (which is the case unless we're on the spine of an
   outermost application). */

expr interpreter::bind(env& vars, vinfo& vi, expr x, bool b, path p, bool a)
{
  assert(!x.is_null());
  expr y;
  switch (x.tag()) {
  case EXPR::VAR: {
    // previously bound variable (e.g., successor rule)
    const symbol& sym = symtab.sym(x.vtag());
    if (sym.f != symtab.anon_sym) { // '_' = anonymous variable
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
    y = x;
    break;
  // application:
  case EXPR::APP: {
    if (p.len() >= MAXDEPTH)
      throw err("error in pattern (nesting too deep)");
    expr u = bind(vars, vi, x.xval1(), b, path(p, 0)),
      v = bind(vars, vi, x.xval2(), 1, path(p, 1));
    y = expr(u, v);
    break;
  }
  // matrix (Pure 0.47+):
  case EXPR::MATRIX: {
    exprll *xs = x.xvals();
    size_t n = xs->size(), m = xs->empty()?0:xs->front().size();
    /* We require a rectangular matrix here. */
    for (exprll::const_iterator it = xs->begin(), end = xs->end();
	 it != end; ++it) {
      if (it->size() != m)
	throw err("error in pattern (non-rectangular matrix)");
    }
    /* Subterm paths inside a matrix are encoded as two groups of bit patterns
       of the form 1...10, where the number of 1's indicates the row or column
       index. First comes the row, then the column index. Hence we need room
       for at least n+m+2 additional bits. */
    if (p.len()+n+m+1 >= MAXDEPTH)
      throw err("error in pattern (nesting too deep)");
    exprll *ys = new exprll;
    path pi(p); size_t l = p.len();
    for (exprll::const_iterator it = xs->begin(); n>0;) {
      ys->push_back(exprl());
      exprl& zs = ys->back();
      path pj(pi, 0); pj.setmsk(l, 1); // mark this as a matrix path
      for (exprl::const_iterator jt = it->begin(); m>0;) {
	expr u = bind(vars, vi, *jt, 1, path(pj, 0), 0);
	zs.push_back(u);
	if (++jt != it->end()) pj += 1; else break;
      }
      if (++it != xs->end()) pi += 1; else break;
    }
    y = expr(EXPR::MATRIX, ys);
    break;
  }
  // these must not occur on the lhs:
  case EXPR::PTR:
  case EXPR::WRAP:
    throw err("pointer or closure not permitted in pattern");
    break;
  // these are already substituted away by lcsubst:
  case EXPR::LAMBDA:
  case EXPR::COND:
  case EXPR::CASE:
  case EXPR::WHEN:
  case EXPR::WITH:
    assert(0 && "this can't happen");
    break;
  default:
    assert(x.tag() > 0);
    const symbol& sym = symtab.sym(x.tag());
    if ((!qual && (x.flags()&EXPR::QUAL)) ||
	(sym.f != symtab.anon_sym &&
	 (sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix ||
	  (p.len() == 0 && !b) || (p.len() > 0 && p.last() == 0 && a)))) {
      // constant or constructor
      if (x.ttag() != 0)
	throw err("error in pattern (misplaced "+ttag_msg(x.ttag())+")");
      y = x;
    } else {
      const symbol& sym =
	symtab.sym(rectify(x.tag(), x.flags()&EXPR::ASQUAL));
      env::iterator it = vars.find(sym.f);
      if (sym.f != symtab.anon_sym) { // '_' = anonymous variable
	if (it != vars.end()) {
	  env_info& info = it->second;
	  if (x.ttag() && x.ttag() != info.ttag)
	    throw err("error in pattern (invalid "+ttag_msg(x.ttag())+")");
	  // non-linearity, record an equality
	  vi.eqns.push_back(veqn(sym.f, *info.p, p));
	} else
	  vars[sym.f] = env_info(x.ttag(), p);
      }
      if (x.ttag() > 0)
	// record a type guard
	vi.guards.push_back(vguard(sym.f, x.ttag(), p));
      y = expr(EXPR::VAR, sym.f, 0, x.ttag(), p);
    }
    break;
  }
  // check for "as" patterns
  if (x.astag() > 0) {
    const symbol& sym =
      symtab.sym(rectify(x.astag(), x.flags()&EXPR::ASQUAL));
    if (sym.f != symtab.anon_sym) {
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
	env_info& info = it->second;
	if (info.ttag)
	  throw err("error in pattern (invalid "+ttag_msg(info.ttag)+")");
	// Check whether the variable occurs in a subterm, this is an error.
	if (p <= *info.p || check_occurrences(vi.eqns, sym.f, p))
	  throw err("error in pattern (recursive variable '"+sym.s+"')");
	// non-linearity, record an equality
	vi.eqns.push_back(veqn(sym.f, *info.p, p));
      }
      vars[sym.f] = env_info(0, p);
      y.set_astag(sym.f);
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
  }
}

static string qualifier(const symbol& sym, string& id)
{
  size_t pos = symsplit(sym.s);
  string qual;
  if (pos == string::npos) {
    qual = "";
    pos = 0;
  } else {
    qual = sym.s.substr(0, pos);
    pos += 2;
  }
  id = sym.s.substr(pos);
  return qual;
}

static void check_typetag(interpreter& interp, yy::location* loc,
			  int32_t tag)
{
  // we only check that the type symbol is declared here
  env::iterator e = interp.typeenv.find(tag);
  if (e == interp.typeenv.end()) {
    interp.warning(*loc, "warning: implicit declaration of type tag '"+
		   interp.symtab.sym(tag).s+"'");
    interp.typeenv[tag];
  }
}

bool interpreter::is_quoteargs(expr x)
{
  int32_t f; uint32_t argc = count_args(x, f);
  env::iterator it = macenv.find(f);
  return it != macenv.end() && argc <= it->second.argc &&
    quoteargs.find(f) != quoteargs.end();
}

void interpreter::funsubstw(set<int32_t>& warned, bool ty_check,
			    expr x, int32_t f, int32_t g, bool b)
{
  if (x.is_null()) return;
  switch (x.tag()) {
  // constants:
  case EXPR::VAR:
    // check for type tags
    if (ty_check && compat && x.ttag() > 0)
      check_typetag(*this, loc, x.ttag());
    break;
  case EXPR::FVAR:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::WRAP:
    break;
  // matrix:
  case EXPR::MATRIX:
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	funsubstw(warned, ty_check, *ys, f, g);
      }
    }
    break;
  // application:
  case EXPR::APP:
    funsubstw(warned, ty_check, x.xval1(), f, g, b);
    if (is_quote(x.xval1().tag()) || is_quoteargs(x)) {
      // suppress warnings in quoted subexpression
      bool s_compat = compat; compat = false;
      funsubstw(warned, ty_check, x.xval2(), f, g);
      compat = s_compat;
    } else
      funsubstw(warned, ty_check, x.xval2(), f, g);
    break;
  // conditionals:
  case EXPR::COND:
    funsubstw(warned, ty_check, x.xval1(), f, g);
    funsubstw(warned, ty_check, x.xval2(), f, g);
    funsubstw(warned, ty_check, x.xval3(), f, g);
    break;
  case EXPR::COND1:
    funsubstw(warned, ty_check, x.xval1(), f, g);
    funsubstw(warned, ty_check, x.xval2(), f, g);
    break;
  // nested closures:
  case EXPR::LAMBDA:
    for (exprl::iterator xs = x.largs()->begin(), end = x.largs()->end();
	   xs != end; xs++) {
      funsubstw(warned, ty_check, *xs, f, g);
    }
    funsubstw(warned, ty_check, x.lrule().lhs, f, g);
    funsubstw(warned, ty_check, x.lrule().rhs, f, g);
    break;
  case EXPR::CASE:
    funsubstw(warned, ty_check, x.xval(), f, g);
    for (rulel::const_iterator it = x.rules()->begin();
	 it != x.rules()->end(); ++it) {
      funsubstw(warned, ty_check, it->lhs, f, g);
      funsubstw(warned, ty_check, it->rhs, f, g);
      funsubstw(warned, ty_check, it->qual, f, g);
    }
    break;
  case EXPR::WHEN:
    funsubstw(warned, ty_check, x.xval(), f, g);
    for (rulel::const_iterator it = x.rules()->begin();
	 it != x.rules()->end(); ++it) {
      funsubstw(warned, ty_check, it->lhs, f, g);
      funsubstw(warned, ty_check, it->rhs, f, g);
      funsubstw(warned, ty_check, it->qual, f, g);
    }
    break;
  case EXPR::WITH: {
    map<int32_t,bool> locals;
    for (env::const_iterator it = x.fenv()->begin();
	 it != x.fenv()->end(); ++it) {
      int32_t h = it->first;
      if (locals.find(h) == locals.end()) {
	locals[h] = symtab.sym(h).unresolved;
	symtab.sym(h).unresolved = false;
      }
    }
    for (env::const_iterator it = x.fenv()->begin();
	 it != x.fenv()->end(); ++it) {
      const env_info& info = it->second;
      const rulel *r = info.rules;
      for (rulel::const_iterator jt = r->begin(); jt != r->end(); ++jt) {
	funsubstw(warned, ty_check, jt->lhs, f, g, true);
	funsubstw(warned, ty_check, jt->rhs, f, g);
	funsubstw(warned, ty_check, jt->qual, f, g);
      }
    }
    funsubstw(warned, ty_check, x.xval(), f, g);
    for (map<int32_t,bool>::const_iterator it = locals.begin(),
	   end = locals.end(); it != end; ++it) {
      int32_t h = it->first;
      bool unresolved = it->second;
      symtab.sym(h).unresolved = unresolved;
    }
    break;
  }
  default:
    assert(x.tag() > 0);
    if (!b && x.tag() == f && g != f && (x.flags()&EXPR::QUAL) == 0) {
      // make sure that we don't accidentally clobber a cached symbol node here
      assert(x != symtab.sym(x.tag()).x);
      x.set_tag(g);
    } else {
      symbol& sym = symtab.sym(x.tag());
      if (sym.unresolved) {
	// unresolved symbol, needs to be promoted to the current namespace
	assert(strstr(sym.s.c_str(), "::") == 0);
	if (!symtab.current_namespace->empty()) {
	  symbol *sym2 = symtab.sym("::"+*symtab.current_namespace+"::"+sym.s);
	  assert(sym2);
	  x.set_tag(sym2->f);
	  if (compat && warned.find(sym.f) == warned.end()) {
	    warning(*loc, "warning: implicit declaration of '"+sym2->s+"'");
	    // prevent cascades of warnings for the same symbol
	    warned.insert(sym.f);
	  }
	} else if (!b) {
	  sym.unresolved = false;
	  /* Experimental: Warn about unknown symbol references in the default
	     namespace, too. */
	  if (compat && x.tag() != f)
	    warning(*loc, "warning: implicit declaration of '"+sym.s+"'");
	}
      }
    }
  }
}

static string symtype(const symbol& sym)
{
  if (sym.fix == nonfix)
    return "nonfix";
  else if (sym.fix == outfix)
    return "outfix";
  else if (sym.prec < PREC_MAX) {
    switch (sym.fix) {
    case infix:
      return "infix";
    case infixl:
      return "infixl";
    case infixr:
      return "infixr";
    case prefix:
      return "prefix";
    case postfix:
      return "postfix";
    case nonfix:
    case outfix:
      break;
    }
  }
  assert(0 && "this can't happen");
  return "";
}

void interpreter::checkfuns(bool ty_check, rule *r)
{
  int32_t f = 0, g = 0;
  expr x = r->lhs, y, z;
  while (x.is_app(y, z)) x = y;
  if (x.tag() <= 0 || x.ttag() != 0) return;
  const symbol& sym = symtab.sym(x.tag());
  string id, qual = qualifier(sym, id);
  f = sym.f; g = f;
  if (!symtab.current_namespace->empty()) {
    /* EXPR::QUAL in the flags signifies a qualified symbol in another
       namespace; this is OK if the symbol is already declared (otherwise the
       lexer will complain about it). If this flag isn't set, the symbol isn't
       qualified and might have been picked up from elsewhere, in which case
       we promote it to the current namespace. Note that we can only do this
       for an ordinary identifier; if the symbol is an operator then
       presumably our parse is botched already, hence in this case we just
       give up and flag an undeclared symbol instead. */
    bool head_needs_fixing =
      qual != *symtab.current_namespace && (x.flags()&EXPR::QUAL) == 0;
    if (head_needs_fixing) {
      if (sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix) {
	if (sym.fix == outfix && sym.g) {
	  const symbol& sym2 =symtab.sym(sym.g);
	  string id2, qual2 = qualifier(sym2, id2);
	  error(*loc, "outfix symbol '"+id+" "+id2+
		"' was not declared in this namespace");
	} else
	  error(*loc, symtype(sym)+" symbol '"+id+
		"' was not declared in this namespace");
	return;
      }
      // Might have to fix up the head symbol.
      symbol *sym2 = symtab.sym(*symtab.current_namespace+"::"+id);
      assert(sym2);
      g = sym2->f;
    } else if (compat2 && (x.flags()&EXPR::QUAL) &&
	       qual == *symtab.current_namespace)
      warning(*loc, "hint: unneeded qualification in '"+sym.s+"'");
  }
  // Promote the head symbol to the current namespace if necessary, and fix up
  // all other unresolved symbols along the way.
  funsubst(ty_check, r->lhs, f, g);
  funsubst(false, r->rhs, f, g);
  funsubst(false, r->qual, f, g);
}

void interpreter::checkfuns(expr x, bool b)
{
  funsubst(false, x, 0, 0, b);
}

void interpreter::checkvars(expr x, bool b)
{
  switch (x.tag()) {
  case EXPR::VAR:
  case EXPR::FVAR:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  // these must not occur on the lhs:
  case EXPR::PTR:
  case EXPR::WRAP:
  case EXPR::LAMBDA:
  case EXPR::COND:
  case EXPR::CASE:
  case EXPR::WHEN:
  case EXPR::WITH:
    break;
  // matrix (Pure 0.47+):
  case EXPR::MATRIX:
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	checkvars(*ys, true);
      }
    }
    break;
  // application:
  case EXPR::APP:
    checkvars(x.xval1(), false); checkvars(x.xval2(), true);
    break;
  default:
    assert(x.tag() > 0);
    symbol& sym = symtab.sym(x.tag());
    if (!b || symtab.current_namespace->empty() || sym.f == symtab.anon_sym ||
	(sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix))
      sym.unresolved = false;
    else if (x.flags()&EXPR::QUAL) {
      string id, qual = qualifier(sym, id);
      if (compat2 && qual == *symtab.current_namespace)
	warning(*loc, "hint: unneeded qualification in '"+sym.s+"'");
      sym.unresolved = false;
    } else {
      string id, qual = qualifier(sym, id);
      if (qual != *symtab.current_namespace) {
	// promote the symbol to the current namespace
	assert(qual.empty());
	symbol *sym2 = symtab.sym("::"+*symtab.current_namespace+"::"+id);
	assert(sym2);
	x.set_tag(sym2->f);
      } else
	sym.unresolved = false;
    }
    break;
  }
  // check for "as" patterns
  if (x.astag() > 0) {
    symbol& sym = symtab.sym(x.astag());
    if (symtab.current_namespace->empty() ||
	sym.f == symtab.anon_sym || (x.flags()&EXPR::ASQUAL) ||
	sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix)
      sym.unresolved = false;
    else {
      string id, qual = qualifier(sym, id);
      if (qual != *symtab.current_namespace) {
	// promote the symbol to the current namespace
	assert(qual.empty());
	symbol *sym2 = symtab.sym("::"+*symtab.current_namespace+"::"+id);
	assert(sym2);
	x.set_astag(sym2->f);
      } else
	sym.unresolved = false;
    }
  }
  // check for type tags
  if (compat && x.ttag() > 0)
    check_typetag(*this, loc, x.ttag());
}

expr interpreter::subst(const env& vars, expr x, uint8_t idx)
{
  if (x.is_null()) return x;
  if (x.astag() > 0)
    throw err("error in expression (misplaced \"as\" pattern)");
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
	v = subst(vars, x.xval2(), idx),
	w = expr(u, v);
      // This flag is still needed during macro substitution.
      w.flags() |= x.flags()&EXPR::PAREN;
      return w;
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
    exprl *u = x.largs(); expr v = subst(vars, x.lrule().rhs, idx);
    return expr::lambda(new exprl(*u), v, x.lrule().vi);
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
      s->push_back(rule(u, v, it->vi, w));
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
      s->push_back(rule(u, v, it->vi));
      if (u.is_var() && u.vtag() == symtab.anon_sym && u.ttag() == 0)
	// anonymous binding, gets thrown away
	;
      else if (++idx == 0)
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
	s.push_back(rule(u, v, jt->vi, w));
      }
      (*f)[g] = env_info(info.argc, s, info.temp);
    }
    return expr::with(u, f);
  }
  default:
    assert(x.tag() > 0);
    if (x.ttag() != 0)
      throw err("error in expression (misplaced "+ttag_msg(x.ttag())+")");
    const symbol& sym = symtab.sym(x.tag());
    env::const_iterator it = vars.find(rectify(sym.f, x.flags()&EXPR::QUAL));
    if (sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix ||
	it == vars.end() || (!qual && (x.flags()&EXPR::QUAL))) {
      // not a bound variable
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
    exprl *u = x.largs(); expr v = fsubst(funs, x.lrule().rhs, idx);
    return expr::lambda(new exprl(*u), v, x.lrule().vi);
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
      s->push_back(rule(u, v, it->vi, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = fsubst(funs, it->rhs, idx);
      s->push_back(rule(u, v, it->vi));
      if (u.is_var() && u.vtag() == symtab.anon_sym && u.ttag() == 0)
	// anonymous binding, gets thrown away
	;
      else if (++idx == 0)
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
	s.push_back(rule(u, v, jt->vi, w));
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

expr interpreter::bsubst(expr x)
{
  expr f; uint32_t n = count_args(x, f);
  // promote type tags
  if (n == 1)
    promote_ttags(f, x, x.xval2());
  else if (n == 2)
    promote_ttags(f, x, x.xval1().xval2(), x.xval2());
  if (!folding || (x.ttag() != EXPR::INT && x.ttag() != EXPR::DBL))
    return x;
  else if (n == 1 && x.xval2().tag() == EXPR::INT) {
    // unary int operations
    int32_t u = x.xval2().ival();
    if (f.tag() == symtab.neg_sym().f)
      return expr(EXPR::INT, -u);
    else if (f.tag() == symtab.not_sym().f)
      return expr(EXPR::INT, !u);
    else if (f.tag() == symtab.bitnot_sym().f)
      return expr(EXPR::INT, ~u);
    else
      return x;
  } else if (n == 1 && x.xval2().tag() == EXPR::DBL) {
    // unary double operations
    double u = x.xval2().dval();
    if (f.tag() == symtab.neg_sym().f)
      return expr(EXPR::DBL, -u);
    else
      return x;
  } else if (n == 2 && x.xval1().xval2().tag() == EXPR::INT &&
	     x.xval2().tag() == EXPR::INT) {
    // binary int operations
    int32_t u = x.xval1().xval2().ival();
    int32_t v = x.xval2().ival();
    if (f.tag() == symtab.or_sym().f)
      return expr(EXPR::INT, u?u:v);
    else if (f.tag() == symtab.and_sym().f)
      return expr(EXPR::INT, u?v:u);
    else if (f.tag() == symtab.bitor_sym().f)
      return expr(EXPR::INT, u|v);
    else if (f.tag() == symtab.bitand_sym().f)
      return expr(EXPR::INT, u&v);
    else if (f.tag() == symtab.shl_sym().f)
      return expr(EXPR::INT, u<<v);
    else if (f.tag() == symtab.shr_sym().f)
      return expr(EXPR::INT, u>>v);
    else if (f.tag() == symtab.less_sym().f)
      return expr(EXPR::INT, u<v);
    else if (f.tag() == symtab.greater_sym().f)
      return expr(EXPR::INT, u>v);
    else if (f.tag() == symtab.lesseq_sym().f)
      return expr(EXPR::INT, u<=v);
    else if (f.tag() == symtab.greatereq_sym().f)
      return expr(EXPR::INT, u>=v);
    else if (f.tag() == symtab.equal_sym().f)
      return expr(EXPR::INT, u==v);
    else if (f.tag() == symtab.notequal_sym().f)
      return expr(EXPR::INT, u!=v);
    else if (f.tag() == symtab.plus_sym().f)
      return expr(EXPR::INT, u+v);
    else if (f.tag() == symtab.minus_sym().f)
      return expr(EXPR::INT, u-v);
    else if (f.tag() == symtab.mult_sym().f)
      return expr(EXPR::INT, u*v);
    else if (f.tag() == symtab.div_sym().f) {
      // catch division by zero
      if (v == 0)
	return x;
      else
	return expr(EXPR::INT, u/v);
    } else if (f.tag() == symtab.mod_sym().f) {
      // catch division by zero
      if (v == 0)
	return x;
      else
	return expr(EXPR::INT, u%v);
    } else
      return x;
  } else if (n == 2 &&
	     (x.xval1().xval2().tag() == EXPR::INT ||
	      x.xval1().xval2().tag() == EXPR::DBL) &&
	     (x.xval2().tag() == EXPR::INT ||
	      x.xval2().tag() == EXPR::DBL)) {
    // binary int/double operations
    double u = x.xval1().xval2().tag() == EXPR::INT
      ? (double)x.xval1().xval2().ival()
      : x.xval1().xval2().dval();
    double v = x.xval2().tag() == EXPR::INT
      ? (double)x.xval2().ival()
      : x.xval2().dval();
    if (f.tag() == symtab.less_sym().f)
      return expr(EXPR::INT, u<v);
    else if (f.tag() == symtab.greater_sym().f)
      return expr(EXPR::INT, u>v);
    else if (f.tag() == symtab.lesseq_sym().f)
      return expr(EXPR::INT, u<=v);
    else if (f.tag() == symtab.greatereq_sym().f)
      return expr(EXPR::INT, u>=v);
    else if (f.tag() == symtab.equal_sym().f)
      return expr(EXPR::INT, u==v);
    else if (f.tag() == symtab.notequal_sym().f)
      return expr(EXPR::INT, u!=v);
    else if (f.tag() == symtab.plus_sym().f)
      return expr(EXPR::DBL, u+v);
    else if (f.tag() == symtab.minus_sym().f)
      return expr(EXPR::DBL, u-v);
    else if (f.tag() == symtab.mult_sym().f)
      return expr(EXPR::DBL, u*v);
    else if (f.tag() == symtab.fdiv_sym().f)
      return expr(EXPR::DBL, u/v);
    else
      return x;
  } else
    return x;
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
      return bsubst(w);
    }
  // conditionals:
  case EXPR::COND: {
    expr u = csubst(x.xval1()),
      v = csubst(x.xval2()),
      w = csubst(x.xval3());
    if (folding && u.tag() == EXPR::INT)
      // Eliminate conditionals if we know the value at compile time.
      if (u.ival())
	return v;
      else
	return w;
    else
      return expr::cond(u, v, w);
  }
  case EXPR::COND1: {
    expr u = csubst(x.xval1()),
      v = csubst(x.xval2());
    return expr::cond1(u, v);
  }
  // nested closures:
  case EXPR::LAMBDA: {
    exprl *u = x.largs(); expr v = csubst(x.lrule().rhs);
    return expr::lambda(new exprl(*u), v, x.lrule().vi);
  }
  case EXPR::CASE: {
    expr u = csubst(x.xval());
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs,	v = csubst(it->rhs),
	w = csubst(it->qual);
      s->push_back(rule(u, v, it->vi, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = csubst(it->rhs);
      s->push_back(rule(u, v, it->vi));
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
	s.push_back(rule(u, v, jt->vi, w));
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
	if (it->second.cval_var) {
	  // cached constant value
#if 1
	  /* Record the cached value in the expression, so that the printer
	     prints the actual value instead of the symbol. This maintains the
	     illusion that a cached constant is substituted into the rhs of
	     definitions, even though as of Pure 0.38 the value is actually
	     stored in a read-only variable at runtime. We recommend to leave
	     this enabled for backward compatibility with Pure <= 0.37. */
	  return expr(x.tag(), it->second.cval_var);
#else
          /* Print cached constants as symbols; this may be useful for
	     debugging purposes. */
          return x;
#endif
	} else
	  // inlined constant value
	  return *it->second.cval;
      else
	return x;
    }
  }
}

/* This is a trimmed-down version of csubst() for performing replacements of
   nonfix const symbols in patterns. It also takes care of substituting
   runtime representations for specials (lambda, conditionals, 'case', 'when',
   'with'). */

expr interpreter::lcsubst(expr x)
{
  if (x.is_null()) return x;
  switch (x.tag()) {
  // constants:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::VAR:
  case EXPR::FVAR:
    return x;
  // these must not occur on the lhs:
  case EXPR::PTR:
  case EXPR::WRAP:
    throw err("pointer or closure not permitted in pattern");
    break;
  // substitute runtime representations for specials:
  case EXPR::COND: {
    expr u = quoted_ifelse(x.xval1(), x.xval2(), x.xval3());
    u.set_astag(x.astag());
    return lcsubst(u);
  }
  case EXPR::LAMBDA: {
    expr u = quoted_lambda(x.largs(), x.lrule().rhs);
    u.set_astag(x.astag());
    return lcsubst(u);
  }
  case EXPR::CASE: {
    expr u = quoted_case(x.xval(), x.rules());
    u.set_astag(x.astag());
    return lcsubst(u);
  }
  case EXPR::WHEN: {
    expr u = quoted_when(x.xval(), x.rules());
    u.set_astag(x.astag());
    return lcsubst(u);
  }
  case EXPR::WITH: {
    expr u = quoted_with(x.xval(), x.fenv());
    u.set_astag(x.astag());
    return lcsubst(u);
  }
  // matrix (Pure 0.47+):
  case EXPR::MATRIX: {
    exprll *us = new exprll;
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      us->push_back(exprl());
      exprl& vs = us->back();
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	vs.push_back(lcsubst(*ys));
      }
    }
    expr w = expr(EXPR::MATRIX, us);
    w.set_astag(x.astag());
    return w;
  }
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

/* Variation of lcsubst which only substitutes quoted specials in the rhs. */

expr interpreter::rsubst(expr x, bool quote)
{
  if (x.is_null()) return x;
  if (x.astag() > 0)
    throw err("error in expression (misplaced \"as\" pattern)");
  switch (x.tag()) {
  // constants:
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::VAR:
  case EXPR::FVAR:
  case EXPR::PTR:
  case EXPR::WRAP:
    return x;
  // substitute runtime representations for specials:
  case EXPR::COND:
    if (quote)
      return rsubst(quoted_ifelse(x.xval1(), x.xval2(), x.xval3()), quote);
    else
      return x;
  case EXPR::COND1:
    if (quote)
      return rsubst(quoted_if(x.xval2(), x.xval1()), quote);
    else
      return x;
  case EXPR::LAMBDA:
    if (quote)
      return rsubst(quoted_lambda(x.largs(), x.lrule().rhs), quote);
    else
      return x;
  case EXPR::CASE:
    if (quote)
      return rsubst(quoted_case(x.xval(), x.rules()), quote);
    else
      return x;
  case EXPR::WHEN:
    if (quote)
      return rsubst(quoted_when(x.xval(), x.rules()), quote);
    else
      return x;
  case EXPR::WITH:
    if (quote)
      return rsubst(quoted_with(x.xval(), x.fenv()), quote);
    else
      return x;
  // matrix (Pure 0.47+):
  case EXPR::MATRIX: {
    exprll *us = new exprll;
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      us->push_back(exprl());
      exprl& vs = us->back();
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	vs.push_back(rsubst(*ys, quote));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP: {
    bool quote1 = quote || is_quote(x.xval1().tag());
    expr u = rsubst(x.xval1(), quote), v = rsubst(x.xval2(), quote1);
    expr w = expr(u, v);
    // This flag is still needed during macro substitution.
    w.flags() |= x.flags()&EXPR::PAREN;
    return w;
  }
  default:
    assert(x.tag() > 0);
    if (x.ttag() != 0)
      throw err("error in expression (misplaced "+ttag_msg(x.ttag())+")");
    return x;
  }
}

/* Substitute embedded quoted 'if's in the rhs. */

static int32_t get2args(expr x, expr& y, expr& z)
{
  expr a, b;
  if (!x.is_app(a, b)) return 0;
  x = a; z = b;
  if (!x.is_app(a, b)) return 0;
  x = a; y = b;
  return x.tag();
}

static int32_t get3args(expr x, expr& y, expr& z, expr& t)
{
  expr a, b;
  if (!x.is_app(a, b)) return 0;
  x = a; t = b;
  if (!x.is_app(a, b)) return 0;
  x = a; z = b;
  if (!x.is_app(a, b)) return 0;
  x = a; y = b;
  return x.tag();
}

expr interpreter::ifsubst(expr x)
{
  if (x.is_null()) return x;
  switch (x.tag()) {
  case EXPR::WHEN: {
    expr u = ifsubst(x.xval());
    if (u != x.xval())
      return expr::when(u, new rulel(*x.rules()));
    else
      return x;
  }
  case EXPR::WITH: {
    expr u = ifsubst(x.xval());
    if (u != x.xval())
      return expr::with(u, new env(*x.fenv()));
    else
      return x;
  }
  case EXPR::APP: {
    expr u, v; exprl xs;
    int32_t f = get2args(x, u, v);
    if (f == symtab.if_sym().f) {
      expr w = expr::cond1(v, u);
      return w;
    } else if (f == symtab.when_sym().f && v.is_list(xs)) {
      expr w = ifsubst(u);
      if (w != u)
	return expr(symtab.when_sym().x, w, v);
      else
	return x;
    } else if (f == symtab.with_sym().f && v.is_list(xs)) {
      for (exprl::iterator y = xs.begin(), end = xs.end(); y!=end; y++) {
	expr u, v;
	if (get2args(*y, u, v) != symtab.eqn_sym().f) return x;
      }
      expr w = ifsubst(u);
      if (w != u)
	return expr(symtab.with_sym().x, w, v);
      else
	return x;
    } else
      return x;
  }
  default:
    return x;
  }
}

/* Do the necessary variable substitutions inside quoted specials. */

expr interpreter::vsubst(expr x, int offs, int offs1, uint8_t idx)
{
  if (x.is_null()) return x;
  switch (x.tag()) {
  case EXPR::VAR:
  case EXPR::FVAR:
    if (x.vidx() < idx)
      /* reference to local environment inside the substituted value; skip */
      return x;
    if (x.tag() == EXPR::FVAR && x.vidx() < idx+offs1)
      /* reference to function symbol in quoted 'with', quote */
      return expr(x.vtag());
    if (x.vidx() < idx+offs)
      /* reference to other symbol in quoted special, quote */
      return expr(x.vtag());
    /* reference to local environment outside the substituted value; shift the
       deBruijn indices accordingly */
    if (offs == 0)
      return x;
    else if (x.tag() == EXPR::VAR)
      return expr(EXPR::VAR, x.vtag(), x.vidx()-offs, x.ttag(), x.vpath());
    else
      return expr(EXPR::FVAR, x.vtag(), x.vidx()-offs);
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
	vs.push_back(vsubst(*ys, offs, offs1, idx));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP: {
    if (x.xval1().tag() == symtab.amp_sym().f) {
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = vsubst(x.xval2(), offs, offs1, idx);
      return expr(symtab.amp_sym().x, v);
    } else if (x.xval1().tag() == EXPR::APP &&
	       x.xval1().xval1().tag() == symtab.catch_sym().f) {
      expr u = vsubst(x.xval1().xval2(), offs, offs1, idx);
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = vsubst(x.xval2(), offs, offs1, idx);
      return expr(symtab.catch_sym().x, u, v);
    } else {
      expr u = vsubst(x.xval1(), offs, offs1, idx),
	v = vsubst(x.xval2(), offs, offs1, idx);
      return expr(u, v);
    }
  }
  // conditionals:
  case EXPR::COND: {
    expr u = vsubst(x.xval1(), offs, offs1, idx),
      v = vsubst(x.xval2(), offs, offs1, idx),
      w = vsubst(x.xval3(), offs, offs1, idx);
    return expr::cond(u, v, w);
  }
  case EXPR::COND1: {
    expr u = vsubst(x.xval1(), offs, offs1, idx),
      v = vsubst(x.xval2(), offs, offs1, idx);
    return expr::cond1(u, v);
  }
  // nested closures:
  case EXPR::LAMBDA: {
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    exprl *u = x.largs(); expr v = vsubst(x.lrule().rhs, offs, offs1, idx);
    return expr::lambda(new exprl(*u), v, x.lrule().vi);
  }
  case EXPR::CASE: {
    expr u = vsubst(x.xval(), offs, offs1, idx);
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs,	v = vsubst(it->rhs, offs, offs1, idx),
	w = vsubst(it->qual, offs, offs1, idx);
      s->push_back(rule(u, v, it->vi, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = vsubst(it->rhs, offs, offs1, idx);
      s->push_back(rule(u, v, it->vi));
      if (u.is_var() && u.vtag() == symtab.anon_sym && u.ttag() == 0)
	// anonymous binding, gets thrown away
	;
      else if (++idx == 0)
	throw err("error in expression (too many nested closures)");
    }
    expr u = vsubst(x.xval(), offs, offs1, idx);
    return expr::when(u, s);
  }
  case EXPR::WITH: {
    expr u = vsubst(x.xval(), offs, offs, idx);
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
	expr u = jt->lhs, v = vsubst(jt->rhs, offs, offs1, idx),
	  w = vsubst(jt->qual, offs, offs1, idx);
	s.push_back(rule(u, v, jt->vi, w));
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

/* This is a simplified version of the above for the lhs in an equation. In
   this case we simply quote all symbols, but we also have to take care of
   specials as well as type tags and as patterns in the lhs, which have to be
   expanded to their runtime representations. */

expr interpreter::vsubst(expr x)
{
  if (x.is_null()) return x;
  switch (x.tag()) {
  case EXPR::VAR:
  case EXPR::FVAR: {
    expr u = expr(x.vtag());
    return quoted_tag(u, x.astag(), x.ttag());
  }
  // constants:
  case EXPR::INT:
    if (x.astag()) {
      expr u = expr(EXPR::INT, x.ival());
      return quoted_tag(u, x.astag());
    } else
      return x;
  case EXPR::BIGINT:
    if (x.astag()) {
      // The expr constructor clobbers its mpz_t argument, so take a copy.
      mpz_t z;
      mpz_init_set(z, x.zval());
      expr u = expr(EXPR::BIGINT, z);
      return quoted_tag(u, x.astag());
    } else
      return x;
  case EXPR::DBL:
    if (x.astag()) {
      expr u = expr(EXPR::DBL, x.dval());
      return quoted_tag(u, x.astag());
    } else
      return x;
  case EXPR::STR:
    if (x.astag()) {
      expr u = expr(EXPR::STR, strdup(x.sval()));
      return quoted_tag(u, x.astag());
    } else
      return x;
  case EXPR::PTR:
    if (x.astag()) {
      expr u = expr(EXPR::PTR, x.pval());
      return quoted_tag(u, x.astag());
    } else
      return x;
  case EXPR::WRAP:
    if (x.astag()) {
      GlobalVar *v = (GlobalVar*)x.pval();
      expr u = wrap_expr(v->x);
      return quoted_tag(u, x.astag());
    } else
      return x;
  // substitute runtime representations for specials:
  case EXPR::COND: {
    expr u = quoted_ifelse(x.xval1(), x.xval2(), x.xval3());
    return quoted_tag(u, x.astag());
  }
  case EXPR::LAMBDA: {
    expr u = quoted_lambda(x.largs(), x.lrule().rhs);
    return quoted_tag(u, x.astag());
  }
  case EXPR::CASE: {
    expr u = quoted_case(x.xval(), x.rules());
    return quoted_tag(u, x.astag());
  }
  case EXPR::WHEN: {
    expr u = quoted_when(x.xval(), x.rules());
    return quoted_tag(u, x.astag());
  }
  case EXPR::WITH: {
    expr u = quoted_with(x.xval(), x.fenv());
    return quoted_tag(u, x.astag());
  }
  // matrix (Pure 0.47+):
  case EXPR::MATRIX: {
    exprll *us = new exprll;
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      us->push_back(exprl());
      exprl& vs = us->back();
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	vs.push_back(vsubst(*ys));
      }
    }
    expr w = expr(EXPR::MATRIX, us);
    return quoted_tag(w, x.astag());
  }
  // application:
  case EXPR::APP: {
    expr u = vsubst(x.xval1()), v = vsubst(x.xval2());
    expr w = expr(u, v);
    return quoted_tag(w, x.astag());
  }
  default: {
    assert(x.tag() > 0);
    if (x.astag() || x.ttag()) {
      expr u = expr(x.tag());
      return quoted_tag(u, x.astag(), x.ttag());
    } else
      return x;
  }
  }
}

/* Perform simple macro substitutions on a compile time expression. Does
   applicative-order (depth-first) evaluation using the defined macro
   substitution rules (which are simple, unconditional term rewriting
   rules). Everything else but macro applications is considered a literal
   here. When we match a macro call, we perform the corresponding reduction
   and evaluate the result recursively.

   Note that in contrast to compiled rewriting rules this is essentially a
   little term rewriting interpreter here, so it's kind of slow compared to
   compiled code, but for macro substitution it should be good enough. */

expr interpreter::macsubst(int32_t h, bool trace, expr x, envstack& estk,
			   uint8_t idx, bool quote)
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
	vs.push_back(macsubst(h, trace, *ys, estk, idx, quote));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP:
    if (quote) {
      expr u = macsubst(h, false, x.xval1(), estk, idx, quote),
	v = macsubst(h, false, x.xval2(), estk, idx, quote),
	w = expr(u, v);
      w.flags() |= x.flags()&EXPR::PAREN;
      return w;
    } else if (is_quote(x.xval1().tag())) {
      expr u = x.xval1(),
	v = macsubst(h, false, x.xval2(), estk, idx, true);
      return expr(u, v);
    } else if (x.xval1().tag() == symtab.amp_sym().f) {
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = macsubst(0, trace, x.xval2(), estk, idx);
      return expr(symtab.amp_sym().x, v);
    } else if (x.xval1().tag() == EXPR::APP &&
	       x.xval1().xval1().tag() == symtab.catch_sym().f) {
      expr u = macsubst(h, trace, x.xval1().xval2(), estk, idx);
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = macsubst(0, trace, x.xval2(), estk, idx);
      return expr(symtab.catch_sym().x, u, v);
    } else {
      expr u = macsubst(h, trace, x.xval1(), estk, idx);
      // See whether we're constructing a quoteargs macro call here, in this
      // case the argument must be quoted to prevent name capture in embedded
      // macro calls.
      int32_t f; uint32_t argc = count_args(x, f);
      env::iterator it = macenv.find(f);
      quote = it != macenv.end() && argc <= it->second.argc &&
	quoteargs.find(f) != quoteargs.end();
      // See whether we're tracing a macro call.
      trace = trace ||
	(!trace_skip && mac_tracepoints.find(f) != mac_tracepoints.end());
      expr v = macsubst(h, trace, x.xval2(), estk, idx, quote);
      expr w = expr(u, v);
      w.flags() |= x.flags()&EXPR::PAREN;
      return macval(h, trace, w, estk, idx);
    }
  // conditionals:
  case EXPR::COND: {
    expr u = macsubst(h, trace, x.xval1(), estk, idx, quote),
      v = macsubst(h, trace, x.xval2(), estk, idx, quote),
      w = macsubst(h, trace, x.xval3(), estk, idx, quote);
    return expr::cond(u, v, w);
  }
  case EXPR::COND1: {
    expr u = macsubst(h, trace, x.xval1(), estk, idx, quote),
      v = macsubst(h, trace, x.xval2(), estk, idx, quote);
    return expr::cond1(u, v);
  }
  // nested closures:
  case EXPR::LAMBDA: {
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    exprl *u = x.largs();
    expr v = macsubst(0, trace, x.lrule().rhs, estk, idx, quote);
    return expr::lambda(new exprl(*u), v, x.lrule().vi);
  }
  case EXPR::CASE: {
    expr u = macsubst(h, trace, x.xval(), estk, idx, quote);
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs,	v = macsubst(0, trace, it->rhs, estk, idx, quote),
	w = macsubst(0, trace, it->qual, estk, idx, quote);
      s->push_back(rule(u, v, it->vi, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = macsubst(h, trace, it->rhs, estk, idx, quote);
      s->push_back(rule(u, v, it->vi));
      if (u.is_var() && u.vtag() == symtab.anon_sym && u.ttag() == 0)
	// anonymous binding, gets thrown away
	;
      else if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      else
	h = 0;
    }
    expr u = macsubst(h, trace, x.xval(), estk, idx, quote);
    return expr::when(u, s);
  }
  case EXPR::WITH: {
    const env *e = x.fenv();
    estk.push_front(enventry(e, idx));
    expr u = macsubst(h, trace, x.xval(), estk, idx, quote);
    env *f = new env;
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    for (env::const_iterator it = e->begin(); it != e->end(); ++it) {
      int32_t g = it->first;
      const env_info& info = it->second;
      const rulel *r = info.rules;
      rulel s;
      for (rulel::const_iterator jt = r->begin(); jt != r->end(); ++jt) {
	int32_t h = headsym(jt->lhs);
	expr u = jt->lhs, v = macsubst(h, trace, jt->rhs, estk, idx, quote),
	  w = macsubst(h, trace, jt->qual, estk, idx, quote);
	s.push_back(rule(u, v, jt->vi, w));
      }
      (*f)[g] = env_info(info.argc, s, info.temp);
    }
    estk.pop_front();
    return expr::with(u, f);
  }
  default:
    assert(x.tag() > 0);
    return quote?x:macval(h, trace, x, estk, idx);
  }
}

/* Perform a single macro reduction step. */

expr interpreter::varsubst(expr x, uint8_t offs, uint8_t offs1, uint8_t idx)
{
  char test;
  if (x.is_null()) return x;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax)
    throw err("recursion too deep in macro expansion");
  switch (x.tag()) {
  case EXPR::FVAR:
    if (x.vidx() < idx+offs1)
      /* reference to local environment inside the substituted value; skip */
      return x;
    /* falls through */
  case EXPR::VAR:
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
	vs.push_back(varsubst(*ys, offs, offs1, idx));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP: {
    if (x.xval1().tag() == symtab.amp_sym().f) {
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = varsubst(x.xval2(), offs, offs1, idx);
      return expr(symtab.amp_sym().x, v);
    } else if (x.xval1().tag() == EXPR::APP &&
	       x.xval1().xval1().tag() == symtab.catch_sym().f) {
      expr u = varsubst(x.xval1().xval2(), offs, offs1, idx);
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = varsubst(x.xval2(), offs, offs1, idx);
      return expr(symtab.catch_sym().x, u, v);
    } else {
      expr u = varsubst(x.xval1(), offs, offs1, idx),
	v = varsubst(x.xval2(), offs, offs1, idx),
	w = expr(u, v);
      w.flags() |= x.flags()&EXPR::PAREN;
      return w;
    }
  }
  // conditionals:
  case EXPR::COND: {
    expr u = varsubst(x.xval1(), offs, offs1, idx),
      v = varsubst(x.xval2(), offs, offs1, idx),
      w = varsubst(x.xval3(), offs, offs1, idx);
    return expr::cond(u, v, w);
  }
  case EXPR::COND1: {
    expr u = varsubst(x.xval1(), offs, offs1, idx),
      v = varsubst(x.xval2(), offs, offs1, idx);
    return expr::cond1(u, v);
  }
  // nested closures:
  case EXPR::LAMBDA: {
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    exprl *u = x.largs(); expr v = varsubst(x.lrule().rhs, offs, offs1, idx);
    return expr::lambda(new exprl(*u), v, x.lrule().vi);
  }
  case EXPR::CASE: {
    expr u = varsubst(x.xval(), offs, offs1, idx);
    if (++idx == 0)
      throw err("error in expression (too many nested closures)");
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs,	v = varsubst(it->rhs, offs, offs1, idx),
	w = varsubst(it->qual, offs, offs1, idx);
      s->push_back(rule(u, v, it->vi, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = varsubst(it->rhs, offs, offs1, idx);
      s->push_back(rule(u, v, it->vi));
      if (u.is_var() && u.vtag() == symtab.anon_sym && u.ttag() == 0)
	// anonymous binding, gets thrown away
	;
      else if (++idx == 0)
	throw err("error in expression (too many nested closures)");
    }
    expr u = varsubst(x.xval(), offs, offs1, idx);
    return expr::when(u, s);
  }
  case EXPR::WITH: {
    expr u = varsubst(x.xval(), offs, offs1+1, idx);
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
	expr u = jt->lhs, v = varsubst(jt->rhs, offs, offs1+1, idx),
	  w = varsubst(jt->qual, offs, offs1+1, idx);
	s.push_back(rule(u, v, jt->vi, w));
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
    exprl *u = y.largs(); expr v = macred(x, y.lrule().rhs, idx);
    return expr::lambda(new exprl(*u), v, y.lrule().vi);
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
      s->push_back(rule(u, v, it->vi, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = y.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = macred(x, it->rhs, idx);
      s->push_back(rule(u, v, it->vi));
      if (u.is_var() && u.vtag() == symtab.anon_sym && u.ttag() == 0)
	// anonymous binding, gets thrown away
	;
      else if (++idx == 0)
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
	s.push_back(rule(u, v, jt->vi, w));
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

exprl interpreter::get_macargs(expr x, bool quote)
{
  expr y, z;
  exprl xs;
  while (x.is_app(y, z)) xs.push_front(rsubst(z, quote)), x = y;
  return xs;
}

static bool sameexpr(expr x, expr y);

static bool sameexprl(exprl *xs, exprl *ys)
{
  exprl::iterator it, jt;
  for (it = xs->begin(), jt = ys->begin();
       it != xs->end() && jt != ys->end(); ++it, ++jt)
    if (!sameexpr(*it, *jt)) return false;
  return it == xs->end() && jt == ys->end();
}

static bool sameexprll(exprll *xs, exprll *ys)
{
  exprll::iterator it, jt;
  for (it = xs->begin(), jt = ys->begin();
       it != xs->end() && jt != ys->end(); ++it, ++jt)
    if (!sameexprl(&*it, &*jt)) return false;
  return it == xs->end() && jt == ys->end();
}

static bool samerule(const rule& x, const rule& y)
{
  return sameexpr(x.lhs, y.lhs) && sameexpr(x.rhs, y.rhs) &&
    sameexpr(x.qual, y.qual);
}

static bool samerulel(rulel *xs, rulel *ys)
{
  rulel::iterator it, jt;
  for (it = xs->begin(), jt = ys->begin();
       it != xs->end() && jt != ys->end(); ++it, ++jt)
    if (!samerule(*it, *jt)) return false;
  return it == xs->end() && jt == ys->end();
}

static bool sameenv(env *xs, env *ys)
{
  env::iterator it, jt;
  for (it = xs->begin(), jt = ys->begin();
       it != xs->end() && jt != ys->end(); ++it, ++jt) {
    if (it->first != jt->first)
      return false;
    env_info &ex = it->second, &ey = jt->second;
    assert(ex.t == env_info::fun && ey.t == env_info::fun);
    if (ex.argc != ey.argc || !samerulel(ex.rules, ey.rules))
      return false;
  }
  return it == xs->end() && jt == ys->end();
}

static bool sameexpr(expr x, expr y)
{
  char test;
  if (interpreter::stackmax > 0 &&
      interpreter::stackdir*(&test - interpreter::baseptr) >=
      interpreter::stackmax)
    throw err("recursion too deep in macro expansion");
 tail:
  if (x == y) return true;
  if (x.is_null() || y.is_null() || x.tag() != y.tag()) return false;
  switch (x.tag()) {
  case EXPR::VAR:
  case EXPR::FVAR:
    return x.vtag() == y.vtag() && x.vidx() == y.vidx();
  case EXPR::INT:
    return x.ival() == y.ival();
  case EXPR::BIGINT:
    return mpz_cmp(x.zval(), y.zval()) == 0;
  case EXPR::DBL:
    return x.dval() == y.dval();
  case EXPR::STR:
    return strcmp(x.sval(), y.sval()) == 0;
  case EXPR::PTR:
    return x.pval() == y.pval();
  case EXPR::WRAP: {
    /* This code path is currently unused, as this type of expression is only
       generated by constant substitutions which are performed after macro
       substitutions in the current implementation. */
    if (x.pval() == y.pval()) return true;
    GlobalVar *xv = (GlobalVar*)x.pval();
    GlobalVar *yv = (GlobalVar*)y.pval();
    return same(xv->x, yv->x);
  }
  case EXPR::MATRIX:
    return sameexprll(x.xvals(), y.xvals());
  case EXPR::APP:
    if (!sameexpr(x.xval1(), y.xval1())) return false;
    /* Fake a tail call here, so that we do not run out of stack space when
       comparing large lists or similar right-recursive structures. */
    x = x.xval2(); y = y.xval2();
    goto tail;
  case EXPR::LAMBDA:
    return sameexprl(x.largs(), y.largs()) &&
      sameexpr(x.lrule().rhs, y.lrule().rhs);
  case EXPR::COND:
    return sameexpr(x.xval1(), y.xval1()) && sameexpr(x.xval2(), y.xval2()) &&
      sameexpr(x.xval3(), y.xval3());
  case EXPR::CASE:
  case EXPR::WHEN:
    return sameexpr(x.xval(), y.xval()) && samerulel(x.rules(), y.rules());
  case EXPR::WITH:
    return sameexpr(x.xval(), y.xval()) && sameenv(x.fenv(), y.fenv());
  default:
    assert(x.tag()>0);
    return true;
  }
}

bool interpreter::checkguards(expr x, const vguardl& guards)
{
  for (vguardl::const_iterator it = guards.begin(); it != guards.end(); ++it) {
    expr u = subterm(x, it->p);
    // try to convert macro argument to a (quoted) runtime expression on the fly
    pure_expr *e = 0, *y = const_value_invoke(u, e, true);
    if (!y) {
      // not convertible, assume false
      if (e) pure_free(e);
      return false;
    }
    bool res = pure_typecheck(it->ttag, y);
    if (!res) return false;
  }
  return true;
}

bool interpreter::checkeqns(expr x, const veqnl& eqns)
{
  for (veqnl::const_iterator it = eqns.begin(); it != eqns.end(); ++it) {
    expr u = subterm(x, it->p), v = subterm(x, it->q);
    if (!sameexpr(u, v)) return false;
  }
  return true;
}

static inline int32_t sym_ttag(int32_t f)
{
  interpreter& interp = *interpreter::g_interp;
  if (f == interp.symtab.int_sym().f)
    return EXPR::INT;
  else if (f == interp.symtab.bigint_sym().f)
    return EXPR::BIGINT;
  else if (f == interp.symtab.double_sym().f)
    return EXPR::DBL;
  else if (f == interp.symtab.string_sym().f)
    return EXPR::STR;
  else if (f == interp.symtab.pointer_sym().f)
    return EXPR::PTR;
  else if (f == interp.symtab.matrix_sym().f)
    return EXPR::MATRIX;
  else {
    assert(f > 0);
    return f;
  }
}

expr interpreter::tagsubst(expr x)
{
  if (x.is_null()) return x;
  switch (x.tag()) {
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::WRAP:
    return x;
  case EXPR::VAR:
  case EXPR::FVAR:
    return expr(x.vtag());
  // matrix (Pure 0.47+):
  case EXPR::MATRIX: {
    exprll *us = new exprll;
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      us->push_back(exprl());
      exprl& vs = us->back();
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	vs.push_back(tagsubst(*ys));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP: {
    expr u, v;
    int32_t f = get2args(x, u, v);
    if (f == symtab.ttag_sym().f && u.tag() > 0 && v.tag() > 0) {
      expr w = u;
      // XXXTODO: We might want to do some plausibility checks here.
      w.set_ttag(sym_ttag(v.tag()));
      return w;
    } else if (f == symtab.astag_sym().f && u.tag() > 0) {
      expr w = tagsubst(v);
      // XXXTODO: We might want to do some plausibility checks here.
      w.set_astag(u.tag());
      return w;
    } else {
      expr u = tagsubst(x.xval1()), v = tagsubst(x.xval2());
      return expr(u, v);
    }
  }
  default:
    return x;
  }
}

bool interpreter::parse_rulel(exprl& xs, rulel& rl)
{
  for (exprl::iterator x = xs.begin(), end = xs.end(); x!=end; x++) {
    expr u, v;
    if (get2args(*x, u, v) == symtab.eqn_sym().f) {
      expr w, c;
      if (get2args(v, w, c) == symtab.if_sym().f) {
	rule r(tagsubst(u), varsubst(w, 1), varsubst(c, 1));
	add_rule(rl, r, true);
      } else {
	rule r(tagsubst(u), varsubst(ifsubst(v), 1));
	add_rule(rl, r, true);
      }
    } else
      return false;
  }
  return true;
}

bool interpreter::parse_simple_rulel(exprl& xs, rulel& rl, int& offs)
{
  offs = 0;
  for (exprl::iterator x = xs.begin(), end = xs.end(); x!=end; x++) {
    expr u, v;
    if (get2args(*x, u, v) == symtab.eqn_sym().f) {
      expr w = tagsubst(u);
      rule r(w, (offs==0)?v:varsubst(v, offs));
      rl.push_back(r);
      if (w.tag() == symtab.anon_sym && w.ttag() == 0)
	// anonymous binding, gets thrown away
	;
      else
	offs++;
    } else {
      rule r(expr(symtab.anon_sym), (offs==0)?*x:varsubst(*x, offs));
      rl.push_back(r);
    }
  }
  return true;
}

bool interpreter::parse_env(exprl& xs, env& e)
{
  for (exprl::iterator x = xs.begin(), end = xs.end(); x!=end; x++) {
    expr u, v;
    if (get2args(*x, u, v) == symtab.eqn_sym().f) {
      expr w, c;
      if (get2args(v, w, c) == symtab.if_sym().f) {
	rule r(tagsubst(u), varsubst(w, 1, 1), varsubst(c, 1, 1));
	add_rule(e, r);
      } else {
	rule r(tagsubst(u), varsubst(ifsubst(v), 1, 1));
	add_rule(e, r);
      }
    } else
      return false;
  }
  return true;
}

expr *interpreter::macspecial(int32_t h, bool trace, expr x,
			      envstack& estk, uint8_t idx)
{
  expr u, v, w;
  if (!specials_only) {
    if (x.tag() == symtab.gensym_sym().f) {
      return new expr(gensym_expr('x'));
    }
    if (x.tag() == symtab.dir_sym().f) {
      const char *s = srcdir.c_str();
      return new expr(EXPR::STR, strdup(s));
    }
    if (x.tag() == symtab.file_sym().f) {
      const char *s = srcabs.c_str();
      return new expr(EXPR::STR, strdup(s));
    }
    if (x.tag() == symtab.namespace_sym().f) {
      const char *s = symtab.current_namespace?
	symtab.current_namespace->c_str():"";
      return new expr(EXPR::STR, strdup(s));
    }
    if (x.tag() == symtab.locals_sym().f) {
      // substitute calls to the '__locals__' builtin
      set<int32_t> done;
      exprl xs;
      for (envstack::iterator it = estk.begin(), end = estk.end();
	   it != end; ++it) {
	uint8_t jdx = it->idx;
	for (env::const_iterator jt = it->e->begin();
	     jt != it->e->end(); ++jt) {
	  int32_t g = jt->first;
	  // make sure that we exclude the host function itself from the
	  // environment, to prevent infinite recursion if the function
	  // happens to be a local parameterless function (a common idiom)
	  if (done.find(g) == done.end() && (g != h || jdx != 0)) {
	    expr y = expr(EXPR::FVAR, g, idx-jdx);
	    expr x = expr(interpreter::g_interp->symtab.mapsto_sym().x,
			  expr(g), y);
	    xs.push_back(x);
	    done.insert(g);
	  }
	}
      }
      return new expr(expr::list(xs));
    }
    if (x.is_app(u, v)) {
      if (u.tag() == symtab.list_sym().f) {
	exprl xs;
	if (v.is_pair() && v.is_tuplel(xs))
	  return new expr(expr::list(xs));
	else
	  return new expr(expr::cons(v, expr::nil()));
      }
      if (u.tag() == symtab.eval_sym().f)
	return new expr(maceval(h, trace, v, estk, idx));
    }
  }
  int32_t f = get2args(x, u, v);
  if (f == symtab.lambda_sym().f) {
    exprl xs;
    if (u.is_list(xs) && !xs.empty()) {
      exprl *ys = new exprl;
      for (exprl::iterator it = xs.begin(), end = xs.end(); it!=end; ++it)
	ys->push_back(tagsubst(*it));
      try {
	expr *y = mklambda_expr(ys, new expr(varsubst(v, 1)));
	return y;
      } catch (err &e) {
	// fail
      }
    }
  } else if (f == symtab.case_sym().f) {
    rulel *r = new rulel; exprl xs;
    if (v.is_list(xs) && !xs.empty() && parse_rulel(xs, *r)) {
      try {
	expr *y = mkcase_expr(new expr(u), r);
	return y;
      } catch (err &e) {
	// fail
      }
    } else
      delete r;
  } else if (f == symtab.when_sym().f) {
    rulel *r = new rulel; exprl xs;
    int offs = 0;
    if (v.is_list(xs) && !xs.empty() && parse_simple_rulel(xs, *r, offs)) {
      try {
	expr *y = mkwhen_expr(new expr((offs==0)?u:varsubst(u, offs)), r);
	return y;
      } catch (err &e) {
	// fail
      }
    } else
      delete r;
  } else if (f == symtab.with_sym().f) {
    env *e = new env; exprl xs;
    if (v.is_list(xs) && !xs.empty() && parse_env(xs, *e)) {
      try {
	expr *y = mkwith_expr(new expr(u), e);
	return y;
      } catch (err &e) {
	// fail
      }
    } else
      delete e;
  } else if (get3args(x, u, v, w) == symtab.ifelse_sym().f) {
    return new expr(expr::cond(u, v, w));
  }
  return 0;
}

static string pname(interpreter& interp, int32_t tag)
{
  if (tag > 0) {
    ostringstream sout;
    sout << interp.symtab.sym(tag).x;
    return sout.str();
  } else
    return "#<closure>";
}

expr interpreter::macval(int32_t h, bool trace, expr x,
			 envstack& estk, uint8_t idx)
{
  char test;
  if (x.is_null()) return x;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax)
    throw err("recursion too deep in macro expansion");
  int32_t f; uint32_t argc = count_args(x, f);
  if (f <= 0) return x;
  // Evaluate built-in macros for the specials.
  expr *z = macspecial(h, trace, x, estk, idx);
  if (z) {
    expr y = *z; delete z;
    if (trace || mac_tracepoints.find(f) != mac_tracepoints.end())
      std::cout << "-- macro " << pname(*this, f) << ": "
		<< x << " --> " << y << '\n';
    return y;
  }
  if (specials_only) return x;
  env::iterator it = macenv.find(f);
  if (it == macenv.end()) return x;
  env_info &info = it->second;
  if (argc != info.argc) return x;
  if (!info.m)
    info.m = new matcher(*info.rules, info.argc+1);
  assert(info.m);
  bool quote = quoteargs.find(f) != quoteargs.end();
  exprl args = get_macargs(x, quote);
  assert(args.size() == argc);
  state *st = info.m->match(args);
  if (st) {
    assert(!st->r.empty());
    // We have to rebuild the macro call here, as specials in the arguments
    // may have been expanded to their quoted representations.
    expr x1 = expr(f);
    for (exprl::iterator it = args.begin(), end = args.end(); it!=end; ++it)
      x1 = expr(x1, *it);
    for (ruleml::iterator rp = st->r.begin(); rp != st->r.end(); ++rp) {
      rule& r = info.m->r[*rp];
      if (checkguards(x1, r.vi.guards) && checkeqns(x1, r.vi.eqns)) {
	expr y = macred(x1, r.rhs);
	if (trace || mac_tracepoints.find(f) != mac_tracepoints.end())
	  std::cout << "-- macro " << pname(*this, f) << ": "
		    << x << " --> " << y << '\n';
	return macsubst(h, trace, y, estk, idx);
      }
    }
  }
  return x;
}

expr interpreter::maceval(int32_t h, bool trace,
			  expr x, envstack& estk, uint8_t idx)
{
  if (x.is_null()) return x;
  switch (x.tag()) {
  // matrix (Pure 0.47+):
  case EXPR::MATRIX: {
    exprll *us = new exprll;
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++) {
      us->push_back(exprl());
      exprl& vs = us->back();
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	vs.push_back(maceval(h, trace, *ys, estk, idx));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP: {
    if (is_quote(x.xval1().tag()))
      return macsubst(h, trace, x.xval2(), estk, idx);
    else if (x.xval1().tag() == symtab.amp_sym().f) {
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = maceval(0, trace, x.xval2(), estk, idx);
      return expr(symtab.amp_sym().x, v);
    } else if (x.xval1().tag() == EXPR::APP &&
	       x.xval1().xval1().tag() == symtab.catch_sym().f) {
      expr u = maceval(h, trace, x.xval1().xval2(), estk, idx);
      if (++idx == 0)
	throw err("error in expression (too many nested closures)");
      expr v = maceval(0, trace, x.xval2(), estk, idx);
      return expr(symtab.catch_sym().x, u, v);
    } else {
      expr u = maceval(h, trace, x.xval1(), estk, idx),
	v = maceval(h, trace, x.xval2(), estk, idx);
      return expr(u, v);
    }
  }
  default:
    return x;
  }
}

expr interpreter::macsval(pure_expr *x)
{
  specials_only = true;
  expr u = pure_expr_to_expr(x);
  expr v = macsubst(0, u);
  specials_only = false;
  return v;
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

static int32_t builtin_type_tag(symtable& symtab, int32_t tag)
{
  if (tag == symtab.int_sym().f)
    return EXPR::INT;
  else if (tag == symtab.bigint_sym().f)
    return EXPR::BIGINT;
  else if (tag == symtab.double_sym().f)
    return EXPR::DBL;
  else if (tag == symtab.string_sym().f)
    return EXPR::STR;
  else if (tag == symtab.pointer_sym().f)
    return EXPR::PTR;
  else if (tag == symtab.matrix_sym().f)
    return EXPR::MATRIX;
  else
    return 0;
}

int32_t interpreter::resolve_type_tag(int32_t tag)
{
  if (tag <= 0 || !folding) return tag;
  env::iterator e = typeenv.find(tag);
  int32_t ftag = tag;
  while (e != typeenv.end() && e->second.t != env_info::none &&
	 e->second.argc == 0 && e->second.rules &&
	 e->second.rules->size() == 1) {
    const rule& r = e->second.rules->front();
    tag = ftag; ftag = r.rhs.tag();
    if (!r.qual.is_null() || ftag <= 0) break;
    e = typeenv.find(ftag);
  }
  if (ftag != tag && ftag > 0 &&
      (ftag = builtin_type_tag(symtab, ftag)))
    // built-in type
    tag = ftag;
  return tag;
}

expr *interpreter::mksym_expr(string *s, int32_t tag)
{
  expr *x;
  const symbol &sym = symtab.checksym(*s);
  env::const_iterator it;
  if (tag == 0) {
    if (*s == "_")
      // Return a new instance here, since the anonymous variable may have
      // multiple occurrences on the lhs.
      x = new expr(symtab.anon_sym);
    else if (s->find("::") != string::npos) {
      // Return a new qualified instance here, so that we don't mistake this
      // for a lhs variable.
      x = new expr(sym.f);
      x->flags() |= EXPR::QUAL;
    } else {
#if 1
      /* TO BE REVIEWED: This used to be a cached function node, but this
	 might clobber the expression flags if the same function symbol is
	 used in different contexts (e.g., as both a global and a local
	 symbol, or in different local environments). */
      x = new expr(sym.f);
#else
      x = new expr(sym.x);
#endif
    }
  } else if (sym.f <= 0 || sym.prec < PREC_MAX ||
	     sym.fix == nonfix || sym.fix == outfix) {
    throw err("error in expression (misplaced "+ttag_msg(tag)+")");
  } else {
    x = new expr(sym.f);
    if (s->find("::") != string::npos)
      x->flags() |= EXPR::QUAL;
    // record type tag:
    x->set_ttag(resolve_type_tag(tag));
  }
  delete s;
  return x;
}

expr *interpreter::mkas_expr(string *s, expr *x)
{
  const symbol &sym = symtab.checksym(*s);
  if (sym.f <= 0 || sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix)
    throw err("error in  \"as\" pattern (bad variable symbol)");
#if 0
  if (x->tag() > 0) {
    // Avoid clobbering cached function symbols.
    // FIXME: Is this needed any more?
    expr *y = new expr(x->tag());
    delete x;
    x = y;
  }
#endif
  x->set_astag(sym.f);
  if (s->find("::") != string::npos)
    x->flags() |= EXPR::ASQUAL;
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
	if (x.tag() == symtab.minus_sym().f) {
	  expr op = expr(symtab.neg_sym().f);
	  op.flags() = x.flags() & (EXPR::QUAL|EXPR::GLOBAL|EXPR::LOCAL);
	  y = uminop(op, y);
	} else
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
  if (out.stk.size() > 1) {
    /* Unprocessed operands remain, most likely this indicates missing
       parentheses around a postfix op inside an application. We treat this as
       an error. */
    ostringstream msg;
    msg << "syntax error, missing parentheses around '"
	<< out.stk.front().x << "'";
    throw err(msg.str());
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

expr interpreter::lambda_expr(exprl *args, expr body)
{
  try {
    expr x(expr::lambda(args, body));
    closure(x.lrule());
    // rebuild the argument list
    size_t n = args->size();
    args->clear();
    for (expr y = x.lrule().lhs; n>0; y = y.xval1(), n--)
      args->push_front(y.xval2());
    return x;
  } catch (err &e) {
    throw e;
    return expr(); // not reached
  }
}

expr *interpreter::mklambda_expr(exprl *args, expr *body)
{
  expr *x;
  try {
    x = new expr(lambda_expr(args, *body));
  } catch (err &e) {
    delete body;
    throw e;
  }
  delete body;
  return x;
}

expr *interpreter::mkcase_expr(expr *x, rulel *r)
{
  expr *u;
  if (r->empty()) {
    u = x; delete r;
  } else {
    u = new expr(expr::cases(rsubst(*x), r));
    delete x;
  }
  return u;
}

expr *interpreter::mkwhen_expr(expr *x, rulel *r)
{
  if (r->empty()) {
    delete r;
    return x;
  }
  expr u = rsubst(*x);
  delete x;
  // x when l1 = r1; ...; lk = rk end  ===
  // case r1 of l1 = ... case rk of lk = x end ... end
  if (r->size() > 0x100) {
    delete r;
    throw err("error in expression (too many nested closures)");
  }
  rulel *s = new rulel;
  uint8_t idx = 0;
  for (rulel::reverse_iterator it = r->rbegin(); it != r->rend(); ++it) {
    env vars; vinfo vi;
    expr v = bind(vars, vi, lcsubst(it->lhs)), w = rsubst(it->rhs);
    u = subst(vars, u, idx);
    uint8_t jdx = 0;
    for (rulel::iterator jt = s->begin(); jt != s->end(); ++jt) {
      expr v = jt->lhs, w = subst(vars, jt->rhs, jdx);
      *jt = rule(v, w, jt->vi);
      // skip anonymous bindings, these are eventually eliminated
      if (!(v.is_var() && v.vtag() == symtab.anon_sym && v.ttag() == 0))
	++jdx;
    }
    s->push_front(rule(v, w, vi));
    // skip anonymous bindings, these are eventually eliminated
    if (!(v.is_var() && v.vtag() == symtab.anon_sym && v.ttag() == 0))
      ++idx;
  }
  delete r;
  return new expr(expr::when(u, s));
}

expr *interpreter::mkwith_expr(expr *x, env *e)
{
  if (e->empty()) {
    delete e;
    return x;
  } else {
    expr v = fsubst(*e, rsubst(*x));
    delete x;
    for (env::iterator it = e->begin(); it != e->end(); ++it) {
      env_info& info = it->second;
      rulel *r = info.rules;
      for (rulel::iterator jt = r->begin(); jt != r->end(); ++jt) {
	expr rhs = fsubst(*e, jt->rhs, 1), qual = fsubst(*e, jt->qual, 1);
	*jt = rule(jt->lhs, rhs, jt->vi, qual);
      }
    }
    return new expr(expr::with(v, e));
  }
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

expr interpreter::mkpat_expr(expr x, expr y1, expr y2, expr z, bool& ispat)
{
  // Check whether we have a nontrivial pattern x which must be filtered. The
  // lambda body is y1, y2 is a wrapped version of y1 (either y1 itself or
  // [y1] in the listmap case), z is the null aggregate ([] or {}) which is to
  // be returned in case of a failed match.
  ispat = x.tag() <= 0 || (x.flags()&EXPR::QUAL);
  if (!ispat) {
    const symbol& sym = symtab.sym(x.tag());
    ispat = sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix;
  }
  if (ispat) {
    // We have a nontrivial pattern here which must be filtered. We translate
    // this to \u -> case u of x = y2; _ = z end, where u is a fresh variable.
    expr u = gensym_expr('v');
    rule r1(x, y2), r2(expr(symtab.anon_sym), z);
    rulel *rl = new rulel;
    add_rule(*rl, r1, true);
    add_rule(*rl, r2, true);
    expr v = expr::cases(u, rl);
    return lambda_expr(new exprl(1, u), v);
  } else
    // Plain variable, nothing special to do.
    return lambda_expr(new exprl(1, x), y1);
}

expr interpreter::gensym_expr(char name)
{
  // generate a new unqualified symbol in the default namespace
  static unsigned count[256];
  char s[20];
  while (1) {
    sprintf(s, "__%c%u__", name, ++count[(int)name]);
    // If the next symbol already exists or cannot be created for some
    // reason, we simply keep on incrementing the counter until we find
    // a good one. This must eventually succeed.
    if (!symtab.lookup(s)) {
      try {
	const symbol &sym = symtab.checksym(s);
	return expr(sym.f);
      } catch (err &e) { }
    }
  }
  assert(0 && "this can't happen");
  return expr();
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
      bool ispat;
      expr pat = c.first, arg = c.second,
	y = mkpat_expr(pat, x, expr(expr::cons(x, expr::nil())),
		       expr::nil(), ispat);
      return
	expr(ispat?symtab.catmap_sym().x:symtab.listmap_sym().x, y, arg);
    } else {
      bool ispat;
      expr pat = c.first, body = mklistcomp_expr(x, next_cs, end),
	arg = c.second, y = mkpat_expr(pat, body, body, expr::nil(), ispat);
      return expr(symtab.catmap_sym().x, y, arg);
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
    expr z(EXPR::MATRIX, new exprll);
    if (c.second.is_null()) {
      expr p = c.first;
      return expr::cond(p, mkmatcomp_expr(x, n, next_cs, end), z);
    } else if (next_cs == end) {
      bool ispat;
      exprll *xs = new exprll(1, exprl(1, x));
      expr u(EXPR::MATRIX, xs);
      expr pat = c.first, arg = c.second, y = mkpat_expr(pat, x, u, z, ispat);
      expr f = (n&1)?symtab.colmap_sym().x:symtab.rowmap_sym().x;
      expr g = (n&1)?symtab.colcatmap_sym().x:symtab.rowcatmap_sym().x;
      return expr(ispat?g:f, y, arg);
    } else {
      bool ispat;
      expr pat = c.first, body = mkmatcomp_expr(x, n-1, next_cs, end),
	arg = c.second, y = mkpat_expr(pat, body, body, z, ispat);
      expr f = (n&1)?symtab.colcatmap_sym().x:symtab.rowcatmap_sym().x;
      return expr(f, y, arg);
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

expr interpreter::quoted_ifelse(expr x, expr y, expr z)
{
  return expr(symtab.ifelse_sym().x, x, y, z);
}

expr interpreter::quoted_if(expr x, expr y)
{
  return expr(symtab.if_sym().x, x, y);
}

expr interpreter::quoted_lambda(exprl *args, expr rhs)
{
  exprl xs;
  for (exprl::iterator it = args->begin(), end = args->end(); it != end; ++it)
    xs.push_back(vsubst(*it));
  return expr(symtab.lambda_sym().x, expr::list(xs), vsubst(rhs, 1));
}

expr interpreter::quoted_case(expr x, rulel *rules)
{
  return expr(symtab.case_sym().x, x, quoted_rules(rules));
}

expr interpreter::quoted_when(expr x, rulel *rules)
{
  assert(!rules->empty());
  int offs = 0;
  expr xs = quoted_simple_rules(rules, offs);
  return expr(symtab.when_sym().x, (offs==0)?x:vsubst(x, offs), xs);
}

expr interpreter::quoted_with(expr x, env *defs)
{
  assert(!defs->empty());
  return expr(symtab.with_sym().x, vsubst(x, 0, 1), quoted_env(defs));
}

expr interpreter::quoted_rules(rulel *rules)
{
  exprl xs;
  for (rulel::iterator it = rules->begin(), end = rules->end(); it!=end; ++it)
    if (it->qual.is_null())
      xs.push_back(expr(symtab.eqn_sym().x, vsubst(it->lhs),
			vsubst(it->rhs, 1)));
    else
      xs.push_back(expr(symtab.eqn_sym().x, vsubst(it->lhs),
			expr(symtab.if_sym().x, vsubst(it->rhs, 1),
			     vsubst(it->qual, 1))));
  return expr::list(xs);
}

expr interpreter::quoted_simple_rules(rulel *rules, int& offs)
{
  exprl xs;
  offs = 0;
  for (rulel::iterator it = rules->begin(), end = rules->end(); it!=end;
       ++it) {
    assert(it->qual.is_null());
    expr u = it->lhs, v = it->rhs;
    xs.push_back(expr(symtab.eqn_sym().x, vsubst(u),
		      (offs==0)?v:vsubst(v, offs)));
    if (u.is_var() && u.vtag() == symtab.anon_sym && u.ttag() == 0)
      // anonymous binding, gets thrown away
      ;
    else
      offs++;
  }
  return expr::list(xs);
}

expr interpreter::quoted_env(env *defs)
{
  exprl xs;
  for (env::iterator it = defs->begin(), end = defs->end(); it!=end; ++it) {
    env_info& info = it->second;
    assert(info.t == env_info::fun && it->first>0);
    for (rulel::iterator jt = info.rules->begin(), end = info.rules->end();
	 jt!=end; ++jt)
      if (jt->qual.is_null())
	xs.push_back(expr(symtab.eqn_sym().x, vsubst(jt->lhs),
			  vsubst(jt->rhs, 1, 2)));
      else
	xs.push_back(expr(symtab.eqn_sym().x, vsubst(jt->lhs),
			  expr(symtab.if_sym().x, vsubst(jt->rhs, 1, 2),
			       vsubst(jt->qual, 1, 2))));
  }
  return expr::list(xs);
}

expr interpreter::quoted_tag(expr x, int32_t astag, int32_t ttag)
{
  expr u, v;
  if (ttag != 0)
    u = expr(symtab.ttag_sym().x, x, symtab.sym(ttag_sym(ttag)).x);
  else
    u = x;
  if (astag != 0)
    v = expr(symtab.astag_sym().x, symtab.sym(astag).x, u);
  else
    v = u;
  return v;
}

pure_expr *interpreter::fun_rules(int32_t f)
{
  env::iterator jt = globenv.find(f);
  list<pure_expr*> xs;
  if (jt != globenv.end() && jt->second.t == env_info::fun) {
    env_info& info = jt->second;
    for (rulel::iterator it = info.rules->begin(), end = info.rules->end();
	 it!=end; ++it)
      if (it->qual.is_null()) {
	expr x = expr(symtab.eqn_sym().x, vsubst(it->lhs),
		      rsubst(vsubst(it->rhs, 1), true));
	xs.push_back(const_value(x, true));
      } else {
	expr x = expr(symtab.eqn_sym().x, vsubst(it->lhs),
		      expr(symtab.if_sym().x,
			   rsubst(vsubst(it->rhs, 1), true),
			   rsubst(vsubst(it->qual, 1), true)));
	xs.push_back(const_value(x, true));
      }
  }
  size_t n = xs.size();
  pure_expr **xv = new pure_expr*[n];
  list<pure_expr*>::iterator x = xs.begin(), end = xs.end();
  for (size_t i = 0; x != end; ++x) xv[i++] = *x;
  pure_expr *y = pure_listv(n, xv);
  delete[] xv;
  return y;
}

pure_expr *interpreter::type_rules(int32_t f)
{
  env::iterator jt = typeenv.find(f);
  list<pure_expr*> xs;
  if (jt != typeenv.end() && jt->second.t == env_info::fun) {
    env_info& info = jt->second;
    for (rulel::iterator it = info.rules->begin(), end = info.rules->end();
	 it!=end; ++it)
      if (it->qual.is_null()) {
	expr x = expr(symtab.eqn_sym().x, vsubst(it->lhs),
		      rsubst(vsubst(it->rhs, 1), true));
	xs.push_back(const_value(x, true));
      } else {
	expr x = expr(symtab.eqn_sym().x, vsubst(it->lhs),
		      expr(symtab.if_sym().x,
			   rsubst(vsubst(it->rhs, 1), true),
			   rsubst(vsubst(it->qual, 1), true)));
	xs.push_back(const_value(x, true));
      }
  }
  size_t n = xs.size();
  pure_expr **xv = new pure_expr*[n];
  list<pure_expr*>::iterator x = xs.begin(), end = xs.end();
  for (size_t i = 0; x != end; ++x) xv[i++] = *x;
  pure_expr *y = pure_listv(n, xv);
  delete[] xv;
  return y;
}

pure_expr *interpreter::interface_patterns(int32_t f)
{
  env::iterator jt = typeenv.find(f);
  list<pure_expr*> xs;
  if (jt != typeenv.end() && jt->second.t == env_info::fun &&
      jt->second.xs) {
    env_info& info = jt->second;
    for (exprl::iterator it = info.xs->begin(), end = info.xs->end();
	 it!=end; ++it) {
      expr x = vsubst(*it);
      xs.push_back(const_value(x, true));
    }
  }
  size_t n = xs.size();
  pure_expr **xv = new pure_expr*[n];
  list<pure_expr*>::iterator x = xs.begin(), end = xs.end();
  for (size_t i = 0; x != end; ++x) xv[i++] = *x;
  pure_expr *y = pure_listv(n, xv);
  delete[] xv;
  return y;
}

pure_expr *interpreter::interface_rules(int32_t f)
{
  env::iterator jt = typeenv.find(f);
  list<pure_expr*> xs;
  if (jt != typeenv.end() && jt->second.t == env_info::fun &&
      jt->second.xs && jt->second.rxs) {
    env_info& info = jt->second;
    for (rulel::iterator it = info.rxs->begin(), end = info.rxs->end();
	 it!=end; ++it) {
      expr x = expr(symtab.eqn_sym().x, vsubst(it->lhs),
		    rsubst(vsubst(it->rhs, 1), true));
      xs.push_back(const_value(x, true));
    }
  }
  size_t n = xs.size();
  pure_expr **xv = new pure_expr*[n];
  list<pure_expr*>::iterator x = xs.begin(), end = xs.end();
  for (size_t i = 0; x != end; ++x) xv[i++] = *x;
  pure_expr *y = pure_listv(n, xv);
  delete[] xv;
  return y;
}

pure_expr *interpreter::mac_rules(int32_t f)
{
  env::iterator jt = macenv.find(f);
  list<pure_expr*> xs;
  if (jt != macenv.end() && jt->second.t == env_info::fun) {
    env_info& info = jt->second;
    for (rulel::iterator it = info.rules->begin(), end = info.rules->end();
	 it!=end; ++it) {
      assert(it->qual.is_null());
      expr x = expr(symtab.eqn_sym().x, vsubst(it->lhs),
		    rsubst(vsubst(it->rhs, 1), true));
      xs.push_back(const_value(x, true));
    }
  }
  size_t n = xs.size();
  pure_expr **xv = new pure_expr*[n];
  list<pure_expr*>::iterator x = xs.begin(), end = xs.end();
  for (size_t i = 0; x != end; ++x) xv[i++] = *x;
  pure_expr *y = pure_listv(n, xv);
  delete[] xv;
  return y;
}

bool interpreter::add_fun_rules(pure_expr *y)
{
  expr x = pure_expr_to_expr(y);
  exprl xs;
  errmsg.clear(); errpos.clear();
  if (!x.is_list(xs)) return false;
  for (exprl::iterator x = xs.begin(), end = xs.end(); x!=end; x++) {
    expr u, v;
    if (get2args(*x, u, v) == symtab.eqn_sym().f) {
      expr w, c;
      try {
	if (restricted) throw err("operation not implemented");
	if (get2args(v, w, c) == symtab.if_sym().f) {
	  rule r(tagsubst(u), w, c);
	  add_rule(globenv, r, true, false);
	} else {
	  rule r(tagsubst(u), ifsubst(v));
	  add_rule(globenv, r, true, false);
	}
      } catch (err &e) {
	errmsg = e.what() + "\n"; errpos.clear(); errpos.push_back(errmsg);
	return false;
      }
    } else
      return false;
  }
  return true;
}

bool interpreter::add_type_rules(pure_expr *y)
{
  expr x = pure_expr_to_expr(y);
  exprl xs;
  errmsg.clear(); errpos.clear();
  if (!x.is_list(xs)) return false;
  for (exprl::iterator x = xs.begin(), end = xs.end(); x!=end; x++) {
    expr u, v;
    if (get2args(*x, u, v) == symtab.eqn_sym().f) {
      expr w, c;
      try {
	if (restricted) throw err("operation not implemented");
	if (get2args(v, w, c) == symtab.if_sym().f) {
	  rule r(tagsubst(u), w, c);
	  add_type_rule(typeenv, r, false);
	} else {
	  rule r(tagsubst(u), ifsubst(v));
	  add_type_rule(typeenv, r, false);
	}
      } catch (err &e) {
	errmsg = e.what() + "\n"; errpos.clear(); errpos.push_back(errmsg);
	return false;
      }
    } else {
      try {
	if (restricted) throw err("operation not implemented");
	rule r(tagsubst(*x), expr(EXPR::INT, 1));
	add_type_rule(typeenv, r, false);
      } catch (err &e) {
	errmsg = e.what() + "\n"; errpos.clear(); errpos.push_back(errmsg);
	return false;
      }
    }
  }
  return true;
}

bool interpreter::add_interface_rules(int32_t tag, pure_expr *y)
{
  expr x = pure_expr_to_expr(y);
  exprl xs;
  errmsg.clear(); errpos.clear();
  if (!x.is_list(xs)) return false;
  bool res = true;
  size_t count = 0;
  for (exprl::iterator x = xs.begin(), end = xs.end(); x!=end; x++) {
    try {
      if (restricted) throw err("operation not implemented");
      expr u(tagsubst(*x));
      add_interface_rule(typeenv, tag, u, false); count++;
    } catch (err &e) {
      errmsg = e.what() + "\n"; errpos.clear(); errpos.push_back(errmsg);
      res = false; break;
    }
  }
  finalize_interface_rules(typeenv, tag, count);
  return res;
}

bool interpreter::add_mac_rules(pure_expr *y)
{
  expr x = pure_expr_to_expr(y);
  exprl xs;
  errmsg.clear(); errpos.clear();
  if (!x.is_list(xs)) return false;
  for (exprl::iterator x = xs.begin(), end = xs.end(); x!=end; x++) {
    expr u, v;
    if (get2args(*x, u, v) == symtab.eqn_sym().f) {
      try {
	if (restricted) throw err("operation not implemented");
	rule r(tagsubst(u), macsubst(0, rsubst(v)));
	add_macro_rule(r, false);
      } catch (err &e) {
	errmsg = e.what() + "\n"; errpos.clear(); errpos.push_back(errmsg);
	return false;
      }
    } else
      return false;
  }
  return true;
}

bool interpreter::add_fun_rules_at(pure_expr *u, pure_expr *y)
{
  // find the rule to insert at
  rulel::iterator p;
  pure_expr *gx;
  size_t n;
  if (pure_is_appv(u, &gx, &n, 0) && n == 2 && gx->tag == symtab.eqn_sym().f) {
    pure_expr **xv;
    (void)pure_is_appv(u, &gx, &n, &xv);
    gx = xv[0];
    while (gx->tag == EXPR::APP) gx = gx->data.x[0];
    if (gx->tag <= 0) return false;
  } else
    return false;
  int32_t g = gx->tag;
  env::iterator jt = globenv.find(g);
  if (jt != globenv.end() && jt->second.t == env_info::fun) {
    env_info& info = jt->second;
    rulel& r = *info.rules;
    p = r.end();
    for (rulel::iterator it = r.begin(), end = r.end(); it!=end; ++it) {
      expr x = it->qual.is_null()
	? expr(symtab.eqn_sym().x, vsubst(it->lhs),
	       rsubst(vsubst(it->rhs, 1), true))
	: expr(symtab.eqn_sym().x, vsubst(it->lhs),
	       expr(symtab.if_sym().x,
		    rsubst(vsubst(it->rhs, 1), true),
		    rsubst(vsubst(it->qual, 1), true)));
      pure_expr *v = const_value(x, true);
      bool eq = same(u, v);
      pure_freenew(v);
      if (eq) {
	p = it;
	break;
      }
    }
    if (p == r.end()) return false;
  } else
    return false;
  // found the rule, insert other rules
  expr x = pure_expr_to_expr(y);
  exprl xs;
  errmsg.clear(); errpos.clear();
  if (!x.is_list(xs)) return false;
  for (exprl::iterator x = xs.begin(), end = xs.end(); x!=end; x++) {
    expr u, v;
    if (get2args(*x, u, v) == symtab.eqn_sym().f) {
      expr w, c;
      try {
	if (restricted) throw err("operation not implemented");
	if (get2args(v, w, c) == symtab.if_sym().f) {
	  rule r(tagsubst(u), w, c);
	  add_rule_at(globenv, r, g, p);
	} else {
	  rule r(tagsubst(u), ifsubst(v));
	  add_rule_at(globenv, r, g, p);
	}
      } catch (err &e) {
	errmsg = e.what() + "\n"; errpos.clear(); errpos.push_back(errmsg);
	return false;
      }
    } else
      return false;
  }
  return true;
}

bool interpreter::add_type_rules_at(pure_expr *u, pure_expr *y)
{
  // find the rule to insert at
  rulel::iterator p;
  pure_expr *gx;
  size_t n;
  if (pure_is_appv(u, &gx, &n, 0) && n == 2 && gx->tag == symtab.eqn_sym().f) {
    pure_expr **xv;
    (void)pure_is_appv(u, &gx, &n, &xv);
    gx = xv[0];
    while (gx->tag == EXPR::APP) gx = gx->data.x[0];
    if (gx->tag <= 0) return false;
  } else
    return false;
  int32_t g = gx->tag;
  env::iterator jt = typeenv.find(g);
  if (jt != typeenv.end() && jt->second.t == env_info::fun) {
    env_info& info = jt->second;
    rulel& r = *info.rules;
    p = r.end();
    for (rulel::iterator it = r.begin(), end = r.end(); it!=end; ++it) {
      expr x = it->qual.is_null()
	? expr(symtab.eqn_sym().x, vsubst(it->lhs),
			   rsubst(vsubst(it->rhs, 1), true))
	: expr(symtab.eqn_sym().x, vsubst(it->lhs),
	       expr(symtab.if_sym().x,
		    rsubst(vsubst(it->rhs, 1), true),
		    rsubst(vsubst(it->qual, 1), true)));
      pure_expr *v = const_value(x, true);
      bool eq = same(u, v);
      pure_freenew(v);
      if (eq) {
	p = it;
	break;
      }
    }
    if (p == r.end()) return false;
  } else
    return false;
  // found the rule, insert other rules
  expr x = pure_expr_to_expr(y);
  exprl xs;
  errmsg.clear(); errpos.clear();
  if (!x.is_list(xs)) return false;
  for (exprl::iterator x = xs.begin(), end = xs.end(); x!=end; x++) {
    expr u, v;
    if (get2args(*x, u, v) == symtab.eqn_sym().f) {
      expr w, c;
      try {
	if (restricted) throw err("operation not implemented");
	if (get2args(v, w, c) == symtab.if_sym().f) {
	  rule r(tagsubst(u), w, c);
	  add_type_rule_at(typeenv, r, g, p);
	} else {
	  rule r(tagsubst(u), ifsubst(v));
	  add_type_rule_at(typeenv, r, g, p);
	}
      } catch (err &e) {
	errmsg = e.what() + "\n"; errpos.clear(); errpos.push_back(errmsg);
	return false;
      }
    } else
      return false;
  }
  return true;
}

bool interpreter::add_interface_rules_at(int32_t tag,
					 pure_expr *u, pure_expr *y)
{
  // find the rule to insert at
  exprl::iterator p;
  env::iterator jt = typeenv.find(tag);
  if (jt != typeenv.end() && jt->second.t == env_info::fun && jt->second.xs) {
    env_info& info = jt->second;
    exprl& xs = *info.xs;
    p = xs.end();
    for (exprl::iterator it = xs.begin(), end = xs.end(); it!=end; ++it) {
      expr x = vsubst(*it);
      pure_expr *v = const_value(x, true);
      bool eq = same(u, v);
      pure_freenew(v);
      if (eq) {
	p = it;
	break;
      }
    }
    if (p == xs.end()) return false;
  } else
    return false;
  // found the rule, insert other rules
  expr x = pure_expr_to_expr(y);
  exprl xs;
  errmsg.clear(); errpos.clear();
  if (!x.is_list(xs)) return false;
  bool res = true;
  size_t count = 0;
  for (exprl::iterator x = xs.begin(), end = xs.end(); x!=end; x++) {
    try {
      if (restricted) throw err("operation not implemented");
      expr u(tagsubst(*x));
      add_interface_rule_at(typeenv, tag, u, p); count++;
    } catch (err &e) {
      errmsg = e.what() + "\n"; errpos.clear(); errpos.push_back(errmsg);
      res = false; break;
    }
  }
  finalize_interface_rules(typeenv, tag, count, &p);
  return res;
}

bool interpreter::add_mac_rules_at(pure_expr *u, pure_expr *y)
{
  // find the rule to insert at
  rulel::iterator p;
  pure_expr *gx;
  size_t n;
  if (pure_is_appv(u, &gx, &n, 0) && n == 2 && gx->tag == symtab.eqn_sym().f) {
    pure_expr **xv;
    (void)pure_is_appv(u, &gx, &n, &xv);
    gx = xv[0];
    while (gx->tag == EXPR::APP) gx = gx->data.x[0];
    if (gx->tag <= 0) return false;
  } else
    return false;
  int32_t g = gx->tag;
  env::iterator jt = macenv.find(g);
  if (jt != macenv.end() && jt->second.t == env_info::fun) {
    env_info& info = jt->second;
    rulel& r = *info.rules;
    p = r.end();
    for (rulel::iterator it = r.begin(), end = r.end(); it!=end; ++it) {
      assert(it->qual.is_null());
      expr x = expr(symtab.eqn_sym().x, vsubst(it->lhs),
		    rsubst(vsubst(it->rhs, 1), true));
      pure_expr *v = const_value(x, true);
      bool eq = same(u, v);
      pure_freenew(v);
      if (eq) {
	p = it;
	break;
      }
    }
    if (p == r.end()) return false;
  } else
    return false;
  // found the rule, insert other rules
  expr x = pure_expr_to_expr(y);
  exprl xs;
  errmsg.clear(); errpos.clear();
  if (!x.is_list(xs)) return false;
  for (exprl::iterator x = xs.begin(), end = xs.end(); x!=end; x++) {
    expr u, v;
    if (get2args(*x, u, v) == symtab.eqn_sym().f) {
      expr w, c;
      try {
	if (restricted) throw err("operation not implemented");
	rule r(tagsubst(u), macsubst(0, rsubst(v)));
	add_macro_rule_at(r, g, p);
      } catch (err &e) {
	errmsg = e.what() + "\n"; errpos.clear(); errpos.push_back(errmsg);
	return false;
      }
    } else
      return false;
  }
  return true;
}

bool interpreter::add_var(int32_t sym, pure_expr *x)
{
  errmsg.clear(); errpos.clear();
  if (sym <= 0 || !x) return false;
  try {
    if (restricted) throw err("operation not implemented");
    defn(sym, x);
    return true;
  } catch (err &e) {
    errmsg = e.what() + "\n"; errpos.clear(); errpos.push_back(errmsg);
    return false;
  }
}

bool interpreter::add_const(int32_t sym, pure_expr *x)
{
  errmsg.clear(); errpos.clear();
  if (sym <= 0 || !x) return false;
  try {
    if (restricted) throw err("operation not implemented");
    const_defn(sym, x);
    return true;
  } catch (err &e) {
    errmsg = e.what() + "\n"; errpos.clear(); errpos.push_back(errmsg);
    return false;
  }
}

bool interpreter::del_fun_rule(pure_expr *x)
{
  pure_expr *g;
  size_t n;
  if (pure_is_appv(x, &g, &n, 0) && n == 2 && g->tag == symtab.eqn_sym().f) {
    pure_expr **xv;
    (void)pure_is_appv(x, &g, &n, &xv);
    g = xv[0];
    while (g->tag == EXPR::APP) g = g->data.x[0];
    if (g->tag <= 0) return false;
  } else
    return false;
  int32_t f = g->tag;
  env::iterator jt = globenv.find(f);
  if (jt != globenv.end() && jt->second.t == env_info::fun) {
    env_info& info = jt->second;
    rulel& r = *info.rules;
    for (rulel::iterator it = r.begin(), end = r.end(); it!=end; ++it) {
      expr u = it->qual.is_null()
	? expr(symtab.eqn_sym().x, vsubst(it->lhs),
	       rsubst(vsubst(it->rhs, 1), true))
	: expr(symtab.eqn_sym().x, vsubst(it->lhs),
	       expr(symtab.if_sym().x,
		    rsubst(vsubst(it->rhs, 1), true),
		    rsubst(vsubst(it->qual, 1), true)));
      pure_expr *y = const_value(u, true);
      bool eq = same(x, y);
      pure_freenew(y);
      if (eq) {
	if (r.size() == 1)
	  clear(f);
	else {
	  r.erase(it);
	  assert(!r.empty());
	  mark_dirty(f);
	}
	return true;
      }
    }
  }
  return false;
}

bool interpreter::del_type_rule(pure_expr *x)
{
  pure_expr *g;
  size_t n;
  if (pure_is_appv(x, &g, &n, 0) && n == 2 && g->tag == symtab.eqn_sym().f) {
    pure_expr **xv;
    (void)pure_is_appv(x, &g, &n, &xv);
    g = xv[0];
    while (g->tag == EXPR::APP) g = g->data.x[0];
    if (g->tag <= 0) return false;
  } else
    return false;
  int32_t f = g->tag;
  env::iterator jt = typeenv.find(f);
  if (jt != typeenv.end() && jt->second.t == env_info::fun) {
    env_info& info = jt->second;
    rulel& r = *info.rules;
    for (rulel::iterator it = r.begin(), end = r.end(); it!=end; ++it) {
      expr u = it->qual.is_null()
	? expr(symtab.eqn_sym().x, vsubst(it->lhs),
	       rsubst(vsubst(it->rhs, 1), true))
	: expr(symtab.eqn_sym().x, vsubst(it->lhs),
	       expr(symtab.if_sym().x,
		    rsubst(vsubst(it->rhs, 1), true),
		    rsubst(vsubst(it->qual, 1), true)));
      pure_expr *y = const_value(u, true);
      bool eq = same(x, y);
      pure_freenew(y);
      if (eq) {
	if (r.size() == 1)
	  clear_type(f);
	else {
	  r.erase(it);
	  assert(!r.empty());
	  mark_dirty_type(f);
	}
	return true;
      }
    }
  }
  return false;
}

bool interpreter::del_interface_rule(int32_t tag, pure_expr *x)
{
  env::iterator jt = typeenv.find(tag);
  if (jt != typeenv.end() && jt->second.t == env_info::fun &&
      jt->second.xs) {
    env_info& info = jt->second;
    exprl& xs = *info.xs;
    for (exprl::iterator it = xs.begin(), end = xs.end(); it!=end; ++it) {
      expr u = vsubst(*it);
      pure_expr *y = const_value(u, true);
      bool eq = same(x, y);
      pure_freenew(y);
      if (eq) {
	if (xs.size() == 1 && info.rules->empty())
	  clear_type(tag);
	else {
	  expr fx; count_args(*it, fx);
	  int32_t f = fx.tag();
	  if (info.compat) info.compat->erase(*it);
	  xs.erase(it);
	  for (exprl::iterator it = xs.begin(), end = xs.end(); it!=end; ++it) {
	    expr fx; count_args(*it, fx);
	    int32_t g = fx.tag();
	    if (g == f) {
	      f = 0; break;
	    }
	  }
	  if (f > 0) fun_types[f].erase(tag);
	  if (xs.empty()) {
	    delete info.xs; info.xs = 0;
	  }
	  mark_dirty_type(tag);
	}
	return true;
      }
    }
  }
  return false;
}

bool interpreter::del_mac_rule(pure_expr *x)
{
  pure_expr *g;
  size_t n;
  if (pure_is_appv(x, &g, &n, 0) && n == 2 && g->tag == symtab.eqn_sym().f) {
    pure_expr **xv;
    (void)pure_is_appv(x, &g, &n, &xv);
    g = xv[0];
    while (g->tag == EXPR::APP) g = g->data.x[0];
    if (g->tag <= 0) return false;
  } else
    return false;
  int32_t f = g->tag;
  env::iterator jt = macenv.find(f);
  if (jt != macenv.end() && jt->second.t == env_info::fun) {
    env_info& info = jt->second;
    rulel& r = *info.rules;
    for (rulel::iterator it = r.begin(), end = r.end(); it!=end; ++it) {
      assert(it->qual.is_null());
      expr u = expr(symtab.eqn_sym().x, vsubst(it->lhs),
		    rsubst(vsubst(it->rhs, 1), true));
      pure_expr *y = const_value(u, true);
      bool eq = same(x, y);
      pure_freenew(y);
      if (eq) {
	if (r.size() == 1)
	  clear_mac(f);
	else {
	  r.erase(it);
	  assert(!r.empty());
	}
	return true;
      }
    }
  }
  return false;
}

bool interpreter::del_var(int32_t sym)
{
  if (sym <= 0 || globenv.find(sym) == globenv.end() ||
      globenv[sym].t != env_info::fvar)
    return false;
  clear(sym);
  return true;
}

bool interpreter::del_const(int32_t sym)
{
  if (sym <= 0 || globenv.find(sym) == globenv.end() ||
      globenv[sym].t != env_info::cvar)
    return false;
  clear(sym);
  return true;
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

const char *interpreter::mklabel(const char *name,
				 uint32_t i, uint32_t j, uint32_t k)
{
  char lab[128];
  sprintf(lab, "%s%u.%u.%u", name, i, j, k);
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
#ifdef HAVE_LLVM_TYPESYMBOLTABLE_H
#include <llvm/TypeSymbolTable.h>
#endif

static inline bool is_c_sym(const string& name)
{
  return name=="main" || sys::DynamicLibrary::SearchForAddressOfSymbol(name);
}

static string mkvarsym(const string& name)
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

static inline bool is_type(const string& name)
{
  return name.compare(0, 7, "$$type.") == 0;
}

static inline bool is_faust(const string& name)
{
  return name.compare(0, 8, "$$faust$") == 0;
}

static inline bool is_faust_internal(const string& name)
{
  return name.compare(0, 12, "$$__faust__$") == 0;
}

static inline string faust_basename(const string& name)
{
  if (is_faust(name)) {
    size_t pos = name.rfind('$');
    assert(pos != string::npos);
    return name.substr(pos+1);
  } else
    return name;
}

static inline bool is_faust_var(const string& name)
{
  return name.compare(0, 9, "$$$faust$") == 0;
}

static bool parse_faust_name(const string& name, string& mod, string& fun)
{
  if (name.compare(0, 8, "$$faust$") != 0) return false;
  mod = name.substr(8);
  size_t p = mod.find('$');
  if (p == string::npos) return false;
  fun = mod.substr(p+1);
  mod.erase(p);
  return true;
}

#if 0
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
#endif

#ifdef __MINGW32__
// The Windows shell doesn't understand backslash escapes, but double
// quotes should work.
static string& quote(string& s)
{
  size_t p = 0, q;
  while ((q = s.find_first_of(" \t", p)) != string::npos) {
    string dq = "\"";
    s = dq+s+dq;
    return s;
  }
  return s;
}
#else
static string& quote(string& s)
{
  size_t p = 0, q;
  while ((q = s.find_first_of(" \t", p)) != string::npos) {
    s.insert(q, 1, '\\');
    p = q+2;
  }
  return s;
}
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

#if NEW_OSTREAM34 && LLVM35
#define NEW_OSTREAM35 1
#endif

#if NEW_OSTREAM35 // LLVM 3.5 cosmetic changes
#include <llvm/Support/FileSystem.h>
#define new_raw_fd_ostream(s,binary,msg) new llvm::raw_fd_ostream(s,msg,(binary)?llvm::sys::fs::F_None:llvm::sys::fs::F_Text)
#else
#if NEW_OSTREAM34 // LLVM 3.4 cosmetic changes
#define new_raw_fd_ostream(s,binary,msg) new llvm::raw_fd_ostream(s,msg,(binary)?llvm::sys::fs::F_Binary:llvm::sys::fs::F_None)
#else
#if NEW_OSTREAM // LLVM >= 2.7 takes an enumeration as the last parameter
#define new_raw_fd_ostream(s,binary,msg) new llvm::raw_fd_ostream(s,msg,(binary)?llvm::raw_fd_ostream::F_Binary:0)
#else
#if LLVM26 // LLVM 2.6 takes two flags (Binary, Force)
#define new_raw_fd_ostream(s,binary,msg) new llvm::raw_fd_ostream(s,binary,1,msg)
#else // LLVM 2.5 and earlier only have the Binary flag
#define new_raw_fd_ostream(s,binary,msg) new llvm::raw_fd_ostream(s,binary,msg)
#endif
#endif
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
      for (value_user_iterator it = value_user_begin(v),
	     end = value_user_end(v); it != end; it++) {
	if (Instruction *inst = dyn_cast<Instruction>(*it)) {
	  Function *g = inst->getParent()->getParent();
	  // Indirect reference through a variable. Note that we're not
	  // interested in loops in the dependency graph, so we check that the
	  // caller is different from the callee.
	  if (g && g != f) {
#if 0
	    std::cout << g->getName().str() << " calls " << f->getName().str()
		      << " via var " << v->getName().str() << '\n';
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
      for (value_user_iterator it = value_user_begin(f),
	     end = value_user_end(f); it != end; it++) {
	if (Instruction *inst = dyn_cast<Instruction>(*it)) {
	  Function *g = inst->getParent()->getParent();
	  // This is a direct call.
	  if (g && g != f) {
#if 0
	    std::cout << g->getName().str() << " calls " << f->getName().str() << '\n';
#endif
#if DEBUG_USED||DEBUG_UNUSED
	    callers[f].insert(g);
#endif
	    callees[g].insert(f);
	  }
	} else if (Constant *c = dyn_cast<Constant>(*it)) {
	  // A function pointer. Check its uses.
	  for (value_user_iterator jt = value_user_begin(c),
		 end = value_user_end(c); jt != end; jt++) {
	    if (Instruction *inst = dyn_cast<Instruction>(*jt)) {
	      // This is a function that refers to f via a pointer.
	      Function *g = inst->getParent()->getParent();
	      if (g && g != f) {
#if 0
		std::cout << g->getName().str() << " calls " << f->getName().str()
			  << " via cst " << c->getName().str() << '\n';
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
    std::cout << "** used function: " << f->getName().str() << " callers:";
    for (set<Function*>::iterator jt = callers[f].begin(),
	   end = callers[f].end(); jt != end; jt++) {
      Function *g = *jt;
      std::cout << " " << g->getName().str();
    }
    std::cout << " callees:";
    for (set<Function*>::iterator jt = callees[f].begin(),
	   end = callees[f].end(); jt != end; jt++) {
      Function *g = *jt;
      std::cout << " " << g->getName().str();
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
      std::cout << "** unused function: " << f->getName().str() << " callers:";
      for (set<Function*>::iterator jt = callers[f].begin(),
	     end = callers[f].end(); jt != end; jt++) {
	Function *g = *jt;
	std::cout << " " << g->getName().str();
      }
      std::cout << " callees:";
      for (set<Function*>::iterator jt = callees[f].begin(),
	     end = callees[f].end(); jt != end; jt++) {
	Function *g = *jt;
	std::cout << " " << g->getName().str();
      }
      std::cout << '\n';
    }
  }
#endif
}

int interpreter::compiler(string out, list<string> libnames, string llcopts)
{
  /* We allow either '-' or *.ll to indicate an LLVM assembler file. In the
     former case, output is written to stdout, which is useful if the output
     is to be processed by other LLVM utilities in a pipe. Other recognized
     extensions for the output file are .bc (LLVM bitcode) as well as .s and
     .o (native assembler and object code). In all other cases, output code is
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
  // LLVM tools used in the build process.
  /* We allow the user to install his own custom builds of the llc and opt
     tools into the /usr/lib/pure directory. */
  string llc = (chkfile(libdir+"llc"+EXEEXT)?libdir:tool_prefix)+"llc";
  string opt = (chkfile(libdir+"opt"+EXEEXT)?libdir:tool_prefix)+"opt";
  /* Everything is already compiled at this point, so all we have to do here
     is to emit the code. We also prepare a main entry point, void
     __pure_main__ (int argc, char **argv), which initializes the interpreter
     so that a minimal runtime environment is available. This function is to
     be called by the main() or other initialization code of the standalone
     module. It takes two arguments, the argc and argv of the interpreter.

     Note that, as of Pure 0.50, the name of the main entry point can be
     configured with the --main option. This allows several modules created
     with the batch compiler to coexist in a single program. However, to make
     this work, *all* Pure-related globals are set to internal linkage if this
     option is used. This implies that Pure functions in the module can't be
     called directly any more, but *must* be invoked through the runtime
     (using pure_appl et al). This is necessary to prevent name clashes
     between different batch-compiled modules, but hopefully isn't too much of
     an obstacle in cases where the --main option is needed. */
  setlocale(LC_ALL, "C");
#if RAW_STREAM
  // As of LLVM 2.7 (svn), these need to be wrapped up in a raw_ostream.
  string error;
  // Note: raw_fd_ostream already handles "-".
  llvm::raw_fd_ostream *codep =
    new_raw_fd_ostream(target.c_str(),bc_target,error);
  if (!error.empty()) {
    std::cerr << "Error opening " << target << '\n';
    exit(1);
  }
  llvm::raw_fd_ostream &code = *codep;
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
  if (symtab.__show__sym > 0) {
    // Make sure that we always include the __show__ function if it's defined.
    Function *__show__fun = module->getFunction("__show__");
    if (__show__fun) used.insert(__show__fun);
  }
  // Always keep required functions (--required pragma).
  for (list<int>::const_iterator it = required.begin();
       it != required.end(); ++it) {
    Function *f = 0;
    map<int32_t,Env>::iterator jt = globalfuns.find(*it);
    if (jt != globalfuns.end()) {
      Env& e = jt->second;
      f = e.h;
    } else {
      map<int32_t,ExternInfo>::iterator kt = externals.find(*it);
      if (kt != externals.end()) {
	ExternInfo& info = kt->second;
	f = info.f;
      }
    }
    if (f) used.insert(f);
  }
  // Always keep the type predicates.
  for (map<int32_t,Env>::iterator it = globaltypes.begin(),
	 end = globaltypes.end(); it != end; ++it) {
    Function *f = it->second.h;
    if (f) used.insert(f);
  }
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
    if (!v.isDeclaration() && !mainname.empty())
      v.setLinkage(GlobalVariable::InternalLinkage);
    // While we're at it, also check for variables pointing to Faust functions
    // and update their initializations.
    string name = v.getName();
    if (is_faust_var(name)) {
      Function *f = module->getFunction(name.substr(1));
      assert(f);
      v.setInitializer(f);
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
    /* FIXME: At present, Faust functions are just always included. We might
       want to check their usage, too, but the current algorithm doesn't do
       this, as the wrappers call the Faust functions indirectly through a
       global variable. Declarations of external functions are also always
       included, for similar reasons. */
    if (strip && used.find(&f) == used.end() && !f.isDeclaration() &&
	!is_faust(f.getName()) && !is_faust_internal(f.getName())) {
      f.dropAllReferences();
      fun_to_be_deleted.push_back(&f);
    } else if (!f.isDeclaration() && !mainname.empty())
      f.setLinkage(Function::InternalLinkage);

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
  if (mainname.empty()) mainname = "__pure_main__";
  vector<llvm_const_Type*> argt;
  argt.push_back(int32_type());
  argt.push_back(VoidPtrTy);
  FunctionType *ft = func_type(void_type(), argt, false);
  Function *main = Function::Create(ft, Function::ExternalLinkage,
				    mainname, module);
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
    if (v.v) {
      if (v.x) {
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
      env::const_iterator e = globenv.find(f);
      if (e != globenv.end() && e->second.t == env_info::cvar &&
	  e->second.cval_v) {
	/* This is a cached constant aggregate. Earlier definitions might
	   still contain dangling references to this symbol, however, so we
	   also need to add the shadowed symbol to the vars table. */
	GlobalVariable *v = (GlobalVariable*)e->second.cval_v;
	vars[f] = v;
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
  args.push_back(b.CreateGEP(syms, mkidxs(idx, idx+2)));
  args.push_back(b.CreateBitCast(b.CreateGEP(vvars, mkidxs(idx, idx+2)),
				 VoidPtrTy));
  args.push_back(b.CreateBitCast(b.CreateGEP(vvals, mkidxs(idx, idx+2)),
				 VoidPtrTy));
  args.push_back(b.CreateGEP(varity, mkidxs(idx, idx+2)));
  args.push_back(b.CreateBitCast(b.CreateGEP(vexterns, mkidxs(idx, idx+2)),
				 VoidPtrTy));
  args.push_back(b.CreateBitCast(sstkvar, VoidPtrTy));
  args.push_back(b.CreateBitCast(fptrvar, VoidPtrTy));
  b.CreateCall(initfun, mkargs(args));
  // Initialize runtime type tag information.
  Function *pure_rttyfun = module->getFunction("pure_add_rtty");
  for (map<int32_t,Env>::iterator it = globaltypes.begin(),
	 end = globaltypes.end(); it != end; ++it) {
    int32_t tag = it->first;
    Function *f = it->second.h;
    int argc = it->second.n;
    vector<Value*> argv(3);
    argv[0] = SInt(tag);
    argv[1] = SInt(argc);
    argv[2] = ConstantExpr::getPointerCast(f, VoidPtrTy);
    b.CreateCall(pure_rttyfun, mkargs(argv));
  }
  // Make Pure pointer RTTI available if present.
  Function *pure_rttifun = module->getFunction("pure_add_rtti");
  for (map<string,int>::const_iterator it = pointer_tags.begin(),
	 end = pointer_tags.end(); it != end; ++it) {
    const string& name = it->first;
    const int tag = it->second;
    vector<Value*> argv(2);
    GlobalVariable *v = global_variable
      (module, ArrayType::get(int8_type(), name.size()+1), true,
       GlobalVariable::InternalLinkage, constant_char_array(name.c_str()),
       "$$str");
    // "cast" the char array to a char*
    Value *idx[2] = { Zero, Zero };
    Value *p = b.CreateGEP(v, mkidxs(idx, idx+2));
    argv[0] = p;
    argv[1] = SInt(tag);
    b.CreateCall(pure_rttifun, mkargs(argv));
  }
  // Make Faust RTTI available if present.
  Function *faust_rttifun = module->getFunction("faust_add_rtti");
  for (bcmap::const_iterator m = loaded_dsps.begin(),
	 end = loaded_dsps.end(); m != end; ++m) {
    const string& modname = m->first;
    const bcdata_t& info = m->second;
    vector<Value*> argv(3);
    GlobalVariable *v = global_variable
      (module, ArrayType::get(int8_type(), modname.size()+1), true,
       GlobalVariable::InternalLinkage, constant_char_array(modname.c_str()),
       "$$faust_str");
    // "cast" the char array to a char*
    Value *idx[2] = { Zero, Zero };
    Value *p = b.CreateGEP(v, mkidxs(idx, idx+2));
    argv[0] = p;
    argv[1] = SInt(info.tag);
    argv[2] = Bool(info.dbl);
    b.CreateCall(faust_rttifun, mkargs(argv));
  }
  // Execute the initialization code of the Pure program (global expressions
  // and variable definitions).
  for (Module::iterator it = module->begin(), end = module->end();
       it != end; ++it) {
    Function *f = &*it;
    if (f != main && is_init(f->getName())) {
      if (!debugging) {
	vector<Value*> argv(2);
	argv[0] = Zero;
	argv[1] = Zero;
	b.CreateCall(module->getFunction("pure_push_args"),
		     mkargs(argv));
      }
      CallInst* v = b.CreateCall(f);
      b.CreateCall(freefun, v);
    }
  }
  b.CreateRet(0);
  verifyFunction(*main);
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
    string cc = "gcc";
    string cxx = "g++";
    const char *env;
    if ((env = getenv("CC"))) cc = env;
    if ((env = getenv("CXX"))) cxx = env;
    bool vflag = (verbose&verbosity::compiler) != 0;
    string libs;
    set<string> libset;
    for (list<string>::iterator it = loaded_libs.begin();
	 it != loaded_libs.end(); ++it) {
      string libnm = *it;
      if (!chkfile(libnm)) {
	// This is not a real library file, so it will presumably give us a
	// linker error. But, as suggested by Alastair Pharo, ld may still be
	// able to find the library file if we add a little magic here.
#if GNU_LINKER
	// Good. Linker supports -l:libname, let's use that.
	libnm = string("-l:")+libnm;
#else
	// The linker doesn't seem to support -l:, but we may still be able to
	// use a standard -l option if the library name follows the usual
	// conventions.
	if (libnm.find("/") == string::npos && libnm.find("lib") == 0) {
	  libnm.erase(0, 3);
	  size_t l = libnm.find(".");
	  libnm = string("-l")+libnm.substr(0, l);
	}
#endif
      }
      if (libset.find(libnm) == libset.end()) {
	libs += " "+quote(libnm);
	libset.insert(libnm);
      }
    }
    for (list<string>::iterator it = libnames.begin();
	 it != libnames.end(); ++it)
      libs += " -l"+quote(*it);
    /* Call llc (and opt) to create a native assembler (or object) file which
       can then be passed to gcc to handle linkage (if requested). */
    string asmfile = (ext==".s")?out:out+".s";
    bool obj_target = false;
    string custom_opts = "";
#if LLVM33
    /* LLVM 3.3 and later generate assembler code which doesn't compile with
       native assemblers on some systems. OTOH, they offer the capability to
       directly generate native object files via llc, which speeds up
       compilation and works around issues with native assembly. This is the
       route we take here. */
    if (ext != ".s") {
      asmfile = (ext==".o")?out:out+".o";
      obj_target = true;
      custom_opts = "-filetype=obj ";
    }
#else
#if LLVM30 && __APPLE__
    // The -disable-cfi seems to be needed on OSX as of LLVM 3.0.
    custom_opts = "-disable-cfi ";
#endif
#endif
    if (!llcopts.empty()) llcopts += " ";
    string cmd = opt+" -f -std-compile-opts "+quote(target)+
      " | "+llc+" "+llcopts+custom_opts+
      string(pic?"-relocation-model=pic ":"")+
      "-o "+quote(asmfile);
    if (vflag) std::cerr << cmd << '\n';
    unlink(asmfile.c_str());
    int status = system(cmd.c_str());
    unlink(target.c_str());
    if (WIFEXITED(status) && WEXITSTATUS(status) == 0 && ext!=".s") {
      // Assemble, if needed.
      string obj = (ext==".o")?out:out+".o";
      if (!obj_target) {
	cmd = cc+" -c "+quote(asmfile)+" -o "+quote(obj);
	if (vflag) std::cerr << cmd << '\n';
	status = system(cmd.c_str());
	unlink(asmfile.c_str());
      }
      if (WIFEXITED(status) && WEXITSTATUS(status) == 0) {
	// Link.
#ifdef __linux__
	string extra_linkopts = (cxx=="g++")?
	  // XXXFIXME: This used to be the default, but some Linux
	  // distributions have started shipping gcc versions which have the
	  // --as-needed linker option enabled by default, which breaks some
	  // Pure modules. Thus we always enforce --no-as-needed now in order
	  // to get back the old behaviour. This might change in the future.
	  " -Wl,--no-as-needed":"";
#else
	string extra_linkopts = "";
#endif
#ifdef LIBDIR
	string auxlibdir = LIBDIR;
#endif
	string linkopts = quote(obj)+extra_linkopts+libs+
#ifdef __MINGW32__
	  /* Link some extra libs and beef up the stack size on Windows. */
	  " -Wl,--stack=0x800000 -lregex -lglob"+
#endif
#ifdef LIBDIR
	  " -L"+quote(auxlibdir)+
#endif
	  " -lpure";
	if (ext != ".o") {
	  // Link.
	  cmd = cxx+" -o "+quote(out)+" "+quote(libdir)+"pure_main.o "+linkopts;
	  if (vflag) std::cerr << cmd << '\n';
	  status = system(cmd.c_str());
	  unlink(obj.c_str());
	} else if (vflag)
	  std::cerr << "Link with: " << cxx << " " << linkopts << '\n';
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
  sym.unresolved = false;
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
  string name = faust_basename(info.name);
  os << "extern " << interp.type_name(info.type) << " " << name << "(";
  size_t n = info.argtypes.size();
  for (size_t i = 0; i < n; i++) {
    if (i > 0) os << ", ";
    os << interp.type_name(info.argtypes[i]);
  }
  if (info.varargs) os << ((n>0)?", ...":"...");
  os << ")";
  if (info.tag > 0) {
    const symbol& sym = interp.symtab.sym(info.tag);
    if (sym.s != info.name) os << " = " << sym.s;
  }
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

void Env::add_key(uint32_t key, uint32_t *refp)
{
  interpreter& interp = *interpreter::g_interp;
  interp.add_key(key, refp);
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
    tag = e.tag; name = e.name; n = e.n; f = e.f; h = e.h;
    args = e.args; envs = e.envs;
    b = e.b; local = e.local; parent = e.parent;
  }
  fmap = e.fmap; xmap = e.xmap; xtab = e.xtab; prop = e.prop; m = e.m;
  if (e.descr) descr = e.descr;
  key = e.key; refp = e.refp;
  return *this;
}

void Env::clear()
{
  /* Note that we deliberately leak memory on refp here, because it may be
     shared by any number of different Env objects and runtime closures. That
     saves us an extra refcounter on the refcounter itself. Oh well. */
  static list<Function*> to_be_deleted;
  if (!f) return; // not initialized
  if (rp) delete rp;
  interpreter& interp = *interpreter::g_interp;
  if (local) {
    // purge local functions
#if DEBUG>2
    std::cerr << "clearing local '" << name << "'\n";
#endif
    if (!refp || *refp == 0) {
      if (h != f) interp.JIT->freeMachineCodeForFunction(h);
      interp.JIT->freeMachineCodeForFunction(f);
    } else {
      /* The code for this function is still used in a closure somewhere. To
	 avoid dangling function pointers, we just unmap the function pointer
	 instead of really freeing the code. NOTE: This effectively makes the
	 code permanent and thus leaks memory on the code pointer. But we
	 can't really help that because we have to get rid of the function IR
	 at this point. */
      if (h != f) interp.JIT->updateGlobalMapping(h, 0);
      interp.JIT->updateGlobalMapping(f, 0);
    }
    f->dropAllReferences(); if (h != f) h->dropAllReferences();
    fmap.clear();
    to_be_deleted.push_back(f); if (h != f) to_be_deleted.push_back(h);
  } else {
#if DEBUG>2
    std::cerr << "clearing global '" << name << "'\n";
#endif
    bool init_code = is_init(name);
    // anonymous globals (doeval, dodefn) are taken care of elsewhere
    if (!init_code) {
      // get rid of the machine code
      bool dead = !refp || *refp == 0;
      if (!dead && *refp == 1) {
	/* The case of global functions (which have their closures cached in
	   global variables) is a bit more involved than the above, since refp
	   will always be at least 1 in this case. To avoid leaking memory on
	   redefined globals, we also check the reference counter of the
	   closure. If it is at most 1 then the global variable (which, since
	   we came here, is about to be cleared or redefined) is the only
	   reference left at this point, hence the code isn't needed any more
	   and can be freed. */
	map<int32_t,GlobalVar>::iterator it = interp.globalvars.find(tag);
	if (it == interp.globalvars.end())
	  dead = true;
	else {
	  GlobalVar& v = it->second;
	  dead = !v.x || v.x->refc <= 1;
	}
      }
      if (dead) {
	if (h != f) interp.JIT->freeMachineCodeForFunction(h);
	interp.JIT->freeMachineCodeForFunction(f);
      } else {
	/* Keep the code for a function which is still bound by a closure.
	   See the remarks above. */
	if (h != f) interp.JIT->updateGlobalMapping(h, 0);
	interp.JIT->updateGlobalMapping(f, 0);
      }
      // only delete the body, this keeps existing references intact
      f->deleteBody();
    }
    // delete all nested environments and reinitialize other body-related data
    fmap.clear(); xmap.clear(); xtab.clear(); prop.clear(); m = 0;
    // now that all references have been removed, delete the function pointers
    for (list<Function*>::iterator fi = to_be_deleted.begin();
	 fi != to_be_deleted.end(); fi++) {
#if LLVM27
      /* XXXFIXME: This appears to be necessary to work around a bug in the
	 lazy JIT of LLVM >=2.7, cf. http://llvm.org/bugs/show_bug.cgi?id=6360.
	 PR#6360 has apparently been fixed since, but test052.pure still fails
	 for me as of LLVM 3.0, so we leave this enabled for now. */
      // LLVM >=2.7 collects the function code anyway if we erase the IR, so
      // we just delete the body instead.
      (*fi)->deleteBody();
#else
      (*fi)->eraseFromParent();
#endif
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
    std::cerr << "** calling function: " << f->getName().data() << '\n';
    f->dump();
    assert(0 && "bad function call");
  }
#endif
  CallInst* v = builder.CreateCall(f, mkargs(args));
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
#if LLVM27
	    CallInst *c2 = cast<CallInst>(c1->clone());
#else
	    CallInst *c2 = c1->clone(
#if LLVM26
				     llvm::getGlobalContext()
#endif
				     );
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
#if USE_FASTCC
    } else if (interp.use_fastcc &&
	       (c->getCalledFunction() ==
		interp.module->getFunction("pure_call") ||
		c->getCalledFunction() ==
		interp.module->getFunction("pure_apply"))) {
      // Indirect call through the runtime. Treated like a tail call.
      pi = c;
#endif
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
    CallInst::Create(free1_fun, mkargs(myargs), "", pi);
  } else if (n+m != 0 || !interp.debugging) {
    vector<Value*> myargs;
    if (pi == ret)
      myargs.push_back(v);
    else
      myargs.push_back(ConstantPointerNull::get(interp.ExprPtrTy));
    myargs.push_back(UInt(n));
    myargs.push_back(UInt(m));
    CallInst::Create(free_fun, mkargs(myargs), "", pi);
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
      new Env(0, 0, x.largs()->size(), x.lrule().rhs, false, true);
    Env& e = *eptr;
    e.build_map(x.lrule().rhs); e.promote_map();
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
  default: {
    interpreter& interp = *interpreter::g_interp;
    if (x.tag() == interp.symtab.func_sym().f) {
      // __func__ builtin. This is treated like a corresponding FVAR.
      // We're looking for a named closure or a lambda.
      if (tag>0 || (n>0 && !descr)) break;
      // Look for a suitable stacked environment.
      EnvStack::iterator ei = envstk.begin();
      uint8_t idx = 1;
      while (ei != envstk.end()) {
	Env *e = *ei;
	if (e->tag>0 || (e->n>0 && !e->descr)) break;
	ei++; idx++;
      }
      if (ei != envstk.end() && (*ei)->local) {
	// this is the target environment, propagate locals to the call site
	Env *fenv = *ei;
	idx++;
	if (fenv->prop.find(this) == fenv->prop.end()) {
	  fenv->prop[this] = idx;
	  props.insert(fenv);
	} else
	  assert(fenv->prop[this] == idx);
      }
    }
    break;
  }
  }
}

void Env::build_map(expr x, rulel::const_iterator r, rulel::const_iterator end)
{
  // build the maps for a 'when' expression (cf. when_codegen())
  // x = subject expression to be evaluated in the context of the bindings
  // r = current pattern binding rule
  // end = end of rule list
  // Skip anonymous bindings.
  interpreter& interp = *interpreter::g_interp;
  while (r != end && r->lhs.is_var() &&
	 r->lhs.vtag() == interp.symtab.anon_sym &&
	 r->lhs.ttag() == 0) {
    build_map(r->rhs);
    ++r;
  }
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
  // we need a separate submap for each rule
  rulel::const_iterator r = rl.begin(), end = rl.end();
  while (r != end) {
    build_map(r->rhs);
    if (!r->qual.is_null()) build_map(r->qual);
    if (++r != end) fmap.next();
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
  while (e != envstk.end() && ((*e)->local || (*e)->tag != tag)) ++e;
  if (e != envstk.end()) {
    assert((*e)->tag == tag);
    return *e;
  } else
    return 0;
}

int32_t interpreter::find_hash(Env *e)
{
  // find the hash for a lambda environment on the stack
  EnvStack::iterator ei = envstk.begin();
  while (ei != envstk.end() && *ei != e) ++ei;
  if (ei == envstk.end()) return 0; // error, environment not on stack
  ei++;
  if (ei == envstk.end()) return 0; // error, no parent environment
  Env *p = *ei;
  EnvMap::const_iterator mi;
  for (mi = p->fmap.act().begin(); mi != p->fmap.act().end(); mi++) {
    int32_t tag = mi->first;
    const Env *f = mi->second;
    if (e == f) return tag;
  }
  return 0; // not found
}

llvm_const_Type *interpreter::make_pointer_type(const string& name)
{
  type_map::iterator it = pointer_types.find(name);
  if (it == pointer_types.end()) {
    string namestr = (name.size()>0 && name[name.size()-1]=='*') ?
      name.substr(0, name.size()-1) : name;
    llvm_const_Type *ty = opaque_type(namestr.c_str());
    pointer_types[name] = PointerType::get(ty, 0);
    it = pointer_types.find(name);
    assert(it != pointer_types.end());
    pointer_type_of[ty] = it;
  }
  return it->second;
}

string mangle_type_name(string name)
{
  /* Type names in LLVM bitcode may well contain special characters not
     permitted in identifiers, so we mangle them here. (This is very
     simplistic and may easily map different bitcode names into the same
     identifier. Oh well.) */
  if (name.empty() || isdigit(name[0]))
    // Probably a temporary name, we don't want those.
    return "";
  for (size_t i = 0, n = name.size(); i < n; i++)
    if (!isalnum(name[i]))
      name[i] = '_';
  return name;
}

string interpreter::pointer_type_name(llvm_const_Type *type)
{
  assert(is_pointer_type(type));
  llvm_const_Type *elem_type = type->getContainedType(0);
  if (is_pointer_type(elem_type)) {
    llvm_const_Type *ty = elem_type->getContainedType(0);
    map<llvm_const_Type*,type_map::iterator>::const_iterator it =
      pointer_type_of.find(ty);
    if (it != pointer_type_of.end())
      return it->second->first+"*";
  }
  map<llvm_const_Type*,type_map::iterator>::const_iterator it =
    pointer_type_of.find(elem_type);
  if (it != pointer_type_of.end())
    return it->second->first;
  /* This is an unknown pointer type, presumably from a bitcode file. Let's
     count the levels of indirection to get the number of *'s right. */
  size_t count = 1;
  while (is_pointer_type(elem_type)) {
    elem_type = elem_type->getContainedType(0);
    count++;
  }
  /* What remains is a non-pointer type, which we consider irreducible. Try to
     resolve that recursively, to get our name for it. If that doesn't work,
     check whether the bitcode file has a name for it. If that doesn't work
     either then just give up and assume "void". */
  string name = type_name(elem_type);
  if (name == "<unknown C type>") {
#if LLVM30
    name.clear();
    if (elem_type->isStructTy()) {
      StructType *ty = (StructType*)elem_type;
      if (ty->hasName()) name = ty->getName();
    }
#else
    name = module->getTypeName(elem_type);
#endif
    name = mangle_type_name(name);
    if (name.empty()) name = "void";
  }
  name.append(count, '*');
  return name;
}

int interpreter::pointer_type_tag(const string& name)
{
  if (name == "void*") return 0; // generic pointer
  map<string,int>::iterator it = pointer_tags.find(name);
  if (it == pointer_tags.end()) {
    int tag = pure_make_tag();
    pointer_tags[name] = tag;
    it = pointer_tags.find(name);
    assert(it != pointer_tags.end());
    pointer_type_with_tag[tag] = it;
  }
  return it->second;
}

llvm_const_Type *interpreter::named_type(string name)
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
    return long_type();
  else if (name == "size_t")
    return size_t_type();
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
    return PointerType::get(long_type(), 0);
  else if (name == "size_t*")
    return PointerType::get(size_t_type(), 0);
  else if (name == "float*")
    return PointerType::get(float_type(), 0);
  else if (name == "double*")
    return PointerType::get(double_type(), 0);
  else if (name == "void**")
    return PointerType::get(VoidPtrTy, 0);
  else if (name == "char**")
    return PointerType::get(CharPtrTy, 0);
  else if (name == "short**" || name == "int16**")
    return PointerType::get(PointerType::get(int16_type(), 0), 0);
  else if (name == "int**" || name == "int32**")
    return PointerType::get(PointerType::get(int32_type(), 0), 0);
  else if (name == "float**")
    return PointerType::get(PointerType::get(float_type(), 0), 0);
  else if (name == "double**")
    return PointerType::get(PointerType::get(double_type(), 0), 0);
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
  else if (name.size() > 0 && name[name.size()-1] == '*') {
    // Generic pointer type. First normalize this a bit, then create a
    // placeholder type which uniquely identifies the type.
    size_t pos = name.find_last_not_of('*');
    if (pos != string::npos) {
      string ptr = name.substr(pos+1);
      name.erase(pos+1);
      if (name == "int8")
	name = "char";
      else if (name == "int16")
	name = "short";
      else if (name == "int32")
	name = "int";
#if SIZEOF_LONG==8
      else if (name == "int64")
	name = "long";
#endif
      name += ptr;
    }
    if (name.size() > 1 && name[name.size()-2] == '*')
      // generic pointer to pointer (effectively treated as void**)
      return PointerType::get
	(make_pointer_type(name.substr(0, name.size()-1)), 0);
    else
      // simple pointer type (effectively treated as void*)
      return make_pointer_type(name);
  } else
    throw err("unknown C type '"+name+"'");
}

string interpreter::type_name(llvm_const_Type *type)
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
  else if (type == PointerType::get(VoidPtrTy, 0))
    return "void**";
  else if (type == PointerType::get(CharPtrTy, 0))
    return "char**";
  else if (type == PointerType::get(PointerType::get(int16_type(), 0), 0))
    return "short**";
  else if (type == PointerType::get(PointerType::get(int32_type(), 0), 0))
    return "int**";
  else if (type == PointerType::get(PointerType::get(float_type(), 0), 0))
    return "float**";
  else if (type == PointerType::get(PointerType::get(double_type(), 0), 0))
    return "double**";
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
  else if (is_pointer_type(type))
    return pointer_type_name(type);
  else
    return "<unknown C type>";
}

llvm_const_Type *interpreter::gslmatrix_type(llvm_const_Type *elem_ty,
					     llvm_const_Type *block_ty,
					     size_t padding)
{
  if (!elem_ty || !block_ty) return 0;
  std::vector<llvm_const_Type*> elts;
  elts.push_back(size_t_type());			// size1
  elts.push_back(size_t_type());			// size2
  elts.push_back(size_t_type());			// tda
  elts.push_back(PointerType::get(elem_ty, 0));		// data
  elts.push_back(PointerType::get(block_ty, 0));	// block
  elts.push_back(int32_type());				// owner
  if (padding>0)
    elts.push_back(array_type(int8_type(), padding));	// padding (64 bit)
  return struct_type(elts);
}

static bool struct_type_eq(llvm_const_Type *type, llvm_const_Type *type2)
{
  if (type == type2) return true;
#ifdef LLVM30
  if (!type || !type2) return false;
  if ((dyn_cast<StructType>(type))->isLayoutIdentical
      (dyn_cast<StructType>(type2)))
    return true;
#endif
  return false;
}

string interpreter::bctype_name(llvm_const_Type *type)
{
  /* This is basically like type_name above, but we need to give special
     treatment to some pointer types (Pure expressions, GSL matrices) which
     may have different representations when coming from an external bitcode
     file. */
  if (is_pointer_type(type)) {
    llvm_const_Type *elem_type = type->getContainedType(0);
    if (is_struct_type(elem_type)) {
      /* XXXFIXME: These checks really need to be rewritten so that they're
	 less compiler-specific. Currently they only work with recent
	 llvm-gcc, clang and dragonegg versions. */
      // Special support for Pure expression pointers, passed through
      // unchanged.
      if (elem_type == module->getTypeByName("struct.pure_expr") ||
	  elem_type == module->getTypeByName("struct._pure_expr"))
	return "expr*";
      // Special support for the GSL matrix types.
      else if (elem_type == module->getTypeByName("struct.gsl_matrix") ||
	       elem_type == module->getTypeByName("struct._gsl_matrix") ||
	       struct_type_eq
	       (elem_type, gslmatrix_type
		(double_type(),
		 module->getTypeByName("struct.gsl_block_struct"))) ||
	       struct_type_eq
	       (elem_type, gslmatrix_type
		(double_type(),
		 module->getTypeByName("struct.gsl_block_struct"), 4)))
	return "dmatrix*";
      else if (elem_type == module->getTypeByName("struct.gsl_matrix_int") ||
	       elem_type == module->getTypeByName("struct._gsl_matrix_int") ||
	       struct_type_eq
	       (elem_type, gslmatrix_type
		(int32_type(),
		 module->getTypeByName("struct.gsl_block_int_struct"))) ||
	       struct_type_eq
	       (elem_type, gslmatrix_type
		(int32_type(),
		 module->getTypeByName("struct.gsl_block_int_struct"), 4)))
	return "imatrix*";
      else if (elem_type == module->getTypeByName
	       ("struct.gsl_matrix_complex") ||
	       elem_type == module->getTypeByName
	       ("struct._gsl_matrix_complex") ||
	       struct_type_eq
	       (elem_type, gslmatrix_type
		(double_type(),
		 module->getTypeByName("struct.gsl_block_complex_struct"))) ||
	       struct_type_eq
	       (elem_type, gslmatrix_type
		(double_type(),
		 module->getTypeByName("struct.gsl_block_complex_struct"), 4)))
	return "cmatrix*";
    }
  }
  return type_name(type);
}

string interpreter::dsptype_name(llvm_const_Type *type)
{
  /* Special version of bctype_name for Faust modules. This doesn't have the
     Pure expression and GSL matrix types, but instead we map i8* to void*. */
  if (type == CharPtrTy)
    return "void*";
  else if (type == PointerType::get(CharPtrTy, 0))
    return "void**";
  else
    return type_name(type);
}

bool interpreter::compatible_types(llvm_const_Type *type1, llvm_const_Type *type2)
{
  if (type1 == type2)
    return true;
  else {
    bool t1 = is_pointer_type(type1), t2 = is_pointer_type(type2);
    return t1 && t1 == t2;
  }
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
				      string asname, bool dll_check)
{
  // translate type names to LLVM types
  size_t n = argtypes.size();
  llvm_const_Type* type = named_type(restype);
  vector<llvm_const_Type*> argt(n);
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
    FunctionType *ft = func_type(type, argt, varargs);
    f = Function::Create(ft, Function::ExternalLinkage, name, module);
    // Enter a fixed association into the dynamic linker table. This ensures
    // that even if the runtime functions can't be resolved via dlopening
    // the interpreter executable (e.g., if the interpreter was linked
    // without -rdynamic), the interpreter will still find them.
    sys::DynamicLibrary::AddSymbol(name, fp);
    always_used.insert(f);
    return f;
  }
  // External C function visible in the Pure program. Note that int32_t gets
  // promoted to int64_t if the default int type of the target platform has
  // 64 bit.
  if (type == int32_type() && sizeof(int) > 4)
    type = int64_type();
  for (size_t i = 0; i < n; i++, atype++)
    if (argt[i] == int32_type() && sizeof(int) > 4)
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
    else if (!active_namespaces.empty())
      _sym->priv = active_namespaces.front().priv;
  }
  assert(_sym);
  symbol& sym = *_sym;
  // Make sure that the symbol is marked as resolved.
  sym.unresolved = false;
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
  FunctionType *ft = func_type(type, argt, varargs);
  Function *g = module->getFunction(name);
  llvm_const_FunctionType *gt = g?g->getFunctionType():0;
  // Check whether we already have an external declaration for this symbol.
  map<int32_t,ExternInfo>::const_iterator it = externals.find(sym.f);
  // Handle the case that the C function was imported *after* the definition
  // of a Pure function of the same name. In this case the C function won't be
  // accessible in the Pure program at all.
  symbol* _fsym = symtab.lookup(name);
  if (_fsym && globenv.find(_fsym->f) != globenv.end() &&
      globenv[_fsym->f].t == env_info::fun && g && !g->isDeclaration() &&
      externals.find(_fsym->f) == externals.end())
    throw err("symbol '"+name+"' is already defined as a Pure function");
  if (it == externals.end() && g && dll_check) {
    // Cross-check with a previous declaration under a different name (might
    // also be a builtin declaration).
    assert(gt);
    if (gt != ft) {
      // If there's a literal type mismatch, we still check types for
      // compatibility here, in the sense that two C types might be used
      // interchangeably for the same kind of data in Pure if they are both
      // pointer types (such as char* instead of void* and vice versa for
      // strings). As of Pure 0.45, we allow such venial mismatches (and just
      // generate a new wrapper), as long as the function is accessed under a
      // new alias. This gives the programmer some leeway (and some rope to
      // hang himself), e.g., to fix up declarations which the bitcode loader
      // didn't get right. Also, as of Pure 0.47, we allow arbitrary extra
      // parameters if the function was previously declared as varargs.
      size_t m = gt->getNumParams();
      bool ok = compatible_types(gt->getReturnType(), type) &&
	(m==n || (gt->isVarArg()>varargs && m<=n)) && gt->isVarArg()>=varargs;
      for (size_t i = 0; ok && i < m; i++) {
	ok = compatible_types(gt->getParamType(i), argt[i]);
      }
      if (!ok) {
	// Give some reasonable diagnostic. gt itself shows as LLVM assembler
	// when printed, so instead we manufacture an ExternInfo for gt which
	// supposedly is more informative and hopefully looks nicer to the
	// Pure programmer. ;-)
	size_t n = gt->getNumParams();
	vector<llvm_const_Type*> argt(n);
	for (size_t i = 0; i < n; i++)
	  argt[i] = gt->getParamType(i);
	ExternInfo info(0, name, gt->getReturnType(), argt, g, gt->isVarArg());
	ostringstream msg;
	msg << "declaration of extern function '" << name
	    << "' does not match previous declaration: " << info;
	throw err(msg.str());
      }
    }
  }
  if (it != externals.end()) {
    // Already declared under the same name, check that declarations
    // match. Here we require the types to be literally the same.
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
  // and resident libraries. (As of Pure 0.44, the function may now also come
  // from a bitcode module, in which case it's to be found in the Pure program
  // module.)
  if (dll_check && !(g && !g->isDeclaration()) &&
      !sys::DynamicLibrary::SearchForAddressOfSymbol(name))
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
  vector<llvm_const_Type*> argt2(n, ExprPtrTy);
  FunctionType *ft2 = func_type(ExprPtrTy, argt2, false);
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
  // emit extra signal and stack checks, if requested
  if (checks) {
    vector<Value*> argv;
    b.CreateCall(module->getFunction("pure_checks"));
  }
  // check for Faust functions, do needed setup
  bool is_faust_fun = is_faust(name);
  string faust_mod, faust_fun; int faust_tag = 0;
  if (is_faust_fun) {
    parse_faust_name(name, faust_mod, faust_fun);
    faust_tag = loaded_dsps[faust_mod].tag;
  }
  // unbox arguments
  bool temps = false, vtemps = false;
  size_t m = gt->getNumParams();
  for (size_t i = 0; i < n; i++) {
    Value *x = args[i];
    llvm_const_Type *type = (i<m)?gt->getParamType(i):argt[i];
    // check for thunks which must be forced
    if (argt[i] != ExprPtrTy) {
      // do a quick check on the tag value
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
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
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(EXPR::INT), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      Value *pv = b.CreateBitCast(x, IntExprPtrTy, "intexpr");
      idx[1] = ValFldIndex;
      Value *iv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "intval");
      unboxed[i] = b.CreateICmpNE(iv, Zero);
    } else if (argt[i] == int8_type()) {
      /* We allow either ints or bigints to be passed for C integers. */
      BasicBlock *intbb = basic_block("int");
      BasicBlock *mpzbb = basic_block("mpz");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 2);
      sw->addCase(SInt(EXPR::INT), intbb);
      sw->addCase(SInt(EXPR::BIGINT), mpzbb);
      f->getBasicBlockList().push_back(intbb);
      b.SetInsertPoint(intbb);
      Value *pv = b.CreateBitCast(x, IntExprPtrTy, "intexpr");
      idx[1] = ValFldIndex;
      Value *intv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "intval");
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(mpzbb);
      b.SetInsertPoint(mpzbb);
      // Handle the case of a bigint (mpz_t -> int).
      Value *mpzv = b.CreateCall(module->getFunction("pure_get_int"), x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = phi_node(b, int32_type(), 2);
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = b.CreateTrunc(phi, int8_type());
    } else if (argt[i] == int16_type()) {
      BasicBlock *intbb = basic_block("int");
      BasicBlock *mpzbb = basic_block("mpz");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 2);
      sw->addCase(SInt(EXPR::INT), intbb);
      sw->addCase(SInt(EXPR::BIGINT), mpzbb);
      f->getBasicBlockList().push_back(intbb);
      b.SetInsertPoint(intbb);
      Value *pv = b.CreateBitCast(x, IntExprPtrTy, "intexpr");
      idx[1] = ValFldIndex;
      Value *intv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "intval");
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(mpzbb);
      b.SetInsertPoint(mpzbb);
      // Handle the case of a bigint (mpz_t -> int).
      Value *mpzv = b.CreateCall(module->getFunction("pure_get_int"), x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = phi_node(b, int32_type(), 2);
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = b.CreateTrunc(phi, int16_type());
    } else if (argt[i] == int32_type()) {
      BasicBlock *intbb = basic_block("int");
      BasicBlock *mpzbb = basic_block("mpz");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 2);
      sw->addCase(SInt(EXPR::INT), intbb);
      sw->addCase(SInt(EXPR::BIGINT), mpzbb);
      f->getBasicBlockList().push_back(intbb);
      b.SetInsertPoint(intbb);
      Value *pv = b.CreateBitCast(x, IntExprPtrTy, "intexpr");
      idx[1] = ValFldIndex;
      Value *intv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "intval");
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(mpzbb);
      b.SetInsertPoint(mpzbb);
      // Handle the case of a bigint (mpz_t -> int).
      Value *mpzv = b.CreateCall(module->getFunction("pure_get_int"), x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = phi_node(b, int32_type(), 2);
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = phi;
    } else if (argt[i] == int64_type()) {
      BasicBlock *intbb = basic_block("int");
      BasicBlock *mpzbb = basic_block("mpz");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 2);
      sw->addCase(SInt(EXPR::INT), intbb);
      sw->addCase(SInt(EXPR::BIGINT), mpzbb);
      f->getBasicBlockList().push_back(intbb);
      b.SetInsertPoint(intbb);
      Value *pv = b.CreateBitCast(x, IntExprPtrTy, "intexpr");
      idx[1] = ValFldIndex;
      Value *intv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "intval");
      intv = b.CreateSExt(intv, int64_type());
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(mpzbb);
      b.SetInsertPoint(mpzbb);
      // Handle the case of a bigint (mpz_t -> long).
      Value *mpzv = b.CreateCall(module->getFunction("pure_get_int64"), x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = phi_node(b, int64_type(), 2);
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = phi;
    } else if (argt[i] == float_type()) {
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(EXPR::DBL), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      Value *pv = b.CreateBitCast(x, DblExprPtrTy, "dblexpr");
      idx[1] = ValFldIndex;
      Value *dv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "dblval");
      unboxed[i] = b.CreateFPTrunc(dv, float_type());
    } else if (argt[i] == double_type()) {
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(EXPR::DBL), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      Value *pv = b.CreateBitCast(x, DblExprPtrTy, "dblexpr");
      idx[1] = ValFldIndex;
      Value *dv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "dblval");
      unboxed[i] = dv;
    } else if (argt[i] == CharPtrTy) {
      /* String conversion. As of Pure 0.45, we also allow real char* pointers
	 and int matrices as byte* inputs here. */
      BasicBlock *ptrbb = basic_block("ptr");
      BasicBlock *strbb = basic_block("str");
      BasicBlock *matrixbb = basic_block("matrix");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 3);
      sw->addCase(SInt(EXPR::PTR), ptrbb);
      sw->addCase(SInt(EXPR::STR), strbb);
      sw->addCase(SInt(EXPR::IMATRIX), matrixbb);
      f->getBasicBlockList().push_back(ptrbb);
      b.SetInsertPoint(ptrbb);
      int tag = pointer_type_tag(CharPtrTy);
      if (tag) {
	// We must check the pointer tag here.
	BasicBlock *checkedbb = basic_block("checked");
	Function *g = module->getFunction("pure_check_tag");
	assert(g);
	vector<Value*> args;
	args.push_back(SInt(tag));
	args.push_back(x);
	Value *chk = b.CreateCall(g, mkargs(args));
	b.CreateCondBr(chk, checkedbb, failedbb);
	f->getBasicBlockList().push_back(checkedbb);
	b.SetInsertPoint(checkedbb);
	ptrbb = checkedbb;
      }
      Value *pv = b.CreateBitCast(x, PtrExprPtrTy, "ptrexpr");
      idx[1] = ValFldIndex;
      Value *ptrv = b.CreateBitCast
	(b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "ptrval"),
	 CharPtrTy);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(strbb);
      b.SetInsertPoint(strbb);
      Value *sv = b.CreateCall(module->getFunction("pure_get_cstring"), x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(matrixbb);
      b.SetInsertPoint(matrixbb);
      Function *get_fun = module->getFunction("pure_get_matrix_data_byte");
      Value *matrixv = b.CreateBitCast(b.CreateCall(get_fun, x), CharPtrTy);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = phi_node(b, CharPtrTy, 3);
      phi->addIncoming(ptrv, ptrbb);
      phi->addIncoming(sv, strbb);
      phi->addIncoming(matrixv, matrixbb);
      unboxed[i] = phi; temps = true; vtemps = true;
      if (type != CharPtrTy)
	unboxed[i] = b.CreateBitCast(unboxed[i], type);
    } else if (argt[i] == PointerType::get(int16_type(), 0) ||
	       argt[i] == PointerType::get(int32_type(), 0) ||
	       argt[i] == PointerType::get(int64_type(), 0) ||
	       argt[i] == PointerType::get(double_type(), 0) ||
	       argt[i] == PointerType::get(float_type(), 0)) {
      /* These get special treatment, because we also allow numeric matrices
	 to be passed as an integer or floating point vector here. */
      bool is_short = argt[i] == PointerType::get(int16_type(), 0);
      bool is_int = argt[i] == PointerType::get(int32_type(), 0);
      bool is_int64 = argt[i] == PointerType::get(int64_type(), 0);
      bool is_float = argt[i] == PointerType::get(float_type(), 0);
      bool is_double = argt[i] == PointerType::get(double_type(), 0);
      BasicBlock *ptrbb = basic_block("ptr");
      BasicBlock *matrixbb = basic_block("matrix");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 3);
      Function *get_fun =
	is_short ? module->getFunction("pure_get_matrix_data_short") :
	is_int ? module->getFunction("pure_get_matrix_data_int") :
	is_int64 ? module->getFunction("pure_get_matrix_data_int64") :
	is_float ? module->getFunction("pure_get_matrix_data_float") :
	is_double ? module->getFunction("pure_get_matrix_data_double") :
	0;
      assert(get_fun);
      sw->addCase(SInt(EXPR::PTR), ptrbb);
      if (is_short || is_int || is_int64)
	sw->addCase(SInt(EXPR::IMATRIX), matrixbb);
      else if (is_float || is_double) {
	sw->addCase(SInt(EXPR::DMATRIX), matrixbb);
	sw->addCase(SInt(EXPR::CMATRIX), matrixbb);
      }
      f->getBasicBlockList().push_back(ptrbb);
      b.SetInsertPoint(ptrbb);
      int tag = pointer_type_tag(argt[i]);
      if (tag) {
	// We must check the pointer tag here.
	BasicBlock *checkedbb = basic_block("checked");
	Function *g = module->getFunction("pure_check_tag");
	assert(g);
	vector<Value*> args;
	args.push_back(SInt(tag));
	args.push_back(x);
	Value *chk = b.CreateCall(g, mkargs(args));
	b.CreateCondBr(chk, checkedbb, failedbb);
	f->getBasicBlockList().push_back(checkedbb);
	b.SetInsertPoint(checkedbb);
	ptrbb = checkedbb;
      }
      Value *pv = b.CreateBitCast(x, PtrExprPtrTy, "ptrexpr");
      idx[1] = ValFldIndex;
      Value *ptrv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "ptrval");
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(matrixbb);
      b.SetInsertPoint(matrixbb);
      Value *matrixv = b.CreateCall(get_fun, x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = phi_node(b, VoidPtrTy, 2);
      phi->addIncoming(ptrv, ptrbb);
      phi->addIncoming(matrixv, matrixbb);
      unboxed[i] = b.CreateBitCast(phi, type); vtemps = true;
    } else if (argt[i] == PointerType::get(VoidPtrTy, 0) ||
	       argt[i] == PointerType::get(CharPtrTy, 0)) {
      /* Conversion of symbolic vectors to void** and char**. */
      bool is_char = argt[i] == PointerType::get(CharPtrTy, 0);
      BasicBlock *ptrbb = basic_block("ptr");
      BasicBlock *matrixbb = basic_block("matrix");
      BasicBlock *smatrixbb = basic_block("smatrix");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 3);
      Function *sget_fun = module->getFunction
	(is_char ? "pure_get_matrix_vector_char" :
	 "pure_get_matrix_vector_void");
      Function *get_fun = is_char ?
	module->getFunction("pure_get_matrix_vector_byte") : 0;
      sw->addCase(SInt(EXPR::PTR), ptrbb);
      if (is_char) sw->addCase(SInt(EXPR::IMATRIX), matrixbb);
      sw->addCase(SInt(EXPR::MATRIX), smatrixbb);
      f->getBasicBlockList().push_back(ptrbb);
      b.SetInsertPoint(ptrbb);
      int tag = pointer_type_tag(argt[i]);
      if (tag) {
	// We must check the pointer tag here.
	BasicBlock *checkedbb = basic_block("checked");
	Function *g = module->getFunction("pure_check_tag");
	assert(g);
	vector<Value*> args;
	args.push_back(SInt(tag));
	args.push_back(x);
	Value *chk = b.CreateCall(g, mkargs(args));
	b.CreateCondBr(chk, checkedbb, failedbb);
	f->getBasicBlockList().push_back(checkedbb);
	b.SetInsertPoint(checkedbb);
	ptrbb = checkedbb;
      }
      Value *pv = b.CreateBitCast(x, PtrExprPtrTy, "ptrexpr");
      idx[1] = ValFldIndex;
      Value *ptrv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "ptrval");
      b.CreateBr(okbb);
      Value *matrixv = 0;
      if (is_char) {
	f->getBasicBlockList().push_back(matrixbb);
	b.SetInsertPoint(matrixbb);
	matrixv = b.CreateCall(get_fun, x);
	b.CreateBr(okbb);
      }
      f->getBasicBlockList().push_back(smatrixbb);
      b.SetInsertPoint(smatrixbb);
      Value *smatrixv = b.CreateCall(sget_fun, x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = phi_node(b, VoidPtrTy, 3);
      phi->addIncoming(ptrv, ptrbb);
      if (is_char) phi->addIncoming(matrixv, matrixbb);
      phi->addIncoming(smatrixv, smatrixbb);
      unboxed[i] = b.CreateBitCast(phi, type); vtemps = true;
    } else if (argt[i] ==
	       PointerType::get(PointerType::get(int16_type(), 0), 0) ||
	       argt[i] ==
	       PointerType::get(PointerType::get(int32_type(), 0), 0) ||
	       argt[i] ==
	       PointerType::get(PointerType::get(int64_type(), 0), 0) ||
	       argt[i] ==
	       PointerType::get(PointerType::get(double_type(), 0), 0) ||
	       argt[i] ==
	       PointerType::get(PointerType::get(float_type(), 0), 0)) {
      /* Conversion of matrices to vectors of pointers pointing to the rows of
	 the matrix. These allow a matrix to be modified in-place. */
      bool is_short = argt[i] ==
	PointerType::get(PointerType::get(int16_type(), 0), 0);
      bool is_int = argt[i] ==
	PointerType::get(PointerType::get(int32_type(), 0), 0);
      bool is_int64 = argt[i] ==
	PointerType::get(PointerType::get(int64_type(), 0), 0);
      bool is_float = argt[i] ==
	PointerType::get(PointerType::get(float_type(), 0), 0);
      bool is_double = argt[i] ==
	PointerType::get(PointerType::get(double_type(), 0), 0);
      BasicBlock *ptrbb = basic_block("ptr");
      BasicBlock *matrixbb = basic_block("matrix");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 3);
      Function *get_fun =
	is_short ? module->getFunction("pure_get_matrix_vector_short") :
	is_int ? module->getFunction("pure_get_matrix_vector_int") :
	is_int64 ? module->getFunction("pure_get_matrix_vector_int64") :
	is_float ? module->getFunction("pure_get_matrix_vector_float") :
	is_double ? module->getFunction("pure_get_matrix_vector_double") :
	0;
      assert(get_fun);
      sw->addCase(SInt(EXPR::PTR), ptrbb);
      if (is_short || is_int || is_int64)
	sw->addCase(SInt(EXPR::IMATRIX), matrixbb);
      else if (is_float || is_double) {
	sw->addCase(SInt(EXPR::DMATRIX), matrixbb);
	sw->addCase(SInt(EXPR::CMATRIX), matrixbb);
      }
      f->getBasicBlockList().push_back(ptrbb);
      b.SetInsertPoint(ptrbb);
      int tag = pointer_type_tag(argt[i]);
      if (tag) {
	// We must check the pointer tag here.
	BasicBlock *checkedbb = basic_block("checked");
	Function *g = module->getFunction("pure_check_tag");
	assert(g);
	vector<Value*> args;
	args.push_back(SInt(tag));
	args.push_back(x);
	Value *chk = b.CreateCall(g, mkargs(args));
	b.CreateCondBr(chk, checkedbb, failedbb);
	f->getBasicBlockList().push_back(checkedbb);
	b.SetInsertPoint(checkedbb);
	ptrbb = checkedbb;
      }
      Value *pv = b.CreateBitCast(x, PtrExprPtrTy, "ptrexpr");
      idx[1] = ValFldIndex;
      Value *ptrv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "ptrval");
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(matrixbb);
      b.SetInsertPoint(matrixbb);
      Value *matrixv = b.CreateCall(get_fun, x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = phi_node(b, VoidPtrTy, 2);
      phi->addIncoming(ptrv, ptrbb);
      phi->addIncoming(matrixv, matrixbb);
      unboxed[i] = b.CreateBitCast(phi, type); vtemps = true;
    } else if (argt[i] == GSLMatrixPtrTy ||
	       argt[i] == GSLDoubleMatrixPtrTy ||
	       argt[i] == GSLComplexMatrixPtrTy ||
	       argt[i] == GSLIntMatrixPtrTy) {
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
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
      unboxed[i] = b.CreateBitCast(matv, type);
    } else if (argt[i] == ExprPtrTy) {
      // passed through
      unboxed[i] = x;
      // Cast the pointer to the proper target type if necessary. This is only
      // necessary in the bitcode interface, since the Pure interpreter uses
      // its own internal representation of the expression data type.
      if (type != ExprPtrTy)
	unboxed[i] = b.CreateBitCast(unboxed[i], type);
    } else if (i == 0 && is_pointer_type(argt[i]) && is_faust_fun) {
      /* The first argument in a Faust call, if it is a pointer, is always the
	 dsp. Check the pointer against the module tag. */
      BasicBlock *ptrbb = basic_block("ptr");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 1);
      sw->addCase(SInt(EXPR::PTR), ptrbb);
      f->getBasicBlockList().push_back(ptrbb);
      b.SetInsertPoint(ptrbb);
      Value *pv = b.CreateBitCast(x, PtrExprPtrTy, "ptrexpr");
      idx[1] = ValFldIndex;
      Value *ptrv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "ptrval");
      Function *g = module->getFunction("pure_check_tag");
      assert(g);
      vector<Value*> args;
      args.push_back(SInt(faust_tag));
      args.push_back(x);
      Value *chk = b.CreateCall(g, mkargs(args));
      b.CreateCondBr(chk, okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = phi_node(b, VoidPtrTy, 1);
      phi->addIncoming(ptrv, ptrbb);
      unboxed[i] = phi;
      // Cast the pointer to the proper target type if necessary.
      if (type != VoidPtrTy)
	unboxed[i] = b.CreateBitCast(unboxed[i], type);
      if (faust_fun == "delete") {
	// Remove the sentry from the dsp pointer which is about to be deleted.
	Function *f = module->getFunction("pure_sentry");
	assert(f);
	vector<Value*> args;
	args.push_back(NullExprPtr);
	args.push_back(x);
	b.CreateCall(f, mkargs(args));
      }
    } else if (argt[i] == VoidPtrTy) {
      BasicBlock *ptrbb = basic_block("ptr");
      BasicBlock *mpzbb = basic_block("mpz");
      BasicBlock *matrixbb = basic_block("matrix");
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
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
      Value *ptrv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "ptrval");
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
      PHINode *phi = phi_node(b, VoidPtrTy, 3);
      phi->addIncoming(ptrv, ptrbb);
      phi->addIncoming(mpzv, mpzbb);
      phi->addIncoming(matrixv, matrixbb);
      unboxed[i] = phi;
      // Cast the pointer to the proper target type if necessary.
      if (type != VoidPtrTy)
	unboxed[i] = b.CreateBitCast(unboxed[i], type);
    } else if (is_pointer_type(argt[i])) {
      /* Generic pointer type. Only a proper pointer is allowed here, and we
	 may have to check its tag. */
      BasicBlock *okbb = basic_block("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, mkidxs(idx, idx+2)), "tag");
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(EXPR::PTR), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      int tag = pointer_type_tag(argt[i]);
      if (tag) {
	// We must check the pointer tag here.
	BasicBlock *checkedbb = basic_block("checked");
	Function *g = module->getFunction("pure_check_tag");
	assert(g);
	vector<Value*> args;
	args.push_back(SInt(tag));
	args.push_back(x);
	Value *chk = b.CreateCall(g, mkargs(args));
	b.CreateCondBr(chk, checkedbb, failedbb);
	f->getBasicBlockList().push_back(checkedbb);
	b.SetInsertPoint(checkedbb);
      }
      Value *pv = b.CreateBitCast(x, PtrExprPtrTy, "ptrexpr");
      idx[1] = ValFldIndex;
      Value *ptrv = b.CreateLoad(b.CreateGEP(pv, mkidxs(idx, idx+2)), "ptrval");
      unboxed[i] = ptrv;
      // Cast the pointer to the proper target type if necessary.
      if (type != VoidPtrTy)
	unboxed[i] = b.CreateBitCast(unboxed[i], type);
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
    b.CreateCall(f, mkargs(args));
  }
  // call the function
  Value* u = 0;
  if (is_faust_fun) {
    /* We do an indirect call here, so that it can be patched up later when a
       Faust dsp gets reloaded. (This works similar to global Pure functions
       which are also invoked indirectly through global variables.) */
    PointerType *fptype = PointerType::get(gt, 0);
    GlobalVariable *v = global_variable
      (module, fptype, false, GlobalVariable::InternalLinkage,
       ConstantPointerNull::get(fptype),
       "$"+name);
    void **fp = (void**)malloc(sizeof(void*));
    assert(fp);
    *fp = JIT->getPointerToFunction(g);
    JIT->addGlobalMapping(v, fp);
    u = b.CreateCall(b.CreateLoad(v), mkargs(unboxed));
  } else
    u = b.CreateCall(g, mkargs(unboxed));
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
  else if (is_pointer_type(type) &&
	   is_pointer_type(type->getContainedType(0))) {
    u = b.CreateCall(module->getFunction("pure_pointer"),
		     b.CreateBitCast(u, VoidPtrTy));
    // We may have to set the proper pointer tag here.
    int tag = pointer_type_tag(type);
    if (tag) {
      Function *f = module->getFunction("pure_tag");
      assert(f);
      vector<Value*> args;
      args.push_back(SInt(tag));
      args.push_back(u);
      b.CreateCall(f, mkargs(args));
    }
  } else if (type == GSLMatrixPtrTy)
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
    if (gt->getReturnType() != ExprPtrTy)
      // bitcast the result to an expr*
      u = b.CreateBitCast(u, ExprPtrTy);
    // check that we actually got a valid pointer; otherwise the call failed
    BasicBlock *okbb = basic_block("ok");
    b.CreateCondBr
      (b.CreateICmpNE(u, NullExprPtr, "cmp"), okbb, noretbb);
    f->getBasicBlockList().push_back(okbb);
    b.SetInsertPoint(okbb);
    // value is passed through
  } else if (is_pointer_type(type)) {
    if (gt->getReturnType() != VoidPtrTy)
      // bitcast the pointer result to a void*
      u = b.CreateBitCast(u, VoidPtrTy);
    u = b.CreateCall(module->getFunction("pure_pointer"), u);
    if (is_faust_fun) {
      // This is a pointer to a Faust dsp instance, tag it with the
      // appropriate module key.
      Function *f = module->getFunction("pure_tag");
      assert(f);
      vector<Value*> args;
      args.push_back(SInt(faust_tag));
      args.push_back(u);
      b.CreateCall(f, mkargs(args));
      // Now add the delete routine as a sentry on the dsp pointer, so that
      // dsp instances free themselves when garbage-collected. FIXME: This
      // assumes that the delete routine always comes before any dsp-creating
      // routines in the Faust module, as we can't do any forward linking
      // here.
      symbol *sym = symtab.lookup(make_absid(faust_mod+"::delete"));
      if (sym) {
	map<int32_t,GlobalVar>::iterator gv = globalvars.find(sym->f);
	if (gv != globalvars.end()) {
	  GlobalVar& v = gv->second;
	  Function *f = module->getFunction("pure_sentry");
	  assert(f);
	  vector<Value*> args;
	  args.push_back(b.CreateLoad(v.v));
	  args.push_back(u);
	  b.CreateCall(f, mkargs(args));
	}
      }
    } else {
      // We may have to set the proper pointer tag here.
      int tag = pointer_type_tag(type);
      if (tag) {
	Function *f = module->getFunction("pure_tag");
	assert(f);
	vector<Value*> args;
	args.push_back(SInt(tag));
	args.push_back(u);
	b.CreateCall(f, mkargs(args));
      }
    }
  } else
    assert(0 && "invalid C type");
  // free temporaries
  if (temps) b.CreateCall(module->getFunction("pure_free_cstrings"));
  if (vtemps) b.CreateCall(module->getFunction("pure_free_cvectors"));
  if (debugging) {
    Function *f = module->getFunction("pure_debug_redn");
    assert(f);
    vector<Value*> args;
    args.push_back(constptr(e));
    args.push_back(constptr(0));
    args.push_back(u);
    b.CreateCall(f, mkargs(args));
  }
  // free arguments (we do that here so that the arguments don't get freed
  // before we know that we don't need them anymore)
  if (n > 0 || !debugging) {
    vector<Value*> freeargs(3);
    freeargs[0] = u;
    freeargs[1] = UInt(n);
    freeargs[2] = Zero;
    b.CreateCall(module->getFunction("pure_pop_args"),
		 mkargs(freeargs));
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
    b.CreateCall(f, mkargs(args));
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
  // first. (Note that, as of Pure 0.48, the default value may also be NULL in
  // the case of a --defined function without equations.)
  pure_expr *cv = set_defined_sym(sym.f) ? 0 : pure_new(pure_const(sym.f));
  assert(JIT);
  GlobalVar& v = globalvars[sym.f];
  if (!v.v) {
    v.v = global_variable
      (module, ExprPtrTy, false, GlobalVariable::InternalLinkage, NullExprPtr,
       mkvarlabel(sym.f));
    JIT->addGlobalMapping(v.v, &v.x);
  }
  if (v.x) pure_free(v.x); v.x = cv;
  Value *defaultv = b.CreateLoad(v.v);
  // We first check for NULL values.
  BasicBlock *goodbb = basic_block("good"), *badbb = basic_block("bad");
  b.CreateCondBr(b.CreateICmpNE(defaultv, NullExprPtr), goodbb, badbb);
  f->getBasicBlockList().push_back(goodbb);
  b.SetInsertPoint(goodbb);
  // Everything's fine, invoke the default value to the arguments and return
  // the result.
  Value *defv = defaultv;
  vector<Value*> myargs(2);
  for (size_t i = 0; i < n; ++i) {
    myargs[0] = b.CreateCall(module->getFunction("pure_new"), defv);
    myargs[1] = b.CreateCall(module->getFunction("pure_new"), args[i]);
    defv = b.CreateCall(module->getFunction("pure_apply"),
			mkargs(myargs));
  }
  if (n > 0 || !debugging) {
    vector<Value*> freeargs(3);
    freeargs[0] = defv;
    freeargs[1] = UInt(n);
    freeargs[2] = Zero;
    b.CreateCall(module->getFunction("pure_pop_args"),
		 mkargs(freeargs));
  }
  b.CreateRet(defv);
  // NULL default value, raise a failed_match exception instead.
  f->getBasicBlockList().push_back(badbb);
  b.SetInsertPoint(badbb);
  // Create a cbox for the failed_match symbol and invoke pure_throw (we can't
  // use unwind() here since there's no environment on the stack).
  {
    int32_t tag = symtab.failed_match_sym().f;
    pure_expr *cv = pure_const(tag);
    GlobalVar& v = globalvars[tag];
    if (!v.v) {
      v.v = global_variable
	(module, ExprPtrTy, false, GlobalVariable::InternalLinkage,
	 NullExprPtr, mkvarlabel(tag));
      JIT->addGlobalMapping(v.v, &v.x);
    }
    if (v.x) pure_free(v.x); v.x = pure_new(cv);
    b.CreateCall(module->getFunction("pure_throw"), b.CreateLoad(v.v));
  }
  b.CreateRet(defaultv);
  verifyFunction(*f);
  if (FPM) FPM->run(*f);
  if (verbose&verbosity::dump) {
#if RAW_STREAM
    raw_ostream& out = outs();
#else
    ostream& out = std::cout;
#endif
    f->print(out);
  }
  externals[sym.f] = ExternInfo(sym.f, name, type, argt, f, varargs);
  return f;
}

Value *interpreter::envptr(bool local)
{
  if (!fptr || !local)
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

expr interpreter::wrap_expr(pure_expr *x, bool check)
{
  ostringstream label;
  label << x;
  GlobalVar *v = new GlobalVar;
  v->v = global_variable
    (module, ExprPtrTy, false, llvm::GlobalVariable::InternalLinkage,
     llvm::ConstantPointerNull::get(ExprPtrTy), "$$tmpvar"+label.str());
  v->x = pure_new(x);
  JIT->addGlobalMapping(v->v, &v->x);
  if (check && (x->tag == EXPR::PTR ||
		(x->tag >= 0 && x->data.clos && x->data.clos->local))) {
    /* These values need special treatment in a batch compilation. */
    nwrapped++;
  }
  return expr(EXPR::WRAP, v);
}

pure_expr *interpreter::const_value_invoke(expr x, pure_expr*& e, bool quote)
{
  // Wrapper around const_value which catches possible exceptions while
  // evaluating lists and tuples.
  pure_aframe *ex = push_aframe(sstk_sz);
  if (setjmp(ex->jmp)) {
    // caught an exception
    size_t sz = ex->sz;
    e = ex->e;
    pop_aframe();
    if (e) pure_new(e);
    for (size_t i = sstk_sz; i-- > sz; )
      if (sstk[i] && sstk[i]->refc > 0)
	pure_free(sstk[i]);
    sstk_sz = sz;
    return 0;
  } else {
    pure_expr *res = const_value(x, quote);
    // normal return
    pop_aframe();
    return res;
  }
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
  case EXPR::COND:
  case EXPR::LAMBDA:
  case EXPR::CASE:
  case EXPR::WHEN:
  case EXPR::WITH:
    return 0;
  default: {
    if (x.tag() > 0) {
      if (quote)
	return pure_const(x.tag());
      else {
	if (//x.tag() == symtab.locals_sym().f ||
	    externals.find(x.tag()) != externals.end())
	  return 0;
	map<int32_t,GlobalVar>::iterator v = globalvars.find(x.tag());
	if (v != globalvars.end() && v->second.x) {
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
    if (v != globalvars.end() && v->second.x) {
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
  begin_stats();
  pure_expr *res = 0;
  if (!keep) {
    // First check whether the value is actually a constant, then we can skip
    // the compilation step. (We only do this if we're not batch-compiling,
    // since in a batch compilation we need the generated code.)
    res = const_value_invoke(x, e);
    if (res || e) {
      end_stats();
      return res;
    }
  }
  // Create an anonymous function to call in order to evaluate the target
  // expression.
  /* NOTE: The environment is allocated dynamically, so that its child
     environments survive for the entire lifetime of any embedded closures,
     which might still be called at a later time. */
  Env *save_fptr = fptr;
  fptr = new Env(0, 0, 0, x, false); fptr->refc = 1;
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
  void *fp = JIT->getPointerToFunction(f.f);
  assert(fp);
  begin_stats();
  res = pure_invoke(fp, &e);
  end_stats();
  // Get rid of our anonymous function.
  JIT->freeMachineCodeForFunction(f.f);
  if (!keep) {
    f.f->eraseFromParent();
    // If there are no more references, we can get rid of the environment now.
    if (fptr->refc == 1)
      delete fptr;
    else
      fptr->refc--;
  }
  fptr = save_fptr;
  if (!astk) {
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

pure_expr *interpreter::dodefn(env vars, const vinfo& vi,
			       expr lhs, expr rhs,
			       pure_expr*& e, bool keep)
{
  char test;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax) {
    e = pure_const(symtab.segfault_sym().f);
    return 0;
  }
  e = 0;
  begin_stats();
  pure_expr *res = 0;
  if (!keep) {
    // First check whether the value is actually a constant, then we can skip
    // the compilation step. (We only do this if we're not batch-compiling,
    // since in a batch compilation we need the generated code.)
    res = const_value_invoke(rhs, e);
    if (e) {
      end_stats();
      return res;
    }
    if (res) {
      matcher m(rule(lhs, rhs));
      if (m.match(res)) {
	// verify type guards
	for (vguardl::const_iterator it = vi.guards.begin();
	     it != vi.guards.end(); ++it) {
	  pure_expr *x = pure_subterm(res, it->p);
	  bool rc = pure_safe_typecheck(it->ttag, x);
	  if (x != res) pure_freenew(x);
	  if (!rc) goto nomatch;
	}
	// verify non-linearities
	for (veqnl::const_iterator it = vi.eqns.begin();
	     it != vi.eqns.end(); ++it) {
	  pure_expr *x = pure_subterm(res, it->p),
	    *y = pure_subterm(res, it->q);
	  bool rc = same(x, y);
	  pure_new(x); pure_new(y);
	  pure_free(x); pure_free(y);
	  if (!rc) goto nomatch;
	}
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
	  pure_new(x);
	  if (v.x) pure_free(v.x);
	  v.x = x;
	}
      } else {
	// Failed match, bail out.
      nomatch:
	pure_freenew(res);
	res = e = 0;
      }
      end_stats();
      return res;
    }
  }
  // Create an anonymous function to call in order to evaluate the rhs
  // expression, match against the lhs and bind variables in lhs accordingly.
  Env *save_fptr = fptr;
  fptr = new Env(0, 0, 0, rhs, false); fptr->refc = 1;
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
  if (!vi.guards.empty()) {
    // verify guards
    for (vguardl::const_iterator it = vi.guards.begin(), end = vi.guards.end();
	 it != end; ++it) {
      BasicBlock *checkedbb = basic_block("typechecked");
      vector<Value*> args(2);
      args[0] = SInt(it->ttag);
      args[1] = vref(arg, it->p);
      Value *check =
	f.builder.CreateCall(module->getFunction("pure_safe_typecheck"),
			     mkargs(args));
      f.builder.CreateCondBr(check, checkedbb, failedbb);
      f.f->getBasicBlockList().push_back(checkedbb);
      f.builder.SetInsertPoint(checkedbb);
    }
  }
  if (!vi.eqns.empty()) {
    // check non-linearities
    for (veqnl::const_iterator it = vi.eqns.begin(), end = vi.eqns.end();
	 it != end; ++it) {
      BasicBlock *checkedbb = basic_block("checked");
      vector<Value*> args(2);
      args[0] = vref(arg, it->p);
      args[1] = vref(arg, it->q);
      Value *check = f.builder.CreateCall(module->getFunction("same"),
					  mkargs(args));
      f.builder.CreateCondBr(check, checkedbb, failedbb);
      f.f->getBasicBlockList().push_back(checkedbb);
      f.builder.SetInsertPoint(checkedbb);
    }
  }
  list<pure_expr*> cache;
  for (env::const_iterator it = vars.begin(); it != vars.end(); ++it) {
    int32_t tag = it->first;
    const env_info& info = it->second;
    assert(info.t == env_info::lvar && info.p);
    // walk the arg value to find the subterm at info.p
    path& p = *info.p;
    Value *x = vref(arg, p);
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
  void *fp = JIT->getPointerToFunction(f.f);
  assert(fp);
  begin_stats();
  res = pure_invoke(fp, &e);
  end_stats();
  // Get rid of our anonymous function.
  JIT->freeMachineCodeForFunction(f.f);
  if (!keep) {
    f.f->eraseFromParent();
    // If there are no more references, we can get rid of the environment now.
    if (fptr->refc == 1)
      delete fptr;
    else
      fptr->refc--;
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
  if (!astk) {
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
    rl->push_back(rule(r->lhs, r->rhs, r->vi, r->qual));
    r++;
  }
  return rl;
}

Value *interpreter::when_codegen(expr x, matcher *m,
				 rulel::const_iterator r,
				 rulel::const_iterator end,
				 rule *rp, int level)
// x = subject expression to be evaluated in the context of the bindings
// m = matching automaton for current rule
// r = current pattern binding rule
// end = end of rule list
{
  while (r != end && r->lhs.is_var() && r->lhs.vtag() == symtab.anon_sym &&
	 r->lhs.ttag() == 0) {
    // anonymous binding, rhs gets thrown away immediately after evaluation
    Value *u = codegen(r->rhs);
    act_builder().CreateCall(module->getFunction("pure_freenew"), u);
    ++r; ++m;
  }
  if (r == end) {
    // check whether we actually generated any environment
    if (!level) return codegen(x);
    // environment was generated, so we're at toplevel
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
    if (!rr.vi.guards.empty()) {
      // verify guards
      for (vguardl::const_iterator it = rr.vi.guards.begin(),
	     end = rr.vi.guards.end(); it != end; ++it) {
	BasicBlock *checkedbb = basic_block("typechecked");
	vector<Value*> args(2);
	args[0] = SInt(it->ttag);
	args[1] = vref(it->tag, it->p);
	Value *check =
	  e.builder.CreateCall(module->getFunction("pure_typecheck"),
			       mkargs(args));
	e.builder.CreateCondBr(check, checkedbb, failedbb);
	e.f->getBasicBlockList().push_back(checkedbb);
	e.builder.SetInsertPoint(checkedbb);
      }
    }
    if (!rr.vi.eqns.empty()) {
      // check non-linearities
      for (veqnl::const_iterator it = rr.vi.eqns.begin(),
	     end = rr.vi.eqns.end(); it != end; ++it) {
	BasicBlock *checkedbb = basic_block("checked");
	vector<Value*> args(2);
	args[0] = vref(it->tag, it->p);
	args[1] = vref(it->tag, it->q);
	Value *check = e.builder.CreateCall(module->getFunction("same"),
					    mkargs(args));
	e.builder.CreateCondBr(check, checkedbb, failedbb);
	e.f->getBasicBlockList().push_back(checkedbb);
	e.builder.SetInsertPoint(checkedbb);
      }
    }
    if (debugging) {
      expr y = (s==end)?x:expr::when(x, copy_rulel(s, end));
      e.rp = new rule(rr.lhs, y);
      debug_rule(e.rp);
    }
    Value *v = when_codegen(x, m+1, s, end, e.rp, level+1);
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

Value *interpreter::get_int_check(Value *u, BasicBlock *failedbb)
{
  Env& e = act_env();
  // bail out if the given value isn't an integer
  verify_tag(u, EXPR::INT, failedbb);
  // get the value
  Value *p = e.builder.CreateBitCast(u, IntExprPtrTy, "intexpr");
  Value *v = e.CreateLoadGEP(p, Zero, ValFldIndex, "intval");
  // collect the temporary, it's not needed any more
  call("pure_freenew", u);
  return v;
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
#ifdef LLVM26
      return b.CreateFSub(Dbl(0.0), u);
#else
      return b.CreateSub(Dbl(0.0), u);
#endif
    else {
      assert(0 && "error in type checker");
      return 0;
    }
  } else if (n == 2 && x.ttag() == EXPR::INT) {
    // binary int operations
    // these two need special treatment (short-circuit evaluation)
    if (f.tag() == symtab.or_sym().f) {
      Value *u = get_int(x.xval1().xval2());
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
      b.CreateBr(endbb);
      iffalsebb = b.GetInsertBlock();
      e.f->getBasicBlockList().push_back(endbb);
      b.SetInsertPoint(endbb);
      PHINode *phi = phi_node(b, int32_type(), 2, "fi");
      phi->addIncoming(u, iftruebb);
      phi->addIncoming(v, iffalsebb);
      return phi;
    } else if (f.tag() == symtab.and_sym().f) {
      Value *u = get_int(x.xval1().xval2());
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
      b.CreateBr(endbb);
      iftruebb = b.GetInsertBlock();
      e.f->getBasicBlockList().push_back(endbb);
      b.SetInsertPoint(endbb);
      PHINode *phi = phi_node(b, int32_type(), 2, "fi");
      phi->addIncoming(u, iffalsebb);
      phi->addIncoming(v, iftruebb);
      return phi;
    }
    if (x.xval1().xval2().ttag() == EXPR::DBL || x.xval2().ttag() == EXPR::DBL)
      // This is actually a mixed int/double operation, handled below.
      goto mixed;
    Value *u = get_int(x.xval1().xval2()), *v = get_int(x.xval2());
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
      PHINode *phi = phi_node(b, int32_type(), 2);
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
  } else {
  mixed:
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
#ifdef LLVM26
    else if (f.tag() == symtab.plus_sym().f)
      return b.CreateFAdd(u, v);
    else if (f.tag() == symtab.minus_sym().f)
      return b.CreateFSub(u, v);
    else if (f.tag() == symtab.mult_sym().f)
      return b.CreateFMul(u, v);
#else
    else if (f.tag() == symtab.plus_sym().f)
      return b.CreateAdd(u, v);
    else if (f.tag() == symtab.minus_sym().f)
      return b.CreateSub(u, v);
    else if (f.tag() == symtab.mult_sym().f)
      return b.CreateMul(u, v);
#endif
    else if (f.tag() == symtab.fdiv_sym().f)
      return b.CreateFDiv(u, v);
    else {
      assert(0 && "error in type checker");
      return 0;
    }
  }
}

bool interpreter::logical_tailcall(int32_t tag, uint32_t n, expr x,
				   const rule *rp)
{
  if (n != 2 || (tag != symtab.or_sym().f && tag != symtab.and_sym().f))
    return false;
  bool is_or = tag == symtab.or_sym().f;
  Env& e = act_env();
  Builder& b = act_builder();
  BasicBlock *okbb = basic_block("ok");
  BasicBlock *nokbb = basic_block("nok");
  BasicBlock *failedbb = basic_block("failed");
  Value *u0 = codegen(x.xval1().xval2()), *u = get_int_check(u0, failedbb);
  Value *condv = b.CreateICmpNE(u, Zero, "cond");
  if (is_or)
    b.CreateCondBr(condv, okbb, nokbb);
  else
    b.CreateCondBr(condv, nokbb, okbb);
  e.f->getBasicBlockList().push_back(okbb);
  b.SetInsertPoint(okbb);
  Value *okval = ibox(u);
  e.CreateRet(okval, rp);
  e.f->getBasicBlockList().push_back(nokbb);
  b.SetInsertPoint(nokbb);
  toplevel_codegen(x.xval2(), rp);
  e.f->getBasicBlockList().push_back(failedbb);
  b.SetInsertPoint(failedbb);
  Value *failedval = call(tag, u0, codegen(x.xval2()));
  e.CreateRet(failedval, rp);
  return true;
}

Value *interpreter::logical_funcall(int32_t tag, uint32_t n, expr x)
{
  if (n != 2 || (tag != symtab.or_sym().f && tag != symtab.and_sym().f))
    return 0;
  bool is_or = tag == symtab.or_sym().f;
  Env& e = act_env();
  Builder& b = act_builder();
  BasicBlock *okbb = basic_block("ok");
  BasicBlock *nokbb = basic_block("nok");
  BasicBlock *endbb = basic_block("end");
  BasicBlock *failedbb = basic_block("failed");
  Value *u0 = codegen(x.xval1().xval2()), *u = get_int_check(u0, failedbb);
  Value *condv = b.CreateICmpNE(u, Zero, "cond");
  if (is_or)
    b.CreateCondBr(condv, okbb, nokbb);
  else
    b.CreateCondBr(condv, nokbb, okbb);
  e.f->getBasicBlockList().push_back(okbb);
  b.SetInsertPoint(okbb);
  Value *okval = ibox(u);
  b.CreateBr(endbb);
  e.f->getBasicBlockList().push_back(nokbb);
  b.SetInsertPoint(nokbb);
  Value *nokval = codegen(x.xval2());
  b.CreateBr(endbb);
  nokbb = b.GetInsertBlock();
  e.f->getBasicBlockList().push_back(failedbb);
  b.SetInsertPoint(failedbb);
  Value *failedval = call(tag, u0, codegen(x.xval2()));
  b.CreateBr(endbb);
  failedbb = b.GetInsertBlock();
  e.f->getBasicBlockList().push_back(endbb);
  b.SetInsertPoint(endbb);
  PHINode *phi = phi_node(b, ExprPtrTy, 3, "fi");
  phi->addIncoming(okval, okbb);
  phi->addIncoming(nokval, nokbb);
  phi->addIncoming(failedval, failedbb);
  return phi;
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
  if (n>0 || !debugging) {
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

void interpreter::toplevel_codegen(expr x, const rule *rp)
{
  if (x.is_null()) {
    /* Result of failed guard. This can only occur at the toplevel. */
    act_env().CreateRet(NullExprPtr, rp);
    return;
  }
#if USE_FASTCC
  if (use_fastcc) {
    if (x.tag() == EXPR::COND) {
      toplevel_cond(x.xval1(), x.xval2(), x.xval3(), rp);
      return;
    }
    if (x.tag() == EXPR::COND1) {
      toplevel_cond(x.xval1(), x.xval2(), expr(), rp);
      return;
    }
    expr f; uint32_t n = count_args(x, f);
    if (f.tag() > 0 && logical_tailcall(f.tag(), n, x, rp))
      // built-in short-circuit ops (&& and ||)
      return;
    Env& e = act_env();
    e.CreateRet(codegen(x), rp);
  } else
#endif
    act_env().CreateRet(codegen(x), rp);
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

Value *interpreter::list_codegen(expr x, bool quote)
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
      const char * tuplev_fun = quote?
	(ttag==EXPR::INT?"pure_inttuplevq":"pure_doubletuplevq"):
	(ttag==EXPR::INT?"pure_inttuplev":"pure_doubletuplev");
      const char * listv_fun = quote?
	(ttag==EXPR::INT?"pure_intlistvq":"pure_doublelistvq"):
	(ttag==EXPR::INT?"pure_intlistv":"pure_doublelistv");
      const char * listv2_fun = quote?
	(ttag==EXPR::INT?"pure_intlistv2q":"pure_doublelistv2q"):
	(ttag==EXPR::INT?"pure_intlistv2":"pure_doublelistv2");
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
	u = codegen(tl, quote);
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
      const char * tuplev_fun = quote?"pure_biginttuplevq":"pure_biginttuplev";
      const char * listv_fun = quote?"pure_bigintlistvq":"pure_bigintlistv";
      const char * listv2_fun = quote?"pure_bigintlistv2q":"pure_bigintlistv2";
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
	u = codegen(tl, quote);
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
      const char * tuplev_fun = quote?"pure_strtuplevq":"pure_strtuplev";
      const char * listv_fun = quote?"pure_strlistvq":"pure_strlistv";
      const char * listv2_fun = quote?"pure_strlistv2q":"pure_strlistv2";
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
	u = codegen(tl, quote);
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
      Value *v = codegen(*it, quote);
      Value *idx[1];
      idx[0] = UInt(i++);
      act_builder().CreateStore
	(v, act_builder().CreateGEP(a, mkidxs(idx, idx+1)));
    }
    Value *u = 0;
    if (!x.is_pair() && tl.tag() != symtab.nil_sym().f)
      u = codegen(tl, quote);
    vector<Value*> args;
    args.push_back(SizeInt(n));
    args.push_back(a);
    const char * tuplev_fun = quote?"pure_tuplevq":"pure_tuplev";
    const char * listv_fun = quote?"pure_listvq":"pure_listv";
    const char * listv2_fun = quote?"pure_listv2q":"pure_listv2";
    if (u == 0)
      v = act_env().CreateCall
	(module->getFunction(x.is_pair()?tuplev_fun:listv_fun),
	 args);
    else {
      args.push_back(u);
      v = act_env().CreateCall
	(module->getFunction(listv2_fun), args);
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
      /* Optimize the case of string matrices. These are coded as a single
	 char array containing all (0-terminated) strings, together with an
	 offset table. */
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
    if (v->x->tag > 0 && v->x->data.clos && !v->x->data.clos->local) {
      /* A global named closure. We eliminate these on the fly. (This case can
	 only arise during batch compilation.) */
      // check for an external
      map<int32_t,ExternInfo>::const_iterator it = externals.find(v->x->tag);
      if (it != externals.end()) {
	const ExternInfo& info = it->second;
	vector<Value*> env;
	// build an fbox for the external
	return call("pure_clos", false, v->x->tag, 0, info.f,
		    NullPtr, info.argtypes.size(), env);
      }
      // check for an existing global variable (if the symbol is bound to a
      // global function, its cbox must exist already)
      map<int32_t,GlobalVar>::iterator v2 = globalvars.find(v->x->tag);
      if (v2 != globalvars.end())
	return act_builder().CreateLoad(v2->second.v);
    }
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
#if LIST_OPT>0
    {
      // special code for lists and tuples
      Value *w = list_codegen(x, quote);
      if (w) return w;
    }
#endif
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
      else if (f.tag() > 0 && (v = logical_funcall(f.tag(), n, x)))
	// built-in short-circuit ops (&& and ||)
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
  // XXXFIXME: the handling of quoted specials is obsolete here
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
    // case expression: treated like an anonymous closure (see the lambda
    // case above) which gets applied to the subject term to be matched
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
    expr t = x.xval();
    rulel::iterator begin = x.rules()->begin(), end = x.rules()->end();
    if (x.xval().tag() == EXPR::VAR &&
	x.rules()->back().lhs.tag() == EXPR::VAR &&
	x.rules()->back().lhs.ttag() == 0 &&
	x.rules()->back().vi.guards.empty() &&
	sameexpr(x.xval(), x.rules()->back().lhs)) {
      /* Handle a "tail binding" of the form 'x when ...; x = y end' where x
	 is an (untagged) local variable. In such a case we can always
	 eliminate x by transforming the expression to 'y when ... end'. If
	 the 'when' clause consists of a single binding, we can eliminate it
	 altogether. */
      t = x.rules()->back().rhs; --end;
      if (end == begin) return codegen(t);
    }
    return when_codegen(t, x.pm(), begin, end);
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
      fun_body(info.m, 0);
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
    // check for a call to the '__func__' builtin
    if (x.tag() == symtab.func_sym().f) {
      /* Find the innermost named closure or lambda. Note that the latter is
	 characterized by having a nonzero argument count and a null descr
	 field. */
      EnvStack::iterator ei = envstk.begin();
      uint8_t idx = 0;
      while (ei != envstk.end()) {
	Env *e = *ei;
	if (e->tag>0 || (e->n>0 && !e->descr))
	  break;
	idx++;
	ei++;
      }
      if (ei != envstk.end()) {
	// this is the target function
	Env *fenv = *ei;
	if (fenv->local) {
	  // Local function, construct the appropriate closure.
	  int32_t tag = fenv->tag;
	  if (tag <= 0)
	    // We have to find the tag for a lambda environment on the stack.
	    tag = find_hash(fenv);
	  assert(tag);
	  return fref(tag, idx+1, true);
	} else
	  // Global function, simply return the fbox.
	  return fbox(*fenv, true);
      }
      // We've fallen from the stack, so we're not inside a function. Fail.
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
      /* XXXFIXME: Can we eliminate the pure_call here if the symbol is known
	 to be a variable or a constant? */
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

Value *interpreter::fbox(Env& f, bool defer)
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
  if (f.n == 0 && !defer && (!f.local || f.tag > 0))
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
		envptr(f.local), f.n, x);
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

// Simple function call with two parameters. This is used to make the
// built-in short-circuit ops extensible.

Value *interpreter::call(int32_t f, Value *x, Value *y)
{
  Value *retv;
  map<int32_t,GlobalVar>::iterator v = globalvars.find(f);
  // If we already have a definition then use it, otherwise create a cbox
  // which can be patched up later.
  if (v != globalvars.end())
    retv = act_builder().CreateLoad(v->second.v);
  else
    retv = cbox(f);
  retv = apply(retv, x);
  retv = apply(retv, y);
  return retv;
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
  PHINode *phi = phi_node(f.builder, ExprPtrTy, 2, "fi");
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
  for (size_t j = 0; i < m; i++, j++) {
    q.set(j, p[i]);
    q.setmsk(j, p.msk(i));
  }
  p = q;
  return k;
}

Value *interpreter::vref(Value *x, path p)
{
  // walk an expression value x to find the subterm at p
  Env &e = act_env();
  Builder& b = e.builder;
  size_t n = p.len();
  Value *tmp = 0;
  for (size_t i = 0; i < n; ) {
    if (p.msk(i)) {
      // matrix path
      uint32_t r = argidx(p, i), c = argidx(p, i);
      Function *f = module->getFunction("matrix_elem_at2");
      x = b.CreateCall3(f, x, UInt(r), UInt(c));
      if (i < n) tmp = x;
    } else {
      x = e.CreateLoadGEP(x, Zero, SubFldIndex(p[i]), mklabel("x", i, p[i]+1));
      i++;
    }
  }
  if (tmp) {
    // collect temporaries
    b.CreateCall(module->getFunction("pure_new"), x);
    b.CreateCall(module->getFunction("pure_freenew"), tmp);
    b.CreateCall(module->getFunction("pure_unref"), x);
  }
  return x;
}

Value *interpreter::vref(int32_t tag, path p)
{
  // local arg reference
  Env &e = act_env();
  Builder& b = e.builder;
  uint32_t k = 0;
  if (e.b)
    // pattern binding
    assert(e.n==1);
  else
    k = argno(e.n, p);
  Value *v = e.args[k];
  size_t n = p.len();
  Value *tmp = 0;
  for (size_t i = 0; i < n; ) {
    if (p.msk(i)) {
      // matrix path
      uint32_t r = argidx(p, i), c = argidx(p, i);
      Function *f = module->getFunction("matrix_elem_at2");
      v = b.CreateCall3(f, v, UInt(r), UInt(c));
      if (i < n) tmp = v;
    } else {
      v = e.CreateLoadGEP(v, Zero, SubFldIndex(p[i]), mklabel("x", i, p[i]+1));
      i++;
    }
  }
  if (tmp) {
    // collect temporaries
    b.CreateCall(module->getFunction("pure_new"), v);
    b.CreateCall(module->getFunction("pure_freenew"), tmp);
    b.CreateCall(module->getFunction("pure_unref"), v);
  }
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

Value *interpreter::fref(int32_t tag, uint8_t idx, bool defer)
{
  // local function reference; box the function as a value on the fly
  assert(!envstk.empty());
  if (idx == 0) {
    // function in current environment ('with'-bound)
    Env& f = *act_env().fmap.act()[tag];
    return fbox(f, defer);
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
  if (f.n == 0 && !defer)
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
		envptr(f.local), f.n, x);
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
  else if (n+m > 0 || !debugging) {
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

void interpreter::backtrace(ostream& out)
{
  if (debugging && !bt.empty()) out << bt;
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
  fun_body(pm, 0, nodefault);
  return f;
}

Function *interpreter::fun_prolog(string name)
{
  Env& f = act_env();
  if (f.f==0) {
    // argument types
    vector<llvm_const_Type*> argt(f.n, ExprPtrTy);
    assert(f.m == 0 || f.local);
    if (f.m > 0) argt.insert(argt.begin(), int32_type());
    // function type
    FunctionType *ft = func_type(ExprPtrTy, argt, false);
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
	// type predicates always have internal linkage
	is_type(name) ||
	// global Pure functions use internal linkage if they would shadow a C
	// function:
	have_c_func ||
	// anonymous and private functions and operators use internal linkage,
	// too:
	f.tag == 0 || symtab.sym(f.tag).priv ||
	symtab.sym(f.tag).prec < PREC_MAX || symtab.sym(f.tag).fix == outfix)
      scope = Function::InternalLinkage;
#if USE_FASTCC
    if (use_fastcc && (f.local || !is_init(name)))
      cc = CallingConv::Fast;
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
      CallInst* v = f.builder.CreateCall(f.f, mkargs(myargs));
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
	raw_ostream& out = outs();
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

void interpreter::fun_body(matcher *pm, matcher *mxs, bool nodefault)
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
  // emit extra signal and stack checks, if requested
  if (checks) {
    vector<Value*> argv;
    f.builder.CreateCall(module->getFunction("pure_checks"));
  }
  BasicBlock *failedbb = basic_block("failed");
  // emit the matching code
  if (debugging && !is_init(f.name)) debug_rule(0);
  complex_match(pm, mxs, failedbb);
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
      defaultv = call("pure_clos", false, f.tag, 0, info.f,
		      NullPtr, f.n, x);
    } else
      defaultv =
	call("pure_clos", f.local, f.tag, f.getkey(), f.h,
	     envptr(f.local), f.n, x);
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
    raw_ostream& out = outs();
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

void interpreter::verify_tag(Value *v, int32_t tag, BasicBlock *failedbb)
{
  // check that the given expression value has the given tag, jump to the
  // given label otherwise
  Env& f = act_env();
  assert(f.f!=0);
  BasicBlock *okbb = basic_block("ok");
  f.builder.CreateCondBr(check_tag(v, tag), okbb, failedbb);
  f.f->getBasicBlockList().push_back(okbb);
  f.builder.SetInsertPoint(okbb);
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
  const trans& t = s->tr.front();
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
  case EXPR::MATRIX: {
    // first do a quick check on the tag so that we may avoid an expensive
    // call if the tags don't match
    BasicBlock *okbb = basic_block("ok");
    if (!tagv) tagv = f.CreateLoadGEP(x, Zero, Zero, "tag");
    SwitchInst *sw = f.builder.CreateSwitch(tagv, failedbb, 4);
    sw->addCase(SInt(EXPR::MATRIX), okbb);
    sw->addCase(SInt(EXPR::DMATRIX), okbb);
    sw->addCase(SInt(EXPR::CMATRIX), okbb);
    sw->addCase(SInt(EXPR::IMATRIX), okbb);
    // next check that the dimensions match
    f.f->getBasicBlockList().push_back(okbb);
    f.builder.SetInsertPoint(okbb);
    okbb = basic_block("check");
    Value *ok = f.builder.CreateCall3(module->getFunction("matrix_check"),
				      x, UInt(t.n), UInt(t.m));
    f.builder.CreateCondBr(ok, okbb, failedbb);
    // finally match the elements
    s = t.st;
    for (uint32_t i = 0; i < t.n; i++)
      for (uint32_t j = 0; j < t.m; j++) {
	/* We only match the element if there's actually anything to match
	   (not just an unqualified var), otherwise we just skip to the next
	   transition. */
	assert(s->tr.size() == 1);
	const trans& t = s->tr.front();
	if (t.tag == EXPR::VAR && t.ttag == 0) {
	  s = t.st; continue;
	}
	f.f->getBasicBlockList().push_back(okbb);
	f.builder.SetInsertPoint(okbb);
	okbb = basic_block("check");
	Value *y = f.builder.CreateCall3
	  (module->getFunction("matrix_elem_at2"), x, UInt(i), UInt(j));
	BasicBlock *elem_okbb = basic_block("elem_ok");
	BasicBlock *elem_nokbb = basic_block("elem_failed");
	simple_match(y, s, elem_okbb, elem_nokbb);
	// collect temporaries
	f.f->getBasicBlockList().push_back(elem_okbb);
	f.builder.SetInsertPoint(elem_okbb);
	f.builder.CreateCall(module->getFunction("pure_freenew"), y);
	f.builder.CreateBr(okbb);
	f.f->getBasicBlockList().push_back(elem_nokbb);
	f.builder.SetInsertPoint(elem_nokbb);
	f.builder.CreateCall(module->getFunction("pure_freenew"), y);
	f.builder.CreateBr(failedbb);
      }
    f.f->getBasicBlockList().push_back(okbb);
    f.builder.SetInsertPoint(okbb);
    f.builder.CreateBr(matchedbb);
    break;
  }
  case EXPR::PTR:
  case EXPR::WRAP:
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

void interpreter::complex_match(matcher *pm, matcher *mxs,
				BasicBlock *failedbb)
{
  Env& f = act_env();
  assert(f.f!=0);
  // Check to see if this is just a trie for a single unguarded
  // pattern-binding rule, then we can employ the simple matcher instead.
  if (f.n == 1 && f.b && pm && !mxs &&
      pm->r.size() == 1 && pm->r[0].qual.is_null() &&
      !pm->r[0].rhs.is_guarded()) {
    Value *arg = f.args[0];
    // emit the matching code
    BasicBlock *matchedbb = basic_block("matched");
    state *start = pm->start;
    simple_match(arg, start, matchedbb, failedbb);
    // matched => emit code for the reduct, and return the result
    f.f->getBasicBlockList().push_back(matchedbb);
    f.builder.SetInsertPoint(matchedbb);
    if (!pm->r[0].vi.guards.empty()) {
      // verify guards
      for (vguardl::const_iterator it = pm->r[0].vi.guards.begin(),
	     end = pm->r[0].vi.guards.end(); it != end; ++it) {
	BasicBlock *checkedbb = basic_block("typechecked");
	vector<Value*> args(2);
	args[0] = SInt(it->ttag);
	args[1] = vref(it->tag, it->p);
	Value *check =
	  f.builder.CreateCall(module->getFunction("pure_typecheck"),
			       mkargs(args));
	f.builder.CreateCondBr(check, checkedbb, failedbb);
	f.f->getBasicBlockList().push_back(checkedbb);
	f.builder.SetInsertPoint(checkedbb);
      }
    }
    if (!pm->r[0].vi.eqns.empty()) {
      // check non-linearities
      for (veqnl::const_iterator it = pm->r[0].vi.eqns.begin(),
	     end = pm->r[0].vi.eqns.end(); it != end; ++it) {
	BasicBlock *checkedbb = basic_block("checked");
	vector<Value*> args(2);
	args[0] = vref(it->tag, it->p);
	args[1] = vref(it->tag, it->q);
	Value *check = f.builder.CreateCall(module->getFunction("same"),
					    mkargs(args));
	f.builder.CreateCondBr(check, checkedbb, failedbb);
	f.f->getBasicBlockList().push_back(checkedbb);
	f.builder.SetInsertPoint(checkedbb);
      }
    }
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
    assert(pm || mxs);
    if (mxs) {
      /* Match an interface description. This case only arises in type
	 predicates. We always do this first, before the regular type rules.
	 If the match fails, we fall back to the regular type rules, if any,
	 or bail out with failure. */
      BasicBlock *iffailedbb = pm?basic_block("iffailed"):failedbb;
      // build the initial stack of expressions to be matched
      assert(f.n==1);
      list<Value*>xs(1, f.args[0]);
      // emit the matching code
      set<rulem> reduced;
      complex_match(mxs, xs, mxs->start, iffailedbb, reduced);
      // The rules to match an interface are generated automatically, so we
      // don't do any warnings about unreduced rules here.
      if (pm) {
	f.f->getBasicBlockList().push_back(iffailedbb);
	f.builder.SetInsertPoint(iffailedbb);
	// interface match failed, fall back to the regular type rules below
      }
    }
    if (pm) {
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
      // It is often an error (although not strictly forbidden) if there are
      // any rules left which will never be reduced, so warn about these.
      for (rulem r = 0; r < pm->r.size(); r++)
	if (reduced.find(r) == reduced.end()) {
	  const rule& rr = pm->r[r];
	  ostringstream msg;
	  msg << "warning: rule never reduced: " << rr << ";";
	  warning(msg.str());
	}
    }
  }
}

// helper macros to set up for the next state

#define next_state(t)						\
  do {								\
    state *s = t->st;						\
    list<Value*> ys = xs; ys.pop_front();			\
    if (ys.empty())						\
      try_rules(pm, s, failedbb, reduced, tmps);		\
    else							\
      complex_match(pm, ys, s, failedbb, reduced, tmps);	\
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
    complex_match(pm, ys, s, failedbb, reduced, tmps);		\
  } while (0)

// same as above, but handles the case of a matrix where we recurse into
// (potentially many) subterms

#define next_statem(t)						\
  do {								\
    state *s = t->st;						\
    list<Value*> ys = xs, zs; ys.pop_front();			\
    list<Value*> tmps1 = tmps;					\
    for (uint32_t i = 0; i < t->n; i++)				\
      for (uint32_t j = 0; j < t->m; j++) {			\
        Value *y = f.builder.CreateCall3			\
	  (module->getFunction("matrix_elem_at2"), x, UInt(i), UInt(j)); \
	zs.push_back(y);					\
	tmps1.push_front(y);					\
      }								\
    ys.splice(ys.begin(), zs);					\
    if (ys.empty())						\
      try_rules(pm, s, failedbb, reduced, tmps1);		\
    else							\
      complex_match(pm, ys, s, failedbb, reduced, tmps1);	\
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
				BasicBlock *failedbb, set<rulem>& reduced,
				const list<Value*>& tmps)
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
       where each entry points to a list of transitions. Matrix patterns are
       handled in the same fashion. */
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
      } else if (t->tag == EXPR::MATRIX) {
	// transition on a matrix, add it to the corresponding list
	tmap[t->tag].tlist.push_back(trans_info(&*t, bb));
	if (!tmap[t->tag].bb) {
	  // no outer label has been generated yet, do it now and add the
	  // target to the outer switch
	  tmap[t->tag].bb =
	    basic_block(mklabel("begin.state", s->s, t->n, t->m));
	  sw->addCase(SInt(EXPR::MATRIX), tmap[t->tag].bb);
	  // this can denote any type of matrix, add the other possible cases
	  sw->addCase(SInt(EXPR::DMATRIX), tmap[t->tag].bb);
	  sw->addCase(SInt(EXPR::CMATRIX), tmap[t->tag].bb);
	  sw->addCase(SInt(EXPR::IMATRIX), tmap[t->tag].bb);
	}
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
      } else if (tag == EXPR::MATRIX) {
	// outer label for a list of matrices of a given dimension; iterate
	// over all alternatives to resolve these in turn
	assert(info.bb && !info.tlist.empty());
	for (list<trans_info>::iterator l = info.tlist.begin();
	     l != info.tlist.end(); l++) {
	  list<trans_info>::iterator k = l; k++;
	  BasicBlock *okbb = l->bb;
	  BasicBlock *trynextbb =
	    basic_block(mklabel("next.state", s->s, l->t->n, l->t->m));
	  Value *ok = f.builder.CreateCall3(module->getFunction("matrix_check"),
					    x, UInt(l->t->n), UInt(l->t->m));
	  f.builder.CreateCondBr(ok, okbb, trynextbb);
	  f.f->getBasicBlockList().push_back(okbb);
	  f.builder.SetInsertPoint(okbb);
	  next_statem(l->t);
	  f.f->getBasicBlockList().push_back(trynextbb);
	  f.builder.SetInsertPoint(trynextbb);
	  if (k == info.tlist.end())
	    f.builder.CreateBr(retrybb);
	}
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
  else {
    // nothing matched in this state, collect temporaries and bail out
    for (list<Value*>::const_iterator it = tmps.begin(); it != tmps.end(); ++it)
      f.builder.CreateCall(module->getFunction("pure_freenew"), *it);
    f.builder.CreateBr(failedbb);
  }
}

/* Finally, the part of the algorithm which emits code for the rule list in a
   final state. Here we have to consider each matched rule in turn, emit code
   for the guard (if any) and execute the code for the first rule with a guard
   returning true. */

#if USE_FASTCC

/* Check for a tail-recursive type definition. At present, these are only
   recognized if the *last* rule of a type has a trivial rhs and is *directly*
   recursive in the *last* type guard on the lhs of the rule. Also, the rule
   may not contain any non-linearities since these are always checked *after*
   the type guards for efficiency. While these are rather strict limitations,
   they work reasonably well for simple recursive types such as the recursive
   list type. More general schemes are conceivable, but only at the cost of
   substantial overhead and they're probably not worth the effort anyway. */

static bool have_tail_guard(matcher *pm, state *s, int32_t tag)
{
  const rulev& rules = pm->r;
  const ruleml& rl = s->r;
  if (rl.empty()) return false;
  rulem r = rl.back();
  const rule& rr = rules[r];
  vguardl::const_reverse_iterator last = rr.vi.guards.rbegin(),
    rend = rr.vi.guards.rend();
  int32_t ival;
  return last != rend &&
    rr.qual.is_null() && !rr.rhs.is_guarded() &&
    rr.rhs.is_int(ival) && ival &&
    last->ttag == tag;
}

#endif

void interpreter::try_rules(matcher *pm, state *s, BasicBlock *failedbb,
			    set<rulem>& reduced, const list<Value*>& tmps)
{
  Env& f = act_env();
  assert(s->tr.empty()); // we're in a final state here
  for (list<Value*>::const_iterator it = tmps.begin(); it != tmps.end(); ++it)
    f.builder.CreateCall(module->getFunction("pure_freenew"), *it);
  const rulev& rules = pm->r;
  // Check if we're compiling a type predicate.
  bool have_type = is_type(f.name);
  env::iterator ty = have_type?typeenv.find(f.tag):typeenv.end();
  assert(!have_type || ty != typeenv.end());
  // If so, check if this is actually the interface part of the type; this
  // case must be treated separately.
  bool have_iface = have_type && pm == ty->second.mxs;
  // Also, check whether the type checking code is eligible for tail call
  // elimination (this can only be done in the interface part if there is no
  // regular type definition).
  bool tail = have_type && (!have_iface || !ty->second.m);
  assert(have_iface ||
	 f.fmap.root.size() == 1 || f.fmap.root.size() == rules.size());
  const ruleml& rl = s->r;
  ruleml::const_iterator r = rl.begin();
  assert(r != rl.end());
  assert(f.fmap.idx == 0);
  BasicBlock* rulebb = basic_block(mklabel("rule.state", s->s, rl.front()));
  f.builder.CreateBr(rulebb);
#if USE_FASTCC
  const bool have_tail = tail && !debugging && use_fastcc && have_type &&
    have_tail_guard(pm, s, f.tag);
#else
  const bool have_tail = false;
#endif
  while (r != rl.end()) {
    const rule& rr = rules[*r];
    reduced.insert(*r);
    // Select the proper environment for the local functions. This is to be
    // skipped for the interface part of a type definition which never has any
    // local environment.
    if (!have_iface) f.fmap.select(*r);
    f.f->getBasicBlockList().push_back(rulebb);
    f.builder.SetInsertPoint(rulebb);
    BasicBlock *okbb = basic_block("ok");
    // determine the next rule block ('failed' if none)
    BasicBlock *nextbb;
    ruleml::const_iterator next_r = r;
    if (++next_r != rl.end())
      nextbb = basic_block(mklabel("rule.state", s->s, *next_r));
    else
      nextbb = failedbb;
    bool tail = have_tail && nextbb == failedbb && rr.vi.eqns.empty();
    if (!rr.vi.guards.empty()) {
      // verify guards
      for (vguardl::const_iterator it = rr.vi.guards.begin(),
	     end = rr.vi.guards.end(); it != end; ) {
	vguardl::const_iterator next_it = it;
	// Skip the last guard in a tail-recursive type definition. We'll
	// handle it below at the end of the code for this state.
	if (++next_it == end && tail) break;
	BasicBlock *checkedbb = basic_block("typechecked");
	vector<Value*> args(2);
	args[0] = SInt(it->ttag);
	args[1] = vref(it->tag, it->p);
	Value *check =
	  f.builder.CreateCall(module->getFunction("pure_typecheck"),
			       mkargs(args));
	f.builder.CreateCondBr(check, checkedbb, nextbb);
	f.f->getBasicBlockList().push_back(checkedbb);
	f.builder.SetInsertPoint(checkedbb);
	it = next_it;
      }
    }
    if (!rr.vi.eqns.empty()) {
      // check non-linearities
      for (veqnl::const_iterator it = rr.vi.eqns.begin(),
	     end = rr.vi.eqns.end(); it != end; ++it) {
	BasicBlock *checkedbb = basic_block("checked");
	vector<Value*> args(2);
	args[0] = vref(it->tag, it->p);
	args[1] = vref(it->tag, it->q);
	Value *check = f.builder.CreateCall(module->getFunction("same"),
					    mkargs(args));
	f.builder.CreateCondBr(check, checkedbb, nextbb);
	f.f->getBasicBlockList().push_back(checkedbb);
	f.builder.SetInsertPoint(checkedbb);
      }
    }
    if (debugging && !is_init(f.name)) debug_rule(&rr);
#if DEBUG>1
    if (!is_init(f.name)) { ostringstream msg;
      msg << "complex match " << f.name << ", trying rule #" << *r;
      debug(msg.str().c_str()); }
#endif
    if (rr.vi.guards.empty() && rr.vi.eqns.empty() &&
	rr.qual.is_null() && !rr.rhs.is_guarded()) {
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
    r = next_r;
    if (tail)
      ; // handled below
    else if (!rr.qual.is_null()) {
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
    } else if (rr.rhs.is_guarded()) {
      // guard wrapped in a closure; generate code for the rhs and to check
      // for null results
      retv = codegen(rr.rhs);
      condv = f.builder.CreateICmpNE(retv, NullExprPtr, "cond");
    }
    if (condv != 0)
      f.builder.CreateCondBr(condv, okbb, nextbb);
    else
      f.builder.CreateBr(okbb);
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
      if (f.n+f.m != 0 || !debugging) {
	// do cleanup
	Function *free_fun = module->getFunction("pure_pop_args");
	f.builder.CreateCall3(free_fun, retv, UInt(f.n), UInt(f.m));
      }
      if (rp) debug_redn(rp, retv);
      f.builder.CreateRet(retv);
    } else if (tail) {
      // Tail-recursive type rule. Perform a direct tail call on the rightmost
      // type guard.
      const vguard& last = rr.vi.guards.back();
      Function *tailfun = globaltypes[last.ttag].f;
      vector<Value*> argv(1);
      assert(f.n==1 && f.m==0);
      argv[0] = vref(last.tag, last.p);
      f.CreateCall(module->getFunction("pure_push_arg"), argv);
      f.CreateRet(f.CreateCall(tailfun, argv));
    } else
      toplevel_codegen(rr.rhs, rp);
    rulebb = nextbb;
  }
  if (!have_iface) f.fmap.first();
}

/* Make sure to make this the very last thing in this file. TargetSelect.h
   pulls in LLVM's config.h file which may stomp on our own config
   settings! */

#if LLVM26
#ifdef HAVE_LLVM_SUPPORT_TARGETSELECT_H
// LLVM 3.0 or later
#include <llvm/Support/TargetSelect.h>
#else
#include <llvm/Target/TargetSelect.h>
#endif

void interpreter::init_llvm_target()
{
  llvm::InitializeNativeTarget();
}
#endif
