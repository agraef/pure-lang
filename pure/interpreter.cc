#include "interpreter.hh"
#include "parser.hh"
#include <sstream>
#include <stdarg.h>
#include <sys/types.h>
#include <regex.h>
#include <fnmatch.h>
#include <glob.h>

#include <llvm/CallingConv.h>
#include <llvm/PassManager.h>
#include <llvm/System/DynamicLibrary.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

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

static void* resolve_external(const std::string& name)
{
  // This is just to give a little more informative error message before we
  // bail out anyway.
  cout.flush();
  cerr << "error trying to resolve external: " << name << endl;
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

interpreter::interpreter()
  : verbose(0), interactive(false), ttymode(false), override(false),
    stats(false), temp(0),
    ps("> "), libdir(""), histfile("/.pure_history"), modname("pure"),
    nerrs(0), modno(-1), modctr(0), source_s(0), result(0), mem(0), exps(0),
    tmps(0), module(0), JIT(0), FPM(0), fptr(0)
{
  if (!g_interp) {
    g_interp = this;
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
  }

  sstk_sz = 0; sstk_cap = 0x10000; // 64K
  sstk = (pure_expr**)malloc(sstk_cap*sizeof(pure_expr*));
  assert(sstk);

  // Initialize the JIT.

  using namespace llvm;

  module = new Module("pure");
  ModuleProvider *PM = new ExistingModuleProvider(module);
  JIT = ExecutionEngine::create(PM);
  FPM = new FunctionPassManager(PM);

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
    VoidPtrTy = PointerType::get(StructType::get(elts), 0);
  }

  // Char pointer type.
  CharPtrTy = PointerType::get(Type::Int8Ty, 0);

  // int and double pointers.
  IntPtrTy = PointerType::get(Type::Int32Ty, 0);
  DoublePtrTy = PointerType::get(Type::DoubleTy, 0);

  // Complex numbers (complex double).
  {
    std::vector<const Type*> elts;
    elts.push_back(ArrayType::get(Type::DoubleTy, 2));
    //elts.push_back(Type::DoubleTy);
    //elts.push_back(Type::DoubleTy);
    ComplexTy = StructType::get(elts);
    ComplexPtrTy = PointerType::get(ComplexTy, 0);
  }

  // GSL matrix types. These are used to marshall GSL matrices in the C
  // interface.
  {
    std::vector<const Type*> elts;
    if (sizeof(size_t) == 4) {
      elts.push_back(Type::Int32Ty);	// size1
      elts.push_back(Type::Int32Ty);	// size2
      elts.push_back(Type::Int32Ty);	// tda
    } else {
      assert(sizeof(size_t) == 8);
      elts.push_back(Type::Int64Ty);	// size1
      elts.push_back(Type::Int64Ty);	// size2
      elts.push_back(Type::Int64Ty);	// tda
    }
    elts.push_back(VoidPtrTy);		// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(Type::Int32Ty);	// owner
    GSLMatrixTy = StructType::get(elts);
    module->addTypeName("struct.gsl_matrix", GSLMatrixTy);
    GSLMatrixPtrTy = PointerType::get(GSLMatrixTy, 0);
  }
  {
    std::vector<const Type*> elts;
    if (sizeof(size_t) == 4) {
      elts.push_back(Type::Int32Ty);	// size1
      elts.push_back(Type::Int32Ty);	// size2
      elts.push_back(Type::Int32Ty);	// tda
    } else {
      assert(sizeof(size_t) == 8);
      elts.push_back(Type::Int64Ty);	// size1
      elts.push_back(Type::Int64Ty);	// size2
      elts.push_back(Type::Int64Ty);	// tda
    }
    elts.push_back(DoublePtrTy);	// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(Type::Int32Ty);	// owner
    GSLDoubleMatrixTy = StructType::get(elts);
    module->addTypeName("struct.gsl_matrix_double", GSLDoubleMatrixTy);
    GSLDoubleMatrixPtrTy = PointerType::get(GSLDoubleMatrixTy, 0);
  }
  {
    std::vector<const Type*> elts;
    if (sizeof(size_t) == 4) {
      elts.push_back(Type::Int32Ty);	// size1
      elts.push_back(Type::Int32Ty);	// size2
      elts.push_back(Type::Int32Ty);	// tda
    } else {
      assert(sizeof(size_t) == 8);
      elts.push_back(Type::Int64Ty);	// size1
      elts.push_back(Type::Int64Ty);	// size2
      elts.push_back(Type::Int64Ty);	// tda
    }
    elts.push_back(ComplexPtrTy);	// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(Type::Int32Ty);	// owner
    GSLComplexMatrixTy = StructType::get(elts);
    module->addTypeName("struct.gsl_matrix_complex", GSLComplexMatrixTy);
    GSLComplexMatrixPtrTy = PointerType::get(GSLComplexMatrixTy, 0);
  }
  {
    std::vector<const Type*> elts;
    if (sizeof(size_t) == 4) {
      elts.push_back(Type::Int32Ty);	// size1
      elts.push_back(Type::Int32Ty);	// size2
      elts.push_back(Type::Int32Ty);	// tda
    } else {
      assert(sizeof(size_t) == 8);
      elts.push_back(Type::Int64Ty);	// size1
      elts.push_back(Type::Int64Ty);	// size2
      elts.push_back(Type::Int64Ty);	// tda
    }
    elts.push_back(IntPtrTy);		// data
    elts.push_back(VoidPtrTy);		// block
    elts.push_back(Type::Int32Ty);	// owner
    GSLIntMatrixTy = StructType::get(elts);
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
    PATypeHolder StructTy = OpaqueType::get();
    std::vector<const Type*> elts;
    elts.push_back(Type::Int32Ty);
    elts.push_back(Type::Int32Ty);
    elts.push_back(PointerType::get(StructTy, 0));
    elts.push_back(PointerType::get(StructTy, 0));
    ExprTy = StructType::get(elts);
    cast<OpaqueType>(StructTy.get())->refineAbstractTypeTo(ExprTy);
    ExprTy = cast<StructType>(StructTy.get());
    module->addTypeName("struct.expr", ExprTy);
  }
  {
    std::vector<const Type*> elts;
    elts.push_back(Type::Int32Ty);
    elts.push_back(Type::Int32Ty);
    elts.push_back(Type::Int32Ty);
    IntExprTy = StructType::get(elts);
    module->addTypeName("struct.intexpr", IntExprTy);
  }
  {
    std::vector<const Type*> elts;
    elts.push_back(Type::Int32Ty);
    elts.push_back(Type::Int32Ty);
    elts.push_back(Type::DoubleTy);
    DblExprTy = StructType::get(elts);
    module->addTypeName("struct.dblexpr", DblExprTy);
  }
  {
    std::vector<const Type*> elts;
    elts.push_back(Type::Int32Ty);
    elts.push_back(Type::Int32Ty);
    elts.push_back(CharPtrTy);
    StrExprTy = StructType::get(elts);
    module->addTypeName("struct.strexpr", StrExprTy);
  }
  {
    std::vector<const Type*> elts;
    elts.push_back(Type::Int32Ty);
    elts.push_back(Type::Int32Ty);
    elts.push_back(VoidPtrTy);
    PtrExprTy = StructType::get(elts);
    module->addTypeName("struct.ptrexpr", PtrExprTy);
  }

  // Corresponding pointer types.

  ExprPtrTy = PointerType::get(ExprTy, 0);
  ExprPtrPtrTy = PointerType::get(ExprPtrTy, 0);
  IntExprPtrTy = PointerType::get(IntExprTy, 0);
  DblExprPtrTy = PointerType::get(DblExprTy, 0);
  StrExprPtrTy = PointerType::get(StrExprTy, 0);
  PtrExprPtrTy = PointerType::get(PtrExprTy, 0);

  sstkvar = new GlobalVariable
    (ExprPtrPtrTy, false, GlobalVariable::InternalLinkage, 0, "$$sstk$$",
     module);
  JIT->addGlobalMapping(sstkvar, &sstk);
  fptrvar = new GlobalVariable
    (VoidPtrTy, false, GlobalVariable::InternalLinkage, 0, "$$fptr$$", module);
  JIT->addGlobalMapping(fptrvar, &fptr);

  // Add prototypes for the runtime interface and enter the corresponding
  // function pointers into the runtime map.

  declare_extern((void*)pure_clos,
		 "pure_clos",       "expr*", -7, "bool", "bool", "int", "int",
		                                 "void*", "void*", "int");
  declare_extern((void*)pure_call,
		 "pure_call",       "expr*",  1, "expr*");
  declare_extern((void*)pure_force,
		 "pure_force",      "expr*",  1, "expr*");
  declare_extern((void*)pure_const,
		 "pure_const",      "expr*",  1, "int");
  declare_extern((void*)pure_int,
		 "pure_int",        "expr*",  1, "int");
  declare_extern((void*)pure_long,
		 "pure_long",       "expr*",  1, "long");
  declare_extern((void*)pure_bigint,
		 "pure_bigint",     "expr*",  2, "int",
		 sizeof(mp_limb_t)==8?"long*":"int*");
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

  declare_extern((void*)pure_matrix_rows,
		 "pure_matrix_rows", "expr*",    -1, "int");
  declare_extern((void*)pure_matrix_columns,
		 "pure_matrix_columns", "expr*", -1, "int");
  declare_extern((void*)pure_symbolic_matrix,
		 "pure_symbolic_matrix", "expr*", 1, "void*");
  declare_extern((void*)pure_double_matrix,
		 "pure_double_matrix", "expr*",   1, "void*");
  declare_extern((void*)pure_complex_matrix,
		 "pure_complex_matrix", "expr*",  1, "void*");
  declare_extern((void*)pure_int_matrix,
		 "pure_int_matrix", "expr*",      1, "void*");

  declare_extern((void*)pure_listl,
		 "pure_listl",      "expr*", -1, "int");
  declare_extern((void*)pure_tuplel,
		 "pure_tuplel",     "expr*", -1, "int");

  declare_extern((void*)pure_cmp_bigint,
		 "pure_cmp_bigint", "int",    3, "expr*", "int",
		 sizeof(mp_limb_t)==8?"long*":"int*");
  declare_extern((void*)pure_cmp_string,
		 "pure_cmp_string", "int",    2, "expr*", "char*");

  declare_extern((void*)pure_get_cstring,
		 "pure_get_cstring", "char*", 1, "expr*");
  declare_extern((void*)pure_free_cstrings,
		 "pure_free_cstrings","void", 0);
  declare_extern((void*)pure_get_bigint,
		 "pure_get_bigint",  "void*", 1, "expr*");
  declare_extern((void*)pure_get_long,
		 "pure_get_long",    "long",  1, "expr*");
  declare_extern((void*)pure_get_int,
		 "pure_get_int",     "int",   1, "expr*");
  declare_extern((void*)pure_get_matrix,
		 "pure_get_matrix",  "void*", 1, "expr*");

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
		 "pure_pop_arg",   "void",    0);
  declare_extern((void*)pure_pop_tail_arg,
		 "pure_pop_tail_arg", "void", 0);

  declare_extern((void*)pure_debug,
		 "pure_debug",      "void",  -2, "int", "char*");
}

interpreter::~interpreter()
{
  // free expression memory
  pure_mem *m = mem, *n;
  while (m) {
    n = m->next;
    delete m;
    m = n;
  }
  // get rid of global environments and the LLVM data
  globalfuns.clear(); globalvars.clear();
  if (JIT) delete JIT;
  if (FPM) delete FPM;
  // if this was the global interpreter, reset it now
  if (g_interp == this) g_interp = 0;
}

void interpreter::init_sys_vars(const string& version,
				const string& host,
				const list<string>& argv)
{
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
  defn("version",	pure_cstring_dup(version.c_str()));
  defn("sysinfo",	pure_cstring_dup(host.c_str()));
#ifdef HAVE_GSL
  defn("gsl_version",	pure_cstring_dup(gsl_version));
#endif
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
    msg << l << ": " << m1 << endl;
    errmsg += msg.str();
  } else {
    cout.flush();
    cerr << l << ": " << m1 << endl;
  }
}

void
interpreter::error(const string& m)
{
  nerrs++;
  if (source_s) {
    ostringstream msg;
    msg << m << endl;
    errmsg += msg.str();
  } else {
    cout.flush();
    cerr << m << endl;
  }
}

void
interpreter::warning(const yy::location& l, const string& m)
{
  if (!source_s) {
    cout.flush();
    cerr << l << ": " << m << endl;
  }
}

void
interpreter::warning(const string& m)
{
  if (!source_s) {
    cout.flush();
    cerr << m << endl;
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
    return s[0]=='/' || s.size() >= 2 && s[1] == ':';
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
  chdir(cwd);
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
    if (name.substr(name.size()-strlen(DLLEXT)) != DLLEXT)
      dllname += DLLEXT;
    // First try to open the library under the given name.
    string aname = searchlib(srcdir, libdir, librarydirs, name);
    if (!llvm::sys::DynamicLibrary::LoadLibraryPermanently(aname.c_str(), &msg))
      return 0;
    else if (dllname == name)
      throw err(msg);
    aname = searchlib(srcdir, libdir, librarydirs, dllname);
    // Now try the name with DLLEXT added.
    if (llvm::sys::DynamicLibrary::LoadLibraryPermanently(aname.c_str(), &msg))
      throw err(msg);
    return 0;
  }
  // ordinary source file
  string fname = searchdir(srcdir, libdir, includedirs, s, check);
  if (check && sources.find(fname) != sources.end())
    // already loaded, skip
    return 0;
  // save local data
  bool l_interactive = interactive;
  string l_source = source;
  int l_nerrs = nerrs;
  uint8_t l_temp = temp;
  const char *l_source_s = source_s;
  string l_srcdir = srcdir;
  int32_t l_modno = modno;
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
  srcdir = dirname(fname);
  if (sticky)
    ; // keep the current module
  else
    modno = modctr++;
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
  srcdir = l_srcdir;
  modno = l_modno;
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
  bool l_interactive = interactive;
  string l_source = source;
  int l_nerrs = nerrs;
  const char *l_source_s = source_s;
  string l_srcdir = srcdir;
  int32_t l_modno = modno;
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
  errmsg.clear();
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
  interactive = l_interactive;
  source = l_source;
  source_s = 0;
  nerrs = l_nerrs;
  source_s = l_source_s;
  srcdir = l_srcdir;
  modno = l_modno;
  // return last computed result, if any
  return result;
}

// Evaluate an expression.

pure_expr *interpreter::eval(expr& x)
{
  globals g;
  save_globals(g);
  pure_expr *e, *res = eval(x, e);
  if (!res && e) pure_free(e);
  restore_globals(g);
  return res;
}

pure_expr *interpreter::eval(expr& x, pure_expr*& e)
{
  globals g;
  save_globals(g);
  compile();
  // promote type tags and substitute macros and constants:
  env vars; expr u = csubst(macsubst(subst(vars, x)));
  compile(u);
  x = u;
  pure_expr *res = doeval(u, e);
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
  expr rhs = csubst(macsubst(subst(vars, x)));
  expr lhs = bind(vars, pat);
  build_env(vars, lhs);
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
  pure_expr *res = dodefn(vars, lhs, rhs, e);
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

// Define global constants (macro definitions).

pure_expr *interpreter::const_defn(expr pat, expr& x)
{
  globals g;
  save_globals(g);
  pure_expr *e, *res = const_defn(pat, x, e);
  if (!res && e) pure_free(e);
  restore_globals(g);
  return res;
}

expr interpreter::pure_expr_to_expr(pure_expr *x)
{
  char test;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax)
    throw err("expression too deep in constant definition");
  switch (x->tag) {
  case EXPR::APP:
    return expr(pure_expr_to_expr(x->data.x[0]),
		pure_expr_to_expr(x->data.x[1]));
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
    if (x->data.p != 0)
      // Only null pointer constants permitted right now.
      throw err("pointer must be null in constant definition");
    return expr(EXPR::PTR, x->data.p);
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
      return expr(EXPR::MATRIX, m);
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
      return expr(EXPR::MATRIX, m);
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
      return expr(EXPR::MATRIX, m);
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
      symbol *rect = symtab.complex_rect_sym(true);
      expr f = rect->x;
      for (size_t i = 0; i < m->size1; i++) {
	xs->push_back(exprl());
	exprl& ys = xs->back();
	for (size_t j = 0; j < m->size2; j++) {
	  expr u = expr(EXPR::DBL, m->data[2*(i * m->tda + j)]);
	  expr v = expr(EXPR::DBL, m->data[2*(i * m->tda + j) + 1]);
	  ys.push_back(expr(f, u, v));
	}
      }
      return expr(EXPR::MATRIX, m);
    } else
      return expr(EXPR::MATRIX, new exprll);
#else
    throw err("GSL matrices not supported in this implementation");
    return expr(EXPR::MATRIX, new exprll);
#endif
  }
  default:
    assert(x->tag > 0);
    if (x->data.clos && x->data.clos->local)
      // There's no way we can capture a local function in a compile time
      // expression right now, so we have to forbid this, too.
      throw err("anonymous closure not permitted in constant definition");
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
  expr rhs = csubst(macsubst(subst(vars, x)));
  expr lhs = bind(vars, pat);
  build_env(vars, lhs);
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
  pure_expr *res = doeval(rhs, e);
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
    os << endl;
  }
  for (size_t i = 0, n = e->fmap.size(); i < n; i++) {
    os << blanks << "FMAP #" << i << ":\n";
    indent += 2;
    map<int32_t,Env>::const_iterator fi;
    for (fi = e->fmap[i].begin(); fi != e->fmap[i].end(); fi++) {
      const Env& e = fi->second;
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
	if (verbose&verbosity::code) std::cout << *info.m << endl;
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
	llvm::cerr << "JIT " << f.f->getName() << " -> " << f.fp << endl;
#endif
	// do a direct call to the runtime to create the fbox and cache it in
	// a global variable
	pure_expr *fv = pure_clos(false, false, f.tag, f.n, f.fp, 0, 0);
	GlobalVar& v = globalvars[f.tag];
	if (!v.v) {
	  v.v = new GlobalVariable
	    (ExprPtrTy, false, GlobalVariable::InternalLinkage, 0,
	     mkvarlabel(f.tag), module);
	  JIT->addGlobalMapping(v.v, &v.x);
	}
	if (v.x) pure_free(v.x); v.x = pure_new(fv);
#if DEBUG>1
	llvm::cerr << "global " << &v.x << " (== "
		   << JIT->getPointerToGlobal(v.v) << ") -> "
		   << (void*)fv << endl;
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
  case EXPR::MATRIX:
    for (exprll::iterator xs = x.xvals()->begin(), end = x.xvals()->end();
	 xs != end; xs++)
      for (exprl::iterator ys = xs->begin(), end = xs->end();
	   ys != end; ys++) {
	compile(*ys);
      }
    break;
  case EXPR::APP:
    compile(x.xval1());
    compile(x.xval2());
    break;
  case EXPR::COND:
    compile(x.xval1());
    compile(x.xval2());
    compile(x.xval3());
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

void interpreter::declare(bool priv, prec_t prec, fix_t fix, list<string> *ids)
{
  for (list<string>::const_iterator it = ids->begin();
       it != ids->end(); ++it) {
    symbol* sym = symtab.xlookup(*it, priv?modno:-1);
    if (sym) {
      // crosscheck declarations
      if (sym->prec != prec || sym->fix != fix) {
	string id = *it;
	delete ids;
	throw err("conflicting fixity declaration for symbol '"+id+"'");
      }
    } else {
      int32_t tag = symtab.xsym(*it, prec, fix, priv?modno:-1).f;
      /* KLUDGE: Already create a globalvars entry here, so that the symbol is
	 properly recognized by the completion routines. */
      pure_expr *cv = pure_const(tag);
      assert(JIT);
      GlobalVar& v = globalvars[tag];
      if (!v.v) {
	v.v = new llvm::GlobalVariable
	  (ExprPtrTy, false, llvm::GlobalVariable::InternalLinkage, 0,
	   mkvarlabel(tag), module);
	JIT->addGlobalMapping(v.v, &v.x);
      }
      if (v.x) pure_free(v.x); v.x = pure_new(cv);
    }
  }
  delete ids;
}

void interpreter::exec(expr *x)
{
  last.clear();
  if (result) pure_free(result); result = 0;
  pure_expr *e, *res = eval(*x, e);
  if ((verbose&verbosity::defs) != 0) cout << *x << ";\n";
  if (!res) {
    ostringstream msg;
    if (e) {
      msg << "unhandled exception '" << e << "' while evaluating '"
	  << *x << "'";
      pure_free(e);
    } else
      msg << "unhandled exception while evaluating '" << *x << "'";
    throw err(msg.str());
  }
  result = pure_new(res);
  delete x;
  if (interactive) {
    cout << result << endl;
    if (stats)
      cout << ((double)clocks)/(double)CLOCKS_PER_SEC << "s\n";
  }
}

void interpreter::define(rule *r)
{
  last.clear();
  pure_expr *e, *res = defn(r->lhs, r->rhs, e);
  if ((verbose&verbosity::defs) != 0)
    cout << "let " << r->lhs << " = " << r->rhs << ";\n";
  if (!res) {
    ostringstream msg;
    if (e) {
      msg << "unhandled exception '" << e << "' while evaluating '"
	  << "let " << r->lhs << " = " << r->rhs << "'";
      pure_free(e);
    } else
      msg << "failed match while evaluating '"
	  << "let " << r->lhs << " = " << r->rhs << "'";
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
  pure_expr *e, *res = const_defn(r->lhs, r->rhs, e);
  if ((verbose&verbosity::defs) != 0)
    cout << "const " << r->lhs << " = " << r->rhs << ";\n";
  if (!res) {
    ostringstream msg;
    if (e) {
      msg << "unhandled exception '" << e << "' while evaluating '"
	  << "const " << r->lhs << " = " << r->rhs << "'";
      pure_free(e);
    } else
      msg << "failed match while evaluating '"
	  << "const " << r->lhs << " = " << r->rhs << "'";
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
      globenv.erase(it);
      clearsym(f);
    }
    it = macenv.find(f);
    if (it != macenv.end())
      macenv.erase(it);
  } else if (f == 0 && temp > 0) {
    // purge all temporary symbols
    for (env::iterator it = globenv.begin(); it != globenv.end(); ) {
      env::iterator jt = it; ++it;
      int32_t f = jt->first;
      env_info& info = jt->second;
      if (info.temp >= temp) {
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
  int32_t f; uint32_t argc = count_args(r.lhs, f);
  if (f <= 0)
    throw err("error in function definition (invalid head symbol)");
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
  assert(!r->lhs.is_null() && r->qual.is_null());
  closure(*r, false);
  int32_t f; uint32_t argc = count_args(r->lhs, f);
  if (f <= 0)
    throw err("error in macro definition (invalid head symbol)");
  env::iterator it = macenv.find(f), jt = globenv.find(f);
  const symbol& sym = symtab.sym(f);
  if (jt != globenv.end()) {
    if (it->second.t == env_info::cvar)
      throw err("symbol '"+sym.s+"' is already defined as a constant");
    else if (it->second.t == env_info::fvar)
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
  expr u = bind(vars, l, b), v = subst(vars, r);
  l = u; r = v;
}

void interpreter::closure(rule& r, bool b)
{
  env vars;
  expr u = expr(bind(vars, r.lhs, b)),
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
    if (sym.s != "_" &&
	(sym.prec < 10 || sym.fix == nullary || p.len() == 0 && !b ||
	 p.len() > 0 && p.last() == 0)) {
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
      if (sym.prec < 10 || sym.fix == nullary)
	throw err("error in pattern (bad variable symbol '"+sym.s+"')");
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
    if (f.ftag() == symtab.neg_sym().f || f.ftag() == symtab.not_sym().f ||
	f.ftag() == symtab.bitnot_sym().f)
      x.set_ttag(EXPR::INT);
  } else if (u.ttag() == EXPR::DBL) {
    // unary double operations
    if (f.ftag() == symtab.neg_sym().f) {
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
      if (f.ftag() == symtab.or_sym().f ||
	  f.ftag() == symtab.and_sym().f ||
	  f.ftag() == symtab.bitor_sym().f ||
	  f.ftag() == symtab.bitand_sym().f ||
	  f.ftag() == symtab.shl_sym().f ||
	  f.ftag() == symtab.shr_sym().f ||
	  f.ftag() == symtab.less_sym().f ||
	  f.ftag() == symtab.greater_sym().f ||
	  f.ftag() == symtab.lesseq_sym().f ||
	  f.ftag() == symtab.greatereq_sym().f ||
	  f.ftag() == symtab.equal_sym().f ||
	  f.ftag() == symtab.notequal_sym().f ||
	  f.ftag() == symtab.plus_sym().f ||
	  f.ftag() == symtab.minus_sym().f ||
	  f.ftag() == symtab.mult_sym().f ||
	  f.ftag() == symtab.div_sym().f ||
	  f.ftag() == symtab.mod_sym().f) {
	x.set_ttag(EXPR::INT);
      } else if (f.ftag() == symtab.fdiv_sym().f) {
	x.set_ttag(EXPR::DBL);
      }
    } else {
      // binary int/double operations
      if (f.ftag() == symtab.less_sym().f ||
	  f.ftag() == symtab.greater_sym().f ||
	  f.ftag() == symtab.lesseq_sym().f ||
	  f.ftag() == symtab.greatereq_sym().f ||
	  f.ftag() == symtab.equal_sym().f ||
	  f.ftag() == symtab.notequal_sym().f) {
	x.set_ttag(EXPR::INT);
      } else if (f.ftag() == symtab.plus_sym().f ||
		 f.ftag() == symtab.minus_sym().f ||
		 f.ftag() == symtab.mult_sym().f ||
		 f.ftag() == symtab.fdiv_sym().f) {
	x.set_ttag(EXPR::DBL);
      }
    }
  } else if (f.ftag() == symtab.or_sym().f || f.ftag() == symtab.and_sym().f)
    /* These two get special treatment, because they have to be evaluated in
       short-circuit mode. Operand types will be checked at runtime if
       necessary. */
    x.set_ttag(EXPR::INT);
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
    if (sym.prec < 10 || sym.fix == nullary || it == vars.end()) {
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
    if (it != funs.end())
      return expr(EXPR::FVAR, sym.f, idx);
    else
      return x;
  }
}

expr interpreter::csubst(expr x)
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
	vs.push_back(csubst(*ys));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP:
    if (x.xval1().tag() == symtab.amp_sym().f) {
      expr v = csubst(x.xval2());
      return expr(symtab.amp_sym().x, v);
    } else if (x.xval1().tag() == EXPR::APP &&
	       x.xval1().xval1().tag() == symtab.catch_sym().f) {
      expr u = csubst(x.xval1().xval2()),
	v = csubst(x.xval2());
      return expr(symtab.catch_sym().x, u, v);
    } else {
      expr u = csubst(x.xval1()),
	v = csubst(x.xval2());
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
    const symbol& sym = symtab.sym(x.tag());
    env::const_iterator it = globenv.find(sym.f);
    if (it != globenv.end() && it->second.t == env_info::cvar)
      // substitute constant value
      return *it->second.cval;
    else
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

expr interpreter::macsubst(expr x)
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
	vs.push_back(macsubst(*ys));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP: {
    expr u = macsubst(x.xval1()),
      v = macsubst(x.xval2());
    expr w = expr(u, v);
    return macval(w);
  }
  // conditionals:
  case EXPR::COND: {
    expr u = macsubst(x.xval1()),
      v = macsubst(x.xval2()),
      w = macsubst(x.xval3());
    return expr::cond(u, v, w);
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
    return macval(x);
  }
}

/* Perform a single macro reduction step. */

expr interpreter::varsubst(expr x, uint8_t offs)
{
  char test;
  if (x.is_null()) return x;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax)
    throw err("recursion too deep in macro expansion");
  switch (x.tag()) {
  case EXPR::VAR:
  case EXPR::FVAR:
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
	vs.push_back(varsubst(*ys, offs));
      }
    }
    return expr(EXPR::MATRIX, us);
  }
  // application:
  case EXPR::APP: {
    expr u = varsubst(x.xval1(), offs),
      v = varsubst(x.xval2(), offs);
    expr w = expr(u, v);
    return macval(w);
  }
  // conditionals:
  case EXPR::COND: {
    expr u = varsubst(x.xval1(), offs),
      v = varsubst(x.xval2(), offs),
      w = varsubst(x.xval3(), offs);
    return expr::cond(u, v, w);
  }
  // nested closures:
  case EXPR::LAMBDA: {
    expr u = x.xval1(), v = varsubst(x.xval2(), offs);
    return expr::lambda(u, v);
  }
  case EXPR::CASE: {
    expr u = varsubst(x.xval(), offs);
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs,	v = varsubst(it->rhs, offs),
	w = varsubst(it->qual, offs);
      s->push_back(rule(u, v, w));
    }
    return expr::cases(u, s);
  }
  case EXPR::WHEN: {
    const rulel *r = x.rules();
    rulel *s = new rulel;
    for (rulel::const_iterator it = r->begin(); it != r->end(); ++it) {
      expr u = it->lhs, v = varsubst(it->rhs, offs);
      s->push_back(rule(u, v));
    }
    expr u = varsubst(x.xval(), offs);
    return expr::when(u, s);
  }
  case EXPR::WITH: {
    expr u = varsubst(x.xval(), offs);
    const env *e = x.fenv();
    env *f = new env;
    for (env::const_iterator it = e->begin(); it != e->end(); ++it) {
      int32_t g = it->first;
      const env_info& info = it->second;
      const rulel *r = info.rules;
      rulel s;
      for (rulel::const_iterator jt = r->begin(); jt != r->end(); ++jt) {
	expr u = jt->lhs, v = varsubst(jt->rhs, offs),
	  w = varsubst(jt->qual, offs);
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
    return y;
  // lhs variable
  case EXPR::VAR:
    if (y.vidx() == idx) {
      /* Substitute the macro variables, which are the lhs values whose idx
	 match the current idx. Note that the deBruijn indices inside the
	 substituted value must then be shifted by idx, to accommodate for any
	 local environments inside the macro definition. */
      expr v = varsubst(subterm(x, y.vpath()), idx);
#if DEBUG>1
      std::cerr << "macro var: " << y << " = " << v
		<< " (" << (uint32_t)idx << ")" << endl;
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
      return expr(u, v);
    }
  // conditionals:
  case EXPR::COND: {
    expr u = macred(x, y.xval1(), idx),
      v = macred(x, y.xval2(), idx),
      w = macred(x, y.xval3(), idx);
    return expr::cond(u, v, w);
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
    std::cerr << "macro expansion: " << x << " -> " << y << endl;
#endif
    return macsubst(y);
  }
  return x;
}

expr* interpreter::uminop(expr *op, expr *x)
{
  if (op->tag() != symtab.sym("-").f) {
    int32_t f = op->tag();
    delete op; delete x;
    throw err("syntax error, '" + symtab.sym(f).s +
	      "' is not a unary operator");
  }
  expr *y;
  // handle special case of a numeric argument
  if (x->tag() == EXPR::BIGINT && (x->flags()&EXPR::OVF) &&
      mpz_cmp_ui(x->zval(), 0x80000000U) == 0)
    // The negated int 0x80000000 can actually be represented as a machine int
    // value, we convert it back on the fly here.
    y = new expr(EXPR::INT, (int32_t)-0x80000000);
  else if (x->tag() == EXPR::INT)
    y = new expr(EXPR::INT, -x->ival());
  else if (x->tag() == EXPR::DBL)
    y = new expr(EXPR::DBL, -x->dval());
  else // otherwise fall back to the neg builtin
    y = new expr(symtab.neg_sym().x, *x);
  delete op; delete x;
  return y;
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

expr *interpreter::mksym_expr(string *s, int8_t tag)
{
  expr *x;
  const symbol &sym = symtab.sym(*s, modno);
  if (tag == 0)
    if (*s == "_")
      // Return a new instance here, since the anonymous variable may have
      // multiple occurrences on the lhs.
      x = new expr(sym.f);
    else
      x = new expr(sym.x);
  else if (sym.f <= 0 || sym.prec < 10 || sym.fix == nullary)
    throw err("error in expression (misplaced type tag)");
  else {
    x = new expr(sym.f);
    // record type tag:
    x->set_ttag(tag);
  }
  delete s;
  return x;
}

expr *interpreter::mkas_expr(string *s, expr *x)
{
  const symbol &sym = symtab.sym(*s, modno);
  if (sym.f <= 0 || sym.prec < 10 || sym.fix == nullary)
    throw err("error in pattern (bad variable symbol '"+sym.s+"')");
  if (x->tag() > 0) {
    // Avoid globbering cached function symbols.
    expr *y = new expr(x->tag());
    delete x;
    x = y;
  }
  x->set_astag(sym.f);
  delete s;
  return x;
}

expr *interpreter::mkcond_expr(expr *x, expr *y, expr *z)
{
  expr *u = new expr(expr::cond(*x, *y, *z));
  delete x; delete y; delete z;
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
  rulel *s = new rulel;
  // x when l1 = r1; ...; lk = rk end  ===
  // case r1 of l1 = ... case rk of lk = x end ... end
  if (r->size() > 0x100)
    throw err("error in expression (too many nested closures)");
  uint8_t idx = 0;
  for (rulel::reverse_iterator it = r->rbegin();
       it != r->rend(); ++it, ++idx) {
    env vars;
    expr v = bind(vars, it->lhs), w = it->rhs;
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
    comp_clause& c = *cs;
    if (c.second.is_null()) {
      expr p = c.first;
      return expr::cond(p, mklistcomp_expr(x, ++cs, end), expr::nil());
    } else {
      expr pat = c.first, body = mklistcomp_expr(x, ++cs, end),
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
    comp_clause& c = *cs;
    if (c.second.is_null()) {
      expr p = c.first;
      return expr::cond(p, mkmatcomp_expr(x, n, ++cs, end),
			expr(EXPR::MATRIX, new exprll));
    } else {
      expr pat = c.first, body = mkmatcomp_expr(x, n-1, ++cs, end),
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

#define Dbl(d)		ConstantFP::get(Type::DoubleTy, d)
#define Bool(i)		ConstantInt::get(Type::Int1Ty, i)
#define UInt(i)		ConstantInt::get(Type::Int32Ty, i)
#define SInt(i)		ConstantInt::get(Type::Int32Ty, (uint64_t)i, true)
#define UInt64(i)	ConstantInt::get(Type::Int64Ty, i)
#define SInt64(i)	ConstantInt::get(Type::Int64Ty, (uint64_t)i, true)
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
  if (sym.prec < 10 || sym.fix == nullary)
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

using namespace llvm;

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
    if (sym.modno >= 0)
      v.v = new GlobalVariable
	(ExprPtrTy, false, GlobalVariable::InternalLinkage, 0,
	 "$$private."+sym.s, module);
    else
      v.v = new GlobalVariable
	(ExprPtrTy, false, GlobalVariable::ExternalLinkage, 0,
	 sym.s, module);
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
  return *this;
}

void Env::clear()
{
  static list<Function*> to_be_deleted;
  if (!f) return; // not initialized
  interpreter& interp = *interpreter::g_interp;
  if (local) {
    // purge local functions
#if DEBUG>2
    llvm::cerr << "clearing local '" << name << "'\n";
#endif
    if (h != f) interp.JIT->freeMachineCodeForFunction(h);
    interp.JIT->freeMachineCodeForFunction(f);
    f->dropAllReferences(); if (h != f) h->dropAllReferences();
    fp = 0;
    fmap.clear();
    to_be_deleted.push_back(f); if (h != f) to_be_deleted.push_back(h);
  } else {
#if DEBUG>2
    llvm::cerr << "clearing global '" << name << "'\n";
#endif
    // The code of anonymous globals (doeval, dodefn) is taken care of
    // elsewhere, we must not collect that here.
    if (!name.empty()) {
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
      llvm::cerr << "** argument mismatch!\n";
      llvm::cerr << "function parameter #" << i << ": "; a->dump();
      llvm::cerr << "provided argument  #" << i << ": "; c->dump();
      ok = false;
    }
  }
  if (ok && a != f->arg_end()) {
    llvm::cerr << "** missing arguments!\n";
    ok = false;
  }
  if (ok && b != args.end() && !f->isVarArg()) {
    llvm::cerr << "** extra arguments!\n";
    ok = false;
  }
  if (!ok) {
    llvm::cerr << "** calling function: " << f->getName() << endl;
    f->dump();
    assert(0 && "bad function call");
  }
#endif
  CallInst* v = builder.CreateCall(f, args.begin(), args.end());
  v->setCallingConv(f->getCallingConv());
  return v;
}

ReturnInst *Env::CreateRet(Value *v)
{
  interpreter& interp = *interpreter::g_interp;
  ReturnInst *ret = builder.CreateRet(v);
  Instruction *pi = ret;
  Function *free_fun = interp.module->getFunction("pure_pop_args");
  Function *free1_fun = interp.module->getFunction("pure_pop_arg");
  if (isa<CallInst>(v)) {
    CallInst* c = cast<CallInst>(v);
    // Check whether the call is actually subject to tail call elimination (as
    // determined by the calling convention).
    if (c->getCallingConv() == CallingConv::Fast) c->setTailCall();
    // Check for a tail call situation (previous instruction must be a call to
    // pure_push_args()).
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
	  CallInst *c2 = c1->clone();
	  c1->getParent()->getInstList().insert(c1, c2);
	  Value *v = BinaryOperator::createSub(c2, UInt(n+m+1), "", c1);
	  BasicBlock::iterator ii(c1);
	  ReplaceInstWithValue(c1->getParent()->getInstList(), ii, v);
	}
      }
    }
    pi = c;
  }
  // We must garbage-collect args and environment here, immediately before the
  // call (if any), or the return instruction otherwise.
  if (pi != ret && n == 1 && m == 0)
    CallInst::Create(free1_fun, "", pi);
  else if (n+m != 0) {
    vector<Value*> myargs;
    if (pi == ret)
      myargs.push_back(v);
    else
      myargs.push_back(ConstantPointerNull::get(interp.ExprPtrTy));
    myargs.push_back(UInt(n));
    myargs.push_back(UInt(m));
    CallInst::Create(free_fun, myargs.begin(), myargs.end(), "", pi);
    if (pi == ret) {
      Value *x[1] = { v };
      CallInst::Create(interp.module->getFunction("pure_unref"), x, x+1, "", ret);
    }
  }
  return ret;
}

// XXXFIXME: this should be TLD
EnvStack Env::envstk;
set<Env*> Env::props;

void Env::push(const char *msg)
{
  envstk.push_front(this);
#if DEBUG>1
  interpreter& interp = *interpreter::g_interp;
  llvm::cerr << "push (" << msg << ") " << this << " -> "
	     << (tag>0?interp.symtab.sym(tag).s:"<<anonymous>>")
	     << endl;
#endif
}

void Env::pop()
{
  assert(envstk.front() == this);
  envstk.pop_front();
#if DEBUG>1
  interpreter& interp = *interpreter::g_interp;
  llvm::cerr << "pop " << this << " -> "
	     << (tag>0?interp.symtab.sym(tag).s:"<<anonymous>>")
	     << endl;
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
      llvm::cerr << name << ": call " << interp.symtab.sym(x.vtag()).s
		 << ":" << (unsigned)x.vidx() << endl;
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
    if (n == 1 && f.tag() == interp.symtab.amp_sym().f) {
      expr y = x.xval2();
      push("&");
      Env* eptr = fmap.act()[-x.hash()] = new Env(0, 0, y, true, true);
      Env& e = *eptr;
      e.build_map(y); e.promote_map();
      pop();
    } else if (n == 2 && f.tag() == interp.symtab.catch_sym().f) {
      expr h = x.xval1().xval2(), y = x.xval2();
      push("catch");
      Env* eptr = fmap.act()[-x.hash()] = new Env(0, 0, y, true, true);
      Env& e = *eptr;
      e.build_map(y); e.promote_map();
      pop();
      build_map(h);
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
  case EXPR::LAMBDA: {
    push("lambda");
    Env* eptr = fmap.act()[-x.hash()] = new Env(0, 1, x.xval2(), true, true);
    Env& e = *eptr;
    e.build_map(x.xval2()); e.promote_map();
    pop();
    break;
  }
  case EXPR::CASE: {
    push("case");
    Env* eptr = fmap.act()[-x.hash()] = new Env(0, 1, x.xval(), true, true);
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
    Env* eptr = fmap.act()[-y.hash()] = new Env(0, 1, y, true, true);
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
	llvm::cerr << "promoting " << interp.symtab.sym(info.vtag).s
		   << " (idx " << (uint32_t)info.idx << ")"
		   << " from "
		   << (tag>0?interp.symtab.sym(tag).s:"<<anonymous>>")
		   << " (offset " << info.idx-i << ") "
		   << " to "
		   << (f->tag>0?interp.symtab.sym(f->tag).s:"<<anonymous>>")
		   << endl;
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
	llvm::cerr << "propagating " << interp.symtab.sym(info.vtag).s
		   << " (idx " << (uint32_t)info.idx << ")"
		   << " from "
		   << (tag>0?interp.symtab.sym(tag).s:"<<anonymous>>")
		   << " (offset " << offs << ") "
		   << " to "
		   << (e.tag>0?interp.symtab.sym(e.tag).s:"<<anonymous>>")
		   << endl;
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
  llvm::cerr << "push (" << msg << ") " << e << " -> "
	     << (e->tag>0?interp.symtab.sym(e->tag).s:"<<anonymous>>")
	     << endl;
#endif
}

void interpreter::pop(Env *e)
{
  assert(envstk.front() == e);
  envstk.pop_front();
#if DEBUG>1
  interpreter& interp = *interpreter::g_interp;
  llvm::cerr << "pop (" << e << ") -> "
	     << (e->tag>0?interp.symtab.sym(e->tag).s:"<<anonymous>>")
	     << endl;
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
    return Type::VoidTy;
  else if (name == "bool")
    return Type::Int1Ty;
  else if (name == "char")
    return Type::Int8Ty;
  else if (name == "short")
    return Type::Int16Ty;
  else if (name == "int")
    return Type::Int32Ty;
  else if (name == "long")
    return Type::Int64Ty;
  else if (name == "float")
    return Type::FloatTy;
  else if (name == "double")
    return Type::DoubleTy;
  else if (name == "char*")
    return CharPtrTy;
  else if (name == "short*")
    return PointerType::get(Type::Int16Ty, 0);
  else if (name == "int*")
    return PointerType::get(Type::Int32Ty, 0);
  else if (name == "long*")
    return PointerType::get(Type::Int64Ty, 0);
  else if (name == "double*")
    return PointerType::get(Type::DoubleTy, 0);
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
  if (type == Type::VoidTy)
    return "void";
  else if (type == Type::Int1Ty)
    return "bool";
  else if (type == Type::Int8Ty)
    return "char";
  else if (type == Type::Int16Ty)
    return "short";
  else if (type == Type::Int32Ty)
    return "int";
  else if (type == Type::Int64Ty)
    return "long";
  else if (type == Type::FloatTy)
    return "float";
  else if (type == Type::DoubleTy)
    return "double";
  else if (type == CharPtrTy)
    return "char*";
  else if (type == PointerType::get(Type::Int16Ty, 0))
    return "short*";
  else if (type == PointerType::get(Type::Int32Ty, 0))
    return "int*";
  else if (type == PointerType::get(Type::Int64Ty, 0))
    return "long*";
  else if (type == PointerType::get(Type::DoubleTy, 0))
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
  return declare_extern(name, restype, argtypes, varargs, fp);
}

Function *interpreter::declare_extern(string name, string restype,
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
    if (argt[i] == Type::VoidTy)
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
    return f;
  }
  // External C function visible in the Pure program. No varargs are allowed
  // here for now. Also, we have to translate some of the parameter types
  // (expr** becomes void*, int32_t gets promoted to int64_t if the default
  // int type of the target platform has 64 bit).
  assert(!varargs);
  if (type == ExprPtrPtrTy)
    type = VoidPtrTy;
  else if (type == Type::Int32Ty && sizeof(int) > 4)
    type = Type::Int64Ty;
  for (size_t i = 0; i < n; i++, atype++)
    if (argt[i] == ExprPtrPtrTy)
      argt[i] = VoidPtrTy;
    else if (argt[i] == Type::Int32Ty && sizeof(int) > 4)
      argt[i] = Type::Int64Ty;
  if (asname.empty()) asname = name;
  symbol& sym = symtab.sym(asname, modno);
  if (globenv.find(sym.f) != globenv.end() &&
      externals.find(sym.f) == externals.end())
    // There already is a Pure function or global variable for this symbol.
    // This is an error (unless the symbol is already declared as an external).
    throw err("symbol '"+asname+"' is already defined as a Pure "+
	      ((globenv[sym.f].t == env_info::fun) ? "function" :
	       (globenv[sym.f].t == env_info::fvar) ? "variable" :
	       (globenv[sym.f].t == env_info::cvar) ? "constant" :
	       "gizmo" /* this can't happen, or at least it shouldn't ;-) */));
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
	  gt->getParamType(i)==CharPtrTy &&
	  argt[i] == VoidPtrTy;
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
				 "$$wrap."+asname, module);
  vector<Value*> args(n), unboxed(n);
  Function::arg_iterator a = f->arg_begin();
  for (size_t i = 0; a != f->arg_end(); ++a, ++i) {
    a->setName(mklabel("arg", i)); args[i] = a;
  }
  Builder b;
  BasicBlock *bb = BasicBlock::Create("entry", f),
    *failedbb = BasicBlock::Create("failed");
  b.SetInsertPoint(bb);
  // unbox arguments
  bool temps = false;
  for (size_t i = 0; i < n; i++) {
    Value *x = args[i];
    // check for thunks which must be forced
    if (argt[i] != ExprPtrTy) {
      // do a quick check on the tag value
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      Value *checkv = b.CreateICmpEQ(tagv, Zero, "check");
      BasicBlock *forcebb = BasicBlock::Create("force");
      BasicBlock *skipbb = BasicBlock::Create("skip");
      b.CreateCondBr(checkv, forcebb, skipbb);
      f->getBasicBlockList().push_back(forcebb);
      b.SetInsertPoint(forcebb);
      b.CreateCall(module->getFunction("pure_force"), x);
      b.CreateBr(skipbb);
      f->getBasicBlockList().push_back(skipbb);
      b.SetInsertPoint(skipbb);
    }
    if (argt[i] == Type::Int1Ty) {
      BasicBlock *okbb = BasicBlock::Create("ok");
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
    } else if (argt[i] == Type::Int8Ty) {
      /* We allow either ints or bigints to be passed for C integers. */
      BasicBlock *intbb = BasicBlock::Create("int");
      BasicBlock *mpzbb = BasicBlock::Create("mpz");
      BasicBlock *okbb = BasicBlock::Create("ok");
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
      PHINode *phi = b.CreatePHI(Type::Int32Ty);
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = b.CreateTrunc(phi, Type::Int8Ty);
    } else if (argt[i] == Type::Int16Ty) {
      BasicBlock *intbb = BasicBlock::Create("int");
      BasicBlock *mpzbb = BasicBlock::Create("mpz");
      BasicBlock *okbb = BasicBlock::Create("ok");
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
      PHINode *phi = b.CreatePHI(Type::Int32Ty);
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = b.CreateTrunc(phi, Type::Int16Ty);
    } else if (argt[i] == Type::Int32Ty) {
      BasicBlock *intbb = BasicBlock::Create("int");
      BasicBlock *mpzbb = BasicBlock::Create("mpz");
      BasicBlock *okbb = BasicBlock::Create("ok");
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
      PHINode *phi = b.CreatePHI(Type::Int32Ty);
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = phi;
    } else if (argt[i] == Type::Int64Ty) {
      BasicBlock *intbb = BasicBlock::Create("int");
      BasicBlock *mpzbb = BasicBlock::Create("mpz");
      BasicBlock *okbb = BasicBlock::Create("ok");
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
      intv = b.CreateSExt(intv, Type::Int64Ty);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(mpzbb);
      b.SetInsertPoint(mpzbb);
      // Handle the case of a bigint (mpz_t -> long).
      Value *mpzv = b.CreateCall(module->getFunction("pure_get_long"), x);
      b.CreateBr(okbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = b.CreatePHI(Type::Int64Ty);
      phi->addIncoming(intv, intbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = phi;
    } else if (argt[i] == Type::FloatTy) {
      BasicBlock *okbb = BasicBlock::Create("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(EXPR::DBL), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      Value *pv = b.CreateBitCast(x, DblExprPtrTy, "dblexpr");
      idx[1] = ValFldIndex;
      Value *dv = b.CreateLoad(b.CreateGEP(pv, idx, idx+2), "dblval");
      unboxed[i] = b.CreateFPTrunc(dv, Type::FloatTy);
    } else if (argt[i] == Type::DoubleTy) {
      BasicBlock *okbb = BasicBlock::Create("ok");
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
      BasicBlock *okbb = BasicBlock::Create("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      b.CreateCondBr
	(b.CreateICmpEQ(tagv, SInt(EXPR::STR), "cmp"), okbb, failedbb);
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      Value *sv = b.CreateCall(module->getFunction("pure_get_cstring"), x);
      unboxed[i] = sv; temps = true;
    } else if (argt[i] == PointerType::get(Type::Int16Ty, 0) ||
	       argt[i] == PointerType::get(Type::Int32Ty, 0) ||
	       argt[i] == PointerType::get(Type::Int64Ty, 0) ||
	       argt[i] == PointerType::get(Type::FloatTy, 0) ||
	       argt[i] == PointerType::get(Type::DoubleTy, 0)) {
      BasicBlock *okbb = BasicBlock::Create("ok");
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
    } else if (argt[i] == GSLMatrixPtrTy ||
	       argt[i] == GSLDoubleMatrixPtrTy ||
	       argt[i] == GSLComplexMatrixPtrTy ||
	       argt[i] == GSLIntMatrixPtrTy) {
      BasicBlock *okbb = BasicBlock::Create("ok");
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
      BasicBlock *ptrbb = BasicBlock::Create("ptr");
      BasicBlock *mpzbb = BasicBlock::Create("mpz");
      BasicBlock *okbb = BasicBlock::Create("ok");
      Value *idx[2] = { Zero, Zero };
      Value *tagv = b.CreateLoad(b.CreateGEP(x, idx, idx+2), "tag");
      SwitchInst *sw = b.CreateSwitch(tagv, failedbb, 3);
      /* We also allow bigints and strings to be passed as a void* here. The
	 former lets you use GMP routines directly in Pure if you declare the
	 mpz_t params as void*. The latter allows a string to be passed
	 without implicitly converting it to the system encoding first. Note
	 that in both cases a direct pointer to the data will be passed, which
	 enables mutation of the data; if this isn't desired then you should
	 copy the data first. */
      sw->addCase(SInt(EXPR::PTR), ptrbb);
      sw->addCase(SInt(EXPR::STR), ptrbb);
      sw->addCase(SInt(EXPR::BIGINT), mpzbb);
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
      f->getBasicBlockList().push_back(okbb);
      b.SetInsertPoint(okbb);
      PHINode *phi = b.CreatePHI(VoidPtrTy);
      phi->addIncoming(ptrv, ptrbb);
      phi->addIncoming(mpzv, mpzbb);
      unboxed[i] = phi;
      if (gt->getParamType(i)==CharPtrTy)
	// An external builtin already has this parameter declared as char*.
	// We allow void* to be passed anyway, so just cast it to char* to
	// make the LLVM typechecker happy.
	unboxed[i] = b.CreateBitCast(unboxed[i], CharPtrTy);
    } else
      assert(0 && "invalid C type");
  }
  // call the function
  Value* u = b.CreateCall(g, unboxed.begin(), unboxed.end());
  // free temporaries
  if (temps) b.CreateCall(module->getFunction("pure_free_cstrings"));
  // box the result
  if (type == Type::VoidTy)
    u = b.CreateCall(module->getFunction("pure_const"),
		     SInt(symtab.void_sym().f));
  else if (type == Type::Int1Ty)
    u = b.CreateCall(module->getFunction("pure_int"),
		     b.CreateZExt(u, Type::Int32Ty));
  else if (type == Type::Int8Ty)
    u = b.CreateCall(module->getFunction("pure_int"),
		     b.CreateSExt(u, Type::Int32Ty));
  else if (type == Type::Int16Ty)
    u = b.CreateCall(module->getFunction("pure_int"),
		     b.CreateSExt(u, Type::Int32Ty));
  else if (type == Type::Int32Ty)
    u = b.CreateCall(module->getFunction("pure_int"), u);
  else if (type == Type::Int64Ty)
    u = b.CreateCall(module->getFunction("pure_long"), u);
  else if (type == Type::FloatTy)
    u = b.CreateCall(module->getFunction("pure_double"),
		     b.CreateFPExt(u, Type::DoubleTy));
  else if (type == Type::DoubleTy)
    u = b.CreateCall(module->getFunction("pure_double"), u);
  else if (type == CharPtrTy)
    u = b.CreateCall(module->getFunction("pure_cstring_dup"), u);
  else if (type == PointerType::get(Type::Int16Ty, 0) ||
	   type == PointerType::get(Type::Int32Ty, 0) ||
	   type == PointerType::get(Type::Int64Ty, 0) ||
	   type == PointerType::get(Type::FloatTy, 0) ||
	   type == PointerType::get(Type::DoubleTy, 0))
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
    BasicBlock *okbb = BasicBlock::Create("ok");
    b.CreateCondBr
      (b.CreateICmpNE(u, NullExprPtr, "cmp"), okbb, failedbb);
    f->getBasicBlockList().push_back(okbb);
    b.SetInsertPoint(okbb);
    // value is passed through
  } else if (type == VoidPtrTy)
    u = b.CreateCall(module->getFunction("pure_pointer"), u);
  else
    assert(0 && "invalid C type");
  // free arguments (we do that here so that the arguments don't get freed
  // before we know that we don't need them anymore)
  if (n > 0) {
    vector<Value*> freeargs(3);
    freeargs[0] = u;
    freeargs[1] = UInt(n);
    freeargs[2] = Zero;
    b.CreateCall(module->getFunction("pure_pop_args"),
		 freeargs.begin(), freeargs.end());
    b.CreateCall(module->getFunction("pure_unref"), u);
  }
  b.CreateRet(u);
  // The call failed. Provide a default value.
  f->getBasicBlockList().push_back(failedbb);
  b.SetInsertPoint(failedbb);
  // free temporaries
  if (temps) b.CreateCall(module->getFunction("pure_free_cstrings"));
  // As default, create a cbox for the function symbol and apply that to our
  // parameters. The cbox may be patched up later to become a Pure function.
  // In effect, this allows an external function to be augmented with Pure
  // equations, but note that the external C function will always be tried
  // first.
  pure_expr *cv = pure_const(sym.f);
  assert(JIT);
  GlobalVar& v = globalvars[sym.f];
  if (!v.v) {
    v.v = new GlobalVariable
      (ExprPtrTy, false, GlobalVariable::InternalLinkage, 0,
       mkvarlabel(sym.f), module);
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
    b.CreateCall(module->getFunction("pure_unref"), defaultv);
  }
  b.CreateRet(defaultv);
  verifyFunction(*f);
  if (FPM) FPM->run(*f);
  if (verbose&verbosity::dump) f->print(std::cout);
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

pure_expr *interpreter::const_value(expr x)
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
  case EXPR::MATRIX:
    return const_matrix_value(x);
  case EXPR::APP:
    return const_app_value(x);
  default: {
    exprl xs;
    if (x.tag() > 0) {
      env::const_iterator it = globenv.find(x.tag());
      map<int32_t,GlobalVar>::iterator v;
      if (externals.find(x.tag()) != externals.end())
	return 0;
      else if (it == globenv.end())
	// unbound symbol
	return pure_const(x.tag());
      else if (it->second.t == env_info::fvar &&
	       (v = globalvars.find(x.tag())) != globalvars.end())
	// variable
	return v->second.x;
      else
	return 0;
    } else if (x.is_list(xs) || x.is_pair() && x.is_tuplex(xs)) {
      // proper lists and tuples
      size_t i, n = xs.size();
      pure_expr **xv = (pure_expr**)malloc(n*sizeof(pure_expr*));
      if (!xv) return 0;
      exprl::iterator it = xs.begin(), end = xs.end();
      for (i = 0; it != end; i++, it++)
	if ((xv[i] = const_value(*it)) == 0) {
	  for (size_t j = 0; j < i; j++)
	    pure_freenew(xv[j]);
	  free(xv);
	  return 0;
	}
      pure_expr *res = (x.is_pair()?pure_tuplev:pure_listv)(n, xv);
      free(xv);
      return res;
    } else
      return 0;
  }
  }
}

pure_expr *interpreter::const_matrix_value(expr x)
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
      vs[j] = const_value(*ys);
      if (!vs[j]) goto err;
    }
    us[i] = pure_matrix_columnsv(m, vs);
    if (!us[i]) goto err;
    delete[] vs;
  }
  ret = pure_matrix_rowsv(n, us);
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
    pure_expr *f = 0, *y = 0;
    if ((f = const_app_value(x.xval1())) && (y = const_value(x.xval2())))
      return pure_app(f, y);
    else {
      if (f) pure_freenew(f);
      return 0;
    }
  } else if (x.tag() > 0) {
    env::const_iterator it = globenv.find(x.tag());
    map<int32_t,GlobalVar>::iterator v;
    if (externals.find(x.tag()) != externals.end())
      return 0;
    else if (it == globenv.end())
      // unbound symbol
      return pure_const(x.tag());
    else if (it->second.t == env_info::fvar &&
	     (v = globalvars.find(x.tag())) != globalvars.end()) {
      // variable value
      pure_expr *y = v->second.x;
      // walk down the spine, if any
      while (y->tag == EXPR::APP) y = y->data.x[0];
      // check if we got a closure subject to evaluation
      if (y->tag >= 0 && y->data.clos)
	return 0;
      else
	// not a callable closure, so it must be a constructor term
	return v->second.x;
    } else
      return 0;
  } else
    return 0;
}

pure_expr *interpreter::doeval(expr x, pure_expr*& e)
{
  char test;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax) {
    e = pure_const(symtab.segfault_sym().f);
    return 0;
  }
  e = 0;
  // First check whether the value is actually a constant, then we can skip
  // the compilation step.
  clock_t t0 = clock();
  pure_expr *res = const_value(x);
  if (interactive && stats) clocks = clock()-t0;
  if (res) return res;
  // Not a constant value. Create an anonymous function to call in order to
  // evaluate the target expression.
  /* NOTE: The environment is allocated dynamically, so that its child
     environments survive for the entire lifetime of any embedded closures,
     which might still be called at a later time. */
  Env *save_fptr = fptr;
  fptr = new Env(0, 0, x, false); fptr->refc = 1;
  Env &f = *fptr;
  push("doeval", &f);
  fun_prolog("");
#if DEBUG>1
  ostringstream msg;
  msg << "doeval: " << x;
  debug(msg.str().c_str());
#endif
  f.CreateRet(codegen(x));
  fun_finish();
  pop(&f);
  // JIT the function.
  f.fp = JIT->getPointerToFunction(f.f);
  assert(f.fp);
  t0 = clock();
  res = pure_invoke(f.fp, &e);
  if (interactive && stats) clocks = clock()-t0;
  // Get rid of our anonymous function.
  JIT->freeMachineCodeForFunction(f.f);
  f.f->eraseFromParent();
  // If there are no more references, we can get rid of the environment now.
  if (fptr->refc == 1)
    delete fptr;
  else
    fptr->refc--;
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

pure_expr *interpreter::dodefn(env vars, expr lhs, expr rhs, pure_expr*& e)
{
  char test;
  if (stackmax > 0 && stackdir*(&test - baseptr) >= stackmax) {
    e = pure_const(symtab.segfault_sym().f);
    return 0;
  }
  e = 0;
  // First check whether the value is actually a constant, then we can skip
  // the compilation step.
  clock_t t0 = clock();
  pure_expr *res = const_value(rhs);
  if (res) {
    matcher m(rule(lhs, rhs));
    if (m.match(rhs)) {
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
	  if (sym.modno >= 0)
	    v.v = new GlobalVariable
	      (ExprPtrTy, false, GlobalVariable::InternalLinkage, 0,
	       "$$private."+sym.s, module);
	  else
	    v.v = new GlobalVariable
	      (ExprPtrTy, false, GlobalVariable::ExternalLinkage, 0,
	       sym.s, module);
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
  // Not a constant value. Create an anonymous function to call in order to
  // evaluate the rhs expression, match against the lhs and bind variables in
  // lhs accordingly.
  Env *save_fptr = fptr;
  fptr = new Env(0, 0, rhs, false); fptr->refc = 1;
  Env &f = *fptr;
  push("dodefn", &f);
  fun_prolog("");
#if DEBUG>1
  ostringstream msg;
  msg << "dodef: " << lhs << "=" << rhs;
  debug(msg.str().c_str());
#endif
  // compute the matchee
  Value *arg = codegen(rhs);
  // emit the matching code
  BasicBlock *matchedbb = BasicBlock::Create("matched");
  BasicBlock *failedbb = BasicBlock::Create("failed");
  matcher m(rule(lhs, rhs));
  if (verbose&verbosity::code) std::cout << m << endl;
  state *start = m.start;
  simple_match(arg, start, matchedbb, failedbb);
  // matched => emit code for binding the variables
  f.f->getBasicBlockList().push_back(matchedbb);
  f.builder.SetInsertPoint(matchedbb);
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
      if (sym.modno >= 0)
	v.v = new GlobalVariable
	  (ExprPtrTy, false, GlobalVariable::InternalLinkage, 0,
	   "$$private."+sym.s, module);
      else
	v.v = new GlobalVariable
	  (ExprPtrTy, false, GlobalVariable::ExternalLinkage, 0,
	   sym.s, module);
      JIT->addGlobalMapping(v.v, &v.x);
    }
    if (v.x) call("pure_free", f.builder.CreateLoad(v.v));
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
  // JIT the function.
  f.fp = JIT->getPointerToFunction(f.f);
  assert(f.fp);
  t0 = clock();
  res = pure_invoke(f.fp, &e);
  if (interactive && stats) clocks = clock()-t0;
  // Get rid of our anonymous function.
  JIT->freeMachineCodeForFunction(f.f);
  f.f->eraseFromParent();
  // If there are no more references, we can get rid of the environment now.
  if (fptr->refc == 1)
    delete fptr;
  else
    fptr->refc--;
  fptr = save_fptr;
  if (!res) {
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

Value *interpreter::when_codegen(expr x, matcher *m,
				 rulel::const_iterator r,
				 rulel::const_iterator end)
// x = subject expression to be evaluated in the context of the bindings
// m = matching automaton for current rule
// r = current pattern binding rule
// end = end of rule list
{
  if (r == end) {
    toplevel_codegen(x);
    return 0;
  } else {
    Env& act = act_env();
    rulel::const_iterator s = r;
    expr y = (++s == end)?x:s->rhs;
    assert(act.fmap.act().find(-y.hash()) != act.fmap.act().end());
    Env& e = *act.fmap.act()[-y.hash()];
    push("when", &e);
    fun_prolog("anonymous");
    BasicBlock *bodybb = BasicBlock::Create("body");
    BasicBlock *matchedbb = BasicBlock::Create("matched");
    BasicBlock *failedbb = BasicBlock::Create("failed");
    e.builder.CreateBr(bodybb);
    e.f->getBasicBlockList().push_back(bodybb);
    e.builder.SetInsertPoint(bodybb);
    Value *arg = e.args[0];
    // emit the matching code
    state *start = m->start;
    simple_match(arg, start, matchedbb, failedbb);
    // matched => emit code for the reduct
    e.f->getBasicBlockList().push_back(matchedbb);
    e.builder.SetInsertPoint(matchedbb);
    Value *v = when_codegen(x, m+1, s, end);
    if (v) e.CreateRet(v);
    // failed => throw an exception
    e.f->getBasicBlockList().push_back(failedbb);
    e.builder.SetInsertPoint(failedbb);
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
	return e.builder.CreateFPToSI(u, Type::Int32Ty);
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
      v = e.builder.CreateFPToSI(v, Type::Int32Ty);
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
	return e.builder.CreateSIToFP(u, Type::DoubleTy);
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
      v = e.builder.CreateSIToFP(v, Type::DoubleTy);
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
    if (f.ftag() == symtab.neg_sym().f)
      return b.CreateSub(Zero, u);
    else if (f.ftag() == symtab.not_sym().f)
      return b.CreateZExt
	(b.CreateICmpEQ(Zero, u, "cmp"), Type::Int32Ty);
    else if (f.ftag() == symtab.bitnot_sym().f)
      return b.CreateXor(UInt(0xffffffff), u);
    else {
      assert(0 && "error in type checker");
      return 0;
    }
  } else if (n == 1 && x.ttag() == EXPR::DBL) {
    // unary double operations
    Value *u = get_double(x.xval2());
    if (f.ftag() == symtab.neg_sym().f)
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
    if (f.ftag() == symtab.or_sym().f) {
      Env& e = act_env();
      Value *condv = b.CreateICmpNE(u, Zero, "cond");
      BasicBlock *iftruebb = b.GetInsertBlock();
      BasicBlock *iffalsebb = BasicBlock::Create("iffalse");
      BasicBlock *endbb = BasicBlock::Create("end");
      b.CreateCondBr(condv, endbb, iffalsebb);
      e.f->getBasicBlockList().push_back(iffalsebb);
      b.SetInsertPoint(iffalsebb);
      Value *v = get_int(x.xval2());
#if DEBUG
      if (u->getType() != v->getType()) {
	llvm::cerr << "** operand mismatch!\n";
	llvm::cerr << "operator:      " << symtab.sym(f.ftag()).s << endl;
	llvm::cerr << "left operand:  "; u->dump();
	llvm::cerr << "right operand: "; v->dump();
	assert(0 && "operand mismatch");
      }
#endif
      Value *condv2 = b.CreateICmpNE(v, Zero, "cond");
      b.CreateBr(endbb);
      iffalsebb = b.GetInsertBlock();
      e.f->getBasicBlockList().push_back(endbb);
      b.SetInsertPoint(endbb);
      PHINode *phi = b.CreatePHI(Type::Int1Ty, "fi");
      phi->addIncoming(condv, iftruebb);
      phi->addIncoming(condv2, iffalsebb);
      return b.CreateZExt(phi, Type::Int32Ty);
    } else if (f.ftag() == symtab.and_sym().f) {
      Env& e = act_env();
      Value *condv = b.CreateICmpNE(u, Zero, "cond");
      BasicBlock *iffalsebb = b.GetInsertBlock();
      BasicBlock *iftruebb = BasicBlock::Create("iftrue");
      BasicBlock *endbb = BasicBlock::Create("end");
      b.CreateCondBr(condv, iftruebb, endbb);
      e.f->getBasicBlockList().push_back(iftruebb);
      b.SetInsertPoint(iftruebb);
      Value *v = get_int(x.xval2());
#if DEBUG
      if (u->getType() != v->getType()) {
	llvm::cerr << "** operand mismatch!\n";
	llvm::cerr << "operator:      " << symtab.sym(f.ftag()).s << endl;
	llvm::cerr << "left operand:  "; u->dump();
	llvm::cerr << "right operand: "; v->dump();
	assert(0 && "operand mismatch");
      }
#endif
      Value *condv2 = b.CreateICmpNE(v, Zero, "cond");
      b.CreateBr(endbb);
      iftruebb = b.GetInsertBlock();
      e.f->getBasicBlockList().push_back(endbb);
      b.SetInsertPoint(endbb);
      PHINode *phi = b.CreatePHI(Type::Int1Ty, "fi");
      phi->addIncoming(condv, iffalsebb);
      phi->addIncoming(condv2, iftruebb);
      return b.CreateZExt(phi, Type::Int32Ty);
    } else {
      Value *v = get_int(x.xval2());
#if DEBUG
      if (u->getType() != v->getType()) {
	llvm::cerr << "** operand mismatch!\n";
	llvm::cerr << "operator:      " << symtab.sym(f.ftag()).s << endl;
	llvm::cerr << "left operand:  "; u->dump();
	llvm::cerr << "right operand: "; v->dump();
	assert(0 && "operand mismatch");
      }
#endif
      if (f.ftag() == symtab.bitor_sym().f)
	return b.CreateOr(u, v);
      else if (f.ftag() == symtab.bitand_sym().f)
	return b.CreateAnd(u, v);
      else if (f.ftag() == symtab.shl_sym().f) {
	// result of shl is undefined if u>=#bits, return 0 in that case
	BasicBlock *okbb = BasicBlock::Create("ok");
	BasicBlock *zerobb = b.GetInsertBlock();
	BasicBlock *endbb = BasicBlock::Create("end");
	Value *cmp = b.CreateICmpULT(v, UInt(32));
	b.CreateCondBr(cmp, okbb, endbb);
	act_env().f->getBasicBlockList().push_back(okbb);
	b.SetInsertPoint(okbb);
	Value *ok = b.CreateShl(u, v);
	b.CreateBr(endbb);
	act_env().f->getBasicBlockList().push_back(endbb);
	b.SetInsertPoint(endbb);
	PHINode *phi = b.CreatePHI(Type::Int32Ty);
	phi->addIncoming(ok, okbb);
	phi->addIncoming(Zero, zerobb);
	return phi;
      } else if (f.ftag() == symtab.shr_sym().f)
	return b.CreateAShr(u, v);
      else if (f.ftag() == symtab.less_sym().f)
	return b.CreateZExt
	  (b.CreateICmpSLT(u, v), Type::Int32Ty);
      else if (f.ftag() == symtab.greater_sym().f)
	return b.CreateZExt
	  (b.CreateICmpSGT(u, v), Type::Int32Ty);
      else if (f.ftag() == symtab.lesseq_sym().f)
	return b.CreateZExt
	  (b.CreateICmpSLE(u, v), Type::Int32Ty);
      else if (f.ftag() == symtab.greatereq_sym().f)
	return b.CreateZExt
	  (b.CreateICmpSGE(u, v), Type::Int32Ty);
      else if (f.ftag() == symtab.equal_sym().f)
	return b.CreateZExt
	  (b.CreateICmpEQ(u, v), Type::Int32Ty);
      else if (f.ftag() == symtab.notequal_sym().f)
	return b.CreateZExt
	  (b.CreateICmpNE(u, v), Type::Int32Ty);
      else if (f.ftag() == symtab.plus_sym().f)
	return b.CreateAdd(u, v);
      else if (f.ftag() == symtab.minus_sym().f)
	return b.CreateSub(u, v);
      else if (f.ftag() == symtab.mult_sym().f)
	return b.CreateMul(u, v);
      else if (f.ftag() == symtab.div_sym().f) {
	// catch division by zero
	if (x.xval2().tag() == EXPR::INT && x.xval2().ival() == 0) {
	  b.CreateCall(module->getFunction("pure_sigfpe"));
	  return v;
	} else {
	  BasicBlock *okbb = BasicBlock::Create("ok");
	  BasicBlock *errbb = BasicBlock::Create("err");
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
      } else if (f.ftag() == symtab.mod_sym().f) {
	// catch division by zero
	if (x.xval2().tag() == EXPR::INT && x.xval2().ival() == 0) {
	  b.CreateCall(module->getFunction("pure_sigfpe"));
	  return v;
	} else {
	  BasicBlock *okbb = BasicBlock::Create("ok");
	  BasicBlock *errbb = BasicBlock::Create("err");
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
      llvm::cerr << "** operand mismatch!\n";
      llvm::cerr << "operator:      " << symtab.sym(f.ftag()).s << endl;
      llvm::cerr << "left operand:  "; u->dump();
      llvm::cerr << "right operand: "; v->dump();
      assert(0 && "operand mismatch");
    }
#endif
    if (f.ftag() == symtab.less_sym().f)
      return b.CreateZExt
	(b.CreateFCmpOLT(u, v), Type::Int32Ty);
    else if (f.ftag() == symtab.greater_sym().f)
      return b.CreateZExt
	(b.CreateFCmpOGT(u, v), Type::Int32Ty);
    else if (f.ftag() == symtab.lesseq_sym().f)
      return b.CreateZExt
	(b.CreateFCmpOLE(u, v), Type::Int32Ty);
    else if (f.ftag() == symtab.greatereq_sym().f)
      return b.CreateZExt
	(b.CreateFCmpOGE(u, v), Type::Int32Ty);
    else if (f.ftag() == symtab.equal_sym().f)
      return b.CreateZExt
	(b.CreateFCmpOEQ(u, v), Type::Int32Ty);
    else if (f.ftag() == symtab.notequal_sym().f)
      return b.CreateZExt
	(b.CreateFCmpONE(u, v), Type::Int32Ty);
    else if (f.ftag() == symtab.plus_sym().f)
      return b.CreateAdd(u, v);
    else if (f.ftag() == symtab.minus_sym().f)
      return b.CreateSub(u, v);
    else if (f.ftag() == symtab.mult_sym().f)
      return b.CreateMul(u, v);
    else if (f.ftag() == symtab.fdiv_sym().f)
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

void interpreter::toplevel_codegen(expr x)
{
#if USE_FASTCC
  assert(!x.is_null());
  if (x.tag() == EXPR::COND) {
    toplevel_cond(x.xval1(), x.xval2(), x.xval3());
    return;
  }
  Env& e = act_env();
#if TAILOPS
  Builder& b = act_builder();
  expr f; uint32_t n = count_args(x, f);
  if (n == 2 && x.ttag() == EXPR::INT &&
      x.xval1().xval2().ttag() != EXPR::DBL &&
      x.xval2().ttag() != EXPR::DBL) {
    if (f.ftag() == symtab.or_sym().f) {
      Value *u = get_int(x.xval1().xval2());
      Value *condv = b.CreateICmpNE(u, Zero, "cond");
      BasicBlock *iftruebb = BasicBlock::Create("iftrue");
      BasicBlock *iffalsebb = BasicBlock::Create("iffalse");
      b.CreateCondBr(condv, iftruebb, iffalsebb);
      e.f->getBasicBlockList().push_back(iftruebb);
      b.SetInsertPoint(iftruebb);
      e.CreateRet(ibox(One));
      e.f->getBasicBlockList().push_back(iffalsebb);
      b.SetInsertPoint(iffalsebb);
      toplevel_codegen(x.xval2());
    } else if (f.ftag() == symtab.and_sym().f) {
      Value *u = get_int(x.xval1().xval2());
      Value *condv = b.CreateICmpNE(u, Zero, "cond");
      BasicBlock *iftruebb = BasicBlock::Create("iftrue");
      BasicBlock *iffalsebb = BasicBlock::Create("iffalse");
      b.CreateCondBr(condv, iftruebb, iffalsebb);
      e.f->getBasicBlockList().push_back(iffalsebb);
      b.SetInsertPoint(iffalsebb);
      e.CreateRet(ibox(Zero));
      e.f->getBasicBlockList().push_back(iftruebb);
      b.SetInsertPoint(iftruebb);
      toplevel_codegen(x.xval2());
    } else
      e.CreateRet(codegen(x));
  } else
#endif
    e.CreateRet(codegen(x));
#else
  act_env().CreateRet(codegen(x));
#endif
}

Value *interpreter::codegen(expr x)
{
  assert(!x.is_null());
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
    // FIXME: Only null pointers are supported right now.
    assert(x.pval() == 0);
    return pbox(x.pval());
  // matrix:
  case EXPR::MATRIX: {
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
	vs[j] = codegen(*ys);
      }
      us[i] =
	act_env().CreateCall(module->getFunction("pure_matrix_columns"), vs);
    }
    return act_env().CreateCall(module->getFunction("pure_matrix_rows"), us);
  }
  // application:
  case EXPR::APP:
    if (x.ttag() != 0) {
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
      else if (n == 2 && f.ftag() == symtab.seq_sym().f) {
	// sequence operator
	Value *u = codegen(x.xval1().xval2());
	act_builder().CreateCall(module->getFunction("pure_freenew"), u);
	return codegen(x.xval2());
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
#if LIST_KLUDGE>0
	/* Alternative code for proper lists and tuples, which considerably
	   speeds up compilation for larger sequences. See the comments at the
	   beginning of interpreter.hh for details. */
	exprl xs;
	if ((x.is_list(xs) || x.is_pair() && x.is_tuple(xs)) &&
	    xs.size() >= LIST_KLUDGE) {
	  size_t i = 0, n = xs.size();
	  vector<Value*> argv(n+1);
	  argv[0] = UInt(n);
	  for (exprl::iterator it = xs.begin(), end = xs.end(); it != end;
	       it++)
	    argv[++i] = codegen(*it);
	  act_env().CreateCall(module->getFunction("pure_new_args"), argv);
	  v = act_env().CreateCall
	    (module->getFunction(x.is_pair()?"pure_tuplel":"pure_listl"),
	     argv);
	  vector<Value*> argv1;
	  argv1.push_back(NullExprPtr);
	  argv1.insert(argv1.end(), argv.begin(), argv.end());
	  act_env().CreateCall(module->getFunction("pure_free_args"), argv1);
	  return v;
	}
	xs.clear();
#endif
	// ordinary function application
	Value *u = codegen(x.xval1()), *v = codegen(x.xval2());
	return apply(u, v);
      }
    }
  // conditional:
  case EXPR::COND:
    return cond(x.xval1(), x.xval2(), x.xval3());
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
      return call("pure_clos", false, false, x.tag(), info.f, NullPtr,
		  info.argtypes.size(), env);
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
// invoked. If thunked is true, a "thunk" (literal, unevaluated fbox) is
// created.

Value *interpreter::fbox(Env& f, bool thunked)
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
    return call("pure_clos", f.local, thunked, f.tag, f.h, envptr(&f), f.n, x);
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
    v.v = new GlobalVariable
      (ExprPtrTy, false, GlobalVariable::InternalLinkage, 0,
       mkvarlabel(tag), module);
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
    unwind(symtab.failed_cond_sym().f);
    return NullExprPtr;
  } else
    // typeless expression, will be checked at runtime
    iv = get_int(x);
  // emit the condition (turn the previous result into a flag)
  Value *condv = f.builder.CreateICmpNE(iv, Zero, "cond");
  // create the basic blocks for the branches
  BasicBlock *thenbb = BasicBlock::Create("then");
  BasicBlock *elsebb = BasicBlock::Create("else");
  BasicBlock *endbb = BasicBlock::Create("end");
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

void interpreter::toplevel_cond(expr x, expr y, expr z)
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
    unwind(symtab.failed_cond_sym().f);
    return;
  } else
    // typeless expression, will be checked at runtime
    iv = get_int(x);
  // emit the condition (turn the previous result into a flag)
  Value *condv = f.builder.CreateICmpNE(iv, Zero, "cond");
  // create the basic blocks for the branches
  BasicBlock *thenbb = BasicBlock::Create("then");
  BasicBlock *elsebb = BasicBlock::Create("else");
  // create the branch instruction and emit the 'then' block
  f.builder.CreateCondBr(condv, thenbb, elsebb);
  f.f->getBasicBlockList().push_back(thenbb);
  f.builder.SetInsertPoint(thenbb);
  toplevel_codegen(y);
  // emit the 'else' block
  f.f->getBasicBlockList().push_back(elsebb);
  f.builder.SetInsertPoint(elsebb);
  toplevel_codegen(z);
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
    llvm::cerr << act_env().name << ": looking for " << symtab.sym(tag).s
	       << ":" << (unsigned)idx << endl;
#endif
    assert(act_env().xmap.find(xmap_key(tag, idx)) != act_env().xmap.end());
    return vref(tag, act_env().xmap[xmap_key(tag, idx)]);
  }
}

Value *interpreter::fref(int32_t tag, uint8_t idx, bool thunked)
{
  // local function reference; box the function as a value on the fly
  assert(!envstk.empty());
  if (idx == 0) {
    // function in current environment ('with'-bound)
    Env& f = *act_env().fmap.act()[tag];
    return fbox(f, thunked);
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
  if (f.n == 0 && !thunked)
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
    return call("pure_clos", f.local, thunked, f.tag, f.h, envptr(&f), f.n, x);
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

Value *interpreter::call(string name, bool local, bool thunked, int32_t tag,
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
  // thunked flag
  args.push_back(Bool(thunked));
  // tag
  args.push_back(SInt(tag));
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
  GlobalVariable *v = new GlobalVariable
    (ArrayType::get(Type::Int8Ty, strlen(s)+1), true,
     GlobalVariable::InternalLinkage, ConstantArray::get(s),
     "", module);
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
  GlobalVariable *v = new GlobalVariable
    (ArrayType::get(Type::Int8Ty, strlen(s)+1), true,
     GlobalVariable::InternalLinkage, ConstantArray::get(s),
     "", module);
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
    Constant *limbs = ConstantArray::get(ArrayType::get(Type::Int64Ty, n), u);
    v = new GlobalVariable
      (ArrayType::get(Type::Int64Ty, n), true,
       GlobalVariable::InternalLinkage, limbs, "", module);
  } else {
    // assume 32 bit limbs
    assert(sizeof(mp_limb_t) == 4);
    // second arg: array of unsigned ints (least significant limb first)
    size_t n = (size_t)(z->_mp_size>=0 ? z->_mp_size : -z->_mp_size);
    vector<Constant*> u(n);
    for (size_t i = 0; i < n; i++) u[i] = UInt(z->_mp_d[i]);
    Constant *limbs = ConstantArray::get(ArrayType::get(Type::Int32Ty, n), u);
    v = new GlobalVariable
      (ArrayType::get(Type::Int32Ty, n), true,
       GlobalVariable::InternalLinkage, limbs, "", module);
  }
  // "cast" the int array to a int*
  ptr = e.CreateGEP(v, Zero, Zero);
}

// Debugger calls.

Value *interpreter::debug(const char *format)
{
  Function *f = module->getFunction("pure_debug");
  assert(f);
  Env& e = act_env();
  GlobalVariable *v = new GlobalVariable
    (ArrayType::get(Type::Int8Ty, strlen(format)+1), true,
     GlobalVariable::InternalLinkage, ConstantArray::get(format),
     "", module);
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
  GlobalVariable *v = new GlobalVariable
    (ArrayType::get(Type::Int8Ty, strlen(format)+1), true,
     GlobalVariable::InternalLinkage, ConstantArray::get(format),
     "", module);
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
  GlobalVariable *v = new GlobalVariable
    (ArrayType::get(Type::Int8Ty, strlen(format)+1), true,
     GlobalVariable::InternalLinkage, ConstantArray::get(format),
     "", module);
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
  GlobalVariable *v = new GlobalVariable
    (ArrayType::get(Type::Int8Ty, strlen(format)+1), true,
     GlobalVariable::InternalLinkage, ConstantArray::get(format),
     "", module);
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

void interpreter::unwind(int32_t tag)
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
    if (f.m > 0) argt.insert(argt.begin(), Type::Int32Ty);
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
      have_c_func = sys::DynamicLibrary::SearchForAddressOfSymbol(name);
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
	f.tag == 0 || symtab.sym(f.tag).modno >= 0 ||
	symtab.sym(f.tag).prec < 10)
      scope = Function::InternalLinkage;
#if USE_FASTCC
    if (!name.empty()) cc = CallingConv::Fast;
#endif
    /* Mangle the name of the C-callable wrapper if it's private, or would
       shadow another C function. */
    string pure_name = name;
    if (f.tag > 0 && symtab.sym(f.tag).modno >= 0)
      pure_name = "$$private."+name;
    else if (have_c_func)
      pure_name = "$$pure."+name;
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
      BasicBlock *bb = BasicBlock::Create("entry", f.h);
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
      if (verbose&verbosity::dump) f.h->print(std::cout);
    }
  }
#if DEBUG>1
  llvm::cerr << "PROLOG FUNCTION " << f.name << endl;
#endif
  // create a new basic block to start insertion into
  BasicBlock *bb = BasicBlock::Create("entry", f.f);
  f.builder.SetInsertPoint(bb);
#if DEBUG>1
  if (!f.name.empty()) { ostringstream msg;
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
  llvm::cerr << "BODY FUNCTION " << f.name << endl;
#endif
  BasicBlock *bodybb = BasicBlock::Create("body");
  f.builder.CreateBr(bodybb);
  f.f->getBasicBlockList().push_back(bodybb);
  f.builder.SetInsertPoint(bodybb);
#if DEBUG>1
  if (!f.name.empty()) { ostringstream msg;
    msg << "body " << f.name;
    debug(msg.str().c_str()); }
#endif
  BasicBlock *failedbb = BasicBlock::Create("failed");
  // emit the matching code
  complex_match(pm, failedbb);
  // emit code for a failed match
  f.f->getBasicBlockList().push_back(failedbb);
  f.builder.SetInsertPoint(failedbb);
  if (nodefault) {
    // failed match is fatal, throw an exception
#if DEBUG>1
  if (!f.name.empty()) { ostringstream msg;
    msg << "failed " << f.name << ", exception";
    debug(msg.str().c_str()); }
#endif
    unwind(symtab.failed_match_sym().f);
  } else {
#if DEBUG>1
  if (!f.name.empty()) { ostringstream msg;
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
    Value *defaultv =
      call("pure_clos", f.local, true, f.tag, f.h, envptr(&f), f.n, x);
    for (size_t i = 0; i < f.n; ++i) {
      Value *arg = f.args[i];
      assert(arg->getType() == ExprPtrTy);
      defaultv = apply(defaultv, arg);
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
  if (verbose&verbosity::dump) f.f->print(std::cout);
#if DEBUG>1
  llvm::cerr << "END BODY FUNCTION " << f.name << endl;
#endif
}

// Helper function to emit special code.

void interpreter::unwind_iffalse(Value *v)
{
  // throw an exception if v == false
  Env& f = act_env();
  assert(f.f!=0);
  BasicBlock *errbb = BasicBlock::Create("err");
  BasicBlock *okbb = BasicBlock::Create("ok");
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
  BasicBlock *errbb = BasicBlock::Create("err");
  BasicBlock *okbb = BasicBlock::Create("ok");
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
  if (!f.name.empty()) { ostringstream msg;
    msg << "simple match " << f.name;
    debug(msg.str().c_str()); }
#endif
  Value *tagv = 0;
  // check for thunks which must be forced
  if (t.tag != EXPR::VAR || t.ttag != 0) {
    // do a quick check on the tag value
    tagv = f.CreateLoadGEP(x, Zero, Zero, "tag");
    Value *checkv = f.builder.CreateICmpEQ(tagv, Zero, "check");
    BasicBlock *forcebb = BasicBlock::Create("force");
    BasicBlock *skipbb = BasicBlock::Create("skip");
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
    BasicBlock *okbb = BasicBlock::Create("ok");
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
    BasicBlock *okbb = BasicBlock::Create("ok");
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
    assert(0 && "not implemented");
    break;
  case EXPR::MATRIX:
    assert(0 && "not implemented");
    break;
  case EXPR::APP: {
    // first match the tag...
    BasicBlock *ok1bb = BasicBlock::Create("arg1");
    BasicBlock *ok2bb = BasicBlock::Create("arg2");
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
  if (f.n == 1 && f.b && pm->r.size() == 1 && pm->r[0].qual.is_null()) {
    Value *arg = f.args[0];
    // emit the matching code
    BasicBlock *matchedbb = BasicBlock::Create("matched");
    state *start = pm->start;
    simple_match(arg, start, matchedbb, failedbb);
    // matched => emit code for the reduct, and return the result
    f.f->getBasicBlockList().push_back(matchedbb);
    f.builder.SetInsertPoint(matchedbb);
#if DEBUG>1
    if (!f.name.empty()) { ostringstream msg;
      msg << "exit " << f.name << ", result: " << pm->r[0].rhs;
      debug(msg.str().c_str()); }
#endif
    toplevel_codegen(pm->r[0].rhs);
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
  BasicBlock *statebb = BasicBlock::Create(mklabel("state", s->s));
  f.builder.CreateBr(statebb);
  f.f->getBasicBlockList().push_back(statebb);
  f.builder.SetInsertPoint(statebb);
#if DEBUG>1
  if (!f.name.empty()) { ostringstream msg;
    msg << "complex match " << f.name << ", state " << s->s;
    debug(msg.str().c_str()); }
#endif
  // blocks for retrying with default transitions after a failed match
  BasicBlock *retrybb = BasicBlock::Create(mklabel("retry.state", s->s));
  BasicBlock *defaultbb = BasicBlock::Create(mklabel("default.state", s->s));
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
    BasicBlock *forcebb = BasicBlock::Create("force");
    BasicBlock *skipbb = BasicBlock::Create("skip");
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
    for (t = t0, i = 0; t != s->tr.end(); t++, i++) {
      // first create the block for this specific transition
      BasicBlock *bb = BasicBlock::Create(mklabel("trans.state", s->s, t->st->s));
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
	    BasicBlock::Create(mklabel("begin.state", s->s, -t->tag));
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
	    BasicBlock::Create(mklabel("next.state", s->s, -tag));
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
	    assert(0 && "not implemented");
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
	(BasicBlock::Create(mklabel("trans.state", s->s, t->st->s)));
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
  BasicBlock* rulebb = BasicBlock::Create(mklabel("rule.state", s->s, rl.front()));
  f.builder.CreateBr(rulebb);
  while (r != rl.end()) {
    const rule& rr = rules[*r];
    reduced.insert(*r);
    f.fmap.select(*r);
    f.f->getBasicBlockList().push_back(rulebb);
    f.builder.SetInsertPoint(rulebb);
#if DEBUG>1
    if (!f.name.empty()) { ostringstream msg;
      msg << "complex match " << f.name << ", trying rule #" << *r;
      debug(msg.str().c_str()); }
#endif
    if (rr.qual.is_null()) {
      // rule always matches, generate code for the reduct and bail out
#if DEBUG>1
      if (!f.name.empty()) { ostringstream msg;
	msg << "exit " << f.name << ", result: " << rr.rhs;
	debug(msg.str().c_str()); }
#endif
      toplevel_codegen(rr.rhs);
      break;
    }
    // check the guard
    Value *iv = 0;
    if (rr.qual.ttag() == EXPR::INT)
      // optimize the case that the guard is an ::int (constant or application)
      iv = get_int(rr.qual);
    else if (rr.qual.ttag() != 0) {
      // wrong type of constant; raise an exception
      // XXXTODO: we might want to optionally invoke the debugger here
      unwind(symtab.failed_cond_sym().f);
#if DEBUG>1
      if (!f.name.empty()) { ostringstream msg;
	msg << "exit " << f.name << ", bad guard (exception)";
	debug(msg.str().c_str()); }
#endif
      break;
    } else
      // typeless expression, will be checked at runtime
      iv = get_int(rr.qual);
    // emit the condition (turn the previous result into a flag)
    Value *condv = f.builder.CreateICmpNE(iv, Zero, "cond");
    BasicBlock *okbb = BasicBlock::Create("ok");
    // determine the next rule block ('failed' if none)
    BasicBlock *nextbb;
    if (++r != rl.end())
      nextbb = BasicBlock::Create(mklabel("rule.state", s->s, *r));
    else
      nextbb = failedbb;
    f.builder.CreateCondBr(condv, okbb, nextbb);
    // ok => guard succeeded, return the reduct, otherwise we fall through
    // to the next rule (if any), or bail out with failure
    f.f->getBasicBlockList().push_back(okbb);
    f.builder.SetInsertPoint(okbb);
#if DEBUG>1
    if (!f.name.empty()) { ostringstream msg;
      msg << "exit " << f.name << ", result: " << rr.rhs;
      debug(msg.str().c_str()); }
#endif
    toplevel_codegen(rr.rhs);
    rulebb = nextbb;
  }
  f.fmap.first();
}
