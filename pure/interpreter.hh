#ifndef INTERPRETER_HH
#define INTERPRETER_HH

#include <llvm/DerivedTypes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Module.h>
#include <llvm/ModuleProvider.h>
#include <llvm/PassManager.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Target/TargetData.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Support/IRBuilder.h>

#include <time.h>
#include <set>
#include <string>
#include "expr.hh"
#include "matcher.hh"
#include "symtable.hh"
#include "runtime.h"

/* Add some debugging output (disable in release version!). */
#ifndef DEBUG
#define DEBUG 1 // extra sanity checks
//#define DEBUG 2 // debug runtime + code execution etc.
#endif
/* Extra memory debugging code (slow!). See runtime.cc for details. */
#ifndef MEMDEBUG
#define MEMDEBUG 0
#endif

/* Experimental support for the "fast" calling convention which is needed to
   get tail call elimination. */
#define USE_FASTCC 1

/* Alternative code generation for the case of proper lists and tuples. This
   is a kludge to work around performance issues with the JIT which (as of
   LLVM 2.3) gets very slow with deeply nested call graphs. The code enabled
   with this option here is actually less efficient for small list/tuple
   values, which is why we impose a lower bound on the list/tuple size (10 by
   default, use 0 to disable this option). */
#define LIST_KLUDGE 10

using namespace std;

/* The Pure interpreter. */

class interpreter;

#include "parser.hh"

// Announce to Flex the prototype we want for lexing function, ...
#define YY_DECL \
  yy::parser::token_type						\
  yylex (yy::parser::semantic_type* yylval,				\
	 yy::parser::location_type* yylloc, interpreter& interp)
// ... and declare it for the parser's sake.
YY_DECL;

// verbosity levels, these can be ORed together
namespace verbosity {
  enum { none = 0, defs = 0x1, envs = 0x2, code = 0x4, dump = 0x8,
	 parser = 0x10, lexer = 0x20 };
};

/* Handle exceptions in the interpreter. */

class err {
public:
  err(const string& what) : m_what(what) { };
  const string& what() const { return m_what; };
private:
  string m_what;
};

/* Data structures used in code generation. */

struct Env;

struct GlobalVar {
  // global variable
  llvm::GlobalVariable* v;
  pure_expr *x;
  GlobalVar() { v = 0; x = 0; }
};

struct VarInfo {
  // info about captured variable
  uint32_t v;	// local proxy (offset into extra parameter block)
  int32_t vtag;	// symbol
  uint8_t idx;	// de Bruijn index
  path p;	// subterm path
  VarInfo(uint32_t _v, int32_t _vtag, uint8_t _idx, path _p)
    : v(_v), vtag(_vtag), idx(_idx), p(_p) {}
};

#define Builder llvm::IRBuilder

typedef list<Env*> EnvStack;
typedef map<int32_t,Env*> EnvMap;
typedef pair<int32_t,uint8_t> xmap_key;

/* Manage local function environments. The FMap structure is organized as a
   forest with one root per rule and one child per 'with' clause. Each node of
   the forest holds a map mapping function symbols to the corresponding
   environments. Initially, there is just one root holding an empty map. New
   roots can be added with 'next', new children with 'push'; 'pop' backs out
   to the parent level.

   Child nodes are initialized to a copy of its parent map, to which you can
   then add any local bindings. After the structure has been built, you can
   use 'first' to reposition the "cursor" so that it points to the first root
   and then traverse the forest using the same sequence of calls to 'next',
   'push' and 'pop'. The 'select' method can be used to position the cursor at
   the given root. The 'act' method returns the current map.

   Implementation: The forest is encoded as a collection of vectors: 'm' holds
   the map for each node, 'root' the node number of each root, 'pred' the
   parent link for each node, and 'succ' the link to the next sibling of each
   node. A 'pred' value of -1 denotes a root node, in which case 'succ' points
   to the next root (or contains -1 to indicate the last root). The 'idx'
   member points to the current node, 'lastidx' to the most recently visited
   child after a 'pop' operation (-1 otherwise). */

struct FMap {
  // map of each node
  vector<EnvMap*> m;
  // vectors encoding the forest structure (see explanation above)
  vector<int32_t> root, pred, succ;
  // index of current node and most previously visited child
  int32_t idx, lastidx;
  // constructor (create one empty map by default)
  FMap() : m(1), root(1, 0), pred(1, -1), succ(1, -1), idx(0), lastidx(-1)
  { m[0] = new EnvMap; }
  // assignment
  FMap& operator= (const FMap& f);
  // clear
  void clear();
  // set index to first, next and given root node
  void first();
  void next();
  void select(size_t n);
  // set index to the parent or next child of the current node
  void push();
  void pop();
  // access the current map
  EnvMap& act() { return *m[idx]; }
};

struct Env {
  // function environment
  int32_t tag; // function id, zero for anonymous functions
  string name; // LLVM assembly function name
  // n = #function args, m = #extra args (captured environment)
  uint32_t n, m;
  // f = the (internal) LLVM function, h = the C-callable stub (if needed)
  llvm::Function *f, *h;
  // fp = pointer to the JITed function (executable code)
  void *fp;
  // function arguments (f.n x expr*)
  vector<llvm::Value*> args;
  // environment pointer (expr**)
  llvm::Value *envs;
  // mapping of captured variables to the corresponding locals
  map<xmap_key,uint32_t > xmap;
  // info about captured variables
  list<VarInfo> xtab;
  // local function environments
  FMap fmap;
  // propagation links for environment variables (pointers to call sites)
  // e in prop means that there's a call with de Bruijn index prop[e] at e
  map<Env*,uint8_t> prop;
  // pattern binding policy:
  // b = true: subterm paths are relative to first arg (assert n==1)
  // b = false: subterm paths are relative to the function call, i.e., last
  // parameter is 1.., second-last 01.., etc.
  bool b;
  // whether the function is local (possibly needs captured environment)
  bool local;
  // builder for this function
  Builder builder;
  // parent environment (if any)
  Env *parent;
  // reference counter
  uint32_t refc;
  // convenience functions for invoking CreateGEP() and CreateLoad()
  llvm::Value *CreateGEP
  (llvm::Value *x, llvm::Value *i, const char* name = "")
  { return builder.CreateGEP(x, i, name); }
  llvm::Value *CreateGEP
  (llvm::Value *x, llvm::Value *i, llvm::Value *j, const char* name = "")
  { llvm::Value* idxs[2] = { i, j };
    return builder.CreateGEP(x, idxs, idxs+2, name); }
  llvm::Value *CreateGEP
  (llvm::Value *x, llvm::Value *i, llvm::Value *j, llvm::Value *k,
   const char* name = "")
  { llvm::Value* idxs[3] = { i, j, k };
    return builder.CreateGEP(x, idxs, idxs+3, name); }
  llvm::LoadInst *CreateLoadGEP
  (llvm::Value *x, llvm::Value *i, const char* name = "")
  { return builder.CreateLoad(CreateGEP(x, i), name); }
  llvm::LoadInst *CreateLoadGEP
  (llvm::Value *x, llvm::Value *i, llvm::Value *j, const char* name = "")
  { return builder.CreateLoad(CreateGEP(x, i, j), name); }
  llvm::LoadInst *CreateLoadGEP
  (llvm::Value *x, llvm::Value *i, llvm::Value *j, llvm::Value *k,
   const char* name = "")
  { return builder.CreateLoad(CreateGEP(x, i, j, k), name); }
  // simplified interface to CreateCall()
  llvm::CallInst *CreateCall(llvm::Function *f,
			     const vector<llvm::Value*>& args);
  // interface to CreateRet() which also takes care of collecting temporaries
  // and patching up tail calls
  llvm::ReturnInst *CreateRet(llvm::Value *v);
  // print the code of all functions in an environment, recursively
  void print(ostream& os) const;
  // default constructor
  Env()
    : tag(0), n(0), m(0), f(0), h(0), fp(0), args(0), envs(0),
      b(false), local(false), parent(0), refc(0) {}
  // environment for an anonymous closure with given body x
  Env(int32_t _tag, uint32_t _n, expr x, bool _b, bool _local = false)
    : tag(_tag), n(_n), m(0), f(0), h(0), fp(0), args(n), envs(0),
      b(_b), local(_local), parent(0), refc(0)
  {
    if (envstk.empty()) {
      assert(!local);
      build_map(x);
      propagate_maps();
    } else {
      assert(local);
      parent = envstk.front();
    }
    fmap.first();
  }
  // environment for a named closure with given definition info
  Env(int32_t _tag, const env_info& info, bool _b, bool _local = false)
    : tag(_tag), n(info.argc), m(0), f(0), h(0), fp(0), args(n), envs(0),
      b(_b), local(_local), parent(0), refc(0)
  {
    if (envstk.empty()) {
      assert(!local);
      build_map(info);
      propagate_maps();
    } else {
      assert(local);
      parent = envstk.front();
    }
    fmap.first();
  }
  // assignment -- this is only allowed if the lvalue is an uninitialized
  // environment for which no LLVM function has been created yet, or if it is
  // a global function to be overridden
  Env& operator= (const Env& e);
  // clearing an environment; this also removes the LLVM code of the function
  void clear();
  // destructor
  ~Env() { clear(); }
private:
  // build the fmap and xmap tables
  static EnvStack envstk;
  static set<Env*> props;
  void push(const char *msg);
  void pop();
  void build_map(expr x);
  void build_map(const rulel& rl);
  void build_map(expr x, rulel::const_iterator r, rulel::const_iterator end);
  void build_map(const env_info& info);
  void promote_map();
  size_t propagate_map();
  static void propagate_maps();
};

struct ExternInfo {
  // info about extern (C) functions callable from the Pure script
  int32_t tag;				// function symbol
  string name;				// real function name
  const llvm::Type* type;		// return type
  vector<const llvm::Type*> argtypes;	// argument types
  llvm::Function *f;			// Pure wrapper for the external
  ExternInfo()
    : tag(0), type(0), argtypes(0), f(0)
  {}
  ExternInfo(int32_t _tag, const string&_name, const llvm::Type *_type,
	     vector<const llvm::Type*> _argtypes, llvm::Function *_f)
    : tag(_tag), name(_name), type(_type), argtypes(_argtypes), f(_f)
  {}
};

ostream &operator<< (ostream& os, const ExternInfo& info);

/* The interpreter. */

typedef set<int32_t> funset;

typedef pair<expr,expr> comp_clause;
typedef list<comp_clause> comp_clause_list;

class interpreter
{
public:
  interpreter();
  virtual ~interpreter();
  // Populate the global environment with some useful variables.
  void init_sys_vars(const string& version = "",
		     const string& host = "",
		     const list<string>& argv = list<string>());

  // Option data. You can modify these according to your needs.
  uint8_t verbose;   // debugging output from interpreter
  bool interactive;  // interactive mode
  bool ttymode;      // connected to a tty
  bool override;     // override mode
  bool stats;        // stats mode (print execution times)
  uint8_t temp;      // temporary level (purgable definitions)
  string ps;         // prompt string
  string libdir;     // library dir to search for source files
  string histfile;   // command history file
  string modname;    // name of output (LLVM) module

  // Additional directories to search for sources and libraries.
  list<string> includedirs, librarydirs;

  // Interpreter state. For internal use only.
  int nerrs;	     // current error count
  string errmsg;     // last reported error (runstr)
  int32_t modno;     // current module key
  int32_t modctr;    // next available module key
  string source;     // the source being parsed
  const char *source_s; // source pointer if input comes from a string
  set<string> sources; // the list of all scripts which have been loaded
  symtable symtab;   // the symbol table
  pure_expr *result; // last computed result
  clock_t clocks;    // last evaluation time, if stats is set
  exprl last;        // last processed lhs collection
  env globenv;       // global function and variable environment
  env macenv;        // global macro environment
  funset dirty;      // "dirty" function entries which need a recompile
  pure_mem *mem;     // runtime expression memory
  pure_expr *exps;   // head of the free list (available expression nodes)
  pure_expr *tmps;   // temporaries list (to be collected after exceptions)

  /*************************************************************************
             Stuff below is to be used by application programs.
   *************************************************************************/

  /* Parse and execute the given source file (stdin if empty), or the given
     list of files. If 'check' is true (the default), a full search is
     performed for relative pathnames (checking include directories and
     PURELIB to locate the script file) and the script is only loaded if it
     wasn't included before. If 'sticky' is true (default is false), the
     current module namespace is kept, otherwise a new namespace is created
     for the loaded module. Using this option isn't recommended, but it is
     used internally by the 'run' command and the '-i' option to give access
     to the private symbols of the executed script when running interactively.

     Returns the last computed expression (if any). (This expression is owned
     by the interpreter and must *not* be freed by the caller.) This is the
     main interface function. If interactive is true, readline is used to get
     interactive input from the user, using ps as the prompt string. Please
     note that due to some global data shared by different interpreter
     instances, you can't run two interpreters concurrently right now. (It is
     possible to run them sequentially, though.) */
  pure_expr *run(const string& source, bool check = true,
		 bool sticky = false);
  pure_expr *run(const list<string>& sources, bool check = true,
		 bool sticky = false);

  /* This works like run() above, but takes the source directly from a
     string. No error messages will be printed, instead any errors reported
     during the most recent invokation of this method are available in
     errmsg. */
  pure_expr *runstr(const string& source);

  /* Evaluate a (compile time) expression and return the (runtime expression)
     result. Returns a null pointer if an exception occurred during the
     evaluation. In such a case, the variant with the extra e parameter
     returns the runtime expression thrown by the exception, if any. Both the
     result and the exception value (if any) are to be freed by the caller. */
  pure_expr *eval(expr& x);
  pure_expr *eval(expr& x, pure_expr*& e);

  /* Evaluate an expression and define global variables. This works like
     eval() above, but also binds the variables in pat to the corresponding
     values. Also, these routines throw a C++ exception of the err type if any
     of the variable symbols to be defined is already bound to a different
     kind of symbol. Otherwise the result is the evaluated expression to be
     matched. Returns a null pointer if an exception occurred during the
     evaluation or if the pattern failed to match. Both the result and the
     exception value (if any) are to be freed by the caller. */
  pure_expr *defn(expr pat, expr& x);
  pure_expr *defn(expr pat, expr& x, pure_expr*& e);

  /* Bind a global variable to a given value. This binds the given variable
     symbol directly to the given value, without matching and evaluating
     anything. It is still checked that the variable symbol is not already
     bound to a different kind of symbol, otherwise an err exception is
     thrown. */
  void defn(int32_t tag, pure_expr *x);
  void defn(const char *varname, pure_expr *x)
  { defn(symtab.sym(varname).f, x); }

  /* Constant definitions. These work like the variable definition methods
     above, but define constant symbols which are directly substituted into
     the right-hand sides of equations rather than being evaluated at
     runtime. The right-hand side expression is evaluated and matched against
     the left-hand side pattern as usual. Unlike variables, existing constant
     symbols cannot be redefined, so they have to be cleared before you can
     give them new values. */
  pure_expr *const_defn(expr pat, expr& x);
  pure_expr *const_defn(expr pat, expr& x, pure_expr*& e);

  /* Directly bind a given constant symbol to a given value. */
  void const_defn(int32_t tag, pure_expr *x);
  void const_defn(const char *varname, pure_expr *x)
  { const_defn(symtab.sym(varname).f, x); }

  /* Purge the definition of a (global constant, variable or function)
     symbol. If the given symbol is zero, pops the most most recent temporary
     definitions level, removing all definitions in that level. */
  void clear(int32_t tag = 0);

  /* Process pending compilations of function definitions. This is also done
     automatically when eval() or defn()/const_defn() is invoked. */
  void compile();

  /* Errors and warnings. These are for various types of messages from the
     compiler. Default is to write error messages to stdout. You might wish to
     derive from this class and override these to implement custom error
     handling. */
  virtual void error(const yy::location& l, const string& m);
  virtual void error(const string& m);
  virtual void warning(const yy::location& l, const string& m);
  virtual void warning(const string& m);

  /*************************************************************************
             Stuff below this line is to be used internally only.
   *************************************************************************/

  // Semantic routines used by the parser.

  env *build_env(rulel *r);
  env *build_env(expr x);
  void build_env(env& vars, expr x);
  void mark_dirty(int32_t f);
  void compile(expr x);
  void declare(bool priv, prec_t prec, fix_t fix, list<string> *ids);
  void define(rule *r);
  void define_const(rule *r);
  void exec(expr *x);
  void clearsym(int32_t f);
  rulel *default_lhs(exprl &l, rulel *rl);
  void add_rules(rulel &rl, rulel *r, bool b);
  void add_rules(env &e, rulel *r, bool toplevel = false);
  void add_rule(rulel &rl, rule &r, bool b);
  void add_rule(env &e, rule &r, bool toplevel = false);
  void add_simple_rule(rulel &rl, rule *r);
  void add_macro_rule(rule *r);
  void promote_ttags(expr f, expr x, expr u);
  void promote_ttags(expr f, expr x, expr u, expr v);
  expr bind(env& vars, expr x, bool b = true, path p = path());
  expr subst(const env& vars, expr x, uint8_t idx = 0);
  expr fsubst(const env& funs, expr x, uint8_t idx = 0);
  expr csubst(expr x);
  expr macsubst(expr x);
  expr varsubst(expr x, uint8_t offs);
  expr macred(expr x, expr y, uint8_t idx = 0);
  expr macval(expr x);
  void closure(expr& l, expr& r, bool b = true);
  void closure(rule& r, bool b = true);
  expr *uminop(expr *op, expr *x);
  expr *mkexpr(expr *x, expr *y);
  expr *mkexpr(expr *x, expr *y, expr *z);
  expr *mksym_expr(string *s, int8_t tag = 0);
  expr *mkas_expr(string *s, expr *x);
  expr *mkcond_expr(expr *x, expr *y, expr *z);
  expr *mklambda_expr(exprl *args, expr *body);
  expr *mkcase_expr(expr *x, rulel *rules);
  expr *mkwhen_expr(expr *x, rulel *rules);
  expr *mkwith_expr(expr *x, env *e);
  exprl *mkrow_exprl(expr *x);
  expr *mklist_expr(expr *x);
  expr *mklistcomp_expr(expr *x, comp_clause_list *cs);
  expr mklistcomp_expr(expr x, comp_clause_list::iterator cs,
		       comp_clause_list::iterator end);
  expr *mkmatcomp_expr(expr *x, comp_clause_list *cs);
  expr mkmatcomp_expr(expr x, size_t n, comp_clause_list::iterator cs,
		      comp_clause_list::iterator end);

  // LLVM code generation and execution.

  llvm::Module *module;
  llvm::ExecutionEngine *JIT;
  llvm::FunctionPassManager *FPM;
  llvm::StructType  *ExprTy, *IntExprTy, *DblExprTy, *StrExprTy, *PtrExprTy;
  llvm::StructType  *ComplexTy, *GSLMatrixTy, *GSLDoubleMatrixTy,
    *GSLComplexMatrixTy, *GSLIntMatrixTy;
  llvm::PointerType *ExprPtrTy, *ExprPtrPtrTy;
  llvm::PointerType *IntExprPtrTy, *DblExprPtrTy, *StrExprPtrTy, *PtrExprPtrTy;
  llvm::PointerType *VoidPtrTy, *CharPtrTy, *IntPtrTy, *DoublePtrTy;
  llvm::PointerType *ComplexPtrTy, *GSLMatrixPtrTy, *GSLDoubleMatrixPtrTy,
    *GSLComplexMatrixPtrTy, *GSLIntMatrixPtrTy;
  const llvm::Type *named_type(string name);
  const char *type_name(const llvm::Type *type);
  map<int32_t,GlobalVar> globalvars;
  map<int32_t,Env> globalfuns;
  list<pure_exception> estk;
  pure_expr** sstk; size_t sstk_cap, sstk_sz;
  llvm::GlobalVariable *sstkvar;
#if DEBUG
  set<pure_expr*> mem_allocations;
#endif
  map<int32_t,ExternInfo> externals;
  llvm::Function *declare_extern(void *fp, string name, string restype,
				 int n, ...);
  llvm::Function *declare_extern(string name, string restype,
				 const list<string>& argtypes,
				 bool varargs = false, void *fp = 0,
				 string asname = "");
private:
  Env *fptr;
  llvm::GlobalVariable *fptrvar;
  llvm::Value *envptr(Env *f);
  EnvStack envstk;
  void push(const char *msg, Env *e);
  void pop(Env *e);
  Env *find_stacked(int32_t tag);
  Env& act_env() { assert(!envstk.empty()); return *envstk.front(); }
  Builder& act_builder() { return act_env().builder; }
  pure_expr *const_value(expr x);
  pure_expr *const_matrix_value(expr x);
  pure_expr *const_app_value(expr x);
  expr pure_expr_to_expr(pure_expr *x);
  pure_expr *doeval(expr x, pure_expr*& e);
  pure_expr *dodefn(env vars, expr lhs, expr rhs, pure_expr*& e);
  llvm::Value *codegen(expr x);
  void toplevel_codegen(expr x);
  llvm::Value *builtin_codegen(expr x);
  llvm::Value *get_int(expr x);
  llvm::Value *get_double(expr x);
  llvm::Value *when_codegen
  (expr x, matcher *m, rulel::const_iterator r, rulel::const_iterator end);
  llvm::Value *funcall(Env *f, llvm::Value *x);
  llvm::Value *funcall(Env *f, uint32_t n, expr x);
  llvm::Value *funcall(int32_t tag, uint8_t idx, uint32_t n, expr x);
  llvm::Value *external_funcall(int32_t tag, uint32_t n, expr x);
  llvm::Value *call(llvm::Value *x);
  llvm::Value *apply(llvm::Value *x, llvm::Value *y);
  llvm::Value *cond(expr x, expr y, expr z);
  void toplevel_cond(expr x, expr y, expr z);
  llvm::Value *fbox(Env& f, bool thunked = false);
  llvm::Value *cbox(int32_t tag);
  llvm::Value *ibox(llvm::Value *i);
  llvm::Value *ibox(int32_t i);
  llvm::Value *zbox(const mpz_t& z);
  llvm::Value *dbox(llvm::Value *d);
  llvm::Value *dbox(double d);
  llvm::Value *sbox(const char *s);
  llvm::Value *pbox(void *p);
  llvm::Value *vref(int32_t tag, path p);
  llvm::Value *vref(int32_t tag, uint32_t offs);
  llvm::Value *vref(int32_t tag, uint8_t idx, path p);
  llvm::Value *fref(int32_t tag, uint8_t idx, bool thunked = false);
  llvm::Value *fcall(Env& f, vector<llvm::Value*>& args,
		     vector<llvm::Value*>& env);
  llvm::Value *fcall(Env& f, vector<llvm::Value*>& env)
  { vector<llvm::Value*> args; return fcall(f, args, env); }
  llvm::Value *call(string name, bool local, bool thunked, int32_t tag,
		    llvm::Function* f, llvm::Value *e, uint32_t argc,
		    vector<llvm::Value*>& vars);
  llvm::Value *call(string name, llvm::Value *x);
  llvm::Value *call(string name, llvm::Value *x, llvm::Value *y);
  llvm::Value *call(string name, llvm::Value *x, llvm::Value *y,
		    llvm::Value *z);
  llvm::Value *call(string name, llvm::Value *x, llvm::Value *y,
		    llvm::Value *z, llvm::Value *t);
  llvm::Value *call(string name, int32_t i);
  llvm::Value *call(string name, const mpz_t& z);
  llvm::Value *call(string name, double d);
  llvm::Value *call(string name, const char *s);
  llvm::Value *call(string name, void *p);
  llvm::Value *call(string name, llvm::Value *x, const mpz_t& z);
  llvm::Value *call(string name, llvm::Value *x, const char *s);
  void make_bigint(const mpz_t& z, llvm::Value*& sz, llvm::Value*& ptr);
  llvm::Value *debug(const char *format);
  llvm::Value *debug(const char *format, llvm::Value *x);
  llvm::Value *debug(const char *format, llvm::Value *x, llvm::Value *y);
  llvm::Value *debug(const char *format, llvm::Value *x, llvm::Value *y,
		     llvm::Value *z);
  void unwind(int32_t tag = 0);
  llvm::Function *fun(string name, matcher *pm, bool nodefault = false);
  llvm::Function *fun_prolog(string name);
  void fun_body(matcher *pm, bool nodefault = false);
  void fun_finish();
  void simple_match(llvm::Value *x, state*& s,
		    llvm::BasicBlock *matchedbb, llvm::BasicBlock *failedbb);
  void complex_match(matcher *pm, llvm::BasicBlock *failedbb);
  void complex_match(matcher *pm, const list<llvm::Value*>& xs, state *s,
		     llvm::BasicBlock *failedbb, set<rulem>& reduced);
  void try_rules(matcher *pm, state *s, llvm::BasicBlock *failedbb,
		 set<rulem>& reduced);
  void unwind_iffalse(llvm::Value *v);
  void unwind_iftrue(llvm::Value *v);
  llvm::Value *check_tag(llvm::Value *v, int32_t tag);
  void verify_tag(llvm::Value *v, int32_t tag);
  list<char*> cache;
  const char *mklabel(const char *name, uint32_t i);
  const char *mklabel(const char *name, uint32_t i, uint32_t j);
  const char *mkvarlabel(int32_t tag);
  void clear_cache();

public:
  // Global data, saved and restored by the run method.
  static uint8_t g_verbose;
  static bool g_interactive;
  static interpreter* g_interp;
  // not saved
  static int brkflag, brkmask;
  static char *baseptr;
  static int stackmax;
  static int stackdir;

private:

  // Utility functions to quickly save and restore the global state.
  struct globals {
    uint8_t verbose;
    bool interactive;
    interpreter* interp;
    globals()
      : verbose(g_verbose), interactive(g_interactive), interp(g_interp) {}
  };
  void save_globals(globals& g)
  {
    if (g_interp != this) {
      g_interp = this;
      g_verbose = verbose;
      g_interactive = interactive;
    }
  }
  void restore_globals(globals& g)
  {
    if (g_interp != g.interp) {
      g_interp = g.interp;
      g_verbose = g.verbose;
      g_interactive = g.interactive;
    }
  }

  // Interface to the lexer.
public:
  bool declare_op;
  string srcdir;
private:
  bool lex_begin(const string& fname = "");
  void lex_end();
};

#endif // ! INTERPRETER_HH
