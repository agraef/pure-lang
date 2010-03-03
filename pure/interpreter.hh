
/* Copyright (c) 2008-2010 by Albert Graef <Dr.Graef@t-online.de>.

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

#ifndef INTERPRETER_HH
#define INTERPRETER_HH

#include <llvm/DerivedTypes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Module.h>
#include <llvm/PassManager.h>
#include <llvm/GlobalValue.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Target/TargetData.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Support/IRBuilder.h>

#include "config.h"

#ifdef HAVE_LLVM_MODULEPROVIDER_H
#include <llvm/ModuleProvider.h>
#else
// LLVM 2.7 and later don't have this header any more.
#define LLVM27 1
#endif

#if LLVM26
#include "llvm/LLVMContext.h"
#endif

#include <time.h>
#include <set>
#include <string>
#include "expr.hh"
#include "matcher.hh"
#include "symtable.hh"
#include "runtime.h"

#include "parserdefs.hh"
#include "parser.hh"

/* Add some debugging output (disable in release version!). */
#ifndef DEBUG
#define DEBUG 1 // extra sanity checks
//#define DEBUG 2 // debug runtime + code execution etc.
#endif
/* Extra memory debugging code (slow!). See runtime.cc for details. */
#ifndef MEMDEBUG
#define MEMDEBUG 0
#endif

/* Support for the "fast" calling convention which is needed to get tail call
   elimination. As of LLVM 2.6, this is still broken on some systems,
   specifically ppc. You can also disable this through configure with the
   --disable-fastcc option, or at runtime with the --notc option. */
#ifndef USE_FASTCC
#ifdef HAVE_FASTCC
#define USE_FASTCC 1
#else
#define USE_FASTCC 0
#endif
#endif

/* Defer JIT compilation of global functions until they're called for the
   first time. Speeds up compilation of small programs at the expense of some
   (negligible) runtime overhead. */
#ifndef DEFER_GLOBALS
#define DEFER_GLOBALS 1
#endif

/* Experimental support for fast code generation, at the expense of code
   quality. As of LLVM 2.4, this doesn't seem to have much effect, and in LLVM
   2.6 it doesn't seem to work at all. We recommend to leave this disabled. */
#ifndef FAST_JIT
#define FAST_JIT 0
#endif

/* Alternative code generation for aggregate values (currently lists, tuples
   and matrices are supported). This works around performance issues with the
   JIT which (as of LLVM 2.3) gets very slow with deeply nested call graphs.
   It also enables some optimizations to tidy up the code for constant
   aggregates. The code enabled with this option here is actually less
   efficient for small aggregates, which is why we impose a lower bound on the
   aggregate size (10 by default, use 0 to disable this option). */
#ifndef LIST_OPT
#define LIST_OPT 10
#endif

/* Activation stack block size. This should be large enough to avoid bunches
   of smaller allocations. The activation stack will be resized automatically
   in chunks of this size when necessary. */
#ifndef ASTACKSZ
#define ASTACKSZ 0x1000
#endif

using namespace std;

/* The Pure interpreter. */

class interpreter;

// verbosity levels, these can be ORed together
namespace verbosity {
  enum { none = 0, defs = 0x1, envs = 0x2, code = 0x4, dump = 0x8,
	 parser = 0x10, lexer = 0x20, compiler = 0x40 };
};

/* Data structures used in code generation. */

struct Env;

struct GlobalVar {
  // global variable
  llvm::GlobalVariable* v;
  pure_expr *xx;
  pure_expr *&x;
  GlobalVar() : x(xx) { v = 0; x = 0; }
  GlobalVar(pure_expr **xp) : x(*xp) { v = 0; x = 0; }
  GlobalVar(const GlobalVar& var) : x((&var.x==&var.xx)?xx:var.x)
  { v = var.v; if (&var.x==&var.xx) x = var.x; }
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

#ifdef NEW_BUILDER
/* LLVM 2.4 has a new IRBuilder class which takes some optional template
   parameters. */
#define Builder llvm::IRBuilder<>
#else
#define Builder llvm::IRBuilder
#endif

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
private:
  uint32_t key; // cached key of a local closure (see getkey() below)
  void add_key(uint32_t key, uint32_t *refp);
public:
  // descriptor for type of environment
  const char *descr;
  // n = #function args, m = #extra args (captured environment)
  uint32_t n, m;
  // f = the (internal) LLVM function, h = the C-callable stub (if needed)
  llvm::Function *f, *h;
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
  // auxiliary rule storage used by when_codegen()
  rule *rp;
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
  // reference counters
  uint32_t refc, *refp;
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
  llvm::ReturnInst *CreateRet(llvm::Value *v, const rule *rp = 0);
  // print the code of all functions in an environment, recursively
  void print(ostream& os) const;
  // default constructor
  Env()
    : tag(0), key(0), descr(0), n(0), m(0), f(0), h(0),
      args(0), envs(0), rp(0), b(false), local(false),
#ifdef LLVM26
      builder(llvm::getGlobalContext()),
#endif
      parent(0), refc(0), refp(new uint32_t)
  { *refp = 0; add_key(getkey(), refp); }
  // environment for an anonymous closure with given body x
  Env(int32_t _tag, const char *_descr, uint32_t _n, expr x,
      bool _b, bool _local = false)
    : tag(_tag), key(0), descr(_descr), n(_n), m(0), f(0), h(0),
      args(n), envs(0), rp(0), b(_b), local(_local),
#ifdef LLVM26
      builder(llvm::getGlobalContext()),
#endif
      parent(0), refc(0), refp(new uint32_t)
  {
    *refp = 0; add_key(getkey(), refp);
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
    : tag(_tag), key(0), descr(0), n(info.argc), m(0), f(0), h(0),
      args(n), envs(0), rp(0), b(_b), local(_local),
#ifdef LLVM26
      builder(llvm::getGlobalContext()),
#endif
      parent(0), refc(0), refp(new uint32_t)
  {
    *refp = 0; add_key(getkey(), refp);
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
  // dummy environment for an external
  Env(int32_t _tag, uint32_t _n, bool _local = false)
    : tag(_tag), key(0), descr(0), n(_n), m(0), f(0), h(0),
      args(n), envs(0), rp(0), b(false), local(false),
#ifdef LLVM26
      builder(llvm::getGlobalContext()),
#endif
      parent(0), refc(0), refp(new uint32_t)
  { *refp = 0; add_key(getkey(), refp); }
  // assignment -- this is only allowed if the lvalue is an uninitialized
  // environment for which no LLVM function has been created yet, or if it is
  // a global function to be overridden
  Env& operator= (const Env& e);
  // clearing an environment; this also removes the LLVM code of the function
  void clear();
  // destructor
  ~Env() { clear(); }
private:
  static uint32_t act_key;
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
public:
  uint32_t getkey() // key which identifies a closure
  { if (key==0) key = ++act_key; return key; }
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

struct DebugInfo {
  // debugger activation record
  size_t n;			// stack level
  size_t sz;			// shadow stack size
  Env *e;			// environment
  const rule *r;		// executed rule
  env vars;			// lhs variable bindings
  pure_expr **args, **envs;	// pointers to args and environment
  DebugInfo(size_t _n, size_t _sz, Env *_e) : n(_n), sz(_sz), e(_e), r(0) {}
};

struct TagInfo {
  // tag file entries
  string tag;
  unsigned line, column;
  TagInfo(const string& t, unsigned l, unsigned c)
    : tag(t), line(l), column(c) {}
};

/* The interpreter. */

typedef set<int32_t> funset;

typedef pair<expr,expr> comp_clause;
typedef list<comp_clause> comp_clause_list;

struct nsinfo {
  // scoped namespaces data
  bool priv; // private/public flag (currently this isn't used by the parser)
  string parent; // parent namespace
  map< string, set<int32_t> > search_namespaces; // saved search namespaces
  nsinfo(const string& ns, const map< string, set<int32_t> >& nss)
    : priv(false), parent(ns), search_namespaces(nss) {}
};

class interpreter
{
public:
  interpreter();
  interpreter(int32_t nsyms, char *syms,
	      pure_expr ***vars, void **vals,
	      int32_t *arities, void **externs,
	      pure_expr ***sstk, void **fptr);
  virtual ~interpreter();
  // Populate the global environment with some useful variables.
  void init_sys_vars(const string& version = "",
		     const string& host = "",
		     const list<string>& argv = list<string>());

  // Option data. You can modify these according to your needs.
  uint8_t verbose;   // debugging output from interpreter
  bool compiling;    // batch compiler mode
  bool eager_jit;    // eager JIT (LLVM 2.7 or later)
  bool interactive;  // interactive mode
  bool debugging;    // debugging mode
  bool checks;	     // extra stack and signal checks (default)
  bool folding;	     // constant folding (default)
  bool use_fastcc;   // fastcc/TCO support (default)
  bool pic;          // create position-independent code (batch compiler)
  bool strip;        // strip unused functions (batch compiler)
  bool restricted;   // restricted mode
  bool ttymode;      // connected to a tty
  bool override;     // override mode
  bool stats;        // stats mode (print execution times)
  bool stats_mem;    // stats mode (print memory usage)
  uint32_t temp;     // temporary level (purgable definitions)
  string ps;         // prompt string
  string libdir;     // library dir to search for source files
  string histfile;   // command history file
  string modname;    // name of output (LLVM) module

  // Additional directories to search for sources and libraries.
  list<string> includedirs, librarydirs;

  // Interpreter state. For internal use only.
  bool qual;	     // whether qualified vars are permitted in binding
  int nerrs;	     // current error count
  string errmsg;     // last reported error (runstr)
  int32_t modno;     // current module key
  int32_t modctr;    // next available module key
  string source;     // the source being parsed
  const char *source_s; // source pointer if input comes from a string
  set<string> sources; // the set of all scripts which have been loaded
  set<string> namespaces; // the set of all declared namespaces
  list<string> loaded_libs; // the list of all loaded libs (lib:...)
  ostream *output;   // redirected output stream for interactive commands
  symtable symtab;   // the symbol table
  pure_expr *result; // last result computed by exec() or parse()
  pure_expr *lastres;// last printed result (interactive mode only)
  exprl last;        // last processed lhs collection
  env globenv;       // global function and variable environment
  env macenv;        // global macro environment
  funset dirty;      // "dirty" function entries which need a recompile
  pure_mem *mem;     // runtime expression memory
  pure_expr *exps;   // head of the free list (available expression nodes)
  pure_expr *tmps;   // temporaries list (to be collected after exceptions)
  size_t freectr;    // size of the free list

  /*************************************************************************
             Stuff below is to be used by application programs.
   *************************************************************************/

  /* Parse and execute the given source file (stdin if empty), or the given
     list of files. If 'check' is true (the default), a full search is
     performed for relative pathnames (checking include directories and
     PURELIB to locate the script file) and the script is only loaded if it
     wasn't included before. If 'sticky' is true (default is false), the
     current module scope is kept, otherwise a new scope is created for the
     loaded module. Using this option isn't recommended, but it is used
     internally by the interactive startup and the 'run' command to make
     namespace and pragma settings of the executed script stick when running
     interactively.

     Returns the last computed expression (if any). (This expression is owned
     by the interpreter and must *not* be freed by the caller.) This is the
     main interface function. If interactive is true, input is read
     interactively from the user, using ps as the prompt string. Please note
     that due to some global data shared by different interpreter instances,
     you can't run two interpreters concurrently right now. (It is possible to
     run them sequentially, though.) */
  pure_expr *run(const string& source, bool check = true,
		 bool sticky = false);
  pure_expr *run(const list<string>& sources, bool check = true,
		 bool sticky = false);

  /* This works like run() above, but takes the source directly from a
     string. No error messages will be printed, instead any errors reported
     during the most recent invokation of this method are available in
     errmsg. parsestr works like runstr, but only parses a simple expression
     and returns it as is. */
  pure_expr *runstr(const string& source);
  pure_expr *parsestr(const string& source);

  /* Evaluate a (compile time) expression and return the (runtime expression)
     result. Returns a null pointer if an exception occurred during the
     evaluation. In such a case, the variant with the extra e parameter
     returns the runtime expression thrown by the exception, if any. The
     'keep' flag indicates whether the code should be output in a batch
     compilation. Both the result and the exception value (if any) are to be
     freed by the caller. */
  pure_expr *eval(expr& x, bool keep);
  pure_expr *eval(expr& x, pure_expr*& e, bool keep);

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
  void defn(const char *varname, pure_expr *x);

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
  void const_defn(const char *varname, pure_expr *x);

  /* Purge the definition of a (global constant, variable or function)
     symbol. If the given symbol is zero, pops the most recent temporary
     definitions level, removing all definitions in that level. */
  void clear(int32_t tag = 0);
  /* Purge the given macro symbol. */
  void clear_mac(int32_t tag);
  /* Purge the rules of the given function or macro symbol at or above the
     given level. */
  void clear_rules(int32_t tag, uint32_t level);
  void clear_mac_rules(int32_t tag, uint32_t level);

  /* Process pending compilations of function definitions. This is also done
     automatically when eval() or defn()/const_defn() is invoked. */
  void compile();

  /* Convert a runtime to a compile time expression. */
  expr pure_expr_to_expr(pure_expr *x);

  /* Errors and warnings. These are for various types of messages from the
     compiler. Default is to write error messages to stdout. You might wish to
     derive from this class and override these to implement custom error
     handling. */
  virtual void error(const yy::location& l, const string& m);
  virtual void error(const string& m);
  virtual void warning(const yy::location& l, const string& m);
  virtual void warning(const string& m);

  /* Check memory usage. The first variation reports the total number of used
     expression nodes, as well as the number of nodes on the freelist. The
     second variation only calculates the total amount (used+free) of
     expression memory being in use. Note that the total may be less than the
     actually allocated size, since the runtime allocates expression memory in
     bigger chunks (128K cells by default). */
  void mem_usage(size_t &used, size_t &free);
  void mem_usage(size_t &total);

  /*************************************************************************
             Stuff below this line is to be used internally only.
   *************************************************************************/

  // Semantic routines used by the parser.

  yy::location* loc;
  env *build_env(rulel *r);
  env *build_env(expr x);
  void build_env(env& vars, expr x);
  void mark_dirty(int32_t f);
  void compile(expr x);
  void using_namespaces(list< pair< string, list<int32_t> > > *items = 0);
  void declare(bool priv, prec_t prec, fix_t fix, list<string> *ids);
  void define(rule *r);
  void define_const(rule *r);
  void exec(expr *x);
  void parse(expr *x);
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
  expr bind(env& vars, veqnl& eqns, expr x, bool b = true, path p = path());
  void checkfuns(expr x);
  void checkvars(expr x, bool b = true);
  expr subst(const env& vars, expr x, uint8_t idx = 0);
  expr fsubst(const env& funs, expr x, uint8_t idx = 0);
  expr bsubst(expr x);
  expr csubst(expr x, bool quote = false);
  expr lcsubst(expr x);
  expr macsubst(expr x, bool quote = false);
  expr varsubst(expr x, uint8_t offs, uint8_t idx = 0);
  expr macred(expr x, expr y, uint8_t idx = 0);
  expr macval(expr x);
  void closure(rule& r, bool b = true);
  expr uminop(expr op, expr x);
  expr *mklsect(expr *x, expr *y);
  expr *mkrsect(expr *x, expr *y);
  expr *mkexpr(expr *x, expr *y);
  expr *mkexpr(expr *x, expr *y, expr *z);
  expr *mksym_expr(string *s, int32_t tag = 0);
  expr *mkas_expr(string *s, expr *x);
  expr *mksimple_expr(OpStack *stk);
  expr parse_simple(list<OpEntry>::iterator& act,
		    list<OpEntry>::iterator end,
		    prec_t min);
  expr *mkcond_expr(expr *x, expr *y, expr *z);
  expr *mkcond1_expr(expr *x, expr *y);
  expr lambda_expr(exprl *args, expr body);
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
#ifdef HAVE_LLVM_MODULEPROVIDER_H
  llvm::ModuleProvider *MP;
#endif
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

  // Helpers for LLVM 2.6 compatibility.
  static const llvm::IntegerType* int1_type()
#ifdef LLVM26
  { return llvm::Type::getInt1Ty(llvm::getGlobalContext()); }
#else
  { return llvm::Type::Int1Ty; }
#endif
  static const llvm::IntegerType* int8_type()
#ifdef LLVM26
  { return llvm::Type::getInt8Ty(llvm::getGlobalContext()); }
#else
  { return llvm::Type::Int8Ty; }
#endif
  static const llvm::IntegerType* int16_type()
#ifdef LLVM26
  { return llvm::Type::getInt16Ty(llvm::getGlobalContext()); }
#else
  { return llvm::Type::Int16Ty; }
#endif
  static const llvm::IntegerType* int32_type()
#ifdef LLVM26
  { return llvm::Type::getInt32Ty(llvm::getGlobalContext()); }
#else
  { return llvm::Type::Int32Ty; }
#endif
  static const llvm::IntegerType* int64_type()
#ifdef LLVM26
  { return llvm::Type::getInt64Ty(llvm::getGlobalContext()); }
#else
  { return llvm::Type::Int64Ty; }
#endif
  static const llvm::Type* float_type()
#ifdef LLVM26
  { return llvm::Type::getFloatTy(llvm::getGlobalContext()); }
#else
  { return llvm::Type::FloatTy; }
#endif
  static const llvm::Type* double_type()
#ifdef LLVM26
  { return llvm::Type::getDoubleTy(llvm::getGlobalContext()); }
#else
  { return llvm::Type::DoubleTy; }
#endif

  static const llvm::Type* void_type()
#ifdef LLVM26
  { return llvm::Type::getVoidTy(llvm::getGlobalContext()); }
#else
  { return llvm::Type::VoidTy; }
#endif
  static llvm::OpaqueType* opaque_type()
#ifdef LLVM26
  { return llvm::OpaqueType::get(llvm::getGlobalContext()); }
#else
  { return llvm::OpaqueType::get(); }
#endif
  static llvm::StructType* struct_type(std::vector<const llvm::Type*>& elts)
#ifdef LLVM26
  { return llvm::StructType::get(llvm::getGlobalContext(), elts); }
#else
  { return llvm::StructType::get(elts); }
#endif

  static llvm::Constant* constant_char_array(const char *s)
#ifdef LLVM26
  { return llvm::ConstantArray::get(llvm::getGlobalContext(), s); }
#else
  { return llvm::ConstantArray::get(s); }
#endif

  static llvm::GlobalVariable* global_variable
  (llvm::Module *M, const llvm::Type *Ty, bool isConstant,
   llvm::GlobalValue::LinkageTypes Linkage,
   llvm::Constant *Init = 0, string Name = "")
#ifdef LLVM26
  { return new llvm::GlobalVariable(*M, Ty, isConstant, Linkage, Init, Name); }
#else
  { return new llvm::GlobalVariable(Ty, isConstant, Linkage, Init, Name, M); }
#endif

  llvm::BasicBlock *basic_block(const char *name, llvm::Function* f = 0)
#ifdef LLVM26
  { return llvm::BasicBlock::Create(llvm::getGlobalContext(), name, f); }
#else
  { return llvm::BasicBlock::Create(name, f); }
#endif

  const llvm::Type *named_type(string name);
  const char *type_name(const llvm::Type *type);
  set<llvm::Function*> always_used;
  map<int32_t,GlobalVar> globalvars;
  map<int32_t,Env> globalfuns;
  pure_aframe *astk;
  pure_expr **__sstk;
  pure_expr **&sstk;
  size_t sstk_cap, sstk_sz;
  llvm::GlobalVariable *sstkvar;
#if DEBUG
  set<pure_expr*> mem_allocations;
#endif
  map<int32_t,ExternInfo> externals;
  pure_aframe *push_aframe(size_t sz);
  void pop_aframe();
  llvm::Function *declare_extern(void *fp, string name, string restype,
				 int n, ...);
  llvm::Function *declare_extern(int priv, string name, string restype,
				 const list<string>& argtypes,
				 bool varargs = false, void *fp = 0,
				 string asname = "");
  void check_used(set<llvm::Function*>& used,
		  map<llvm::GlobalVariable*,llvm::Function*>& varmap);
  int compiler(string out, list<string> libnames);
  list<DebugInfo> debug_info;
  set<int32_t> breakpoints, tmp_breakpoints;
  int32_t stoplevel;
  bool debug_skip;
  string bt;
  llvm::Value *debug_rule(const rule *r);
  llvm::Value *debug_redn(const rule *r, llvm::Value *v = 0);
  void debug_init();
  void backtrace(ostream& out);
private:
  void init();
  void init_llvm_target();
  Env *__fptr;
  Env *&fptr;
  llvm::GlobalVariable *fptrvar;
  llvm::Value *envptr(bool local);
  llvm::Value *constptr(const void *p);
  EnvStack envstk;
  void push(const char *msg, Env *e);
  void pop(Env *e);
  Env *find_stacked(int32_t tag);
  Env& act_env() { assert(!envstk.empty()); return *envstk.front(); }
  Builder& act_builder() { return act_env().builder; }
  bool is_quote(int32_t f)
  { return f == symtab.quote_sym().f || f == symtab.quoteop_sym().f; }
  expr wrap_expr(pure_expr *x);
  pure_expr *const_value(expr x, bool quote = false);
  pure_expr *const_value_invoke(expr x, pure_expr*& e, bool quote = false);
  pure_expr *const_matrix_value(expr x, bool quote = false);
  pure_expr *const_app_value(expr x);
  pure_expr *doeval(expr x, pure_expr*& e, bool keep = false);
  pure_expr *dodefn(env vars, veqnl eqns, expr lhs, expr rhs, pure_expr*& e,
		    bool keep = false);
  llvm::Value *codegen(expr x, bool quote = false);
  void toplevel_codegen(expr x, const rule *rp);
  llvm::Value *builtin_codegen(expr x);
  llvm::Value *matrix_codegen(expr x);
  llvm::Value *list_codegen(expr x, bool quote = false);
  llvm::Value *get_int_check(llvm::Value *u, llvm::BasicBlock *failedbb);
  llvm::Value *get_int(expr x);
  llvm::Value *get_double(expr x);
  llvm::Value *when_codegen(expr x, matcher *m, rulel::const_iterator r,
			    rulel::const_iterator end, rule *rp = 0);
  llvm::Value *funcall(Env *f, llvm::Value *x);
  llvm::Value *funcall(Env *f, uint32_t n, expr x);
  llvm::Value *funcall(int32_t tag, uint8_t idx, uint32_t n, expr x);
  bool logical_tailcall(int32_t tag, uint32_t n, expr x, const rule *rp);
  llvm::Value *logical_funcall(int32_t tag, uint32_t n, expr x);
  llvm::Value *external_funcall(int32_t tag, uint32_t n, expr x);
  llvm::Value *call(llvm::Value *x);
  llvm::Value *call(int32_t f, llvm::Value *x, llvm::Value *y);
  llvm::Value *apply(llvm::Value *x, llvm::Value *y);
  llvm::Value *applc(llvm::Value *x, llvm::Value *y);
  llvm::Value *cond(expr x, expr y, expr z);
  void toplevel_cond(expr x, expr y, expr z, const rule *rp);
  llvm::Value *fbox(Env& f);
  llvm::Value *cbox(int32_t tag);
  llvm::Value *ibox(llvm::Value *i);
  llvm::Value *ibox(int32_t i);
  llvm::Value *zbox(const mpz_t& z);
  llvm::Value *dbox(llvm::Value *d);
  llvm::Value *dbox(double d);
  llvm::Value *sbox(const char *s);
  llvm::Value *pbox(void *p);
  llvm::Value *vref(llvm::Value *x, path p);
  llvm::Value *vref(int32_t tag, path p);
  llvm::Value *vref(int32_t tag, uint32_t offs);
  llvm::Value *vref(int32_t tag, uint8_t idx, path p);
  llvm::Value *fref(int32_t tag, uint8_t idx);
  llvm::Value *fcall(Env& f, vector<llvm::Value*>& args,
		     vector<llvm::Value*>& env);
  llvm::Value *fcall(Env& f, vector<llvm::Value*>& env)
  { vector<llvm::Value*> args; return fcall(f, args, env); }
  llvm::Value *call(string name, bool local, int32_t tag, uint32_t key,
		    llvm::Function* f, llvm::Value *e,
		    uint32_t argc, vector<llvm::Value*>& vars);
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
  void unwind(int32_t tag = 0, bool terminate = true);
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
  void verify_tag(llvm::Value *v, int32_t tag, llvm::BasicBlock *failedbb);
  list<char*> cache;
  const char *mklabel(const char *name, uint32_t i);
  const char *mklabel(const char *name, uint32_t i, uint32_t j);
  const char *mkvarlabel(int32_t tag);
  void clear_cache();
  string make_qualid(const string& id);
  string make_absid(const string& id);

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

  static bool g_init;

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

  // Activation stack for handling indirect calls and exceptions.

  pure_aframe *ap, *abp, *aep, *afreep; // TLD
  list<pure_aframe*> aplist;

  pure_aframe *get_aframe()
  {
    pure_aframe *a;
    if (abp < aep)
      return abp++;
    else if ((a = afreep)) {
      afreep = a->prev; return a;
    } else if ((a = (pure_aframe*)malloc(ASTACKSZ*sizeof(pure_aframe)))) {
      abp = ap = a; aep = ap+ASTACKSZ;
      aplist.push_back(ap);
      return abp++;
    } else
      return 0;
  }

  void free_aframe(pure_aframe *a)
  {
    if (a+1 == abp)
      abp--;
    else {
      a->prev = afreep; afreep = a;
    }
  }

  // Evaluation statistics.

public:
  size_t memctr;
  void begin_stats();
  void end_stats();
  void report_stats();

private:
  clock_t clocks;
  size_t memsize, old_memctr;

  // ctags/etags support.

public:
  string tagsfile, tagsdir;
  int tags;
  string srcabs;
  unsigned int line, column;

  void add_tags(rulel *rl);
  void add_tags(rule *r);
  void add_tags(expr x);
  void add_tags(const string& id, const string& asid);
  void add_tags(list<string> *ids);
  void add_tag(const string& tagname, const string& file,
	       unsigned int line, unsigned int column);
  void print_tags();

private:
  bool tags_init;
  list<string> tag_files;
  map< string, list<TagInfo> > tag_list;
  void init_tags();

  // Namespaces.

public:
  void set_namespace(string *ns);
  void clear_namespace();

  // Stack of scoped namespaces (Pure 0.43+).
  list<nsinfo> active_namespaces; // namespaces currently in scope
  void push_namespace(string *ns);
  void pop_namespace();

  // Closure keys and associated refcounters.

private:
  map<uint32_t,uint32_t*> keys;

public:
  void add_key(uint32_t key, uint32_t *refp)
  {
    keys[key] = refp;
  }
  uint32_t *get_refp(uint32_t key)
  {
    map<uint32_t,uint32_t*>::const_iterator it = keys.find(key);
    if (it == keys.end())
      return 0;
    else
      return it->second;
  }

  // Interface to the lexer.

public:
  bool declare_op;
  string srcdir, xsym_prefix;

private:
  bool lex_begin(const string& fname = "", bool esc = false);
  void lex_end();
};

#endif // ! INTERPRETER_HH
