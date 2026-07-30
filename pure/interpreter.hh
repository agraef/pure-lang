
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

#ifndef INTERPRETER_HH
#define INTERPRETER_HH

// Undefine PIC macro from config.h to avoid conflict with LLVM's
// PassInstrumentationCallbacks parameter name (PIC)
#ifdef PIC
#undef PIC
#endif

#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>

#include <time.h>
#include <pthread.h>
#include <atomic>
#include <list>
#include <set>
#include <unordered_map>
#include <string>
#include "expr.hh"
#include "matcher.hh"
#include "symtable.hh"
#include "runtime.h"

// LLVM version detection for API compatibility
#include <llvm/Config/llvm-config.h>

// Modern LLVM includes (20.1+)
#include <llvm/IR/Verifier.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>

#include "parserdefs.hh"
// Get rid of silly warnings in bison-generated position.hh.
#pragma GCC diagnostic ignored "-Wparentheses"
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

/* Some of our code currently assumes that these must be either 32 or 64 bit. */
#if SIZEOF_LONG!=4 && SIZEOF_LONG!=8
#error "Unknown size of long type."
#endif
#if SIZEOF_SIZE_T!=4 && SIZEOF_SIZE_T!=8
#error "Unknown size of size_t type."
#endif

/* Permitted characters for --escape mode. */
#ifndef ESCAPECHARS
#define ESCAPECHARS "!$%&*,:<>@\\|"
#endif

/* Texmacs support (http://www.texmacs.org/). */
#define DATA_BEGIN "\002verbatim:"
#define DATA_END "\005"
#define PROMPT_BEGIN "\002verbatim:\002prompt#"
#define PROMPT_END "\005\005"
#define SCHEME_BEGIN "\002scheme:"
#define SCHEME_END "\005"
#define COMMAND_BEGIN "\002command:"
#define COMMAND_END "\005"
#define TEXMACS_BEGIN (interpreter::g_interp->texmacs?DATA_BEGIN:"")
#define TEXMACS_END (interpreter::g_interp->texmacs?DATA_END:"")
#define TEXMACS_BEGIN_PROMPT (interpreter::g_interp->texmacs?PROMPT_BEGIN:"")
#define TEXMACS_END_PROMPT (interpreter::g_interp->texmacs?PROMPT_END:"")
#define TEXMACS_BEGIN_SCHEME (interpreter::g_interp->texmacs?SCHEME_BEGIN:"")
#define TEXMACS_END_SCHEME (interpreter::g_interp->texmacs?SCHEME_END:"")
#define TEXMACS_BEGIN_COMMAND (interpreter::g_interp->texmacs?COMMAND_BEGIN:"")
#define TEXMACS_END_COMMAND (interpreter::g_interp->texmacs?COMMAND_END:"")

using namespace std;

/* The Pure interpreter. */

class interpreter;

// Global LLVM context pointer set by interpreter, used by Env constructors
// before the interpreter class definition is complete.
// The LLVM context of the active interpreter (interpreter::g_interp). Each
// interpreter owns its own context; IR built for one interpreter must use
// that interpreter's context. Defined in interpreter.cc, where
// interpreter is complete (Env's constructors below need it earlier).
llvm::LLVMContext& pure_active_llvm_context();

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

// Modern LLVM 20.1+ uses IRBuilder<> template and user_iterator
using Builder = llvm::IRBuilder<>;
#define value_user_iterator Value::user_iterator
#define value_user_begin(x) x->user_begin()
#define value_user_end(x) x->user_end()

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
  // ty = pointee type for GEP index calculation (required by opaque ptrs)
  llvm::Value *CreateGEP
  (llvm::Type *ty, llvm::Value *x, llvm::Value *i,
   const char* name = "")
  { return builder.CreateGEP(ty, x, i, name); }
  llvm::Value *CreateGEP
  (llvm::Type *ty, llvm::Value *x, llvm::Value *i,
   llvm::Value *j, const char* name = "")
  { llvm::Value* idxs[2] = { i, j };
    return builder.CreateGEP(ty, x,
      llvm::ArrayRef<llvm::Value*>(idxs, 2), name);
  }
  llvm::Value *CreateGEP
  (llvm::Type *ty, llvm::Value *x, llvm::Value *i,
   llvm::Value *j, llvm::Value *k, const char* name = "")
  { llvm::Value* idxs[3] = { i, j, k };
    return builder.CreateGEP(ty, x,
      llvm::ArrayRef<llvm::Value*>(idxs, 3), name); }
  // ty = pointee type for the GEP; load type is inferred from GEP result
  llvm::LoadInst *CreateLoadGEP
  (llvm::Type *ty, llvm::Value *x, llvm::Value *i,
   const char* name = "")
  { auto *gep = llvm::cast<llvm::GetElementPtrInst>(
      CreateGEP(ty, x, i));
    return builder.CreateLoad(gep->getResultElementType(),
      gep, name); }
  llvm::LoadInst *CreateLoadGEP
  (llvm::Type *ty, llvm::Value *x, llvm::Value *i,
   llvm::Value *j, const char* name = "")
  { auto *gep = llvm::cast<llvm::GetElementPtrInst>(
      CreateGEP(ty, x, i, j));
    return builder.CreateLoad(gep->getResultElementType(),
      gep, name); }
  llvm::LoadInst *CreateLoadGEP
  (llvm::Type *ty, llvm::Value *x, llvm::Value *i,
   llvm::Value *j, llvm::Value *k, const char* name = "")
  { auto *gep = llvm::cast<llvm::GetElementPtrInst>(
      CreateGEP(ty, x, i, j, k));
    return builder.CreateLoad(gep->getResultElementType(),
      gep, name); }
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
      builder(pure_active_llvm_context()),
      parent(0), refc(0), refp(new uint32_t)
  { *refp = 0; add_key(getkey(), refp); }
  // environment for an anonymous closure with given body x
  Env(int32_t _tag, const char *_descr, uint32_t _n, expr x,
      bool _b, bool _local = false)
    : tag(_tag), key(0), descr(_descr), n(_n), m(0), f(0), h(0),
      args(n), envs(0), rp(0), b(_b), local(_local),
      builder(pure_active_llvm_context()),
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
      builder(pure_active_llvm_context()),
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
      builder(pure_active_llvm_context()),
      parent(0), refc(0), refp(new uint32_t)
  { *refp = 0; add_key(getkey(), refp); }
  // copy constructor - needed because Env objects are stored by value in maps
  // Must increment refp refcount since we're sharing the refp pointer
  Env(const Env& e);
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
  bool varargs;				// varargs function
  llvm::Type* type;		// return type
  vector<llvm::Type*> argtypes;	// argument types
  llvm::Function *f;			// Pure wrapper for the external
  // Source-level type name strings.  With opaque pointers (LLVM 15+)
  // all pointer LLVM types collapse to `ptr`, so we keep the original
  // names for correct dispatch and serialization.
  string restype_name;
  vector<string> argtype_names;
  ExternInfo()
    : tag(0), varargs(false), type(0), argtypes(0), f(0)
  {}
  ExternInfo(int32_t _tag, const string&_name, llvm::Type *_type,
	     vector<llvm::Type*> _argtypes, llvm::Function *_f,
	     bool _varargs = false)
    : tag(_tag), name(_name), varargs(_varargs),
      type(_type), argtypes(_argtypes), f(_f)
  {}
  ExternInfo(int32_t _tag, const string&_name, llvm::Type *_type,
	     vector<llvm::Type*> _argtypes, llvm::Function *_f,
	     const string& _restype_name,
	     const vector<string>& _argtype_names,
	     bool _varargs = false)
    : tag(_tag), name(_name), varargs(_varargs),
      type(_type), argtypes(_argtypes), f(_f),
      restype_name(_restype_name), argtype_names(_argtype_names)
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

struct rtty_info {
  int argc;
  void *fp;
  rtty_info() : argc(0), fp(0) {}
  rtty_info(uint8_t _argc, void *_fp) : argc(_argc), fp(_fp) {}
};

struct nsinfo {
  // scoped namespaces data
  bool priv; // private/public flag (currently this isn't used by the parser)
  string parent; // parent namespace
  map< string, set<int32_t> > search_namespaces; // saved search namespaces
  nsinfo(const string& ns, const map< string, set<int32_t> >& nss)
    : priv(false), parent(ns), search_namespaces(nss) {}
};

struct bcdata_t {
  map<string,bool> priv; // private flag (per namespace)
  bool dbl; // data representation (Faust dsp only)
  time_t t; // timestamp (Faust dsp only)
  int tag; // type tag (Faust dsp only)
  list<llvm::Function*> funptrs; // external functions (Faust dsp only)
  list<llvm::GlobalVariable*> varptrs; // global variables (Faust dsp only)
  bcdata_t() : dbl(false), t(0), tag(0) {}
  void declare(const string& _ns, bool _priv)
  { priv[_ns] = _priv; }
  bool declared(const string& ns)
  { return priv.find(ns) != priv.end(); }
};

typedef map<string,bcdata_t> bcmap;

typedef map<string,llvm::Type*> type_map;

struct enventry {
  const env* e;
  uint8_t idx;
  enventry(const env* _e, uint8_t _idx) : e(_e), idx(_idx) {}
};
typedef list<enventry> envstack;

struct pointer_type_extra_info {
  bool (*equal_cb)(void*, void*);
  uint32_t (*hash_cb)(void*);
  const char *(*printer_cb)(void*);
  int (*prec_cb)(void*);
  pointer_type_extra_info()
    : equal_cb(0), hash_cb(0), printer_cb(0), prec_cb(0) {}
};

struct errinfo {
  int line1, col1, line2, col2;
  string filename, msg;
  errinfo() :
    line1(0), col1(0), line2(0), col2(0) {}
  errinfo(const string& _msg) :
    line1(0), col1(0), line2(0), col2(0), msg(_msg) {}
  errinfo(const string& _filename, int l1, int c1, int l2, int c2,
	  const string& _msg) :
    line1(l1), col1(c1), line2(l2), col2(c2), filename(_filename), msg(_msg) {}
};

enum cvd_type {
  cvd_none, cvd_void, cvd_char,
  cvd_byte, cvd_short, cvd_int, cvd_int64, cvd_float, cvd_double
};

struct cvector_data {
  pure_expr *x;
  void *v, *w;
  cvd_type ty;
  bool vdata;
  cvector_data(pure_expr *_x, void *_v, cvd_type _ty, bool _vdata = false) :
    x(_x), v(_v), w(0), ty(_ty), vdata(_vdata)
  {
    if (vdata && ty == cvd_char && v) {
      // Back up the original vector, so that we can free the temporary
      // strings afterwards.
      size_t n = 0;
      char **s = (char**)v;
      for (; s[n]; n++) ;
      w = malloc((n+1)*sizeof(char*));
      if (!w) return;
      memcpy(w, v, (n+1)*sizeof(char*));
    }
  }
};

// Per-thread evaluation arena.
//
// Holds the expression-memory block chain, the free list, the
// temporaries list, and the allocation counters -- the state
// new_expr()/free_expr()/pure_new_internal()/pure_free_internal() in
// runtime.cc mutate on every single expression allocation -- plus the
// shadow (GC root) stack and the activation-frame arena used by
// pure_catch/pure_throw and the tail-call trampoline. Before this, all
// of these lived directly on `interpreter`, so two threads running the
// same interpreter's compiled code would race on the free list, the
// shadow stack, and the exception-frame pool, corrupting the heap (this
// is the barrier the design's §5.1 identifies as blocking
// intra-interpreter parallelism). Now each (interpreter, thread) pair
// gets its own pure_ectx, allocated lazily on first use and cached
// thread-locally, so the allocation hot path stays lock-free.
struct pure_ectx {
  pure_mem *mem = 0;    // runtime expression memory (block chain)
  pure_expr *exps = 0;  // head of the free list
  pure_expr *tmps = 0;  // temporaries list (collected after exceptions)
  size_t freectr = 0;   // size of the free list
  size_t memctr = 0;    // low-water free-list mark since last stats reset
  // Shadow (GC root) stack: the JIT-generated code's view of "the
  // current call's argument/environment slots", read via the
  // pure_get_sstk() runtime accessor (interpreter.cc's codegen for
  // vref()/envptr() calls it instead of loading the old process-global
  // $$sstk$$ variable).
  pure_expr **sstk = 0;
  size_t sstk_cap = 0, sstk_sz = 0;
  // Scratch argument-vector buffer for pure_apply()'s saturated-call
  // path (runtime.cc). Built and consumed within a single call, never
  // needs to persist across calls -- it was `static` purely to avoid a
  // stack allocation, which made it a second free-list-style race
  // between threads sharing one interpreter (the same class of bug
  // the per-thread arena fixed for mem/exps/tmps/sstk/astk). PURE_ECTX_MAXARGS
  // must match MAXARGS in funcall.h; runtime.cc static_asserts this
  // (funcall.h's `funcall` macro collides with interpreter::funcall(),
  // an unrelated codegen member, so this header cannot include
  // funcall.h itself).
  #define PURE_ECTX_MAXARGS 64
  pure_expr *apply_argv[PURE_ECTX_MAXARGS];
  // Activation-frame arena for pure_catch/pure_throw and the indirect-
  // call trampoline (see push_aframe/pop_aframe/get_aframe/free_aframe
  // in interpreter.hh/cc). ap/abp/aep bound the current malloc'd block;
  // afreep chains freed frames; aplist tracks every block so they can
  // all be freed when this ectx is torn down. astk is the top of the
  // (logical, prev-linked) activation stack.
  pure_aframe *astk = 0;
  pure_aframe *ap = 0, *abp = 0, *aep = 0, *afreep = 0;
  std::list<pure_aframe*> aplist;
  // Lazily allocate the initial shadow-stack buffer and the first
  // aframe block on first real construction (i.e. the first time a
  // thread touches this interpreter's ectx() -- see interpreter::ectx()
  // in interpreter.cc). Mirrors the eager allocation the old
  // interpreter::init_llvm_target() used to do once per interpreter;
  // now it happens once per (interpreter, thread).
  pure_ectx()
  {
    sstk_cap = 0x10000; // 64K
    sstk = (pure_expr**)malloc(sstk_cap*sizeof(pure_expr*));
    assert(sstk);
    ap = (pure_aframe*)malloc(ASTACKSZ*sizeof(pure_aframe));
    assert(ap); aplist.push_back(ap);
    abp = ap; aep = ap+ASTACKSZ;
  }
  ~pure_ectx()
  {
    pure_mem *m = mem, *n;
    while (m) { n = m->next; delete m; m = n; }
    if (sstk) free(sstk);
    for (std::list<pure_aframe*>::iterator it = aplist.begin();
	 it != aplist.end(); ++it)
      free(*it);
  }
};

class interpreter
{
public:
  int argc; char **argv; // saved command line of the interpreter
  interpreter(int argc, char **argv);
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
  bool compat;       // enable backward compatibility warnings
  bool compat2;      // enable forward compatibility hints
  bool compiling;    // batch compiler mode
  bool eager_jit;    // accepted for CLI compat, not used by ORC JIT
  bool interactive;  // interactive mode
  bool debugging;    // debugging mode
  bool texmacs;      // texmacs mode (http://www.texmacs.org/)
  bool symbolic;     // symbolic mode (--symbolic pragma)
  bool checks;	     // extra stack and signal checks (default)
  bool folding;	     // constant folding (default)
  bool consts;	     // precompute constants at compile time (default)
  bool bigints;	     // use bigints for all integer literals (off by default)
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
  string mainname;   // name of main entry point (batch compilation)

  // Additional directories to search for sources and libraries.
  list<string> includedirs, librarydirs;

  // This is set by the frontend during startup to indicate that the
  // interpreter is running an interactive session.
  bool interactive_mode;
  // Set this to indicate command escape mode (interactive commands are
  // prefixed with the specified char, if it's non-null).
  char escape_mode;

  // User-defined options (conditional compilation pragmas).
private:
  map<string,bool*> codegen_options;
  map<string,bool> source_options;
  set<string> readonly_options;
public:
  int source_level, skip_level;
  bitset<64> else_stack;
  bool is_enabled(const string& optname);
  bool is_defined(const string& optname);
  void enable(const string& optname, bool flag);

  // Interpreter state. For internal use only.
  int32_t last_tag;  // pointer tags
  bool qual;	     // whether qualified vars are permitted in binding
  bool logging;	     // logging of error messages and warnings
  int nerrs;	     // current error count
  string errmsg;     // last reported error (runstr)
  list<errinfo> errpos; // more detailed error information
  int32_t modno;     // current module key
  int32_t modctr;    // next available module key
  string source;     // the source being parsed
  const char *source_s; // source pointer if input comes from a string
  set<string> sources; // the set of all scripts which have been loaded
  set<string> namespaces; // the set of all declared namespaces
  list<string> loaded_libs; // the list of all loaded libs (lib:...)
  bcmap loaded_bcs;  // the set of all loaded bitcode modules (bc:...)
  bcmap loaded_dsps; // the set of all loaded Faust modules (dsp:...)
  map<int,bcmap::iterator> dsp_mods; // reverse mapping of Faust modules
  map<int32_t,rtty_info> rtty; // runtime type tag information
  list<int> required; // required symbols (--required pragma)
  set<int> eager;    // eager compilation symbols (--eager pragma)
  set<int> defined;  // defined symbols (--defined pragma)
  set<int> nodefined; // non-defined symbols (--nodefined pragma)
  set<int> quoteargs; // macros with autoquoted args (--quoteargs pragma)
  ostream *output;   // redirected output stream for interactive commands
  symtable symtab;   // the symbol table
  pure_expr *result; // last result computed by exec() or parse()
  pure_expr *lastres;// last printed result (interactive mode only)
  exprl last;        // last processed lhs collection
  env globenv;       // global function and variable environment
  env macenv;        // global macro environment
  env typeenv;       // global type environment
  funset dirty;      // "dirty" function entries which need a recompile
  funset dirty_types;// "dirty" type entries which need a recompile
  // Per-thread evaluation arena.
  // mem/exps/tmps/freectr/memctr are accessed via ectx(), which returns
  // the calling thread's private pure_ectx for *this* interpreter -- so
  // several threads can run the same interpreter's compiled code at once,
  // each allocating from its own arena with no locking on the hot path.
  // The field names are preserved as ectx() members (not interpreter
  // members), so the ~60 existing runtime.cc call sites only need
  // `interp.mem` rewritten to `interp.ectx().mem` -- a single mechanical,
  // grep-verified substitution, not a semantic change per site.
  //
  // Defined inline, here, rather than out-of-line in interpreter.cc:
  // profiling `fib 30` showed the out-of-line version costing ~13% of
  // total runtime (an unordered_map lookup on every hot-path call, PLUS
  // a real cross-shared-library call at every runtime.cc/printer.cc call
  // site, since interpreter.cc and runtime.cc are separate translation
  // units and the function was not header-inline). The single-slot cache
  // below turns the overwhelmingly common case (same interpreter, same
  // thread, called millions of times in a row -- a thread only
  // time-shares *different* interpreters in the uncommon pd-pure-style
  // embedding pattern) into one inlined pointer comparison; the map is
  // the correctness fallback for an actual interpreter switch on this
  // thread, and for enumeration/purge on interpreter deletion.
  inline pure_ectx& ectx()
  {
    if (id == last_ectx_id) return *last_ectx_ptr;
    pure_ectx& e = ectx_cache[id];
    last_ectx_id = id;
    last_ectx_ptr = &e;
    return e;
  }
  // Drop this thread's cached ectx for this interpreter. Called from
  // ~interpreter() (by the destroying thread) so that if this
  // interpreter's address is reused by a later `new interpreter`, the
  // destroying thread does not resolve the new interpreter's ectx() to
  // the old, now-freed arena. The existing embedding contract already
  // requires pure_delete_interp to be called from the interpreter's own
  // controlling thread (the create-then-run model: pd-pure, pure-lv2,
  // and our own concurrency tests all create/use/delete on one thread),
  // so this is not a new constraint, just documented explicitly.
  inline void purge_ectx()
  {
    if (id == last_ectx_id) { last_ectx_id = 0; last_ectx_ptr = 0; }
    ectx_cache.erase(id);
  }
  map<uint32_t,void*> locals; // interpreter-local storage for applications

  bool defined_sym(int fno) {
    if (symbolic)
      return defined.find(fno) != defined.end();
    else
      return nodefined.find(fno) == nodefined.end();
  }

  bool set_defined_sym(int fno) {
    bool act_defined = defined_sym(fno);
    if (defined.find(fno) == defined.end() &&
	nodefined.find(fno) == nodefined.end()) {
      if (act_defined)
	defined.insert(fno);
      else
	nodefined.insert(fno);
    }
    return act_defined;
  }

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

     The 'priv' flag should only be used for bitcode imports, in which case it
     should be either true or false to indicate whether imported symbols
     should be private.

     Returns the last computed expression (if any). (This expression is owned
     by the interpreter and must *not* be freed by the caller.) This is the
     main interface function. If interactive is true, input is read
     interactively from the user, using ps as the prompt string. Please note
     that due to some global data shared by different interpreter instances,
     you can't run two interpreters concurrently right now. (It is possible to
     run them sequentially, though.) */
  pure_expr *run(int priv, const string& source, bool check = true,
		 bool sticky = false);
  pure_expr *run(int priv, const list<string>& sources, bool check = true,
		 bool sticky = false);
  pure_expr *run(const string& source, bool check = true,
		 bool sticky = false)
  { return run(-1, source, check, sticky); }
  pure_expr *run(const list<string>& sources, bool check = true,
		 bool sticky = false)
  { return run(-1, sources, check, sticky); }

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
  void defn(int32_t tag, pure_expr *x, bool deprecated = false);
  void defn(const char *varname, pure_expr *x, bool deprecated = false);
  set<int32_t> deprecated_vars;

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
  /* Purge the given type symbol. */
  void clear_type(int32_t tag);
  /* Purge the rules of the given function, macro or type symbol at or above
     the given level. */
  void clear_rules(int32_t tag, uint32_t level);
  void clear_mac_rules(int32_t tag, uint32_t level);
  void clear_type_rules(int32_t tag, uint32_t level);

  /* Process pending compilations of function definitions. This is also done
     automatically when eval() or defn()/const_defn() is invoked. */
  void compile();

  /* Force the given functions and all its callees to be JIT-compiled
     immediately. This is also done automatically by compile() whenever a
     function marked eager is recompiled. NOTE: Passing the empty set of
     function symbols JITs all functions. This is *very* slow and not
     recommended. */
  void jit_now(const set<int> fnos = set<int>(), bool recurse = false);
  // Simplified interface which lets you specify either a single function to
  // be JITed, or to JIT everything (fno==-1).
  void jit_now(int32_t fno = -1, bool recurse = false)
  {
    if (fno<0)
      jit_now(set<int>(), recurse);
    else {
      set<int> fnos; fnos.insert(fno);
      jit_now(fnos, recurse);
    }
  }

  /* Convert a runtime to a compile time expression. (The 'check' flag is for
     internal purposes and shouldn't be set by client applications.) */
  expr pure_expr_to_expr(pure_expr *x, bool check = false);

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
  void mark_dirty_type(int32_t f);
  void compile(expr x);
  void using_namespaces(list< pair< string, list<int32_t> > > *items = 0);
  void declare(bool priv, prec_t prec, fix_t fix, list<string> *ids);
  void define(rule *r);
  void define_const(rule *r);
  void exec(expr *x, bool noexec = false);
  void parse(expr *x);
  void clearsym(int32_t f);
  void cleartypesym(int32_t f);
  rulel *default_lhs(exprl &l, rulel *rl);
  void add_rules(rulel &rl, rulel *r, bool b);
  void add_rules(env &e, rulel *r, bool headless = false, bool toplevel = false);
  void add_rule(rulel &rl, rule &r, bool b);
  void add_rule(env &e, rule &r, bool toplevel, bool check);
  void add_rule(env &e, rule &r, bool toplevel = false)
  { add_rule(e, r, toplevel, true); }
  void add_rule_at(env &e, rule &r, int32_t g, rulel::iterator& p);
  void add_type_rules(env &e, rulel *r);
  void add_type_rule(env &e, rule &r, bool check = true);
  void add_type_rule_at(env &e, rule &r, int32_t g, rulel::iterator& p);
  map< int32_t, set<int32_t> > fun_types;
  void add_interface_rule(env &e, int32_t tag, expr& x, bool check = true);
  void add_interface_rule_at(env &e, int32_t tag, expr& x, exprl::iterator& p);
  int add_sub_interface(env &e, int32_t tag, int32_t iface);
  void finalize_interface_rules(env &e, int32_t tag, size_t count,
				exprl::iterator *p = 0);
  rulel *compile_interface(env &e, int32_t tag);
  void add_simple_rule(rulel &rl, rule *r);
  void add_macro_rules(rulel *r);
  void add_macro_rule(rule& r, bool check = true);
  void add_macro_rule_at(rule& r, int32_t g, rulel::iterator& p);
  void promote_ttags(expr f, expr x, expr u);
  void promote_ttags(expr f, expr x, expr u, expr v);
  string ttag_msg(int32_t tag);
  int32_t rectify(int32_t f, bool b);
  expr bind(env& vars, vinfo& vi,
	    expr x, bool b = true, path p = path(), bool a = true);
  void funsubstw(set<int32_t>& warned, bool ty_check,
		 expr x, int32_t f, int32_t g, bool b = false);
  void funsubst(bool ty_check, expr x, int32_t f, int32_t g, bool b = false)
  { set<int32_t> warned; funsubstw(warned, ty_check, x, f, g, b); }
  void checkfuns(bool ty_check, rule *r);
  void checkfuns(expr x, bool b = false);
  void checkvars(expr x, bool b = true);
  expr subst(const env& vars, expr x, uint8_t idx = 0);
  expr fsubst(const env& funs, expr x, uint8_t idx = 0);
  expr bsubst(expr x);
  expr csubst(expr x, bool quote = false);
  expr lcsubst(expr x);
  expr rsubst(expr x, bool quote = false);
  expr ifsubst(expr x);
  expr vsubst(expr x, int offs, int offs1, uint8_t idx = 0);
  expr vsubst(expr x, int offs) { return vsubst(x, offs, offs, 0); }
  expr vsubst(expr x);
  expr macsubst(int32_t h, expr x, bool quote = false)
  { envstack estk; return macsubst(h, false, x, estk, 0, quote); }
  expr macsubst(int32_t h, bool trace, expr x, envstack& estk, uint8_t idx,
		bool quote = false);
  expr varsubst(expr x, uint8_t offs, uint8_t offs1, uint8_t idx = 0);
  expr varsubst(expr x, int offs) { return varsubst(x, offs, 0, 0); }
  expr macred(expr x, expr y, uint8_t idx = 0);
  expr macval(int32_t h, bool trace, expr x, envstack& estk, uint8_t idx);
  expr maceval(int32_t h, bool trace, expr x, envstack& estk, uint8_t idx);
  expr macsval(pure_expr *x);
  bool specials_only;
  expr *macspecial(int32_t h, bool trace, expr x, envstack& estk, uint8_t idx);
  exprl get_macargs(expr x, bool quote);
  expr tagsubst(expr x);
  bool parse_rulel(exprl& xs, rulel& r);
  bool parse_simple_rulel(exprl& xs, rulel& r, int& offs);
  bool parse_env(exprl& xs, env& e);
  bool checkguards(expr x, const vguardl& guards);
  bool checkeqns(expr x, const veqnl& eqns);
  void closure(rule& r, bool b = true);
  expr uminop(expr op, expr x, bool have_uminus);
  expr *mklsect(expr *x, expr *y);
  expr *mkrsect(expr *x, expr *y);
  expr *mkexpr(expr *x, expr *y);
  expr *mkexpr(expr *x, expr *y, expr *z);
  expr *mksym_expr(string *s, int32_t tag = 0);
  expr *mkas_expr(string *s, expr *x);
  expr *mksimple_expr(OpStack *stk);
  int32_t resolve_type_tag(int32_t tag);
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
  expr mkpat_expr(expr x, expr y1, expr y2, expr z, bool& ispat);
  expr gensym_expr(char name);
  expr quoted_ifelse(expr x, expr y, expr z);
  expr quoted_if(expr x, expr y);
  expr quoted_lambda(exprl *args, expr rhs);
  expr quoted_case(expr x, rulel *rules);
  expr quoted_when(expr x, rulel *rules);
  expr quoted_with(expr x, env *defs);
  expr quoted_rules(rulel *rules);
  expr quoted_simple_rules(rulel *rules, int& offs);
  expr quoted_env(env *defs);
  expr quoted_tag(expr x, int32_t astag, int32_t ttag = 0);
  pure_expr *fun_rules(int32_t f);
  pure_expr *type_rules(int32_t f);
  pure_expr *interface_patterns(int32_t f);
  pure_expr *interface_rules(int32_t f);
  pure_expr *mac_rules(int32_t f);
  bool add_fun_rules(pure_expr *x);
  bool add_type_rules(pure_expr *x);
  bool add_interface_rules(int32_t f, pure_expr *x);
  bool add_mac_rules(pure_expr *x);
  bool add_fun_rules_at(pure_expr *y, pure_expr *x);
  bool add_type_rules_at(pure_expr *y, pure_expr *x);
  bool add_interface_rules_at(int32_t f, pure_expr *y, pure_expr *x);
  bool add_mac_rules_at(pure_expr *y, pure_expr *x);
  bool add_var(int32_t sym, pure_expr *x);
  bool add_const(int32_t sym, pure_expr *x);
  bool del_fun_rule(pure_expr *x);
  bool del_type_rule(pure_expr *x);
  bool del_interface_rule(int32_t f, pure_expr *x);
  bool del_mac_rule(pure_expr *x);
  bool del_var(int32_t sym);
  bool del_const(int32_t sym);

  // LLVM code generation and execution.

  // ORC JIT v2 infrastructure
  std::unique_ptr<llvm::orc::LLJIT> JIT;
  std::unique_ptr<llvm::orc::ThreadSafeContext> TSCtx;
  llvm::LLVMContext *Context;  // Raw pointer to context for easy access
  llvm::Module *module;  // Raw pointer owned by ThreadSafeModule

  std::unique_ptr<llvm::PassBuilder> PB;
  std::unique_ptr<llvm::LoopAnalysisManager> LAM;
  std::unique_ptr<llvm::FunctionAnalysisManager> FAM;
  std::unique_ptr<llvm::CGSCCAnalysisManager> CGAM;
  std::unique_ptr<llvm::ModuleAnalysisManager> MAM;
  llvm::orc::ResourceTrackerSP ModuleRT;
  // Track both ResourceTrackers and their associated Env objects
  struct ModuleResources {
    llvm::orc::ResourceTrackerSP RT;
    std::set<Env*> active_envs;
  };
  std::vector<ModuleResources> OldModuleResources;
  std::set<std::string> SubmittedSymbols;
  // Symbols defined by the most recent submit_module() call (the module
  // under ModuleRT); see release_eval_code.
  std::vector<std::string> LastSubmitted;
  std::set<std::string> FreedSymbols;
  std::set<std::string> AbsoluteSymbols;
  std::map<std::string, llvm::orc::ResourceTrackerSP> SymbolTrackers;
  llvm::StructType  *ExprTy, *IntExprTy, *DblExprTy, *StrExprTy, *PtrExprTy;
  llvm::StructType  *ComplexTy, *GSLMatrixTy, *GSLDoubleMatrixTy,
    *GSLComplexMatrixTy, *GSLIntMatrixTy;
  llvm::PointerType *ExprPtrTy, *ExprPtrPtrTy;
  llvm::PointerType *IntExprPtrTy, *DblExprPtrTy, *StrExprPtrTy, *PtrExprPtrTy;
  llvm::PointerType *VoidPtrTy, *CharPtrTy, *IntPtrTy, *DoublePtrTy;
  llvm::PointerType *ComplexPtrTy, *GSLMatrixPtrTy, *GSLDoubleMatrixPtrTy,
    *GSLComplexMatrixPtrTy, *GSLIntMatrixPtrTy;

  // ORC JIT v2 symbol resolution helpers
  void* lookup_symbol(const std::string& name);
  void define_symbol(const std::string& name, void* addr);
  void free_function_code(llvm::Function* f);
  void optimize_function(llvm::Function* f);
  // Erase an LLVM function from the module. Use this instead of
  // Function::eraseFromParent(): the function analysis manager caches
  // analysis results per Function (dominator trees, scalar evolution,
  // MemorySSA, ...) and does not notice when a function is deleted, so
  // erasing without clearing first leaks those results.
  void erase_function(llvm::Function *f);
  // Same for Function::deleteBody(), which leaves the declaration in the
  // module but invalidates every cached analysis of the body.
  void delete_function_body(llvm::Function *f);
  void submit_module();
  void cleanup_old_modules(size_t keep_recent = 10);
  // Free the machine code of a finished pure_eval/doeval (see doeval).
  bool owns_module(llvm::orc::ResourceTrackerSP rt);
  void release_eval_code(llvm::orc::ResourceTrackerSP rt);
  // Symbols defined by the module of a doeval that is still running.
  std::map<llvm::orc::ResourceTracker*, std::vector<std::string>> EvalSymbols;
  bool module_dirty;

  // LLVM type convenience accessors (require Context)
  llvm::IntegerType* int1_type()   { return llvm::Type::getInt1Ty(*Context); }
  llvm::IntegerType* int8_type()   { return llvm::Type::getInt8Ty(*Context); }
  llvm::IntegerType* int16_type()  { return llvm::Type::getInt16Ty(*Context); }
  llvm::IntegerType* int32_type()  { return llvm::Type::getInt32Ty(*Context); }
  llvm::IntegerType* int64_type()  { return llvm::Type::getInt64Ty(*Context); }
  llvm::Type*        float_type()  { return llvm::Type::getFloatTy(*Context); }
  llvm::Type*        double_type() { return llvm::Type::getDoubleTy(*Context); }
  llvm::Type*        void_type()   { return llvm::Type::getVoidTy(*Context); }

  llvm::IntegerType* long_type()
  {
#if SIZEOF_LONG==4
    return int32_type();
#else
    return int64_type();
#endif
  }

  llvm::IntegerType* size_t_type()
  {
#if SIZEOF_SIZE_T==4
    return int32_type();
#else
    return int64_type();
#endif
  }

  // OpaqueType is now StructType in modern LLVM
  llvm::StructType* opaque_type(const char *name)
  { return llvm::StructType::create(*Context, name); }

  // anonymous struct types
  llvm::StructType* struct_type(std::vector<llvm::Type*>& elts)
  {
    llvm::ArrayRef<llvm::Type*> myelts = elts;
    return llvm::StructType::get(*Context, myelts);
  }

  // named struct types
  llvm::StructType* struct_type(const char *name, std::vector<llvm::Type*>& elts)
  {
    llvm::StructType *ty = llvm::StructType::create(*Context, name);
    llvm::ArrayRef<llvm::Type*> myelts = elts;
    ty->setBody(myelts);
    return ty;
  }

  // array types
  llvm::ArrayType* array_type(llvm::Type* ty, size_t num_elts)
  { return llvm::ArrayType::get(ty, num_elts); }

  llvm::FunctionType* func_type(llvm::Type* res, std::vector<llvm::Type*>& args, bool varargs)
  {
    llvm::ArrayRef<llvm::Type*> myargs = args;
    return llvm::FunctionType::get(res, myargs, varargs);
  }

  bool is_pointer_type(llvm::Type *ty)
  { return ty->isPointerTy(); }

  bool is_struct_type(llvm::Type *ty)
  { return ty->isStructTy(); }

  llvm::Constant* constant_char_array(const char *s)
  { return llvm::ConstantDataArray::getString(*Context, s); }

  llvm::GlobalVariable* global_variable
  (llvm::Module *M, llvm::Type *Ty, bool isConstant,
   llvm::GlobalValue::LinkageTypes Linkage,
   llvm::Constant *Init = 0, string Name = "")
  { return new llvm::GlobalVariable(*M, Ty, isConstant, Linkage, Init, Name); }

  llvm::BasicBlock *basic_block(const char *name, llvm::Function* f = 0)
  { return llvm::BasicBlock::Create(*Context, name, f); }

  llvm::PHINode *phi_node(llvm::IRBuilder<> &b, llvm::Type *ty,
			  unsigned n, const char *name = "")
  { return b.CreatePHI(ty, n, name); }

  type_map pointer_types;
  map<llvm::Type*,type_map::iterator> pointer_type_of;
  map<string,int> pointer_tags;
  map<int,map<string,int>::iterator> pointer_type_with_tag;
  map<int,pointer_type_extra_info> pointer_type_info;

  llvm::Type *make_pointer_type(const string& name);
  string pointer_type_name(llvm::Type *type);
  int pointer_type_tag(const string& name);
  int pointer_type_tag(llvm::Type *type)
  {
    assert(is_pointer_type(type));
    return pointer_type_tag(type_name(type));
  }

  llvm::Type *named_type(string name);
  string type_name(llvm::Type *type);
  string bctype_name(llvm::Type *type);
  string dsptype_name(llvm::Type *type);
  bool compatible_types(llvm::Type *type1, llvm::Type *type2);
  llvm::Type *gslmatrix_type(llvm::Type *elem_ty,
				  llvm::Type *block_ty,
				  size_t padding = 0);
  set<llvm::Function*> always_used;
  map<int32_t,GlobalVar> globalvars;
  map<int32_t,Env> globalfuns, globaltypes;
  // astk, sstk, sstk_cap, sstk_sz moved into pure_ectx,
  // the activation-stack top pointer and the
  // shadow (GC root) stack buffer are per-(interpreter,thread) state,
  // reached via ectx(). The JIT no longer binds a process-global
  // $$sstk$$ variable to a fixed interpreter member; instead generated
  // code calls the pure_get_sstk()/pure_set_sstk() runtime accessors
  // (see vref()/envptr() below and pure_get_sstk in runtime.cc), which
  // resolve to the calling thread's own ectx(). sstkvar/fptrvar remain
  // as LLVM Function* handles to those accessor declarations (renamed
  // from "variables" in spirit, kept as members to avoid touching every
  // call site that names them).
  llvm::Function *sstkvar;
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
				 string asname = "", bool dll_check = true);
  void check_used(set<llvm::Function*>& used,
		  map<llvm::GlobalVariable*,llvm::Function*>& varmap);
  int compiler(string out, list<string> libnames, string llcopts = "");
  list<DebugInfo> debug_info;
  set<int32_t> breakpoints, tmp_breakpoints, tracepoints, mac_tracepoints;
  int32_t stoplevel, tracelevel;
  bool debug_skip, trace_skip;
  string bt;
  llvm::Value *debug_rule(const rule *r);
  llvm::Value *debug_redn(const rule *r, llvm::Value *v = 0);
  void debug_init();
  void backtrace(ostream& out);
  bool stopped(int32_t tag)
  {
    if (!interactive)
      return false;
    if (tag>0 && !debug_skip) {
      if (!tmp_breakpoints.empty()) {
	if (tmp_breakpoints.find(tag) !=
	    tmp_breakpoints.end()) {
	  tmp_breakpoints.clear();
	  return true;
	}
      } else if (breakpoints.find(tag) != breakpoints.end())
	return true;
    }
    if (stoplevel < 0 ||
	debug_info.size() <= (uint32_t)stoplevel)
      return true;
    return false;
  }
  bool traced(int32_t tag)
  {
    if (!interactive)
      return false;
    if (tracelevel >= 0 && debug_info.size() > (uint32_t)tracelevel)
      return true;
    if (tag>0 && tracepoints.find(tag) != tracepoints.end()) {
      if (!trace_skip && tracelevel < 0) tracelevel = debug_info.size();
      return true;
    }
    return false;
  }
  bool stopped(Env *e) { return stopped(e->tag); }
  bool traced(Env *e) { return traced(e->tag); }
  // Faust interface.
  bool LoadFaustDSP(bool priv, const char *name, string *msg,
		    const char *modnm = 0);
  // Generic LLVM bitcode interface.
  bool LoadBitcode(bool priv, const char *name, string *msg);
  // Handle inline code.
  void inline_code(bool priv, string &code);
  // Per-interpreter reentrancy lock (formerly the global interpreter
  // lock). Recursive so a thread already holding it can re-enter (e.g. a
  // Pure callback from C that re-invokes the same interpreter). Only
  // serializes access to *this* interpreter; distinct interpreters run
  // concurrently. See pure_lock_interp in runtime.cc.
  pthread_mutex_t lock;
  // Global context switching for interpreters. The sstk half of this
  // dance is gone: sstk now lives in pure_ectx,
  // looked up per-thread via ectx(), so there is no single
  // interpreter-wide slot left to save/restore when switching the
  // active interpreter on one thread. fptr is unaffected (see the
  // comment on fptr's declaration below): it is compile-time-scoped
  // state, still a plain interpreter member.
  inline void save_context()
  {
    __baseptr_save = interpreter::baseptr;
    if (__fptr_save) *__fptr_save = fptr;
  }
  inline void restore_context()
  {
    interpreter::baseptr = __baseptr_save;
    if (__fptr_save) fptr = *__fptr_save;
  }
  void swap_interpreters(interpreter *interp);
private:
  void init();
  void init_llvm_target();
  char *__baseptr_save;
  int nwrapped;
  // fptr: the Env currently being compiled for a one-off, compile-lock-
  // protected evaluation (doeval/dodefn's temporary closure for `eval`,
  // global-variable initializers, etc; see interpreter.cc). Unlike
  // sstk, this is never touched by JIT'd code outside that narrow,
  // single-threaded, immediately-executed-and-discarded window, so it
  // stays a plain interpreter member -- moving it into pure_ectx is not
  // needed for thread-safety.
  Env *__fptr, **__fptr_save;
  Env *&fptr;
  llvm::GlobalVariable *fptrvar;
  llvm::Value *envptr(bool local);
  llvm::Value *constptr(const void *p);
  EnvStack envstk;
  void push(const char *msg, Env *e);
  void pop(Env *e);
  Env *find_stacked(int32_t tag);
  int32_t find_hash(Env *e);
  Env& act_env() { assert(!envstk.empty()); return *envstk.front(); }
  Builder& act_builder() { return act_env().builder; }
  bool is_quote(int32_t f)
  { return f == symtab.quote_sym().f || f == symtab.quoteop_sym().f; }
  bool is_quoteargs(expr x);
  expr wrap_expr(pure_expr *x, bool check = false);
  pure_expr *const_value(expr x, bool quote = false);
  pure_expr *const_value_invoke(expr x, pure_expr*& e, bool quote = false);
  pure_expr *const_matrix_value(expr x, bool quote = false);
  pure_expr *const_app_value(expr x);
  pure_expr *doeval(expr x, pure_expr*& e, bool keep = false);
  pure_expr *dodefn(env vars, const vinfo& vi,
		    expr lhs, expr rhs, pure_expr*& e,
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
			    rulel::const_iterator end,
			    rule *rp = 0, int level = 0);
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
  llvm::Value *fbox(Env& f, bool defer = false);
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
  llvm::Value *fref(int32_t tag, uint8_t idx, bool defer = false);
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
  void fun_body(matcher *pm, matcher *mxs, bool nodefault = false);
  void fun_finish();
  void simple_match(llvm::Value *x, state*& s,
		    llvm::BasicBlock *matchedbb, llvm::BasicBlock *failedbb);
  void complex_match(matcher *pm, matcher *mxs, llvm::BasicBlock *failedbb);
  void complex_match(matcher *pm, const list<llvm::Value*>& xs, state *s,
		     llvm::BasicBlock *failedbb, set<rulem>& reduced,
		     const list<llvm::Value*>& tmps);
  void complex_match(matcher *pm, const list<llvm::Value*>& xs, state *s,
		     llvm::BasicBlock *failedbb, set<rulem>& reduced)
  { list<llvm::Value*> tmps;
    complex_match(pm, xs, s, failedbb, reduced, tmps); }
  void try_rules(matcher *pm, state *s, llvm::BasicBlock *failedbb,
		 set<rulem>& reduced, const list<llvm::Value*>& tmps);
  void try_rules(matcher *pm, state *s, llvm::BasicBlock *failedbb,
		 set<rulem>& reduced)
  { list<llvm::Value*> tmps;
    try_rules(pm, s, failedbb, reduced, tmps); }
  void unwind_iffalse(llvm::Value *v);
  void unwind_iftrue(llvm::Value *v);
  llvm::Value *check_tag(llvm::Value *v, int32_t tag);
  void verify_tag(llvm::Value *v, int32_t tag);
  void verify_tag(llvm::Value *v, int32_t tag, llvm::BasicBlock *failedbb);
  list<char*> cache;
  const char *mklabel(const char *name, uint32_t i);
  const char *mklabel(const char *name, uint32_t i, uint32_t j);
  const char *mklabel(const char *name, uint32_t i, uint32_t j, uint32_t k);
  const char *mkvarlabel(int32_t tag);
  void clear_cache();
  string make_qualid(const string& id);
  string make_absid(const string& id);

public:
  // Global data, saved and restored by the run method.
  static uint8_t g_verbose;
  static bool g_interactive;
  // The active interpreter and the C-stack context are thread-local: each
  // OS thread has its own current interpreter and stack base, so distinct
  // interpreters run concurrently on distinct threads without a global
  // lock (removing the GIL). Within a single interpreter, its own lock
  // still serializes reentrant access.
  static thread_local interpreter* g_interp;
  // not saved
  static thread_local int brkflag, brkmask;
  static thread_local char *baseptr;
  // Genuinely global, read-only after startup.
  static int stackmax;
  static int stackdir;
  // Monotonic, never-reused identity for ectx() lookups (interpreter.cc).
  // A raw `this` pointer is not safe to use as the cache key: once an
  // interpreter is deleted, a later `new interpreter` may reuse its
  // address, and a thread-local cache entry left behind under the old
  // address would then resolve to the wrong (or freed) arena. id is
  // assigned once per instance and never repeats for the life of the
  // process.
  const uint64_t id;

  // Destructors for interpreter-local storage.
  static map<uint32_t, void (*)(void*)> locals_destroy_cb;

  // Some data used in the runtime to keep track of temporary strings and
  // vectors. These must be interpreter-local s.t. they are not overwritten if
  // an interpreter instance gets invoked recursively from another. XXXFIXME:
  // These should actually become TLD when the interpreter goes multithreaded.

  list<char*> temps; // TLD
  list<cvector_data> cvector_temps; // TLD

private:

  static bool g_init;
  static std::atomic<uint64_t> next_id;
  static uint64_t new_id() { return next_id.fetch_add(1, std::memory_order_relaxed); }

  // Per-thread ectx() cache (see the comment on ectx() above). One map
  // per thread -- mutated only by its own thread, so no lock is needed --
  // plus a single-slot fast path in front of it. last_ectx_id==0 means
  // the slot is empty (0 is never a valid `id`: new_id() starts at 1).
  static thread_local std::unordered_map<uint64_t, pure_ectx> ectx_cache;
  static thread_local uint64_t last_ectx_id;
  static thread_local pure_ectx *last_ectx_ptr;

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

  // Activation stack for handling indirect calls and exceptions. Moved
  // into pure_ectx: ap/abp/aep/afreep/aplist are the
  // per-thread aframe arena, get_aframe()/free_aframe() now take the
  // calling thread's ectx() explicitly so they can be called from the
  // free functions in runtime.cc as well as from interpreter methods.

  static pure_aframe *get_aframe(pure_ectx& ectx)
  {
    pure_aframe *a;
    if (ectx.abp < ectx.aep)
      return ectx.abp++;
    else if ((a = ectx.afreep)) {
      ectx.afreep = a->prev; return a;
    } else if ((a = (pure_aframe*)malloc(ASTACKSZ*sizeof(pure_aframe)))) {
      ectx.abp = ectx.ap = a; ectx.aep = ectx.ap+ASTACKSZ;
      ectx.aplist.push_back(ectx.ap);
      return ectx.abp++;
    } else
      return 0;
  }

  static void free_aframe(pure_ectx& ectx, pure_aframe *a)
  {
    if (a+1 == ectx.abp)
      ectx.abp--;
    else {
      a->prev = ectx.afreep; ectx.afreep = a;
    }
  }

  // Evaluation statistics.

public:
  // memctr (low-water free-list mark since the last report) lives in
  // ectx() alongside mem/exps/tmps/freectr -- it's touched on the same
  // allocation hot path (new_expr/free_expr in runtime.cc), so it must
  // be per-thread for the same reason. begin_stats/end_stats read it via
  // ectx() too; see interpreter.cc.
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
  void set_namespace(string *ns, int32_t bracket = 0);
  void clear_namespace(int32_t bracket = 0);

  // Stack of scoped namespaces (Pure 0.43+).
  list<nsinfo> active_namespaces; // namespaces currently in scope
  void push_namespace(string *ns, int32_t bracket = 0);
  void pop_namespace();

  // Closure keys and associated refcounters.

private:
  map<uint32_t,uint32_t*> keys;
  // Reference counts for refp pointers themselves (to fix deliberate leak)
  // Use unordered_map for O(1) average case instead of map's O(log n)
  std::unordered_map<uint32_t*, uint32_t> refp_refcounts;

public:
  void add_key(uint32_t key, uint32_t *refp)
  {
    keys[key] = refp;
    // Increment refcount for this refp pointer
    refp_refcounts[refp]++;
  }
  uint32_t *get_refp(uint32_t key)
  {
    map<uint32_t,uint32_t*>::const_iterator it = keys.find(key);
    if (it == keys.end())
      return 0;
    else
      return it->second;
  }
  void addref_refp(uint32_t *refp)
  {
    if (!refp) return;
    refp_refcounts[refp]++;
  }
  void release_refp(uint32_t *refp)
  {
    if (!refp) return;
    auto it = refp_refcounts.find(refp);
    if (it != refp_refcounts.end()) {
      if (--it->second == 0) {
        refp_refcounts.erase(it);
        // Only delete refp if no closures reference it either
        // *refp counts how many closures are using this refp
        if (*refp == 0) {
          delete refp;
        }
        // If *refp > 0, closures still reference it; they will call
        // try_free_refp() when they decrement *refp to 0.
      }
    }
  }
  // Called from pure_free_clos when *refp reaches 0.  If no Env tracks
  // this refp anymore (refp_refcounts entry already erased), we are the
  // last user and must free the pointer to prevent a leak.
  void try_free_refp(uint32_t *refp)
  {
    if (!refp) return;
    if (refp_refcounts.find(refp) == refp_refcounts.end()) {
      // No Env references this refp anymore -- we're the last user.
      delete refp;
    }
  }

  // Interface to the lexer.

public:
  bool declare_op;
  string srcdir, *xsym_prefix, xcode;
  void begin_code() { xcode.clear(); }
  void add_code(const char *s) { xcode.append(s); }
  void end_code() { }

private:
  bool lex_begin(const string& fname = "", bool esc = false);
  void lex_end();
};

#endif // ! INTERPRETER_HH
