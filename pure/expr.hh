
#ifndef EXPR_HH
#define EXPR_HH

#include <stdint.h>
#include <assert.h>
#include <gmp.h>
#include <bitset>
#include <list>
#include <map>
#include <string>

using namespace std;

/* Pure expression data structure and related stuff. */

/* Symbol precedences and fixity. Standard precedence levels of simple
   expressions are 0..9 (weakest to strongest), inside each level infix
   (non-associative) operators have the lowest (0), postfix operators the
   highest (4) precedence.

   These are reconciled into a single figure, "normalized precedence" nprec =
   10*prec+fixity, which ranges from 0 (weakest infix operator on level 0) to
   94 (strongest postfix operator on level 9).

   This scheme is extended to other syntactic constructions in expressions as
   follows. Primary expressions have nprec = 100, applications 95 (above all
   operators, but below the primaries), lambda -30, case/when/with -20, and
   if-then-else -10 (the latter all bind weaker than any other operator).

   As a special case of primary expressions, a symbol may also have the
   special 'nullary' fixity value, indicating a constant symbol (with nprec =
   100, just like any other primary expression). */

typedef int8_t prec_t;
// Don't change the order of these constants, some code depends on it!
enum fix_t { infix, infixl, infixr, prefix, postfix, nullary };

prec_t nprec(prec_t prec, fix_t fix = infix);

inline prec_t nprec(prec_t prec, fix_t fix)
{
  if (fix == nullary) fix = infix;
  return 10*prec+fix;
}

/* Subexpression paths are used in pattern matching. These are actually
   implemented as bitsets where 0 = left, 1 = right subtree of an
   application. Maximum size of a path is given by the constant MAXDEPTH
   below. */

#define MAXDEPTH 64

class path {
  size_t size;
  bitset<MAXDEPTH> v;
public:
  path() : size(0), v(0) { }
  path(size_t sz) : size(sz), v(0) { assert(size<=MAXDEPTH); }
  path(const path& p) : size(p.size), v(p.v) { }
  path(const path& p, bool n) : size(p.size+1), v(p.v)
  { assert(size<=MAXDEPTH); v[size-1] = n; }
  bool operator== (const path& rhs) const
  { return size == rhs.size && v == rhs.v; }
  bool operator!= (const path& rhs) const
  { return size != rhs.size || v == rhs.v; }
  bool operator[] (size_t i) const
  { assert(i<MAXDEPTH); return v[i]; }
  void set(size_t i, bool n)
  { assert(i<MAXDEPTH); v[i] = n; }
  bool last()  const { assert(size>0); return v[size-1]; }
  size_t len() const { return size; }
};

/* Smart expression pointers (see below for the full definition). These are
   used recursively as components in matrix representations and rule lists in
   the expression data structure. */

class expr;

/* Expression lists and lists of those. These are used to represent
   collections of expressions and generic matrix data in a structured way
   which facilitates code generation. In the case of exprll, each list member
   represents a matrix "row" which is in turn described by a list of
   "columns". */

typedef list<expr> exprl;
typedef list<exprl> exprll;

/* Rule lists are used to encode collections of equations and other rule sets
   in 'case' expressions and the like. See the definition of the rule struct
   at the end of this header. */

struct rule;
typedef list<rule> rulel;

/* Environments provide a map of function and variable symbols to the
   corresponding rule sets (used in 'with' expressions and the global
   environment), subterm paths (used to perform variable binding for 'case'
   and 'when' clauses on the fly) and variable values (used in the global
   environment). See the definition of the env_info struct at the end of this
   header. */

struct env_info;
typedef map<int32_t,env_info> env;

/* The expression data structure. NOTE: EXPR is for internal use, only to be
   used via the smart pointer type expr below! */

struct matcher; // declared in matcher.hh

struct EXPR {

  // special type tags:
  enum {
    VAR		= 0,	// locally bound variable
    FVAR	= -1,   // locally bound function
    APP		= -2,	// function application
    // built-in (C) types:
    INT		= -3,	// 32 bit signed integer
    BIGINT	= -4,	// bigint (mpz_t)
    DBL		= -5,	// double precision floating point number
    STR		= -6,	// utf-8 string (char*)
    PTR		= -7,	// generic pointer (void*)
    // conditionals and binding expressions:
    COND	= -8,	// conditional expression (if-then-else)
    LAMBDA	= -9,	// lambda expression
    CASE	= -10,	// case expression
    WHEN	= -11,	// when expression
    WITH	= -12,	// with expression
    // GSL matrix types:
    MATRIX	= -32,  // generic GSL matrix, symbolic matrices
    DMATRIX	= -31,	// double matrix
    CMATRIX	= -30,	// complex matrix
    IMATRIX	= -29,	// integer matrix
    /* Other values in the range -17..-32 are reserved for later use in the
       runtime expression data structure. Note that all GSL-related tags,
       taken as an unsigned binary quantity, are of the form 0xffffffe0+t,
       where the least significant nibble t=0x0..0xf denotes corresponding
       subtypes in runtime matrix data. For compile time expressions only the
       EXPR::MATRIX tag (t=0) is used. */
  };

  // special flag values used during compilation:
  enum {
    OVF		= 1,	// overflowed int constant -> bigint
    PAREN	= 1<<1,	// parenthesized expression
  };

  uint32_t refc;  // reference counter
  int32_t tag;	  // type tag or nullary symbol

  // data:
  union {
    int32_t i;	  // INT
    mpz_t   z;    // BIGINT
    double  d;	  // DBL
    char   *s;	  // STR
    void   *p;	  // PTR
    struct {	  // VAR, FVAR
      int32_t vtag; // real symbol
      path   *p;    // subterm path (VAR)
      uint8_t idx;  // de Bruin index
    } v;
    EXPR   *x[3]; // APP, LAMBDA, COND
    exprll *xs;   // MATRIX
    struct {	  // CASE, WHEN, WITH
      EXPR  *x;   // expression
      union {
	rulel *r; // rule list (CASE, WHEN)
	env   *e; // function environment (WITH)
      };
    } c;
  } data;

  // matching automaton (LAMBDA, CASE; vector for WHEN):
  matcher *m;

  // compilation flags:
  uint16_t flags;

  // extra built-in type tag used in code generation:
  int8_t ttag;

  // "as" patterns:
  int32_t astag;
  path *aspath;

  EXPR *incref() { refc++; return this; }
  uint32_t decref() { if (refc > 0) --refc; return refc; }
  void del() { if (decref() == 0) delete this; }
  static EXPR *newref(EXPR *x) { return x?x->incref():0; }

  EXPR(int32_t _tag) :
    refc(0), tag(_tag), m(0), flags(0), ttag(0), astag(0), aspath(0) { }
  EXPR(int32_t _tag, int32_t _vtag, uint8_t _idx,
       int8_t _ttag = 0, const path& _p = path()) :
    refc(0), tag(_tag), m(0), flags(0), ttag(_ttag), astag(0), aspath(0)
  { assert(_tag == VAR || _tag == FVAR);
    data.v.vtag = _vtag; data.v.idx = _idx;
    data.v.p = (_tag == VAR)?new path(_p):0; }
  EXPR(int32_t _tag, int32_t _i) :
    refc(0), tag(_tag), m(0), flags(0), ttag(_tag), astag(0), aspath(0)
  { assert(_tag == INT); data.i = _i; }
  EXPR(int32_t _tag, mpz_t _z, bool c = false) :
    refc(0), tag(_tag), m(0), flags(c?OVF:0), ttag(_tag), astag(0), aspath(0)
  { assert(_tag == BIGINT); mpz_init_set(data.z, _z); mpz_clear(_z); }
  EXPR(int32_t _tag, double _d) :
    refc(0), tag(_tag), m(0), flags(0), ttag(_tag), astag(0), aspath(0)
  { assert(_tag == DBL); data.d = _d; }
  explicit EXPR(int32_t _tag, char *_s) :
    refc(0), tag(_tag), m(0), flags(0), ttag(_tag), astag(0), aspath(0)
  { assert(_tag == STR); data.s = _s; }
  explicit EXPR(int32_t _tag, void *_p) :
    refc(0), tag(_tag), m(0), flags(0), ttag(_tag), astag(0), aspath(0)
  { assert(_tag == PTR); data.p = _p; }
  EXPR(int32_t _tag, EXPR *_arg1, EXPR *_arg2, EXPR *_arg3) :
    refc(0), tag(_tag), m(0), flags(0), ttag(0), astag(0), aspath(0)
  { assert(_tag == COND);
    data.x[0] = newref(_arg1); data.x[1] = newref(_arg2);
    data.x[2] = newref(_arg3); }
  EXPR(int32_t _tag, EXPR *_arg, EXPR *_body) :
    refc(0), tag(_tag), m(0), flags(0), ttag(0), astag(0), aspath(0)
  { assert(_tag == LAMBDA);
    data.x[0] = newref(_arg); data.x[1] = newref(_body); }
  EXPR(int32_t _tag, EXPR *_arg, rulel *_rules) :
    refc(0), tag(_tag), m(0), flags(0), ttag(0), astag(0), aspath(0)
  { assert(_tag == CASE || _tag == WHEN);
    data.c.x = newref(_arg); data.c.r = _rules; }
  EXPR(int32_t _tag, EXPR *_arg, env *_e) :
    refc(0), tag(_tag), m(0), flags(0), ttag(0), astag(0), aspath(0)
  { assert(_tag == WITH);
    data.c.x = newref(_arg); data.c.e = _e; }
  EXPR(int32_t _tag, exprll *_args) :
    refc(0), tag(_tag), m(0), flags(0), ttag(0), astag(0), aspath(0)
  { assert(_tag == MATRIX); data.xs = _args; }
  EXPR(EXPR *_fun, EXPR *_arg) :
    refc(0), tag(APP), m(0), flags(0), ttag(0), astag(0), aspath(0)
  { data.x[0] = newref(_fun); data.x[1] = newref(_arg); }
  EXPR(EXPR *_fun, EXPR *_arg1, EXPR *_arg2) :
    refc(0), tag(APP), m(0), flags(0), ttag(0), astag(0), aspath(0)
  { data.x[0] = new EXPR(_fun, _arg1);
    data.x[0]->incref(); data.x[1] = newref(_arg2); }
  EXPR(EXPR *_fun, EXPR *_arg1, EXPR *_arg2, EXPR *_arg3) :
    refc(0), tag(APP), m(0), flags(0), ttag(0), astag(0), aspath(0)
  { data.x[0] = new EXPR(_fun, _arg1, _arg2);
    data.x[0]->incref(); data.x[1] = newref(_arg3); }

  ~EXPR();
};

/* Smart expression pointers. These take care of reference counting
   automagically. */

class expr {
  EXPR* p;
  // debug helper
  void debug(const string &msg);
  // smart pointer machinery
  EXPR* operator-> () const { assert(p); return p; }
  EXPR& operator*  () const { assert(p); return *p; }
  // hash table
  static map<EXPR*,uint32_t> h;
  static uint32_t key;

public:
  expr() : p(0) { }
  expr(EXPR* _p) : p(_p) { if (p) p->incref(); }
  ~expr() { if (p) { if (p->refc == 1) h.erase(p); p->del(); } }
  expr(const expr& x) : p(x.p) { if (p) p->incref(); }
  expr& operator= (const expr& x)
  {
    EXPR* const old = p;
    p = x.p;
    if (p) p->incref();
    if (old) old->del();
    return *this;
  }
  bool operator== (const expr& x) const
  { return p == x.p; }
  bool operator!= (const expr& x) const
  { return p != x.p; }

  // constructors
  expr(int32_t tag) :
    p(new EXPR(tag)) { p->incref(); }
  expr(int32_t tag, int32_t vtag, uint8_t idx,
       int8_t ttag = 0, const path& _p = path()) :
    p(new EXPR(tag, vtag, idx, ttag, _p)) { p->incref(); }
  expr(int32_t tag, int32_t i) :
    p(new EXPR(tag, i)) { p->incref(); }
  expr(int32_t tag, mpz_t z, bool c = false) :
    p(new EXPR(tag, z, c)) { p->incref(); }
  expr(int32_t tag, double d) :
    p(new EXPR(tag, d)) { p->incref(); }
  explicit expr(int32_t tag, char *s) :
    p(new EXPR(tag, s)) { p->incref(); }
  explicit expr(int32_t tag, void *_p) :
    p(new EXPR(tag, _p)) { p->incref(); }
  expr(int32_t tag, expr arg, expr body) :
    p(new EXPR(tag, &*arg, &*body)) { p->incref(); }
  expr(int32_t tag, expr arg1, expr arg2, expr arg3) :
    p(new EXPR(tag, &*arg1, &*arg2, &*arg3)) { p->incref(); }
  expr(int32_t tag, expr arg, rulel *rules) :
    p(new EXPR(tag, &*arg, rules)) { p->incref(); }
  expr(int32_t tag, expr arg, env *e) :
    p(new EXPR(tag, &*arg, e)) { p->incref(); }
  expr(int32_t tag, exprll *args) :
    p(new EXPR(tag, args)) { p->incref(); }
  expr(expr fun, expr arg) :
    p(new EXPR(&*fun, &*arg)) { p->incref(); }
  expr(expr fun, expr arg1, expr arg2) :
    p(new EXPR(&*fun, &*arg1, &*arg2)) { p->incref(); }
  expr(expr fun, expr arg1, expr arg2, expr arg3) :
    p(new EXPR(&*fun, &*arg1, &*arg2, &*arg3)) { p->incref(); }

  // static methods to generate special types of expressions
  static expr lambda(expr x, expr y);
  static expr cond(expr x, expr y, expr z);
  static expr cases(expr x, rulel *rules);
  static expr when(expr x, rulel *rules);
  static expr with(expr x, env *e);
  static expr nil();
  static expr cons(expr x, expr y);
  static expr list(const exprl& xs);
  static expr voidx();
  static expr pair(expr x, expr y);
  static expr tuple(const exprl& xs);

  // generates a unique id for each expr (valid for its lifetime)
  uint32_t hash();

  // access functions
  uint32_t refc()  const { return p->refc; }
  int32_t  tag()   const { return p->tag; }
  int8_t   ttag()  const { return p->ttag; }
  int32_t  vtag()  const { assert(p->tag == EXPR::VAR || p->tag == EXPR::FVAR);
                           return p->data.v.vtag; }
  int32_t  ftag()  const { return (p->tag == EXPR::FVAR)?p->data.v.vtag:
				  p->tag; }
  uint8_t  vidx()  const { assert(p->tag == EXPR::VAR || p->tag == EXPR::FVAR);
                           return p->data.v.idx; }
  path    &vpath() const { assert(p->tag == EXPR::VAR || p->tag == EXPR::FVAR);
                           return *p->data.v.p; }
  int32_t  ival()  const { assert(p->tag == EXPR::INT);
                           return p->data.i; }
  mpz_t   &zval()  const { assert(p->tag == EXPR::BIGINT);
                           return p->data.z; }
  double   dval()  const { assert(p->tag == EXPR::DBL);
                           return p->data.d; }
  char    *sval()  const { assert(p->tag == EXPR::STR);
                           return p->data.s; }
  void    *pval()  const { assert(p->tag == EXPR::PTR);
                           return p->data.p; }
  expr     xval1() const { assert(p->tag == EXPR::APP ||
				  p->tag == EXPR::COND ||
				  p->tag == EXPR::LAMBDA);
                           return expr(p->data.x[0]); }
  expr     xval2() const { assert(p->tag == EXPR::APP ||
				  p->tag == EXPR::COND ||
				  p->tag == EXPR::LAMBDA);
                           return expr(p->data.x[1]); }
  expr     xval3() const { assert(p->tag == EXPR::COND);
                           return expr(p->data.x[2]); }
  expr     xval()  const { assert(p->tag == EXPR::CASE ||
				  p->tag == EXPR::WHEN ||
				  p->tag == EXPR::WITH);
                           return expr(p->data.c.x); }
  exprll  *xvals() const { assert(p->tag == EXPR::MATRIX);
                           return p->data.xs; }
  rulel   *rules() const { assert(p->tag == EXPR::CASE ||
				  p->tag == EXPR::WHEN);
                           return p->data.c.r; }
  env     *fenv()  const { assert(p->tag == EXPR::WITH);
                           return p->data.c.e; }
  matcher *&pm()   const { assert(p->tag == EXPR::LAMBDA ||
				  p->tag == EXPR::CASE ||
				  p->tag == EXPR::WHEN);
                           return p->m; }
  uint16_t&flags() const { return p->flags; }
  int32_t  astag() const { return p->astag; }
  path   &aspath() const { assert(p->aspath); return *p->aspath; }

  void set_ttag(int8_t tag) { p->ttag = tag; }
  void set_astag(int32_t tag) { p->astag = tag; }
  void set_aspath(const path& _p)
  { if (p->aspath) delete p->aspath; p->aspath = new path(_p); }

  bool is_null()   const { return p==0; }
  bool is_fun()    const { return p->tag > 0; }
  bool is_var()    const { return p->tag == EXPR::VAR; }
  bool is_fvar()   const { return p->tag == EXPR::FVAR; }
  bool is_int()    const { return p->tag == EXPR::INT; }
  bool is_dbl()    const { return p->tag == EXPR::DBL; }
  bool is_ptr()    const { return p->tag == EXPR::PTR; }
  bool is_app()    const { return p->tag == EXPR::APP; }
  bool is_lambda() const { return p->tag == EXPR::LAMBDA; }
  bool is_cond()   const { return p->tag == EXPR::COND; }
  bool is_case()   const { return p->tag == EXPR::CASE; }
  bool is_when()   const { return p->tag == EXPR::WHEN; }
  bool is_with()   const { return p->tag == EXPR::WITH; }

  bool is_fun(int32_t& f) const
  { if (p->tag > 0) {
      f = p->tag;
      return true;
    } else
      return false;
  }
  bool is_var(int32_t& v, uint8_t& idx, int8_t& ttag, path& _p) const
  { if (p->tag == EXPR::VAR) {
      v = p->data.v.vtag;
      idx = p->data.v.idx;
      ttag = p->ttag;
      _p = *p->data.v.p;
      return true;
    } else
      return false;
  }
  bool is_fvar(int32_t& v, uint8_t& idx) const
  { if (p->tag == EXPR::FVAR) {
      v = p->data.v.vtag;
      idx = p->data.v.idx;
      return true;
    } else
      return false;
  }
  bool is_int(int32_t &i) const
  { if (p->tag == EXPR::INT) {
      i = p->data.i;
      return true;
    } else
      return false;
  }
  bool is_dbl(double &d) const
  { if (p->tag == EXPR::DBL) {
      d = p->data.d;
      return true;
    } else
      return false;
  }
  bool is_ptr(void *&_p) const
  { if (p->tag == EXPR::PTR) {
      _p = p->data.p;
      return true;
    } else
      return false;
  }
  bool is_app(expr &x, expr &y) const
  { if (p->tag == EXPR::APP) {
      x = expr(p->data.x[0]);
      y = expr(p->data.x[1]);
      return true;
    } else
      return false;
  }
  bool is_lambda(expr &x, expr &y, matcher *&m) const
  { if (p->tag == EXPR::LAMBDA) {
      x = expr(p->data.x[0]);
      y = expr(p->data.x[1]);
      m = p->m;
      return true;
    } else
      return false;
  }
  bool is_cond(expr &x, expr &y, expr &z) const
  { if (p->tag == EXPR::COND) {
      x = expr(p->data.x[0]);
      y = expr(p->data.x[1]);
      z = expr(p->data.x[2]);
      return true;
    } else
      return false;
  }
  bool is_case(expr &x, rulel *&r, matcher *&m) const
  { if (p->tag == EXPR::CASE) {
      x = expr(p->data.c.x);
      r = p->data.c.r;
      m = p->m;
      return true;
    } else
      return false;
  }
  bool is_when(expr &x, rulel *&r, matcher *&m) const
  { if (p->tag == EXPR::WHEN) {
      x = expr(p->data.c.x);
      r = p->data.c.r;
      m = p->m;
      return true;
    } else
      return false;
  }
  bool is_with(expr &x, env *&e) const
  { if (p->tag == EXPR::WITH) {
      x = expr(p->data.c.x);
      e = p->data.c.e;
      return true;
    } else
      return false;
  }
  bool is_nil()    const;
  bool is_cons()   const;
  bool is_list()   const;
  bool is_voidx()  const;
  bool is_pair()   const;
  // This is always true, as we consider a singleton as a tuple, too. Use
  // is_pair() to test for a "real" tuple instead.
  bool is_tuple()  const { return true; }
  // Check for proper (normalized) tuples.
  bool is_tuplex() const;
  bool is_cons(expr &x, expr &y) const;
  bool is_list(exprl &xs) const;
  bool is_pair(expr &x, expr &y) const;
  // Always true (see note above). Use is_pair() && istuple(xs) to test for a
  // "real" tuple instead.
  bool is_tuple(exprl &xs) const;
  // Check for proper (normalized) tuples.
  bool is_tuplex(exprl &xs) const;
  // Special check for tuples used in list construction.
  bool is_tuplel(exprl &xs) const;
};

/* Rules of the form: lhs -> rhs [if qual]. */

struct rule {
  expr lhs, rhs, qual;
  uint8_t temp;
  rule(expr l, expr r, expr q = expr(), uint8_t t = 0)
    : lhs(l), rhs(r), qual(q), temp(t) { }
};

/* Environment entries. */

struct env_info {
  enum { none, lvar, cvar, fvar, fun } t;
  uint8_t temp;
  union {
    // local variable binding (lvar):
    struct {
      int8_t ttag;
      path *p;
    };
    // constant definition (cvar):
    expr *cval;
    // free variable definition (fvar):
    void *val; // pointer to memory location holding a runtime expression
    // function definition (fun):
    struct {
      uint32_t argc;
      rulel *rules;
      matcher *m;
    };
  };
  env_info() : t(none) { }
  env_info(int8_t _ttag, path _p, uint8_t _temp = 0)
    : t(lvar), temp(_temp), ttag(_ttag), p(new path(_p)) { }
  env_info(expr x, uint8_t _temp = 0)
    : t(cvar), temp(_temp), cval(new expr) { *cval = x; }
  env_info(void *v, uint8_t _temp = 0)
    : t(fvar), temp(_temp), val(v) { }
  env_info(uint32_t c, rulel r, uint8_t _temp = 0)
    : t(fun), temp(_temp), argc(c), rules(new rulel(r)), m(0) { }
  env_info(const env_info& e);
  env_info& operator= (const env_info& e);
  ~env_info();
};

#endif // ! EXPR_HH
