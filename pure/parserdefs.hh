#ifndef PARSERDEFS_HH
#define PARSERDEFS_HH

#include "expr.hh"

class interpreter;

struct sym_info {
  bool special, priv;
  prec_t prec;
  fix_t fix;
  sym_info(bool s, bool v, prec_t p, fix_t f) :
    special(s), priv(v), prec(p), fix(f) { }
};

struct rhs_info {
  expr *r, *q;
  rhs_info(expr *x) { assert(x); r = x; q = 0; }
  rhs_info(expr *x, expr *y) { assert(x); assert(y); r = x; q = y; }
  ~rhs_info() { assert(r); delete r; if (q) delete q; }
  expr rhs() { assert(r); return *r; }
  expr qual() { return q?*q:expr(); }
};

struct rule_info {
  exprl l;
  env e;
};

struct pat_rule_info {
  exprl l;
  rulel rl;
};

typedef pair<expr,expr> comp_clause;
typedef list<comp_clause> comp_clause_list;

#endif // ! PARSERDEFS_HH
