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

/* Data structures used by the operator precedence parser. */

struct OpEntry {
  bool is_op;
  expr x;
  OpEntry(bool _is_op, expr _x) : is_op(_is_op), x(_x) {}
};

struct OpStack {
  list<OpEntry> stk;
  OpStack *push_op(expr x) { stk.push_back(OpEntry(true, x)); return this; }
  OpStack *push_arg(expr x) { stk.push_back(OpEntry(false, x)); return this; }
  OpStack *push_op(expr *x) { stk.push_back(OpEntry(true, *x)); delete x; return this; }
  OpStack *push_arg(expr *x) { stk.push_back(OpEntry(false, *x)); delete x; return this; }
  void pop() { assert(!stk.empty()); stk.pop_back(); }
  expr *last_op()
  {
    list<OpEntry>::reverse_iterator it = stk.rbegin(), end = stk.rend();
    while (it != end && !it->is_op) ++it;
    if (it == end)
      return 0;
    else
      return &it->x;
  }
};

#endif // ! PARSERDEFS_HH
