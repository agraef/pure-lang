
#include "expr.hh"
#include "interpreter.hh"
#include "matcher.hh"

EXPR::~EXPR()
{
  switch (tag) {
  case BIGINT:
    mpz_clear(data.z);
    break;
  case STR:
    if (data.s) free(data.s);
    break;
  case VAR:
    if (data.v.p) delete data.v.p;
    break;
  case APP:
    if (data.x[0]) data.x[0]->del();
    if (data.x[1]) data.x[1]->del();
    break;
  case LAMBDA:
    if (data.x[0]) data.x[0]->del();
    if (data.x[1]) data.x[1]->del();
    if (m) delete m;
    break;
  case COND:
    if (data.x[0]) data.x[0]->del();
    if (data.x[1]) data.x[1]->del();
    if (data.x[2]) data.x[2]->del();
    break;
  case MATRIX:
    if (data.xs) delete data.xs;
    break;
  case CASE:
    if (data.c.x) data.c.x->del();
    if (data.c.r) delete data.c.r;
    if (m) delete m;
    break;
  case WHEN:
    if (data.c.x) data.c.x->del();
    if (data.c.r) delete data.c.r;
    if (m) delete[] m;
    break;
  case WITH:
    if (data.c.x) data.c.x->del();
    if (data.c.e) delete data.c.e;
    break;
  }
  if (aspath) delete aspath;
}

map<EXPR*,uint32_t> expr::h;
uint32_t expr::key = 0;

uint32_t expr::hash()
{
  assert(p);
  map<EXPR*,uint32_t>::iterator i = h.find(p);
  if (i == h.end()) {
    uint32_t k = h[p] = key++;
#if DEBUG>1
    cout << "new hash " << k << " = " << p << " " << *this << endl;
#endif
    assert(key > 0 && "expr hash table overflow");
    return k;
  } else {
#if DEBUG>1
    cout << "hash " << i->second << " = " << p << " " << *this << endl;
#endif
    return i->second;
  }
}

void expr::debug(const string &msg)
{
  if (p) cout << msg << ": " << *this << endl;
}

expr expr::lambda(expr arg, expr body)
{
  return expr(EXPR::LAMBDA, arg, body);
}

expr expr::cond(expr x, expr y, expr z)
{
  return expr(EXPR::COND, x, y, z);
}

expr expr::cases(expr x, rulel *rules)
{
  assert(!rules->empty());
  return expr(EXPR::CASE, x, rules);
}

expr expr::when(expr x, rulel *rules)
{
  assert(!rules->empty());
  return expr(EXPR::WHEN, x, rules);
}

expr expr::with(expr x, env *e)
{
  assert(!e->empty());
  return expr(EXPR::WITH, x, e);
}

expr expr::nil()
{
  return interpreter::g_interp->symtab.nil_sym().x;
}

expr expr::cons(expr x, expr y)
{
  return expr(interpreter::g_interp->symtab.cons_sym().x, x, y);
}

static expr list_expr(exprl::const_iterator it, exprl::const_iterator end)
{
  if (it == end)
    return expr::nil();
  else {
    expr x = *it;
    return expr::cons(x, list_expr(++it, end));
  }
}

expr expr::list(const exprl& xs)
{
  return list_expr(xs.begin(), xs.end());
}

expr expr::voidx()
{
  return interpreter::g_interp->symtab.void_sym().x;
}

expr expr::pair(expr x, expr y)
{
  return expr(interpreter::g_interp->symtab.pair_sym().x, x, y);
}

static expr tuple_expr(exprl::const_iterator it, exprl::const_iterator end)
{
  expr x = *it;
  if (++it == end)
    return x;
  else
    return expr::pair(x, tuple_expr(it, end));
}

expr expr::tuple(const exprl& xs)
{
  if (xs.empty())
    return expr::voidx();
  else
    return tuple_expr(xs.begin(), xs.end());
}

bool expr::is_nil() const
{
  return tag() == interpreter::g_interp->symtab.nil_sym().f;
}

bool expr::is_cons() const
{
  expr x, y, u, v;
  return is_app(x, y) && x.is_app(u, v) &&
    u.tag() == interpreter::g_interp->symtab.cons_sym().f;
}

bool expr::is_list() const
{
  expr x, y;
  if (is_cons(x, y))
    return y.is_list();
  else
    return is_nil();
}

bool expr::is_voidx() const
{
  return tag() == interpreter::g_interp->symtab.void_sym().f;
}

bool expr::is_pair() const
{
  expr x, y, u, v;
  return is_app(x, y) && x.is_app(u, v) &&
    u.tag() == interpreter::g_interp->symtab.pair_sym().f;
}

bool expr::is_tuplex() const
{
  expr x, y;
  if (is_pair(x, y))
    return !x.is_pair() && y.is_tuplex();
  else
    return true;
}

bool expr::is_cons(expr &x, expr &y) const
{
  expr u, v;
  return is_app(u, y) && u.is_app(v, x) &&
    v.tag() == interpreter::g_interp->symtab.cons_sym().f;
}

bool expr::is_list(exprl &xs) const
{
  expr x, y;
  if (is_cons(x, y)) {
    xs.push_back(x);
    return y.is_list(xs);
  } else if (is_nil())
    return true;
  else {
    xs.clear();
    return false;
  }
}

bool expr::is_pair(expr &x, expr &y) const
{
  expr u, v;
  return is_app(u, y) && u.is_app(v, x) &&
    v.tag() == interpreter::g_interp->symtab.pair_sym().f;
}

bool expr::is_tuple(exprl &xs) const
{
  expr x, y;
  if (is_pair(x, y))
    return x.is_tuple(xs) && y.is_tuple(xs);
  else {
    xs.push_back(*this);
    return true;
  }
}

bool expr::is_tuplex(exprl &xs) const
{
  expr x, y;
  if (is_pair(x, y))
    if (x.is_pair()) {
      xs.clear();
      return false;
    } else {
      xs.push_back(x);
      return y.is_tuplex(xs);
    }
  else {
    xs.push_back(*this);
    return true;
  }
}

bool expr::is_tuplel(exprl &xs) const
{
  expr x, y;
  if (is_pair(x, y) && !(flags()&EXPR::PAREN))
    return x.is_tuplel(xs) && y.is_tuplel(xs);
  else {
    xs.push_back(*this);
    return true;
  }
}

env_info::env_info(const env_info& e) : t(e.t), temp(e.temp) {
  switch (t) {
  case none:
    break;
  case lvar:
    ttag = e.ttag;
    p = new path(*e.p);
    break;
  case cvar:
    cval = new expr;
    *cval = *e.cval;
    break;
  case fvar:
    val = e.val;
    break;
  case fun:
    argc = e.argc;
    rules = new rulel(*e.rules);
    if (e.m)
      m = new matcher(*rules, argc+1);
    else
      m = 0;
    break;
  }
}

env_info& env_info::operator= (const env_info& e)
{
  switch (t) {
  case none:
    break;
  case lvar:
    delete p;
    break;
  case cvar:
    delete cval;
    break;
  case fvar:
    break;
  case fun:
    delete rules;
    if (m) delete m;
    break;
  }
  if ((t != cvar && t != fvar) || temp > e.temp)
    temp = e.temp;
  t = e.t;
  switch (t) {
  case none:
    break;
  case lvar:
    ttag = e.ttag;
    p = new path(*e.p);
    break;
  case cvar:
    cval = new expr;
    *cval = *e.cval;
    break;
  case fvar:
    val = e.val;
    break;
  case fun:
    argc = e.argc;
    rules = new rulel(*e.rules);
    if (e.m)
      m = new matcher(*rules, argc+1);
    else
      m = 0;
    break;
  }
  return *this;
}

env_info::~env_info() {
  switch (t) {
  case none:
    break;
  case lvar:
    delete p;
    break;
  case cvar:
    delete cval;
    break;
  case fvar:
    break;
  case fun:
    delete rules;
    if (m) delete m;
    break;
  }
}
