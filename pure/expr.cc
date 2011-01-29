
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

#include "expr.hh"
#include "interpreter.hh"
#include "matcher.hh"

EXPR::~EXPR()
{
#if 1
  /* Horrible kludge to avoid stack overflows while recursing into deep
     expressions. If we run into this, we simply bail out. Thus the compiler
     will continue to work but we'll leak memory (possibly copious amounts).
     To avoid this, this destructor should really be implemented
     non-recursively, but in practice this condition will only arise if the
     Pure program uses huge constant expressions. */
  char test;
  if (interpreter::stackmax > 0 &&
      interpreter::stackdir*(&test - interpreter::baseptr) >=
      interpreter::stackmax)
    // nesting too deep -- give up
    return;
#endif
  switch (tag) {
  case BIGINT:
    mpz_clear(data.z);
    break;
  case STR:
    if (data.s) free(data.s);
    break;
  case WRAP:
    if (data.p) {
      GlobalVar *v = (GlobalVar*)data.p;
      interpreter::g_interp->JIT->updateGlobalMapping(v->v, 0);
      v->v->eraseFromParent();
      pure_free(v->x);
      delete v;
    }
    break;
  case VAR:
    if (data.v.p) delete data.v.p;
    break;
  case APP: {
    if (data.x[0]) data.x[0]->del();
    EXPR *x = data.x[1];
#if 1
    /* We handle this case in a partially iterative fashion, to prevent stack
       overflows on deep right-recursive structures such as lists. */
    while (x && x->tag == APP && x->refc == 1) {
      EXPR *y = x->data.x[1]; x->data.x[1] = 0;
      assert(x->decref() == 0);
      delete x;
      x = y;
    }
#endif
    if (x) x->del();
    break;
  }
  case COND1:
    if (data.x[0]) data.x[0]->del();
    if (data.x[1]) data.x[1]->del();
    break;
  case COND:
    if (data.x[0]) data.x[0]->del();
    if (data.x[1]) data.x[1]->del();
    if (data.x[2]) data.x[2]->del();
    break;
  case LAMBDA:
    if (data.l.xs) delete data.l.xs;
    if (data.l.r) delete data.l.r;
    /* We're deliberately leaking memory in debugging mode here, since the
       rule pointer of the lambda might still be needed by the debugger. */
    if (m && !interpreter::g_interp->debugging) delete m;
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

expr expr::lambda(exprl *args, expr body, vguardl guards, veqnl eqns)
{
  assert(!args->empty());
  // create a fake lhs
  expr lhs(interpreter::g_interp->symtab.anon_sym);
  for (exprl::const_iterator it = args->begin(), end = args->end();
       it != end; ++it)
    lhs = expr(lhs, *it);
  return expr(EXPR::LAMBDA, args, new rule(lhs, body, guards, eqns));
}

expr expr::cond(expr x, expr y, expr z)
{
  return expr(EXPR::COND, x, y, z);
}

expr expr::cond1(expr x, expr y)
{
  return expr(EXPR::COND1, x, y);
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

bool expr::is_guarded() const
{
  EXPR *x = p;
  while (x->tag == EXPR::WITH || x->tag == EXPR::WHEN)
    x = x->data.c.x;
  return x->tag == EXPR::COND1;
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
  /* Implemented iteratively, to avoid stack overflows. */
  expr x = *this, y, z;
  while (x.is_cons(y, z))
    x = z;
  return x.is_nil();
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

bool expr::is_cons(expr &x, expr &y) const
{
  expr u, v;
  return is_app(u, y) && u.is_app(v, x) &&
    v.tag() == interpreter::g_interp->symtab.cons_sym().f;
}

bool expr::is_list(exprl &xs) const
{
  /* Implemented iteratively, to avoid stack overflows. */
  expr x = *this, y, z;
  while (x.is_cons(y, z)) {
    xs.push_back(y);
    x = z;
  }
  if (x.is_nil())
    return true;
  else {
    xs.clear();
    return false;
  }
}

bool expr::is_list2(exprl &xs, expr& tl) const
{
  /* Implemented iteratively, to avoid stack overflows. */
  expr x = *this, y, z;
  while (x.is_cons(y, z)) {
    xs.push_back(y);
    x = z;
  }
  if (xs.empty())
    return false;
  else {
    tl = x;
    return true;
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
  if (is_pair()) {
    expr x = *this, y, z;
    while (x.is_pair(y, z)) {
      xs.push_back(y);
      x = z;
    }
    xs.push_back(x);
    return true;
  } else
    return false;
}

bool expr::is_tuplel(exprl &xs) const
{
  expr x = *this, y, z;
  while (x.is_pair(y, z) && !(x.flags()&EXPR::PAREN)) {
    (void)y.is_tuplel(xs); // always true
    x = z;
  }
  xs.push_back(x);
  return true;
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
    cval_var = e.cval_var;
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
    cval_var = e.cval_var;
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
