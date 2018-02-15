
/* Copyright (c) 2008-2012 by Albert Graef <Dr.Graef@t-online.de>.

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

#include "matcher.hh"
#include <set>

trans::trans(int32_t _tag, int8_t _ttag)
  : tag(_tag), st(new state), ttag(_ttag)
{
  //assert(_tag == EXPR::VAR || _tag == EXPR::APP || _tag > 0);
}

trans::trans(int32_t _tag, int32_t _i)
  : tag(_tag), i(_i), st(new state), ttag(_tag)
{
  assert(_tag == EXPR::INT);
}

trans::trans(int32_t _tag, const mpz_t& _z)
  : tag(_tag), st(new state), ttag(_tag)
{
  assert(_tag == EXPR::BIGINT);
  mpz_init_set(z, _z);
}

trans::trans(int32_t _tag, double _d)
  : tag(_tag), d(_d), st(new state), ttag(_tag)
{
  assert(_tag == EXPR::DBL);
}

trans::trans(int32_t _tag, const char *_s)
  : tag(_tag), s(_s), st(new state), ttag(_tag)
{
  assert(_tag == EXPR::STR);
}

trans::trans(int32_t _tag, size_t _n, size_t _m)
  : tag(_tag), n(_n), m(_m), st(new state), ttag(_tag)
{
  assert(_tag == EXPR::MATRIX);
}

trans::trans(const trans& tr)
  : tag(tr.tag), st(new state(*tr.st)), ttag(tr.ttag)
{
  switch (tag) {
  case EXPR::INT:
    i = tr.i;
    break;
  case EXPR::BIGINT:
    mpz_init_set(z, tr.z);
    break;
  case EXPR::DBL:
    d = tr.d;
    break;
  case EXPR::STR:
    s = tr.s;
    break;
  case EXPR::MATRIX:
    n = tr.n; m = tr.m;
    break;
  default:
    break;
  }
}

trans& trans::operator = (const trans& tr)
{
  tag = tr.tag; ttag = tr.tag;
  if (st) delete st; st = new state(*tr.st);
  switch (tag) {
  case EXPR::INT:
    i = tr.i;
    break;
  case EXPR::BIGINT:
    mpz_init_set(z, tr.z);
    break;
  case EXPR::DBL:
    d = tr.d;
    break;
  case EXPR::STR:
    s = tr.s;
    break;
  case EXPR::MATRIX:
    n = tr.n; m = tr.m;
    break;
  default:
    break;
  }
  return *this;
}

trans::~trans()
{
  if (tag == EXPR::BIGINT) mpz_clear(z);
  delete st;
}

/* TA matching algorithm. */

// This is needed to get access to the GlobalVar struct.
#include "interpreter.hh"

static inline int tagcmp(int32_t tag1, int32_t tag2)
{
  int s1 = tag1<=0?-1:1, s2 = tag2<=0?-1:1;
  if (s1 != s2)
    return s1 - s2;
  else
    return s1*tag1 - s1*tag2;
}

state *matcher::match(state *st, expr x)
{
  if (x.tag() == EXPR::WRAP) {
    // Wrapped runtime expression, defer to the corresponding matcher.
    assert(x.pval());
    GlobalVar *v = (GlobalVar*)x.pval();
    return match(st, v->x);
  }
  // look for a matching transition
  transl::const_iterator t;
  for (t = st->tr.begin(); t != st->tr.end(); t++)
    if (t->tag == x.tag()) {
      switch (x.tag()) {
      case EXPR::INT:
	if (x.ival() == t->i) return t->st;
	break;
      case EXPR::BIGINT:
	if (mpz_cmp(x.zval(), t->z) == 0) return t->st;
	break;
      case EXPR::DBL:
	if (x.dval() == t->d) return t->st;
	break;
      case EXPR::STR:
	if (strcmp(x.sval(), t->s) == 0) return t->st;
	break;
      case EXPR::MATRIX: {
	exprll *xs = x.xvals();
	if (xs->size() != t->n) break;
	bool ok = true;
	for (exprll::const_iterator it = xs->begin(), end = xs->end();
	     it != end && ok; ++it) {
	  ok = it->size() == t->m;
	}
	if (!ok) break;
	state *next = t->st;
	for (exprll::iterator it = xs->begin(), end = xs->end();
	     it != end && next; it++) {
	  for (exprl::iterator jt = it->begin(), end = it->end();
	       jt != end && next; jt++) {
	    next = match(next, *jt);
	  }
	}
	return next;
      }
      case EXPR::APP: {
	state *next = match(t->st, x.xval1());
	return next?match(next, x.xval2()):0;
      }
      default:
	return t->st;
      }
    } else if (tagcmp(t->tag, x.tag()) > 0)
      break;
  // no literal match, check for a matching qualified variable transition
  if (x.tag() < EXPR::APP) {
    for (t = st->tr.begin(); t != st->tr.end() && t->tag == EXPR::VAR; t++) {
      if (t->ttag == 0)
	continue;
      else if (t->ttag == x.tag())
	return t->st;
      else if (t->ttag < x.tag())
	break;
    }
  }
  // still no match, use default transition if present
  if ((t = st->tr.begin()) != st->tr.end() &&
      t->tag == EXPR::VAR && t->ttag == 0)
    return t->st;
  return 0;
}

state *matcher::match(state *st, const exprl& x)
{
  for (exprl::const_iterator it = x.begin(), end = x.end();
       it != end && st; it++)
    st = match(st, *it);
  return st;
}

#include "gsl_structs.h"

state *matcher::match(state *st, pure_expr *x)
{
  // look for a matching transition
  transl::const_iterator t;
  for (t = st->tr.begin(); t != st->tr.end(); t++)
    if (t->tag == x->tag ||
	(t->tag == EXPR::MATRIX &&
	 x->tag >= EXPR::DMATRIX && x->tag <= EXPR::IMATRIX)) {
      switch (x->tag) {
      case EXPR::INT:
	if (x->data.i == t->i) return t->st;
	break;
      case EXPR::BIGINT:
	if (mpz_cmp(x->data.z, t->z) == 0) return t->st;
	break;
      case EXPR::DBL:
	if (x->data.d == t->d) return t->st;
	break;
      case EXPR::STR:
	if (strcmp(x->data.s, t->s) == 0) return t->st;
	break;
      case EXPR::MATRIX: {
	gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
	const size_t k = m->size1, l = m->size2, tda = m->tda;
	if (k != t->n || l != t->m) break;
	state *next = t->st;
	for (size_t i = 0; i<k && next; i++)
	  for (size_t j = 0; j<l && next; j++)
	    next = match(next, m->data[i*tda+j]);
	return next;
      }
      case EXPR::DMATRIX: {
	gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
	const size_t k = m->size1, l = m->size2, tda = m->tda;
	if (k != t->n || l != t->m) break;
	state *next = t->st;
	for (size_t i = 0; i<k && next; i++)
	  for (size_t j = 0; j<l && next; j++)
	    next = match(next, m->data[i*tda+j]);
	return next;
      }
      case EXPR::IMATRIX: {
	gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
	const size_t k = m->size1, l = m->size2, tda = m->tda;
	if (k != t->n || l != t->m) break;
	state *next = t->st;
	for (size_t i = 0; i<k && next; i++)
	  for (size_t j = 0; j<l && next; j++)
	    next = match(next, m->data[i*tda+j]);
	return next;
      }
      case EXPR::CMATRIX: {
	gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
	const size_t k = m->size1, l = m->size2, tda = m->tda;
	if (k != t->n || l != t->m) break;
	state *next = t->st;
	for (size_t i = 0; i<k && next; i++)
	  for (size_t j = 0; j<l && next; j++) {
	    size_t n = 2*(i*tda+j);
	    next = match(next, m->data[n], m->data[n+1]);
	  }
	return next;
      }
      case EXPR::APP: {
	state *next = match(t->st, x->data.x[0]);
	return next?match(next, x->data.x[1]):0;
      }
      default:
	return t->st;
      }
    } else if (tagcmp(t->tag, x->tag) > 0)
      break;
  // no literal match, check for a matching qualified variable transition
  if (x->tag < EXPR::APP) {
    for (t = st->tr.begin(); t != st->tr.end() && t->tag == EXPR::VAR; t++) {
      if (t->ttag == 0)
	continue;
      else if (t->ttag == x->tag ||
	       (t->ttag == EXPR::MATRIX &&
		x->tag >= EXPR::DMATRIX && x->tag <= EXPR::IMATRIX))
	return t->st;
      else if (t->ttag < x->tag)
	break;
    }
  }
  // still no match, use default transition if present
  if ((t = st->tr.begin()) != st->tr.end() &&
      t->tag == EXPR::VAR && t->ttag == 0)
    return t->st;
  return 0;
}

state *matcher::match(state *st, double x)
{
  transl::const_iterator t;
  for (t = st->tr.begin(); t != st->tr.end(); t++)
    if (t->tag == EXPR::DBL && x == t->d)
      return t->st;
    else if (t->tag < EXPR::DBL || t->tag > 0)
      break;
  // no literal match, check for a matching qualified variable transition
  for (t = st->tr.begin(); t != st->tr.end() && t->tag == EXPR::VAR; t++)
    if (t->ttag == 0)
      continue;
    else if (t->ttag == EXPR::DBL)
      return t->st;
    else if (t->ttag < EXPR::DBL)
      break;
  // still no match, use default transition if present
  if ((t = st->tr.begin()) != st->tr.end() &&
      t->tag == EXPR::VAR && t->ttag == 0)
    return t->st;
  return 0;
}

static inline expr make_complex(double a, double b)
{
  interpreter& interp = *interpreter::g_interp;
  symbol &rect = interp.symtab.complex_rect_sym();
  return expr(rect.x, expr(EXPR::DBL, a), expr(EXPR::DBL, b));
}

state *matcher::match(state *st, double x, double y)
{
  return match(st, make_complex(x, y));
}

state *matcher::match(state *st, int x)
{
  transl::const_iterator t;
  for (t = st->tr.begin(); t != st->tr.end(); t++)
    if (t->tag == EXPR::INT && x == t->i)
      return t->st;
    else if (t->tag < EXPR::INT || t->tag > 0)
      break;
  // no literal match, check for a matching qualified variable transition
  for (t = st->tr.begin(); t != st->tr.end() && t->tag == EXPR::VAR; t++)
    if (t->ttag == 0)
      continue;
    else if (t->ttag == EXPR::INT)
      return t->st;
    else if (t->ttag < EXPR::INT)
      break;
  // still no match, use default transition if present
  if ((t = st->tr.begin()) != st->tr.end() &&
      t->tag == EXPR::VAR && t->ttag == 0)
    return t->st;
  return 0;
}

/* TA construction algorithm. */

state *matcher::make(const rule& ru, uint32_t skip)
{
  uint32_t rn = r.size();
  start = new state;
  state *end = make_state(start, rn, ru.lhs, skip);
  r.push_back(ru);
  end->r.push_back(rn++);
  build(start);
  return start;
}

state *matcher::make(const rulel& rl, uint32_t skip)
{
  start = 0;
  uint32_t rn = r.size();
  for (rulel::const_iterator ri = rl.begin(); ri != rl.end(); ++ri, ++rn) {
    uint32_t skp = skip;
    state *init = new state, *end = make_state(init, rn, ri->lhs, skp);
    r.push_back(*ri);
    end->r.push_back(rn);
    if (start) {
      merge_state(start, init);
      delete init;
    } else
      start = init;
  }
  if (start) build(start);
  return start;
}

state *matcher::make_state(state *st, uint32_t r, expr x, uint32_t& skip)
{
  if (skip > 0) {
    assert(x.tag() == EXPR::APP || x.tag() > 0);
    --skip;
    if (x.tag() == EXPR::APP) {
      state *next = make_state(st, r, x.xval1(), skip);
      return make_state(next, r, x.xval2(), skip);
    } else
      return st;
  }
  st->r.push_back(r);
  switch (x.tag()) {
  case EXPR::APP: {
    st->tr.push_back(trans(EXPR::APP));
    state *next = st->tr.begin()->st;
    next = make_state(next, r, x.xval1(), skip);
    next = make_state(next, r, x.xval2(), skip);
    return next;
  }
  case EXPR::VAR: {
    int8_t ttag = 0;
    if (x.ttag()<0) ttag = (int8_t)x.ttag();
    st->tr.push_back(trans(EXPR::VAR, ttag));
    return st->tr.begin()->st;
  }
  case EXPR::MATRIX: {
    exprll *xs = x.xvals();
    size_t n = xs->size(), m = xs->empty()?0:xs->front().size();
    /* We require a rectangular matrix here, but this isn't checked, the front
       end should guarantee this. */
#if 0
    for (exprll::const_iterator it = xs->first(), end = xs->end();
	 it != end; ++it) { assert(it->size() == m); }
#endif
    st->tr.push_back(trans(EXPR::MATRIX, n, m));
    state *next = st->tr.begin()->st;
    for (exprll::iterator it = xs->begin(), end = xs->end();
	 it != end; it++) {
      for (exprl::iterator jt = it->begin(), end = it->end();
	   jt != end; jt++) {
	next = make_state(next, r, *jt, skip);
      }
    }
    return next;
  }
  case EXPR::INT:
    st->tr.push_back(trans(EXPR::INT, x.ival()));
    return st->tr.begin()->st;
  case EXPR::BIGINT:
    st->tr.push_back(trans(EXPR::BIGINT, x.zval()));
    return st->tr.begin()->st;
  case EXPR::DBL:
    st->tr.push_back(trans(EXPR::DBL, x.dval()));
    return st->tr.begin()->st;
  case EXPR::STR:
    st->tr.push_back(trans(EXPR::STR, x.sval()));
    return st->tr.begin()->st;
  default:
    //assert(x.tag() > 0);
    st->tr.push_back(trans(x.tag()));
    return st->tr.begin()->st;
  }
}

state *matcher::make_vstate(int n, state *st)
{
  ruleml r = st->r;
  state *prefix = new state, *current = prefix;
  while (n-- > 0) {
    current->r = r;
    current->tr.push_back(trans(EXPR::VAR));
    current = current->tr.begin()->st;
  }
  *current = *st;
  return prefix;
}

void matcher::merge_state(state *st1, state *st2)
{
  merge_rules(st1->r, st2->r);
  merge_trans(st1->tr, st2->tr);
}

void matcher::merge_rules(ruleml& r1, ruleml& r2)
{
  ruleml cpr2 = r2;
  r1.merge(cpr2);
  r1.unique();
}

void matcher::merge_trans(transl& tr1, transl& tr2)
{
  // we only deal with the case that the merged automaton is a trie here:
  assert(tr2.size() <= 1);
  if (tr2.empty())
    ;
  else if (tr1.empty()) {
    transl cptr2 = tr2;
    tr1.splice(tr1.end(), cptr2);
  } else switch (tr2.begin()->tag) {
  case EXPR::APP:
    merge_ftrans(tr1, EXPR::APP, tr2.begin()->st);
    break;
  case EXPR::MATRIX:
    merge_mtrans(tr1, tr2.begin()->n, tr2.begin()->m, tr2.begin()->st);
    break;
  case EXPR::VAR:
    merge_vtrans(tr1, tr2.begin()->ttag, tr2.begin()->st);
    break;
  case EXPR::INT:
    merge_ctrans_int(tr1, tr2.begin()->i, tr2.begin()->st);
    break;
  case EXPR::BIGINT:
    merge_ctrans_bigint(tr1, tr2.begin()->z, tr2.begin()->st);
    break;
  case EXPR::DBL:
    merge_ctrans_double(tr1, tr2.begin()->d, tr2.begin()->st);
    break;
  case EXPR::STR:
    merge_ctrans_string(tr1, tr2.begin()->s, tr2.begin()->st);
    break;
  default:
    assert(tr2.begin()->tag > 0);
    merge_ftrans(tr1, tr2.begin()->tag, tr2.begin()->st);
    break;
  }
}

void matcher::merge_ftrans(transl& tr, int32_t tag, state *st)
{
  assert(tag == EXPR::APP || tag > 0);
  transl::iterator t;
  // look for a matching transition
  for (t = tr.begin(); t != tr.end(); t++) {
    if (t->tag == tag) {
      merge_state(t->st, st);
      return;
    } else if ((tag == EXPR::APP)
	       ? (t->tag < EXPR::APP || t->tag > 0) : (t->tag > tag))
      break;
  }
  // none found, create a new one
  trans t1 = trans(tag);
  // see whether we got an untyped var transition in this state
  transl::iterator t0 = tr.begin();
  if (t0 != tr.end() && t0->tag == EXPR::VAR && t0->ttag == 0) {
    /* Make the new state a copy of the old one for the var transition and
       then merge in the new transition. (Note that we cannot go the other way
       round since we generally assume that the subautomaton to be merged is a
       trie. This is guaranteed for st, but not for t0->st anymore.) */
    if (tag == EXPR::APP) {
      // binary symbol
      delete t1.st;
      t1.st = make_vstate(2, t0->st);
    } else
      // nullary symbol
      *t1.st = *t0->st;
    merge_state(t1.st, st);
  } else
    // no var transition either, just insert a new transition
    *t1.st = *st;
  tr.insert(t, t1);
}

void matcher::merge_mtrans(transl& tr, size_t n, size_t m, state *st)
{
  transl::iterator t;
  // look for a matching transition
  for (t = tr.begin(); t != tr.end(); t++) {
    if (t->tag == EXPR::MATRIX && t->n == n && t->m == m) {
      merge_state(t->st, st);
      return;
    } else if (t->tag < EXPR::MATRIX || t->tag > 0)
      break;
  }
  // none found, create a new one
  trans t1 = trans(EXPR::MATRIX, n, m);
  // see whether we got a matching var transition in this state
  transl::iterator t0 = tr.begin();
  while (t0 != tr.end() && t0->tag == EXPR::VAR && t0->ttag != EXPR::MATRIX)
    t0++;
  if (t0 == tr.end() || t0->tag != EXPR::VAR)
    // no matching var transition found, use an untyped one if available
    t0 = tr.begin();
  if (t0 != tr.end() && t0->tag == EXPR::VAR &&
      (t0->ttag == EXPR::MATRIX || t0->ttag == 0)) {
    /* Make the new state a copy of the old one for the var transition and
       then merge in the new transition. (Note that we cannot go the other way
       round since we generally assume that the subautomaton to be merged is a
       trie. This is guaranteed for st, but not for t0->st anymore.) */
    delete t1.st;
    t1.st = make_vstate(n*m, t0->st);
    merge_state(t1.st, st);
  } else
    // no var transition either, just insert a new transition
    *t1.st = *st;
  tr.insert(t, t1);
}

void matcher::merge_vtrans(transl& tr, int8_t ttag, state *st)
{
  transl::iterator t, t0 = tr.begin();
  /* If we don't have a variable transition for the given type tag in this
     state yet then create a new one. */
  for (t = t0; t != tr.end() && t->tag == EXPR::VAR && t->ttag > ttag; t++) ;
  if (t == tr.end() || t->tag != EXPR::VAR || t->ttag < ttag) {
    trans t1 = trans(EXPR::VAR, ttag);
    if (ttag != 0 && t0 != tr.end() && t0->tag == EXPR::VAR && t0->ttag == 0) {
      /* We have a typed variable transition, and there's already an existing
	 untyped (default) var transition. Make the new state a copy of the
	 old default state and merge in the new transition. */
      *t1.st = *t0->st;
      merge_state(t1.st, st);
    }
    /* Insert the new var transition before all others with smaller type tags,
       so that the list of var transitions is kept in descending order, with
       an unqualified var transition (which is always the default transition
       in a state) up front. */
    tr.insert(t, t1);
    t0 = tr.begin();
  }
  /* Add completions of the given state to existing transitions. */
  for (t = t0; t != tr.end(); t++) {
    switch (t->tag) {
    case EXPR::VAR:
      // matching variable symbol
      if (ttag == 0 || ttag == t->ttag)
	merge_state(t->st, st);
      break;
    case EXPR::APP:
      // binary symbol
      if (ttag == 0) {
	state *st1 = make_vstate(2, st);
	merge_state(t->st, st1);
	delete st1;
      }
      break;
    case EXPR::MATRIX:
      // matrix
      if (ttag == 0 || ttag == EXPR::MATRIX) {
	state *st1 = make_vstate(t->n*t->m, st);
	merge_state(t->st, st1);
	delete st1;
      }
      break;
    default:
      // matching constant or nullary symbol
      if (ttag == 0 || ttag == t->tag)
	merge_state(t->st, st);
      break;
    }
  }
}

void matcher::merge_ctrans_int(transl& tr, int32_t x, state *st)
{
  transl::iterator t;
  // look for a matching transition
  for (t = tr.begin(); t != tr.end(); t++) {
    if (t->tag == EXPR::INT && t->i == x) {
      merge_state(t->st, st);
      return;
    } else if (t->tag > 0 || t->tag < EXPR::INT)
      break;
  }
  // none found, create a new one
  trans t1 = trans(EXPR::INT, x);
  // see whether we got a matching var transition in this state
  transl::iterator t0 = tr.begin();
  while (t0 != tr.end() && t0->tag == EXPR::VAR && t0->ttag != EXPR::INT)
    t0++;
  if (t0 == tr.end() || t0->tag != EXPR::VAR)
    // no matching var transition found, use an untyped one if available
    t0 = tr.begin();
  if (t0 != tr.end() && t0->tag == EXPR::VAR &&
      (t0->ttag == EXPR::INT || t0->ttag == 0)) {
    *t1.st = *t0->st;
    merge_state(t1.st, st);
  } else
    // no var transition either, just insert a new transition
    *t1.st = *st;
  tr.insert(t, t1);
}

void matcher::merge_ctrans_bigint(transl& tr, const mpz_t& x, state *st)
{
  transl::iterator t;
  // look for a matching transition
  for (t = tr.begin(); t != tr.end(); t++) {
    if (t->tag == EXPR::BIGINT && mpz_cmp(t->z, x) == 0) {
      merge_state(t->st, st);
      return;
    } else if (t->tag > 0 || t->tag < EXPR::BIGINT)
      break;
  }
  // none found, create a new one
  trans t1 = trans(EXPR::BIGINT, x);
  // see whether we got a matching var transition in this state
  transl::iterator t0 = tr.begin();
  while (t0 != tr.end() && t0->tag == EXPR::VAR && t0->ttag != EXPR::BIGINT)
    t0++;
  if (t0 == tr.end() || t0->tag != EXPR::VAR)
    // no matching var transition found, use an untyped one if available
    t0 = tr.begin();
  if (t0 != tr.end() && t0->tag == EXPR::VAR &&
      (t0->ttag == EXPR::BIGINT || t0->ttag == 0)) {
    *t1.st = *t0->st;
    merge_state(t1.st, st);
  } else
    // no var transition either, just insert a new transition
    *t1.st = *st;
  tr.insert(t, t1);
}

void matcher::merge_ctrans_double(transl& tr, double x, state *st)
{
  transl::iterator t;
  // look for a matching transition
  for (t = tr.begin(); t != tr.end(); t++) {
    if (t->tag == EXPR::DBL && t->d == x) {
      merge_state(t->st, st);
      return;
    } else if (t->tag > 0 || t->tag < EXPR::DBL)
      break;
  }
  // none found, create a new one
  trans t1 = trans(EXPR::DBL, x);
  // see whether we got a matching var transition in this state
  transl::iterator t0 = tr.begin();
  while (t0 != tr.end() && t0->tag == EXPR::VAR && t0->ttag != EXPR::DBL)
    t0++;
  if (t0 == tr.end() || t0->tag != EXPR::VAR)
    // no matching var transition found, use an untyped one if available
    t0 = tr.begin();
  if (t0 != tr.end() && t0->tag == EXPR::VAR &&
      (t0->ttag == EXPR::DBL || t0->ttag == 0)) {
    *t1.st = *t0->st;
    merge_state(t1.st, st);
  } else
    // no var transition either, just insert a new transition
    *t1.st = *st;
  tr.insert(t, t1);
}

void matcher::merge_ctrans_string(transl& tr, const char *x, state *st)
{
  transl::iterator t;
  // look for a matching transition
  for (t = tr.begin(); t != tr.end(); t++) {
    if (t->tag == EXPR::STR && strcmp(t->s, x) == 0) {
      merge_state(t->st, st);
      return;
    } else if (t->tag > 0 || t->tag < EXPR::STR)
      break;
  }
  // none found, create a new one
  trans t1 = trans(EXPR::STR, x);
  // see whether we got a matching var transition in this state
  transl::iterator t0 = tr.begin();
  while (t0 != tr.end() && t0->tag == EXPR::VAR && t0->ttag != EXPR::STR)
    t0++;
  if (t0 == tr.end() || t0->tag != EXPR::VAR)
    // no matching var transition found, use an untyped one if available
    t0 = tr.begin();
  if (t0 != tr.end() && t0->tag == EXPR::VAR &&
      (t0->ttag == EXPR::STR || t0->ttag == 0)) {
    *t1.st = *t0->st;
    merge_state(t1.st, st);
  } else
    // no var transition either, just insert a new transition
    *t1.st = *st;
  tr.insert(t, t1);
}

void matcher::build(state *_st)
{
  st.push_back(_st);
  _st->s = s++;
  transl::const_iterator t;
  for (t = _st->tr.begin(); t != _st->tr.end(); t++)
    build(t->st);
}
