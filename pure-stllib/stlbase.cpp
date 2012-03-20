/* stl.cpp -- Support for stl containers of pure_expr*
    
Copyright (c) 2011-2012 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlvec, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlvec distribution package for details.

*/

#include "stlbase.hpp"
#include <iostream>
#include <stdlib.h>
#include <cstring>

using namespace std;


/**** Interpreter Local Storage Global Symbols **************************/

static px* px_newsym(const char *name)
{
  return pure_new(pure_symbol(pure_sym(name)));
}

px* px_cons_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym(":");
  return sym;
}

px* px_null_list_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("[]");
  return sym;
}

px* px_rocket_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("=>");
  return sym;
}

px* px_less_than_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("<");
  return sym;
}

px* px_equal_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("==");
  return sym;
}

px* px_greater_than_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym(">");
  return sym;
}

px* px_same_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("===");
  return sym;
}

px* px_bad_function_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("bad_function");
  return sym;
}

px* px_out_of_bounds_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("out_of_bounds");
  return sym;
}

px* px_range_overflow_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym(":");
  return sym;
}

px* px_range_overlap_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("range_overlap");
  return sym;
}

px* px_bad_argument_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("bad_argument");
  return sym;
}

px* px_failed_cond_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("failed_cond");
  return sym;
}

/**** Helpers for px* ***************************************************/

ostream& operator<<(ostream& os, px* x){
  char *s = str(x);
  os << s;
  free(s);
  return os;
}

/**** Handles to hold pure_expr* ****************************************/

px_handle::px_handle() : pxp_(0)
{ 
}

px_handle::px_handle(px* p)
{
  if (p) {
    if (p->refc > 0)
      ++(p->refc);
    else
      pure_new(p);
  }
  pxp_ = p;
}

px_handle::px_handle(const px_handle& pxh)
{
  px* p = pxh.pxp_;
  if (p) {
    if (p->refc > 0)
      ++(p->refc);
    else
      pure_new(p);
  }
  pxp_ = p;
}

px_handle& px_handle::operator=(const px_handle& rhs)
{
  if (&rhs!=this){
    if (pxp_) {
      if (pxp_->refc > 1)
        --(pxp_->refc);
      else
        pure_free(pxp_);
    }
    pxp_ = rhs.pxp_;
    if (pxp_)
      ++(pxp_->refc);
  }
  return *this;
}

px_handle:: ~px_handle()
{
  if (pxp_) {
    if (pxp_->refc > 1)
      --(pxp_->refc);
    else
      pure_free(pxp_);
  }
}

ostream& operator<<(ostream& os, const px_handle& pxh)
{
  os << pxh.pxp();
  return os;
}

/*** Function objects to lift px* functions to pxh functions ***********/

enum {
  INT		= -3,	// 32 bit signed integer
  BIGINT	= -4,	// bigint (mpz_t)
  DBL		= -5,	// double precision floating point number
  STR		= -6,	// utf-8 string (char*)
};

pxh_fun& pxh_fun::operator=(const pxh_fun& rhs)
{
  if (&rhs!=this){
    pure_new(rhs.fun_);
    pure_free(fun_);
    fun_ = rhs.fun_;
  }
  return *this;
}

pxh pxh_fun1::operator()(const pxh& arg) const
{
  px* exception = 0;
  px* ret =  pure_appxl(fun_, &exception, 1, arg.pxp());
  if (exception) throw exception;
  if (!ret) bad_function();
  return pxh(ret);
}

pxh pxh_fun2::operator()(const pxh& arg1, const pxh& arg2) const
{
  px* exception = 0;
  px* ret =  pure_appxl(fun_, &exception, 2, arg1.pxp(), arg2.pxp());
  if (exception) throw exception;
  if (!ret) bad_function();
  return pxh(ret);
}

bool pxh_pred1::operator()(const pxh& arg) const
{
  px* exception = 0;
  int32_t ret;
  px* pxres = pure_appxl(fun_, &exception, 1, arg.pxp());
  if (exception) throw exception;
  if (!pxres) bad_function();
  int ok = pure_is_int(pxres, &ret);
  pure_freenew(pxres);
  if (!ok) failed_cond();
  return ok && ret;
}

pxh_pred2::pxh_pred2(px* f) : pxh_fun(f) 
{
  is_eq = is_same = is_lt = is_gt = false;
  if ( same(f,px_less_than_sym()) ) 
    is_lt = true;
  else if ( same(f,px_greater_than_sym()) ) 
    is_gt = true;
  else if ( same(f,px_equal_sym()) ) 
    is_eq = true;
  else if ( same(f,px_same_sym()) ) 
    is_same = true;
  is_fast = is_eq || is_same || is_lt || is_gt;
}

bool pxh_pred2::operator()(const pxh& x_pxh, const pxh& y_pxh) const
{
  bool ret = 0;
  int comp;
  px* x = const_cast<px*>(x_pxh.pxp());
  px* y = const_cast<px*>(y_pxh.pxp());
  bool can_optimize = is_fast && (x->tag == y->tag && x->tag < 0);
  if (can_optimize) {
    if (is_lt || is_gt) {
      switch (x->tag) {
      case STR:
        comp = strcmp(x->data.s, y->data.s);
        if (comp==0)
          return 0;
        else
          ret = comp < 0;
        break;
      case INT:
        ret = x->data.i < y->data.i;
        break;
      case DBL:
        ret = x->data.d < y->data.d;
        break;
      case BIGINT:
        ret = bigint_cmp(x->data.z, y->data.z) < 0;
        break;
      }
      return is_lt ? ret : !ret;
    }
    else {
      if (x==y) {
        ret = 1;
      }
      else {
        switch (x->tag) {
        case STR:
          ret = strcmp(x->data.s, y->data.s) == 0;
          break;
        case INT:
          ret = x->data.i == y->data.i;
          break;
        case DBL:
          ret = x->data.d == y->data.d;
          break;
        case BIGINT:
          ret = bigint_cmp(x->data.z, y->data.z) == 0;
          break;
        }
      }
      return ret;
    }
  }
  px* exception = 0;
  px* pxres =  pure_appxl(fun_, &exception, 2, x, y);
  if (exception) throw exception;
  if (!pxres) bad_function();
  int satisfied; 
  if ( !pure_is_int(pxres, &satisfied) ) bad_argument();
  pure_freenew(pxres);
  return satisfied != 0;
}

// bool pxh_pred2::operator()(const pxh& left, const pxh& right) const
// {
//   int32_t ret;
//   px* exception = 0;
//   px* pxres = pure_appxl(fun_, &exception, 2, left.pxp(), right.pxp());
//   if (exception) throw exception;
//   if (!pxres) bad_function();
//   int ok = pure_is_int(pxres, &ret);
//   pure_freenew(pxres);
//   if (!ok) failed_cond();
//   return ok && ret;
// }

bool pxhpair_less::operator()(const pxhpair& left,
                              const pxhpair& right) const
{
  try {
    const pxh& lf = left.first;
    const pxh& rf = right.first;
    if (first_less(lf,rf)) return 1;
    if (first_less(rf,lf)) return 0;
    return (second_less(left.second, right.second));
  }
  catch (px* e) {
    pure_throw(e);
  }
}

bool pxhpair_equal::operator()(const pxhpair& lhs,
                               const pxhpair& rhs) const
{
  try {
    bool ok = first_equal(lhs.first, rhs.first)  && 
              second_equal(lhs.second, rhs.second);
    return ok;
  }
  catch (px* e) {
    pure_throw(e);
  }
}

bool pxhpair_first_equal::operator()(const pxhpair& lhs,
                                     const pxhpair& rhs) const
{
  try {
    return first_equal(lhs.first, rhs.first);
  }
  catch (px* e) {
    pure_throw(e);
  }
}

bool pxhpair_first_equivalent::operator()(const pxhpair& lhs,
                                          const pxhpair& rhs) const
{
  try {
    bool ok = !first_less(lhs.first, rhs.first) && 
      !first_less(rhs.first, lhs.first);
    return ok;
  }
  catch (px* e) {
    pure_throw(e);
  }
}

bool pxhpair_equivalent::operator()(const pxhpair& lhs,
                                    const pxhpair& rhs) const
{
  try {
    bool ok = !first_less(lhs.first, rhs.first) && 
              !first_less(rhs.first, lhs.first) && 
              second_equal(lhs.second, rhs.second);
    return ok;
  }
  catch (px* e) {
    pure_throw(e);
  }
}

pxh pxh_gen::operator()()
{
  px* exception = 0;
  px* ret =  pure_appxl(fun_, &exception, 1, pure_tuplel(0));
  if (exception) throw exception;
  if (!ret) bad_function();
  return pxh(ret);
}

int stl_refc(pure_expr *x)
{
  return x->refc - 1; // want x->refc before x was passed into here
}


/*** Errors ********************************************************/

void bad_function() {pure_throw(px_bad_function_sym());}
void index_error() {pure_throw(px_out_of_bounds_sym());}
void range_overflow() {pure_throw(px_range_overflow_sym());}
void range_overlap() {pure_throw(px_range_overlap_sym());}
void bad_argument() {pure_throw(px_bad_argument_sym());}
void failed_cond() {pure_throw(px_failed_cond_sym());}

/*** Helpers ********************************************************/

bool pxrocket_to_pxlhs_pxrhs(px* pxr, px** lhs, px** rhs)
{
  px* app;
  size_t argc;
  px** args;
  bool ok = pure_is_appv(pxr, &app, &argc, &args) && argc == 2;
  if (ok) {
    *lhs = args[0];
    *rhs = args[1];
  }
  free(args);
  return ok;
}

px* pxlhs_pxrhs_to_pxrocket(const px* lhs, const px* rhs)
{
  px* rocket = px_rocket_sym();
  return pure_appl(rocket, 2, lhs, rhs); 
}

bool pxrocket_to_pxhpair(px* pxr, pxhpair& pair)
{
  px* lhs = 0;
  px* rhs = 0;
  bool ret = pxrocket_to_pxlhs_pxrhs(pxr, &lhs, &rhs);
  pair.first = lhs;
  pair.second = rhs;
  return ret;
}

px* pxhpair_to_pxrocket(const pxhpair& pair)
{
  return pxlhs_pxrhs_to_pxrocket(pair.first, pair.second);
}

px* pxhpair_to_pxlhs(const pxhpair& pair)
{
  return pair.first;
}

px* pxh_to_pxp(pxh h)
{
  return h.pxp();
}

/**** Symbols and functions for stlmap and stlvec *********************/

px* stl_begin_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("stl::smbeg");
  return sym;
}

px* stl_end_sym()
{
  static ILS<px*> _sym = NULL; px* &sym = _sym();
  if (!sym) sym = px_newsym("stl::smend");
  return sym;
}

void stl_sv_delete(sv* p){
  delete(p);
}

sv* stl_sv_make_empty()
{
  sv* ret  = new sv;
  return ret;
}
