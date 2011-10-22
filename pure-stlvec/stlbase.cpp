/* stl.cpp -- Support for stl containers of pure_expr*
    
Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

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

using namespace std;

/**** Helpers for pure_expr *******************************************/

ostream& operator<<(ostream& os, px* x){
  char *s = str(x);
  os << s;
  free(s);
  return os;
}

static bool px_trace_enabled = false;

static bool sv_trace_enabled = false;

void stl_set_px_trace(bool enable)
{
  px_trace_enabled = enable;
}

void stl_set_sv_trace(bool enable) 
{
  sv_trace_enabled = enable;
}

bool stl_px_trace_enabled()
{
  return px_trace_enabled;
}

bool stl_sv_trace_enabled()
{
  return sv_trace_enabled;
}

#ifdef STL_DEBUG
px* px_new(px* x)
{
  void* ptr = x;
  if (px_trace_enabled)
    cerr << "[px_new(" << x << ") --> ";
  pure_new(x);
  if (px_trace_enabled)
    cerr << x->refc << "]"  << endl;
  return x;
}

void px_free(px* x)
{
  void* ptr = x;
  if (x->refc <= 0) {
    cerr << "[tried to free px with refc le 0\n";
    cerr << ptr << "]\n";
    bad_argument();
  }
  if (px_trace_enabled)
    cerr << "[px_free(" << x << ") --> ";
  if (x->refc > 1) {
    pure_free(x);
    if (px_trace_enabled)
      cerr << x->refc << "]" << endl;
  }
  else {
    if (px_trace_enabled)
      cerr << 0 << "]" << endl;
    pure_free(x);    
  }
}

void px_freenew(px* x)
{
  void* ptr = x;
  if (px_trace_enabled) {
    cerr << "[px_freenew(" << x << ") with refc = ";
    cerr << x->refc <<  "]" << endl;
  }
  pure_freenew(x);
}

void px_ref(px* x)
{
  void* ptr = x;
  if (px_trace_enabled)
    cerr << "[px_ref(" << x << ") --> ";
  pure_ref(x);
  if (px_trace_enabled)
    cerr << x->refc << "]"  << endl;
}

void px_unref(px* x)
{
  void* ptr = x;
  if (x->refc <= 0) {
    cerr << "[tried to unref px with refc le 0\n";
    cerr << ptr << "]\n";
    bad_argument();
  }
  if (px_trace_enabled)
    cerr << "[px_unref(" << x << ") --> ";
  if (x->refc > 1) {
    pure_unref(x);
    if (px_trace_enabled)
      cerr << x->refc << "]" << endl;
  }
  else {
    if (px_trace_enabled)
      cerr << 0 << "]" << endl;
    pure_unref(x);    
  }
}

#endif


/**** Handles to hold pure_expr* ****************************************/

px_handle::px_handle() : pxp_(px_new(pure_pointer(0))){}

px_handle& px_handle::operator=(const px_handle& rhs)
{
  if (&rhs!=this){
    px_new(rhs.pxp_);
    px_free(pxp_);
    pxp_ = rhs.pxp_;
  }
  return *this;
}

ostream& operator<<(ostream& os, const px_handle& pxh)
{
  os << pxh.pxp();
  return os;
}

/*** Function objects to lift px* functions to pxh functions ***/

pxh_fun& pxh_fun::operator=(const pxh_fun& rhs)
{
  if (&rhs!=this){
    px_new(rhs.fun_);
    px_free(fun_);
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
  px_freenew(pxres);
  if (!ok) failed_cond();
  return ok && ret;
}

bool pxh_pred2::operator()(const pxh& left, const pxh& right) const
{
  int32_t ret;
  px* exception = 0;
  px* pxres = pure_appxl(fun_, &exception, 2, left.pxp(), right.pxp());
  if (exception) throw exception;
  if (!pxres) bad_function();
  int ok = pure_is_int(pxres, &ret);
  px_freenew(pxres);
  if (!ok) failed_cond();
  return ok && ret;
}

pxh pxh_gen::operator()()
{
  px* exception = 0;
  px* ret =  pure_appxl(fun_, &exception, 1, pure_tuplel(0));
  if (exception) throw exception;
  if (!ret) bad_function();
  return pxh(ret);
}

extern "C"
int stl_refc(pure_expr *x){
  return x->refc - 1; // want refc before pass into here
}

/**** Helpers **********************************************************/

void stl_throw_sym(const char *name)
{
  pure_throw(pure_symbol(pure_sym(name)));
}

int cons_tag()
{
  static int tag = pure_getsym(":");
  return tag;
}

int null_list_tag()
{
  static int tag = pure_getsym("[]");
  return tag;
}

int rocket_tag()
{
  static int tag = pure_getsym("=>");
  return tag;
}

bool rocket_to_pair(px* rp, px** lhs, px** rhs)
{
  px* app;
  size_t argc;
  px** args;
  bool ok = pure_is_appv(rp, &app, &argc, &args) && argc == 2;
  if (ok) {
    *lhs = args[0];
    *rhs = args[1];
  }
  free(args);
  return ok;
}

px* pair_to_rocket(px* lhs, px* rhs)
{
  px* rocket = pure_const(rocket_tag());
  return pure_appl(rocket, 2, lhs, rhs); 
} 

px* pxh_to_pxp(pxh h){return h.pxp();}

px* stl_begin()
{
  return pure_symbol(pure_sym("stl_begin_iterator"));
}

px* stl_end()
{
  return pure_symbol(pure_sym("stl_end_iterator"));
}

px* stl_insert()
{
  return pure_symbol(pure_sym("stl_insert_iterator"));
}

px* stl_back_insert()
{
  return pure_symbol(pure_sym("stl_back_insert_iterator"));
}

