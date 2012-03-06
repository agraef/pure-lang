/* stlvec.cpp -- C++ support for stlvec.pure
    
Copyright (c) 2011-2012 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlvec, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlvec distribution package for details.

*/

#include <stdlib.h>
#include <iostream>
#include <algorithm>
#include <numeric>
#include "stlvec.hpp"

using namespace std;

/*** sv_range functions *********************************************/

// treats svback as invalid
static bool set_iter(sv* vec, int ndx, svi& iter)
{
  size_t vec_sz = vec->size();
  if (ndx == svbeg)
    iter = vec->begin();
  else if (ndx == svend)
    iter = vec->end();
  else {
    if (ndx < 0 || ndx > vec_sz) return false;
    if (ndx == vec_sz)
      iter = vec->end();
    else
      iter = vec->begin() + ndx;
  }
  return true;  
}

static sv* get_sv_from_app(px* app){
  void* ret = 0;
  px* fun;
  size_t argc;
  px** args;
  pure_is_appv(app, &fun, &argc, &args);
  if (argc == 1 && !pure_is_pointer(args[0], &ret))
    ret = 0;
  free(args);
  return (sv*)ret; 
}

// The svrev feature is currently ignored
// Individual functions will decide to recognize, ignore, or throw error
sv_range::sv_range(px* svi_tuple)
{
  size_t tpl_sz;
  px** elems;
  int bfr;
  
  is_valid = true;
  pure_is_tuplev(svi_tuple, &tpl_sz, &elems);
  vec = get_sv_from_app(elems[0]);
  if (!vec) {
    is_valid = false;
    goto done;
  }
  num_iters = tpl_sz-1;
  is_reversed = false;
  if (tpl_sz > 1 && pure_is_int(elems[tpl_sz-1], &bfr) && bfr == svrev){
    is_reversed = true;
    num_iters--;
  }
  if (num_iters > sv_max_num_iters) {
    is_valid = false;
    goto done;
  }
  if (num_iters == 0) {
    iters[0] = vec->begin();
    iters[1] = vec->end();
    num_iters = 2;
  }
  else {
    for (int i = 0; i<num_iters; i++) {
      if ( !pure_is_int(elems[i+1], &bfr) ) {
        is_valid = false;
        goto done;
      }
      if (!set_iter(vec, bfr, iters[i]) ) {
        is_valid = false;
        if (bfr == svback)
            goto done;
        free(elems);
        index_error();
      }
    }
    for (int i = 1;  i<num_iters; i++) {
      if (iters[i] < iters[i-1]) {
          is_valid = false;
          goto done;
        }
    }
  }

 done:
  free(elems);
}

sv_back_iter::sv_back_iter(px* svi_tuple)
{
  size_t sz;
  px** elems;
  int bfr;
  
  is_valid = true;
  pure_is_tuplev(svi_tuple, &sz, &elems);
  if (sz != 2 || !pure_is_int(elems[1], &bfr) || bfr != svback) {
    is_valid = false;
    goto done;
  }
  vec = get_sv_from_app(elems[0]);
  if (!vec) {
    is_valid = false;
    goto done;
  }

 done:
  free(elems);
}

int sv_range::size()
{
  int ret = 0;
  if (is_valid) {
    if (num_iters >= 2)
      ret = end() - beg();
    else
      ret = vec->end() - beg();
  }
  return ret;
}

bool sv_range::contains(sv* iter_vec, const svi& iter)
{
  return (vec == iter_vec) && (beg() <= iter) && (iter < end()); 
}

bool sv_range::overlaps(sv_range&  iters)
{
  sv* vec1 = iters.vec;
  if (vec != vec1) return false;
  return !( (this->end() <= iters.beg()) || (iters.end() < beg()) );
}

/*** Helpers for stlvec.cpp and sva_xxx *****************************/

int iter_pos(sv* vec, svi iter)
{
  return iter == vec->end() ? svend : iter - vec->begin();
}

/*** Helpers for stlvec.cpp only ************************************/

typedef sv::size_type svsize_t;

static inline int ndx_ok(sv* vec, int n)
{
  return n >= 0 && n < vec->size();
}

/*** Functions for sv vector<pxh> ***********************************/

sv* stl_sv_make_from_xs(px* xs_or_vec)
{ 
  px** elems = NULL;
  size_t sz;
  sv* ret = 0;
  if ( pure_is_listv(xs_or_vec, &sz, &elems) ) {
    ret = new sv(elems, elems+sz);
    free(elems);
  }
  else if (matrix_type(xs_or_vec) == 0) {
    int sz = matrix_size(xs_or_vec); 
    pure_expr** data = (pure_expr**) pure_get_matrix_data(xs_or_vec);
    ret = new sv(data, data+sz);
  }
  else
    bad_argument();
  return ret;
}

sv* stl_sv_make_n(px* x, int n)
{ 
  if (n<0) bad_argument();
  sv* ret = new sv(n, x);
  return ret;
}

sv* stl_sv_dup(px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  sv* ret = new sv(rng.beg(), rng.end()); 
  return ret;
}

px* stl_sv_vector(px* tpl) 
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  size_t sz = rng.size();
  if (!sz)
    return pure_matrix_columnsv(0,NULL);
  px** bfr = (px**)malloc(sizeof(px*)*sz);
  transform(rng.beg(), rng.end(), bfr, pxh_to_pxp);
  px* ret = pure_matrix_columnsv(sz, bfr);
  free(bfr);
  return ret;
}

void stl_sv_reserve(sv* vec, int n)
{
  vec->reserve(n);
}

int stl_sv_size(sv* vec)
{
  return vec->size();
}

int stl_sv_iter_size(px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  return rng.size();
}

int stl_sv_max_size(sv* vec)
{
  return vec->max_size();
}

bool stl_sv_empty(sv* vec)
{
  return vec->empty();
}

int stl_sv_capacity(sv* vec)
{
  return vec->capacity();
}

px* stl_sv_get(sv* vec, int n)
{
  if (!ndx_ok(vec,n)) index_error();
  return (*vec)[n].pxp();
}

void stl_sv_put(sv* vec, int n, px* val)
{
  if (ndx_ok(vec, n)) {
    (*vec)[n] = val;
  }
  else
    index_error();
}

px* stl_sv_front(sv* vec)
{
  return (*vec)[0].pxp();
}

px* stl_sv_back(sv* vec)
{
  return (*vec)[vec->size()-1].pxp();
}

void stl_sv_push_back(sv* vec, px* val)
{
  vec->push_back(val);
}

void stl_sv_pop_back(sv* vec)
{
  vec->pop_back();
}

void stl_sv_splice(sv* vec, int p, px* xs_or_tpl)
{
  svi beg1;
  size_t sz = 0;
  px** elems = NULL;
  if (p==svback) p = svend;
  if ( !set_iter(vec, p, beg1) )
    index_error();
  if (pure_is_listv(xs_or_tpl, &sz, &elems)) {
    vec->insert(beg1, elems, elems+sz );
    free(elems);    
  } 
  else {
    sv_range rng(xs_or_tpl);
    if (!rng.is_valid || rng.num_iters != 2) bad_argument();
    vec->insert(beg1, rng.beg(), rng.end());  
  }
}

void stl_sv_erase(px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  (rng.vec)->erase(rng.beg(), rng.end());
}

void stl_sv_clear(sv* vec)
{
  vec->clear();
}

bool stl_sv_allpairs(px* comp, px* tpl1, px* tpl2)
{
  pxh_pred2 fun(comp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  try {
    return equal(rng1.beg(), rng1.end(), rng2.beg(), fun);
  } catch (px* e) {
    pure_throw(e);
    return 0;
  }
}

px* stl_sv_list(px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  sv& v = *(rng.vec);
  int b = rng.beg() - v.begin(); 
  int e = b + rng.size(); 
  px* cons = px_cons_sym();
  px* nl = px_null_list_sym();
  if (b>=e) return nl;
  px* res = 0;
  px* y = 0;
  for (int i = b; i < e; i++){
    px* fx =  v[i].pxp();
    px* last = pure_app(pure_app(cons,fx),nl);
    if (!res)
      res = y = last;
    else {
      y->data.x[1] = pure_new(last);
      y = last;
    }
  }
  return res;  
}

px*  stl_sv_listmap(px* fun, px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  sv& v = *(rng.vec);
  int b = rng.beg() - v.begin(); 
  int e = b + rng.size(); 
  px* cons = px_cons_sym();
  px* nl = px_null_list_sym();
  if (b>=e) return nl;
  px* exception = 0;
  px* res = 0;
  px* y = 0;
  for (int i = b; i < e; i++){
    px* fx = pure_appxl(fun, &exception, 1, v[i].pxp());
    if (exception) {
      if (res) pure_freenew(res);
      pure_throw(exception);
    }
    px* last = pure_app(pure_app(cons,fx),nl);
    if (!res)
      res = y = last;    
    else {
      y->data.x[1] = pure_new(last);
      y = last;
    }
  }
  return res;  
}

px* stl_sv_listcatmap(px* fun, px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  sv& v = *(rng.vec);
  int b = rng.beg() - v.begin(); 
  int e = b + rng.size(); 
  px* cons = px_cons_sym();
  px* nl = px_null_list_sym();
  if (b>=e) return nl;
  px* exception = 0;
  px* elm;
  px* res = 0;
  px* y = 0;
  px* *elms;
  size_t sz;
  for (int i = b; i < e; i++){
    px* fx =  pure_appxl(fun, &exception, 1, v[i].pxp());
    if (exception) {
      if (res) pure_freenew(res);
      if (fx) pure_freenew(fx);
      pure_throw(exception);
    }
    if ( !pure_is_listv(fx, &sz, &elms) ){
      if (fx) pure_freenew(fx);
      if (res) pure_freenew(res);
      bad_argument();      
    }
    for (int j = 0; j < sz; j++) {
      px* last = pure_app(pure_app(cons,elms[j]),nl);
      if (!res)
        res = y = last;    
      else {
        y->data.x[1] = pure_new(last);
        y = last;
      }
    }
    pure_freenew(fx);
    free(elms);
  }

  return res;  
}

static px* sv_foldl_rng(px* fun, px* val, svi beg, svi end)
{ 
  px* res = pure_new(val);
  px* exception = 0;
  for (svi i = beg; i != end; i++){
    px* fxy =  pure_appxl(fun, &exception, 2, res, i->pxp());
    if (exception) {
      if (res) pure_free(res);
      throw exception;
    }
    pure_new(fxy);  // FIX -- fxy is not NULL
    pure_free(res);
    res = fxy;
  }
  pure_unref(res);
  return res;
}

px* stl_sv_foldl(px* fun, px* val, px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    return sv_foldl_rng(fun, val, rng.beg(), rng.end());
  } catch (px* e) {
    pure_throw(e);
    return 0;
  }
}

px* stl_sv_foldl1(px* fun, px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  if (rng.size() < 2) bad_argument();
  try {
    pxh valh = *(rng.beg());
    return sv_foldl_rng(fun, valh.pxp(), rng.beg()+1, rng.end());  
  }catch (px* e) {
    pure_throw(e);
    return 0;
  }
}

static px* sv_foldr_rng(px* fun, px* val, reverse_svi beg, reverse_svi end)
{ 
  px* res = pure_new(val);
  px* exception = 0;
  for (reverse_svi i = beg; i != end; i++){
    px* fxy =  pure_appxl(fun, &exception, 2, i->pxp(), res);
    if (exception) {
      if (res) pure_free(res);
      throw exception;
    }
    pure_new(fxy);
    pure_free(res);
    res = fxy;
  }
  pure_unref(res);
  return res;
}

px* stl_sv_foldr(px* fun, px* val, px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  reverse_svi beg(rng.end());
  reverse_svi end(rng.beg());
  try {
    return sv_foldr_rng(fun, val, beg, end);
  }catch (px* e) {
    pure_throw(e);
    return 0;
  }
}

px* stl_sv_foldr1(px* fun, px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  if (rng.size() < 2) bad_argument();
  reverse_svi beg(rng.end());
  reverse_svi end(rng.beg());
  try {
    pxh valh = *beg;
    return sv_foldr_rng(fun, valh.pxp(), beg+1, end);
  }catch (px* e) {
    pure_throw(e);
    return 0;
  }
}

