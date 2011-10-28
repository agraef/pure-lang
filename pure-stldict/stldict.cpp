/* stldict.cpp -- C++ support for stldict.pure

--- DRAFT - FOR DISCUSSON PURPOSES ONLY ---
    
Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stldict, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stldict distribution package for details.

*/

#include <stdlib.h>
#include <iostream>
#include <algorithm>
#include <numeric>
#include "stldict.hpp"

using namespace std;

/*** Helpers for stldict.cpp only ************************************/

static px* null_value() 
{
  static px* nv = pure_pointer(0);
  return nv;
}

static void set_kv(sdi i, px** k, px** v)
{
  const pxh h_key = i->first;
  pxh h_val = i->second;
  *k = h_key.pxp();
  *v = h_val.pxp();
}

static int is_less(px* cmp, px* x, px* y) 
{
  int32_t lt;
  px* exception = 0;
  px* pxres = pure_appxl(cmp, &exception, 2, x, y);
  if (exception) pure_throw(exception);
  if (!pxres) bad_function();
  int ok = pure_is_int(pxres, &lt);
  px_freenew(pxres);
  if (!ok) failed_cond();
  return lt;
}

static pxh_pair sd_rocket_to_pair(pxh rp)
{
  px *k, *v;
  bool ok = rocket_to_pair(rp.pxp(), &k, &v);
  return pxh_pair(k,v);
}

static px* sd_pair_to_rocket(const pxh_pair& kv)
{
  return pair_to_rocket(kv.first.pxp(), kv.second.pxp());
}

static px* sd_pair_to_key(const pxh_pair& kv)
{
  return kv.first.pxp();
}

static int sd_get_size(sd* dict, sdi b, sdi e)
{
 size_t sz = 0;
 sdmap& mp = dict->mp;
 if (b == mp.begin() && e == mp.end())
    sz = mp.size();
 else 
   while(b++ != e) sz++;
 return sz;
}

static void update_aux(sd* dict, px* k, px* v)
{
  sdmap& mp = dict->mp;
  sdi i;
  //cerr << "update_aux k: " << k << ", v: " << v << endl;
  if ( dict->get_cached_sdi(k, i) ) {
    i->second = v;
  }
  else {
    pair<sdi,bool> i_ok = mp.insert(pxh_pair(k,v));
    if (!i_ok.second)
      i_ok.first->second = v;
    dict->cache_sdi(i_ok.first);
  }
} 

static bool insert_aux(sd* dict, px* kv)
{
  px *k, *v;
  bool ok = true;
  if (dict->keys_only) {
    k = kv;
    v = null_value();
  } 
  else {
    if ( rocket_to_pair(kv, &k, &v) )
      ;
    else if (dict->has_dflt) {
      k = kv;
      v = dict->dflt.pxp();
    }
    else {
      k = kv;
      v = null_value();
      ok = false;
    }
  }
  update_aux(dict, k, v);
  return ok;
}

/*** stldict members  ***********************************************/

stldict::stldict(px* cmp, bool keyonly) : 
  mp(pxh_pred2(cmp)), px_comp(cmp), dflt(null_value()),
  has_dflt(0), has_recent_sdi(0), keys_only(keyonly), 
  recent_sdi(mp.end()) {}

stldict::stldict(px* cmp, bool ko, px* d) :
    mp(pxh_pred2(cmp)), px_comp(cmp), dflt(d), 
    has_dflt(1), has_recent_sdi(0), keys_only(ko),
    recent_sdi(mp.end()) {}

sdi stldict::find(px* key)
{
  sdi iter;
  if (key == sdbeg())
    iter = mp.begin();
  else if (key == sdend())
    iter = mp.end();
  else
    iter = mp.find(key);
  return iter;  
}

bool stldict::get_cached_sdi(px* k, sdi& i)
{
  bool found = k != sdbeg() && k != sdend() &&
    has_recent_sdi && same(recent_sdi->first.pxp(), k);
  if (found) i = recent_sdi;
  //if (found) cerr << "get_cached_sdi, found k: " << k << endl;
  return found;
}

void stldict::cache_sdi(sdi& i)
{
  if ( i != mp.end() ) {
    has_recent_sdi = true;
    recent_sdi = i;
  }
}

void stldict::clear_cache()
{
  has_recent_sdi = 0;
}

void stldict::erase(sdi pos)
{
  if (has_recent_sdi && recent_sdi == pos)
    has_recent_sdi = false;
  mp.erase(pos);
}

int stldict::erase(px* k)
{
  int ret = 1;
  if ( !mp.empty() ) {
    sdi i;
    if ( get_cached_sdi(k, i) )
      erase(i);
    else {
      if (k == sdbeg())
        erase(mp.begin());
      else if (k != sdend())
        mp.erase(k);
      else
        ret = 0;
    }
  }
  return ret;
}

void stldict::erase(sdi first, sdi last)
{
  has_recent_sdi = false;
  mp.erase(first, last);
}

void stldict::clear()
{
  has_recent_sdi = false;
  mp.clear();
}

/*** sd_iters functions *********************************************/

static sdi get_iter(sdmap& mp , px* key, bool upper)
{
  sdi iter;
  if (key == sdbeg())
    iter = mp.begin();
  else if (key == sdend())
    iter = mp.end();
  else {
    //pxh pxh_key(key);
    if (upper) 
      iter = mp.upper_bound(key);
    else 
      iter = mp.lower_bound(key);
  }
  return iter;  
}

static px* iter_to_key(const sdmap& mp, const sdi& it)
{
  if (it == mp.end()) return sdend();
  if (it == mp.begin()) return sdbeg();
  return it->first.pxp();
}

static sd* get_sd_from_app(px* app)
{
  void* ret = 0;
  px* fun;
  size_t argc;
  px** args;
  pure_is_appv(app, &fun, &argc, &args);
  if (argc == 1 && !pure_is_pointer(args[0], &ret))
    ret = 0;
  free(args);
  return (sd*)ret; 
}

// sets two iterators [beg, end)
// if first >= last then use  mp.end(), mp.end()
sd_iters::sd_iters(px* sdi_tuple)
{
  size_t tpl_sz;
  px** elems;
  
  is_valid = true;
  pure_is_tuplev(sdi_tuple, &tpl_sz, &elems);
  dict = get_sd_from_app(elems[0]);
  sdmap &mp = dict->mp;
  int num_iters = tpl_sz-1;
  if (num_iters != 0 && num_iters != 2) {
      is_valid = false;
      return;
  }
  if (num_iters == 0 || mp.size() == 0) {
    begin_it = mp.begin();
    end_it = mp.end();
    free(elems);
  }
  else {
    px* b = elems[1];
    px* e = elems[2];
    px* comp = dict->px_comp.pxp();    
    free(elems);
    begin_it = get_iter(mp, b, 0);
    end_it = get_iter(mp,e,1);
    if (begin_it == mp.end()) {
      end_it = begin_it;
      return;
    }
    sdi prec_it = end_it;
    if ( prec_it != mp.begin() && e != sdend() ) {
      prec_it--;
      px* pkey = prec_it->first.pxp();
      if ( !is_less(comp, pkey, e) ) end_it = prec_it;
    }    
    if (end_it != mp.end()) { 
      px* bkey = begin_it->first.pxp();
      px* ekey = end_it->first.pxp();
      if ( !is_less(comp, bkey, ekey) )
        begin_it = end_it = mp.end();
    }
  }
}

/*** Functions for map<pxh,pxh,pxh_pred2> *******************************/

sd* sd_make_empty(px* comp, int keys_only)
{
  sd* ret  = new sd(comp, keys_only);
  
#ifdef STL_DEBUG
  if (stl_sd_trace_enabled())
    cerr << "TRACE SD:    new sd*: " << ret << endl;
#endif
  return ret;
}

void sd_delete(sd* p){
#ifdef STL_DEBUG
  if (stl_sd_trace_enabled())
    cerr << "TRACE SD: delete sd*: " << p << endl;
#endif
  delete(p);
}

px* sd_make_vector(px* tpl) 
{
  sd_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  sdi b = itrs.beg();
  sdi e = itrs.end();
  int sz = sd_get_size(itrs.dict, b, e);
  if (!sz)
    return pure_matrix_columnsv(0,NULL);
  px** bfr = (px**)malloc(sizeof(px*)*sz);
  if (itrs.dict->keys_only) 
    transform(b, e, bfr, sd_pair_to_key);
  else
    transform(b, e, bfr, sd_pair_to_rocket);
  px* ret = pure_matrix_columnsv(sz, bfr);
  free(bfr);
  return ret;
}

sv* sd_make_stlvec(px* tpl) 
{
  sd_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  sdi b = itrs.beg();
  sdi e = itrs.end();
  sv* ret = new sv;
  if (itrs.dict->keys_only) 
    transform(b, e, back_inserter(*ret), sd_pair_to_key);
  else
    transform(b, e, back_inserter(*ret), sd_pair_to_rocket);
  return ret;
}

void sd_set_default(sd* dict, px* val)
{
  dict->dflt = val;
  dict->has_dflt = 1;
}

px* sd_get_default(sd* dict)
{
  int ok = 1;
  px* val;
  if (dict->has_dflt) 
    val = dict->dflt.pxp();  
  else {
    val = null_value();
    ok = 0;
  }
  return pure_tuplel(2, pure_int(ok), val);
}

int sd_size(px* tpl)
{
  sd_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  return sd_get_size(itrs.dict, itrs.beg(), itrs.end());
}

px* sd_bounds(px* tpl)
{
  px** elems;
  px *ub, *lb;
  size_t tpl_sz;
  pure_is_tuplev(tpl, &tpl_sz, &elems);
  sd* dict = get_sd_from_app(elems[0]);
  sdmap &mp = dict->mp;
  int num_iters = tpl_sz-1;
  switch (tpl_sz) {
  case 1:
    lb = sdbeg();
    ub = sdend();
    break;
  case 2:
    lb = iter_to_key( mp, get_iter(mp, elems[1], 0) );
    ub = iter_to_key( mp, get_iter(mp, elems[1], 1) );
    break;
  case 3:
    lb = iter_to_key( mp, get_iter(mp, elems[1], 0) ); 
    ub = iter_to_key( mp, get_iter(mp, elems[2], 1) );     
    break;
  }
  free(elems);
  return pure_tuplel(2,lb,ub);
}

int sd_member(sd* dict, px* key)
{
  int ret = 0;
  sdmap& mp = dict->mp;
  sdi i;
  if (!mp.empty()) {
    if (dict->get_cached_sdi(key, i) ) {
      ret = 1;
    }
    else {
      i = dict->find(key);
      if (i != mp.end()) {
        dict->cache_sdi(i);
        ret = 1;
      }
    }
  }
  return ret;
}

px* sd_prev(sd* dict, px* key)
{
  sdmap& mp = dict->mp;
  if (mp.empty()) index_error();
  sdi i = mp.end();
  if ( !dict->get_cached_sdi(key,i) )
    i = dict->find(key);
  if ( i == mp.begin() || i==mp.end() && key != sdend())
    index_error();
  else
    i--;
  dict->cache_sdi(i);
  return iter_to_key(mp, i);
}

px* sd_next(sd* dict, px* key)
{
  sdmap& mp = dict->mp;
  sdi i = mp.end();
  if (mp.empty()) index_error();
  if ( !dict->get_cached_sdi(key,i) )
    i = dict->find(key);
  if ( i == mp.end() )
    index_error();
  else
    i++;
  dict->cache_sdi(i);
  return iter_to_key(mp, i);
}

px* sd_get(sd* dict, px* key)
{
  sdmap &mp = dict->mp; 
  px* ret = 0;
  sdi i;
  if ( !dict->get_cached_sdi(key, i) ) 
    i = mp.find(key);
  if (i != mp.end()) {
    dict->cache_sdi(i);
    ret = dict->keys_only ? key : i->second.pxp();
  }
  else if (dict->has_dflt) {
    px* dv = dict->dflt.pxp();
    if (dict->keys_only) {
      if (dict->has_dflt)
        ret = dv;
      else
        index_error();
    }
    else {
      update_aux(dict, key, dv);
      ret = dv;
    }
  } 
  else {
        index_error();
  }
  return ret;
}

sd* sd_setop(int op, px* tpl1, px* tpl2)
{
  sd_iters itrs1(tpl1);
  sd_iters itrs2(tpl2);
  if (!itrs1.is_valid || !itrs2.is_valid) bad_argument;
  sd* dict = itrs1.dict;
  px* px_comp = dict->px_comp.pxp();
  sd* res = new sd(px_comp, dict->keys_only, dict->dflt.pxp());
  sdmap& mp = res->mp;
  try {
    switch (op) {
    case stl_sd_union:
      set_union(itrs1.beg(), itrs1.end(), 
                itrs2.beg(), itrs2.end(),
                inserter(mp, mp.end()), mp.value_comp());
      break;
    case stl_sd_difference:
      set_difference(itrs1.beg(), itrs1.end(),
                     itrs2.beg(), itrs2.end(),
                     inserter(mp, mp.end()), mp.value_comp());
      break;
    case stl_sd_intersection:
      set_intersection(itrs1.beg(), itrs1.end(),
                       itrs2.beg(), itrs2.end(),
                       inserter(mp, mp.end()), mp.value_comp());
      break;
    case stl_sd_symmetric_difference:
      set_symmetric_difference(itrs1.beg(), itrs1.end(),
                               itrs2.beg(), itrs2.end(),
                               inserter(mp, mp.end()), mp.value_comp());
      break;
    otherwise:
      bad_argument();
    }
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

void sd_update(sd* dict, px* key, px* val)
{
  if (dict->keys_only) val = null_value();
  update_aux(dict, key, val);
}

void sd_update_with(sd* dict, px* key, px* unaryfun)
{
  if (dict->keys_only) {
    update_aux(dict, key, null_value());
    return;
  }
  if (!dict->has_dflt)
    failed_cond();
  px* old_val = sd_get(dict,key);
  px* exception = 0;
  px* new_val = pure_appxl(unaryfun, &exception, 1, old_val);
  if (exception) pure_throw(exception);
  if (!new_val) bad_function();
  update_aux(dict, key, new_val);
}

px* sd_first(px* tpl)
{
  sd_iters itrs(tpl);
  if ( !itrs.is_valid ) bad_argument();
  sdi b = itrs.beg();
  if (b != itrs.end())
    return itrs.dict->keys_only ? sd_pair_to_key(*b) : sd_pair_to_rocket(*b);
  index_error();
}

px* sd_last(px* tpl)
{
  sd_iters itrs(tpl);
  if ( !itrs.is_valid ) bad_argument();
  sdi e = itrs.end();
  if (itrs.beg() != e--)
    return itrs.dict->keys_only ? sd_pair_to_key(*e) : sd_pair_to_rocket(*e);
  index_error();
}

void sd_rmfirst(px* tpl)
{
  sd_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  sdmap& m = itrs.dict->mp;
  if ( itrs.beg() != itrs.end() )
    m.erase(itrs.beg());
  else
    index_error();
}

void sd_rmlast(px* tpl)
{
  sd_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  sdmap& m = itrs.dict->mp;
  if ( itrs.beg() != itrs.end() )
    m.erase(--itrs.end());
  else
    index_error();
}

void sd_insert(sd* dict, px* kv)
{
  if ( !insert_aux(dict, kv) ) bad_argument();
}

void sd_insert_elms_xs(sd* dict, px* src)
{
  sdmap& mp = dict->mp;
  size_t sz = 0;
  px** elems = NULL;
  bool ok;
  if (pure_is_listv(src, &sz, &elems)) {
    for (int i = 0; i<sz; i++)
      if ( !insert_aux(dict, elems[i]) ) bad_argument();
    free(elems);
  } else if (matrix_type(src) == 0) {
    sz = matrix_size(src); 
    elems = (pure_expr**) pure_get_matrix_data(src);
    for (int i = 0; i<sz; i++) 
      if ( !insert_aux(dict, elems[i]) ) bad_argument();
  } 
}

void sd_insert_elms_stldict(sd* dict, px* tpl)
{
  sd_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  for (sdi i = itrs.beg(); i!=itrs.end(); i++)
    update_aux(dict, i->first.pxp(),i->second.pxp());
}

void sd_insert_elms_stlvec(sd* dict, px* tpl)
{
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  sdmap& mp = dict->mp;
  for (svi i = itrs.beg(); i!=itrs.end(); i++)
    if ( !insert_aux(dict, i->pxp()) ) bad_argument();
}

void sd_erase(px* tpl)
{
  sd_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  itrs.dict->mp.erase(itrs.beg(), itrs.end());
}

void sd_clear(sd* dict)
{
  dict->clear();
}

/* multi map version

void sd_remove(sd* dict, px* k)
{
  sdmap& mp = dict->mp;
  sdi i = mp.lower_bound(k);
  if (i!=mp.end()) {
    px* dk = i->first.pxp();
    px* cmp = dict->px_comp.pxp();
    if ( !is_less(cmp, dk, k) && !is_less(cmp, dk, k) )
      mp.erase(i);
  }
}

int sd_remove_all(sd* dict, px* k)
{
  return dict->mp.erase(k);
}

void sd_remove_kv(sd* dict, px* kv)
{
  sdmap& mp = dict->mp;
  size_t sz = 0;
  px *k, *v;
  if (dict->keys_only) {
    sd_remove(sd, kv);
  }
  if (rocket_to_pair(kv, &k, &v) ) {
    sdi b = mp.lower_bound(k);
    sdi e = mp.upper_bound(k);
    while(b++ != e) {
      px* dv = b->second.pxp();
      px* cmp = dict->px_comp.pxp();
      if (!is_less(cmp, v, dv) && !is_less(cmp, dv, v) )
        mp.erase(i);
    }
  } else 
    bad_argument();
}

*/

void sd_remove(sd* dict, px* k)
{
  dict->mp.erase(k);
}

int sd_remove_all(sd* dict, px* k)
{
  return dict->mp.erase(k);
}

void sd_remove_kv(sd* dict, px* kv)
{
  sdmap& mp = dict->mp;
  size_t sz = 0;
  px *k, *v;
  px* cmp = dict->px_comp.pxp();
  if (dict->keys_only) {
    sd_remove(dict, kv);
  }
  else if (rocket_to_pair(kv, &k, &v) ) {
    sdi i = mp.lower_bound(k);
    if (i != mp.end() ) {
      px* dv = i->second.pxp();
      if (!is_less(cmp, v, dv) && !is_less(cmp, dv, v) )
        mp.erase(i);
    }
  } 
  else 
    bad_argument();
}

bool sd_allpairs(px* comp, px* tpl1, px* tpl2)
{
  pxh_pair_pred2 fun(comp);
  sd_iters itrs1(tpl1);
  sd_iters itrs2(tpl2);
  if (!itrs1.is_valid) bad_argument();
  if (!itrs2.is_valid) bad_argument();
  try {
    return equal(itrs1.beg(), itrs1.end(), itrs2.beg(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

px* apply_fun(px* fun, int what, sdi i, px** exception) {
  px *key, *val, *pxi, *res = 0;
  int bfr;
  set_kv(i, &key, &val);
  if (what == stl_sd_both) {
    pxi = pair_to_rocket(key, val);  // bump k, v refc
    px_new(pxi);
   }
  else if (what == stl_sd_key)
    pxi = key; 
  else
    pxi = val;
  *exception = 0;
  if (!pure_is_int(fun, &bfr)) {
    res = pure_appxl(fun, exception, 1, pxi);
  }
  else {
    res = pxi;
  }
  if (what==stl_sd_both)
    px_unref(pxi);  
  return res;
}

// if fun is any int then it is treated as identity (i.e., ignored)
px* sd_listmap(px* fun, px* tpl, int what)
{
  sd_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  if (itrs.dict->keys_only) what = stl_sd_key;
  sdi b = itrs.beg(); 
  sdi e = itrs.end();
  bool xx = b == e;
  px* cons = pure_const(cons_tag());
  px* nl = pure_const(null_list_tag());
  px* res = nl;
  px* y = 0;
  px* exception;
  for (sdi i = b; i != e; i++){
    px* pxi = apply_fun(fun, what, i, &exception);
    if (exception) {
      if (res) px_freenew(res);
      if (pxi) px_freenew(pxi);
      pure_throw(exception);
    }
    px* last = pure_app(pure_app(cons,pxi),nl);
    if (res==nl)
      res = y = last;
    else {
      y->data.x[1] = px_new(last);
      y = last;
    }
  }
  return res;  
}

px* sd_listcatmap(px* fun, px* tpl, int what)
{
  sd_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  if (itrs.dict->keys_only) what = stl_sd_key;
  sdi b = itrs.beg(); 
  sdi e = itrs.end(); 
  px* cons = pure_const(cons_tag());
  px* nl = pure_const(null_list_tag());
  px* res = nl;
  px* y = 0;
  px* exception;
  px* *elms;
  size_t sz;
  for (sdi i = b; i != e; i++){
    px* pxi = apply_fun(fun, what, i, &exception);
    if (exception) {
      if (res) px_freenew(res);
      if (pxi) px_freenew(pxi);
      pure_throw(exception);
    }
    if ( !pure_is_listv(pxi, &sz, &elms) ){
      px_freenew(pxi);
      if (res) px_freenew(res);
      bad_argument();      
    }
    for (int j = 0; j < sz; j++) {
      px* last = pure_app(pure_app(cons,elms[j]),nl);
      if (res==nl)
        res = y = last;    
      else {
        y->data.x[1] = px_new(last);
        y = last;
      }
    }
    px_freenew(pxi);
    free(elms);
  }
  return res;  
}

static px* sd_foldl_itrs(px* fun, px* val, sdi beg, sdi end, int mode)
{ 
  px* res = px_new(val);
  px* exception = 0;
  for (sdi i = beg; i != end; i++){
    px* fun_v = pure_appl(fun, 1, res);
    px* fxy = apply_fun(fun_v, mode, i, &exception);
    if (exception) {
      px_unref(res);
      throw exception;
    }
    px_new(fxy);
    px_free(res);
    res = fxy;
  }
  px_unref(res);
  return res;
}

px* sd_foldl(px* fun, px* val, px* tpl)
{
  sd_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  int mode =  itrs.dict->keys_only ? stl_sd_key : stl_sd_both;
  try {
    return sd_foldl_itrs(fun, val, itrs.beg(), itrs.end(), mode);
  } catch (px* e) {
    pure_throw(e);
  }
}

px* sd_foldl1(px* fun, px* tpl)
{
  sd_iters itrs(tpl);
  if ( !itrs.is_valid ) bad_argument();
  int mode =  itrs.dict->keys_only ? stl_sd_key : stl_sd_both;
  if (itrs.beg() == itrs.end() ) return 0;
  sdi b = itrs.beg();
  px* val;
  if (mode == stl_sd_key)
    val = b->first.pxp();
  else
    val = pair_to_rocket(b->first.pxp(), b->second.pxp());
  try {
    return sd_foldl_itrs(fun, val, ++b, itrs.end(), mode);
  } catch (px* e) {
    pure_throw(e);
  }
}

static px* sd_foldr_itrs(px* fun, px* val, sdi beg, sdi end, int mode)
{ 
  px* res = px_new(val);
  px* exception = 0;
  sdi i = end;
  while (i-- != beg) {
    px* fun_v = apply_fun(fun, mode, i, &exception);
    px_ref(fun_v);
    px* fxy = pure_appxl(fun_v, &exception, 1, res);
    if (exception) {
      px_unref(res);
      throw exception;
    }
    px_unref(fun_v);
    px_new(fxy);
    px_free(res);
    res = fxy;
  }
  px_unref(res);
  return res;
}

px* sd_foldr(px* fun, px* val, px* tpl)
{
  sd_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  int mode = itrs.dict->keys_only ? stl_sd_key : stl_sd_both;
  try {
    return sd_foldr_itrs(fun, val, itrs.beg(), itrs.end(), mode);
  } catch (px* x) {
    pure_throw(x);
  }
}

px* sd_foldr1(px* fun, px* tpl)
{
  sd_iters itrs(tpl);
  if ( !itrs.is_valid ) bad_argument();
  int mode =  itrs.dict->keys_only ? stl_sd_key : stl_sd_both;
  if ( itrs.beg() == itrs.end() ) return 0;
  sdi e = --itrs.end();
  px* val;
  if (mode == stl_sd_key)
    val = e->first.pxp();
  else
    val = pair_to_rocket(e->first.pxp(), e->second.pxp());
  try {
    return sd_foldr_itrs(fun, val, itrs.beg(), e, mode);
  } catch (px* x) {
    pure_throw(x);
  }
}
