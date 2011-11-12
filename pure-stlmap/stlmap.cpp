/* stlmap.cpp -- C++ support for stlmap.pure

--- DRAFT - FOR DISCUSSON PURPOSES ONLY ---
    
Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlmap, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlmap distribution package for details.

*/

#include <stdlib.h>
#include <iostream>
#include <algorithm>
#include <numeric>
#include "stlmap.hpp"

using namespace std;

// #define STL_INSERT_SEMANTICS

/*** Helpers for debugging only ************************************/

static bool sm_trace_enabled = false;

void stl_set_sm_trace(bool enable) 
{
  sm_trace_enabled = enable;
}

bool stl_sm_trace_enabled()
{
  return sm_trace_enabled;
}

/*** Helpers for stlmap.cpp only ************************************/

static void set_kv(pmi i, px** k, px** v)
{
  const pxh h_key = i->first;
  pxh h_val = i->second;
  *k = h_key.pxp();
  *v = h_val.pxp();
}

static pxh_pair sm_rocket_to_pair(pxh rp)
{
  px *k, *v;
  bool ok = rocket_to_pair(rp.pxp(), &k, &v);
  return pxh_pair(k,v);
}

static px* sm_pair_to_rocket(const pxh_pair& kv)
{
  return pair_to_rocket(kv.first.pxp(), kv.second.pxp());
}

static px* sm_pair_to_key(const pxh_pair& kv)
{
  return kv.first.pxp();
}

static int sm_get_size(sm* smp, pmi b, pmi e)
{
 size_t sz = 0;
 pxhmap& mp = smp->mp;
 if (b == mp.begin() && e == mp.end())
    sz = mp.size();
 else if (b == e)
   sz = 0;
 else
   while(b++ != e) sz++;
 return sz;
}

static pmi update_aux(sm* smp, px* k, px* v)
{
  pxhmap& mp = smp->mp;
  pmi pos;
  if ( smp->get_cached_pmi(k, pos) ) {
    pos->second = v;
  }   
  else {
    pair<pmi,bool> i_ok = mp.insert(pxh_pair(k,v));
    if (!i_ok.second) i_ok.first->second = v;
    pos = i_ok.first;
   }
  return pos;
}

// increases inserted by 1 iff inserted new value
static bool insert_aux(sm* smp, px* kv, pmi& pos, int& inserted)
{
  px *k, *v;
  bool ok = true;
  if (smp->keys_only) {
    k = kv;
    v = NULL;
  } 
  else {
    if ( rocket_to_pair(kv, &k, &v) )
      ;
    else if (smp->has_dflt) {
      k = kv;
      v = smp->dflt.pxp();
    }
    else {
      k = kv;
      v = NULL;
      ok = false;
    }
  }
  if (ok) {
#ifdef STL_INSERT_SEMANTICS
    // Do NOT override existing values
    if ( !smp->get_cached_pmi(k, pos) ) {
      pair<pmi,bool> i_ok = smp->mp.insert(pxh_pair(k,v));
      pos = i_ok.first;
      inserted += i_ok.second;
    }
#else
    // Always use new values
    inserted++;
    pos = update_aux(smp, k, v); 
#endif
  }
  return ok;
}

// applies a pure function of one argument to element at *i
// assumes fun (k=>y) or f k.
static px* apply_fun(px* fun, int what, pmi i, px** exception) {
  px *key, *val, *pxi, *res = 0;
  int bfr;
  set_kv(i, &key, &val);
  if (what == stl_sm_both) {
    pxi = pair_to_rocket(key, val);  // bump k, v refc
    px_new(pxi);
   }
  else if (what == stl_sm_key)
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
  if (what==stl_sm_both)
    px_unref(pxi);  
  return res;
}

static px* listmap_aux(px* fun, pmi b, pmi e, int what) 
{
  bool xx = b == e;
  px* cons = px_cons_sym();
  px* nl = px_null_list_sym();
  px* res = nl;
  px* y = 0;
  px* exception;
  for (pmi i = b; i != e; i++){
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

/*** stlmap members  ***********************************************/

stlmap::stlmap(px* cmp, px* val_cmp, px* val_eql, bool keyonly):
  mp(pxh_pred2(cmp)), keys_only(keyonly),
  px_comp(cmp), px_val_comp(val_cmp), px_val_equal(val_eql),
  has_recent_pmi(0), latest_pmi_pos(0),
  has_dflt(0), dflt(NULL) {}

stlmap::stlmap(px* cmp, px* val_cmp, px* val_eql, bool keyonly, px *d):
  mp(pxh_pred2(cmp)), keys_only(keyonly),
  px_comp(cmp), px_val_comp(val_cmp), px_val_equal(val_eql),
  has_recent_pmi(0), latest_pmi_pos(0), 
  has_dflt(1), dflt(d) {}

pmi stlmap::find(px* key)
{
  pmi iter;
  if (key == smbeg())
    iter = mp.begin();
  else if (key == smend())
    iter = mp.end();
  else
    iter = mp.find(key);
  return iter;  
}

void stlmap::cache_pmi(const pmi& i)
{
  if ( i != mp.end() ) {
    if (!has_recent_pmi) {
      recent_pmi.clear();
      has_recent_pmi = true;
      latest_pmi_pos = 0;
    }
    size_t num_cached = recent_pmi.size();
    if (num_cached<SM_CACHE_SZ) {
      recent_pmi.push_back(i);
      latest_pmi_pos = num_cached;
    }
    else {
      latest_pmi_pos = (latest_pmi_pos + 1) % SM_CACHE_SZ;
      recent_pmi[latest_pmi_pos] = i;
    }
  }
}

bool stlmap::get_cached_pmi(px* k, pmi& i)
{
  bool ret = false;
  if ( k != smend() && has_recent_pmi ) {
    size_t num_cached = recent_pmi.size();
    size_t pos = 0; 
    for (; pos < num_cached; pos++) {
      if ( same(recent_pmi[pos]->first.pxp(), k) ) {
        i = recent_pmi[pos];
        ret = true;
         break;
      }
    }
  }
  //cerr << "get_cached_pmi, k: " << k << ", ret: " << ret << endl;
  return ret;
}

void stlmap::clear()
{
  has_recent_pmi = false;
  mp.clear();
}

int stlmap::erase(pmi pos)
{
  if (has_recent_pmi) {
    vector<pmi>::iterator end = 
      remove(recent_pmi.begin(),recent_pmi.end(),pos);
    recent_pmi.erase(end,recent_pmi.end());
  }
  mp.erase(pos);
  return 1;
}

int stlmap::erase(px* k)
{
  int ret = 0;
  if ( !mp.empty() ) {
    ret = 1;
    pmi i;
    if ( get_cached_pmi(k, i) )
      erase(i);
    else {
      if (k == smbeg())
        erase(mp.begin());
      else if (k != smend()) {
        ret = mp.erase(k);
      }
      else
        ret = 0;
    }
  }
  return ret;
}

int stlmap::erase(pmi first, pmi last)
{
  has_recent_pmi = false;
  size_t sz = mp.size();
  mp.erase(first, last);
  return sz - mp.size();
}

/*** sm_range functions *********************************************/

enum {gi_find, gi_lower, gi_upper};

static pmi get_iter(pxhmap& mp , px* key, int mode)
{
  pmi iter;
  if (key == smbeg())
    iter = mp.begin();
  else if (key == smend())
    iter = mp.end();
  else {
    if (mode==gi_upper) 
      iter = mp.upper_bound(key);
    else if (mode==gi_lower)
      iter = mp.lower_bound(key);
    else 
      iter = mp.find(key);
  }
  return iter;  
}

static px* iter_to_key(const pxhmap& mp, const pmi& it)
{
  if (it == mp.end()) return smend();
  if (it == mp.begin()) return smbeg();
  return it->first.pxp();
}

static sm* get_sm_from_app(px* app)
{
  void* ret = 0;
  px* fun;
  size_t argc;
  px** args;
  pure_is_appv(app, &fun, &argc, &args);
  if (argc == 1 && !pure_is_pointer(args[0], &ret))
    ret = 0;
  free(args);
  return (sm*)ret; 
}

sm_range::sm_range(px* pmi_tuple)
{
  size_t tpl_sz;
  px** elems;  
  is_valid = true;
  pure_is_tuplev(pmi_tuple, &tpl_sz, &elems);
  smp = get_sm_from_app(elems[0]);
  pxhmap &mp = smp->mp;
  pxhmap::key_compare k_cmp = mp.key_comp();
  num_iters = tpl_sz-1;
  pmi et;
  if (num_iters > 2) {
    is_valid = false;
    goto done;
  }
  if (num_iters == 0 || mp.size() == 0) {
    begin_it = mp.begin();
    end_it = mp.end();
    num_iters = 2;
    goto done;
  }
  {
    px* b = elems[1];
    px* e = num_iters == 2 ? elems[2] : b;
    begin_it = get_iter(mp, b, gi_lower);
    if (b == smbeg()) b = begin_it->first.pxp();
    if (num_iters == 1) {
      if ( begin_it == mp.end() ||
           k_cmp(b, begin_it->first) || 
           k_cmp(begin_it->first,b)) {
        begin_it = end_it = mp.end(); 
        goto done;
      }  
      end_it = begin_it;
      while (++end_it != mp.end())
        if ( k_cmp(b, end_it->first) ) break;
      goto done;
    }
    else if ( begin_it == mp.end() || e == smend()) {
      end_it = mp.end();
      goto done;
    }
    pmi i = get_iter(mp, e, gi_upper); // upper to high
    while (i != mp.begin() ) {
      pmi prev = i; prev--;
      if ( k_cmp(prev->first,e) ) break;
      i = prev;
    }
    if (i == mp.begin())
      end_it = begin_it;
    else
      end_it = i;
  }
 done:
  free(elems);
  if (end_it != mp.end() && k_cmp(end_it->first, begin_it->first) )
    end_it = begin_it;
}

/*** Pure interface support functions  *************************************/

static px* add_pointer_tag(sm* smp)
{
  bool ko = smp->keys_only;
  px* ptr = pure_tag(ko?stlset_tag():stlmap_tag(), pure_pointer(smp));
#ifdef STL_DEBUG
  if (stl_sm_trace_enabled())
    cerr << "TRACE SM:    new sm*: " << smp << endl;
#endif
  return ptr;
}

px* sm_make_empty(px* comp, px* v_comp, px* v_eql, int keys_only)
{
  return add_pointer_tag( new sm(comp, v_comp, v_eql, keys_only) );
}

void sm_delete(sm* p){
#ifdef STL_DEBUG
  if (stl_sm_trace_enabled())
    cerr << "TRACE SM: delete sm*: " << p << endl;
#endif
  delete(p);
}

bool sm_is_set(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return rng.smp->keys_only;
}

int sm_less(px* tpl1, px* tpl2)
{
  sm_range rng1(tpl1);
  sm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument;
  try {
    return lexicographical_compare(rng1.beg(), rng1.end(),
                                   rng2.beg(), rng2.end(), 
                                   rng1.smp->mp.value_comp());
  }
  catch (px* e) {
    pure_throw(e);
  }
}

bool sm_equal(px* tpl1, px* tpl2)
{
  sm_range rng1(tpl1);
  sm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument;
  if (sm_size(tpl1) != sm_size(tpl2)) return 0;
  sm* smp = rng1.smp;
  try {
    if (smp->keys_only) {
      pxh_pair_first_equal comp(smp->px_val_equal.pxp());   
      return equal(rng1.beg(), rng1.end(), rng2.beg(), comp);
    }
    else {
      pxh_pair_equivalent comp(smp->px_comp.pxp(),smp->px_val_equal.pxp());   
      return equal(rng1.beg(), rng1.end(), rng2.beg(), comp);
    }
  }
  catch (px* e) {
    pure_throw(e);
  }
}

px* sm_make_vector(px* tpl) 
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  pmi b = rng.beg();
  pmi e = rng.end();
  int sz = sm_get_size(rng.smp, b, e);
  if (!sz)
    return pure_matrix_columnsv(0,NULL);
  px** bfr = (px**)malloc(sizeof(px*)*sz);
  if (rng.smp->keys_only) 
    transform(b, e, bfr, sm_pair_to_key);
  else
    transform(b, e, bfr, sm_pair_to_rocket);
  px* ret = pure_matrix_columnsv(sz, bfr);
  free(bfr);
  return ret;
}

sv* sm_make_stlvec(px* tpl) 
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  pmi b = rng.beg();
  pmi e = rng.end();
  sv* ret = new sv;
  if (rng.smp->keys_only) 
    transform(b, e, back_inserter(*ret), sm_pair_to_key);
  else
    transform(b, e, back_inserter(*ret), sm_pair_to_rocket);
  return ret;
}

px* sm_set_default(sm* smp, px* val)
{
  if (smp->keys_only) return 0; // fail
  smp->dflt = val;
  smp->has_dflt = 1;
  return val;
}

px* sm_get_default(sm* smp)
{
  if (smp->keys_only) return 0; // fail
  int ok = 1;
  px* val;
  if (smp->has_dflt) 
    val = smp->dflt.pxp();  
  else {
    val = NULL;
    ok = 0;
  }
  return pure_tuplel(2, pure_int(ok), val);
}

int sm_size(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return sm_get_size(rng.smp, rng.beg(), rng.end());
}

px* sm_bounds(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument;
  pxhmap& mp = rng.smp->mp;
  return pure_tuplel(2,iter_to_key(mp, rng.beg()),
                       iter_to_key(mp, rng.end())); 
}

int sm_member(sm* smp, px* key)
{
  int ret = 0;
  pxhmap& mp = smp->mp;
  pmi i;
  if (!mp.empty()) {
    if (smp->get_cached_pmi(key, i) ) {
      ret = 1;
    }
    else {
      i = smp->find(key);
      if (i != mp.end()) {
        smp->cache_pmi(i);
        ret = 1;
      }
    }
  }
  return ret;
}

px* sm_prev_key(sm* smp, px* key)
{
  pxhmap& mp = smp->mp;
  if (mp.empty()) index_error();
  pmi i = mp.end();
  if ( !smp->get_cached_pmi(key,i) )
    i = smp->find(key);
  if ( i == mp.begin() || i==mp.end() && key != smend() )
    index_error();
  else
    i--;
  smp->cache_pmi(i);
  return iter_to_key(mp, i);
}

px* sm_next_key(sm* smp, px* key)
{
  pxhmap& mp = smp->mp;
  pmi i = mp.end();
  if (mp.empty()) index_error();
  if ( !smp->get_cached_pmi(key,i) )
    i = smp->find(key);
  if ( i != mp.end() ) i++;
  smp->cache_pmi(i);
  return iter_to_key(mp, i);
}

px* sm_get_elm(sm* smp, px* key, int what)
{
  pxhmap &mp = smp->mp; 
  px* ret = 0;
  pmi i;
  if ( !smp->get_cached_pmi(key, i) ) {
    i = get_iter(mp, key, gi_find);  
    smp->cache_pmi(i);
  }
  if (i != mp.end()) {
    switch (what) {
    case stl_sm_key:
      ret = i->first.pxp();
      break;
    case stl_sm_val:
      ret = smp->keys_only ? pure_int(1) : i->second.pxp();
      break;
    case stl_sm_both:
      ret = smp->keys_only ? i->first.pxp() : sm_pair_to_rocket(*i);
    }
  }
  else {
    switch (what) {
    case stl_sm_key:
      ret = smend();
      break;
    case stl_sm_val:
      if (smp->keys_only) 
        ret = pure_int(0);
      else if (smp->has_dflt)
        ret = smp->dflt.pxp();
      else
        index_error();
      break;
    case stl_sm_both:
      if (smp->keys_only) 
        ret = pure_int(0);
      else if (smp->has_dflt)
        ret = pair_to_rocket(i->first.pxp(), smp->dflt.pxp());
      else
        index_error();
      break;
      return smp->keys_only ? i->first.pxp(): sm_pair_to_rocket(*i);
    }
  }
  return ret;
}

bool sm_includes(px* tpl1, px* tpl2)
{
  sm_range rng1(tpl1);
  sm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument;
  try {
    return includes(rng1.beg(), rng1.end(), rng2.beg(), rng2.end(), 
                    rng1.smp->mp.value_comp());
  }
  catch (px* e) {
    pure_throw(e);
  }
}

px* sm_setop(int op, px* tpl1, px* tpl2)
{
  sm_range rng1(tpl1);
  sm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument;
#ifndef STL_INSERT_SEMANTICS
  if (op != stl_sm_difference) {
    sm_range temp = rng2;
    rng2 = rng1;
    rng1 = temp;
  }
#endif
  sm* smp = rng1.smp;
  sm* trg = new sm(smp->px_comp.pxp(),
                   smp->px_val_comp.pxp(),
                   smp->px_val_equal.pxp(),
                   smp->keys_only, smp->dflt.pxp());
  pxhmap& mp = trg->mp;
  try {
    switch (op) {
    case stl_sm_merge:
      merge(rng1.beg(), rng1.end(), 
            rng2.beg(), rng2.end(),
            inserter(mp, mp.end()), mp.value_comp());
      break;
    case stl_sm_union:
      set_union(rng1.beg(), rng1.end(), 
                rng2.beg(), rng2.end(),
                inserter(mp, mp.end()), mp.value_comp());
      break;
    case stl_sm_difference:
      set_difference(rng1.beg(), rng1.end(),
                     rng2.beg(), rng2.end(),
                     inserter(mp, mp.end()), mp.value_comp());
      break;
    case stl_sm_intersection:
      set_intersection(rng1.beg(), rng1.end(),
                       rng2.beg(), rng2.end(),
                       inserter(mp, mp.end()), mp.value_comp());
      break;
    case stl_sm_symmetric_difference:
      set_symmetric_difference(rng1.beg(), rng1.end(),
                               rng2.beg(), rng2.end(),
                               inserter(mp, mp.end()), mp.value_comp());
      break;
    default:
      bad_argument();
    }
    return add_pointer_tag(trg);
  } catch (px* e) {
    pure_throw(e);
  }
}

px* sm_update(sm* smp, px* key, px* val)
{
  if (smp->keys_only) return 0; // fail for sets
  pmi pos = update_aux(smp, key, val);
  smp->cache_pmi(pos);
  return val;
}

px* sm_update_with(sm* smp, px* key, px* unaryfun)
{
  if (smp->keys_only) return 0; // fail for sets
  if (!smp->has_dflt) failed_cond();
  pmi i;
  if ( !smp->get_cached_pmi(key, i) ) {
    // uses default only if not already stored
    pair<pmi,bool> i_ok = smp->mp.insert(pxh_pair(key,smp->dflt.pxp()));
    i = i_ok.first;
    smp->cache_pmi(i);
  }  
  px* old_val = i->second.pxp();
  px* exception = 0;
  px* new_val = pure_appxl(unaryfun, &exception, 1, old_val);
  if (exception) pure_throw(exception);
  if (!new_val) bad_function();
  i->second = new_val;
  return new_val;
}

int sm_insert_elm(sm* smp, px* kv)
{
  int num_inserted = 0;
  pmi pos;
  if ( !insert_aux(smp, kv, pos, num_inserted) ) bad_argument();
  smp->cache_pmi(pos);
  return num_inserted;
}

int sm_insert_elms_xs(sm* smp, px* src)
{
  pxhmap& mp = smp->mp;
  size_t sz = 0;
  px** elems = NULL;
  bool ok;
  int num_inserted = 0;
  pmi pos;
  if (pure_is_listv(src, &sz, &elems)) {
    for (int i = 0; i<sz; i++)
      if ( !insert_aux(smp, elems[i], pos, num_inserted) ) bad_argument();
    free(elems);
  } else if (matrix_type(src) == 0) {
    sz = matrix_size(src); 
    elems = (pure_expr**) pure_get_matrix_data(src);
    for (int i = 0; i<sz; i++) 
      if ( !insert_aux(smp, elems[i], pos, num_inserted) ) bad_argument();
  } 
  return num_inserted;
}

int sm_insert_elms_stlmap(sm* smp, px* tpl)
{
  sm_range rng(tpl);
  pxhmap& mp = smp->mp;
  size_t oldsz = mp.size();
  if (smp == rng.smp) bad_argument();
  if (rng.beg() != rng.end()) {
    if (!rng.is_valid) bad_argument();
    for (pmi i = rng.beg(); i!=rng.end(); i++)
      update_aux(smp, i->first.pxp(), i->second.pxp());
  }
  return mp.size() - oldsz;
}

int sm_insert_elms_stlvec(sm* smp, px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  int num_inserted = 0;
  pxhmap& mp = smp->mp;
  pmi pos;
  for (svi i = rng.beg(); i!=rng.end(); i++)
    if ( !insert_aux(smp, i->pxp(), pos, num_inserted) ) bad_argument();
  return num_inserted;
}

int sm_erase(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return rng.smp->erase(rng.beg(), rng.end());
}

int sm_erase_if(px* pred, px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  sm* smp = rng.smp;
  pxhmap &mp = smp->mp;
  size_t sz = mp.size();
  int mode = smp->keys_only ? stl_sm_key : stl_sm_both;
  px* exception;
  int fun_res = 0;
  pmi i = rng.beg();
  pmi end = rng.end();
  while (i!=end) {
    pmi trg = i++;
    px* pxres = apply_fun(pred, mode, trg, &exception);
    if (exception) pure_throw(exception);
    if ( pure_is_int(pxres, &fun_res) && fun_res ) {
      smp->erase(trg);
     }   
  }
  return sz - mp.size();
}

int sm_clear(sm* smp)
{
  size_t sz = smp->mp.size();
  smp->clear();
  return sz;
}

int sm_remove(sm* smp, px* k)
{
  return smp->erase(k);
}

int sm_remove_if(sm* smp, px* k, px* pred)
{
  int ret = 0;
  pxhmap &mp = smp->mp;
  pmi i;
  pxh_pred1 fun(pred);
  if (!smp->keys_only) {
    try {
      if ( !smp->get_cached_pmi(k, i) ) 
        i = get_iter(mp, k, gi_find);  
      if ( i != mp.end() && fun(i->second) ) {
        smp->erase(i);
        ret = 1;
      }
      else
        ret = 0;
    } catch (px* e) {
      pure_throw(e);
    }
  }
  return ret;
}

px* sm_listmap(px* fun, px* tpl, int what)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  if (rng.smp->keys_only) what = stl_sm_key;
  return listmap_aux(fun, rng.beg(), rng.end(), what);
}

px* sm_listcatmap(px* fun, px* tpl, int what)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  if (rng.smp->keys_only) what = stl_sm_key;
  pmi b = rng.beg(); 
  pmi e = rng.end(); 
  px* cons = px_cons_sym();
  px* nl = px_null_list_sym();
  px* res = nl;
  px* y = 0;
  px* exception;
  px* *elms;
  size_t sz;
  for (pmi i = b; i != e; i++){
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

static px* sm_foldl_rng(px* fun, px* val, pmi i, pmi end, int mode)
{ 
  px* res = px_new(val);
  px* exception = 0;
  while (i != end){
    pmi trg = i++;
    px* fun_v = pure_appl(fun, 1, res);
    px* fxy = apply_fun(fun_v, mode, trg, &exception);
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

px* sm_foldl(px* fun, px* val, px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  int mode =  rng.smp->keys_only ? stl_sm_key : stl_sm_both;
  try {
    return sm_foldl_rng(fun, val, rng.beg(), rng.end(), mode);
  } catch (px* e) {
    pure_throw(e);
  }
}

px* sm_foldl1(px* fun, px* tpl)
{
  sm_range rng(tpl);
  if ( !rng.is_valid ) bad_argument();
  int mode =  rng.smp->keys_only ? stl_sm_key : stl_sm_both;
  if (rng.beg() == rng.end() ) return 0;
  pmi b = rng.beg();
  px* val;
  if (mode == stl_sm_key)
    val = b->first.pxp();
  else
    val = pair_to_rocket(b->first.pxp(), b->second.pxp());
  try {
    return sm_foldl_rng(fun, val, ++b, rng.end(), mode);
  } catch (px* e) {
    pure_throw(e);
  }
}

static px* sm_foldr_rng(px* fun, px* val, pmi beg, pmi end, int mode)
{ 
  px* res = px_new(val);
  px* exception = 0;
  pmi i = end;
  while (i != beg) {
    pmi trg = --i;
    px* fun_v = apply_fun(fun, mode, trg, &exception);
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

px* sm_foldr(px* fun, px* val, px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  int mode = rng.smp->keys_only ? stl_sm_key : stl_sm_both;
  try {
    return sm_foldr_rng(fun, val, rng.beg(), rng.end(), mode);
  } catch (px* x) {
    pure_throw(x);
  }
}

px* sm_foldr1(px* fun, px* tpl)
{
  sm_range rng(tpl);
  if ( !rng.is_valid ) bad_argument();
  int mode =  rng.smp->keys_only ? stl_sm_key : stl_sm_both;
  if ( rng.beg() == rng.end() ) return 0;
  pmi e = --rng.end();
  px* val;
  if (mode == stl_sm_key)
    val = e->first.pxp();
  else
    val = pair_to_rocket(e->first.pxp(), e->second.pxp());
  try {
    return sm_foldr_rng(fun, val, rng.beg(), e, mode);
  } catch (px* x) {
    pure_throw(x);
  }
}
