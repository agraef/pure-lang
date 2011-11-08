/* stlmmap.cpp -- C++ support for stlmmap.pure

--- DRAFT - FOR DISCUSSON PURPOSES ONLY ---
    
Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlmap, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/)
.

This software is distributed under a BSD-style license in the hope 
//- ()

hat it will be useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
COPYING file included with th e pure-stlmap distribution package for details.

*/

#include <stdlib.h>
#include <iostream>
#include <algorithm>
#include <numeric>
#include "stlmmap.hpp"

using namespace std;

/*** Helpers for debugging only ************************************/

static bool smm_trace_enabled = false;

void stl_set_smm_trace(bool enable) 
{
  smm_trace_enabled = enable;
}

bool stl_smm_trace_enabled()
{
  return smm_trace_enabled;
}

/*** Helpers for stlmmap.cpp only ************************************/

static void set_kv(pmmi i, px** k, px** v)
{
  const pxh h_key = i->first;
  pxh h_val = i->second;
  *k = h_key.pxp();
  *v = h_val.pxp();
}

static pxh_pair smm_rocket_to_pair(pxh rp)
{
  px *k, *v;
  bool ok = rocket_to_pair(rp.pxp(), &k, &v);
  return pxh_pair(k,v);
}

static px* smm_pair_to_rocket(const pxh_pair& kv)
{
  return pair_to_rocket(kv.first.pxp(), kv.second.pxp());
}

static px* smm_pair_to_key(const pxh_pair& kv)
{
  return kv.first.pxp();
}

static int smm_get_size(smm* smmp, pmmi b, pmmi e)
{
 size_t sz = 0;
 pxhmmap& mp = smmp->mp;
 if (b == mp.begin() && e == mp.end())
    sz = mp.size();
 else if (b == e)
   sz = 0;
 else
   while(b++ != e) sz++;
 return sz;
}

static void update_aux(smm* smmp, px* k, px* v)
{
  pxhmmap& mp = smmp->mp;
  pmmi pos;
  if ( smmp->keys_only && smmp->has_recent_pmmi ) {
    pos = smmp->recent_pmmi;
    if ( !same(pos->first.pxp(), k) )
      pos = smmp->mp.insert(smmp->recent_pmmi,pxh_pair(k,v));
    pos->second = v;
  }   
  else {
    pos = mp.insert(pxh_pair(k,v));
  }
  if (smmp->keys_only)
    smmp->cache_pmmi(pos);  
}

static bool insert_aux(smm* smmp, px* kv)
{
  px *k, *v;
  bool ok = true;
  if (smmp->keys_only) {
    k = kv;
    v = NULL;
  } 
  else {
    if ( rocket_to_pair(kv, &k, &v) )
      ;
    else if (smmp->has_dflt) {
      k = kv;
      v = smmp->dflt.pxp();
    }
    else {
      k = kv;
      v = NULL;
      ok = false;
    }
  }
  if (ok) {
    pmmi pos;
    if (smmp->has_recent_pmmi)
      pos = smmp->mp.insert(smmp->recent_pmmi,pxh_pair(k,v));
    else {
      pos = smmp->mp.insert(pxh_pair(k,v));
    }
    if (smmp->keys_only)
      smmp->cache_pmmi(pos);
  }
  return ok;
}

static px* apply_fun(px* fun, int what, pmmi i, px** exception) {
  px *key, *val, *pxi, *res = 0;
  int bfr;
  set_kv(i, &key, &val);
  if (what == stl_smm_both) {
    pxi = pair_to_rocket(key, val);  // bump k, v refc
    px_new(pxi);
  }
  else if (what == stl_smm_key)
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
  if (what==stl_smm_both)
    px_unref(pxi);  
  return res;
}

// if fun is any int then it is treated as identity (i.e., ignored)
static px* listmap_aux(px* fun, pmmi b, pmmi e, int what) 
{
  bool xx = b == e;
  px* cons = pure_const(cons_tag());
  px* nl = pure_const(null_list_tag());
  px* res = nl;
  px* y = 0;
  px* exception;
  for (pmmi i = b; i != e; i++){
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

/*** stlmmap members  ***********************************************/

stlmmap::stlmmap(px* cmp, px* val_cmp, px* val_eql, bool keyonly):
  mp(pxh_pred2(cmp)), keys_only(keyonly),
  px_comp(cmp), px_val_comp(val_cmp), px_val_equal(val_eql),
  has_recent_pmmi(0), recent_pmmi(mp.end()), 
  has_dflt(0), dflt(NULL) {}

stlmmap::stlmmap(px* cmp, px* val_cmp, px* val_eql, bool keyonly, px *d):
  mp(pxh_pred2(cmp)), keys_only(keyonly),
  px_comp(cmp), px_val_comp(val_cmp), px_val_equal(val_eql),
  has_recent_pmmi(0), recent_pmmi(mp.end()), 
  has_dflt(1), dflt(d) {}

pmmi stlmmap::find(px* key)
{
  pmmi iter;
  if (key == smmbeg())
    iter = mp.begin();
  else if (key == smmend())
    iter = mp.end();
  else
    iter = mp.find(key);
  return iter;  
}

bool stlmmap::get_cached_pmmi(px* k, pmmi& i)
{
  bool found = k != smmbeg() && k != smmend() &&
    has_recent_pmmi && same(recent_pmmi->first.pxp(), k);
  if (found) i = recent_pmmi;
#ifdef STL_DEBUG
  if (found && stl_smm_trace_enabled())
    cerr <<"get_cached_pmmi, found " << k <<" with refc:"<< k->refc << endl;
#endif
  return found;
}

void stlmmap::cache_pmmi(pmmi i)
{
  if ( i != mp.end() ) {
    has_recent_pmmi = true;
    recent_pmmi = i;
  }
}

void stlmmap::clear()
{
  has_recent_pmmi = 0;
  mp.clear();
}

int stlmmap::erase(pmmi pos)
{
  if (has_recent_pmmi && recent_pmmi == pos)
    has_recent_pmmi = 0;
  mp.erase(pos);
  return 1;
}

int stlmmap::erase(px* k)
{
  int ret = 0;
  if ( !mp.empty() ) {
    ret = 1;
    pmmi i;
    if ( get_cached_pmmi(k, i) )
      erase(i);
    else {
      if (k == smmbeg())
        erase(mp.begin());
      else if (k != smmend()) {
        ret = mp.erase(k);
      }
      else
        ret = 0;
    }
  }
  return ret;
}

int stlmmap::erase(pmmi first, pmmi last)
{
  has_recent_pmmi = false;
  size_t sz = mp.size();
  mp.erase(first, last);
  return sz - mp.size();
}

/*** smm_iters functions *********************************************/

enum {gi_find, gi_lower, gi_upper};

static pmmi get_iter(pxhmmap& mp , px* key, int mode)
{
  pmmi iter;
  if (key == smmbeg())
    iter = mp.begin();
  else if (key == smmend())
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

static px* iter_to_key(const pxhmmap& mp, const pmmi& it)
{
  if (it == mp.end()) return smmend();
  if (it == mp.begin()) return smmbeg();
  return it->first.pxp();
}

static smm* get_smm_from_app(px* app)
{
  void* ret = 0;
  px* fun;
  size_t argc;
  px** args;
  pure_is_appv(app, &fun, &argc, &args);
  if (argc == 1 && !pure_is_pointer(args[0], &ret))
    ret = 0;
  free(args);
  return (smm*)ret; 
}

// sets two iterators [beg, end)
// one key goes to "k, k++)
// if first >= last then use  mp.end(), mp.end()
smm_iters::smm_iters(px* pmmi_tuple)
{
  size_t tpl_sz;
  px** elems;  
  is_valid = true;
  pure_is_tuplev(pmmi_tuple, &tpl_sz, &elems);
  smmp = get_smm_from_app(elems[0]);
  pxhmmap &mp = smmp->mp;
  pxhmmap::key_compare k_cmp = mp.key_comp();
  num_iters = tpl_sz-1;
  pmmi et;
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
    if (b == smmbeg()) b = begin_it->first.pxp();
    if (num_iters == 1) {
      if ( begin_it == mp.end() || 
           k_cmp(b, begin_it->first) ||
           k_cmp(begin_it->first,b) ){
        begin_it = end_it = mp.end(); 
        goto done;
      }        
      end_it = begin_it;
      while (++end_it != mp.end())
        if ( k_cmp(b, end_it->first) ) break;
      goto done;
    }
    if ( begin_it == mp.end() || e == smmend() ) {
      end_it = mp.end();
      goto done;
    }
    pmmi i = get_iter(mp, e, gi_upper); // upper to high
    while (i != mp.begin() ) {
      pmmi prev = i; prev--;
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
    begin_it=end_it;
}

/*** Functions for multimap<pxh,pxh,pxh_pred2> *******************************/

smm* smm_make_empty(px* comp, px* v_comp, px* v_eql, int keys_only)
{
  smm* ret  = new smm(comp, v_comp, v_eql, keys_only);
#ifdef STL_DEBUG
  if (stl_smm_trace_enabled())
    cerr << "TRACE SD:    new smm*: " << (void*)ret << endl;
#endif
  return ret;
}

void smm_delete(smm* p){
#ifdef STL_DEBUG
  if (stl_smm_trace_enabled())
    cerr << "TRACE SD: delete smm*: " << p << endl;
#endif
  delete(p);
}

bool smm_is_set(px* tpl)
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  return itrs.smmp->keys_only;
}

int smm_compare(px* tpl1, px* tpl2)
{
  smm_iters itrs1(tpl1);
  smm_iters itrs2(tpl2);
  if (!itrs1.is_valid || !itrs2.is_valid) bad_argument;
  pxh_pred2 kless = itrs1.smmp->px_comp.pxp();
  pxh_pred2 vless = itrs1.smmp->px_val_comp.pxp();
  bool have_vals = !itrs1.smmp->keys_only;
  pmmi beg1 = itrs1.beg();
  pmmi end1 = itrs1.end();
  pmmi beg2 = itrs2.beg();
  pmmi end2 = itrs2.end();
  try {
    for ( ; (beg1 != end1) && (beg2 != end2); beg1++, beg2++ ) {
        if (kless(beg1->first,beg2->first)) return -1;
        if (kless(beg2->first,beg1->first)) return  1;
        if (have_vals) {
          if (vless(beg1->second,beg2->second)) return -1;
          if (vless(beg2->second,beg1->second)) return  1;
        }
    }
    if (beg1 == end1)
      return (beg2 == end2) ? 0 : -1;
    else
      return 1;
  }
  catch (px* e) {
    pure_throw(e);
  }
}

bool smm_equal(px* tpl1, px* tpl2)
{
  smm_iters itrs1(tpl1);
  smm_iters itrs2(tpl2);
  if (!itrs1.is_valid || !itrs2.is_valid) bad_argument;
  if ( smm_size(tpl1) != smm_size(tpl2) ) return false;
  pxh_pred2 kless = itrs1.smmp->px_comp.pxp();
  pxh_pred2 vequal = itrs1.smmp->px_val_equal.pxp();
  bool have_vals = !itrs1.smmp->keys_only;
  pmmi beg1 = itrs1.beg();
  pmmi end1 = itrs1.end();
  pmmi beg2 = itrs2.beg();
  pmmi end2 = itrs2.end();
  try {
    for ( ; (beg1 != end1) && (beg2 != end2); beg1++, beg2++ ) {
        if (kless(beg1->first,beg2->first)) return false;
        if (kless(beg2->first,beg1->first)) return false;
        if (have_vals)
          if (!vequal(beg1->second,beg2->second)) return false;
    }
    return beg1 == end1 && beg2 == end2;
  }
  catch (px* e) {
    pure_throw(e);
  }
}

px* smm_make_vector(px* tpl) 
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  pmmi b = itrs.beg();
  pmmi e = itrs.end();
  int sz = smm_get_size(itrs.smmp, b, e);
  if (!sz)
    return pure_matrix_columnsv(0,NULL);
  px** bfr = (px**)malloc(sizeof(px*)*sz);
  if (itrs.smmp->keys_only) 
    transform(b, e, bfr, smm_pair_to_key);
  else
    transform(b, e, bfr, smm_pair_to_rocket);
  px* ret = pure_matrix_columnsv(sz, bfr);
  free(bfr);
  return ret;
}

sv* smm_make_stlvec(px* tpl) 
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  pmmi b = itrs.beg();
  pmmi e = itrs.end();
  sv* ret = new sv;
  if (itrs.smmp->keys_only) 
    transform(b, e, back_inserter(*ret), smm_pair_to_key);
  else
    transform(b, e, back_inserter(*ret), smm_pair_to_rocket);
  return ret;
}

px* smm_set_default(smm* smmp, px* val)
{
  if (smmp->keys_only) return 0; // fail
  smmp->dflt = val;
  smmp->has_dflt = 1;
  return val;
}

px* smm_get_default(smm* smmp)
{
  if (smmp->keys_only) return 0; // fail
  int ok = 1;
  px* val;
  if (smmp->has_dflt) 
    val = smmp->dflt.pxp();  
  else {
    val = NULL;
    ok = 0;
  }
  return pure_tuplel(2, pure_int(ok), val);
}

int smm_size(px* tpl)
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  return smm_get_size(itrs.smmp, itrs.beg(), itrs.end());
}

px* smm_bounds(px* tpl)
{
  px** elems;
  px *ub, *lb;
  size_t tpl_sz;
  pure_is_tuplev(tpl, &tpl_sz, &elems);
  smm* smmp = get_smm_from_app(elems[0]);
  pxhmmap &mp = smmp->mp;
  int num_iters = tpl_sz-1;
  switch (tpl_sz) {
  case 1:
    lb = smmbeg();
    ub = smmend();
    break;
  case 2:
    lb = iter_to_key( mp, get_iter(mp, elems[1], gi_lower) );
    ub = iter_to_key( mp, get_iter(mp, elems[1], gi_upper) );
    break;
  case 3:
    lb = iter_to_key( mp, get_iter(mp, elems[1], gi_lower) ); 
    ub = iter_to_key( mp, get_iter(mp, elems[2], gi_upper) );     
    break;
  }
  free(elems);
  return pure_tuplel(2,lb,ub);
}

int smm_member(smm* smmp, px* key)
{
  int ret = 0;
  pxhmmap& mp = smmp->mp;
  pmmi i;
  if (!mp.empty()) {
    if (smmp->get_cached_pmmi(key, i) ) {
      ret = 1;
    }
    else {
      i = smmp->find(key);
      if (i != mp.end()) {
        smmp->cache_pmmi(i);
        ret = 1;
      }
    }
  }
  return ret;
}

px* smm_prev(smm* smmp, px* key)
{
  pxhmmap& mp = smmp->mp;
  if (mp.empty()) index_error(); 
  pmmi i = mp.end();
  if ( !smmp->get_cached_pmmi(key,i) )
    i = get_iter(mp,key,gi_find);
  if ( i == mp.begin() ) index_error();
  pxhmmap::value_compare kc = mp.value_comp();
  pmmi prev;
  if (i == mp.end()) {
    prev = --i;
  }
  else {
    while (i != mp.begin() ) {
      prev = i; prev--;
      if (kc(*prev,*i) ) break;
      i--;
    }
    if (i == mp.begin()) failed_cond();
  }
  smmp->cache_pmmi(prev);
  return iter_to_key(mp, prev);
}

px* smm_next(smm* smmp, px* key)
{
  pxhmmap& mp = smmp->mp;
  if (mp.empty()) index_error(); 
  pmmi i = mp.end();
  if ( !smmp->get_cached_pmmi(key,i) )
    i = get_iter(mp,key,gi_find);
  if ( i == mp.end() ) index_error();
  pxhmmap::value_compare kc = mp.value_comp();
  while (true) {
    pmmi prev = i++;
    if (i==mp.end() || kc(*prev,*i) ) break;
  }
  smmp->cache_pmmi(i);
  return iter_to_key(mp, i);
}

px* smm_get(smm* smmp, px* key)
{
  pxhmmap &mp = smmp->mp; 
  px* ret = 0;
  pmmi beg;
  if ( !smmp->get_cached_pmmi(key, beg) ) 
    beg = get_iter(mp, key, gi_find);
  if (beg != mp.end()) {
    pair<pmmi,pmmi> lb_ub = mp.equal_range(beg->first);
    beg = lb_ub.first;
    pmmi end = lb_ub.second;
    smmp->cache_pmmi(beg);
    if (smmp->keys_only) {
      ret = pure_int( smm_get_size(smmp, beg, end) );
    } else {
      ret = listmap_aux(pure_int(0), beg, end, stl_smm_val);
    }
  }
  else if (smmp->keys_only)
    ret =  pure_int(0);
  else if (smmp->has_dflt)
    ret = smmp->dflt.pxp();
  else 
    index_error();
  return ret;
}

bool smm_includes(px* tpl1, px* tpl2)
{
  smm_iters itrs1(tpl1);
  smm_iters itrs2(tpl2);
  if (!itrs1.is_valid || !itrs2.is_valid) bad_argument;
  try {
    return includes(itrs1.beg(), itrs1.end(), itrs2.beg(), itrs2.end(), 
                    itrs1.smmp->mp.value_comp());
  }
  catch (px* e) {
    pure_throw(e);
  }
}

smm* smm_setop(int op, px* tpl1, px* tpl2)
{
  smm_iters itrs1(tpl1);
  smm_iters itrs2(tpl2);
  if (!itrs1.is_valid || !itrs2.is_valid) bad_argument;
  smm* smmp = itrs1.smmp;
  smm* res = new smm(smmp->px_comp.pxp(),
                     smmp->px_val_comp.pxp(),
                     smmp->px_val_equal.pxp(),
                     smmp->keys_only, smmp->dflt.pxp());
  pxhmmap& mp = res->mp;
  try {
    switch (op) {
    case stl_smm_union:
      set_union(itrs1.beg(), itrs1.end(), 
                itrs2.beg(), itrs2.end(),
                inserter(mp, mp.end()), mp.value_comp());
      break;
    case stl_smm_difference:
      set_difference(itrs1.beg(), itrs1.end(),
                     itrs2.beg(), itrs2.end(),
                     inserter(mp, mp.end()), mp.value_comp());
      break;
    case stl_smm_intersection:
      set_intersection(itrs1.beg(), itrs1.end(),
                       itrs2.beg(), itrs2.end(),
                       inserter(mp, mp.end()), mp.value_comp());
      break;
    case stl_smm_symmetric_difference:
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

// FIX ME
px* smm_update(smm* smmp, px* key, px* val)
{
  if (smmp->keys_only) return 0; // fail for sets
  update_aux(smmp, key, val);
  return val;
}

px* smm_first(px* tpl)
{
  smm_iters itrs(tpl);
  if ( !itrs.is_valid ) bad_argument();
  pmmi b = itrs.beg();
  if (b != itrs.end())
    return itrs.smmp->keys_only ? smm_pair_to_key(*b) : smm_pair_to_rocket(*b);
  index_error();
}

px* smm_last(px* tpl)
{
  smm_iters itrs(tpl);
  if ( !itrs.is_valid ) bad_argument();
  pmmi e = itrs.end();
  if (itrs.beg() != e--)
    return itrs.smmp->keys_only ? smm_pair_to_key(*e) : smm_pair_to_rocket(*e);
  index_error();
}

void smm_rmfirst(px* tpl)
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  if ( itrs.beg() != itrs.end() )
    itrs.smmp->erase(itrs.beg());
  else
    index_error();
}

void smm_rmlast(px* tpl)
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  if ( itrs.beg() != itrs.end() )
    itrs.smmp->erase(--itrs.end());
  else
    index_error();
}

void smm_insert_elm(smm* smmp, px* kv)
{
  if ( !insert_aux(smmp, kv) ) bad_argument();
}

void smm_insert_elms_xs(smm* smmp, px* src)
{
  pxhmmap& mp = smmp->mp;
  size_t sz = 0;
  px** elems = NULL;
  bool ok;
  if (pure_is_listv(src, &sz, &elems)) {
    for (int i = 0; i<sz; i++)
      if ( !insert_aux(smmp, elems[i]) ) bad_argument();
    free(elems);
  } else if (matrix_type(src) == 0) {
    sz = matrix_size(src); 
    elems = (pure_expr**) pure_get_matrix_data(src);
    for (int i = 0; i<sz; i++) 
      if ( !insert_aux(smmp, elems[i]) ) bad_argument();
  } 
}

void smm_insert_elms_stlmmap(smm* smmp, px* tpl)
{
  smm_iters itrs(tpl);
  if (itrs.beg() != itrs.end()) {
    pmmi inserted = smmp->mp.begin();
    if (!itrs.is_valid) bad_argument();
    if (smmp->keys_only) {
      for (pmmi i = itrs.beg(); i!=itrs.end(); i++)
        inserted = smmp->mp.insert(inserted, *i);
      smmp->cache_pmmi(inserted);
    }
    else {
      for (pmmi i = itrs.beg(); i!=itrs.end(); i++)
        update_aux(smmp, i->first.pxp(), i->second.pxp());
    }
  }
}

void smm_insert_elms_stlvec(smm* smmp, px* tpl)
{
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  pxhmmap& mp = smmp->mp;
  for (svi i = itrs.beg(); i!=itrs.end(); i++)
    if ( !insert_aux(smmp, i->pxp()) ) bad_argument();
}

px* smm_update_vals_xs(smm* smmp, px* k, px* src)
{
  if (smmp->keys_only) return 0;
  pxhmmap& mp = smmp->mp;
  pmmi trgi = get_iter(mp, k, gi_lower);
  pmmi ub = get_iter(mp, k, gi_upper);  
  size_t src_sz = 0;
  px** elems = NULL;
  bool ok;
  int i;
  if (pure_is_listv(src, &src_sz, &elems)) {
    for (i = 0; i<src_sz && trgi != ub; i++, trgi++)
      trgi->second = elems[i];
    if (i <src_sz)
      for (; i<src_sz; i++)
        update_aux(smmp, k, elems[i]);
    else
      mp.erase(trgi,ub);
    free(elems);
  } 
  else if (matrix_type(src) == 0) {
    src_sz = matrix_size(src); 
    elems = (pure_expr**) pure_get_matrix_data(src);
    for (i = 0; i<src_sz && trgi != ub; i++, trgi++)
      trgi->second = elems[i];
    if (i <src_sz)
      for (; i<src_sz; i++)
        update_aux(smmp, k, elems[i]);
    else
      mp.erase(trgi,ub);
  }
  return pure_int(i);
}

// FIX
// void smm_replace_vals_stlvec(smm* smmp, px* tpl)
// {
//   sv_iters itrs(tpl);
//   if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
//   pxhmmap& mp = smmp->mp;
//   for (svi i = itrs.beg(); i!=itrs.end(); i++)
//     if ( !insert_aux(smmp, i->pxp()) ) bad_argument();
// }

int smm_erase(px* tpl)
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  return itrs.smmp->erase(itrs.beg(), itrs.end());
}

int smm_erase_first(px* tpl)
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  if ( itrs.beg() != itrs.end() )
    itrs.smmp->erase(itrs.beg());
}

static int smm_erase_if_aux(px* pred, px* tpl, bool first_only)
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  smm* smmp = itrs.smmp;
  pxhmmap &mp = smmp->mp;
  size_t sz = mp.size();
  int mode = smmp->keys_only ? stl_smm_key : stl_smm_both;
  px* exception;
  int fun_res = 0;
  pmmi i = itrs.beg();
  pmmi end = itrs.end();
  while (i!=end) {
    pmmi trg = i++;
    px* pxres = apply_fun(pred, mode, trg, &exception);
    if (exception) pure_throw(exception);
    if ( pure_is_int(pxres, &fun_res) && fun_res ) {
      smmp->erase(trg);
      if (first_only) break;
     } 
  }
  return sz - mp.size();
}

int smm_erase_if(px* pred, px* tpl)
{
  return smm_erase_if_aux(pred, tpl, 0);
}

int smm_erase_first_if(px* pred, px* tpl)
{
  return smm_erase_if_aux(pred, tpl, 1);
}

int smm_clear(smm* smmp)
{
  size_t sz = smmp->mp.size();
  smmp->clear();
  return sz;
}

int smm_remove(smm* smmp, px* k, int all)
{
  int ret = 1;
  if (all) {
    ret = smmp->erase(k);
  }
  else {
    pxhmmap &mp = smmp->mp;
    pmmi i;
    if ( !smmp->get_cached_pmmi(k, i) ) 
      i = get_iter(mp, k, gi_find);  
    if (i != mp.end())
      smmp->erase(i);
    else
      ret = 0;
  }
}

int smm_remove_if(smm* smmp, px* k, px* pred, int all)
{
  int ret = 0;
  pxhmmap &mp = smmp->mp;
  pxh_pred1 fun(pred);
  pair<pmmi,pmmi> lb_ub = mp.equal_range(k);
  pmmi i = lb_ub.first;
  pmmi end = lb_ub.second;
  try {
    while (i != end) {
      pmmi trg = i;
      if ( fun(i++->second) ) {
        smmp->erase(trg);
        ret++;
        if (!all) break;
      }
    }
  } catch (px* e) {
    pure_throw(e);
  }
  return ret;
}

bool smm_allpairs(px* comp, px* tpl1, px* tpl2)
{
  bool all_ok;
  smm_iters itrs1(tpl1);
  smm_iters itrs2(tpl2);
  if (!itrs1.is_valid) bad_argument();
  if (!itrs2.is_valid) bad_argument();

  smm* smmp1 = itrs1.smmp;
  smm* smmp2 = itrs1.smmp;
  try {
    if (smmp1->keys_only) {
      if (!smmp2->keys_only) bad_argument();
      pxh_pred2 fun(comp);
      pmmi i1 = itrs1.beg();
      pmmi i2 = itrs2.beg();
      pmmi end1 = itrs1.end();
      pmmi end2  =itrs2.end();
      all_ok = 1;
      while(i1 != end1) {
        if (i2 == end2  || !fun(i1->first, i2->first) ) {
          all_ok = 0;
          break;
        }
        i1++; i2 ++;
      }
      if (all_ok && i2 != end2) 
        all_ok = 0;
    }
    else {
      if (smmp2->keys_only) bad_argument();
      if ( smm_size(tpl1) != smm_size(tpl2) ) {
        all_ok = 0;
      }
      else {
        pxh_pair_pred2 fun(comp);
        all_ok = equal(itrs1.beg(), itrs1.end(), itrs2.beg(), fun);
      }
    }
  } catch (px* e) {
    pure_throw(e);
  }
  return all_ok;
}

px* smm_listmap(px* fun, px* tpl, int what)
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  if (itrs.smmp->keys_only) what = stl_smm_key;
  return listmap_aux(fun, itrs.beg(), itrs.end(), what);
}

px* smm_listcatmap(px* fun, px* tpl, int what)
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  if (itrs.smmp->keys_only) what = stl_smm_key;
  pmmi b = itrs.beg(); 
  pmmi e = itrs.end(); 
  px* cons = pure_const(cons_tag());
  px* nl = pure_const(null_list_tag());
  px* res = nl;
  px* y = 0;
  px* exception;
  px* *elms;
  size_t sz;
  for (pmmi i = b; i != e; i++){
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

static px* smm_foldl_itrs(px* fun, px* val, pmmi i, pmmi end, int mode)
{ 
  px* res = px_new(val);
  px* exception = 0;
  while (i != end){
    pmmi trg = i++;
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

px* smm_foldl(px* fun, px* val, px* tpl)
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  int mode =  itrs.smmp->keys_only ? stl_smm_key : stl_smm_both;
  try {
    return smm_foldl_itrs(fun, val, itrs.beg(), itrs.end(), mode);
  } catch (px* e) {
    pure_throw(e);
  }
}

px* smm_foldl1(px* fun, px* tpl)
{
  smm_iters itrs(tpl);
  if ( !itrs.is_valid ) bad_argument();
  int mode =  itrs.smmp->keys_only ? stl_smm_key : stl_smm_both;
  if (itrs.beg() == itrs.end() ) return 0;
  pmmi b = itrs.beg();
  px* val;
  if (mode == stl_smm_key)
    val = b->first.pxp();
  else
    val = pair_to_rocket(b->first.pxp(), b->second.pxp());
  try {
    return smm_foldl_itrs(fun, val, ++b, itrs.end(), mode);
  } catch (px* e) {
    pure_throw(e);
  }
}

static px* smm_foldr_itrs(px* fun, px* val, pmmi beg, pmmi end, int mode)
{ 
  px* res = px_new(val);
  px* exception = 0;
  pmmi i = end;
  while (i != beg) {
    pmmi trg = --i;
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

px* smm_foldr(px* fun, px* val, px* tpl)
{
  smm_iters itrs(tpl);
  if (!itrs.is_valid) bad_argument();
  int mode = itrs.smmp->keys_only ? stl_smm_key : stl_smm_both;
  try {
    return smm_foldr_itrs(fun, val, itrs.beg(), itrs.end(), mode);
  } catch (px* x) {
    pure_throw(x);
  }
}

px* smm_foldr1(px* fun, px* tpl)
{
  smm_iters itrs(tpl);
  if ( !itrs.is_valid ) bad_argument();
  int mode =  itrs.smmp->keys_only ? stl_smm_key : stl_smm_both;
  if ( itrs.beg() == itrs.end() ) return 0;
  pmmi e = --itrs.end();
  px* val;
  if (mode == stl_smm_key)
    val = e->first.pxp();
  else
    val = pair_to_rocket(e->first.pxp(), e->second.pxp());
  try {
    return smm_foldr_itrs(fun, val, itrs.beg(), e, mode);
  } catch (px* x) {
    pure_throw(x);
  }
}
