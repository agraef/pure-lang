/* stlmmap.cpp -- C++ support for stlmmap.pure

--- DRAFT - FOR DISCUSSON PURPOSES ONLY ---
    
Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlmap, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/)
.

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with th e purse-stlmap distribution package for details.

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
 pxhmmap& mmp = smmp->mmp;
 if (b == mmp.begin() && e == mmp.end())
    sz = mmp.size();
 else if (b == e)
   sz = 0;
 else
   while(b++ != e) sz++;
 return sz;
}

static pmmi update_aux(smm* smmp, px* k, px* v)
{
  pxhmmap& mmp = smmp->mmp;
  return mmp.insert(pxh_pair(k,v));
}

// increases inserted by 1 iff inserted new value
static bool insert_aux(smm* smmp, px* kv, pmmi& pos, int& inserted)
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
    pos = smmp->mmp.insert(pxh_pair(k,v));
    inserted ++;
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
  px* cons = px_cons_sym();
  px* nl = px_null_list_sym();
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
  mmp(pxh_pred2(cmp)), keys_only(keyonly),
  px_comp(cmp), px_val_comp(val_cmp), px_val_equal(val_eql),
  has_recent_pmmi(0), latest_pmmi_pos(0),
  has_dflt(0), dflt(NULL) {}

stlmmap::stlmmap(px* cmp, px* val_cmp, px* val_eql, bool keyonly, px *d):
  mmp(pxh_pred2(cmp)), keys_only(keyonly),
  px_comp(cmp), px_val_comp(val_cmp), px_val_equal(val_eql),
  has_recent_pmmi(0),  latest_pmmi_pos(0),
  has_dflt(1), dflt(d) {}

pmmi stlmmap::find(px* key)
{
  pmmi iter;
  if (key == smmbeg())
    iter = mmp.begin();
  else if (key == smmend())
    iter = mmp.end();
  else
    iter = mmp.find(key);
  return iter;  
}

void stlmmap::cache_pmmi(pmmi& i)
{
  if ( i != mmp.end() ) {
    if (!has_recent_pmmi) {
      recent_pmmi.clear();
      has_recent_pmmi = true;
      latest_pmmi_pos = 0;
    }
    size_t num_cached = recent_pmmi.size();
    if (num_cached<SMM_CACHE_SZ) {
      recent_pmmi.push_back(i);
      latest_pmmi_pos = num_cached;
    }
    else {
      latest_pmmi_pos = (latest_pmmi_pos + 1) % SMM_CACHE_SZ;
      recent_pmmi[latest_pmmi_pos] = i;
    }
  }
}

bool stlmmap::get_cached_pmmi(px* k, pmmi& i)
{
  bool ret = false;
  if ( k != smmend() && has_recent_pmmi ) {
    size_t num_cached = recent_pmmi.size();
    size_t pos = 0; 
    for (; pos < num_cached; pos++) {
      if ( same(recent_pmmi[pos]->first.pxp(), k) ) {
        i = recent_pmmi[pos];
        ret = true;
         break;
      }
    }
  }
  //cerr << "get_cached_pmmi, k: " << k << ", ret: " << ret << endl;
  return ret;
}

void stlmmap::clear()
{
  has_recent_pmmi = 0;
  mmp.clear();
}

int stlmmap::erase(pmmi pos)
{
  if (has_recent_pmmi) {
    vector<pmmi>::iterator end = 
      remove(recent_pmmi.begin(),recent_pmmi.end(),pos);
    recent_pmmi.erase(end,recent_pmmi.end());
  }
  mmp.erase(pos);
  return 1;
}

int stlmmap::erase(px* k)
{
  int ret = 0;
  if ( !mmp.empty() ) {
    ret = 1;
    pmmi i;
    if ( get_cached_pmmi(k, i) )
      erase(i);
    else {
      if (k == smmbeg())
        erase(mmp.begin());
      else if (k != smmend()) {
        ret = mmp.erase(k);
      }
      else
        ret = 0;
    }
  }
  has_recent_pmmi = false;
  return ret;
}

int stlmmap::erase(pmmi first, pmmi last)
{
  size_t sz = mmp.size();
  mmp.erase(first, last);
  has_recent_pmmi = false;
  return sz - mmp.size();
}

/*** smm_range functions *********************************************/

enum {gi_find, gi_lower, gi_upper};

static pmmi get_iter(pxhmmap& mmp , px* key, int mode)
{
  pmmi iter;
  if (key == smmbeg())
    iter = mmp.begin();
  else if (key == smmend())
    iter = mmp.end();
  else {
    if (mode==gi_upper) 
      iter = mmp.upper_bound(key);
    else if (mode==gi_lower)
      iter = mmp.lower_bound(key);
    else 
      iter = mmp.find(key);
  }
  return iter;  
}

static px* iter_to_key(const pxhmmap& mmp, const pmmi& it)
{
  if (it == mmp.end()) return smmend();
  if (it == mmp.begin()) return smmbeg();
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
// if first >= last then use  mmp.end(), mmp.end()
smm_range::smm_range(px* pmmi_tuple)
{
  size_t tpl_sz;
  px** elems;  
  is_valid = true;
  pure_is_tuplev(pmmi_tuple, &tpl_sz, &elems);
  smmp = get_smm_from_app(elems[0]);
  pxhmmap &mmp = smmp->mmp;
  pxhmmap::key_compare k_cmp = mmp.key_comp();
  num_iters = tpl_sz-1;
  pmmi et;
  if (num_iters > 2) {
    is_valid = false;
    goto done;
  }
  if (num_iters == 0 || mmp.size() == 0) {
    begin_it = mmp.begin();
    end_it = mmp.end();
    num_iters = 2;
    goto done;
  }
  {
    px* b = elems[1];
    px* e = num_iters == 2 ? elems[2] : b;
    begin_it = get_iter(mmp, b, gi_lower);
    if (b == smmbeg()) b = begin_it->first.pxp();
    if (num_iters == 1) {
      if ( begin_it == mmp.end() || 
           k_cmp(b, begin_it->first) ||
           k_cmp(begin_it->first,b) ){
        begin_it = end_it = mmp.end(); 
        goto done;
      }        
      end_it = begin_it;
      while (++end_it != mmp.end())
        if ( k_cmp(b, end_it->first) ) break;
      goto done;
    }
    if ( begin_it == mmp.end() || e == smmend() ) {
      end_it = mmp.end();
      goto done;
    }
    pmmi i = get_iter(mmp, e, gi_upper); // upper to high
    while (i != mmp.begin() ) {
      pmmi prev = i; prev--;
      if ( k_cmp(prev->first,e) ) break;
      i = prev;
    }
    if (i == mmp.begin())
      end_it = begin_it;
    else
      end_it = i;
  }
 done:
  free(elems);
  if (end_it != mmp.end() && k_cmp(end_it->first, begin_it->first) )
    begin_it=end_it;
}

/*** Functions for multimap<pxh,pxh,pxh_pred2> *******************************/

static px* add_pointer_tag(smm* smmp)
{
  bool ko = smmp->keys_only;
  px* ptr = pure_tag(ko?stlmset_tag():stlmmap_tag(), pure_pointer(smmp));
#ifdef STL_DEBUG
  if (stl_sm_trace_enabled())
    cerr << "TRACE SMM:    new smm*: " << smmp << endl;
#endif
  return ptr;
}

px* smm_make_empty(px* comp, px* v_comp, px* v_eql, int keys_only)
{
  return add_pointer_tag( new smm(comp, v_comp, v_eql, keys_only) );
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
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return rng.smmp->keys_only;
}

int smm_less(px* tpl1, px* tpl2)
{
  smm_range rng1(tpl1);
  smm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument;
  smm* smmp = rng1.smmp;
  try {
    if (smmp->keys_only) {
      return lexicographical_compare(rng1.beg(), rng1.end(),
                                     rng2.beg(), rng2.end(), 
                                     smmp->mmp.value_comp());
    }
    else {
      pxh_pair_less comp(smmp->px_comp.pxp(),smmp->px_val_comp.pxp());   
      return lexicographical_compare(rng1.beg(), rng1.end(),
                                     rng2.beg(), rng2.end(), 
                                     comp);
    }
  }
  catch (px* e) {
    pure_throw(e);
  }
}

bool smm_equal(px* tpl1, px* tpl2)
{
  smm_range rng1(tpl1);
  smm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument;
  if (smm_size(tpl1) != smm_size(tpl2)) return 0;
  smm* smmp = rng1.smmp;
  try {
    if (smmp->keys_only) {
      pxh_pair_first_equal comp(smmp->px_val_equal.pxp());   
      return equal(rng1.beg(), rng1.end(), rng2.beg(), comp);
    }
    else {
      pxh_pair_equivalent comp(smmp->px_comp.pxp(),smmp->px_val_equal.pxp());   
      return equal(rng1.beg(), rng1.end(), rng2.beg(), comp);
    }
  }
  catch (px* e) {
    pure_throw(e);
  }
}

px* smm_make_vector(px* tpl) 
{
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  pmmi b = rng.beg();
  pmmi e = rng.end();
  int sz = smm_get_size(rng.smmp, b, e);
  if (!sz)
    return pure_matrix_columnsv(0,NULL);
  px** bfr = (px**)malloc(sizeof(px*)*sz);
  if (rng.smmp->keys_only) 
    transform(b, e, bfr, smm_pair_to_key);
  else
    transform(b, e, bfr, smm_pair_to_rocket);
  px* ret = pure_matrix_columnsv(sz, bfr);
  free(bfr);
  return ret;
}

sv* smm_make_stlvec(px* tpl) 
{
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  pmmi b = rng.beg();
  pmmi e = rng.end();
  sv* ret = new sv;
  if (rng.smmp->keys_only) 
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
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return smm_get_size(rng.smmp, rng.beg(), rng.end());
}

px* smm_bounds(px* tpl)
{
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument;
  pxhmmap& mmp = rng.smmp->mmp;
  return pure_tuplel(2,iter_to_key(mmp, rng.beg()),
                       iter_to_key(mmp, rng.end())); 
}

int smm_member(smm* smmp, px* key)
{
  int ret = 0;
  pxhmmap& mmp = smmp->mmp;
  pmmi i;
  if (!mmp.empty()) {
    if (smmp->get_cached_pmmi(key, i) ) {
      ret = 1;
    }
    else {
      i = smmp->find(key);
      if (i != mmp.end()) {
        smmp->cache_pmmi(i);
        ret = 1;
      }
    }
  }
  return ret;
}

px* smm_prev_key(smm* smmp, px* key)
{
  pxhmmap& mmp = smmp->mmp;
  if (mmp.empty()) index_error(); 
  pmmi i = mmp.end();
  if ( !smmp->get_cached_pmmi(key,i) )
    i = get_iter(mmp,key,gi_find);
  if ( i == mmp.begin() ) index_error();
  pxhmmap::value_compare kc = mmp.value_comp();
  pmmi prev;
  if (i == mmp.end()) {
    prev = --i;
  }
  else {
    while (i != mmp.begin() ) {
      prev = i; prev--;
      if (kc(*prev,*i) ) break;
      i--;
    }
    if (i == mmp.begin()) failed_cond();
  }
  smmp->cache_pmmi(prev);
  return iter_to_key(mmp, prev);
}

px* smm_next_key(smm* smmp, px* key)
{
  pxhmmap& mmp = smmp->mmp;
  if (mmp.empty()) index_error(); 
  pmmi i = mmp.end();
  if ( !smmp->get_cached_pmmi(key,i) )
    i = get_iter(mmp,key,gi_find);
  if ( i == mmp.end() ) index_error();
  pxhmmap::value_compare kc = mmp.value_comp();
  while (true) {
    pmmi prev = i++;
    if (i==mmp.end() || kc(*prev,*i) ) break;
  }
  smmp->cache_pmmi(i);
  return iter_to_key(mmp, i);
}


px* smm_get_elm(smm* smmp, px* key, int what)
{
  pxhmmap &mmp = smmp->mmp; 
  px* ret = 0;
  pmmi i;
  if ( !smmp->get_cached_pmmi(key, i) ) 
    i = get_iter(mmp, key, gi_find);  
  if (i != mmp.end()) {
    pair<pmmi,pmmi> lb_ub = mmp.equal_range(i->first);
    pmmi beg = lb_ub.first;
    pmmi end = lb_ub.second;
    smmp->cache_pmmi(beg);
    if (what==stl_smm_key)
      ret = i->first.pxp();
    else if (smmp->keys_only)
      ret = pure_int( smm_get_size(smmp, beg, end) );
    else 
      ret = listmap_aux(pure_int(0), beg, end, what);
  }
  else {
    switch (what) {
    case stl_smm_key:
      ret = smmend();
      break;
    case stl_smm_val:
    case stl_smm_both:
     if (smmp->keys_only) 
        ret = pure_int(0);
      else
        ret = pure_listl(0);
      break;
    }
  }
  return ret;
}

bool smm_includes(px* tpl1, px* tpl2)
{
  smm_range rng1(tpl1);
  smm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument;
  smm* smmp = rng1.smmp;
  try {
    if (smmp->keys_only) {
      return includes(rng1.beg(), rng1.end(),
                      rng2.beg(), rng2.end(), smmp->mmp.value_comp());
    }
    else {
      pxh_pair_less comp(smmp->px_comp.pxp(),smmp->px_val_comp.pxp());   
      return includes(rng1.beg(), rng1.end(),
                      rng2.beg(), rng2.end(), comp);
    }
  }
  catch (px* e) {
    pure_throw(e);
  }
}

px* smm_setop(int op, px* tpl1, px* tpl2)
{
  smm_range rng1(tpl1);
  smm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument;
  smm* smmp = rng1.smmp;
  smm* trg = new smm(smmp->px_comp.pxp(),
                     smmp->px_val_comp.pxp(),
                     smmp->px_val_equal.pxp(),
                     smmp->keys_only, smmp->dflt.pxp());
  pxhmmap& mmp = trg->mmp;
  try {
    if (trg->keys_only) {
      pxhmmap::value_compare comp = mmp.value_comp();
      switch (op) {
      case stl_smm_merge:
        merge(rng1.beg(), rng1.end(), 
              rng2.beg(), rng2.end(),
              inserter(mmp, mmp.end()), comp);
        break;
      case stl_smm_union:
        set_union(rng1.beg(), rng1.end(), 
                  rng2.beg(), rng2.end(),
                  inserter(mmp, mmp.end()), comp);
        break;
      case stl_smm_difference:
        set_difference(rng1.beg(), rng1.end(),
                       rng2.beg(), rng2.end(),
                       inserter(mmp, mmp.end()), comp);
        break;
      case stl_smm_intersection:
        set_intersection(rng1.beg(), rng1.end(),
                         rng2.beg(), rng2.end(),
                         inserter(mmp, mmp.end()), comp);
        break;
      case stl_smm_symmetric_difference:
        set_symmetric_difference(rng1.beg(), rng1.end(),
                                 rng2.beg(), rng2.end(),
                                 inserter(mmp, mmp.end()), comp);
        break;
      default:
        bad_argument();
      }
    }
    else {
      pxh_pair_less comp(trg->px_comp.pxp(),trg->px_val_comp.pxp());
      switch (op) {
      case stl_smm_merge:
        merge(rng1.beg(), rng1.end(), 
              rng2.beg(), rng2.end(),
              inserter(mmp, mmp.end()), comp);
        break;
      case stl_smm_union:
        set_union(rng1.beg(), rng1.end(), 
                  rng2.beg(), rng2.end(),
                  inserter(mmp, mmp.end()), comp);
        break;
      case stl_smm_difference:
        set_difference(rng1.beg(), rng1.end(),
                       rng2.beg(), rng2.end(),
                       inserter(mmp, mmp.end()), comp);
        break;
      case stl_smm_intersection:
        set_intersection(rng1.beg(), rng1.end(),
                         rng2.beg(), rng2.end(),
                         inserter(mmp, mmp.end()), comp);
        break;
      case stl_smm_symmetric_difference:
        set_symmetric_difference(rng1.beg(), rng1.end(),
                                 rng2.beg(), rng2.end(),
                                 inserter(mmp, mmp.end()), comp);
        break;
      default:
        bad_argument();
      }
    }
    return add_pointer_tag(trg);
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

int smm_insert_elm(smm* smmp, px* kv)
{
  int num_inserted = 0;
  pmmi pos;
  if ( !insert_aux(smmp, kv, pos, num_inserted) ) bad_argument();
  smmp->cache_pmmi(pos);
  return num_inserted;
}

int smm_insert_elms_xs(smm* smmp, px* src)
{
  pxhmmap& mmp = smmp->mmp;
  size_t sz = 0;
  px** elems = NULL;
  bool ok;
  int num_inserted = 0;
  pmmi pos;
  if (pure_is_listv(src, &sz, &elems)) {
    for (int i = 0; i<sz; i++)
      if ( !insert_aux(smmp, elems[i], pos, num_inserted) ) bad_argument();
    free(elems);
  } else if (matrix_type(src) == 0) {
    sz = matrix_size(src); 
    elems = (pure_expr**) pure_get_matrix_data(src);
    for (int i = 0; i<sz; i++) 
      if ( !insert_aux(smmp, elems[i], pos, num_inserted) ) bad_argument();
  } 
  return num_inserted;
}

int smm_insert_elms_stlmmap(smm* smmp, px* tpl)
{
  smm_range rng(tpl);
  pxhmmap& mmp = smmp->mmp;
  size_t oldsz = mmp.size();
  if (smmp == rng.smmp) bad_argument();
  if (rng.beg() != rng.end()) {
    if (!rng.is_valid) bad_argument();
    for (pmmi i = rng.beg(); i!=rng.end(); i++)
      update_aux(smmp, i->first.pxp(), i->second.pxp());
  }
  return mmp.size() - oldsz;
}

int smm_insert_elms_stlvec(smm* smmp, px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  int num_inserted = 0;
  pxhmmap& mmp = smmp->mmp;
  pmmi pos;
  for (svi i = rng.beg(); i!=rng.end(); i++)
    if ( !insert_aux(smmp, i->pxp(), pos, num_inserted) ) bad_argument();
  return num_inserted;
}

px* smm_update_vals_xs(smm* smmp, px* k, px* src)
{
  if (smmp->keys_only) return 0;
  pxhmmap& mmp = smmp->mmp;
  pmmi trgi = get_iter(mmp, k, gi_lower);
  pmmi ub = get_iter(mmp, k, gi_upper);  
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
      smmp->erase(trgi,ub);
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
      smmp->erase(trgi,ub);
  }
  return pure_int(i);
}

int smm_erase(px* tpl)
{
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return rng.smmp->erase(rng.beg(), rng.end());
}

int smm_erase_first(px* tpl)
{
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  if ( rng.beg() != rng.end() )
    rng.smmp->erase(rng.beg());
}

static int smm_erase_if_aux(px* pred, px* tpl, bool first_only)
{
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  smm* smmp = rng.smmp;
  pxhmmap &mmp = smmp->mmp;
  size_t sz = mmp.size();
  int mode = smmp->keys_only ? stl_smm_key : stl_smm_both;
  px* exception;
  int fun_res = 0;
  pmmi i = rng.beg();
  pmmi end = rng.end();
  while (i!=end) {
    pmmi trg = i++;
    px* pxres = apply_fun(pred, mode, trg, &exception);
    if (exception) pure_throw(exception);
    if ( pure_is_int(pxres, &fun_res) && fun_res ) {
      smmp->erase(trg);
      if (first_only) break;
    } 
  }
  return sz - mmp.size();
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
  size_t sz = smmp->mmp.size();
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
    pxhmmap &mmp = smmp->mmp;
    pmmi i;
    if ( !smmp->get_cached_pmmi(k, i) ) 
      i = get_iter(mmp, k, gi_find);  
    if (i != mmp.end())
      smmp->erase(i);
    else
      ret = 0;
  }
}

int smm_remove_if(smm* smmp, px* k, px* pred, int all)
{
  int ret = 0;
  pxhmmap &mmp = smmp->mmp;
  pxh_pred1 fun(pred);
  pair<pmmi,pmmi> lb_ub = mmp.equal_range(k);
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

px* smm_listmap(px* fun, px* tpl, int what)
{
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  if (rng.smmp->keys_only) what = stl_smm_key;
  return listmap_aux(fun, rng.beg(), rng.end(), what);
}

px* smm_listcatmap(px* fun, px* tpl, int what)
{
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  if (rng.smmp->keys_only) what = stl_smm_key;
  pmmi b = rng.beg(); 
  pmmi e = rng.end(); 
  px* cons = px_cons_sym();
  px* nl = px_null_list_sym();
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

static px* smm_foldl_rng(px* fun, px* val, pmmi i, pmmi end, int mode)
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
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  int mode =  rng.smmp->keys_only ? stl_smm_key : stl_smm_both;
  try {
    return smm_foldl_rng(fun, val, rng.beg(), rng.end(), mode);
  } catch (px* e) {
    pure_throw(e);
  }
}

px* smm_foldl1(px* fun, px* tpl)
{
  smm_range rng(tpl);
  if ( !rng.is_valid ) bad_argument();
  int mode =  rng.smmp->keys_only ? stl_smm_key : stl_smm_both;
  if (rng.beg() == rng.end() ) return 0;
  pmmi b = rng.beg();
  px* val;
  if (mode == stl_smm_key)
    val = b->first.pxp();
  else
    val = pair_to_rocket(b->first.pxp(), b->second.pxp());
  try {
    return smm_foldl_rng(fun, val, ++b, rng.end(), mode);
  } catch (px* e) {
    pure_throw(e);
  }
}

static px* smm_foldr_rng(px* fun, px* val, pmmi beg, pmmi end, int mode)
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
  smm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  int mode = rng.smmp->keys_only ? stl_smm_key : stl_smm_both;
  try {
    return smm_foldr_rng(fun, val, rng.beg(), rng.end(), mode);
  } catch (px* x) {
    pure_throw(x);
  }
}

px* smm_foldr1(px* fun, px* tpl)
{
  smm_range rng(tpl);
  if ( !rng.is_valid ) bad_argument();
  int mode =  rng.smmp->keys_only ? stl_smm_key : stl_smm_both;
  if ( rng.beg() == rng.end() ) return 0;
  pmmi e = --rng.end();
  px* val;
  if (mode == stl_smm_key)
    val = e->first.pxp();
  else
    val = pair_to_rocket(e->first.pxp(), e->second.pxp());
  try {
    return smm_foldr_rng(fun, val, rng.beg(), e, mode);
  } catch (px* x) {
    pure_throw(x);
  }
}
