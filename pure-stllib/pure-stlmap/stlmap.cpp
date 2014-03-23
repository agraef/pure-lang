/* stlmap.cpp -- C++ support for stlmap.pure

Copyright (c) 2012 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlmap, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlmap distribution package for details.

*/

#include <cassert>
#include <stdlib.h>
#include <iostream>
#include <algorithm>
#include <numeric>
#include "stlmap.hpp"

using namespace std;

enum {stl_sm_key =1, stl_sm_val, stl_sm_elm,
      stl_sm_iter, stl_sm_iter_dflt};

enum {stl_sm_lower_bound=1, stl_sm_upper_bound, stl_sm_equal_range};

enum {stl_sm_merge = 1, stl_sm_union, stl_sm_difference, 
      stl_sm_intersection, stl_sm_symmetric_difference};

enum {stl_sm_at_beginning = 1, stl_sm_at_pastend};

static int stlmap_tag() 
{
  static ILS<int> _t = 0; int &t = _t();
  if (!t) t = pure_pointer_tag("stlmap*");
  return t;
}

static int stlmap_iter_tag() 
{
  static ILS<int> _t = 0; int &t = _t();
  if (!t) t = pure_pointer_tag("stlmap_iter*");
  return t;
}

// static void dump_smp(sm* smp) {
//   PR2(dump_smp,smp->keys_only,smp->has_dflt);
//   PR2(dump_smp,smp->dflt,smp->dflt.pxp()->refc);
//   PR(dump_smp,smp->cache_key);
//   PR(dump_smp,smp->mp.size());
// }

/*** Helpers for stlmap.cpp only ************************************/

enum {gi_find, gi_lower, gi_upper};

static int range_size(sm* smp, pmi b, pmi e)
{
  size_t sz = 0;
  pxhmap& mp = smp->mp;
  if (b == mp.begin() && e == mp.end())
    sz = mp.size();
  else if (b == e)
    sz = 0;
  else
    while(b != mp.end() && b++ != e) sz++;
  return sz;
}


static bool extract_kv(sm* smp, px* kv, px*& k, px*& v)
{
  bool ok = true;
  if (smp->keys_only) {
    k = kv;
    v = NULL;
  } 
  else if ( !pxrocket_to_pxlhs_pxrhs(kv, &k, &v) ) {
    k = kv;
    v = NULL;
    ok = false;
  }
  return ok;
}

// increases inserted by 1 iff inserted a elm or changed existing val
static bool insert_aux(sm* smp, px* kv, pmi& pos, int& inserted, bool replace)
{
  px *k, *v;
  bool ok = extract_kv(smp,kv,k,v);
  if (ok) {
    if ( smp->cache_key.pxp() == k) {
      if (replace) {
        smp->cache_iter->second = v;
        inserted++;
      }
    }
    else {
      pair<pmi,bool> i_ok = smp->mp.insert(pxhpair(k,v));
      pos = i_ok.first;
      if (i_ok.second)
        inserted += 1;
      else if (replace) {
        pos->second = v;
        inserted++;
      }
    }
  }
  return ok;
}

static px* px_pointer(sm* smp)
{
 static ILS<px*> _sym = NULL; px*& sym = _sym();
  if (!sym) sym = pure_new(pure_symbol(pure_sym("stl::stl_sm_delete")));
  px* ptr = pure_tag( stlmap_tag(), pure_pointer(smp));
  return pure_sentry(sym,ptr);
}

static px* px_pointer(sm_iter* smip)
{
  static ILS<px*> _sym = NULL; px*& sym = _sym();
  if (!sym) sym = pure_new(pure_symbol(pure_sym("stl::stl_sm_iter_delete")));
  px* ptr = pure_tag(stlmap_iter_tag(), pure_pointer(smip));
  return pure_sentry(sym,ptr);
}

static bool get_smp(px* pxsmp, sm** smpp)
{
  void* ptr;
  bool ok = false;
  if ( pure_is_pointer(pxsmp, &ptr) ) {
    ok = pure_get_tag(pxsmp) == stlmap_tag();
  }
  *smpp = ok ? (sm*)ptr : NULL; 
  return ok;
}

static bool get_smip(px* pxsmip, int& tag, sm_iter** itr)
{
  void* ptr;
  sm_iter* smip;
  bool ok = pure_is_pointer(pxsmip,&ptr);
  if (ok) { 
    smip = (sm_iter*)ptr;
    tag = pure_get_tag(pxsmip);
    if ( tag != stlmap_iter_tag() ) ok = false;
    if (ok) *itr = smip;
  }
  return ok;
}

static pmi get_iter(sm* smp , px* key, int mode)
{
  pxhmap& mp = smp->mp;
  pmi iter;
  if (key == smbeg()) {
    iter = mp.begin();
  }
  else if (key == smend()) {
    iter = mp.end();
  }
  else if (smp->cache_key.pxp() == key) {
    iter = smp->cache_iter;
    if (mode==gi_upper) iter++;
  }
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
  return it->first;
}

static px* get_elm_aux(sm* smp, pmi i, int what) 
{
  px* res = 0;
  pxhmap &mp = smp->mp; 
  if (i != mp.end()) {
    switch (what) {
    case stl_sm_key:
      res = i->first; 
      break;
    case stl_sm_val:
      if (smp->keys_only)
        res = i->first;
      else
        res = i->second; 
      break;
    case stl_sm_elm:
      px* key = i->first;     
      res = smp->keys_only ? key : pxhpair_to_pxrocket(*i);
      break;
    }
  }
  else {
    if (what==stl_sm_key)
      res = smend();
    else
      index_error();
  }
  return res;
}

static px* sm_foldl_aux(px* fun, px* val, const sm_range rng, pmi i,  int mode)
{ 
  pmi end = rng.end();
  sm* smp = rng.smp();
  pmi sac_end = smp->mp.end(); 
  px* res = pure_new(val); 
  px* exception = 0;
  while (i != end  && i != sac_end){
    pmi trg_i = i++;
    px* trg = get_elm_aux(smp, trg_i, mode);
    px* fxy = pure_appxl(fun, &exception, 2, res, trg);
    if (exception) {
      pure_freenew(res);
      throw exception;
    }
    pure_new(fxy);
    pure_free(res);
    res = fxy;
  }
  pure_unref(res);
  if (i!=end) {
    pure_freenew(res);
    bad_argument();    
  }
  return res;
}

static px* sm_foldr_aux(px* fun, px* val, const sm_range& rng, pmi i, int mode)
{ 
  pmi beg = rng.beg();
  sm* smp = rng.smp();
  pmi sac_beg = smp->mp.begin();  
  px* res = pure_new(val);
  px* exception = 0;
  while (i != beg && i != sac_beg) {
    pmi trg_i = --i;
    px* trg = get_elm_aux(smp, trg_i, mode);
    px* fxy = pure_appxl(fun, &exception, 2, trg, res);
    if (exception) {
      pure_freenew(res);
      throw exception;
    }
    pure_new(fxy);
    pure_free(res);
    res = fxy;
  }
  pure_unref(res);
  if (i!=beg) {
    pure_freenew(res);
    bad_argument();    
  }
  return res;
}

/*** sm_iter members  ***********************************************/

sm_iter::sm_iter(px* pxsmp, pmi i) : pxhsmp(pxsmp), iter(i), is_valid(1)
{
  if (iter != smp()->mp.end())
    smp()->smis.push_back(this);
}

sm_iter::~sm_iter() 
{
  smp()->remove_sm_iter(this);
}

sm* sm_iter::smp () const
{
  void* ret;
  pure_is_pointer( pxhsmp, &ret);
  return static_cast<sm*>(ret);
}

ostream& operator<<(ostream& os, const sm_iter* smip)
{
  if (smip->is_valid) {
    if (smip->iter == smip->smp()->mp.end())
      os << "pastend iterator";
    else
      os << smip->iter->first;
  }
  else {
    os << "invalid iterator";
  }
  return os;
}

/*** stlmap members  ***********************************************/

stlmap::stlmap(px* cmp, px* val_cmp, px* val_eql, bool keyonly):
  mp(pxh_pred2(cmp)), cache_key( pxh(0) ),
  keys_only(keyonly), has_dflt(0), dflt(NULL),
  px_comp(cmp), px_val_comp(val_cmp), px_val_equal(val_eql)
{
  //PR(stlmap,this);
}

stlmap::stlmap(px* cmp, px* val_cmp, px* val_eql, bool keyonly, px *d):
  mp(pxh_pred2(cmp)),  cache_key( pxh(0) ),
  keys_only(keyonly), has_dflt(1), dflt(d),
  px_comp(cmp), px_val_comp(val_cmp), px_val_equal(val_eql)
{
  //PR(stlmap,this);  
}

stlmap::~stlmap()
{
  //cerr << "~stlmap: " << this << endl;
  assert(smis.size()==0);
}

px* stlmap::parameter_tuple()
{
  px* ret = 0;
  px* kc = px_comp;
  if (keys_only) {
    px* nl = pure_listl(0);
    ret = pure_tuplel(5,pure_int(1),kc,nl,nl,nl);
  }
  else {
    px* df = dflt;
    px* vc = px_val_comp;
    px* ve = px_val_equal;
    ret = pure_tuplel(5, pure_int(0), kc, df, vc, ve);
  }
  return ret;
}

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

struct has_pmi {
  has_pmi(pmi _iter) : iter(_iter) {}
  bool operator()(sm_iter* smip){return smip->iter == iter;} 
  pmi iter;
};

void stlmap::remove_sm_iter(sm_iter* smip)
{
  smis.erase( remove(smis.begin(), smis.end(), smip), smis.end() );
}

void stlmap::clear_ki_cache() {
  cache_key = pxh(0);
  cache_iter = mp.end();
}

void stlmap::kill_cache_iter(pmi pos)
{
  if ( cache_iter == pos ) {
    cache_key = pxh(0);
    cache_iter = mp.end();
  }
}

void stlmap::invalidate_iter(pmi pos)
{
  if ( pos == mp.end() ) return;
  kill_cache_iter(pos);
  vector<sm_iter*>::iterator i = smis.begin(), pe;
  has_pmi is_trg(pos);
  while( i!=smis.end() ) {
    if ( is_trg(*i) ) (*i)->is_valid = 0;
    i++;
  }
  pe = remove_if( smis.begin(), smis.end(), is_trg );
  smis.erase( pe, smis.end() );
}

void stlmap::invalidate_all_iters()
{
  for (vector<sm_iter*>::iterator i = smis.begin(); i!= smis.end(); i++) {
    (*i)->is_valid = 0;
  }
  clear_ki_cache();
  smis.clear();
}

int stlmap::erase(pmi pos)
{
  invalidate_iter(pos);
  mp.erase(pos);
  return 1;
}

int stlmap::erase(px* k)
{
  int ret = 0;
  if ( !mp.empty() ) {
    ret = 1;
    pmi i;
    if ( cache_key.pxp() == k )
      erase(i);
    else {
      if (k == smbeg()) {
        erase(mp.begin());
      }
      else if (k != smend()) {
        pair<pmi,pmi> er = mp.equal_range(k);
        ret = erase(er.first, er.second);
       }
      else
        ret = 0;
    }
  }
  return ret;
}

int stlmap::erase(pmi first, pmi last)
{
  int ret = 0;
  for (pmi pos = first; pos != last; pos++) {
    invalidate_iter(pos); ret++;
  }
  mp.erase(first, last);
  return ret;
}

void stlmap::clear()
{
  invalidate_all_iters();
  mp.clear();
}

/*** sm_range functions *********************************************/

bool sm_range::init_from_iters(px** pxsmip, int sz)
{
  if (sz==0 || sz>2) return false;
  num_iters = sz;
  int tag;
  sm_iter* smip;
  if ( !get_smip(pxsmip[0],tag,&smip) || !smip->is_valid ) return false;
  is_valid = false;
  sm* smp = smip->smp();
  pxhsmp = smip->pxhsmp;
  begin_it = smip->iter;
  if (num_iters == 2) {
    pxhmap& mp = smp->mp;
    pxhmap::key_compare k_cmp = mp.key_comp();
    if (get_smip(pxsmip[1],tag,&smip) && smip->is_valid && smip->smp()==smp) 
      end_it = smip->iter;
    else
      goto done;
    if ( begin_it == mp.end() && end_it != mp.end() )
      goto done;
    if ( end_it != mp.end() && k_cmp(end_it->first,begin_it->first) )
      goto done;
  } 
  else {
    end_it = end_it++;
  }
  is_valid = true;
 done:
  return is_valid;
}

bool sm_range::init_from_keys(px** smp_keys, int tpl_sz)
{
  is_valid = false;
  sm* smp;
  if ( !get_smp(smp_keys[0], &smp) ) return false;
  pxhsmp = smp_keys[0];
  pxhmap &mp = smp->mp;
  pxhmap::key_compare k_cmp = mp.key_comp();
  num_iters = tpl_sz-1;
  pmi et;
  if (num_iters > 2) return false;

  is_valid = true;
  if (num_iters == 0 || mp.size() == 0) {
    begin_it = mp.begin();
    end_it = mp.end();
    return true;
  }
  
  px* b = smp_keys[1];
  px* e = num_iters == 2 ? smp_keys[2] : b;
  begin_it = get_iter(smp, b, gi_lower);
  if (b == smbeg()) b = begin_it->first;
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
  else {
    pmi i = get_iter(smp, e, gi_upper); // upper to high
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
  if (end_it != mp.end() && k_cmp(end_it->first, begin_it->first) )
    end_it = begin_it;
  return is_valid;
}

sm_range::sm_range(px* pmi_tuple)
{
  size_t tpl_sz;
  px** elems;
  pure_is_tuplev(pmi_tuple, &tpl_sz, &elems);
  try {
    init_from_iters(elems, tpl_sz) || init_from_keys(elems, tpl_sz);
  }
  catch (px* e) {
    free(elems);
    pure_throw(e);
  }
  free(elems);
} 

sm* sm_range::smp() const
{
  void* ptr;
  pure_is_pointer(pxhsmp, &ptr);
  return static_cast<sm*>(ptr);
}

/*** Pure interface support functions  *************************************/

px* stl_sm_type_tags()
{
  return pure_tuplel(2, pure_int(stlmap_tag()), pure_int(stlmap_iter_tag())); 
}

px* stl_sm_make_empty(px* comp, px* v_comp, px* v_eql, px* dflt, int keys_only)
{
  return px_pointer( new sm(comp, v_comp, v_eql, keys_only, dflt) );
}

void stl_sm_delete(sm* smp){
  delete(smp);
}

void stl_sm_iter_delete(sm_iter* smip){
  delete(smip);
}

px* stl_sm_container_info(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();  
  return rng.smp()->parameter_tuple();
} 

int stl_sm_size(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return range_size(rng.smp(), rng.beg(), rng.end());
}

bool stl_sm_empty(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return rng.num_iters == 0 ? rng.smp()->mp.empty() : stl_sm_size(tpl) == 0;
}

int  stl_sm_count(px* pxsmp, px* key)
{
  sm* smp;
  if ( !get_smp(pxsmp,&smp) ) bad_argument();
  return smp->mp.count(key);
}

bool stl_sm_is_set(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return rng.smp()->keys_only;
}

px* stl_sm_find(px* pxsmp, px* key, int what)
{
  sm* smp;
  if ( !get_smp(pxsmp,&smp) ) bad_argument();
  pxhmap& mp  = smp->mp;
  pmi i = get_iter(smp, key, gi_find);
  if (what==stl_sm_iter_dflt && i == mp.end() && smp->has_dflt){
    px* dflt = smp->dflt;
    pair<pmi,bool> i_ok = smp->mp.insert(pxhpair(key,dflt));
    return px_pointer(new sm_iter(pxsmp, i_ok.first));
  }
  else if (what==stl_sm_iter || what == stl_sm_iter_dflt) {
    return px_pointer(new sm_iter(pxsmp, i)); 
  }
  else {
    smp->cache_key = pxh(key);
    smp->cache_iter = i;
    return get_elm_aux(smp, i, what);
  }
}

px* stl_sm_get(sm* smp, px* key)
{
  pxhmap &mp = smp->mp;
  pmi i;

  if ( smp->cache_key.pxp() == key ) {
    i = smp->cache_iter;
  }
  else {
    if (key == smbeg()) {
      i = mp.begin();
    }
    else {
      i = mp.find(key);
      if (i != mp.end()) {
        smp->cache_key = pxh(key);
        smp->cache_iter = i;
      }
    }
  }
  if (i == mp.end()) index_error();
  return (smp->keys_only) ? i->first : i->second;
}

// px* stl_sm_get(sm* smp, px* key)
// {
//   pxhmap& mp = smp->mp;
//   pmi i;
//   i = mp.find(key);
//   if (i == mp.end()) index_error();
//   return (smp->keys_only) ? i->first : i->second;
// }

px* stl_sm_copy_iter(px* pxsmip)
{
  sm_iter* smip;
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) || !smip->is_valid ) bad_argument();
  return px_pointer( new sm_iter(smip->pxhsmp, pmi(smip->iter)) );
}

px* stl_sm_begin(px* pxsmp)
{
  sm* smp;
  if ( !get_smp(pxsmp, &smp) ) failed_cond();
  return px_pointer(  new sm_iter(pxsmp, smp->mp.begin()) );
}

px* stl_sm_end(px* pxsmp)
{
  sm* smp;
  if ( !get_smp(pxsmp, &smp) ) failed_cond();
  return px_pointer(  new sm_iter(pxsmp, smp->mp.end()) );
}

px* stl_sm_iter_bounds(px* pxsmp, px* key, int what)
{
  px* res = 0;
  sm* smp;
  px* pxsmip_l = 0;
  px* pxsmip_r = 0;
  pair<pmi,pmi> l_u;
  if ( !get_smp(pxsmp, &smp) ) failed_cond();
  pxhmap& mp = smp->mp;
  switch (what) {
  case stl_sm_lower_bound:
    res = px_pointer( new sm_iter(pxsmp, mp.lower_bound(key)) );
    break;
  case stl_sm_upper_bound:
    res = px_pointer( new sm_iter(pxsmp, mp.upper_bound(key)) );
    break;
  case stl_sm_equal_range:
    l_u = mp.equal_range(key);
    pxsmip_l = px_pointer( new sm_iter(pxsmp, l_u.first) );
    pxsmip_r = px_pointer( new sm_iter(pxsmp, l_u.second) );
    res = pure_tuplel(2, pxsmip_l, pxsmip_r);
    break;
  default:
    bad_argument();
  }
  return res;
}

// valid, container, i1, i2,
px* stl_sm_range_info(px* tpl)
{
  px* ret;
  sm_range rng(tpl);
  bool ok = rng.is_valid;
  px* valid = pure_int(ok);
  if (rng.is_valid) {
    px* container = rng.pxhsmp;
    px* beg = px_pointer( new sm_iter(rng.pxhsmp, rng.beg()) );
    px* end = px_pointer( new sm_iter(rng.pxhsmp, rng.end()) );
    ret = pure_tuplel(4, valid, container, beg, end);
  }
  else {
    px* null_ptr = pure_pointer(0);
    ret = pure_tuplel(4, valid, null_ptr, null_ptr, null_ptr);
  }
  return ret;
}

px* stl_sm_move_iter(px* pxsmip, int count)
{
  sm_iter* smip;
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) ) return 0;
  if ( !smip->is_valid ) bad_argument();
  pmi& i = smip->iter;
  pmi beg = smip->smp()->mp.begin();
  pmi end = smip->smp()->mp.end();
  while (count > 0) {
    if (i == end) return pxsmip;
    count--; i++;
  }
  while (count++ < 0) {
    if (i == beg) index_error();
    i--;
  }
  return pxsmip;
}

px* stl_sm_iter_is_at(px* pxsmip, int where)
{
  px* res = 0;
  sm_iter* smip;
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) || !smip->is_valid ) return 0;
  switch (where) {
  case stl_sm_at_beginning:
    res = pure_int( smip->smp()->mp.begin() == smip->iter );
    break;
  case stl_sm_at_pastend:
    res = pure_int( smip->smp()->mp.end() == smip->iter );
    break;
  default:
    bad_argument();
  }
  return res;
}

// fix return is_valid, container, key, val 
px* stl_sm_iter_info(px* pxsmip)
{
  sm_iter* smip;
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) ) return 0;
  px* valid = pure_int( smip->is_valid );
  px* cont = smip->pxhsmp;
  pmi i = smip->iter;
  sm* smp = smip->smp();
  px* key; px* val;
  if (smip->is_valid && i != smp->mp.end() ) {
    key = iter_to_key(smp->mp, i);    
    if (smp->keys_only) 
      val = i->first;
    else
      val = i->second;
  } 
  else {
    key = smend();
    val = pure_listl(0);
  }
  return pure_tuplel(4,valid, cont, key, val);
}

px* stl_sm_equal_iter(px* pxsmip1, px* pxsmip2)
{
  sm_iter* smip1;
  int tag1;
  if ( !get_smip(pxsmip1,tag1,&smip1) || !smip1->is_valid ) bad_argument();
  sm* smp1 = smip1->smp();
  sm_iter* smip2;
  int tag2;
  if ( !get_smip(pxsmip2,tag2,&smip2) || !smip2->is_valid ) bad_argument();
  sm* smp2 = smip2->smp();
  bool compatible = true;
  if (smp1->keys_only) 
    compatible = smp2->keys_only;
  else
    compatible = !smp2->keys_only;
  if (!compatible) bad_argument();
  return pure_int( smip1->iter == smip2->iter ); 
}

px* stl_sm_get_at(px* pxsmip, int what)
{
  sm_iter* smip;
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) || !smip->is_valid ) bad_argument();
  sm* smp = smip->smp();
  if (smip->iter == smp->mp.end()) index_error();
  if ( what==stl_sm_elm && smp->keys_only ) what = stl_sm_key; 
  return get_elm_aux(smp, smip->iter, what);
}

px* stl_sm_get_elm_at_inc(px* pxsmip)
{
  sm_iter* smip; 
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) || !smip->is_valid ) bad_argument();
  sm* smp = smip->smp();
  pmi& i = smip->iter;
  if ( i == smp->mp.end() ) index_error();
  int what = smp->keys_only ? stl_sm_key : stl_sm_elm; 
  px* ret = get_elm_aux(smp, i, what);
  i++;
  return ret;
}

px* stl_sm_put_at(px* pxsmip, px* val)
{
  sm_iter* smip;
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) || !smip->is_valid ) bad_argument();
  if ( tag != stlmap_iter_tag() ) bad_argument();
  pmi itr = smip->iter;
  sm* smp = smip->smp();
  if (itr == smp->mp.end()) index_error();
  smip->iter->second = val;
  return val;
}

px* stl_sm_insert_hinted(px* pxsmp, px* pxsmip, px* kv)
{
  sm* smp; pmi pos;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  sm_iter* smip;
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) || !smip->is_valid ) bad_argument();
  px *k, *v;
  if ( !same(smip->pxhsmp,pxsmp) ) bad_argument();
  if ( !extract_kv(smp,kv,k,v) ) bad_argument();
  try {
    pos = smp->mp.insert(smip->iter, pxhpair(k,v));
  }
  catch (px* e) {
    pure_throw(e);
  }
  return px_pointer( new sm_iter(pxsmp, pos) );
}

// returns iterator - no cache
px* stl_sm_insert_elm(px* pxsmp, px* kv)
{
  bool replace = 0;
  sm* smp; pmi pos;
  if (!get_smp(pxsmp,&smp)) bad_argument();
  int num_inserted = 0;
  try {
    if ( !insert_aux(smp, kv, pos, num_inserted, replace) )
      bad_argument();
  }
  catch (px* e) {
    pure_throw(e);
  }
  px* it = px_pointer(new sm_iter(pxsmp, pos));
  return pure_tuplel(2,it,pure_int(num_inserted));
}

int stl_sm_insert(px* pxsmp, px* src, bool replace)
{
  sm* smp; pmi pos;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  size_t sz = 0;
  px** elems = NULL;
  int num_inserted = 0;
  try {
    if (pure_is_listv(src, &sz, &elems)) {
      for (size_t i = 0; i<sz; i++)
        if ( !insert_aux(smp, elems[i], pos, num_inserted, replace) ) 
          bad_argument();
      free(elems);
    } else if (matrix_type(src) == 0) {
      sz = matrix_size(src); 
      px** melems = (pure_expr**) pure_get_matrix_data(src);
      for (size_t i = 0; i<sz; i++) 
        if ( !insert_aux(smp, melems[i], pos, num_inserted, replace) ) 
          bad_argument();
    }
    else if ( !insert_aux(smp, src, pos, num_inserted, replace) )
      bad_argument();
  }
  catch (px* e){
    free(elems);
    pure_throw(e);
  }
  return num_inserted;
}

int stl_sm_insert_stlmap(px* pxsmp, px* tpl, bool replace)
{
  int num_inserted = 0; 
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  if (smp == rng.smp()) bad_argument();
  try {
    if (replace) {
       for (pmi i = rng.beg(); i!=rng.end(); i++) {
        pair<pmi,bool> i_ok = smp->mp.insert(*i);
        if (!i_ok.second)
          (i_ok.first)->second = i->second;
        num_inserted++;
      }
    }
    else {
      pxhmap& mp = smp->mp;
      int oldsz = mp.size();
      mp.insert(rng.beg(),rng.end());
      num_inserted = mp.size() - oldsz;
    } 
  } catch (px* e) {
    pure_throw(e);
  } 
  return num_inserted;
}

int stl_sm_insert_stlvec(px* pxsmp, sv* sv_p, bool replace)
{
  sm* smp; pmi pos;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  int num_inserted = 0;
  try {
    for (sv::iterator i = sv_p->begin(); i!=sv_p->end(); i++) {
      if ( !insert_aux(smp, *i, pos, num_inserted, replace) )
        bad_argument();
    }
  } catch (px* e) {
    pure_throw(e);
  }
  return num_inserted;
}

void stl_sm_swap(px* pxsmp1, px* pxsmp2)
{
  sm* smp1; sm* smp2;
  if ( !get_smp(pxsmp1, &smp1) ) failed_cond();
  if ( !get_smp(pxsmp2, &smp2) ) failed_cond();
  smp1->invalidate_all_iters();
  smp2->invalidate_all_iters();
  smp1->mp.swap(smp2->mp);
}

int stl_sm_clear(px* pxsmp)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  size_t sz = smp->mp.size();
  smp->clear();
  return sz;
}

int stl_sm_erase(px* pxsmp, px* trg)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  int res = 0;
  size_t trg_sz;
  px** elems;
  pure_is_tuplev(trg, &trg_sz, &elems);
  if (trg_sz == 1) {
    sm_iter* smip;
    int tag;
    if ( !get_smip(trg,tag,&smip) || !smip->is_valid ) bad_argument();
    if ( !same(pxsmp, smip->pxhsmp) ) bad_argument();
    smip->smp()->erase(smip->iter);
    res = 1;
  }
  else {
    sm_range rng(trg);
    if (!rng.is_valid) bad_argument();
    if ( !same(pxsmp, rng.pxhsmp) ) bad_argument();
    res = rng.smp()->erase(rng.beg(), rng.end());
  }
  return res;
}

bool stl_sm_equal(px* tpl1, px* tpl2)
{
  bool res = false;
  sm_range rng1(tpl1);
  sm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument();
  if (stl_sm_size(tpl1) != stl_sm_size(tpl2)) return 0;
  sm* smp = rng1.smp();
  try {
    if (smp->keys_only) {
      pxhpair_first_equivalent comp(smp->px_comp);   
      res = equal(rng1.beg(), rng1.end(), rng2.beg(), comp);
    }
    else {
      pxhpair_equivalent comp(smp->px_comp,smp->px_val_equal);   
      res = equal(rng1.beg(), rng1.end(), rng2.beg(), comp);
    }
  }
  catch (px* e) {
    pure_throw(e);
  }
  return res;
}

bool stl_sm_less(px* tpl1, px* tpl2)
{
  bool res = false;
  sm_range rng1(tpl1);
  sm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument();
  try {
    res = lexicographical_compare(rng1.beg(), rng1.end(),
                                  rng2.beg(), rng2.end(), 
                                  rng1.smp()->mp.value_comp());
  }
  catch (px* e) {
    pure_throw(e);
  }
  return res;
}

bool stl_sm_includes(px* tpl1, px* tpl2)
{
  bool res = false;
  sm_range rng1(tpl1);
  sm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument();
  try {
    res = includes(rng1.beg(), rng1.end(), rng2.beg(), rng2.end(), 
                   rng1.smp()->mp.value_comp());
  }
  catch (px* e) {
    pure_throw(e);
  }
  return res;
}

px* stl_sm_setop(int op, px* tpl1, px* tpl2)
{
  px* res = 0;
  sm_range rng1(tpl1);
  sm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument();
#ifdef PURE_INSERT_SEMANTICS
  if (op != stl_sm_difference) {
    sm_range temp = rng2;
    rng2 = rng1;
    rng1 = temp;
  }
#endif
  sm* smp = rng1.smp();
  sm* trg = new sm(smp->px_comp,
                   smp->px_val_comp,
                   smp->px_val_equal,
                   smp->keys_only, smp->dflt);
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
    res = px_pointer(trg);
  } catch (px* e) {
    pure_throw(e);
  }
  return res;
}

px* stl_sm_make_vector(px* tpl) 
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  pmi b = rng.beg();
  pmi e = rng.end();
  int sz = range_size(rng.smp(), b, e);
  if (!sz)
    return pure_matrix_columnsv(0,NULL);
  px** bfr = (px**)malloc(sizeof(px*)*sz);
  if (rng.smp()->keys_only) 
    transform(b, e, bfr, pxhpair_to_pxlhs);
  else
    transform(b, e, bfr, pxhpair_to_pxrocket);
  px* ret = pure_matrix_columnsv(sz, bfr);
  free(bfr);
  return ret;
}

void stl_sm_fill_stlvec(px* tpl, sv* svp) 
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  pmi b = rng.beg();
  pmi e = rng.end();
  if (rng.smp()->keys_only) 
    transform(b, e, back_inserter(*svp), pxhpair_to_pxlhs);
  else
    transform(b, e, back_inserter(*svp), pxhpair_to_pxrocket);
}

/*** Mapping and folding ***********************************************/

px* stl_sm_listmap(px* fun, px* tpl, int what)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  sm* smp = rng.smp();
  if (smp->keys_only) what = stl_sm_key;
  pmi sac_end = smp->mp.end();
  pmi b = rng.beg();
  pmi e = rng.end();
  px* cons = px_cons_sym();
  px* nl = pure_listl(0);
  px* res = nl;
  px* y = 0;
  px* exception;
  int use_function = 1;
  if (pure_is_int(fun,&use_function))
    use_function = false;
  else
    use_function = true;
  pmi i = b;
  for (;i != e && i != sac_end; i++){
    px* trg = get_elm_aux(smp, i, what);
    px* pxi = trg;
    if (use_function) {
      pxi = pure_appxl(fun, &exception, 1, trg);
      if (exception) {
        if (res) pure_freenew(res);
        if (pxi) pure_freenew(pxi);
        pure_throw(exception);
      }
    }
    px* last = pure_app(pure_app(cons,pxi),nl);
    if (res==nl)
      res = y = last;
    else {
      y->data.x[1] = pure_new(last);
      y = last;
    }
  }
  if (i!=e) {
    pure_freenew(res);
    bad_argument();    
  }
  return res;
}

px* stl_sm_listcatmap(px* fun, px* tpl, int what)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  sm* smp = rng.smp();
  if (smp->keys_only) what = stl_sm_key;
  pmi i = rng.beg(); 
  pmi e = rng.end(); 
  pmi end = smp->mp.end();
  px* cons = px_cons_sym();
  px* nl = pure_listl(0);
  px* res = nl;
  px* y = 0;
  px* exception;
  px* *elms;
  size_t sz;
  for (;i != e && i != end; i++){
    px* trg = get_elm_aux(smp, i, what);
    px* pxi = pure_appxl(fun, &exception, 1, trg);
    if (exception) {
      if (res) pure_freenew(res);
      if (pxi) pure_freenew(pxi);
      pure_throw(exception);
    }
    if ( !pure_is_listv(pxi, &sz, &elms) ){
      pure_freenew(pxi);
      if (res) pure_freenew(res);
      bad_argument();      
    }
    for (size_t j = 0; j < sz; j++) {
      px* last = pure_app(pure_app(cons,elms[j]),nl);
      if (res==nl)
        res = y = last;    
      else {
        y->data.x[1] = pure_new(last);
        y = last;
      }
    }
    pure_freenew(pxi);
    free(elms);
  }
  if (i!=e) {
    pure_freenew(res);
    bad_argument();    
  }
  return res;  
}

px* stl_sm_foldl(px* fun, px* val, px* tpl)
{
  px* ret = 0;
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  sm* smp = rng.smp();
  int mode =  smp->keys_only ? stl_sm_key : stl_sm_elm;
  try {
    ret = sm_foldl_aux(fun, val, rng, rng.beg(), mode);
  } catch (px* e) {
    pure_throw(e);
  }
  return ret;
}

px* stl_sm_foldl1(px* fun, px* tpl)
{
  px* res = 0;
  sm_range rng(tpl);
  if ( !rng.is_valid ) bad_argument();
  sm* smp = rng.smp();
  int mode =  smp->keys_only ? stl_sm_key : stl_sm_elm;
  pmi end = smp->mp.end();
  pmi b = rng.beg();
  pmi e = rng.end();
  if ( b==e || b==end ) bad_argument();
  px* val;
  if (mode == stl_sm_key)
    val = b->first;
  else
    val = pxlhs_pxrhs_to_pxrocket(b->first,b->second);
  try {
    res = sm_foldl_aux(fun, val, rng, ++b, mode);
  } catch (px* e) {
    pure_throw(e);
  }
  return res;
}

px* stl_sm_foldr(px* fun, px* val, px* tpl)
{
  px* res = 0;
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  int mode = rng.smp()->keys_only ? stl_sm_key : stl_sm_elm;
  try {
    res = sm_foldr_aux(fun, val, rng, rng.end(), mode);
  } catch (px* x) {
    pure_throw(x);
  }
  return res;
}

px* stl_sm_foldr1(px* fun, px* tpl)
{
  px* res = 0;
  sm_range rng(tpl);
  if ( !rng.is_valid ) bad_argument();
  sm* smp = rng.smp();
  int mode =  smp->keys_only ? stl_sm_key : stl_sm_elm;
  pmi beg = smp->mp.begin();
  pmi end = smp->mp.end();
  pmi b = rng.beg();
  pmi e = rng.end();
  if ( b==e || b==end || e==beg) bad_argument();
  e--;
  px* val;
  if (mode == stl_sm_key)
    val = e->first;
  else
    val = pxlhs_pxrhs_to_pxrocket(e->first,e->second);
  try {
    res = sm_foldr_aux(fun, val, rng, e, mode);
  } catch (px* x) {
    pure_throw(x);
  }
  return res;
}

void stl_sm_do(px* fun, px* tpl)
{ 
  sm_range rng(tpl);
  if ( !rng.is_valid ) bad_argument();
  sm* smp = rng.smp();
  int mode =  smp->keys_only ? stl_sm_key : stl_sm_elm;
  pmi i = rng.beg();
  pmi e = rng.end();
  px* exception = 0;
  while (i != e) {
    px* trg = get_elm_aux(smp, i++, mode);
    px* fx = pure_appxl(fun, &exception, 1, trg);
    pure_freenew(fx);
    if (exception) pure_throw(exception);
  }
}

/*** Key oriented interface support ***************************************/

int stl_sm_member(sm* smp, px* key)
{
  int ret = 0;
  pxhmap&  mp = smp->mp;
  pmi i;
  // FIX use get_iter
  if (smp->cache_key.pxp() == key ) {
    ret = 1;
  }
  else {
    i = (smp->mp).find(key);
    if (i != mp.end()) {
      smp->cache_key = pxh(key);
      smp->cache_iter = i;
      ret = 1;
    }
  } 
  return ret;
}

px* stl_sm_bounds(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  sm* smp = rng.smp();
  pxhmap& mp = smp->mp;
  return pure_tuplel(2,iter_to_key(mp, rng.beg()),
                       iter_to_key(mp, rng.end())); 
}

px* stl_sm_prev_key(px* pxsmp, px* key)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  pxhmap& mp = smp->mp;
  pmi i = get_iter(smp, key, gi_find);
  if ( i == mp.begin() || (i==mp.end() && key != smend()) )
    index_error();
  else
    i--;
  smp->cache_key = pxh(key);
  smp->cache_iter = i;
  px* ret = iter_to_key(mp, i);
  return ret;
}

px* stl_sm_next_key(px* pxsmp, px* key)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  pxhmap& mp = smp->mp;
  pmi i = get_iter(smp, key, gi_find);
  if ( i != mp.end() ) i++;
  smp->cache_key = pxh(key);
  smp->cache_iter = i;
  px* ret = iter_to_key(mp, i);
  return ret;
}

px* stl_sm_replace(sm* smp, px* key, px* val)
{
  if (smp->keys_only) bad_argument();
  pxhmap& mp = smp->mp;
  pmi i;
  if ( smp->cache_key.pxp() == key ) {
    i = smp->cache_iter;
    i->second = val;
  }
  else {
    i = (smp->mp).find(key);
    if ( i == mp.end() ) index_error();
    i->second = val;
    smp->cache_key = pxh(key);
    smp->cache_iter = i;
  }
  return val;
}

px* stl_sm_put(sm* smp, px* key, px* val)
{
  if (smp->keys_only) bad_argument();
  pxhmap& mp = smp->mp;
  pmi i;
  if ( smp->cache_key.pxp() == key ) {
    i = smp->cache_iter;
    i->second = val;
  }
  else {
    pair<pmi,bool> i_ok = mp.insert(pxhpair(key,val));
    i = i_ok.first;
    if (!i_ok.second) i->second = val;
    smp->cache_key = pxh(key);
    smp->cache_iter = i;
  }
  return val;
}

px* stl_sm_replace_with(px* pxsmp, px* key, px* unaryfun)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  if (smp->keys_only) return 0; // fail for sets
  if (!smp->has_dflt) failed_cond();
  pmi i;

  if ( smp->cache_key.pxp() == key ) {
    i = smp->cache_iter;
  }
  else {
    // uses default only if not already stored
    pair<pmi,bool> i_ok = smp->mp.insert(pxhpair(key,smp->dflt));
    i = i_ok.first;
    smp->cache_key = pxh(key);
    smp->cache_iter = i;
  }  
  px* old_val = i->second;
  px* exception = 0;
  px* new_val = pure_appxl(unaryfun, &exception, 1, old_val);
  if (exception) pure_throw(exception);
  if (!new_val) bad_function();
  i->second = new_val;
  return new_val;
}
