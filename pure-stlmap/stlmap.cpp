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

#include <cassert>
#include <stdlib.h>
#include <iostream>
#include <algorithm>
#include <numeric>
#include "stlmap.hpp"

using namespace std;

static int stlmap_tag() 
{
  static ILS<int> _t = 0; int &t = _t();
  if (!t) t = pure_pointer_tag("stlmap*");
  return t;
}

static int stlset_tag() 
{
  static ILS<int> _t = 0; int &t = _t();
  if (!t) t = pure_pointer_tag("stlset*");
  return t;
}

static int stlmap_iter_tag() 
{
  static ILS<int> _t = 0; int &t = _t();
  if (!t) t = pure_pointer_tag("stlmap_iter*");
  return t;
}

static int stlset_iter_tag() 
{
  static ILS<int> _t = 0; int &t = _t();
  if (!t) t = pure_pointer_tag("stlset_iter*");
  return t;
}


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
    pair<pmi,bool> i_ok = mp.insert(pxhpair(k,v));
    if (!i_ok.second) i_ok.first->second = v;
    pos = i_ok.first;
   }
  return pos;
}

static bool extract_kv(sm* smp, px* kv, px*& k, px*& v)
{
  bool ok = true;
  if (smp->keys_only) {
    k = kv;
    v = NULL;
  } 
  else {
    if ( pxrocket_to_pxlhs_pxrhs(kv, &k, &v) )
      ;
    else if (smp->has_dflt) {
      k = kv;
      v = smp->dflt;
    }
    else {
      k = kv;
      v = NULL;
      ok = false;
    }
  }
  return ok;
}

// increases inserted by 1 iff inserted new value
static bool insert_aux(sm* smp, px* kv, pmi& pos, int& inserted)
{
  px *k, *v;
  bool ok = extract_kv(smp,kv,k,v);
  if (ok) {
    if ( !smp->get_cached_pmi(k, pos) ) {
      pair<pmi,bool> i_ok = smp->mp.insert(pxhpair(k,v));
      pos = i_ok.first;
      inserted += i_ok.second;
    }
  }
  return ok;
}

static px* px_pointer(sm* smp)
{
  static ILS<px*> _sym = NULL; px*& sym = _sym();
  if (!sym) sym = pure_new(pure_symbol(pure_sym("stl::sm_delete")));
  int tag = smp->keys_only ? stlset_tag() : stlmap_tag();
  px* ptr = pure_tag( tag, pure_pointer(smp));
  return pure_sentry(sym,ptr);
}

static px* px_pointer(sm_iter* smip)
{
  static ILS<px*> _sym = NULL; px*& sym = _sym();
  if (!sym) sym = pure_new(pure_symbol(pure_sym("stl::sm_iter_delete")));
  int tag = smip->smp()->keys_only ? stlset_iter_tag() : stlmap_iter_tag();
  px* ptr = pure_tag( tag, pure_pointer(smip));
  return pure_sentry(sym,ptr);
}

static bool get_smp(px* pxsmp, sm** smpp)
{
  void* ptr;
  bool ok = false;
  if ( pure_is_pointer(pxsmp, &ptr) ) {
    int tag = pure_get_tag(pxsmp);
    ok = tag == stlmap_tag() || tag == stlset_tag();
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
    if ( tag != stlmap_iter_tag() && tag != stlset_iter_tag() ) ok = false;
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
  else if (smp->get_cached_pmi(key, iter) ) {
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
  px* ret;
  pxhmap &mp = smp->mp; 
  if (i != mp.end()) {
    switch (what) {
    case stl_sm_key:
      ret = i->first; 
      break;
    case stl_sm_val:
      if (smp->keys_only)
        ret = i->first;
      else
        ret = i->second; 
      break;
    case stl_sm_elm:
      px* key = i->first;     
      ret = smp->keys_only ? key : pxhpair_to_pxrocket(*i);
      break;
    }
  }
  else {
    if (what==stl_sm_key)
      ret = smend();
    else
      index_error();
  }
  return ret;
}

static px* sm_foldl_rng(px* fun, px* val, const sm_range rng, pmi i,  int mode)
{ 
  pmi end = rng.end();
  sm* smp = rng.smp();
  pmi sac_end = smp->mp.end(); 
  px* res = px_new(val); 
  px* exception = 0;
  while (i != end  && i != sac_end){
    pmi trg_i = i++;
    px* trg = get_elm_aux(smp, trg_i, mode);
    px* fxy = pure_appxl(fun, &exception, 2, res, trg);
    if (exception) {
      px_freenew(res);
      throw exception;
    }
    px_new(fxy);
    px_free(res);
    res = fxy;
  }
  px_unref(res);
  if (i!=end) {
    pure_freenew(res);
    bad_argument();    
  }
  return res;
}

static px* sm_foldr_rng(px* fun, px* val, const sm_range& rng, pmi i, int mode)
{ 
  pmi beg = rng.beg();
  sm* smp = rng.smp();
  pmi sac_beg = smp->mp.begin();  
  px* res = px_new(val);
  px* exception = 0;
  while (i != beg && i != sac_beg) {
    pmi trg_i = --i;
    px* trg = get_elm_aux(smp, trg_i, mode);
    px* fxy = pure_appxl(fun, &exception, 2, trg, res);
    if (exception) {
      px_freenew(res);
      throw exception;
    }
    px_new(fxy);
    px_free(res);
    res = fxy;
  }
  px_unref(res);
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
  mp(pxh_pred2(cmp)), keys_only(keyonly),
  px_comp(cmp), px_val_comp(val_cmp), px_val_equal(val_eql),
  has_recent_pmi(0), latest_pmi_pos(0),
  has_dflt(0), dflt(NULL)
{
  //PR(stlmap,this);
}

stlmap::stlmap(px* cmp, px* val_cmp, px* val_eql, bool keyonly, px *d):
  mp(pxh_pred2(cmp)), keys_only(keyonly),
  px_comp(cmp), px_val_comp(val_cmp), px_val_equal(val_eql),
  has_recent_pmi(0), latest_pmi_pos(0), 
  has_dflt(1), dflt(d)
{
  //PR(stlmap,this);  
}

stlmap::~stlmap()
{
  //PR(~stlmap,this);
  assert(smis.size()==0);
  //clear_all_iters();
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

void stlmap::cache_pmi(const pmi& i)
{
  if ( i != mp.end() ) {
    if (!has_recent_pmi) {
      recent_pmi.clear();
      has_recent_pmi = true;
      latest_pmi_pos = 0;
    }
    size_t num_cached = recent_pmi.size();
    for (int pos = 0; pos < num_cached; pos++)
      if ( recent_pmi[pos] == i ) return;
    if (num_cached<SM_CACHE_SZ) {
      recent_pmi.push_back(i);
      latest_pmi_pos = num_cached;
    }
    else {
      latest_pmi_pos = (latest_pmi_pos + 1) % SM_CACHE_SZ;
      recent_pmi[latest_pmi_pos] = i;
    }
    //PR2(stlmap::cache_pmi - cached,i->first,latest_pmi_pos);
  }
}

bool stlmap::get_cached_pmi(px* k, pmi& i)
{
  bool ret = false;
  if ( k != smend() && has_recent_pmi ) {
    size_t num_cached = recent_pmi.size();
    size_t pos = 0; 
    for (; pos < num_cached; pos++) {
      //      if ( same(recent_pmi[pos]->first, k) ) {
      if ( recent_pmi[pos]->first == k ) {
        i = recent_pmi[pos];
        ret = true;
        break;
      }
    }
  }
  // if (ret==true) {
  //   PR(stlmap::get_cached_pmi success,k);
  // }
  return ret;
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

void stlmap::clear_iter(pmi pos)
{
  if ( pos == mp.end() ) return;
  if (has_recent_pmi) {
    vector<pmi>::iterator i = ::find(recent_pmi.begin(),recent_pmi.end(),pos);
    if (i != recent_pmi.end()) recent_pmi.erase(i);
  }
  vector<sm_iter*>::iterator i = smis.begin(), pe;
  has_pmi is_trg(pos);
  while( i!=smis.end() ) {
    if ( is_trg(*i) ) (*i)->is_valid = 0;
    i++;
  }
  pe = remove_if( smis.begin(), smis.end(), is_trg );
  smis.erase( pe, smis.end() );
}

void stlmap::clear_all_iters()
{
  for (vector<sm_iter*>::iterator i = smis.begin(); i!= smis.end(); i++) {
    (*i)->is_valid = 0;
  }
  smis.clear();
}

int stlmap::erase(pmi pos)
{
  clear_iter(pos);
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
    clear_iter(pos); ret++;
  }
  mp.erase(first, last);
  return ret;
}

void stlmap::clear()
{
  has_recent_pmi = false;
  clear_all_iters();
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
  pxhsmp = smp_keys[0];
  sm* smp;
  if ( !get_smp(pxhsmp, &smp) ) return false;
  
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
    bool ok = init_from_iters(elems, tpl_sz) || init_from_keys(elems, tpl_sz);
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

px* sm_type_tags()
{
  return pure_tuplel(4, pure_int(stlmap_tag()), pure_int(stlmap_iter_tag()), 
                        pure_int(stlset_tag()), pure_int(stlset_iter_tag()));
}

px* sm_make_empty(px* comp, px* v_comp, px* v_eql, px* dflt, int keys_only)
{
  return px_pointer( new sm(comp, v_comp, v_eql, keys_only, dflt) );
}

void sm_delete(sm* smp){
  delete(smp);
}

void sm_iter_delete(sm_iter* smip){
  delete(smip);
}

px* sm_container_info(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();  
  return rng.smp()->parameter_tuple();
} 

int sm_size(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return range_size(rng.smp(), rng.beg(), rng.end());
}

bool sm_empty(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return rng.num_iters == 0 ? rng.smp()->mp.empty() : sm_size(tpl) == 0;
}

int  sm_count(px* pxsmp, px* key)
{
  sm* smp;
  if ( !get_smp(pxsmp,&smp) ) bad_argument();
  return smp->mp.count(key);
}

bool sm_is_set(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return rng.smp()->keys_only;
}

px* sm_find(px* pxsmp, px* key, int what)
{
  sm* smp;
  if ( !get_smp(pxsmp,&smp) ) bad_argument();
  pxhmap& mp  = smp->mp;
  int num_inserted;
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
    smp->cache_pmi(i);
    return get_elm_aux(smp, i, what);
  }
}

px* sm_copy_iter(px* pxsmip)
{
  sm_iter* smip;
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) || !smip->is_valid ) bad_argument();
  return px_pointer( new sm_iter(smip->pxhsmp, pmi(smip->iter)) );
}

px* sm_begin(px* pxsmp)
{
  sm* smp;
  if ( !get_smp(pxsmp, &smp) ) failed_cond();
  return px_pointer(  new sm_iter(pxsmp, smp->mp.begin()) );
}

px* sm_end(px* pxsmp)
{
  sm* smp;
  if ( !get_smp(pxsmp, &smp) ) failed_cond();
  return px_pointer(  new sm_iter(pxsmp, smp->mp.end()) );
}

px* sm_bounds(px* pxsmp, px* key, int what)
{
  sm* smp;
  sm_iter* smip;
  if ( !get_smp(pxsmp, &smp) ) failed_cond();
  pxhmap& mp = smp->mp;
  switch (what) {
  case stl_sm_lower_bound:
    return px_pointer( new sm_iter(pxsmp, mp.lower_bound(key)) );
  case stl_sm_upper_bound:
    return px_pointer( new sm_iter(pxsmp, mp.upper_bound(key)) );
  case stl_sm_equal_range:
    pair<pmi,pmi> l_u = mp.equal_range(key);
    px* pxsmip_l = px_pointer( new sm_iter(pxsmp, l_u.first) );
    px* pxsmip_r = px_pointer( new sm_iter(pxsmp, l_u.second) );
    return pure_tuplel(2, pxsmip_l, pxsmip_r);
  }
  bad_argument();
}

// valid, container, i1, i2,
px* sm_range_info(px* tpl)
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

px* sm_move_iter(px* pxsmip, int count)
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

px* sm_iter_is_at(px* pxsmip, int where)
{
  px* ret;
  sm_iter* smip;
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) || !smip->is_valid ) return 0;
  switch (where) {
  case stl_sm_at_beginning:
    return pure_int( smip->smp()->mp.begin() == smip->iter );
  case stl_sm_at_pastend:
    return pure_int( smip->smp()->mp.end() == smip->iter );
  default:
    bad_argument();
  }
}

// fix return is_valid, container, key, val 
px* sm_iter_info(px* pxsmip)
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

px* sm_equal_iter(px* pxsmip1, px* pxsmip2)
{
  sm_iter* smip1;
  int tag1;
  if ( !get_smip(pxsmip1,tag1,&smip1) || !smip1->is_valid ) bad_argument();
  sm_iter* smip2;
  int tag2;
  if ( !get_smip(pxsmip2,tag2,&smip2) || !smip2->is_valid ) bad_argument();
  if (tag1 != tag2) return 0; // fail
  return pure_int( smip1->iter == smip2->iter ); 
}

px* sm_get_at(px* pxsmip, int what)
{
  sm_iter* smip;
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) || !smip->is_valid ) bad_argument();
  if (smip->iter == smip->smp()->mp.end()) index_error();
  if ( what==stl_sm_elm && tag==stlset_iter_tag() ) what = stl_sm_key; 
  return get_elm_aux(smip->smp(), smip->iter, what);
}

px* sm_get_elm_at_inc(px* pxsmip)
{
  sm_iter* smip; 
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) || !smip->is_valid ) bad_argument();
  pmi& i = smip->iter;
  if ( i == smip->smp()->mp.end() ) index_error();
  int what = tag==stlset_iter_tag() ? stl_sm_key : stl_sm_elm; 
  px* ret = get_elm_aux(smip->smp(), i, what);
  i++;
  return ret;
}

px* sm_put_at(px* pxsmip, px* val)
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

px* sm_insert_hinted(px* pxsmp, px* pxsmip, px* kv)
{
  sm* smp; pmi pos;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  sm_iter* smip;
  int tag;
  if ( !get_smip(pxsmip,tag,&smip) || !smip->is_valid ) bad_argument();
  px *k, *v;
  if ( !extract_kv(smp,kv,k,v) ) bad_argument();
  if ( !same(smip->pxhsmp,pxsmp) ) bad_argument();
  try {
    pos = smp->mp.insert(smip->iter, pxhpair(k,v));
  }
  catch (px* e) {
    pure_throw(e);
  }
  return px_pointer( new sm_iter(pxsmp, pos) );
}

px* sm_insert_elm(px* pxsmp, px* kv)
{
  sm* smp; pmi pos;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  int num_inserted = 0;
  try {
    if ( !insert_aux(smp, kv, pos, num_inserted) ) bad_argument();
  }
  catch (px* e) {
    pure_throw(e);
  }
  px* it = px_pointer(new sm_iter(pxsmp, pos));
  smp->cache_pmi(pos);
  return pure_tuplel(2,it,pure_int(num_inserted));
}

int sm_insert_elms_xs(px* pxsmp, px* src)
{
  sm* smp; pmi pos;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  size_t sz = 0;
  px** elems = NULL;
  bool ok;
  int num_inserted = 0;
  try {
    if (pure_is_listv(src, &sz, &elems)) {
      for (int i = 0; i<sz; i++)
        if ( !insert_aux(smp, elems[i], pos, num_inserted) ) bad_argument();
      free(elems);
    } else if (matrix_type(src) == 0) {
      sz = matrix_size(src); 
      px** melems = (pure_expr**) pure_get_matrix_data(src);
      for (int i = 0; i<sz; i++) 
        if ( !insert_aux(smp, melems[i], pos, num_inserted) ) bad_argument();
    }
  }
  catch (px* e){
    free(elems);
    pure_throw(e);
  }
  return num_inserted;
}

int sm_insert_elms_stlmap(px* pxsmp, px* tpl)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  if (smp == rng.smp()) bad_argument();

  pxhmap& mp = smp->mp;
  size_t oldsz = mp.size();
  mp.insert(rng.beg(),rng.end());
  return mp.size() - oldsz;
}

int sm_insert_elms_stlvec(px* pxsmp, sv* sv_p)
{
  sm* smp; pmi pos;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  int num_inserted = 0;
  pxhmap& mp = smp->mp;
  try {
    for (sv::iterator i = sv_p->begin(); i!=sv_p->end(); i++)
      if ( !insert_aux(smp, *i, pos, num_inserted) ) bad_argument();
  }
  catch (px* e) {
    pure_throw(e);
  }
  return num_inserted;
}

px*  sm_swap(px* pxsmp1, px* pxsmp2)
{
  sm* smp1; sm* smp2;
  if ( !get_smp(pxsmp1, &smp1) ) failed_cond();
  if ( !get_smp(pxsmp2, &smp2) ) failed_cond();
  smp1->has_recent_pmi = false;
  smp2->has_recent_pmi = false;
  smp1->mp.swap(smp2->mp);
}

int sm_clear(px* pxsmp)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  size_t sz = smp->mp.size();
  smp->clear();
  return sz;
}

int sm_erase(px* pxsmp, px* trg)
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

bool sm_equal(px* tpl1, px* tpl2)
{
  sm_range rng1(tpl1);
  sm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument;
  if (sm_size(tpl1) != sm_size(tpl2)) return 0;
  sm* smp = rng1.smp();
  try {
    if (smp->keys_only) {
      pxhpair_first_equivalent comp(smp->px_comp);   
      return equal(rng1.beg(), rng1.end(), rng2.beg(), comp);
    }
    else {
      pxhpair_equivalent comp(smp->px_comp,smp->px_val_equal);   
      return equal(rng1.beg(), rng1.end(), rng2.beg(), comp);
    }
  }
  catch (px* e) {
    pure_throw(e);
  }
}

int sm_less(px* tpl1, px* tpl2)
{
  sm_range rng1(tpl1);
  sm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument;
  try {
    return lexicographical_compare(rng1.beg(), rng1.end(),
                                   rng2.beg(), rng2.end(), 
                                   rng1.smp()->mp.value_comp());
  }
  catch (px* e) {
    pure_throw(e);
  }
}

bool sm_includes(px* tpl1, px* tpl2)
{
  sm_range rng1(tpl1);
  sm_range rng2(tpl2);
  if (!rng1.is_valid || !rng2.is_valid) bad_argument;
  try {
    return includes(rng1.beg(), rng1.end(), rng2.beg(), rng2.end(), 
                    rng1.smp()->mp.value_comp());
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
    return px_pointer(trg);
  } catch (px* e) {
    pure_throw(e);
  }
}

px* sm_make_vector(px* tpl) 
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

void sm_fill_stlvec(px* tpl, sv* svp) 
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

px* sm_listmap(px* fun, px* tpl, int what)
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
        if (res) px_freenew(res);
        if (pxi) px_freenew(pxi);
        pure_throw(exception);
      }
    }
    px* last = pure_app(pure_app(cons,pxi),nl);
    if (res==nl)
      res = y = last;
    else {
      y->data.x[1] = px_new(last);
      y = last;
    }
  }
  if (i!=e) {
    pure_freenew(res);
    bad_argument();    
  }
  return res;
}

px* sm_listcatmap(px* fun, px* tpl, int what)
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
  if (i!=e) {
    pure_freenew(res);
    bad_argument();    
  }
  return res;  
}

px* sm_foldl(px* fun, px* val, px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  sm* smp = rng.smp();
  int mode =  smp->keys_only ? stl_sm_key : stl_sm_elm;
  pmi end = smp->mp.end();
  try {
    px* ret = sm_foldl_rng(fun, val, rng, rng.beg(), mode);
    return ret;
  } catch (px* e) {
    pure_throw(e);
  }
}

px* sm_foldl1(px* fun, px* tpl)
{
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
    return sm_foldl_rng(fun, val, rng, ++b, mode);
  } catch (px* e) {
    pure_throw(e);
  }
}

px* sm_foldr(px* fun, px* val, px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  int mode = rng.smp()->keys_only ? stl_sm_key : stl_sm_elm;
  try {
    px* ret = sm_foldr_rng(fun, val, rng, rng.end(), mode);
    return ret;
  } catch (px* x) {
    pure_throw(x);
  }
}

px* sm_foldr1(px* fun, px* tpl)
{
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
    return sm_foldr_rng(fun, val, rng, e, mode);
  } catch (px* x) {
    pure_throw(x);
  }
}

/*** Key oriented interface support ***************************************/

int sm_member(px* pxsmp, px* key)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
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

px* sm_bounding_keys(px* tpl)
{
  sm_range rng(tpl);
  if (!rng.is_valid) bad_argument;
  sm* smp = rng.smp();
  pxhmap& mp = smp->mp;
  smp->cache_pmi(rng.beg());
  smp->cache_pmi(rng.end());
  return pure_tuplel(2,iter_to_key(mp, rng.beg()),
                       iter_to_key(mp, rng.end())); 
}

px* sm_prev_key(px* pxsmp, px* key)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
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

px* sm_next_key(px* pxsmp, px* key)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  pxhmap& mp = smp->mp;
  pmi i = mp.end();
  if (mp.empty()) index_error();
  if ( !smp->get_cached_pmi(key,i) )
    i = smp->find(key);
  if ( i != mp.end() ) i++;
  smp->cache_pmi(i);
  return iter_to_key(mp, i);
}

px* sm_update(px* pxsmp, px* key, px* val)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  if (smp->keys_only) return 0; // fail for sets
  pmi pos = update_aux(smp, key, val);
  smp->cache_pmi(pos);
  return pxsmp;
}

px* sm_update_with(px* pxsmp, px* key, px* unaryfun)
{
  sm* smp;
  if (!get_smp(pxsmp,&smp) ) bad_argument();
  if (smp->keys_only) return 0; // fail for sets
  if (!smp->has_dflt) failed_cond();
  pmi i;
  if ( !smp->get_cached_pmi(key, i) ) {
    // uses default only if not already stored
    pair<pmi,bool> i_ok = smp->mp.insert(pxhpair(key,smp->dflt));
    i = i_ok.first;
    smp->cache_pmi(i);
  }  
  px* old_val = i->second;
  px* exception = 0;
  px* new_val = pure_appxl(unaryfun, &exception, 1, old_val);
  if (exception) pure_throw(exception);
  if (!new_val) bad_function();
  i->second = new_val;
  return new_val;
}
