/* stlhmap.cpp -- C++ support for stlhmap.pure

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
#include "stlhmap.hpp"

using namespace std;

/*** Helpers for debugging only ************************************/

static bool sm_trace_enabled = false;

void stl_set_shm_trace(bool enable) 
{
  sm_trace_enabled = enable;
}

bool stl_shm_trace_enabled()
{
  return sm_trace_enabled;
}

/*** Helpers for stlhmap.cpp only ************************************/

enum {gi_find, gi_lower, gi_upper};

static int range_size(shm* shmp, phmi b, phmi e)
{
  size_t sz = 0;
  pxhhmap& hmp = shmp->hmp;
  if (b == hmp.begin() && e == hmp.end())
    sz = hmp.size();
  else if (b == e)
    sz = 0;
  else
    while(b++ != e) sz++;
  return sz;
}

static phmi update_aux(shm* shmp, px* k, px* v)
{
  pxhhmap& hmp = shmp->hmp;
  phmi pos;
  if ( shmp->get_cached_phmi(k, pos) ) {
    pos->second = v;
  }   
  else {
     pair<phmi,bool> i_ok = hmp.insert(pxhpair(k,v));
    if (!i_ok.second) i_ok.first->second = v;
    pos = i_ok.first;
   }
  return pos;
}

static bool extract_kv(shm* shmp, px* kv, px*& k, px*& v)
{
  bool ok = true;
  if (shmp->keys_only) {
    k = kv;
    v = NULL;
  } 
  else {
    if ( pxrocket_to_pxlhs_pxrhs(kv, &k, &v) )
      ;
    else if (shmp->has_dflt) {
      k = kv;
      v = shmp->dflt;
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
static bool insert_aux(shm* shmp, px* kv, phmi& pos, int& inserted)
{
  px *k, *v;
  bool ok = extract_kv(shmp,kv,k,v);
  if (ok) {
    if ( !shmp->get_cached_phmi(k, pos) ) {
      pair<phmi,bool> i_ok = shmp->hmp.insert(pxhpair(k,v));
      pos = i_ok.first;
      inserted += i_ok.second;
    }
  }
  return ok;
}

static px* px_pointer(shm* shmp)
{
  static ILS<px*> _sym = NULL; px*& sym = _sym();
  if (!sym) sym = pure_new(pure_symbol(pure_sym("stl::sm_delete")));
  int tag = shmp->keys_only ? stlhset_tag() : stlhmap_tag();
  px* ptr = pure_tag( tag, pure_pointer(shmp));
#ifdef STL_DEBUG
  if (stl_sm_trace_enabled())
    cerr << "TRACE SM:    new shm*: " << shmp << endl;
#endif
  return pure_sentry(sym,ptr);
}

static px* px_pointer(shm_iter* shmip)
{
  static ILS<px*> _sym = NULL; px*& sym = _sym();
  if (!sym) sym = pure_new(pure_symbol(pure_sym("stl::shm_iter_delete")));
  int tag = shmip->shmp()->keys_only ? stlhset_iter_tag() : stlhmap_iter_tag();
  px* ptr = pure_tag( tag, pure_pointer(shmip));
#ifdef STL_DEBUG
  if (stl_shm_trace_enabled())
    cerr << "TRACE SHMI:    new shmi*: " << shmip << endl;
#endif
  return pure_sentry(sym,ptr);
}

static bool get_shmp(px* pxshmp, shm** shmpp)
{
  void* ptr;
  bool ok = false;
  if ( pure_is_pointer(pxshmp, &ptr) ) {
    int tag = pure_get_tag(pxshmp);
    ok = tag == stlhmap_tag() || tag == stlhset_tag();
  }
  *shmpp = ok ? (shm*)ptr : NULL; 
  return ok;
}

static bool get_shmip(px* pxshmip, int& tag, shm_iter** itr)
{
  void* ptr;
  shm_iter* shmip;
  bool ok = pure_is_pointer(pxshmip,&ptr);
  if (ok) { 
    shmip = (shm_iter*)ptr;
    tag = pure_get_tag(pxshmip);
    if ( tag != stlhmap_iter_tag() && tag != stlhset_iter_tag() ) ok = false;
    if (ok) *itr = shmip;
  }
  return ok;
}

static phmi get_iter(shm* shmp , px* key, int mode)
{
  pxhhmap& hmp = shmp->hmp;
  phmi iter;
  if (key == shmbeg()) {
    iter = hmp.begin();
  }
  else if (key == shmend()) {
    iter = hmp.end();
  }
  else if (!shmp->get_cached_phmi(key, iter) ) {
    iter = hmp.find(pxh(key));
    shmp->cache_phmi(iter);
  }
  return iter;
}

static px* iter_to_key(const pxhhmap& hmp, const phmi& it)
{
  if (it == hmp.end()) return shmend();
  if (it == hmp.begin()) return shmbeg();
  return it->first;
}

static px* get_elm_aux(shm* shmp, phmi i, int what) 
{
  px* ret;
  pxhhmap &hmp = shmp->hmp; 
  if (i != hmp.end()) {
    switch (what) {
    case stl_shm_key:
      ret = i->first;
      break;
    case stl_shm_val:
      ret = shmp->keys_only ? i->first : i->second;
      break;
    case stl_shm_elm:
      px* key = i->first;     
      ret = shmp->keys_only ? key : pxhpair_to_pxrocket(*i);
      break;
    }
  }
  else {
    switch (what) {
    case stl_shm_key:
      ret = shmend();
      break;
    case stl_shm_val:
      if (shmp->keys_only) 
        ret = pure_int(0);
      else
        index_error();
      break;
    case stl_shm_elm:
      if (shmp->keys_only) 
        ret = pure_int(0);
      else
        index_error();
      break;
      px* key = i->first;
      return shmp->keys_only ? key : pxhpair_to_pxrocket(*i);
    }
  }
  return ret;
}

static px* shm_foldl_rng(px* fun, px* val, const shm_range rng, phmi i,  int mode)
{ 
  phmi end = rng.end();
  shm* shmp = rng.shmp();
  phmi sac_end = shmp->hmp.end(); 
  px* res = px_new(val); 
  px* exception = 0;
  while (i != end  && i != sac_end){
    phmi trg_i = i++;
    px* trg = get_elm_aux(shmp, trg_i, mode);
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

/*** shm_iter hmehmbers  ***********************************************/

shm_iter::shm_iter(px* pxshmp, phmi i) : pxhshmp(pxshmp), iter(i), is_valid(1)
{
  if (iter != shmp()->hmp.end())
    shmp()->shmis.push_back(this);
}

shm_iter::~shm_iter() 
{
  shmp()->remove_shm_iter(this);
}

shm* shm_iter::shmp () const
{
  void* ret;
  pure_is_pointer( pxhshmp, &ret);
  return static_cast<shm*>(ret);
}

ostream& operator<<(ostream& os, const shm_iter* shmip)
{
  if (shmip->is_valid) {
    if (shmip->iter == shmip->shmp()->hmp.end())
      os << "pastend iterator";
    else
      os << shmip->iter->first;
  }
  else {
    os << "invalid iterator";
  }
  return os;
}

/*** stlhmap hmehmbers  ***********************************************/

stlhmap::stlhmap(px* hash, px* eql, px *d, bool keyonly):
  hmp(5, pxh_hash(hash), pxh_pred2(eql)), keys_only(keyonly),
  has_recent_phmi(0), latest_phmi_pos(0), 
  has_dflt(1), dflt(d)
{
  //PR(stlhmap,this);  
}

stlhmap::~stlhmap()
{
  //PR(~stlhmap,this);
  assert(shmis.size()==0);
  //clear_all_iters();
}

px* stlhmap::parameter_tuple()
{
  px* hf = hmp.hash_function().pxfun();
  px* eq = hmp.key_eq().pxfun();
  px* df = has_dflt ? dflt.pxp() : pure_listl(0);
  px* bc = pure_int( hmp.bucket_count() );
  return pure_tuplel(5,pure_int(keys_only),bc,hf,eq,df);
}

phmi stlhmap::find(px* key)
{
  phmi iter;
  if (key == shmbeg())
    iter = hmp.begin();
  else if (key == shmend())
    iter = hmp.end();
  else
    iter = hmp.find(key);
  return iter;  
}

void stlhmap::cache_phmi(const phmi& i)
{
  if ( i != hmp.end() ) {
    if (!has_recent_phmi) {
      recent_phmi.clear();
      has_recent_phmi = true;
      latest_phmi_pos = 0;
    }
    size_t num_cached = recent_phmi.size();
    if (num_cached<SM_CACHE_SZ) {
      recent_phmi.push_back(i);
      latest_phmi_pos = num_cached;
    }
    else {
      latest_phmi_pos = (latest_phmi_pos + 1) % SM_CACHE_SZ;
      recent_phmi[latest_phmi_pos] = i;
    }
  }
}

bool stlhmap::get_cached_phmi(px* k, phmi& i)
{
  bool ret = false;
  if ( k != shmend() && has_recent_phmi ) {
    size_t num_cached = recent_phmi.size();
    size_t pos = 0; 
    for (; pos < num_cached; pos++) {
      if ( same(recent_phmi[pos]->first, k) ) {
        i = recent_phmi[pos];
        ret = true;
        break;
      }
    }
  }
  // PR2(get_cached_phmi,k,ret)
  return ret;
}

struct has_phmi {
  has_phmi(phmi _iter) : iter(_iter) {}
  bool operator()(shm_iter* shmip){return shmip->iter == iter;} 
  phmi iter;
};

void stlhmap::remove_shm_iter(shm_iter* shmip)
{
  shmis.erase( remove(shmis.begin(), shmis.end(), shmip), shmis.end() );
}

void stlhmap::clear_iter(phmi pos)
{
  if ( pos == hmp.end() ) return;
  if (has_recent_phmi) {
    vector<phmi>::iterator i = ::find(recent_phmi.begin(),recent_phmi.end(),pos);
    if (i != recent_phmi.end()) recent_phmi.erase(i);
  }
  vector<shm_iter*>::iterator i = shmis.begin(), pe;
  has_phmi is_trg(pos);
  while( i!=shmis.end() ) {
    if ( is_trg(*i) ) (*i)->is_valid = 0;
    i++;
  }
  pe = remove_if( shmis.begin(), shmis.end(), is_trg );
  shmis.erase( pe, shmis.end() );
}

void stlhmap::clear_all_iters()
{
  for (vector<shm_iter*>::iterator i = shmis.begin(); i!= shmis.end(); i++) {
    (*i)->is_valid = 0;
  }
  shmis.clear();
}

int stlhmap::erase(phmi pos)
{
  clear_iter(pos);
  hmp.erase(pos);
  return 1;
}

int stlhmap::erase(px* k)
{
  int ret = 0;
  if ( !hmp.empty() ) {
    ret = 1;
    phmi i;
    if ( get_cached_phmi(k, i) )
      erase(i);
    else {
      if (k == shmbeg())
        erase(hmp.begin());
      else if (k != shmend()) {
        pair<phmi,phmi> er = hmp.equal_range(k);
        ret = erase(er.first, er.second);
       }
      else
        ret = 0;
    }
  }
  return ret;
}

int stlhmap::erase(phmi first, phmi last)
{
  int ret = 0;
  for (phmi pos = first; pos != last; pos++) {
    clear_iter(pos); ret++;
  }
  hmp.erase(first, last);
  return ret;
}

void stlhmap::clear()
{
  has_recent_phmi = false;
  clear_all_iters();
  hmp.clear();
}

/*** shm_range functions *********************************************/

bool shm_range::init_from_iters(px** pxshmip, int sz)
{
  if (sz==0 || sz>2) return false;
  num_iters = sz;
  int tag;
  shm_iter* shmip;
  if ( !get_shmip(pxshmip[0],tag,&shmip) || !shmip->is_valid ) return false;

  is_valid = false;
  shm* shmp = shmip->shmp();
  pxhshmp = shmip->pxhshmp;
  begin_it = shmip->iter;
  if (num_iters == 2) {
    pxhhmap& hmp = shmp->hmp;
    if (get_shmip(pxshmip[1],tag,&shmip) && 
        shmip->is_valid && shmip->shmp()==shmp) 
      end_it = shmip->iter;
    else
      goto done;
  } 
  else {
    end_it = end_it++;
  }
  is_valid = true;
 done:
  return is_valid;
}

shm_range::shm_range(px* phmi_tuple)
{
  size_t tpl_sz;
  px** elms;
  pure_is_tuplev(phmi_tuple, &tpl_sz, &elms);
  try {
    bool ok = init_from_iters(elms, tpl_sz);
  }
  catch (px* e) {
    free(elms);
    pure_throw(e);
  }
  free(elms);
} 

shm* shm_range::shmp() const
{
  void* ptr;
  pure_is_pointer(pxhshmp, &ptr);
  return static_cast<shm*>(ptr);
}

/*** Pure interface support functions  *************************************/

px* shm_type_tags()
{
  return pure_tuplel(4, pure_int(stlhmap_tag()), pure_int(stlhmap_iter_tag()), 
                        pure_int(stlhset_tag()), pure_int(stlhset_iter_tag()));
}

px*  shm_make_empty(px* hash, px* eql, px* dflt, int keys_only)
{
  return px_pointer( new shm(hash, eql, dflt, keys_only) );
}

void shm_delete(shm* shmp){
#ifdef STL_DEBUG
  if (stl_shm_trace_enabled())
    cerr << "TRACE SHM: delete shm*: " << shmp << endl;
#endif
  delete(shmp);
}

void shm_iter_delete(shm_iter* shmip){
#ifdef STL_DEBUG
  if (stl_shm_trace_enabled())
    cerr << "TRACE SHM: delete shmi*: " << shmip << endl;
#endif
  delete(shmip);
}

px* shm_parameters(px* tpl)
{
  shm_range rng(tpl);
  if (!rng.is_valid) bad_argument();  
  return rng.shmp()->parameter_tuple();
} 

int shm_size(px* tpl)
{
  shm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return range_size(rng.shmp(), rng.beg(), rng.end());
}

bool shm_empty(px* tpl)
{
  shm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return rng.num_iters == 0 ? rng.shmp()->hmp.empty() : shm_size(tpl) == 0;
}

int  shm_count(px* pxshmp, px* key)
{
  shm* shmp;
  if ( !get_shmp(pxshmp,&shmp) ) bad_argument();
  return shmp->hmp.count(key);
}

bool shm_is_set(px* tpl)
{
  shm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  return rng.shmp()->keys_only;
}

px* shm_find(px* pxshmp, px* key, int what)
{
  shm* shmp;
  if ( !get_shmp(pxshmp,&shmp) ) bad_argument();
  px* ret = 0;
  pxhhmap& hmp  = shmp->hmp;
  int num_inserted;
  phmi i = get_iter(shmp, key, gi_find);
  if (what==stl_shm_iter_dflt && i == hmp.end() && shmp->has_dflt){
    px* dflt = shmp->dflt;
    pair<phmi,bool> i_ok = shmp->hmp.insert(pxhpair(key,dflt));
    return px_pointer(new shm_iter(pxshmp, i_ok.first));
  }
  else if (what==stl_shm_iter || what == stl_shm_iter_dflt)
    return px_pointer(new shm_iter(pxshmp, i)); 
  else
    return get_elm_aux(shmp, i, what);
}

px* shm_copy_iter(px* pxshmip)
{
  shm_iter* shmip;
  int tag;
  if ( !get_shmip(pxshmip,tag,&shmip) || !shmip->is_valid ) bad_argument();
  return px_pointer( new shm_iter(shmip->pxhshmp, phmi(shmip->iter)) );
}

px* shm_begin(px* pxshmp)
{
  shm* shmp;
  if ( !get_shmp(pxshmp, &shmp) ) failed_cond();
  return px_pointer(  new shm_iter(pxshmp, shmp->hmp.begin()) );
}

px* shm_end(px* pxshmp)
{
  shm* shmp;
  if ( !get_shmp(pxshmp, &shmp) ) failed_cond();
  return px_pointer(  new shm_iter(pxshmp, shmp->hmp.end()) );
}

px* shm_bounds(px* pxshmp, px* key, int what)
{
  shm* shmp;
  shm_iter* shmip;
  if ( !get_shmp(pxshmp, &shmp) ) failed_cond();
  pxhhmap& hmp = shmp->hmp;
  pair<phmi,phmi> l_u = hmp.equal_range(key);
  px* pxshmip_l = px_pointer( new shm_iter(pxshmp, l_u.first) );
  px* pxshmip_r = px_pointer( new shm_iter(pxshmp, l_u.second) );
  return pure_tuplel(2, pxshmip_l, pxshmip_r);
}

// valid, container, i1, i2,
px* shm_range_info(px* tpl)
{
  px* ret;
  shm_range rng(tpl);
  bool ok = rng.is_valid;
  px* valid = pure_int(ok);
  if (rng.is_valid) {
    px* container = rng.pxhshmp;
    px* beg = px_pointer( new shm_iter(rng.pxhshmp, rng.beg()) );
    px* end = px_pointer( new shm_iter(rng.pxhshmp, rng.end()) );
    ret = pure_tuplel(4, valid, container, beg, end);
  }
  else {
    px* null_ptr = pure_pointer(0);
    ret = pure_tuplel(4, valid, null_ptr, null_ptr, null_ptr);
  }
  return ret;
}

px* shm_move_iter(px* pxshmip, int count)
{
  shm_iter* shmip;
  int tag;
  if ( !get_shmip(pxshmip,tag,&shmip) ) return 0;
  if ( !shmip->is_valid ) bad_argument();
  phmi& i = shmip->iter;
  phmi beg = shmip->shmp()->hmp.begin();
  phmi end = shmip->shmp()->hmp.end();
  while (count > 0) {
    if (i == end) return pxshmip;
    count--; i++;
  }
  return pxshmip;
}

px* shm_iter_is_at(px* pxshmip, int where)
{
  px* ret;
  shm_iter* shmip;
  int tag;
  if ( !get_shmip(pxshmip,tag,&shmip) || !shmip->is_valid ) return 0;
  switch (where) {
  case stl_shm_at_beginning:
    return pure_int( shmip->shmp()->hmp.begin() == shmip->iter );
  case stl_shm_at_pastend:
    return pure_int( shmip->shmp()->hmp.end() == shmip->iter );
  default:
    bad_argument();
  }
}

// fix return is_valid, container, key, val 
px* shm_iter_info(px* pxshmip)
{
  shm_iter* shmip;
  int tag;
  if ( !get_shmip(pxshmip,tag,&shmip) ) return 0;
  px* valid = pure_int( shmip->is_valid );
  px* cont = shmip->pxhshmp;
  phmi i = shmip->iter;
  shm* shmp = shmip->shmp();
  px* key; px* val;
  if (shmip->is_valid && i != shmp->hmp.end() ) {
    key = iter_to_key(shmp->hmp, i);    
    if (shmp->keys_only) 
      val = i->first;
    else
      val = i->second;
  } 
  else {
    key = shmend();
    val = pure_listl(0);
  }
  return pure_tuplel(4,valid, cont, key, val);
}

px* shm_equal_iter(px* pxshmip1, px* pxshmip2)
{
  shm_iter* shmip1;
  int tag1;
  if ( !get_shmip(pxshmip1,tag1,&shmip1) || !shmip1->is_valid ) bad_argument();
  shm_iter* shmip2;
  int tag2;
  if ( !get_shmip(pxshmip2,tag2,&shmip2) || !shmip2->is_valid ) bad_argument();
  if (tag1 != tag2) return 0; // fail
  return pure_int( shmip1->iter == shmip2->iter ); 
}

px* shm_get_at(px* pxshmip, int what)
{
  shm_iter* shmip;
  int tag;
  if ( !get_shmip(pxshmip,tag,&shmip) || !shmip->is_valid ) bad_argument();
  if (shmip->iter == shmip->shmp()->hmp.end()) index_error();
  if ( what==stl_shm_elm && tag==stlhset_iter_tag() ) what = stl_shm_key; 
  return get_elm_aux(shmip->shmp(), shmip->iter, what);
}

px* shm_get_elm_at_inc(px* pxshmip)
{
  shm_iter* shmip; 
  int tag;
  if ( !get_shmip(pxshmip,tag,&shmip) || !shmip->is_valid ) bad_argument();
  phmi& i = shmip->iter;
  if ( i == shmip->shmp()->hmp.end() ) index_error();
  int what = tag==stlhset_iter_tag() ? stl_shm_key : stl_shm_elm; 
  px* ret = get_elm_aux(shmip->shmp(), i, what);
  i++;
  return ret;
}

px* shm_put_at(px* pxshmip, px* val)
{
  shm_iter* shmip;
  int tag;
  if ( !get_shmip(pxshmip,tag,&shmip) || !shmip->is_valid ) bad_argument();
  if ( tag != stlhmap_iter_tag() ) bad_argument();
  phmi itr = shmip->iter;
  shm* shmp = shmip->shmp();
  if (itr == shmp->hmp.end()) index_error();
  shmip->iter->second = val;
  return val;
}

px* shm_insert_hinted(px* pxshmp, px* pxshmip, px* kv)
{
  shm* shmp; phmi pos;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  shm_iter* shmip;
  int tag;
  if ( !get_shmip(pxshmip,tag,&shmip) || !shmip->is_valid ) bad_argument();
  px *k, *v;
  if ( !extract_kv(shmp,kv,k,v) ) bad_argument();
  if ( !same(shmip->pxhshmp,pxshmp) ) bad_argument();
  try {
    pos = shmp->hmp.insert(shmip->iter, pxhpair(k,v));
  }
  catch (px* e) {
    pure_throw(e);
  }
  return px_pointer( new shm_iter(pxshmp, pos) );
}

px* shm_insert_elm(px* pxshmp, px* kv)
{
  shm* shmp; phmi pos;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  int num_inserted = 0;
  try {
    if ( !insert_aux(shmp, kv, pos, num_inserted) ) bad_argument();
  }
  catch (px* e) {
    pure_throw(e);
  }
  shmp->cache_phmi(pos);
  px* it = px_pointer(new shm_iter(pxshmp, pos));
  return pure_tuplel(2,it,pure_int(num_inserted));
}

int shm_insert_elms_xs(px* pxshmp, px* src)
{
  shm* shmp; phmi pos;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  size_t sz = 0;
  px** elehms = NULL;
  bool ok;
  int num_inserted = 0;
  try {
    if (pure_is_listv(src, &sz, &elehms)) {
      for (int i = 0; i<sz; i++)
        if ( !insert_aux(shmp, elehms[i], pos, num_inserted) ) bad_argument();
      free(elehms);
    } else if (matrix_type(src) == 0) {
      sz = matrix_size(src); 
      px** hmelehms = (pure_expr**) pure_get_matrix_data(src);
      for (int i = 0; i<sz; i++) 
        if ( !insert_aux(shmp, hmelehms[i], pos, num_inserted) ) bad_argument();
    }
  }
  catch (px* e){
    free(elehms);
    pure_throw(e);
  }
  return num_inserted;
}

int shm_insert_elms_stlhmap(px* pxshmp1, px* pxshmp2)
{
  shm *shmp1, *shmp2;
  if (!get_shmp(pxshmp1,&shmp1) ) bad_argument();
  if (!get_shmp(pxshmp2,&shmp2) ) bad_argument();
  pxhhmap& hmp1 = shmp1->hmp;
  pxhhmap& hmp2 = shmp2->hmp;
  size_t oldsz = hmp1.size();
  try {
    hmp1.insert(hmp2.begin(),hmp2.end());
  }
  catch (px* e) {
    pure_throw(e);
  }
  return hmp1.size() - oldsz;
}

int shm_insert_elms_stlvec(px* pxshmp, px* tpl)
{
  shm* shmp; phmi pos;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  int num_inserted = 0;
  pxhhmap& hmp = shmp->hmp;
  try {
    for (svi i = rng.beg(); i!=rng.end(); i++)
      if ( !insert_aux(shmp, *i, pos, num_inserted) ) bad_argument();
  }
  catch (px* e) {
    pure_throw(e);
  }
  return num_inserted;
}

px*  shm_swap(px* pxshmp1, px* pxshmp2)
{
  shm* shmp1; shm* shmp2;
  if ( !get_shmp(pxshmp1, &shmp1) ) failed_cond();
  if ( !get_shmp(pxshmp2, &shmp2) ) failed_cond();
  shmp1->has_recent_phmi = false;
  shmp2->has_recent_phmi = false;
  shmp1->hmp.swap(shmp2->hmp);
}

int shm_clear(px* pxshmp)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  size_t sz = shmp->hmp.size();
  shmp->clear();
  return sz;
}

int shm_erase(px* pxshmp, px* trg)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  int res = 0;
  size_t trg_sz;
  px** elehms;
  pure_is_tuplev(trg, &trg_sz, &elehms);
  if (trg_sz == 1) {
    shm_iter* shmip;
    int tag;
    if ( !get_shmip(trg,tag,&shmip) || !shmip->is_valid ) bad_argument();
    if ( !same(pxshmp, shmip->pxhshmp) ) bad_argument();
    shmip->shmp()->erase(shmip->iter);
    res = 1;
  }
  else {
    shm_range rng(trg);
    if (!rng.is_valid) bad_argument();
    if ( !same(pxshmp, rng.pxhshmp) ) bad_argument();
    res = rng.shmp()->erase(rng.beg(), rng.end());
  }
  return res;
}

bool shm_equal(px* pxshmp1, px* pxshmp2)
{
  shm *shmp1, *shmp2;
  if ( !get_shmp(pxshmp1,&shmp1) || !get_shmp(pxshmp2,&shmp2) ) bad_argument();
  try {
    return shmp1->hmp == shmp2->hmp;
  }
  catch (px* e) {
    pure_throw(e);
  }
}

px* shm_make_vector(px* tpl) 
{
  shm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  phmi b = rng.beg();
  phmi e = rng.end();
  int sz = range_size(rng.shmp(), b, e);
  if (!sz)
    return pure_matrix_columnsv(0,NULL);
  px** bfr = (px**)malloc(sizeof(px*)*sz);
  if (rng.shmp()->keys_only) 
    transform(b, e, bfr, pxhpair_to_pxlhs);
  else
    transform(b, e, bfr, pxhpair_to_pxrocket);
  px* ret = pure_matrix_columnsv(sz, bfr);
  free(bfr);
  return ret;
}

void shm_fill_stlvec(px* tpl, sv* svp) 
{
  shm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  phmi b = rng.beg();
  phmi e = rng.end();
  if (rng.shmp()->keys_only) 
    transform(b, e, back_inserter(*svp), pxhpair_to_pxlhs);
  else
    transform(b, e, back_inserter(*svp), pxhpair_to_pxrocket);
}

/*** HMapping and folding ***********************************************/

px* shm_listmap(px* fun, px* tpl, int what)
{
  shm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  shm* shmp = rng.shmp();
  if (shmp->keys_only) what = stl_shm_key;
  phmi sac_end = shmp->hmp.end();
  phmi b = rng.beg();
  phmi e = rng.end();
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
  phmi i = b;
  for (;i != e && i != sac_end; i++){
    px* trg = get_elm_aux(shmp, i, what);
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

px* shm_listcatmap(px* fun, px* tpl, int what)
{
  shm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  shm* shmp = rng.shmp();
  if (shmp->keys_only) what = stl_shm_key;
  phmi i = rng.beg(); 
  phmi e = rng.end(); 
  phmi end = shmp->hmp.end();
  px* cons = px_cons_sym();
  px* nl = pure_listl(0);
  px* res = nl;
  px* y = 0;
  px* exception;
  px* *elms;
  size_t sz;
  for (;i != e && i != end; i++){
    px* trg = get_elm_aux(shmp, i, what);
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

px* shm_foldl(px* fun, px* val, px* tpl)
{
  shm_range rng(tpl);
  if (!rng.is_valid) bad_argument();
  shm* shmp = rng.shmp();
  int mode =  shmp->keys_only ? stl_shm_key : stl_shm_elm;
  phmi end = shmp->hmp.end();
  try {
    px* ret = shm_foldl_rng(fun, val, rng, rng.beg(), mode);
    return ret;
  } catch (px* e) {
    pure_throw(e);
  }
}

px* shm_foldl1(px* fun, px* tpl)
{
  shm_range rng(tpl);
  if ( !rng.is_valid ) bad_argument();
  shm* shmp = rng.shmp();
  int mode =  shmp->keys_only ? stl_shm_key : stl_shm_elm;
  phmi end = shmp->hmp.end();
  phmi b = rng.beg();
  phmi e = rng.end();
  if ( b==e || b==end ) bad_argument();
  px* val;
  if (mode == stl_shm_key)
    val = b->first;
  else
    val = pxlhs_pxrhs_to_pxrocket(b->first,b->second);
  try {
    return shm_foldl_rng(fun, val, rng, ++b, mode);
  } catch (px* e) {
    pure_throw(e);
  }
}


/*** Key oriented interface support ***************************************/

int shm_member(px* pxshmp, px* key)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  int ret = 0;
  pxhhmap& hmp = shmp->hmp;
  phmi i;
  if (!hmp.empty()) {
    if (shmp->get_cached_phmi(key, i) ) {
      ret = 1;
    }
    else {
      i = shmp->find(key);
      if (i != hmp.end()) {
        shmp->cache_phmi(i);
        ret = 1;
      }
    }
  }
  return ret;
}

px* shm_update(px* pxshmp, px* key, px* val)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  if (shmp->keys_only) return 0; // fail for sets
  phmi pos = update_aux(shmp, key, val);
  shmp->cache_phmi(pos);
  return val;
}

px* shm_update_with(px* pxshmp, px* key, px* unaryfun)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  if (shmp->keys_only) return 0; // fail for sets
  if (!shmp->has_dflt) failed_cond();
  phmi i;
  if ( !shmp->get_cached_phmi(key, i) ) {
    // uses default only if not already stored
    pair<phmi,bool> i_ok = shmp->hmp.insert(pxhpair(key,shmp->dflt));
    i = i_ok.first;
    shmp->cache_phmi(i);
  }  
  px* old_val = i->second;
  px* exception = 0;
  px* new_val = pure_appxl(unaryfun, &exception, 1, old_val);
  if (exception) pure_throw(exception);
  if (!new_val) bad_function();
  i->second = new_val;
  return new_val;
}
