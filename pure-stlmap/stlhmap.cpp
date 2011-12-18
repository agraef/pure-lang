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

/*** Helpers *************************************************************/

static phmi update_aux(shm* shmp, px* k, px* v)
{
  pxhhmap& hmp = shmp->hmp;
  pair<phmi,bool> i_ok = hmp.insert(pxhpair(k,v));
  if (!i_ok.second) i_ok.first->second = v;
  return i_ok.first;
}

static bool extract_kv(shm* shmp, px* kv, px*& k, px*& v)
{
  bool ok = pxrocket_to_pxlhs_pxrhs(kv, &k, &v);
  if (shmp->keys_only) {
    if (!ok) k = kv; 
    v = NULL;
    ok = true;
  }
  return ok;
}

// increases inserted by 1 iff inserted new value
static bool insert_aux(shm* shmp, px* kv, phmi& pos, int& inserted)
{
  px *k, *v;
  bool ok = extract_kv(shmp,kv,k,v);
  if (ok) {
    pair<phmi,bool> i_ok = shmp->hmp.insert(pxhpair(k,v));
    pos = i_ok.first;
    inserted += i_ok.second;
  }
  return ok;
}

static px* px_pointer(shm* shmp)
{
  static ILS<px*> _sym = NULL; px*& sym = _sym();
  if (!sym) sym = pure_new(pure_symbol(pure_sym("stl::shm_delete")));
  int tag = shmp->keys_only ? stlhset_tag() : stlhmap_tag();
  px* ptr = pure_tag( tag, pure_pointer(shmp));
  return pure_sentry(sym,ptr);
}

static px* px_pointer(shm_iter* shmip)
{
  static ILS<px*> _sym = NULL; px*& sym = _sym();
  if (!sym) sym = pure_new(pure_symbol(pure_sym("stl::shm_iter_delete")));
  int tag = shmip->shmp()->keys_only ? stlhset_iter_tag() : stlhmap_iter_tag();
  px* ptr = pure_tag( tag, pure_pointer(shmip));
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

static phmi get_iter(shm* shmp , px* key)
{
  pxhhmap& hmp = shmp->hmp;
  phmi iter;
  if (key == shmbeg())
    iter = hmp.begin();
  else if (key == shmend())
    iter = hmp.end();
  else  
    iter = hmp.find(pxh(key));
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
      if (shmp->keys_only)
        ret = i->first;
      else
        ret = i->second; 
      break;
    case stl_shm_elm:
      px* key = i->first;     
      ret = shmp->keys_only ? key : pxhpair_to_pxrocket(*i);
      break;
    }
  }
  else {
    if (what==stl_shm_key)
      ret = shmend(); 
    else 
      index_error();
  }
  return ret;
}

static px* shm_foldl_rng(px* fun, px* val, shm* shmp, phmi i, int mode)
{ 
  phmi end = shmp->hmp.end(); 
  px* res = px_new(val); 
  px* exception = 0;
  while (i != end){
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
  return res;
}

/*** shm_iter mehmbers  ***********************************************/

shm_iter::shm_iter(px* pxshmp, phmi i) : pxhshmp(pxshmp), iter(i){}

shm* shm_iter::shmp () const
{
  void* ret;
  pure_is_pointer( pxhshmp, &ret);
  return static_cast<shm*>(ret);
}

ostream& operator<<(ostream& os, const shm_iter* shmip)
{
  if (shmip->iter == shmip->shmp()->hmp.end())
    os << "pastend iterator";
  else
    os << shmip->iter->first;
  return os;
}

/*** stlhmap members  ***********************************************/

stlhmap::stlhmap(px* hash, px* keql, px* veql, bool keyonly):
  keys_only(keyonly), hmp(0, pxh_hash(hash), pxh_pred2(keql)),
  px_key_equal(keql), px_val_equal(veql)  
{
  //PR(stlhmap,this);  
}

struct has_phmi {
  has_phmi(phmi _iter) : iter(_iter) {}
  bool operator()(shm_iter* shmip){return shmip->iter == iter;} 
  phmi iter;
};

/*** Pure interface support functions  *************************************/

px* shm_type_tags()
{
  return pure_tuplel(4, pure_int(stlhmap_tag()), pure_int(stlhmap_iter_tag()), 
                        pure_int(stlhset_tag()), pure_int(stlhset_iter_tag()));
}

px*  shm_make_empty(px* hash, px* eql, int keys_only)
{
  return px_pointer( new shm(hash, eql, keys_only) );
}

px*  shm_copy(px* pxshmp)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  pxhhmap& hmp = shmp->hmp;
  return px_pointer( new shm(*shmp) );
}

void shm_delete(shm* shmp){
  delete(shmp);
}

void shm_reserve(px* pxshmp, double max_load, int count)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  pxhhmap& hmp = shmp->hmp;  
  if (max_load > 0.0) 
    hmp.max_load_factor(max_load);
  if (count > 0)
    hmp.reserve(count);
}

void shm_iter_delete(shm_iter* shmip)
{
  delete(shmip);
}

px* shm_hash_info(px* pxshmp)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  pxhhmap& hmp = shmp->hmp;
  px* ko = pure_int(shmp->keys_only);
  px* bc = pure_int( hmp.bucket_count() );
  px* lf = pure_double( hmp.load_factor() );
  px* mlf = pure_double( hmp.max_load_factor() );
  px* hf = hmp.hash_function().pxfun();
  px* eq = hmp.key_eq().pxfun();
  return pure_tuplel(6,ko,bc,lf,mlf,hf,eq);
}

int shm_size(px* pxshmp)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  return shmp->hmp.size();
}

bool shm_empty(px* pxshmp)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  return shmp->hmp.empty();
}

int  shm_count(px* pxshmp, px* key)
{
  shm* shmp;
  if ( !get_shmp(pxshmp,&shmp) ) bad_argument();
  return shmp->hmp.count(key);
}

bool shm_is_set(px* pxshmp)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  return shmp->keys_only;
}

px* shm_find(px* pxshmp, px* key, int what)
{
  shm* shmp;
  if ( !get_shmp(pxshmp,&shmp) ) bad_argument();
  px* ret = 0;
  phmi i = get_iter(shmp, key);
  return get_elm_aux(shmp, i, what);
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
  return pure_int(num_inserted);
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
  shmp1->hmp.swap(shmp2->hmp);
}

int shm_clear(px* pxshmp)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  size_t sz = shmp->hmp.size();
  shmp->hmp.clear();
  return sz;
}

int shm_erase(px* pxshmp, px* key)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  pxhhmap& hmp = shmp->hmp;
  size_t sz = hmp.size();
  try {
    hmp.erase(key);
  } 
  catch (px* e) {
    pure_throw(e);
  }
  return sz - hmp.size();
}

bool shm_equal(px* pxshmp1, px* pxshmp2)
{
  shm *shmp1, *shmp2;
  if ( !get_shmp(pxshmp1,&shmp1) || !get_shmp(pxshmp2,&shmp2) ) bad_argument();
  pxhhmap& hmp1 = shmp1->hmp;
  pxhhmap& hmp2 = shmp2->hmp;
  try {
    if (shmp1->keys_only) {
      pxhpair_first_equal comp(shmp1->px_key_equal);   
      return equal(hmp1.begin(), hmp1.end(), hmp2.begin(), comp);
    }
    else {
      pxhpair_equal comp(shmp1->px_key_equal,shmp1->px_val_equal);   
      return equal(hmp1.begin(), hmp1.end(), hmp2.begin(), comp);
    }
  }
  catch (px* e) {
    pure_throw(e);
  }
}

px* shm_make_vector(px* pxshmp) 
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  pxhhmap& hmp = shmp->hmp;
  phmi b = hmp.begin();
  phmi e = hmp.end();
  int sz = hmp.size();
  if (!sz)
    return pure_matrix_columnsv(0,NULL);
  px** bfr = (px**)malloc(sizeof(px*)*sz);
  if (shmp->keys_only) 
    transform(b, e, bfr, pxhpair_to_pxlhs);
  else
    transform(b, e, bfr, pxhpair_to_pxrocket);
  px* ret = pure_matrix_columnsv(sz, bfr);
  free(bfr);
  return ret;
}

void shm_fill_stlvec(px* pxshmp, sv* svp) 
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  pxhhmap& hmp = shmp->hmp;
  phmi b = hmp.begin();
  phmi e = hmp.end();
  if (shmp->keys_only) 
    transform(b, e, back_inserter(*svp), pxhpair_to_pxlhs);
  else
    transform(b, e, back_inserter(*svp), pxhpair_to_pxrocket);
}

/*** Mapping and folding ***********************************************/

px* shm_listmap(px* fun, px* pxshmp, int what)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  if (shmp->keys_only) what = stl_shm_key;
  pxhhmap& hmp = shmp->hmp;
  phmi b = hmp.begin();
  phmi e = hmp.end();
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
  for (;i != e; i++){
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
  return res;
}

px* shm_listcatmap(px* fun, px* pxshmp, int what)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  if (shmp->keys_only) what = stl_shm_key;
  pxhhmap& hmp = shmp->hmp;
  phmi i = hmp.begin();
  phmi e = hmp.end();
  px* cons = px_cons_sym();
  px* nl = pure_listl(0);
  px* res = nl;
  px* y = 0;
  px* exception;
  px* *elms;
  size_t sz;
  for (;i != e; i++){
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
  return res;  
}

px* shm_foldl(px* fun, px* val, px* pxshmp)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  int mode =  shmp->keys_only ? stl_shm_key : stl_shm_elm;
  try {
    px* ret = shm_foldl_rng(fun, val, shmp, shmp->hmp.begin(), mode);
    return ret;
  } catch (px* e) {
    pure_throw(e);
  }
}

px* shm_foldl1(px* fun, px* pxshmp)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  int mode =  shmp->keys_only ? stl_shm_key : stl_shm_elm;
  phmi e = shmp->hmp.end();
  phmi b = shmp->hmp.begin();
  if ( b==e) bad_argument();
  px* val;
  if (mode == stl_shm_key)
    val = b->first;
  else
    val = pxlhs_pxrhs_to_pxrocket(b->first,b->second);
  try {
    return shm_foldl_rng(fun, val, shmp, ++b, mode);
  } catch (px* e) {
    pure_throw(e);
  }
}

/*** Key oriented interface support ***************************************/

int shm_member(px* pxshmp, px* key)
{
  shm* shmp;
  if (!get_shmp(pxshmp,&shmp) ) bad_argument();
  return get_iter(shmp, key) != shmp->hmp.end();
}

px* shm_update(px* pxshmp, px* key, px* val)
{
  shm* shmp;
  if ( !get_shmp(pxshmp,&shmp) ) bad_argument();
  if (shmp->keys_only) return 0; // fail for sets
  phmi pos = update_aux(shmp, key, val);
  return val;
}

/*** Lazy list iterator support ******************************************/

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

px* shm_get_elm_at_inc(px* pxshmip)
{
  shm_iter* shmip; 
  int tag;
  if ( !get_shmip(pxshmip,tag,&shmip) ) bad_argument();
  phmi& i = shmip->iter;
  if ( i == shmip->shmp()->hmp.end() ) index_error();
  int what = tag==stlset_iter_tag() ? stl_shm_key : stl_shm_elm; 
  px* ret = get_elm_aux(shmip->shmp(), i, what);
  i++;
  return ret;
}

px* shm_equal_iter(px* pxshmip1, px* pxshmip2)
{
  shm_iter* shmip1;
  int tag1;
  if ( !get_shmip(pxshmip1,tag1,&shmip1) ) bad_argument();
  shm_iter* shmip2;
  int tag2;
  if ( !get_shmip(pxshmip2,tag2,&shmip2) ) bad_argument();
  if (tag1 != tag2) return 0; // fail
  return pure_int( shmip1->iter == shmip2->iter ); 
}

