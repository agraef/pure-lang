/* stlmap.hpp -- C++ support for stlmap.pure
    
Copyright (c) 2012 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlmap, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlmap distribution package for details.

*/

#ifndef STLMAP_H
#define STLMAP_H

#include <iostream>
#include <map>
#include "stlbase.hpp"

typedef std::map<pxh,pxh,pxh_pred2> pxhmap;
typedef pxhmap::iterator pmi;

struct sm_iter;

struct sm_key_iter {
  sm_key_iter(px* k, pmi i) : iter(i), key(k) {};
  pmi iter;
  pxh key;
};

struct stlmap {
  pxhmap mp;
  pxh cache_key;
  bool keys_only;
  bool has_dflt;
  pxh dflt;
  pxh px_comp;
  pxh px_val_comp;
  pxh px_val_equal;
  pmi cache_iter;
  std::vector<sm_iter*> smis; // sm_iters in Pure land

  stlmap(px* key_comp, px* val_comp, px* val_equal, bool keyonly); 
  stlmap(px* key_comp, px* val_comp, px* val_equal, bool keyonly, px* dflt);
  ~stlmap();
  px* parameter_tuple();
  pmi  find(px* key);
  void clear();
  int  erase(pmi pos);
  int  erase(px* k);
  int  erase(pmi first, pmi last);
  void invalidate_iter(pmi pos);
  void invalidate_all_iters();
  void remove_sm_iter(sm_iter*);
  void kill_cache_iter(pmi pos);
  void clear_ki_cache();
};

typedef stlmap sm; 

struct sm_range {
  bool is_valid;
  int num_iters;
  pxh pxhsmp;
  pmi begin_it;
  pmi end_it;

  sm_range(px* tpl);
  bool init_from_iters(px** elems, int tlp_sz);
  bool init_from_keys(px** elems, int num_keys);
  pmi beg() const {return begin_it;}
  pmi end() const {return end_it;}
  sm* smp() const;
};

struct sm_iter {
  pxh pxhsmp;
  pmi iter;
  bool is_valid;

  sm_iter(px* pxsmp, pmi i);
  ~sm_iter();
  sm* smp() const;
};

/*** C interface for C++ map of PX Handles ***/

extern "C" {
  px*  stl_sm_type_tags();
  px*  stl_sm_make_empty(px* comp, px* val_comp, 
                     px* val_equal, px* dflt, int keys_only);
  void stl_sm_delete(sm* smp);
  void stl_sm_iter_delete(sm_iter* smip);
  px*  stl_sm_container_info(px* tpl);
  int  stl_sm_size(px* tpl);
  bool stl_sm_empty(px* tpl); 
  int  stl_sm_count(px* pxsmp, px* key);
  bool stl_sm_is_set(px* tpl);
  px*  stl_sm_find(px* pxsmp, px* key, int what);
  px*  stl_sm_copy_iter(px* pxsmip);
  px*  stl_sm_begin(px* pxsmp);
  px*  stl_sm_end(px* pxsmp); 
  px*  stl_sm_iter_bounds(px* pxsmp, px* key, int what);
  px*  stl_sm_range_info(px* rng);
  px*  stl_sm_move_iter(px* pxsmip, int dist);
  px*  stl_sm_iter_is_at(px* pxsmip, int where);
  px*  stl_sm_iter_info(px* pxsmip);
  px*  stl_sm_equal_iter(px* pxsmip1, px* pxsmip2);

  px*  stl_sm_get_at(px* pxsmip, int what);
  px*  stl_sm_get_elm_at_inc(px* pxsmip);
  px*  stl_sm_put_at(px* pxsmip, px* val);
  px*  stl_sm_insert_hinted(px* pxsmp, px* pxsmip, px* kv);
  px*  stl_sm_insert_elm(px* pxsmp, px* kv);
  int  stl_sm_insert(px* pxsmp, px* src, bool replace);
  int  stl_sm_insert_stlmap(px* pxsmp, px* tpl, bool replace);
  int  stl_sm_insert_stlvec(px* pxsmp, sv* svp, bool replace);
  px*  stl_sm_replace(sm* smp, px* key, px* val);
  void stl_sm_swap(px* pxsmp1, px* pxsmp2);
  int  stl_sm_clear(px* pxsmp);
  int  stl_sm_erase(px* pxsmp, px* trg); 

  bool stl_sm_equal(px* tpl1, px* tlp2);
  bool stl_sm_less(px* tpl1, px* tlp2);
  bool stl_sm_includes(px* tpl1, px* tpl2);
  px*  stl_sm_setop(int op, px* tpl1, px* tpl2);

  px*  stl_sm_make_vector(px* tpl);
  void stl_sm_fill_stlvec(px* tpl, sv* svp);

  px*  stl_sm_listmap(px* fun, px* tpl, int what);
  px*  stl_sm_listcatmap(px* fun, px* tpl, int what);
  px*  stl_sm_foldl(px* fun, px* val, px* tpl);
  px*  stl_sm_foldl1(px* fun, px* tpl);
  px*  stl_sm_foldr(px* fun, px* val, px* tpl);
  px*  stl_sm_foldr1(px* fun, px* tpl);
  void stl_sm_do(px* fun, px* tpl);

  int  stl_sm_member(sm* smp, px* key);
  px*  stl_sm_bounds(px* rng);
  px*  stl_sm_prev_key(px* pxsmp, px* key);
  px*  stl_sm_next_key(px* pxsmp, px* key);
  
  px*  stl_sm_get(sm* smp, px* key);
  px*  stl_sm_put(sm* smp, px* key, px* val);
  px*  stl_sm_replace_with(px* pxsmp, px* key, px* binfun);

}

inline px* smbeg(){return stl_begin_sym();}
inline px* smend(){return stl_end_sym();}

#endif // STLMAP_H
