/* stlmap.hpp -- C++ support for stlmap.pure
    
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

#ifndef STLMAP_H
#define STLMAP_H

#include <iostream>
#include <map>
#include "stlbase.hpp"

typedef std::map<pxh,pxh,pxh_less> pxhmap;
typedef pxhmap::iterator pmi;

const size_t SM_CACHE_SZ = 3;  // must be 3 for cache to work right

struct sm_iter;

struct sm_key_iter {
  sm_key_iter(px* k, pmi i) : iter(i), key(k) {};
  pxh key;
  pmi iter;
};

struct stlmap {
  bool keys_only;
  bool has_dflt;
  int last_in_pos;
  pxhmap mp;
  pxh px_comp;
  pxh px_val_comp;
  pxh px_val_equal;
  pxh dflt;
  std::vector<sm_key_iter> ki_cache;
  std::vector<sm_iter*> smis; // sm_iters in Pure land

  stlmap(px* key_comp, px* val_comp, px* val_equal, bool keyonly); 
  stlmap(px* key_comp, px* val_comp, px* val_equal, bool keyonly, px* dflt);
  ~stlmap();
  px* parameter_tuple();
  pmi  find(px* key);
  bool get_cached_pmi(px* k, pmi& i); 
  void cache_pmi(px* key, pmi& i);
  void clear();
  int  erase(pmi pos);
  int  erase(px* k);
  int  erase(pmi first, pmi last);
  void invalidate_iter(pmi pos);
  void invalidate_all_iters();
  void remove_sm_iter(sm_iter*);
  void kill_ki_cache_elm(pmi pos);
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
  bool is_valid;
  pmi iter;
  pxh pxhsmp;

  sm_iter(px* pxsmp, pmi i);
  ~sm_iter();
  sm* smp() const; 
};

enum {stl_sm_key =1, stl_sm_val, stl_sm_elm,
      stl_sm_iter, stl_sm_iter_dflt};

enum {stl_sm_lower_bound=1, stl_sm_upper_bound, stl_sm_equal_range};

enum {stl_sm_merge = 1, stl_sm_union, stl_sm_difference, 
      stl_sm_intersection, stl_sm_symmetric_difference};

enum {stl_sm_at_beginning = 1, stl_sm_at_pastend};

/*** C interface for C++ map of PX Handles ***/

extern "C" {
  px*  sm_type_tags();
  px*  sm_make_empty(px* comp, px* val_comp, 
                     px* val_equal, px* dflt, int keys_only);
  void sm_delete(sm* smp);
  void sm_iter_delete(sm_iter* smip);
  px*  sm_container_info(px* tpl);
  int  sm_size(px* tpl);
  bool sm_empty(px* tpl); 
  int  sm_count(px* pxsmp, px* key);
  bool sm_is_set(px* tpl);
  px*  sm_find(px* pxsmp, px* key, int what);
  px*  sm_copy_iter(px* pxsmip);
  px*  sm_begin(px* pxsmp);
  px*  sm_end(px* pxsmp); 
  px*  sm_bounds(px* pxsmp, px* key, int what);
  px*  sm_range_info(px* rng);
  px*  sm_move_iter(px* pxsmip, int dist);
  px*  sm_iter_is_at(px* pxsmip, int where);
  px*  sm_iter_info(px* pxsmip);
  px*  sm_equal_iter(px* pxsmip1, px* pxsmip2);

  px*  sm_get_at(px* pxsmip, int what);
  px*  sm_get_elm_at_inc(px* pxsmip);
  px*  sm_put_at(px* pxsmip, px* val);
  px*  sm_insert_hinted(px* pxsmp, px* pxsmip, px* kv);
  px*  sm_insert_elm(px* pxsmp, px* kv);
  int  sm_insert(px* pxsmp, px* src);
  int  sm_insert_elms_stlmap(px* pxsmp, px* tpl);
  int  sm_insert_elms_stlvec(px* pxsmp, sv* svp);
  px*  sm_swap(px* pxsmp1, px* pxsmp2);
  int  sm_clear(px* pxsmp);
  int  sm_erase(px* pxsmp, px* trg); 

  bool sm_equal(px* tpl1, px* tlp2);
  int  sm_less(px* tpl1, px* tlp2);
  bool sm_includes(px* tpl1, px* tpl2);
  px*  sm_setop(int op, px* tpl1, px* tpl2);

  px*  sm_make_vector(px* tpl);
  void sm_fill_stlvec(px* tpl, sv* svp);

  px*  sm_listmap(px* fun, px* tpl, int what);
  px*  sm_listcatmap(px* fun, px* tpl, int what);
  px*  sm_foldl(px* fun, px* val, px* tpl);
  px*  sm_foldl1(px* fun, px* tpl);
  px*  sm_foldr(px* fun, px* val, px* tpl);
  px*  sm_foldr1(px* fun, px* tpl);
  void sm_do(px* fun, px* tpl);

  int  sm_member(px* pxsmp, px* key);
  px*  sm_bounding_keys(px* rng);
  px*  sm_prev_key(px* pxsmp, px* key);
  px*  sm_next_key(px* pxsmp, px* key);
  px*  sm_update(px* pxsmp, px* key, px* val);
  px*  sm_update_with(px* pxsmp, px* key, px* binfun);

}

inline px* smbeg(){return stlbegin_sym();}
inline px* smend(){return stlend_sym();}

#endif // STLMAP_H
