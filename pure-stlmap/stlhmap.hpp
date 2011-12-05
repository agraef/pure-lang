/* stlhmap.hpp -- C++ support for stlhmap.pure
    
--- DRAFT - FOR DISCUSSON PURPOSES ONLY ---

Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlhmap, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlmap distribution package for details.

*/

#ifndef STLHMAP_H
#define STLHMAP_H

#define PR(x,y) cerr << #x ", " #y ": " << y << endl;
#define PR2(x,y,z) cerr << #x ", " #y ": " << y << ", " #z ": " << z << endl;

#include <iostream>
#include <unordered_map>
#include <vector>
#include "stlbase.hpp"
#include "stlvec.hpp"

typedef std::unordered_map<pxh,pxh,pxh_hash,pxh_pred2> pxhhmap;
typedef pxhhmap::iterator phmi;

const size_t SM_CACHE_SZ = 4;

struct shm_iter;

struct stlhmap {
  bool keys_only;
  bool has_dflt;
  bool has_recent_phmi;
  size_t latest_phmi_pos;
  pxhhmap hmp;
  pxh dflt;
  std::vector<phmi> recent_phmi;
  std::vector<shm_iter*> shmis; // sm_iters in Pure land

  stlhmap(px* hash, px* eql, px* dflt, bool keys_only); 
  ~stlhmap();
  px* parameter_tuple();
  phmi  find(px* key);
  bool get_cached_phmi(px* k, phmi& i);
  void cache_phmi(const phmi& i);
  void clear();
  int  erase(phmi pos);
  int  erase(px* k);
  int  erase(phmi first, phmi last);
  void clear_iter(phmi pos);
  void clear_all_iters();
  void remove_shm_iter(shm_iter*);
};

typedef stlhmap shm; 

struct shm_range {
  bool is_valid;
  int num_iters;
  pxh pxhshmp;
  phmi begin_it;
  phmi end_it;

  shm_range(px* tpl);
  bool init_from_iters(px** elems, int tlp_sz);
  bool init_from_keys(px** elems, int num_keys);
  phmi beg() const {return begin_it;}
  phmi end() const {return end_it;}
  shm* shmp() const;
};

struct shm_iter {
  bool is_valid;
  phmi iter;
  pxh pxhshmp;

  shm_iter(px* pxshmp, phmi i);
  ~shm_iter();
  shm* shmp() const; 
};

enum {stl_shm_key =1, stl_shm_val, stl_shm_elm,
      stl_shm_iter, stl_shm_iter_dflt};

enum {stl_shm_lower_bound=1, stl_shm_upper_bound, stl_shm_equal_range};

enum {stl_shm_merge = 1, stl_shm_union, stl_shm_difference, 
      stl_shm_intersection, stl_shm_symmetric_difference};

enum {stl_shm_at_beginning = 1, stl_shm_at_pastend};

/*** C interface for C++ map of PX Handles ***/

extern "C" {
  px*  shm_type_tags();
  px*  shm_make_empty(px* hash, px* eql, px* dflt, int keys_only);
  void shm_delete(shm* shmp);
  void shm_iter_delete(shm_iter* shmip);
  px*  shm_parameters(px* tpl);
  int  shm_size(px* tpl);
  bool shm_empty(px* tpl); 
  int  shm_count(px* pxshmp, px* key);
  bool shm_is_set(px* tpl);
  px*  shm_find(px* pxshmp, px* key, int what);
  px*  shm_copy_iter(px* pxshmip);
  px*  shm_begin(px* pxshmp);
  px*  shm_end(px* pxshmp); 
  px*  shm_bounds(px* pxshmp, px* key, int what);
  px*  shm_range_info(px* rng);
  px*  shm_move_iter(px* pxshmip, int dist);
  px*  shm_iter_is_at(px* pxshmip, int where);
  px*  shm_iter_info(px* pxshmip);
  px*  shm_equal_iter(px* pxshmip1, px* pxshmip2);

  px*  shm_get_at(px* pxshmip, int what);
  px*  shm_get_elm_at_inc(px* pxshmip);
  px*  shm_put_at(px* pxshmip, px* val);
  px*  shm_insert_hinted(px* pxshmp, px* pxshmip, px* kv);
  px*  shm_insert_elm(px* pxshmp, px* kv);
  int  shm_insert_elms_xs(px* pxshmp, px* src);
  int  shm_insert_elms_stlhmap(px* pxshmp1, px* pxshmp2);
  int  shm_insert_elms_stlvec(px* pxshmp, px* tpl);
  px*  shm_swap(px* pxshmp1, px* pxshmp2);
  int  shm_clear(px* pxshmp);
  int  shm_erase(px* pxshmp, px* trg); 

  bool shm_equal(px* pxshmp1, px* pxshmp2);
  px*  shm_make_vector(px* tpl);
  void shm_fill_stlvec(px* tpl, sv* svp);

  px*  shm_listmap(px* fun, px* tpl, int what);
  px*  shm_listcatmap(px* fun, px* tpl, int what);
  px*  shm_foldl(px* fun, px* val, px* tpl);
  px*  shm_foldl1(px* fun, px* tpl);
  px*  shm_foldr(px* fun, px* val, px* tpl);
  px*  shm_foldr1(px* fun, px* tpl);

  int  shm_member(px* pxshmp, px* key);
  px*  shm_bounding_keys(px* rng);
  px*  shm_prev_key(px* pxshmp, px* key);
  px*  shm_next_key(px* pxshmp, px* key);
  px*  shm_update(px* pxshmp, px* key, px* val);
  px*  shm_update_with(px* pxshmp, px* key, px* binfun);

  void stl_set_shm_trace(bool enable);
  bool stl_shm_trace_enabled();

}

inline px* shmbeg(){return stlbegin_sym();}
inline px* shmend(){return stlend_sym();}

#endif // STLHMAP_H
