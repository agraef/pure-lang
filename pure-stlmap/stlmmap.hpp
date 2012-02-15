/* stlmmap.hpp -- C++ support for stlmmap.pure
    
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

#ifndef STLMMAP_H
#define STLMMAP_H

#include <iostream>
#include <map>
#include "stlbase.hpp"
#include "stlvec.hpp"

typedef std::multimap<pxh,pxh,pxh_pred2> pxhmmap;
typedef pxhmmap::iterator pmmi;

const size_t SM_CACHE_SZ = 4;

struct smm_iter;

struct stlmmap {
  bool keys_only;
  bool has_dflt;
  bool has_recent_pmmi;
  size_t latest_pmmi_pos;
  pxhmmap mmp;
  pxh px_comp;
  pxh px_val_comp;
  pxh px_val_equal;
  pxh dflt;
  std::vector<pmmi> recent_pmmi;
  std::vector<smm_iter*> smis; // smm_iters in Pure land

  stlmmap(px* key_comp, px* val_comp, px* val_equal, bool keyonly); 
  stlmmap(px* key_comp, px* val_comp, px* val_equal, bool keyonly, px* d);
  ~stlmmap();

  px* parameter_tuple();
  pmmi  find(px* key);
  bool get_cached_pmmi(px* k, pmmi& i);
  void cache_pmmi(const pmmi& i);
  void clear();
  int  erase(pmmi pos);
  int  erase(px* k);
  int  erase(pmmi first, pmmi last);
  void clear_iter(pmmi pos);
  void clear_all_iters();
  void remove_smm_iter(smm_iter*);
};

typedef stlmmap smm; 

struct smm_range {
  bool is_valid;
  pxh pxhsmmp;
  int num_iters;
  pmmi begin_it;
  pmmi end_it;

  smm_range(px* tpl);
  bool init_from_iters(px** elems, int tlp_sz);
  bool init_from_keys(px** elems, int num_keys);
  pmmi beg() const {return begin_it;}
  pmmi end() const {return end_it;}
  smm* smmp() const;
};

struct smm_iter {
  bool is_valid;
  pmmi iter;
  pxh pxhsmmp;

  smm_iter(px* pxsmmp, pmmi i);
  ~smm_iter();
  smm* smmp() const; 
};

enum {stl_smm_key =1, stl_smm_val, stl_smm_elm,
      stl_smm_iter, stl_smm_iter_dflt};

enum {stl_smm_lower_bound=1, stl_smm_upper_bound, stl_smm_equal_range};

enum {stl_smm_merge = 1, stl_smm_union, stl_smm_difference, 
      stl_smm_intersection, stl_smm_symmetric_difference};

enum {stl_smm_at_beginning = 1, stl_smm_at_pastend};

/*** C interface for C++ map of PX Handles ***/

extern "C" {
  px*  smm_type_tags();
  px*  smm_make_empty(px* comp, px* val_comp, 
                     px* val_equal, px* dflt, int keys_only);
  void smm_delete(smm* smmp);
  void smm_iter_delete(smm_iter* smmip); 
  px*  smm_container_info(px* tpl);
  int  smm_size(px* tpl);
  bool smm_empty(px* tpl); 
  int  smm_count(px* pxsmmp, px* key);
  bool smm_is_set(px* tpl);
  px*  smm_find(px* pxsmmp, px* key, int what);
  px*  smm_copy_iter(px* pxsmmip);
  px*  smm_begin(px* pxsmmp);
  px*  smm_end(px* pxsmmp); 
  px*  smm_bounds(px* pxsmmp, px* key, int what);
  px*  smm_range_info(px* rng);
  px*  smm_move_iter(px* pxsmmip, int dist);
  px*  smm_iter_is_at(px* pxsmmip, int where);
  px*  smm_iter_info(px* pxsmmip);
  px*  smm_equal_iter(px* pxsmmip1, px* pxsmmip2);

  px*  smm_get_at(px* pxsmmip, int what);
  px*  smm_get_elm_at_inc(px* pxsmmip);
  px*  smm_put_at(px* pxsmmip, px* val);
  px*  smm_insert_hinted(px* pxsmmp, px* pxsmmip, px* kv);
  px*  smm_insert_elm(px* pxsmmp, px* kv);
  int  smm_insert(px* pxsmmp, px* src);
  int  smm_insert_stlmmap(px* pxsmmp, px* tpl);
  int  smm_insert_stlvec(px* pxsmmp, sv* svp);
  px*  smm_swap(px* pxsmmp1, px* pxsmmp2);
  int  smm_clear(px* pxsmmp);
  int  smm_erase(px* pxsmmp, px* trg); 

  bool smm_equal(px* tpl1, px* tlp2);
  int  smm_less(px* tpl1, px* tlp2);
  bool smm_includes(px* tpl1, px* tpl2);
  px*  smm_setop(int op, px* tpl1, px* tpl2);

  px*  smm_make_vector(px* tpl);
  void smm_fill_stlvec(px* tpl, sv* svp);

  px*  smm_listmap(px* fun, px* tpl, int what);
  px*  smm_listcatmap(px* fun, px* tpl, int what);
  px*  smm_foldl(px* fun, px* val, px* tpl);
  px*  smm_foldl1(px* fun, px* tpl);
  px*  smm_foldr(px* fun, px* val, px* tpl);
  px*  smm_foldr1(px* fun, px* tpl);
  void smm_do(px* fun, px* tpl);

  int  smm_member(px* pxsmmp, px* key);
  px*  smm_bounding_keys(px* rng);
  px*  smm_prev_key(px* pxsmmp, px* key);
  px*  smm_next_key(px* pxsmmp, px* key);
  px*  smm_update(px* pxsmmp, px* key, px* src);

  void stl_set_smm_trace(bool enable);
  bool stl_smm_trace_enabled();

}

inline px* smbeg(){return stlbegin_sym();}
inline px* smend(){return stlend_sym();}

#endif // STLMMAP_H
