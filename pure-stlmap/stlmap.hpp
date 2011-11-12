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
#include <vector>
#include "stlbase.hpp"
#include "stlvec.hpp"

typedef std::map<pxh,pxh,pxh_pred2> pxhmap;
typedef pxhmap::iterator pmi;

const size_t SM_CACHE_SZ = 4;

struct stlmap {
  stlmap(px* key_comp, px* val_comp, px* val_equal, bool keyonly); 
  stlmap(px* key_comp, px* val_comp, px* val_equal, bool keyonly, px* d);

  pmi  find(px* key);
  bool get_cached_pmi(px* k, pmi& i);
  void cache_pmi(const pmi& i);
  void clear();
  int  erase(pmi pos);
  int  erase(px* k);
  int  erase(pmi first, pmi last);

  pxhmap mp;
  pxh px_comp;
  pxh px_val_comp;
  pxh px_val_equal;
  pxh dflt;
  bool has_recent_pmi;
  size_t latest_pmi_pos;
  std::vector<pmi> recent_pmi;
  bool has_dflt;
  bool keys_only;
};

typedef stlmap sm; 

struct sm_iters {
  sm* smp;
  bool is_valid;
  int num_iters;
  pmi begin_it;
  pmi end_it;
  sm_iters(px* tpl);
  pmi beg(){return begin_it;}
  pmi end(){return end_it;}
};

struct sm_insert_iter {
  sm* smp;
  bool is_valid;
  sm_insert_iter(px* tpl);
};

px* iter_key(sm* smp, pmi iter);

enum {stl_sm_key = 1, stl_sm_val, stl_sm_both};

enum {stl_sm_merge = 1, stl_sm_union, stl_sm_difference, 
      stl_sm_intersection, stl_sm_symmetric_difference};

/*** C interface for C++ map of PX Handles ***/

extern "C" {
  sm*  sm_make_empty(px* comp, px* val_comp, px* val_equal, int keys_only);
  void sm_delete(sm* smp);
  bool sm_is_set(px* tpl);
  bool sm_equal(px* tpl1, px* tlp2);
  int  sm_less(px* tpl1, px* tlp2);
  bool sm_includes(px* tpl1, px* tpl2);
  sm*  sm_setop(int op, px* tpl1, px* tpl2);
  px*  sm_make_vector(px* tpl);
  sv*  sm_make_stlvec(px* tpl);
  px*  sm_set_default(sm* smp, px* val);
  px*  sm_get_default(sm* smp);
  int  sm_size(px* tpl);
  px*  sm_bounds(px* tpl);
  int  sm_member(sm* smp, px* key);
  px*  sm_prev_key(sm* smp, px* key);
  px*  sm_next_key(sm* smp, px* key);
  px*  sm_get_elm(sm* smp, px* key, int what);
  px*  sm_update(sm* smp, px* key, px* val);
  px*  sm_update_with(sm* smp, px* key, px* binfun);
  int  sm_insert_elm(sm* smp, px* kv);
  int  sm_insert_elms_xs(sm* smp, px* src);
  int  sm_insert_elms_stlmap(sm* smp, px* tpl);
  int  sm_insert_elms_stlvec(sm* smp, px* tpl);
  int  sm_clear(sm* smp);
  int  sm_erase(px* tpl);
  int  sm_erase_if(px* pred, px* it);
  int  sm_remove(sm* smp, px* x);
  int  sm_remove_if(sm* smp, px* x, px* pred);
  px*  sm_listmap(px* fun, px* tpl, int what);
  px*  sm_listcatmap(px* fun, px* tpl, int what);
  px*  sm_foldl(px* fun, px* val, px* tpl);
  px*  sm_foldl1(px* fun, px* tpl);
  px*  sm_foldr(px* fun, px* val, px* tpl);
  px*  sm_foldr1(px* fun, px* tpl);

  void stl_set_sm_trace(bool enable);
  bool stl_sm_trace_enabled();

}

inline px* smbeg(){return stl_begin();}
inline px* smend(){return stl_end();}
inline px* pminsert(){return stl_insert();}

#endif // STLMAP_H
