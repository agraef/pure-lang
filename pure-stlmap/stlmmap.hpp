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

struct stlmmap {
  stlmmap(px* cmp, bool keyonly); 
  stlmmap(px* cmp, bool keyonly, px* d);

  pmmi find(px* key);
  bool get_cached_pmmi(px* k, pmmi& i);
  void cache_pmmi(pmmi i);
  void clear();
  int  erase(pmmi pos);
  int  erase(px* k);
  int  erase(pmmi first, pmmi last);

  pxhmmap mp;
  pxh px_comp;
  pxh dflt;
  bool has_recent_pmmi;
  pmmi recent_pmmi;
  bool has_dflt;
  bool keys_only;
};

typedef stlmmap smm; 

struct smm_iters {
  smm* smmp;
  bool is_valid;
  int num_iters;
  pmmi begin_it;
  pmmi end_it;
  smm_iters(px* tpl);
  pmmi beg(){return begin_it;}
  pmmi end(){return end_it;}
};

struct smm_insert_iter {
  smm* smmp;
  bool is_valid;
  smm_insert_iter(px* tpl);
};

px* iter_key(smm* smmp, pmmi iter);

enum {stl_smm_key = 1, stl_smm_val, stl_smm_both};

enum {stl_smm_union = 1, stl_smm_difference, 
      stl_smm_intersection, stl_smm_symmetric_difference};

/*** C interface for C++ multimap of PX Handles ***/

extern "C" {
  smm*  smm_make_empty(px* comp, int keys_only);
  bool smm_is_set(px* tpl);
  bool smm_includes(px* tpl1, px* tpl2);
  smm*  smm_setop(int op, px* tpl1, px* tpl2);
  void smm_delete(smm* smmp);
  px*  smm_make_vector(px* tpl);
  sv*  smm_make_stlvec(px* tpl);
  px*  smm_set_default(smm* smmp, px* val);
  px*  smm_get_default(smm* smmp);
  int  smm_size(px* tpl);
  px*  smm_bounds(px* tpl);
  int  smm_member(smm* smmp, px* key);
  px*  smm_prev(smm* smmp, px* key);
  px*  smm_next(smm* smmp, px* key);
  px*  smm_get(smm* smmp, px* key);
  px*  smm_first(px* tpl);
  px*  smm_last(px* tpl);
  px*  smm_update(smm* smmp, px* key, px* val);
  px*  smm_update_vals_xs(smm* smmp, px* k, px* src);
  void smm_insert_elm(smm* smmp, px* kv);
  void smm_insert_elms_xs(smm* smmp, px* src);
  void smm_insert_elms_stlmmap(smm* smmp, px* tpl);
  void smm_insert_elms_stlvec(smm* smmp, px* tpl);
  void smm_rmfirst(px* tpl);
  void smm_rmlast(px* tpl);
  int  smm_clear(smm* smmp);
  int  smm_erase(px* tpl);
  int  smm_erase_if(px* pred, px* tpl);
  int  smm_erase_first(px* tpl);
  int  smm_erase_first_if(px* pred, px* tpl);
  bool smm_allpairs(px* fun, px* tpl1, px* tpl2);
  px*  smm_listmap(px* fun, px* tpl, int what);
  px*  smm_listcatmap(px* fun, px* tpl, int what);
  px*  smm_foldl(px* fun, px* val, px* tpl);
  px*  smm_foldl1(px* fun, px* tpl);
  px*  smm_foldr(px* fun, px* val, px* tpl);
  px*  smm_foldr1(px* fun, px* tpl);

  void stl_set_smm_trace(bool enable);
  bool stl_smm_trace_enabled();

}

inline px* smmbeg(){return stl_begin();}
inline px* smmend(){return stl_end();}
inline px* smminsert(){return stl_insert();}

#endif // STLMMAP_H
