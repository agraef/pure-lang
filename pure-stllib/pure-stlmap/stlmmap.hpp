/* stlmmap.hpp -- C++ support for stlmmap.pure
    
Copyright (c) 2012 by Peter Summerland <p.summerland@gmail.com>.

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

typedef std::multimap<pxh,pxh,pxh_pred2> pxhmmap;
typedef pxhmmap::iterator pmmi;

struct smm_iter;

struct stlmmap {
  bool keys_only;
  bool has_dflt;
  pxhmmap mmp;
  pxh px_comp;
  pxh px_val_comp;
  pxh px_val_equal;
  pxh dflt;
  std::vector<smm_iter*> smis; // smm_iters in Pure land

  stlmmap(px* key_comp, px* val_comp, px* val_equal, bool keyonly); 
  stlmmap(px* key_comp, px* val_comp, px* val_equal, bool keyonly, px* d);
  ~stlmmap();

  px* parameter_tuple();
  pmmi find(px* key);
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

/*** C interface for C++ map of PX Handles ***/

extern "C" {
  px*  stl_smm_type_tags();
  px*  stl_smm_make_empty(px* comp, px* val_comp, 
                     px* val_equal, px* dflt, int keys_only);
  void stl_smm_delete(smm* smmp);
  void stl_smm_iter_delete(smm_iter* smmip); 
  px*  stl_smm_container_info(px* tpl);
  int  stl_smm_size(px* tpl);
  bool stl_smm_empty(px* tpl); 
  int  stl_smm_count(px* pxsmmp, px* key);
  bool stl_smm_is_set(px* tpl);
  px*  stl_smm_find(px* pxsmmp, px* key, int what);
  px*  stl_smm_copy_iter(px* pxsmmip);
  px*  stl_smm_begin(px* pxsmmp);
  px*  stl_smm_end(px* pxsmmp); 
  px*  stl_smm_iter_bounds(px* pxsmmp, px* key, int what);
  px*  stl_smm_range_info(px* rng);
  px*  stl_smm_move_iter(px* pxsmmip, int dist);
  px*  stl_smm_iter_is_at(px* pxsmmip, int where);
  px*  stl_smm_iter_info(px* pxsmmip);
  px*  stl_smm_equal_iter(px* pxsmmip1, px* pxsmmip2);

  px*  stl_smm_get_at(px* pxsmmip, int what);
  px*  stl_smm_get_elm_at_inc(px* pxsmmip);
  px*  stl_smm_put_at(px* pxsmmip, px* val);
  px*  stl_smm_insert_hinted(px* pxsmmp, px* pxsmmip, px* kv);
  px*  stl_smm_insert_elm(px* pxsmmp, px* kv);
  int  stl_smm_insert(px* pxsmmp, px* src);
  int  stl_smm_insert_stlmmap(px* pxsmmp, px* tpl);
  int  stl_smm_insert_stlvec(px* pxsmmp, sv* svp);
  px*  stl_smm_swap(px* pxsmmp1, px* pxsmmp2);
  int  stl_smm_clear(px* pxsmmp);
  int  stl_smm_erase(px* pxsmmp, px* trg); 

  bool stl_smm_equal(px* tpl1, px* tlp2);
  bool stl_smm_less(px* tpl1, px* tlp2);
  bool stl_smm_includes(px* tpl1, px* tpl2);
  px*  stl_smm_setop(int op, px* tpl1, px* tpl2);

  px*  stl_smm_make_vector(px* tpl);
  void stl_smm_fill_stlvec(px* tpl, sv* svp);

  px*  stl_smm_listmap(px* fun, px* tpl, int what);
  px*  stl_smm_listcatmap(px* fun, px* tpl, int what);
  px*  stl_smm_foldl(px* fun, px* val, px* tpl);
  px*  stl_smm_foldl1(px* fun, px* tpl);
  px*  stl_smm_foldr(px* fun, px* val, px* tpl);
  px*  stl_smm_foldr1(px* fun, px* tpl);
  void stl_smm_do(px* fun, px* tpl);

  int  stl_smm_member(px* pxsmmp, px* key);
  px*  stl_smm_bounds(px* rng);
  px*  stl_smm_prev_key(px* pxsmmp, px* key);
  px*  stl_smm_next_key(px* pxsmmp, px* key);
  px*  stl_smm_get(px* pxsmmp, px* key);
  void stl_smm_put(px* pxsmmp, px* key, px* src);

}

inline px* smbeg(){return stl_begin_sym();}
inline px* smend(){return stl_end_sym();}

#endif // STLMMAP_H
