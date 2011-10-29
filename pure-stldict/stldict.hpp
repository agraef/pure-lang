/* stldict.hpp -- C++ support for stldict.pure
    
--- DRAFT - FOR DISCUSSON PURPOSES ONLY ---

Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stldict, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlvec distribution package for details.

*/

#ifndef STLDICT_H
#define STLDICT_H

#include <iostream>
#include <map>
#include "stlbase.hpp"
#include "stlvec.hpp"

/* sd, sdi and friends - A sd is a struct that has a map of pxh's and cache of
   recently used iterators. It is the data structure pointed to by a sd on the
   Pure side of the interface. A sdi is an iterator on an sd's map.*/

typedef std::map<pxh,pxh,pxh_pred2> sdmap;
typedef sdmap::iterator sdi;

struct stldict {
  stldict(px* cmp, bool keyonly); 
  stldict(px* cmp, bool keyonly, px* d);

  sdi  find(px* key);
  bool get_cached_sdi(px* k, sdi& i);
  void cache_sdi(sdi& i);
  void clear();
  void erase(sdi pos);
  int erase(px* k);
  void erase(sdi first, sdi last);

  sdmap mp;
  pxh px_comp;
  pxh dflt;
  bool has_recent_sdi;
  sdi recent_sdi;
  bool has_dflt;
  bool keys_only;
};

typedef stldict sd; 

struct sd_iters {
  sd* dict;
  bool is_valid;
  sdi begin_it;
  sdi end_it;
  sd_iters(px* tpl);
  sdi beg(){return begin_it;}
  sdi end(){return end_it;}
};

struct sd_insert_iter {
  sd* dict;
  bool is_valid;
  sd_insert_iter(px* tpl);
};

px* iter_key(sd* dict, sdi iter);

enum {stl_sd_key = 1, stl_sd_val, stl_sd_both};

enum {stl_sd_union = 1, stl_sd_difference, 
      stl_sd_intersection, stl_sd_symmetric_difference};

/*** C interface for C++ map of PX Handles ***/

extern "C" {
  sd*  sd_make_empty(px* comp, int keys_only);
  bool sd_is_set(px* tpl);
  sd*  sd_setop(int op, px* tpl1, px* tpl2);
  void sd_delete(sd* dict);
  px*  sd_make_vector(px* tpl);
  sv*  sd_make_stlvec(px* tpl);
  px*  sd_set_default(sd* dict, px* val);
  px*  sd_get_default(sd* dict);
  int  sd_size(px* tpl);
  px*  sd_bounds(px* tpl);
  int  sd_member(sd* dict, px* key);
  px*  sd_prev(sd* dict, px* key);
  px*  sd_next(sd* dict, px* key);
  px*  sd_get(sd* dict, px* key);
  px*  sd_first(px* tpl);
  px*  sd_last(px* tpl);
  px*  sd_update(sd* dict, px* key, px* val);
  px*  sd_update_with(sd* dict, px* key, px* binfun);
  void sd_insert_elm(sd* dict, px* kv);
  void sd_insert_elms_xs(sd* dict, px* src);
  void sd_insert_elms_stldict(sd* dict, px* tpl);
  void sd_insert_elms_stlvec(sd* dict, px* tpl);
  void sd_rmfirst(px* tpl);
  void sd_rmlast(px* tpl);
  void sd_erase(px* tpl);
  void sd_clear(sd* dict);
  void sd_remove(sd* dict, px* x);
  int  sd_remove_all(sd* dict, px* x);
  void sd_remove_kv(sd* dict, px* x);
  bool sd_allpairs(px* fun, px* tpl1, px* tpl2);
  px*  sd_listmap(px* fun, px* tpl, int what);
  px*  sd_listcatmap(px* fun, px* tpl, int what);
  px*  sd_foldl(px* fun, px* val, px* tpl);
  px*  sd_foldl1(px* fun, px* tpl);
  px*  sd_foldr(px* fun, px* val, px* tpl);
  px*  sd_foldr1(px* fun, px* tpl);

  void stl_set_sd_trace(bool enable);
  bool stl_sd_trace_enabled();

}

inline px* sdbeg(){return stl_begin();}
inline px* sdend(){return stl_end();}
inline px* sdinsert(){return stl_insert();}

#endif // STLDICT_H
