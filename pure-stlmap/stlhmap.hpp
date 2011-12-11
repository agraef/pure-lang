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
#include "stlbase.hpp"
#include "stlvec.hpp"

typedef std::unordered_map<pxh,pxh,pxh_hash,pxh_pred2> pxhhmap;
typedef pxhhmap::iterator phmi;

struct shm_iter;

struct stlhmap {
  bool keys_only;
  pxhhmap hmp;
  stlhmap(px* hash, px* eql, bool keys_only); 
  ~stlhmap(){};
};

typedef stlhmap shm; 

struct shm_iter {
  phmi iter;
  pxh pxhshmp;
  shm_iter(px* pxshmp, phmi i);
  ~shm_iter(){};
  shm* shmp() const; 
};

enum {stl_shm_key =1, stl_shm_val, stl_shm_elm};

/*** C interface for C++ unordered map **********************************/

extern "C" {
  px*  shm_type_tags();
  px*  shm_make_empty(px* hash, px* eql, int keys_only);
  px*  shm_copy(px* pxshmp);
  void shm_delete(shm* shmp);
  void shm_reserve(px* pxshmp, double max_load, int elm_count);
  px*  shm_hash_info(px* pxshmp);
  int  shm_size(px* tpl);
  bool shm_empty(px* tpl); 
  int  shm_count(px* pxshmp, px* key);
  bool shm_is_set(px* tpl);
  px*  shm_find(px* pxshmp, px* key, int what);
  px*  shm_insert_elm(px* pxshmp, px* kv);
  int  shm_insert_elms_xs(px* pxshmp, px* src);
  int  shm_insert_elms_stlvec(px* pxshmp, px* tpl);
  px*  shm_swap(px* pxshmp1, px* pxshmp2);
  int  shm_clear(px* pxshmp);
  int  shm_erase(px* pxshmp, px* trg); 

  px*  shm_begin(px* pxshmp);
  px*  shm_end(px* pxshmp); 
  px*  shm_get_elm_at_inc(px* pxshmip);
  px*  shm_equal_iter(px* pxshmip1, px* pxshmip2);

  bool shm_equal(px* pxshmp1, px* pxshmp2);    
  px*  shm_make_vector(px* tpl);
  void shm_fill_stlvec(px* tpl, sv* svp);

  px*  shm_listmap(px* fun, px* shmp, int what);
  px*  shm_listcatmap(px* fun, px* tpl, int what);
  px*  shm_foldl(px* fun, px* val, px* tpl);
  px*  shm_foldl1(px* fun, px* tpl);

  int  shm_member(px* pxshmp, px* key);
  px*  shm_update(px* pxshmp, px* key, px* val);

}

inline px* shmbeg(){return stlbegin_sym();}
inline px* shmend(){return stlend_sym();}

#endif // STLHMAP_H
