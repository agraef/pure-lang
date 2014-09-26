/* stlhmap.hpp -- C++ support for stlhmap.pure
    
Copyright (c) 2012 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlmap, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlmap distribution package for details.

*/

#ifndef STLHMAP_H
#define STLHMAP_H

#include <iostream>
#include <utility>

/* Define this for C++ TR1 extensions support. This is needed to get
   unordered_map for some older C++ libraries which don't provide full C++11
   support yet. */
//#define HAVE_TR1
#ifdef HAVE_TR1
#include <tr1/unordered_map>
using namespace std;
using namespace std::tr1;
#else
#include <unordered_map>
using namespace std;
#endif

#include "stlbase.hpp"

typedef pure_expr px;
typedef std::pair<px*,px*> pxp_pair;

struct px_hash
{
  size_t operator()(px* x) const
  { return ::hash(x); };
};

struct px_same {
  bool operator()(px* x, px* y) const
  { return same(x,y); }
};

struct px_pair_same {
  bool operator()(const pxp_pair& pr1, const pxp_pair& pr2) const
  {
    bool ok = 0;
    if ( same(pr1.first,pr2.first) ) {
      px* pr1_val = pr1.second; // 0 if pr1 is a set element
      px* pr2_val = pr2.second; // 0 if pr2 is a set element
      if ( pr1_val)
        ok = pr2_val && same(pr1_val,pr2_val);
      else
        ok = !pr2_val;
    }
    return ok;
  }
};

typedef unordered_map<px*,px*,px_hash,px_same> pxhmap;
typedef pxhmap::iterator pxhmapi;

struct stlhmap {
  bool keys_only;
  pxhmap hm;
  stlhmap(bool ko);
  ~stlhmap();
  void refc_elms();
  void free_elms();
};

typedef stlhmap sh; 

enum {stl_sh_key =1, stl_sh_val, stl_sh_elm};

/*** C interface for C++ unordered map **********************************/

extern "C" {
  px*  stl_shm_type_tags();
  px*  stl_shm_make_empty(int keys_only);
  px*  stl_shm_copy(px* pxshp);
  void stl_shm_delete(sh* shp);
  void stl_shm_reserve(px* pxshp, double max_load, int elm_count);
  px*  stl_shm_info(px* pxshp);
  int  stl_shm_bucket_size(px* pxshp, int i);
  int  stl_shm_size(px* pxshp);
  bool stl_shm_empty(px* pxshp); 
  int  stl_shm_count(px* pxshp, px* key);
  bool stl_shm_is_set(px* pxshp);
  px*  stl_shm_get(sh* shp, px* key);
  px*  stl_shm_find(px* pxshp, px* key, int what);
  int  stl_shm_insert(px* pxshp, px* src, bool replace);
  int  stl_shm_insert_stlhmap(px* pxshp1, px* pxshp2, bool replace);
  int  stl_shm_insert_stlvec(px* pxshp, sv* svp, bool replace);
  px*  stl_shm_replace(stlhmap* pmap, px* key, px* val);
  void stl_shm_swap(px* pxshp1, px* pxshp2);
  int  stl_shm_clear(px* pxshp);
  int  stl_shm_erase(px* pxshp, px* trg); 

  bool stl_shm_equal(px* pxshp1, px* pxshp2);    
  px*  stl_shm_make_vector(px* pxshp);

  px*  stl_shm_listmap(px* fun, px* shp, int what);
  px*  stl_shm_listcatmap(px* fun, px* pxshp, int what);
  int  stl_shm_member(sh* shp, px* key);
  px*  stl_shm_put(sh* shp, px* key, px* val);

  px*  stl_shm_foldl(px* fun, px* val, px* pxshp);
  px*  stl_shm_foldl1(px* fun, px* tpl);
  void stl_shm_do(px* fun, px* pxshp);

  void stl_shm_fill_stlvec(px* pxshp, sv* svp);
}

#endif // STLHMAP_H
