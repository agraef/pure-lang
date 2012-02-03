/* stlhash.hpp -- C++ support for stlhash.pure
    
--- DRAFT - FOR DISCUSSON PURPOSES ONLY ---

Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlhash, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlmap distribution package for details.

*/

#ifndef STLHASH_H
#define STLHASH_H

#include <iostream>
#include <utility>
#include <unordered_map>
#include "stlbase.hpp"

typedef pure_expr px;
typedef std::pair<px*,px*> pxp_pair;

struct px_hash
{
  size_t operator()(px* x) const
  { return hash(x); };
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
      px* rhs1 = pr1.second;
      px* rhs2 = pr2.second;
      if ( rhs1 )
        ok = rhs2 && same(rhs1,rhs2); 
      else
        ok = !rhs2;
    }
    return ok;
  }
};

typedef std::unordered_map<px*,px*,px_hash,px_same> pxhmap;
typedef pxhmap::iterator pxhmapi;

struct stlhash {
  bool keys_only;
  pxhmap hm;
  stlhash(bool ko);
  ~stlhash();
  void refc_elms();
  void free_elms();
};

typedef stlhash sh; 

enum {stl_sh_key =1, stl_sh_val, stl_sh_elm};

/*** C interface for C++ unordered map **********************************/

extern "C" {
  px*  sh_type_tags();
  px*  sh_make_empty(int keys_only);
  px*  sh_copy(px* pxshp);
  void sh_delete(sh* shp);
  void sh_reserve(px* pxshp, double max_load, int elm_count);
  px*  sh_info(px* pxshp);
  int  sh_bucket_size(px* pxshp, int i);
  int  sh_size(px* pxshp);
  bool sh_empty(px* pxshp); 
  int  sh_count(px* pxshp, px* key);
  bool sh_is_set(px* pxshp);
  px*  sh_find(px* pxshp, px* key);
  px*  sh_insert_elm(px* pxshp, px* kv);
  int  sh_insert_elms_xs(px* pxshp, px* src);
  px*  sh_swap(px* pxshp1, px* pxshp2);
  int  sh_clear(px* pxshp);
  int  sh_erase(px* pxshp, px* trg); 

  bool sh_equal(px* pxshp1, px* pxshp2);    
  px*  sh_make_vector(px* pxshp);

  px*  sh_listmap(px* fun, px* shp, int what);
  px*  sh_listcatmap(px* fun, px* pxshp, int what);
  int  sh_member(px* pxshp, px* key);
  px*  sh_update(px* pxshp, px* key, px* val);
  px*  sh_update_with(px* pxshp, px* key, px* unaryfun);

}

#endif // STLHASH_H