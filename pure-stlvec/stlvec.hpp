/* stlvec.hpp -- C++ support for stlvec.pure
    
Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlvec, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlvec distribution package for details.

*/

#ifndef STLVEC_H
#define STLVEC_H

#include <iostream>
#include <vector>
#include "stlbase.hpp"

/* sv, svi and friends - A sv is a vector of pxh's. It is the data structure
   pointed to by a stlvec on the Pure side of the interface. A svi is an
   iterator on a sv.*/

typedef std::vector<pxh> sv;
typedef sv::iterator svi;
typedef sv::const_iterator const_svi;
typedef sv::reverse_iterator reverse_svi;
typedef sv::const_reverse_iterator const_reverse_svi;

const int svbeg  =  0;
const int svend  = -1; 
const int svback = -2;  // back_insert_iterator
const int svrev  = -3;  // request reversal of iterators

const int sv_max_num_iters = 3;

struct sv_iters {
  sv* vec;
  svi iters[sv_max_num_iters];
  int num_iters;
  int sz;
  bool is_reversed;
  bool is_valid;

  sv_iters(px* tpl);
  svi beg(){return iters[0];}
  svi mid(){return iters[1];}
  svi end(){return num_iters > 2 ? iters[2] : iters[1];}
  int size();
  bool contains(sv*, const svi&);
  bool overlaps(sv_iters&);
};

struct sv_back_iter{
  sv* vec;
  bool is_valid;
  sv_back_iter(px* tpl);
};

int iter_pos(sv* vec, svi iter);

/*** C interface for vector of PX Handles ***/

extern "C" {
  sv*  sv_make_a();
  sv*  sv_make_b(px* xs);
  sv*  sv_make_c(px* xs, int n);
  void sv_delete(sv* sv_p);
  sv*  sv_dup(px* tpl);
  px*  sv_vector(px* it);
  void sv_reserve(sv* vec, int n);
  int  sv_size(sv* vec);
  int  sv_iter_size(px* tpl);
  bool sv_empty(sv* vec);
  int  sv_capacity(sv* vec);

  px*  sv_get(sv* vec, int pos);
  void sv_put(sv* vec, int pos, px* val);
  px*  sv_front(sv* vec);
  px*  sv_back(sv* vec);

  void sv_push_back(sv* vec, px* val);
  void sv_splice(sv* vec, int b, px* xs_or_tpl);
  void sv_pop_back(sv* vec);
  void sv_erase(px* tpl);
  void sv_clear(sv* vec);
  bool sv_equal(px* fun, px* tpl1, px* tpl2);

  px*  sv_listmap(px* fun, px* tpl);
  px*  sv_listcatmap(px* fun, px* tpl);
  px*  sv_foldl(px* fun, px* val, px* tpl);
  px*  sv_foldl1(px* fun, px* tpl);
  px*  sv_foldr(px* fun, px* val, px* tpl);
  px*  sv_foldr1(px* fun, px* tpl);

}

#endif // STLVEC_H
