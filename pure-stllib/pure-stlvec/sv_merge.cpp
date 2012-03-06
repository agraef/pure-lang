/* sv_merge.cpp -- C++ support for sv_merge.pure
    
Copyright (c) 2011-2012 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlvec, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlvec distribution package for details.

*/

#include "sv_algorithm.hpp"
#include "stlvec.hpp"
#include <algorithm>

using namespace std;


int  stl_sva_merge(px* tpl1, px* tpl2, px* tpl3, px* cmp)
{
  int res = 0;
  pxh_less fun(cmp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  sv_range trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (rng1.size() + rng2.size() > trg.size()) range_overflow();
      svi last3 = merge(rng1.beg(), rng1.end(), rng2.beg(), rng2.end(),
                        trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      merge(rng1.beg(), rng1.end(), rng2.beg(), rng2.end(),
            back_inserter(*bak.vec), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

void stl_sva_inplace_merge(px* tpl, px* cmp)
{
  pxh_less fun(cmp);
  sv_range rng(tpl);
  try {
    if (!rng.is_valid || rng.num_iters != 3) bad_argument();
    inplace_merge(rng.beg(), rng.mid(), rng.end(), fun);  
  } catch (px* e) {
    pure_throw(e);
  }
}

bool stl_sva_includes(px* tpl1, px* tpl2, px* cmp)
{
  pxh_less fun(cmp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  try {
    return includes(rng1.beg(),rng1.end(),rng2.beg(),rng2.end(),fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

int stl_sva_set_union(px* tpl1, px* tpl2, px* tpl3, px* cmp)
{
  int res = 0;
  pxh_less fun(cmp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  sv_range trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (rng1.size() > trg.size() || rng2.size() > trg.size())
        range_overflow();
      svi last3 = set_union(rng1.beg(), rng1.end(),
                            rng2.beg(), rng2.end(),
                            trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      set_union(rng1.beg(), rng1.end(), rng2.beg(), rng2.end(),
                back_inserter(*bak.vec), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

int  stl_sva_set_intersection(px* tpl1, px* tpl2, px* tpl3, px* cmp)
{
  int res = 0;
  pxh_less fun(cmp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  sv_range trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      svi last3 = set_intersection(rng1.beg(), rng1.end(),
                                   rng2.beg(), rng2.end(),
                                   trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      set_intersection(rng1.beg(), rng1.end(),
                       rng2.beg(), rng2.end(),
                       back_inserter(*bak.vec), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

int  stl_sva_set_difference(px* tpl1, px* tpl2, px* tpl3, px* cmp)
{
  int res = 0;
  pxh_less fun(cmp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  sv_range trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      svi last3 = set_difference(rng1.beg(), rng1.end(),
                                 rng2.beg(), rng2.end(),
                                 trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      set_difference(rng1.beg(), rng1.end(),
                     rng2.beg(), rng2.end(),
                     back_inserter(*bak.vec), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

int  stl_sva_set_symmetric_difference(px* tpl1, px* tpl2, px* tpl3, px* cmp)
{
  int res = 0;
  pxh_less fun(cmp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  sv_range trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      svi last3 = set_symmetric_difference(rng1.beg(), rng1.end(),
                                           rng2.beg(), rng2.end(),
                                           trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      set_symmetric_difference(rng1.beg(), rng1.end(),
                               rng2.beg(), rng2.end(),
                               back_inserter(*bak.vec), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}
