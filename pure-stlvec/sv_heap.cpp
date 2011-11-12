/* sv_merge.cpp -- C++ support for sv_merge.pure
    
Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

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

void sva_push_heap(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    push_heap(rng.beg(), rng.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}


void sva_pop_heap(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    pop_heap(rng.beg(), rng.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_make_heap(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    make_heap(rng.beg(), rng.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_sort_heap(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    sort_heap(rng.beg(), rng.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}
