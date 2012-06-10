/* sv_nonmodifying.cpp -- C++ support for sv_nonmodifying.pure
    
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

void stl_sva_for_each(px* tpl, px* unary_fun)
{
  pxh_fun1 fun(unary_fun);
  sv_range rng(tpl);
  try {
    if (!rng.is_valid || rng.num_iters != 2) bad_argument();
    for_each(rng.beg(), rng.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

int stl_sva_find_if(px* tpl, px* pred)
{
  int res = 0;
  pxh_pred1 fun(pred);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    svi res_it = find_if(rng.beg(), rng.end(), fun);
    res = iter_pos(rng.vec, res_it);
  } catch (px* e) {
    pure_throw(e);
  }
  return res;
}

int  stl_sva_find_first_of(px* tpl1, px* tpl2, px* comp)
{
  int res = 0;
  pxh_pred2 fun(comp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  try {
    svi res_it = find_first_of(rng1.beg(), rng1.end(),
                               rng2.beg(), rng2.end(), fun);
    res = iter_pos(rng1.vec, res_it);
  } catch (px* e) {
    pure_throw(e);
  }
  return res;
}

int stl_sva_adjacent_find(px* tpl, px* comp)
{
  int res = 0;
  pxh_pred2 fun(comp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    svi res_it = adjacent_find(rng.beg(), rng.end(), fun);
    res = iter_pos(rng.vec, res_it);
  } catch (px* e) {
    pure_throw(e);
  }
  return res;
}

int stl_sva_count_if(px* tpl, px* unary_pred)
{
  int res = 0;
  pxh_pred1 fun(unary_pred);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    res = count_if(rng.beg(), rng.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
  return res;
}

px* stl_sva_mismatch(px* tpl1, px* tpl2, px* comp)
{
  px* res = 0;
  pxh_pred2 fun(comp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  try {
    pair<svi,svi> res_pair = mismatch(rng1.beg(), rng1.end(),
                                      rng2.beg(), fun);
    svi svi_i = res_pair.first;
    svi svi_j = res_pair.second;
    int i = iter_pos(rng1.vec, svi_i);
    int j = iter_pos(rng2.vec, svi_j);
    res = pure_tuplel(2, pure_int(i), pure_int(j));
  } catch (px* e) {
    pure_throw(e);
  }
  return res;
}

bool stl_sva_equal(px* tpl1, px* tpl2, px* comp)
{
  return stl_sv_allpairs(comp, tpl1, tpl2);
}

int stl_sva_search(px* tpl1, px* tpl2, px* comp)
{
  int res = 0;
  pxh_pred2 fun(comp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  try {
    svi res_it = search(rng1.beg(), rng1.end(), rng2.beg(), rng2.end(), fun);
    res = iter_pos(rng1.vec, res_it);
  } catch (px* e) {
    pure_throw(e);
  }
  return res;
}

int stl_sva_search_n(px* tpl, int count, px* val,  px* comp)
{
  int res = 0;
  pxh_pred2 fun(comp);
  pxh value(val);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    svi res_it = search_n(rng.beg(), rng.end(), count, value, fun);
    res = iter_pos(rng.vec, res_it);
  } catch (px* e) {
    pure_throw(e);
  }
  return res;
}

int stl_sva_find_end(px* tpl1, px* tpl2, px* comp)
{
  int res = 0;
  pxh_pred2 fun(comp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  try {
    svi res_it = find_end(rng1.beg(), rng1.end(), rng2.beg(), rng2.end(), fun);
    res = iter_pos(rng1.vec, res_it);
  } catch (px* e) {
    pure_throw(e);
  }
  return res;
}
