/* sv_nonmodifying.cpp -- C++ support for sv_nonmodifying.pure
    
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

void sva_for_each(px* tpl, px* unary_fun)
{
  pxh_fun1 fun(unary_fun);
  sv_iters itrs(tpl);
  try {
    if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
    for_each(itrs.beg(), itrs.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

int sva_find_if(px* tpl, px* pred)
{
  pxh_pred1 fun(pred);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    svi res = find_if(itrs.beg(), itrs.end(), fun);
    return iter_pos(itrs.vec, res);
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_find_first_of(px* tpl1, px* tpl2, px* comp)
{
  pxh_pred2 fun(comp);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 2) bad_argument();
  try {
    svi res = find_first_of(itrs1.beg(), itrs1.end(),
                            itrs2.beg(), itrs2.end(), fun);
    return iter_pos(itrs1.vec, res);
  } catch (px* e) {
    pure_throw(e);
  }
}

int sva_adjacent_find(px* tpl, px* comp)
{
  pxh_pred2 fun(comp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    svi res = adjacent_find(itrs.beg(), itrs.end(), fun);
    return iter_pos(itrs.vec, res);
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_count_if(px* tpl, px* unary_pred)
{
  pxh_pred1 fun(unary_pred);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    return count_if(itrs.beg(), itrs.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

px*  sva_mismatch(px* tpl1, px* tpl2, px* comp)
{
  pxh_pred2 fun(comp);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 2) bad_argument();
  try {
    pair<svi,svi> res_pair = mismatch(itrs1.beg(), itrs1.end(),
                                      itrs2.beg(), fun);
    svi svi_i = res_pair.first;
    svi svi_j = res_pair.second;
    int i = iter_pos(itrs1.vec, svi_i);
    int j = iter_pos(itrs2.vec, svi_j);
    return pure_tuplel(2, pure_int(i), pure_int(j));
  } catch (px* e) {
    pure_throw(e);
  }
}

bool sva_equal(px* tpl1, px* tpl2, px* comp)
{
  sv_equal(comp, tpl1, tpl2);
}

int  sva_search(px* tpl1, px* tpl2, px* comp)
{
  pxh_pred2 fun(comp);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 2) bad_argument();
  try {
    svi res = search(itrs1.beg(), itrs1.end(), itrs2.beg(), itrs2.end(), fun);
    return iter_pos(itrs1.vec, res);
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_search_n(px* tpl, int count, px* val,  px* comp)
{
  pxh_pred2 fun(comp);
  pxh value(val);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    svi res = search_n(itrs.beg(), itrs.end(), count, value, fun);
    return iter_pos(itrs.vec, res);
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_find_end(px* tpl1, px* tpl2, px* comp)
{
  pxh_pred2 fun(comp);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 2) bad_argument();
  try {
    svi res = find_end(itrs1.beg(), itrs1.end(), itrs2.beg(), itrs2.end(), fun);
    return iter_pos(itrs1.vec, res);
  } catch (px* e) {
    pure_throw(e);
  }
}
