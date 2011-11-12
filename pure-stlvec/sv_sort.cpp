/* sv_sort.cpp -- C++ support for sv_sort.pure
    
Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlvec, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlvec distribution package for details.

*/

#include "stlvec.hpp"
#include "sv_algorithm.hpp"
#include <algorithm>

using namespace std;

void sva_sort(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    sort(rng.beg(), rng.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_stable_sort(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    stable_sort(rng.beg(), rng.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_partial_sort(px* tpl, px* cmp){
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 3) bad_argument();
  try {
    partial_sort(rng.beg(), rng.mid(), rng.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_partial_sort_copy(px* tpl1, px* tpl2, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 2) bad_argument();
  if ( rng1.overlaps(rng2) ) range_overlap();
  try {
  partial_sort_copy(rng1.beg(), rng1.end(), rng2.beg(), rng2.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_nth_element(px* tpl, px* cmp){
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 3) bad_argument();
  try {
    nth_element(rng.beg(), rng.mid(), rng.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_lower_bound(px* tpl, px* val, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    svi res = lower_bound(rng.beg(), rng.end(), val, fun);  
    return iter_pos(rng.vec, res);
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_upper_bound(px* tpl, px* val, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    svi res = upper_bound(rng.beg(), rng.end(), val, fun);  
    return iter_pos(rng.vec, res);
  } catch (px* e) {
    pure_throw(e);
  }
}

px*  sva_equal_range(px* tpl, px* val, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    pair<svi,svi> res_pair = equal_range(rng.beg(), rng.end(), val, fun);
    svi svi_i = res_pair.first;
    svi svi_j = res_pair.second;
    int i = svi_i == rng.vec->end() ? svend : svi_i - rng.vec->begin(); 
    int j = svi_j == rng.vec->end() ? svend : svi_j - rng.vec->begin(); 
    return pure_tuplel(2, pure_int(i), pure_int(j));  
  } catch (px* e) {
    pure_throw(e);
  }
}

bool sva_binary_search(px* tpl, px* val, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_range rng(tpl);
  try {
    if (!rng.is_valid || rng.num_iters != 2) bad_argument();
    return binary_search(rng.beg(), rng.end(), val, fun);  
  } catch (px* e) {
    pure_throw(e);
  }
}
