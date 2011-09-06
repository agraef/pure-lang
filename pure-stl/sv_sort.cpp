/* sv_sort.cpp -- C++ support for sv_sort.pure
    
   Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>

   DRAFT FOR DISCUSSION PURPOSES ONLY.

*/

#include "stlvec.hpp"
#include "sv_algorithm.hpp"
#include <algorithm>

using namespace std;

void sva_sort(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    sort(itrs.beg(), itrs.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_stable_sort(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    stable_sort(itrs.beg(), itrs.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_partial_sort(px* tpl, px* cmp){
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 3) bad_argument();
  try {
    partial_sort(itrs.beg(), itrs.mid(), itrs.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_partial_sort_copy(px* tpl1, px* tpl2, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 2) bad_argument();
  if ( itrs1.overlaps(itrs2) ) range_overlap();
  try {
  partial_sort_copy(itrs1.beg(), itrs1.end(), itrs2.beg(), itrs2.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_nth_element(px* tpl, px* cmp){
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 3) bad_argument();
  try {
    nth_element(itrs.beg(), itrs.mid(), itrs.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_lower_bound(px* tpl, px* val, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    svi res = lower_bound(itrs.beg(), itrs.end(), val, fun);  
    return iter_pos(itrs.vec, res);
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_upper_bound(px* tpl, px* val, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    svi res = upper_bound(itrs.beg(), itrs.end(), val, fun);  
    return iter_pos(itrs.vec, res);
  } catch (px* e) {
    pure_throw(e);
  }
}

px*  sva_equal_range(px* tpl, px* val, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    pair<svi,svi> res_pair = equal_range(itrs.beg(), itrs.end(), val, fun);
    svi svi_i = res_pair.first;
    svi svi_j = res_pair.second;
    int i = svi_i == itrs.vec->end() ? svend : svi_i - itrs.vec->begin(); 
    int j = svi_j == itrs.vec->end() ? svend : svi_j - itrs.vec->begin(); 
    return pure_tuplel(2, pure_int(i), pure_int(j));  
  } catch (px* e) {
    pure_throw(e);
  }
}

bool sva_binary_search(px* tpl, px* val, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  try {
    if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
    return binary_search(itrs.beg(), itrs.end(), val, fun);  
  } catch (px* e) {
    pure_throw(e);
  }
}
