/* sv_merge.cpp -- C++ support for sv_merge.pure
    
   Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>

   DRAFT FOR DISCUSSION PURPOSES ONLY.

*/

#include "sv_algorithm.hpp"
#include "stlvec.hpp"
#include <algorithm>

using namespace std;

void sva_push_heap(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    push_heap(itrs.beg(), itrs.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}


void sva_pop_heap(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    pop_heap(itrs.beg(), itrs.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_make_heap(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    make_heap(itrs.beg(), itrs.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_sort_heap(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    sort_heap(itrs.beg(), itrs.end(), fun);
  } catch (px* e) {
    pure_throw(e);
  }
}
