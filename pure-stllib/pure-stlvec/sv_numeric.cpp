/* sv_numeric.cpp -- C++ support for sv_numeric.pure
    
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
#include <numeric>

using namespace std;

px*  stl_sva_accumulate(px* tpl, px* val, px* bin_op)
{
  pxh_fun2 fun(bin_op);
  pxh valh(val);
  sv_range rng1(tpl);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  try {
    pxh resh = accumulate(rng1.beg(), rng1.end(), valh, fun);
    px* res = resh.pxp();
    resh.release();
    pure_unref(res);
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

px*  stl_sva_inner_product(px* tpl1,
                       px* tpl2, px* val,
                       px* bin_op1, px* bin_op2)
{
  pxh_fun2 fun1(bin_op1);
  pxh_fun2 fun2(bin_op2);
  pxh valh(val);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  try {
    if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
    if (!rng2.is_valid || rng2.num_iters != 1) bad_argument();
    if (rng1.size() > rng2.size()) range_overflow();
    pxh resh = inner_product(rng1.beg(), rng1.end(), rng2.beg(),
                             valh, fun1, fun2);
    px* res = resh.pxp();
    resh.release();
    pure_unref(res);
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

int  stl_sva_partial_sum(px* tpl1, px* tpl2, px* bin_op)
{
  int res = 0;
  pxh_fun2 fun(bin_op);
  sv_range rng(tpl1);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  sv_range trg(tpl2);
  sv_back_iter bak(tpl2);
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (rng.size() > trg.size()) range_overflow();
      svi last2 = partial_sum(rng.beg(), rng.end(), trg.beg(), fun);
      res = iter_pos(trg.vec, last2);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      partial_sum(rng.beg(), rng.end(), back_inserter(*v), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

int  stl_sva_adjacent_difference(px* tpl1, px* tpl2, px* bin_op)
{
  int res = 0;
  pxh_fun2 fun(bin_op);
  sv_range rng(tpl1);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  sv_range trg(tpl2);
  sv_back_iter bak(tpl2);
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (rng.size() > trg.size()) range_overflow();
      svi last2 = adjacent_difference(rng.beg(), rng.end(), trg.beg(), fun);
      res = iter_pos(trg.vec, last2);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      adjacent_difference(rng.beg(), rng.end(), back_inserter(*v), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}


