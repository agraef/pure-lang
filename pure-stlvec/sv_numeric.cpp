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

px*  sva_accumulate(px* tpl, px* val, px* bin_op)
{
  pxh_fun2 fun(bin_op);
  pxh valh(val);
  sv_iters itrs1(tpl);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  try {
    pxh resh = accumulate(itrs1.beg(), itrs1.end(), valh, fun);
    px* res = resh.pxp();
    resh.release();
    px_unref(res);
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

px*  sva_inner_product(px* tpl1,
                       px* tpl2, px* val,
                       px* bin_op1, px* bin_op2)
{
  pxh_fun2 fun1(bin_op1);
  pxh_fun2 fun2(bin_op2);
  pxh valh(val);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  try {
    if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
    if (!itrs2.is_valid || itrs2.num_iters != 1) bad_argument();
    if (itrs1.size() > itrs2.size()) range_overflow();
    pxh resh = inner_product(itrs1.beg(), itrs1.end(), itrs2.beg(),
                             valh, fun1, fun2);
    px* res = resh.pxp();
    resh.release();
    px_unref(res);
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_partial_sum(px* tpl1, px* tpl2, px* bin_op)
{
  int res = 0;
  pxh_fun2 fun(bin_op);
  sv_iters itrs(tpl1);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  sv_iters trg(tpl2);
  sv_back_iter bak(tpl2);
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (itrs.size() > trg.size()) range_overflow();
      svi last2 = partial_sum(itrs.beg(), itrs.end(), trg.beg(), fun);
      res = iter_pos(trg.vec, last2);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      partial_sum(itrs.beg(), itrs.end(), back_inserter(*v), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_adjacent_difference(px* tpl1, px* tpl2, px* bin_op)
{
  int res = 0;
  pxh_fun2 fun(bin_op);
  sv_iters itrs(tpl1);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  sv_iters trg(tpl2);
  sv_back_iter bak(tpl2);
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (itrs.size() > trg.size()) range_overflow();
      svi last2 = adjacent_difference(itrs.beg(), itrs.end(), trg.beg(), fun);
      res = iter_pos(trg.vec, last2);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      adjacent_difference(itrs.beg(), itrs.end(), back_inserter(*v), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}


