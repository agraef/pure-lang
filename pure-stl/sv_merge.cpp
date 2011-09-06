/* sv_merge.cpp -- C++ support for sv_merge.pure
    
   Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>

   DRAFT FOR DISCUSSION PURPOSES ONLY.

*/

#include "sv_algorithm.hpp"
#include "stlvec.hpp"
#include <algorithm>

using namespace std;


int  sva_merge(px* tpl1, px* tpl2, px* tpl3, px* cmp)
{
  int res = 0;
  pxh_pred2 fun(cmp);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  sv_iters trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (itrs1.size() + itrs2.size() > trg.size()) range_overflow();
      svi last3 = merge(itrs1.beg(), itrs1.end(), itrs2.beg(), itrs2.end(),
                        trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      merge(itrs1.beg(), itrs1.end(), itrs2.beg(), itrs2.end(),
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

void sva_inplace_merge(px* tpl, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs(tpl);
  try {
    if (!itrs.is_valid || itrs.num_iters != 3) bad_argument();
    inplace_merge(itrs.beg(), itrs.mid(), itrs.end(), fun);  
  } catch (px* e) {
    pure_throw(e);
  }
}

bool sva_includes(px* tpl1, px* tpl2, px* cmp)
{
  pxh_pred2 fun(cmp);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 2) bad_argument();
  try {
    return includes(itrs1.beg(),itrs1.end(),itrs2.beg(),itrs2.end(),fun);
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_set_union(px* tpl1, px* tpl2, px* tpl3, px* cmp)
{
  int res = 0;
  pxh_pred2 fun(cmp);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  sv_iters trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (itrs1.size() > trg.size() || itrs2.size() > trg.size())
        range_overflow();
      svi last3 = set_union(itrs1.beg(), itrs1.end(),
                            itrs2.beg(), itrs2.end(),
                            trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      set_union(itrs1.beg(), itrs1.end(), itrs2.beg(), itrs2.end(),
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

int  sva_set_intersection(px* tpl1, px* tpl2, px* tpl3, px* cmp)
{
  int res = 0;
  pxh_pred2 fun(cmp);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  sv_iters trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      svi last3 = set_intersection(itrs1.beg(), itrs1.end(),
                                   itrs2.beg(), itrs2.end(),
                                   trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      set_intersection(itrs1.beg(), itrs1.end(),
                       itrs2.beg(), itrs2.end(),
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

int  sva_set_difference(px* tpl1, px* tpl2, px* tpl3, px* cmp)
{
  int res = 0;
  pxh_pred2 fun(cmp);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  sv_iters trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      svi last3 = set_difference(itrs1.beg(), itrs1.end(),
                                 itrs2.beg(), itrs2.end(),
                                 trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      set_difference(itrs1.beg(), itrs1.end(),
                     itrs2.beg(), itrs2.end(),
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

int  sva_set_symmetric_difference(px* tpl1, px* tpl2, px* tpl3, px* cmp)
{
  int res = 0;
  pxh_pred2 fun(cmp);
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  sv_iters trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      svi last3 = set_symmetric_difference(itrs1.beg(), itrs1.end(),
                                           itrs2.beg(), itrs2.end(),
                                           trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      set_symmetric_difference(itrs1.beg(), itrs1.end(),
                               itrs2.beg(), itrs2.end(),
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
