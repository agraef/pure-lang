/* sv_modifying.cpp -- C++ support for sv_modifying.pure
    
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

int sva_copy(px* tpl1, px* tpl2)
{
  int res = 0;
  sv_range rng(tpl1);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  sv_range trg(tpl2);
  sv_back_iter bak(tpl2);
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      svi first2 = trg.beg();
      if (rng.contains(trg.vec, first2) ) range_overlap();
      if (rng.size() > trg.size()) range_overflow();
      svi last2 = copy(rng.beg(), rng.end(), first2);
      res = iter_pos(trg.vec,last2);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==rng.vec) bad_argument();
      copy(rng.beg(), rng.end(), back_inserter(*v));
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

int sva_copy_backward(px* tpl1, px* tpl2)
{
  int res = 0;
  sv_range rng(tpl1);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  sv_range trg(tpl2);
  try {
    if (trg.is_valid && trg.num_iters == 1) {
      svi last2 = trg.beg();
      int trg_max_sz = last2 - trg.vec->begin(); 
      if (rng.contains(trg.vec, last2) ) range_overlap();
      if (rng.size() > trg_max_sz) range_overflow();
      svi first2 = copy_backward(rng.beg(), rng.end(), last2);
      res = iter_pos(trg.vec, first2);
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_swap_ranges(px* tpl1, px* tpl2){
  int res = 0;
  sv_range rng(tpl1);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  sv_range trg(tpl2);
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      svi first2 = trg.beg();
      if (rng.size() > trg.size()) range_overflow();
      if (rng.contains(trg.vec, first2) ) range_overlap();
      swap_ranges(rng.beg(), rng.end(), first2);
    }
    else
      bad_argument();
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_transform(px* tpl1, px* tpl2, px* unary_op)
{
  int res = 0;
  pxh_fun1 fun(unary_op);
  sv_range rng(tpl1);
  try {
    if (!rng.is_valid || rng.num_iters != 2) bad_argument();
    sv_range trg(tpl2);
    sv_back_iter bak(tpl2);
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (rng.size() > trg.size()) range_overflow();
      svi last2 = transform(rng.beg(), rng.end(), trg.beg(), fun);
      res = iter_pos(trg.vec, last2);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      transform(rng.beg(), rng.end(), back_inserter(*v), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_transform_2(px* tpl1, px* tpl2, px* tpl3, px* bin_op)
{
  int res = 0;
  pxh_fun2 fun(bin_op);
  sv_range rng1(tpl1);
  sv_range rng2(tpl2);
  sv_range trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!rng1.is_valid || rng1.num_iters != 2) bad_argument();
  if (!rng2.is_valid || rng2.num_iters != 1) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (rng1.size() > trg.size() || 
          rng1.size() > rng2.size()) range_overflow();
      svi last3 = transform(rng1.beg(),rng1.end(),rng2.beg(),trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      transform(rng1.beg(), rng1.end(), rng2.beg(), back_inserter(*v), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_replace_if(px* tpl, px* pred, px* val)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    replace_if(rng.beg(), rng.end(), pxh_pred1(pred), val);
  } catch (px* e) {
    pure_throw(e);
  }
}

int sva_replace_copy_if(px* tpl1, px* tpl2, px* pred, px* val){
  int res = 0;
  pxh_pred1 fun(pred);
  sv_range rng(tpl1);
  sv_range trg(tpl2);
  sv_back_iter bak(tpl2);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      svi first2 = trg.beg();
      if (rng.contains(trg.vec, first2) ) bad_argument();
      if (rng.size() > trg.size()) range_overflow();
      replace_copy_if(rng.beg(), rng.end(), first2, fun, val);
      res = svend;
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==rng.vec) bad_argument();
      replace_copy_if(rng.beg(), rng.end(), back_inserter(*v), fun, val);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_fill(px* tpl, px* val)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    fill(rng.beg(), rng.end(), val);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_fill_n(px* tpl, int n, px* val)
{
  sv_range trg(tpl);
  sv_back_iter bak(tpl);
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      if (n > trg.size()) range_overflow();
      fill_n(trg.beg(), n, val);
    }
    else if (bak.is_valid) {
      fill_n(back_inserter(*bak.vec), n, val);
    }
    else
      bad_argument();
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_generate(px* tpl, px* gen)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  pxh_gen genh(gen);
  try {
    generate(rng.beg(), rng.end(), genh);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_generate_n(px* tpl, int n, px* gen)
{
  sv_range trg(tpl);
  sv_back_iter bak(tpl);
  pxh_gen genh(gen);
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      if (n > trg.size()) range_overflow();
      generate_n(trg.beg(), n, genh);
    }
    else if (bak.is_valid) {
      generate_n(back_inserter(*bak.vec), n, genh);
    }
    else
      bad_argument();
  } catch (px* e) {
    pure_throw(e);
  }
}

int sva_remove_if(px* tpl, px* pred)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    svi pei = remove_if(rng.beg(), rng.end(), pxh_pred1(pred));
    return iter_pos(rng.vec, pei);
  } catch (px* e) {
    pure_throw(e);
  }
}

int sva_remove_copy_if(px* tpl1, px* tpl2, px* pred)
{
  int res = 0;
  pxh_pred1 fun(pred);
  sv_range rng(tpl1);
  sv_range trg(tpl2);
  sv_back_iter bak(tpl2);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      svi first2 = trg.beg();
      if (rng.contains(trg.vec, first2) ) bad_argument();
      if (rng.size() > trg.size()) range_overflow();
      svi pei = remove_copy_if(rng.beg(), rng.end(), first2, fun);
      res =  iter_pos(trg.vec, pei);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==rng.vec) bad_argument();
      remove_copy_if(rng.beg(), rng.end(), back_inserter(*v), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_unique(px* tpl, px* bin_pred)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    svi pei = unique(rng.beg(), rng.end(), pxh_pred2(bin_pred));
    return iter_pos(rng.vec, pei);
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_unique_copy(px* tpl1, px* tpl2, px* bin_pred)
{
  int res = 0;
  pxh_pred2 fun(bin_pred);
  sv_range rng(tpl1);
  sv_range trg(tpl2);
  sv_back_iter bak(tpl2);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      svi first2 = trg.beg();
      if (rng.contains(trg.vec, first2) ) bad_argument();
      if (rng.size() > trg.size()) range_overflow();
      svi pei = unique_copy(rng.beg(), rng.end(), first2, fun);
      res =  iter_pos(trg.vec, pei);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==rng.vec) bad_argument();
      unique_copy(rng.beg(), rng.end(), back_inserter(*v), fun);
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_reverse(px* tpl)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    reverse(rng.beg(), rng.end());
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_reverse_copy(px* tpl1, px* tpl2)
{
  int res = 0;
  sv_range rng(tpl1);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  sv_range trg(tpl2);
  sv_back_iter bak(tpl2);
  try {
    if (trg.is_valid && trg.num_iters == 1) {
      if (trg.vec==rng.vec) bad_argument();
      if (rng.size() > trg.size()) range_overflow();
      svi pei = reverse_copy(rng.beg(), rng.end(), trg.beg()); 
      res =  iter_pos(trg.vec, pei);   
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==rng.vec) bad_argument();
      reverse_copy(rng.beg(), rng.end(), back_inserter(*v));
      res = svend;
    }
    else
      bad_argument();
    return res;
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_rotate(px* tpl) 
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 3) bad_argument();
  try {
  rotate(rng.beg(), rng.mid(), rng.end());
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_rotate_copy(px* tpl1, px* tpl2){
  sv_range rng(tpl1);
  if (!rng.is_valid || rng.num_iters != 3) bad_argument();
  sv_range trg(tpl2);
  sv_back_iter bak(tpl2);
  try {
    if (trg.is_valid && trg.num_iters <= 2) {
      if (trg.vec==rng.vec) bad_argument();
      if (rng.size() > trg.size()) range_overflow();
      rotate_copy(rng.beg(), rng.mid(), rng.end(), trg.beg());    
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==rng.vec) bad_argument();
      rotate_copy(rng.beg(), rng.mid(), rng.end(), back_inserter(*v));
    }
    else
      bad_argument();
  } catch (px* e) {
    pure_throw(e);
  }
}


void sva_random_shuffle(px* tpl) 
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    random_shuffle(rng.beg(), rng.end());  
  } catch (px* e) {
    pure_throw(e);
  }
}


int sva_partition(px* tpl, px* pred)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    sv* v = rng.vec;
    svi mid = partition(rng.beg(), rng.end(), pxh_pred1(pred));
    return iter_pos(v,mid);
  } catch (px* e) {
    pure_throw(e);
  }
}

int sva_stable_partition(px* tpl, px* pred)
{
  sv_range rng(tpl);
  if (!rng.is_valid || rng.num_iters != 2) bad_argument();
  try {
    sv* v = rng.vec;
    svi mid = stable_partition(rng.beg(), rng.end(), pxh_pred1(pred));
    return iter_pos(v,mid);
  } catch (px* e) {
    pure_throw(e);
  }
}
