/* sv_modifying.cpp -- C++ support for sv_modifying.pure
    
   Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>

   DRAFT FOR DISCUSSION PURPOSES ONLY.

*/

#include "sv_algorithm.hpp"
#include "stlvec.hpp"
#include <algorithm>

using namespace std;

int sva_copy(px* tpl1, px* tpl2)
{
  int res = 0;
  sv_iters itrs(tpl1);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  sv_iters trg(tpl2);
  sv_back_iter bak(tpl2);
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      svi first2 = trg.beg();
      if (itrs.contains(trg.vec, first2) ) range_overlap();
      if (itrs.size() > trg.size()) range_overflow();
      svi last2 = copy(itrs.beg(), itrs.end(), first2);
      res = iter_pos(trg.vec,last2);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==itrs.vec) bad_argument();
      copy(itrs.beg(), itrs.end(), back_inserter(*v));
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
  sv_iters itrs(tpl1);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  sv_iters trg(tpl2);
  try {
    if (trg.is_valid && trg.num_iters == 1) {
      svi last2 = trg.beg();
      int trg_max_sz = last2 - trg.vec->begin(); 
      if (itrs.contains(trg.vec, last2) ) range_overlap();
      if (itrs.size() > trg_max_sz) range_overflow();
      svi first2 = copy_backward(itrs.beg(), itrs.end(), last2);
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
  sv_iters itrs(tpl1);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  sv_iters trg(tpl2);
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      svi first2 = trg.beg();
      if (itrs.size() > trg.size()) range_overflow();
      if (itrs.contains(trg.vec, first2) ) range_overlap();
      swap_ranges(itrs.beg(), itrs.end(), first2);
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
  sv_iters itrs(tpl1);
  try {
    if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
    sv_iters trg(tpl2);
    sv_back_iter bak(tpl2);
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (itrs.size() > trg.size()) range_overflow();
      svi last2 = transform(itrs.beg(), itrs.end(), trg.beg(), fun);
      res = iter_pos(trg.vec, last2);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      transform(itrs.beg(), itrs.end(), back_inserter(*v), fun);
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
  sv_iters itrs1(tpl1);
  sv_iters itrs2(tpl2);
  sv_iters trg(tpl3);
  sv_back_iter bak(tpl3);
  if (!itrs1.is_valid || itrs1.num_iters != 2) bad_argument();
  if (!itrs2.is_valid || itrs2.num_iters != 1) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters <= 2) ) {
      if (itrs1.size() > trg.size() || 
          itrs1.size() > itrs2.size()) range_overflow();
      svi last3 = transform(itrs1.beg(),itrs1.end(),itrs2.beg(),trg.beg(),fun);
      res = iter_pos(trg.vec, last3);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      transform(itrs1.beg(), itrs1.end(), itrs2.beg(), back_inserter(*v), fun);
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
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    replace_if(itrs.beg(), itrs.end(), pxh_pred1(pred), val);
  } catch (px* e) {
    pure_throw(e);
  }
}

int sva_replace_copy_if(px* tpl1, px* tpl2, px* pred, px* val){
  int res = 0;
  pxh_pred1 fun(pred);
  sv_iters itrs(tpl1);
  sv_iters trg(tpl2);
  sv_back_iter bak(tpl2);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      svi first2 = trg.beg();
      if (itrs.contains(trg.vec, first2) ) bad_argument();
      if (itrs.size() > trg.size()) range_overflow();
      replace_copy_if(itrs.beg(), itrs.end(), first2, fun, val);
      res = svend;
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==itrs.vec) bad_argument();
      replace_copy_if(itrs.beg(), itrs.end(), back_inserter(*v), fun, val);
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
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    fill(itrs.beg(), itrs.end(), val);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_fill_n(px* tpl, int n, px* val)
{
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 1) bad_argument();
  try {
    fill_n(itrs.beg(), n, val);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_generate(px* tpl, px* gen)
{
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  pxh_gen genh(gen);
  try {
    generate(itrs.beg(), itrs.end(), genh);
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_generate_n(px* tpl, int n, px* gen)
{
  sv_iters trg(tpl);
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
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    svi pei = remove_if(itrs.beg(), itrs.end(), pxh_pred1(pred));
    return iter_pos(itrs.vec, pei);
  } catch (px* e) {
    pure_throw(e);
  }
}

int sva_remove_copy_if(px* tpl1, px* tpl2, px* pred)
{
  int res = 0;
  pxh_pred1 fun(pred);
  sv_iters itrs(tpl1);
  sv_iters trg(tpl2);
  sv_back_iter bak(tpl2);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      svi first2 = trg.beg();
      if (itrs.contains(trg.vec, first2) ) bad_argument();
      if (itrs.size() > trg.size()) range_overflow();
      svi pei = remove_copy_if(itrs.beg(), itrs.end(), first2, fun);
      res =  iter_pos(trg.vec, pei);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==itrs.vec) bad_argument();
      remove_copy_if(itrs.beg(), itrs.end(), back_inserter(*v), fun);
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
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    svi pei = unique(itrs.beg(), itrs.end(), pxh_pred2(bin_pred));
    return iter_pos(itrs.vec, pei);
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_unique_copy(px* tpl1, px* tpl2, px* bin_pred)
{
  int res = 0;
  pxh_pred2 fun(bin_pred);
  sv_iters itrs(tpl1);
  sv_iters trg(tpl2);
  sv_back_iter bak(tpl2);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    if ( (trg.is_valid && trg.num_iters == 1) ) {
      svi first2 = trg.beg();
      if (itrs.contains(trg.vec, first2) ) bad_argument();
      if (itrs.size() > trg.size()) range_overflow();
      svi pei = unique_copy(itrs.beg(), itrs.end(), first2, fun);
      res =  iter_pos(trg.vec, pei);
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==itrs.vec) bad_argument();
      unique_copy(itrs.beg(), itrs.end(), back_inserter(*v), fun);
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
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    reverse(itrs.beg(), itrs.end());
  } catch (px* e) {
    pure_throw(e);
  }
}

int  sva_reverse_copy(px* tpl1, px* tpl2)
{
  int res = 0;
  sv_iters itrs(tpl1);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  sv_iters trg(tpl2);
  sv_back_iter bak(tpl2);
  try {
    if (trg.is_valid && trg.num_iters == 1) {
      if (trg.vec==itrs.vec) bad_argument();
      if (itrs.size() > trg.size()) range_overflow();
      svi pei = reverse_copy(itrs.beg(), itrs.end(), trg.beg()); 
      res =  iter_pos(trg.vec, pei);   
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==itrs.vec) bad_argument();
      reverse_copy(itrs.beg(), itrs.end(), back_inserter(*v));
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
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 3) bad_argument();
  try {
  rotate(itrs.beg(), itrs.mid(), itrs.end());
  } catch (px* e) {
    pure_throw(e);
  }
}

void sva_rotate_copy(px* tpl1, px* tpl2){
  sv_iters itrs(tpl1);
  if (!itrs.is_valid || itrs.num_iters != 3) bad_argument();
  sv_iters trg(tpl2);
  sv_back_iter bak(tpl2);
  try {
    if (trg.is_valid && trg.num_iters <= 2) {
      if (trg.vec==itrs.vec) bad_argument();
      if (itrs.size() > trg.size()) range_overflow();
      rotate_copy(itrs.beg(), itrs.mid(), itrs.end(), trg.beg());    
    }
    else if (bak.is_valid) {
      sv* v = bak.vec;
      if (v==itrs.vec) bad_argument();
      rotate_copy(itrs.beg(), itrs.mid(), itrs.end(), back_inserter(*v));
    }
    else
      bad_argument();
  } catch (px* e) {
    pure_throw(e);
  }
}


void sva_random_shuffle(px* tpl) 
{
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    random_shuffle(itrs.beg(), itrs.end());  
  } catch (px* e) {
    pure_throw(e);
  }
}


int sva_partition(px* tpl, px* pred)
{
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    sv* v = itrs.vec;
    svi mid = partition(itrs.beg(), itrs.end(), pxh_pred1(pred));
    return iter_pos(v,mid);
  } catch (px* e) {
    pure_throw(e);
  }
}

int sva_stable_partition(px* tpl, px* pred)
{
  sv_iters itrs(tpl);
  if (!itrs.is_valid || itrs.num_iters != 2) bad_argument();
  try {
    sv* v = itrs.vec;
    svi mid = stable_partition(itrs.beg(), itrs.end(), pxh_pred1(pred));
    return iter_pos(v,mid);
  } catch (px* e) {
    pure_throw(e);
  }
}
