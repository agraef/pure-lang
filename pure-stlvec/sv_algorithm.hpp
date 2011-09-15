#ifndef STLVEC_ALGORITHM_H
#define STLVEC_ALGORITHM_H

#include "stlvec.hpp"

extern "C" {

  /**** Nonmodifying Sequence Algorithms *************************************/

  void sva_for_each(px* tpl, px* unary_fun);
  int  sva_find_if(px* tpl, px* pred);  
  int  sva_find_first_of(px* tpl1, px* tpl2, px* cmp);
  int  sva_adjacent_find(px* tpl, px* cmp);
  int  sva_count_if(px* tpl, px* unary_pred);
  px*  sva_mismatch(px* tpl1, px* tpl2, px* cmp);
  bool sva_equal(px* tpl, px* tpl2, px* cmp);

  int  sva_search(px* tpl1, px* tpl2, px* cmp);
  int  sva_search_n(px* tpl1, int count, px* val,  px* cmp);

  int  sva_find_end(px* tpl1, px* tpl2, px* cmp);

  /**** Modifying Sequence Algorithms ****************************************/

  int  sva_copy(px* tpl1, px* tpl2);
  int  sva_copy_backward(px* tpl1, px* tpl2);
  void sva_swap_ranges(px* tpl1, px* tpl2);
  int  sva_transform(px* tpl1, px* tpl2, px* unary_op);
  int  sva_transform_2(px* tpl1, px* tpl2, px* tpl3, px* bin_op);
  void sva_replace_if(px* tpl, px* pred, px* val);
  int  sva_replace_copy_if(px* tpl1, px* tpl2, px* pred, px* val);
  void sva_fill(px* tpl, px* val);
  void sva_fill_n(px* tpl, int n, px* val);
  void sva_generate(px* tpl, px* gen);
  void sva_generate_n(px* tpl, int n, px* gen);
  int  sva_remove_if(px* tpl, px* pred);
  int  sva_remove_copy_if(px* tpl1, px* tpl2, px* pred);
  int  sva_unique(px* tpl, px* bin_pred);
  int  sva_unique_copy(px* tpl1, px* tpl2, px* bin_pred);
  void sva_reverse(px* tpl);
  int  sva_reverse_copy(px* tpl1, px* tpl2);
  void sva_rotate(px* tpl);
  void sva_rotate_copy(px* tpl1, px* tpl2);
  void sva_random_shuffle(px* tpl);  
  int  sva_partition(px* tpl, px* pred);
  int  sva_stable_partition(px* tpl, px* pred);

  /**** Sort Related Algorithms ************************************/

  void sva_sort(px* tpl, px* cmp);
  void sva_stable_sort(px* tpl, px* cmp);
  void sva_partial_sort(px* tpl, px* cmp);
  void sva_partial_sort_copy(px* tpl1, px* tpl2, px* cmp);
  void sva_nth_element(px* tpl, px* cmp);
  int  sva_lower_bound(px* tpl, px* val, px* cmp);
  int  sva_upper_bound(px* tpl, px* val, px* cmp);
  px*  sva_equal_range(px* tpl, px* val, px* cmp);
  bool sva_binary_search(px* tpl, px* val, px* cmp);

  /**** Merge (operating on sorted ranges) ***************************/

  int  sva_merge(px* tpl1, px* tpl2, px* tpl3, px* cmp);
  void sva_inplace_merge(px* tpl, px* cmp);
  bool sva_includes(px* tpl1, px* tpl2, px* cmp);
  int  sva_set_union(px* tpl1, px* tpl2, px* tpl3, px* cmp);
  int  sva_set_intersection(px* tpl1, px* tpl2, px* tpl3, px* cmp);
  int  sva_set_difference(px* tpl1, px* tpl2, px* tpl3, px* cmp);
  int  sva_set_symmetric_difference(px* tpl1, px* tpl2,px* tpl3,px* cmp);
 
  /**** Heap *********************************************************/

  void sva_push_heap(px* tpl1, px* cmp);
  void sva_pop_heap(px* tpl1, px* cmp);
  void sva_make_heap(px* tpl1, px* cmp);
  void sva_sort_heap(px* tpl1, px* cmp);

  /**** Minmax *******************************************************/
  
  int  sva_min_element(px* tpl, px* cmp);
  int  sva_max_element(px* tpl, px* cmp);
  bool sva_lexicographical_compare(px* tpl1, px* tpl2, px* cmp);
  bool sva_next_permutation(px* tpl, px* cmp);
  bool sva_prev_permutation(px* tpl, px* cmp);

  /**** Numeric Algorithms *******************************/
  
  px*  sva_accumulate(px* tpl1, px* val, px* bin_op);
  px*  sva_inner_product(px* tpl1,
                         px* tpl2, px* val,
                         px* bin_op1, px* bin_op2);
  int  sva_partial_sum(px* tpl1, px* tpl2, px* bin_op);
  int  sva_adjacent_difference(px* tpl1, px* tpl2, px* bin_op);

}
#endif // STLVEC_ALGORITHM_H
