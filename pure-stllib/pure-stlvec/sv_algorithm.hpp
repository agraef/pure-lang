#ifndef STLVEC_ALGORITHM_H
#define STLVEC_ALGORITHM_H

#include "stlvec.hpp"

extern "C" {

  /**** Nonmodifying Sequence Algorithms *************************************/

  void stl_sva_for_each(px* tpl, px* unary_fun);
  int  stl_sva_find_if(px* tpl, px* pred);  
  int  stl_sva_find_first_of(px* tpl1, px* tpl2, px* cmp);
  int  stl_sva_adjacent_find(px* tpl, px* cmp);
  int  stl_sva_count_if(px* tpl, px* unary_pred);
  px*  stl_sva_mismatch(px* tpl1, px* tpl2, px* cmp);
  bool stl_sva_equal(px* tpl, px* tpl2, px* cmp);

  int  stl_sva_search(px* tpl1, px* tpl2, px* cmp);
  int  stl_sva_search_n(px* tpl1, int count, px* val,  px* cmp);

  int  stl_sva_find_end(px* tpl1, px* tpl2, px* cmp);

  /**** Modifying Sequence Algorithms ****************************************/

  int  stl_sva_copy(px* tpl1, px* tpl2);
  int  stl_sva_copy_backward(px* tpl1, px* tpl2);
  void stl_sva_swap_ranges(px* tpl1, px* tpl2);
  int  stl_sva_transform(px* tpl1, px* tpl2, px* unary_op);
  int  stl_sva_transform_2(px* tpl1, px* tpl2, px* tpl3, px* bin_op);
  void stl_sva_replace_if(px* tpl, px* pred, px* val);
  int  stl_sva_replace_copy_if(px* tpl1, px* tpl2, px* pred, px* val);
  void stl_sva_fill(px* tpl, px* val);
  void stl_sva_fill_n(px* tpl, int n, px* val);
  void stl_sva_generate(px* tpl, px* gen);
  void stl_sva_generate_n(px* tpl, int n, px* gen);
  int  stl_sva_remove_if(px* tpl, px* pred);
  int  stl_sva_remove_copy_if(px* tpl1, px* tpl2, px* pred);
  int  stl_sva_unique(px* tpl, px* bin_pred);
  int  stl_sva_unique_copy(px* tpl1, px* tpl2, px* bin_pred);
  void stl_sva_reverse(px* tpl);
  int  stl_sva_reverse_copy(px* tpl1, px* tpl2);
  void stl_sva_rotate(px* tpl);
  void stl_sva_rotate_copy(px* tpl1, px* tpl2);
  void stl_sva_random_shuffle(px* tpl, int seed);  
  int  stl_sva_partition(px* tpl, px* pred);
  int  stl_sva_stable_partition(px* tpl, px* pred);

  /**** Sort Related Algorithms ************************************/

  void stl_sva_sort(px* tpl, px* cmp);
  void stl_sva_stable_sort(px* tpl, px* cmp);
  void stl_sva_partial_sort(px* tpl, px* cmp);
  void stl_sva_partial_sort_copy(px* tpl1, px* tpl2, px* cmp);
  void stl_sva_nth_element(px* tpl, px* cmp);
  int  stl_sva_lower_bound(px* tpl, px* val, px* cmp);
  int  stl_sva_upper_bound(px* tpl, px* val, px* cmp);
  px*  stl_sva_equal_range(px* tpl, px* val, px* cmp);
  bool stl_sva_binary_search(px* tpl, px* val, px* cmp);

  /**** Merge (operating on sorted ranges) ***************************/

  int  stl_sva_merge(px* tpl1, px* tpl2, px* tpl3, px* cmp);
  void stl_sva_inplace_merge(px* tpl, px* cmp);
  bool stl_sva_includes(px* tpl1, px* tpl2, px* cmp);
  int  stl_sva_set_union(px* tpl1, px* tpl2, px* tpl3, px* cmp);
  int  stl_sva_set_intersection(px* tpl1, px* tpl2, px* tpl3, px* cmp);
  int  stl_sva_set_difference(px* tpl1, px* tpl2, px* tpl3, px* cmp);
  int  stl_sva_set_symmetric_difference(px* tpl1, px* tpl2,px* tpl3,px* cmp);
 
  /**** Heap *********************************************************/

  void stl_sva_push_heap(px* tpl1, px* cmp);
  void stl_sva_pop_heap(px* tpl1, px* cmp);
  void stl_sva_make_heap(px* tpl1, px* cmp);
  void stl_sva_sort_heap(px* tpl1, px* cmp);

  /**** Minmax *******************************************************/
  
  int  stl_sva_min_element(px* tpl, px* cmp);
  int  stl_sva_max_element(px* tpl, px* cmp);
  bool stl_sva_lexicographical_compare(px* tpl1, px* tpl2, px* cmp);
  bool stl_sva_next_permutation(px* tpl, px* cmp);
  bool stl_sva_prev_permutation(px* tpl, px* cmp);

  /**** Numeric Algorithms *******************************/
  
  px*  stl_sva_accumulate(px* tpl1, px* val, px* bin_op);
  px*  stl_sva_inner_product(px* tpl1,
                         px* tpl2, px* val,
                         px* bin_op1, px* bin_op2);
  int  stl_sva_partial_sum(px* tpl1, px* tpl2, px* bin_op);
  int  stl_sva_adjacent_difference(px* tpl1, px* tpl2, px* bin_op);

}
#endif // STLVEC_ALGORITHM_H
