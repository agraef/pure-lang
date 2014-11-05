#define ATS_EXTERN_PREFIX "atslistdemo__"

%{#
#include "ats_list_demo/CATS/ats_list_demo.cats"
%}

(* An abstract pointer representing a general Pure expression. *)
abstype purexp = $extype "pure_expr *"

(* Support for Pure exceptions. *)
fun pure_throw : purexp -> void = "mac#%"
fun pure_not_a_cons_exception : purexp -> purexp = "mac#%"
fun pure_not_a_list_exception : purexp -> purexp = "mac#%"
fun pure_incompatible_list_length_exception :
    (purexp, size_t) -> purexp = "mac#%"

(* A wrapper around pure_listv. *)
fun pure_listv :
    {n : nat} {m : nat | n <= m}
    (size_t n, &(@[purexp][m])) -> purexp = "mac#%"

(* A wrapper around pure_is_int from <pure/runtime.h>. For simplicity,
   this wrapper initializes the integer to zero if the boolean return
   value is false. *)
fun pure_is_int :
    (purexp, &int32? >> int32 i) ->
      #[b : bool; i : int | b == true || i == 0] bool b = "mac#%"

(* A wrapper around pure_is_listv from <pure/runtime.h>. For
   simplicity, this wrapper initializes the size to zero if the
   boolean return value is false. The third argument may be a null
   pointer. *)
fun pure_is_listv :
    (purexp, &size_t? >> size_t i, ptr) ->
      #[b : bool; i : nat | b == true || i == 0] bool b = "mac#%"

(* Return a linear array of the same length as the given Pure list,
   and with the same elements in the same order. *)
fun purexp_list_to_array :
    purexp ->
      [n : nat] [adr : addr]
      (array_v (purexp, adr, n), mfree_libc_v (adr) | size_t n, ptr adr)

(* Free an array, such as that returned by purexp_list_to_array. *)
fun free_purexp_array :
    {n : int} {adr : addr}
    (array_v (purexp, adr, n), mfree_libc_v (adr) | ptr adr) -> void
      = "mac#%"

(* Return an empty Pure list. *)
fun pure_nil : () -> purexp = "mac#%"

(* Is the expression an empty Pure list? *)
fun pure_is_nil : purexp -> bool = "mac#%"

(* Return the cons of two Pure expressions. *)
fun pure_cons : (purexp, purexp) -> purexp = "mac#%"

(* Is the expression a Pure cons cell? *)
fun pure_is_cons : purexp -> bool = "mac#%"

(* Get the head and/or tail of a Pure cons cell. *)
fun pure_get_head_tail :
    (purexp, &purexp? >> purexp, &purexp? >> purexp) -> void = "mac#%"
fun pure_head_tail : purexp -> @(purexp, purexp) (* head, tail *)
fun pure_head : purexp -> purexp = "mac#%"
fun pure_tail : purexp -> purexp = "mac#%"

(* Pattern matching for Pure lists. Make the type linear (datavtype
   instead of datatype) so we can use the libc malloc without
   leaks. Otherwise, to minimize leakage, one would have to use a
   conservative garbage collector such as the Boehm GC. *)
datavtype pure_list_match_type =
  | pure_list_cons of @(purexp, purexp) (* head, tail *)
  | pure_list_nil of ()

fun pure_list_match : purexp -> pure_list_match_type

(* Return the length of a Pure list. *)
fun pure_list_length : purexp -> size_t

(* Return the reverse of a Pure list. *)
fun pure_list_reverse : purexp -> purexp

(* Stably merge two ordered lists. The first argument is a ‘less than’
   function. This implementation is not tail recursive. *)
fun pure_list_merge : (purexp, purexp, purexp) -> purexp = "ext#"

(* Stably mergesort a list. The first argument is a ‘less than’
   function.  This implementation is not tail recursive. *)
fun pure_list_mergesort : (purexp, purexp) -> purexp = "ext#"

(* Rotate the first n elements from the head of a Pure list to its
   tail. This implementation uses a temporary array of the same length
   as the list. *)
fun pure_list_rotate : (purexp, size_t) -> purexp = "ext#"

(* Exported versions of fundamental functions, for access from Pure
   code. These functions are redundant with Pure’s own functionality,
   and are included for the sake of demonstration. *)
fun pure_nil__extern : () -> purexp = "ext#"
fun pure_is_nil__extern : purexp -> bool = "ext#"
fun pure_cons__extern : (purexp, purexp) -> purexp = "ext#"
fun pure_is_cons__extern : purexp -> bool = "ext#"
fun pure_head__extern : purexp -> purexp = "ext#"
fun pure_tail__extern : purexp -> purexp = "ext#"
fun pure_list_length__extern : purexp -> size_t = "ext#"
fun pure_list_reverse__extern : purexp -> purexp = "ext#"