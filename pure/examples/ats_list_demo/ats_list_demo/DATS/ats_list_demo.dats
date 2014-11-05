#define ATS_DYNLOADFLAG 0

#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload "ats_list_demo/SATS/ats_list_demo.sats"

implement purexp_list_to_array lst =
  let
    var size : size_t
    var exprs : ptr
    val is_list = pure_is_listv (lst, size, addr@ exprs)
    val () =
      if not is_list then
        pure_throw (pure_not_a_list_exception lst)
    val [n : int] size = g1ofg0_uint size
    val [adr : addr] exprs = g1ofg0_ptr exprs
    extern praxi make_pf_view :
        {adr : addr; n : nat} () -> array_v (purexp, adr, n)
    extern praxi make_pf_free :
        {adr : addr} () -> mfree_libc_v (adr)
  in
    (make_pf_view {adr, n} (), make_pf_free {adr} () | size, exprs)
  end

implement pure_head_tail lst =
  let
    var head : purexp
    var tail : purexp
    val () = pure_get_head_tail (lst, head, tail)
  in
    @(head, tail)
  end

implement pure_list_match lst =
  if pure_is_nil lst then
    pure_list_nil ()
  else
    let
      var head : purexp
      var tail : purexp
      val () = pure_get_head_tail (lst, head, tail)
    in
      pure_list_cons (@(head, tail))
    end

implement pure_list_length (lst) =
  let
    var length : size_t
    val is_list = pure_is_listv (lst, length, the_null_ptr)
  in
    if is_list then
      length
    else
      begin
        pure_throw (pure_not_a_list_exception lst);
        i2sz 0                  (* To satisfy type-checking. *)
      end
  end

implement pure_list_reverse (lst) =
  reverse (lst, pure_nil ())
   where
    {
      fun reverse (old_lst : purexp, new_lst : purexp) : purexp =
        let
          val match = pure_list_match old_lst
        in
          case+ match of
            | ~pure_list_cons (@(head, tail)) =>
              reverse (tail, pure_cons (head, new_lst))
            | ~pure_list_nil () =>
              new_lst
        end
    }

implement pure_list_merge (lt, xs, ys) =
  (* Note: this implementation is not tail-recursive. It is based on
     an example in ‘Introduction to Programming in ATS’. *)
  let
    val match_xs = pure_list_match xs
  in
    case+ match_xs of
      | ~pure_list_cons (@(x, xs1)) =>
        let
          val match_ys = pure_list_match ys
        in
          case+ match_ys of
            | ~pure_list_cons (@(y, ys1)) =>
              let
                val lt_result =
                  $extfcall (purexp, "pure_appl", lt, i2sz 2, y, x)
                var y_is_lt_x : int32
                val is_int = pure_is_int (lt_result, y_is_lt_x)
              in
                if is_int && y_is_lt_x = $UNSAFE.cast {int32} 0 then
                  (* x <= y *)
                  pure_cons (x, pure_list_merge (lt, xs1, ys))
                else
                  (* y < x *)
                  pure_cons (y, pure_list_merge (lt, xs, ys1))
              end
            | ~pure_list_nil () => xs
        end
      | ~pure_list_nil () => ys
  end

implement pure_list_mergesort (lt, xs) =
  (* Note: this implementation is not tail-recursive. Also, it is a
     pure mergesort, with no special handling of small lists. It is
     based on an example in ‘Introduction to Programming in ATS’. *)
  msort (lt, xs, pure_list_length xs)
   where
    {
      fun msort (lt : purexp, xs : purexp, n : size_t) : purexp =
        if n <= 1 then
          xs
        else
          split (lt, xs, n, half n, pure_nil ())

      and split (lt : purexp, xs : purexp, n : size_t,
                 i : size_t, xsf : purexp) : purexp =
        if i2sz 0 < i then
          let
            val @(x, xs) = pure_head_tail xs
          in
            split (lt, xs, n, pred i, pure_cons (x, xsf))
          end
        else
          pure_list_merge (lt,
                           msort (lt, pure_list_reverse xsf, half n),
                           msort (lt, xs, n - half n))
    }

implement pure_list_rotate (lst, n) =
  (* Note: this implementation uses a temporary array of the same
     length as the list. *)
  let
    val [n : int] n = g1ofg0_uint n
  in
    if n = i2sz 0 then
      lst                (* A trivial case: there is nothing to move. *)
    else
      let
        (* Typechecking will fail if the array created here is not
           freed. *)
        val [sz : int] (pf_view, pf_free | size, arr) =
          purexp_list_to_array lst
      in
        if size < n then
          begin  (* The list is too short. Throw a Pure exception. *)
            free_purexp_array (pf_view, pf_free | arr);
            pure_throw (pure_incompatible_list_length_exception (lst, n));
            lst                   (* To satisfy the typechecker. *)
          end
        else if size = i2sz 0 then
          begin  (* A trivial case: the array is empty. *)
            free_purexp_array (pf_view, pf_free | arr);
            lst
          end
        else if n = size then
          begin  (* A trivial case: there is nothing to move. *)
            free_purexp_array (pf_view, pf_free | arr);
            lst
          end
        else
          let
            fun rotate
                {i : nat | i < sz} {n : pos | n < sz}
                (A : &(@[purexp][sz]), i : size_t i,
                 increment : size_t n, temp : purexp) : void =
              let
                val j = (i + increment) mod size
              in
                if j = i2sz 0 then
                  A[i] := temp
                else
                  begin
                    A[i] := A[j];
                    rotate (A, j, increment, temp)
                  end
              end

            val () = rotate (!arr, i2sz 0, n, arr->[0])
            val result = pure_listv (size, !arr)
            val () = free_purexp_array (pf_view, pf_free | arr);
          in
            result
          end
      end
  end

implement pure_nil__extern () = pure_nil ()
implement pure_is_nil__extern (expr) = pure_is_nil (expr)
implement pure_cons__extern (a, b) = pure_cons (a, b)
implement pure_is_cons__extern (expr) = pure_is_cons (expr)
implement pure_head__extern (lst) = pure_head (lst)
implement pure_tail__extern (lst) = pure_tail (lst)
implement pure_list_length__extern (lst) = pure_list_length (lst)
implement pure_list_reverse__extern (lst) = pure_list_reverse (lst)
