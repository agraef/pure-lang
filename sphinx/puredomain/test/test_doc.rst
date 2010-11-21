================
Pure Domain Test
================

Passes test if all links in `Test cases`_ below are valid.

Sample descriptions
===================

.. default-role:: emphasis
.. default-domain:: pure

Default Namespace
-----------------

This is a function in the default namespace:

.. function:: sort p xs

   Sorts the elements of the list `xs` in ascending order according to the
   given predicate `p`.

Another entry for the same function. This happens a lot in Pure, as functions
are frequently overloaded.

.. function:: sort /matrix p x::matrix

   Like :func:`sort` above. Sorts the elements of a matrix `x` according to
   the given predicate `p`.

Operator notation is supported as well.

.. function:: infix + x y

   Add two values. This works with numbers, strings and lists.

.. function:: prefix - x

   Unary minus. Negates a number.

.. function:: postfix & x

   Creates a thunk which gets evaluated lazily. This is the only postfix
   operator defined in the prelude.

.. function:: outfix {: :} x

   An outfix operator.

Module and Namespace
--------------------

.. module:: gsl
   :synopsis: Pure GSL module
   :platform: Windows, Mac, Unix

This is documentation for the :mod:`gsl` module.

The following functions are also declared in the `gsl` namespace (modules and
namespaces are separate in Pure, like in C++).

.. namespace:: gsl

.. function:: sort_vector m::matrix

   Implements `gsl_sort` and `gsl_sort_int`.

.. function:: sort_vector_index m::matrix

   Implements `gsl_sort_index` and `gsl_sort_int_index`.

More markup
-----------

.. currentmodule:: None
.. namespace:: None

The usual markup for embellished function descriptions is supported as well.

.. function:: foldl f a xs -> b

   Accumulate the binary function `f` over all members of `xs`, starting from
   the initial value `a` and working from the front of the list towards its
   end.
   
   :param f:  accumulating function
   :type  f:  closure
   :param a:  initial value
   :type  a:  any type
   :param xs: values to be accumulated
   :type  xs: list
   :return:   accumulated value `b`
   :rtype:    any (usually the same as `a`, but that depends on `f`)

There's also special markup for extern functions, macros, constructors,
variables and constants.

.. extern:: puts s::string

   Output a string on the terminal.

.. macro:: void (catmap f x) = do f x
   		void (listmap f x) = do f x

   Helper macro to execute a list comprehension which is evaluated solely for
   its side-effects.

.. constructor:: infix : x xs

   The list constructor. Note that ``[x1,x2,...,xn] === x1:x2:...:xn:[]``.

.. variable:: stdin

   The standard input stream. This is a built-in variable.

.. constant:: c = 299792

   A constant. The speed of light, what else?

Test cases
==========

Unqualified access in the default namespace
-------------------------------------------

See :func:`foldl` and :func:`sort` above. Also see :func:`sort/matrix` for the
matrix version. Also see :macro:`void`, :var:`stdin`, :const:`c`.

This should work just as well (using ``pure:obj`` as the default role):

.. default-role:: obj

See `foldl` and `sort` above. Also see `sort/matrix` for the matrix
version. Also see `void`, `stdin`, `c`.

Unqualified access in a custom namespace
----------------------------------------

.. namespace:: gsl

Current namespace is "gsl" from the `gsl` module.

See `sort_vector` and `sort_vector_index` above.

Qualified access works, too:

See `gsl::sort_vector` and `gsl::sort_vector_index` above.

Qualified access in a different namespace
-----------------------------------------

.. namespace:: dummy

Current namespace is "dummy".

See `gsl::sort_vector` and `gsl::sort_vector_index` above.

See `::foldl` and `::sort` above. Also see `::sort/matrix` for the matrix
version. Also see `::void`, `::stdin`, `::c`.

Qualified access suppressing the namespace qualifier in the display
-------------------------------------------------------------------

See `~gsl::sort_vector` and `~gsl::sort_vector_index` above.

See `~::foldl` and `~::sort` above. Also see `~::sort/matrix` for the matrix
version. Also see `~::void`, `~::stdin`, `~::c`.
