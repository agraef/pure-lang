<a name="doc-pure-mpfr"></a>

pure-mpfr
=========

<a name="module-mpfr"></a>

Version 0.5, March 06, 2017

Albert Graef &lt;<aggraef@gmail.com>&gt;

The [GNU MPFR](http://www.mpfr.org/) library is a C library for
multiple-precision floating-point computations with correct rounding. It is
based on [GMP](http://gmplib.org) which Pure also uses for its bigint support.

This module makes the MPFR multiprecision floats (henceforth referred to as
`mpfr` numbers or values) available in Pure, so that they work with the other
types of Pure numbers in an almost seamless fashion. Pure `mpfr` values are
represented as pointers which can readily be passed as arguments to the MPFR
functions, so the representation only involves minimal overhead on the Pure
side.

The module defines the type of `mpfr` values as an instance of Pure's `real`
type, so that it becomes a well-behaved citizen of Pure's numeric tower.
Memory management of these values is automatic. You can create an `mpfr` value
from any other kind of Pure real value (`int`, `bigint` or `double`), or from
a string in decimal notation, using the `mpfr` function. Back conversions are
provided from `mpfr` to `int`, `bigint`, `double` and `string` (the latter by
means of a custom pretty-printer installed by this module, so that mpfr values
are printed in a format similar to the `printf %g` format). Integration with
Pure's `complex` type is provided as well.

Please note that this module needs more testing and the API hasn't been
finalized yet, but it should be perfectly usable already. As usual, please
report any bugs on the Pure issue tracker, on the Pure mailing list, or
directly to the author, see <http://purelang.bitbucket.org/>.

Copying
-------

Copyright (c) 2011 by Albert Graef.

pure-mpfr is free software: you can redistribute it and/or modify it under the
terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

pure-mpfr is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
details.

You should have received a copy of the GNU Lesser General Public License along
with this program. If not, see &lt;<http://www.gnu.org/licenses/>&gt;.

Installation
------------

Get the latest source from
<https://bitbucket.org/purelang/pure-lang/downloads/pure-mpfr-0.5.tar.gz>.

Run `make` to compile the module and `make install` (as root) to install it in
the Pure library directory. This requires GNU make, and of course you need to
have Pure and libmpfr installed.

`make` tries to guess your Pure installation directory and platform-specific
setup. If it gets this wrong, you can set some variables manually, please
check the Makefile for details.

------------------------------------------------------------------------------

> **Note:** This module requires Pure 0.50 or later and libmpfr 3.x (3.0.0 has
> been tested). Older libmpfr versions (2.x) probably require some work.

------------------------------------------------------------------------------

Usage
-----

After installation, you can use the operations of this module by placing the
following import declaration in your Pure programs:

    using mpfr;

------------------------------------------------------------------------------

> **Note:** This also pulls in the [math](#module-math) standard library
> module, whose operations are overloaded by the `mpfr` module in order to
> provide support for `mpfr` values. Thus you don't need to explicitly import
> the `math` module when using the `mpfr` module.
>
> If you use both the [mpfr](#module-mpfr) module and the
> [pointers](#module-pointers) standard library module in your script, make
> sure that you import the `pointers` module *after* `mpfr`, so that the
> definitions of pointer arithmetic in the `pointers` module do not interfere
> with the overloading of arithmetic operations in the `mpfr` module.

------------------------------------------------------------------------------

### Precision and Rounding

The following operations of the MPFR library are provided to inspect and
change the default precision and rounding modes used by MPFR.

<a name="mpfr_get_default_prec"></a>`mpfr_get_default_prec`, <a name="mpfr_set_default_prec"></a>`mpfr_set_default_prec prec`
:   Get and set the default precision in terms of number of bits in the
    mantissa, including the sign. MPFR initially sets this to 53 (matching the
    mantissa size of `double` values). It can be changed to any desired value
    not less than 2.

<a name="mpfr_get_prec"></a>`mpfr_get_prec x`
:   Get the precision of an `mpfr` number `x`. Note that `mpfr` numbers always
    keep the precision they were created with, but it is possible to create a
    new `mpfr` number with any given precision from an existing `mpfr` number
    using the [`mpfr`](#mpfr) function, see below.

<a name="mpfr_get_default_rounding_mode"></a>`mpfr_get_default_rounding_mode`, <a name="mpfr_set_default_rounding_mode"></a>`mpfr_set_default_rounding_mode rnd`
:   Get and set the default rounding mode, which is used for all arithmetic
    operations and mathematical functions provided by this module. The given
    rounding mode `rnd` must be one of the supported rounding modes listed
    below.

<a name="MPFR_RNDN"></a>*constant* `MPFR_RNDN // round to nearest, with ties to even`

:   

<a name="MPFR_RNDZ"></a>*constant* `MPFR_RNDZ // round toward zero`

:   

<a name="MPFR_RNDU"></a>*constant* `MPFR_RNDU // round toward +Inf`

:   

<a name="MPFR_RNDD"></a>*constant* `MPFR_RNDD // round toward -Inf`

:   

<a name="MPFR_RNDA"></a>*constant* `MPFR_RNDA // round away from zero`
:   Supported rounding modes. Please check the MPFR documentation for details.

<!-- -->
In addition, the following operations enable you to control the precision in
textual representations of `mpfr` values. This information is used by the
custom pretty-printer for `mpfr` values installed by the module.

<a name="mpfr_get_print_prec"></a>`mpfr_get_print_prec`, <a name="mpfr_set_print_prec"></a>`mpfr_set_print_prec prec`
:   Get and set the precision (number of decimal digits in the mantissa) used
    by the pretty-printer.

<!-- -->
### MPFR Numbers

The module defines the following data type for representing `mpfr` values,
which is a subtype of the Pure [`real`](#real/type) type:

<a name="mpfr/type"></a>*type* `mpfr`
:   This is a tagged pointer type (denoted `mpfr*` in Pure extern
    declarations) which is compatible with the `mpfr_t` and `mpfr_ptr` data
    types of the MPFR C library. Members of this type are "cooked" pointers,
    which are allocated dynamically and freed automatically when they are
    garbage-collected (by means of a corresponding Pure sentry).

<a name="mpfrp"></a>`mpfrp x`
:   Type predicate checking for `mpfr` values.

<!-- -->
### Conversions

The following operations are provided to convert between `mpfr` numbers and
other kinds of Pure `real` values.

<a name="mpfr"></a>`mpfr x`, <a name="mpfr/2"></a>`mpfr (x,prec)`, <a name="mpfr/3"></a>`mpfr (x,prec,rnd)`

:   This function converts any real number ([`int`](#int/type),
    [`bigint`](#bigint/type), [`double`](#double/type),
    [`rational`](#rational/type), [`mpfr`](#mpfr/type)) to an `mpfr` value.

    Optionally, it is possible to specify a precision (number of bits in the
    mantissa) `prec` and a rounding mode `rnd` (one of the `MPFR_RND`
    constants), otherwise MPFR's default precision and rounding mode are used
    (see [Precision and Rounding](#precision-and-rounding) above). Note that
    this function may also be used to convert an `mpfr` to a new `mpfr`
    number, possibly with a different precision and rounding.

    The argument `x` can also be a string denoting a floating point number in
    decimal notation with optional sign, decimal point and/or scaling factor,
    which is parsed and converted to an `mpfr` number using the corresponding
    MPFR function.

<a name="int/mpfr"></a>`int x`, <a name="bigint/mpfr"></a>`bigint x`, <a name="double/mpfr"></a>`double x`
:   Convert an `mpfr` number x to the corresponding type of real number.
    Please note that there is no `rational` conversion, as MPFR does not
    provide such an operation, but if you need this then you can first convert
    `x` to a `double` and then apply the standard library
    [`rational`](#rational) function to it (this may loose precision, of
    course).

<a name="str/mpfr"></a>`str x`
:   By virtue of the custom pretty-printer provided by this module, the
    standard library [`str`](#str) function can be used to obtain a printable
    representation of an `mpfr` number `x` in decimal notation. The result is
    a string.

<a name="floor/mpfr"></a>`floor x`, <a name="ceil/mpfr"></a>`ceil x`, <a name="round/mpfr"></a>`round x`, <a name="trunc/mpfr"></a>`trunc x`

:   

<a name="frac/mpfr"></a>`frac x`
:   Rounding and truncation functions. These all take and yield `mpfr`
    numbers. [`frac`](#frac/mpfr) returns the fractional part of an `mpfr`
    number, i.e., `x-trunc x`.

<!-- -->
### Arithmetic

The following standard operators (see the [Pure Library
Manual](#pure-library-manual)) are overloaded to provide `mpfr` arithmetic and
comparisons. These all handle mixed `mpfr`/`real` operands.

<a name="-/mpfr"></a>`- x`, <a name="+/mpfr"></a>`x + y`, <a name="-/mpfr"></a>`x - y`, <a name="*/mpfr"></a>`x * y`, <a name="//mpfr"></a>`x / y`

:   

<a name="^/mpfr"></a>`x ^ y`
:   Arithmetic operations.

<a name="==/mpfr"></a>`x == y`, <a name="~=/mpfr"></a>`x ~= y`, <a name="<=/mpfr"></a>`x <= y`, <a name="&gt;=/mpfr"></a>`x >= y`

:   

<a name="</mpfr"></a>`x < y`, <a name="&gt;/mpfr"></a>`x > y`
:   Comparisons.

<!-- -->
### Math Functions

The following functions from the [math](#module-math) module are overloaded to
provide support for `mpfr` values. Note that it is also possible to invoke the
corresponding functions from the MPFR library in a direct fashion, using the
same function names with an additional `_mpfr` suffix. These functions also
accept other kinds of `real` arguments which are converted to `mpfr` before
applying the MPFR function.

<a name="abs/mpfr"></a>`abs x`
:   Absolute value (this is implemented directly, so there's no corresponding
    `_mpfr` function for this).

<a name="sqrt/mpfr"></a>`sqrt x`, <a name="exp/mpfr"></a>`exp x`, <a name="ln/mpfr"></a>`ln x`, <a name="log/mpfr"></a>`log x`
:   Square root, exponential and logarithms.

<a name="sin/mpfr"></a>`sin x`, <a name="cos/mpfr"></a>`cos x`, <a name="tan/mpfr"></a>`tan x`, <a name="asin/mpfr"></a>`asin x`

:   

<a name="acos/mpfr"></a>`acos x`, <a name="atan/mpfr"></a>`atan x`, <a name="atan2/mpfr"></a>`atan2 y x`
:   Trigonometric functions.

<a name="sinh/mpfr"></a>`sinh x`, <a name="cosh/mpfr"></a>`cosh x`, <a name="tanh/mpfr"></a>`tanh x`, <a name="asinh/mpfr"></a>`asinh x`

:   

<a name="acosh/mpfr"></a>`acosh x`, <a name="atanh/mpfr"></a>`atanh x`
:   Hyperbolic trigonometric functions.

<!-- -->
### Complex Number Support

The following functions from the [math](#module-math) module are overloaded to
provide support for complex values involving `mpfr` numbers:

<a name="complex/mpfr"></a>`complex x`, <a name="polar/mpfr"></a>`polar x`, <a name="rect/mpfr"></a>`rect x`, <a name="cis/mpfr"></a>`cis x`, <a name="arg/mpfr"></a>`arg x`

:   

<a name="re/mpfr"></a>`re x`, <a name="im/mpfr"></a>`im x`, <a name="conj/mpfr"></a>`conj x`

:   <!-- -->

Examples
--------

Import the module and set the default precision:

    > using mpfr;
    > mpfr_set_default_prec 64; // extended precision (long double on x86)
    ()

Calculate pi with the current precision. Note that mixed arithmetic works with
any combination of real and mpfr numbers.

    > let Pi = 4*atan (mpfr 1);
    > pi; Pi; abs (Pi-pi);
    3.14159265358979
    3.14159265358979323851
    1.22514845490862001043e-16

    > let Pi2 = Pi^2;
    > Pi2; sqrt Pi2; sqrt Pi2 == Pi;
    9.86960440108935861941
    3.14159265358979323851
    1

You can also query the precision of a number and change it on the fly:

    > Pi; mpfr_get_prec Pi;
    3.14159265358979323851
    64
    > let Pi1 = mpfr (Pi,53); Pi1; mpfr_get_prec Pi1;
    3.1415926535897931
    53

Complex `mpfr` numbers work, too:

    > let z = mpfr 2^(1/i); z;
    0.769238901363972126565+:-0.638961276313634801184
    > let z = ln z/ln (mpfr 2); z;
    0.0+:-1.0
    > abs z, arg z;
    1.0,-1.57079632679489661926
    > polar z;
    1.0<:-1.57079632679489661926
