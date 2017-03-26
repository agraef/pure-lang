<a name="doc-pure-rational"></a>

Pure-Rational - Rational number library for the Pure programming language
=========================================================================

Version 0.1, March 06, 2017

| Rob Hubbard
| Albert Graef &lt;<aggraef@gmail.com>&gt;
| Jiri Spitz &lt;<jiri.spitz@bluetone.cz>&gt;

This package provides a Pure port of
[Q+Q](http://q-lang.sourceforge.net/addons.html), Rob Hubbard's rational
number library for the Q programming language. The port was done by Jiri
Spitz. It contains `rational.pure`, a collection of utility functions for
rational numbers, and `rat_interval.pure`, a module for doing interval
arithmetic needed by `rational.pure`. These modules are designed to work with
the `math.pure` module (part of the standard Pure library), which contains the
definition of Pure's rational type and implements the basic rational
arithmetic.

This document is an edited version of Rob's original [Q+Q
manual](http://downloads.sourceforge.net/q-lang/rational.pdf?download)
available from the Q website, slightly adjusted to account for the Pure
specifics of the implementation. In particular, note that the operations
provided by `rational.pure` and `rat_interval.pure` live in their own
`rational` and `interval` namespaces, respectively, so if you want to get
unqualified access to the symbols of these modules (as the examples in this
manual assume) then you'll have to import the modules as follows:

    using rational, rat_interval;
    using namespace rational, interval;

Also note that [rational](#module-rational) always pulls in the
[math](#module-math) module, so you don't have to import the latter explicitly
if you are using [rational](#module-rational).

Another minor difference to the Q version of this module is that rational
results always have Pure bigints as their numerators and denominators, hence
the `L` suffix in the printed results. Also, unary minus binds weaker in Pure
than the rational division operator, so a negative rational number will be
printed as, e.g., `(-1L)%2L`, which looks a bit funny but is correct since
Pure rationals always carry their sign in the numerator.

Copying
-------

| Copyright (c) 2006 - 2010 by Rob Hubbard.
| Copyright (c) 2006 - 2010 by Albert Graef &lt;<aggraef@gmail.com>&gt;.
| Copyright (c) 2010 by Jiri Spitz &lt;<jiri.spitz@bluetone.cz>&gt;.

Pure-rational is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

Pure-rational is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU Public License along with this
program. If not, see &lt;<http://www.gnu.org/licenses/>&gt;.

Installation
------------

Get the latest source from
<https://bitbucket.org/purelang/pure-lang/downloads/pure-rational-0.1.tar.gz>.

Then run `make install` (as root) to install pure-rational in the Pure library
directory. This requires GNU make, and of course you need to have Pure
installed.

`make install` tries to guess your Pure installation directory. If it gets
this wrong, you can install using `make install prefix=/usr` which sets the
installation prefix. Please see the Makefile for details.

Introduction
------------

### The Rational Module

<a name="module-rational"></a>

This module provides additional operations on the rational number type
provided by the `math.pure` module in the standard library. The module is
compatible with Pure version 0.43 (onwards).

### The Files and the Default Prelude

The implementation of the rational type and associated utilities is
distributed across various files.

#### math.pure and Other Files

The file `math.pure` defines the type, its constructors and 'deconstructors'
and basic arithmetical and mathematical operators and functions. This is part
of the standard Pure library. A few definitions associated with rationals are
also defined in other standard library modules. In particular, the type tests
are contained in `primitives.pure`.

It is also possible to create rational complex numbers (in addition to double
complex numbers and integral or Gaussian complex numbers). That is, rationals
play nicely with the complex number constructors provided in the `math.pure`
module. This is discussed further in [Rational Complex
Numbers](#rational-complex-numbers).

#### rational.pure

Additional 'rational utilities', not included in the `math.pure` module, are
defined in `rational.pure`. The functions include further arithmetical and
mathematical operators and functions, continued fraction support,
approximation routines and string formatting and evaluation.

The rational utilities include some 'rational complex number' functions.

#### rat\_interval.pure

Amongst the rational utilities are some functions that return a rational
interval. The file `rat_interval.pure` is a partial implementation of interval
arithmetic. Intervals are discussed further in [Intervals](#intervals).

### Notation

Throughout this document, the parameters q, q0, q1, ... usually denote
rationals (∈ **Q**), parameters z, ... usually denote integers (∈ **Z**), r,
... usually denote real numbers (∈ **R**), c, ... usually denote complex
numbers (∈ **C**), n, ... usually denote parameters of any numeric type, v,
... usually denote parameters of any interval type, and x, ... usually denote
parameters of any type.

The reals are not just the doubles, but include rationals and integers. The
term 'rational' usually refers to a rational number ∈ **Q** ⊃ **Z**, or an
expression of type rational or integer.

The Rational Type
-----------------

### Constructors

Rationals are constructed with the `%` exact division operator, and other
kinds of numbers can be converted to rationals with the `rational` function.
These are both defined in math.pure.

<a name="%/rational"></a>`n1 % n2`
:   is the exact division operator, which may be used as a constructor (for
    integers n1 and n2). This is described in [More on
    Division](#more-on-division).

<a name="rational/rational"></a>`rational x`
:   converts the given number `x` to a rational.

<!-- -->
**Example 1** Constructing a fraction:

    > 44%14;
    22L%7L

**Example 2** Converting from an integer:

    > rational 3;
    3L%1L

### 'Deconstructors'

A rational number is in simplest form if the numerator and denominator are
coprime (i.e. do not have a factor in common) and the denominator is positive
(and, specifically, non-zero). Sometimes the term 'irreducible' is used for a
rational in simplest form. This is a property of the representation of the
rational number and not of the number itself.

<a name="num/rational"></a>`num q`
:   given a rational or integer q, returns the ‘(signed) simplest numerator’,
    i.e. the numerator of the normalised form of q.

<a name="den/rational"></a>`den q`
:   given a rational or integer q, returns the ‘(positive) simplest
    denominator’, i.e. the denominator of the normalised form of q.

<a name="rational::num_den"></a>`rational::num_den q`
:   given a rational or integer q, returns a pair (n, d) containing the
    (signed) simplest numerator n and the (positive) simplest denominator d.
    This is the inverse (up to equivalence) of rational as defined on integer
    pairs (see [Constructors](#constructors)).

<!-- -->
**Example 3** Using num\_den to obtain a representation in simplest form:

    > let q = (44%(-14));
    > num q;
    -22L
    > den q;
    7L
    > num_den q;
    -22L,7L
    > num_den 3;
    3L,1L
    > num_den (-3);
    -3L,1L

Together, [`num`](#num) and [`den`](#den) are a pair of 'decomposition'
operators, and [`num_den`](#rational::num_den) is also a decomposition
operator. There are others (see [Decomposition](#decomposition)). The integer
and fraction function (see [Integer and Fraction
Parts](#integer-and-fraction-parts)) may be used in conjunction with
[`num_den_gauss`](#rational::num_den_gauss) to decompose a rational into
integer, numerator and denominator parts.

### Type and Value Tests

The functions [`rationalp`](#rationalp) and [`ratvalp`](#ratvalp) and other
rational variants are new for rationals and the standard functions
[`exactp`](#exactp) and [`inexactp`](#inexactp) are extended for rationals.

A value is 'exact', or of an exact type, if it is of a type that is able to
represent the values returned by arithmetical operations exactly; in a sense,
it is 'closed' under arithmetical operations. Otherwise, a value is 'inexact'.
Inexact types are able to store some values only approximately.

The doubles are not an exact type. The results of some operations on some
values that are stored exactly, can’t be stored exactly. (Furthermore, doubles
are intended to represent real numbers; no irrational number (∈ **R** \\
**Q**) can be stored exactly as a double; even some rational (∈ **Q**) numbers
are not stored exactly.)

The rationals are an exact type. All rational numbers (subject to available
resources, of course) are stored exactly. The results of the arithmetical
operations on rationals are rationals represented exactly. Beware that the
standard `intvalp` and [`ratvalp`](#ratvalp) may return 1 even if the value is
of double type. However, these functions may be combined with
[`exactp`](#exactp).

<a name="exactp/rational"></a>`exactp x`
:   returns whether x has an exact value.

<a name="inexactp/rational"></a>`inexactp x`
:   returns whether x has an inexact value.

<a name="rationalp/rational"></a>`rationalp x`
:   returns whether x is of rational type.

<a name="ratvalp/rational"></a>`ratvalp x`
:   returns whether x has a rational value.

<!-- -->
**Example 4** Rational value tests:

    > let l = [9, 9%1, 9%2, 4.5, sqrt 2, 1+i, inf, nan];
    > map exactp l;
    [1,1,1,0,0,1,0,0]
    > map inexactp l;
    [0,0,0,1,1,0,1,1]
    > map rationalp l;
    [0,1,1,0,0,0,0,0]
    > map ratvalp l;
    [1,1,1,1,1,0,0,0]
    > map (\x -> (exactp x && ratvalp x)) l; // "has exact rational value"
    [1,1,1,0,0,0,0,0]
    > map intvalp l; // for comparison
    [1,1,0,0,0,0,0,0]
    > map (\x -> (exactp x && intvalp x)) l; // "has exact integer value"
    [1,1,0,0,0,0,0,0]

See [Rational Complex Numbers](#rational-complex-numbers) for details about
rational complex numbers, and [Rational Complex Type and Value
Tests](#rational-complex-type-and-value-tests) for details of their type and
value tests.

Arithmetic
----------

### Operators

The standard arithmetic operators (+), (−) and (\*) are overloaded to have at
least one rational operand. If both operands are rational then the result is
rational. If one operand is integer, then the result is rational. If one
operand is double, then the result is double.

The operators (/) and (%) are overloaded for division on at least one rational
operand. The value returned by (/) is always `inexact` (in the sense of [Type
and Value Tests](#type-and-value-tests)). The value returned by (%) is `exact`
(if it exists).

The standard function `pow` is overloaded to have a rational left operand. If
`pow` is passed integer operands where the right operand is negative, then a
rational is returned. The right operand should be an integer; negative values
are permitted (because q^−z^ = 1/q^z^). It is not overloaded to also have a
rational right operand because such values are not generally rational (e.g.
q^1/n^ = ^n^√q).

The standard arithmetic operator (\^) is also overloaded, but produces a
double value (as always).

**Example 5** Arithmetic:

    > 5%7 + 2%3;
    29L%21L
    > str_mixed ans;
    "1L+8L/21L"
    > 1 + 2%3;
    5L%3L
    > ans + 1.0;
    2.66666666666667
    > 3%8 - 1%3;
    1L%24L
    > (11%10) ^ 3;
    1.331
    > pow (11%10) 3;
    1331L%1000L
    > pow 3 5;
    243L
    > pow 3 (-5);
    1L%243L

(See the function [`str_mixed`](#rational::str_mixed).)

Beware that (/) on integers will not produce a rational result.

**Example 6** Division:

    > 44/14;
    3.14285714285714
    > 44%14;
    22L%7L
    > str_mixed ans;
    "3L+1L/7L"

(See the function [`str_mixed`](#rational::str_mixed).)

### More on Division

There is a rational-aware divide operator on the numeric types:

<a name="%/rational"></a>`n1 % n2`
:   returns the quotient (∈ **Q**) of n1 and n2. If n1 and n2 are rational or
    integer then the result is rational. This operator has the precedence of
    division (/).

<!-- -->
**Example 7** Using % like a constructor:

    > 44 % 14;
    22L%7L
    > 2 + 3%8; // "2 3/8"
    19L%8L
    > str_mixed ans;
    "2L+3L/8L"

(See the function [`str_mixed`](#rational::str_mixed).)

<a name="rational::reciprocal"></a>`rational::reciprocal n`
:   returns the reciprocal of n: 1/n.

<!-- -->
**Example 8** Reciprocal:

    > reciprocal (22%7);
    7L%22L

The following division functions are parameterised by a rounding mode
`roundfun`. The available rounding modes are described in [Rounding to
Integer](#rounding-to-integer).

<a name="rational::divide"></a>`rational::divide roundfun n d`
:   for rationals n and d returns a pair (q, r) of 'quotient' and 'remainder'
    where q is an integer and r is a rational such that |r| &lt; |d| (or
    better) and n = q \* d + r. Further conditions may hold, depending on the
    chosen rounding mode `roundfun` (see [Rounding to
    Integer](#rounding-to-integer)). If `roundfun` = [`floor`](#floor) then 0
    ≤ r &lt; d. If `roundfun` = [`ceil`](#ceil) then −d &lt; r ≤ 0. If
    `roundfun` = [`trunc`](#trunc) then |r| &lt; |d| and sgn r ∈ {0, sgn d}.
    If `roundfun` = [`round`](#round), `roundfun` =
    [`round_zero_bias`](#round_zero_bias) or `roundfun` =
    [`round_unbiased`](#rational::round_unbiased) then |r| ≤ d/2.

<a name="rational::quotient"></a>`rational::quotient roundfun nN d`
:   returns just the quotient as produced by [`divide`](#rational::divide)
    roundfun n d.

<a name="rational::modulus"></a>`rational::modulus roundfun n d`
:   returns just the remainder as produced by [`divide`](#rational::divide)
    roundfun n d.

<a name="div/rational"></a>`q1 div q2`
:   (overload of the built-in div) q1 and q2 may be rational or integer.
    Returns an integer.

<a name="mod/rational"></a>`q1 mod q2`
:   (overload of the built-in mod) q1 and q2 may be rational or integer.
    Returns a rational. If q = q1 div q2 and r = q1 mod q2 then q1 = q \* q2 +
    q, q ∈ **Z**, |r| &lt; |q2| and sgn r ∈ {0, sgn q2}.

<!-- -->
### Relations — Equality and Inequality Tests

The standard arithmetic operators (==), (\~=), (&lt;), (&lt;=), (&gt;),
(&gt;=) are overloaded to have at least one rational operand. The other
operand may be rational, integer or double.

**Example 9** Inequality:

    > 3%8 < 1%3;
    0

### Comparison Function

<a name="rational::cmp"></a>`rational::cmp n1 n2`
:   is the 'comparison' (or 'compare') function, and returns sgn (n1 − n2);
    that is, it returns −1 if n1 &lt; n2, 0 if n1 = n2, and +1 if n1 &gt; n2.

<!-- -->
**Example 10** Compare:

    > cmp (3%8) (1%3);
    1

Mathematical Functions
----------------------

Most mathematical functions, including the elementary functions (sin, sin
^−1^, sinh, sinh^−1^, cos, ... , exp, ln, ... ), are not closed on the set of
rational numbers. That is, most mathematical functions do not yield a rational
number in general when applied to a rational number. Therefore the elementary
functions are not defined for rationals. To apply these functions, first apply
a cast to double, or compose the function with a cast.

### Absolute Value and Sign

The standard `abs` and `sgn` functions are overloaded for rationals.

<a name="abs/rational"></a>`abs q`
:   returns absolute value, or magnitude, |q| of q; abs q = |q| = q × sgn q
    (see below).

<a name="sgn/rational"></a>`sgn q`
:   returns the sign of q as an integer; returns −1 if q &lt; 0, 0 if q = 0,
    +1 if q &gt; 0.

<!-- -->
Together, these functions satisfy the property ∀q • (sgn q) \* (abs q) = q
(i.e. ∀q • (sgn q) \* |q| = q). Thus these provide a pair of 'decomposition'
operators; there are others (see [Decomposition](#decomposition)).

### Greatest Common Divisor (GCD) and Least Common Multiple (LCM)

The standard functions `gcd` and `lcm` are overloaded for rationals, and
mixtures of integer and rational.

<a name="gcd/rational"></a>`gcd n1 n2`
:   The GCD is also known as the Highest Common Factor (HCF). The GCD of
    rationals q1 and q2 is the largest (therefore positive) rational f such
    that f divides into both q1 and q2 exactly, i.e. an integral number of
    times. This is not defined for n1 and n2 both zero. For integral q1 and
    q2, this definition coincides with the usual definition of GCD for
    integers.

<!-- -->
**Example 11** With two rationals:

    > let a = 7%12;
    > let b = 21%32;
    > let f = gcd a b;
    > f;
    7L%96L
    > a % f;
    8L%1L
    > b % f;
    9L%1L

**Example 12** With a rational and an integer:

    > let f = gcd (6%5) 4;
    > f;
    2L%5L
    > (6%5) % f;
    3L%1L
    > 4 % f;
    10L%1L

**Example 13** With integral rationals and with integers:

    > gcd (rational 18) (rational 24);
    6L%1L
    > gcd 18 24;
    6

**Example 14** The behaviour with negative numbers:

    > gcd (rational (-18)) (rational 24);
    6L%1L
    > gcd (rational 18) (rational (-24));
    6L%1L
    > gcd (rational (-18)) (rational (-24));
    6L%1L

<a name="lcm/rational"></a>`lcm n1 n2`
:   The LCM of rationals q1 and q2 is the smallest positive rational m such
    that both q1 and q2 divide m exactly. This is not defined for n1 and n2
    both zero. For integral q1 and q2, this definition coincides with the
    usual definition of LCM for integers.

<!-- -->
**Example 15** With two rationals:

    > let a = 7%12;
    > let bB = 21%32;
    > let m = lcm a b;
    > m;
    21L%4L
    > m % a;
    9L%1L
    > m % b;
    8L%1L

**Example 16** With a rational and an integer:

    > let m = lcm (6%5) 4;
    > m;
    12L%1L
    > m % (6%5);
    10L%1L

**Example 17** The behaviour with negative numbers:

    > lcm (rational (-18)) (rational 24);
    72L%1L
    > lcm (rational 18) (rational (-24));
    72L%1L
    > lcm (rational (-18)) (rational (-24));
    72L%1L

Together, the GCD and LCM have the following property when applied to two
numbers: (gcd q1 q2) \* (lcm q1 q2) = |q1 \* q2|.

### Extrema (Minima and Maxima)

The standard `min` and `max` functions work with rational values.

**Example 18** Maximum:

    > max (3%8) (1%3);
    3L%8L

Special Rational Functions
--------------------------

### Complexity

The 'complexity' (or 'complicatedness') of a rational is a measure of the
greatness of its simplest (positive) denominator.

The complexity of a number is not itself made available, but various functions
and operators are provided to allow complexities to be compared. Generally, it
does not make sense to operate directly on complexity values.

The complexity functions in this section may be applied to integers (the least
complex), rationals, or reals (doubles; the most complex).

Functions concerning 'complexity' are named with 'cplx', whereas functions
concerning 'complex numbers' (see [Rational Complex
Numbers](#rational-complex-numbers)) are named with 'comp'.

#### Complexity Relations

<a name="rational::eq_cplx"></a>`n1 rational::eq_cplx n2`
:   "\[is\] equally complex \[to\]" — returns 1 if n1 and n2 are equally
    complex; returns 0 otherwise. Equal complexity is not the same a equality;
    n1 and n2 are equally complex if their simplest denominators are equal.
    Equal complexity forms an equivalence relation on rationals.

<!-- -->
**Example 19** Complexity equality test:

    > (1%3) eq_cplx (100%3);
    1
    > (1%4) eq_cplx (1%5);
    0
    > (3%3) eq_cplx (1%3); // LHS is not in simplest form
    0

<a name="rational::not_eq_cplx"></a>`n1 rational::not_eq_cplx n2`
:   "not equally complex" — returns 0 if n1 and n2 are equally complex;
    returns 1 otherwise.

<a name="rational::less_cplx"></a>`n1 rational::less_cplx n2`
:   "\[is\] less complex \[than\]" (or "simpler") — returns 1 if n1 is
    strictly less complex than n2; returns 0 otherwise. This forms a partial
    strict ordering on rationals.

<!-- -->
**Example 20** Complexity inequality test:

    > (1%3) less_cplx (100%3);
    0
    > (1%4) less_cplx (1%5);
    1
    > (3%3) less_cplx (1%3); // LHS is not in simplest form
    1

<a name="rational::less_eq_cplx"></a>`n1 rational::less_eq_cplx n2`
:   "less or equally complex" (or "not more complex") — returns 1 if n1 is
    less complex than or equally complex to n2; returns 0 otherwise. This
    forms a partial non-strict ordering on rationals.

<a name="rational::more_cplx"></a>`n1 rational::more_cplx n2`
:   "\[is\] more complex \[than\]" — returns 1 if n1 is strictly more complex
    than n2; returns 0 otherwise. This forms a partial strict ordering on
    rationals.

<a name="rational::more_eq_cplx"></a>`n1 rational::more_eq_cplx n2`
:   "more or equally complex" (or "not less complex") — returns 1 if n1 is
    more complex than or equally complex to n2; returns 0 otherwise. This
    forms a partial non-strict ordering on rationals.

<!-- -->
#### Complexity Comparison Function

<a name="rational::cmp_complexity"></a>`rational::cmp_complexity n1 n2`
:   is the 'complexity comparison' function, and returns the sign of the
    difference in complexity; that is, it returns −1 if n1 is less complex
    than n2, 0 if n1 and n2 are equally complex (but not necessarily equal),
    and +1 if n1 is more complex than n2.

<!-- -->
**Example 21** Complexity comparison:

    > cmp_complexity (1%3) (100%3);
    0
    > cmp_complexity (1%4) (1%5);
    -1
    > cmp_complexity (3%3) (1%3); // LHS is not in simplest form
    -1

#### Complexity Extrema

<a name="rational::least_cplx"></a>`rational::least_cplx n1 n2`
:   returns the least complex of n1 and n2; if they’re equally complex, n1 is
    returned.

<!-- -->
**Example 22** Complexity selection:

    > least_cplx (100%3) (1%3);
    100L%3L
    > least_cplx (1%5) (1%4);
    1L%4L
    > least_cplx (1%3) (3%3); // second argument not in simplest form
    1L%1L

<a name="rational::most_cplx"></a>`rational::most_cplx n1 n2`
:   returns the most complex of n1 and n2; if they’re equally complex, n1 is
    returned.

<!-- -->
#### Other Complexity Functions

<a name="rational::complexity_rel"></a>`rational::complexity_rel n1 op n2`
:   returns "complexity-of n1" compared by operator op to the “complexity-of
    n2”. This is equivalent to prefix complexity rel op n1 n2 (below), but is
    the more readable form.

<!-- -->
**Example 23** Complexity relations:

    > complexity_rel (1%3) (==) (100%3);
    1
    > complexity_rel (1%4) (<=) (1%5);
    1
    > complexity_rel (1%4) (>) (1%5);
    0

<a name="rational::prefix_complexity_rel"></a>`rational::prefix_complexity_rel op n1 n2`
:   returns the same as complexity\_rel n1 op n2, but this form is more
    convenient for currying.

<!-- -->
### Mediants and Farey Sequences

<a name="rational::mediant"></a>`rational::mediant q1 q2`

:   returns the canonical mediant of the rationals q1 and q2, a form of
    (nonarithmetic) average on rationals. The mediant of the representations
    n1/d1 = q1 and n2/d2 = q2, where d1 and d2 must be positive, is defined as
    (n1 + n2)/(d1 + d2). A mediant of the rationals q1 and q2 is a mediant of
    some representation of each of q1 and q2. That is, the mediant is
    dependent upon the representations and therefore is not well-defined as a
    function on pairs of rationals. The canonical mediant always assumes the
    simplest representation, and therefore is well-defined as a function on
    pairs of rationals.

    By the phrase “the mediant” (as opposed to just “a mediant”) we always
    mean “the canonical mediant”.

    If q1 &lt; q2, then any mediant q is always such that q1 &lt; q &lt; q2.

    The (canonical) mediant has some special properties. If q1 and q2 are
    integers, then the mediant is the arithmetic mean. If q1 and q2 are unit
    fractions (reciprocals of integers), then the mediant is the harmonic
    mean. The mediant of q and 1/q is ±1, (which happens to be a geometric
    mean with the correct sign, although this is a somewhat uninteresting and
    degenerate case).

<!-- -->
**Example 24** Mediants:

    > mediant (1%4) (3%10);
    2L%7L
    > mediant 3 7; // both integers
    5L%1L
    > mediant 3 8; // both integers again
    11L%2L
    > mediant (1%3) (1%7); // both unit fractions
    1L%5L
    > mediant (1%3) (1%8); // both unit fractions again
    2L%11L
    > mediant (-10) (-1%10);
    (-1L)%1L

<a name="rational::farey"></a>`rational::farey k`
:   for an integer k, farey returns the ordered list containing the order-k
    Farey sequence, which is the ordered list of all rational numbers between
    0 and 1 inclusive with (simplest) denominator at most k.

<!-- -->
**Example 25** A Farey sequence:

    > map str_mixed (farey 6);
    ["0L","1L/6L","1L/5L","1L/4L","1L/3L","2L/5L","1L/2L","3L/5L","2L/3L",
    "3L/4L","4L/5L","5L/6L","1L"]

(See the function [`str_mixed`](#rational::str_mixed).)

Farey sequences and mediants are closely related. Three rationals q1 &lt; q2
&lt; q3 are consecutive members of a Farey sequence if and only if q2 is the
mediant of q1 and q3. If rationals q1 = n1/d1 &lt; q2 = n2/d2 are consecutive
members of a Farey sequence, then n2d1 − n1d2 = 1.

### Rational Type Simplification

<a name="rational::rat_simplify"></a>`rational::rat_simplify q`
:   returns q with rationals simplified to integers, if possible.

<!-- -->
**Example 26** Rational type simplification:

    > let l = [9, 9%1, 9%2, 4.5, 9%1+i, 9%2+i]; l;
    [9,9L%1L,9L%2L,4.5,9L%1L+:1,9L%2L+:1]
    > map rat_simplify l;
    [9,9,9L%2L,4.5,9+:1,9L%2L+:1]

See [Rational Complex Numbers](#rational-complex-numbers) for details about
rational complex numbers, and [Rational Complex Type
Simplification](#rational-complex-type-simplification) for details of their
type simplification.

**Q** -&gt; **Z** — Rounding
----------------------------

### Rounding to Integer

Some of these are new functions, and some are overloads of standard functions.
The behaviour of the overloads is consistent with that of the standard
functions.

<a name="floor/rational"></a>`floor q`
:   (overload of standard function) returns q rounded downwards, i.e. towards
    −1, to an integer, usually denoted bQc.

<a name="ceil/rational"></a>`ceil q`
:   (overload of standard function) returns q rounded upwards, i.e. towards
    +1, to an integer, usually denoted dQe.

<a name="trunc/rational"></a>`trunc q`
:   (overload of standard function) returns q truncated, i.e. rounded towards
    0, to an integer.

<a name="round/rational"></a>`round q`
:   (overload of standard function) returns q 'rounded off', i.e. rounded to
    the nearest integer, with ‘half-integers’ (values that are an integer plus
    a half) rounded away from zero.

<a name="rational::round_zero_bias"></a>`rational::round_zero_bias q`
:   (new function) returns q 'rounded off', i.e. rounded to the nearest
    integer, but with ‘half-integers’ rounded towards zero.

<a name="rational::round_unbiased"></a>`rational::round_unbiased q`
:   (new function) returns q rounded to the nearest integer, with
    ‘half-integers’ rounded to the nearest even integer.

<!-- -->
**Example 27** Illustration of the different rounding modes:

    > let l = iterwhile (<= 3) (+(1%2)) (- rational 3);
    > map double l; // (just to show the values in a familiar format)
    [-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0]
    > map floor l;
    [-3L,-3L,-2L,-2L,-1L,-1L,0L,0L,1L,1L,2L,2L,3L]
    > map ceil l;
    [-3L,-2L,-2L,-1L,-1L,0L,0L,1L,1L,2L,2L,3L,3L]
    > map trunc l;
    [-3L,-2L,-2L,-1L,-1L,0L,0L,0L,1L,1L,2L,2L,3L]
    > map round l;
    [-3L,-3L,-2L,-2L,-1L,-1L,0L,1L,1L,2L,2L,3L,3L]
    > map round_zero_bias l;
    [-3L,-2L,-2L,-1L,-1L,0L,0L,0L,1L,1L,2L,2L,3L]
    > map round_unbiased l;
    [-3L,-2L,-2L,-2L,-1L,0L,0L,0L,1L,2L,2L,2L,3L]

(See the function [`double`](#double).)

### Integer and Fraction Parts

<a name="rational::integer_and_fraction"></a>`rational::integer_and_fraction roundfun q`
:   returns a pair (z, f) where z is the 'integer part' as an integer, f is
    the 'fraction part' as a rational, where the rounding operations are
    performed using rounding mode `roundfun` (see [Rounding to
    Integer](#rounding-to-integer)).

<!-- -->
**Example 28** Integer and fraction parts with the different rounding modes:

    > let nc = -22%7;
    > integer_and_fraction floor nc;
    -4L,6L%7L
    > integer_and_fraction trunc nc;
    -3L,(-1L)%7L
    > integer_and_fraction round nc;
    -3L,(-1L)%7L

It is always the case that z and f have the property that q = z + f. However,
the remaining properties depend upon the choice of `roundfun`. Thus this
provides a 'decomposition' operator; there are others (see
[Decomposition](#decomposition)). If `roundfun` = [`floor`](#floor) then 0 ≤ f
&lt; 1. If `roundfun` = [`ceil`](#ceil) then −1 &lt; f ≤ 0. If `roundfun` =
[`trunc`](#trunc) then |f| &lt; 1 and sgn f ∈ {0, sgn q}. If `roundfun` =
[`round`](#round), `roundfun` =
[`round_zero_bias`](#rational::round_zero_bias) or `roundfun` =
[`round_unbiased`](#rational::round_unbiased) then |f| ≤ 1/2.

<a name="rational::fraction"></a>`rational::fraction roundfun q`
:   returns just the 'fraction part' as a rational, where the rounding
    operations are performed using `roundfun`. The corresponding function
    'integer' is not provided, as integer `roundfun` q would be just
    `roundfun` q. The integer and fraction function (probably with
    [`trunc`](#trunc) or [`floor`](#floor) rounding mode) may be used in
    conjunction with [`num_den`](#rational::num_den) (see
    ['Deconstructors'](#deconstructors)) to decompose a rational into integer,
    numerator and denominator parts.

<a name="int/rational"></a>`int q`
:   overloads the built-in int and returns the 'integer part' of q consistent
    with the built-in.

<a name="frac/rational"></a>`frac q`
:   overloads the built-in frac and returns the 'fraction part' of q
    consistent with the built-in.

<!-- -->
**Example 29** Standard integer and fraction parts:

    > let nc = -22%7;
    > int nc;
    -3
    > frac nc;
    (-1L)%7L

Rounding to Multiples
---------------------

<a name="rational::round_to_multiple"></a>`rational::round_to_multiple roundfun multOf q`
:   returns q rounded to an integer multiple of a non-zero value multOf, using
    `roundfun` as the rounding mode (see [Rounding to
    Integer](#rounding-to-integer)). Note that it is the multiple that is
    rounded in the prescribed way, and not the final result, which may make a
    difference in the case that multOf is negative. If that is not the desired
    behaviour, pass this function the absolute value of multOf rather than
    multOf. Similar comments apply to the following functions.

<a name="rational::floor_multiple"></a>`rational::floor_multiple multOf q`
:   returns q rounded to a downwards integer multiple of multOf.

<a name="rational::ceil_multiple"></a>`rational::ceil_multiple multOf q`
:   returns q rounded to an upwards integer multiple of multOf.

<a name="rational::trunc_multiple"></a>`rational::trunc_multiple multOf q`
:   returns q rounded towards zero to an integer multiple of multOf.

<a name="rational::round_multiple"></a>`rational::round_multiple multOf q`
:   returns q rounded towards the nearest integer multiple of multOf, with
    half-integer multiples rounded away from 0.

<a name="rational::round_multiple_zero_bias"></a>`rational::round_multiple_zero_bias multOf q`
:   returns q rounded towards the nearest integer multiple of multOf, with
    half-integer multiples rounded towards 0.

<a name="rational::round_multiple_unbiased"></a>`rational::round_multiple_unbiased multOf q`
:   returns q rounded towards the nearest integer multiple of multOf, with
    half-integer multiples rounded to an even multiple.

<!-- -->
**Example 30** Round to multiple:

    > let l = [34.9, 35, 35%1, 35.0, 35.1];
    > map double l; // (just to show the values in a familiar format)
    [34.9,35.0,35.0,35.0,35.1]
    > map (floor_multiple 10) l;
    [30.0,30L,30L,30.0,30.0]
    > map (ceil_multiple 10) l;
    [40.0,40L,40L,40.0,40.0]
    > map (trunc_multiple 10) l;
    [30.0,30L,30L,30.0,30.0]
    > map (round_multiple 10) l;
    [30.0,40L,40L,40.0,40.0]
    > map (round_multiple_zero_bias 10) l;
    [30.0,30L,30L,30.0,40.0]
    > map (round_multiple_unbiased 10) l;
    [30.0,40L,40L,40.0,40.0]

(See the function [`double`](#double).)

The round multiple functions may be used to find a fixed denominator
approximation of a number. (The simplest denominator may actually be a proper
factor of the chosen value.) To approximate for a bounded (rather than
particular) denominator, use rational approx max den instead (see [Best
Approximation with Bounded
Denominator](#best-approximation-with-bounded-denominator)).

**Example 31** Finding the nearest q = n/d value to 1/e ≈ 0.368 where d = 1000
(actually, where d|1000):

    > let co_E = exp (-1);
    co_E;
    0.367879441171442
    > round_multiple (1%1000) (rational co_E);
    46L%125L
    > 1000 * ans;
    368L%1L

**Example 32** Finding the nearest q = n/d value to 1/φ ≈ 0.618 where d = 3
^5^ = 243 (actually, where d|243):

    > let co_Phi = (sqrt 5 - 1) / 2;
    > round_multiple (1%243) (rational co_Phi);
    50L%81L

Other methods for obtaining a rational approximation of a number are described
in [R -&gt; Q — Approximation](#r--gt-q-approximation).

**Q** -&gt; **R** — Conversion / Casting
----------------------------------------

<a name="double/rational"></a>`double q`
:   (overload of built-in) returns a double having a value as close as
    possible to q. (Overflow, underflow and loss of accuracy are potential
    problems. rationals that are too absolutely large or too absolutely small
    may overflow or underflow; some rationals can not be represented exactly
    as a double.)

<!-- -->
**R** -&gt; **Q** — Approximation
---------------------------------

This section describes functions that approximate a number (usually a double)
by a rational. See [Rounding to Multiples](#rounding-to-multiples) for
approximation of a number by a rational with a fixed denominator. See [Numeral
String -&gt; Q — Approximation](#numeral-string--gt-q-approximation) for
approximation by a rational of a string representation of a real number.

### Intervals

<a name="module-rat_interval"></a>

Some of the approximation functions return an *interval*. The file
`rat_interval.pure` is a basic implementation of interval arithmetic, and is
not included in the default prelude. It is not intended to provide a complete
implementation of interval arithmetic. The notions of 'open' and 'closed'
intervals are not distinguished. Infinite and half-infinite intervals are not
specifically provided. Some operations and functions may be missing. The most
likely functions to be used are simply the 'deconstructors'; see [Interval
Constructors and 'Deconstructors'](#interval-constructors-and-deconstructors).

#### Interval Constructors and 'Deconstructors'

Intervals are constructed with the function interval.

<a name="interval::interval"></a>`interval::interval (n1, n2)`
:   given a pair of numbers (z1 &lt;= z2), this returns the interval z1..z2.
    This is the inverse of `lo_up`.

<!-- -->
**Example 33** Constructing an interval:

    > let v = interval (3, 8);
    > v;
    interval::Ivl 3 8

<a name="interval::lower"></a>`interval::lower v`
:   returns the infimum (roughly, minimum) of v.

<a name="interval::upper"></a>`interval::upper v`
:   returns the supremum (roughly, maximum) of v.

<a name="interval::lo_up"></a>`interval::lo_up v`
:   returns a pair (l, u) containing the lower l and upper u extrema of the
    interval v. This is the inverse of interval as defined on number pairs.

<!-- -->
**Example 34** Deconstructing an interval:

    > lower v;
    3
    > upper v;
    8
    > lo_up v;
    3,8

#### Interval Type Tests

<a name="exactp/rational"></a>`exactp v`
:   returns whether an interval v has exact extrema.

<a name="inexactp/rational"></a>`inexactp v`
:   returns whether an interval v has an inexact extremum.

<a name="interval::intervalp"></a>`interval::intervalp x`
:   returns whether x is of type interval.

<a name="interval::interval_valp"></a>`interval::interval_valp x`
:   returns whether x has an interval value.

<a name="interval::ratinterval_valp"></a>`interval::ratinterval_valp x`
:   returns whether x has an interval value with rational extrema.

<a name="interval::intinterval_valp"></a>`interval::intinterval_valp x`
:   returns whether x has an interval value with integral extrema.

<!-- -->
**Example 35** Interval value tests:

    > let l = [interval(0,1), interval(0,1%1), interval(0,3%2), interval(0,1.5)];
    > map exactp l;
    [1,1,1,0]
    > map inexactp l;
    [0,0,0,1]
    > map intervalp l;
    [1,1,1,1]
    > map interval_valp l;
    [1,1,1,1]
    > map ratinterval_valp l;
    [1,1,1,1]
    > map intinterval_valp l;
    [1,1,0,0]

#### Interval Arithmetic Operators and Relations

The standard arithmetic operators (+), (−), (\*), (/) and (%) are overloaded
for intervals. The divide operators (/) and (%) do not produce a result if the
right operand is an interval containing 0.

<a name="example-36"></a>

**Example 36** Some intervals:

    > let a = interval (11, 19);
    > let b = interval (16, 24);
    > let c = interval (21, 29);
    > let d = interval (23, 27);

**Example 37** Interval arithmetic:

    > let p = interval (0, 1);
    > let s = interval (-1, 1);
    > a + b;
    interval::Ivl 27 43
    > a - b;
    interval::Ivl (-13) 3
    > a * b;
    interval::Ivl 176 456
    > p * 2;
    interval::Ivl 0 2
    > (-2) * p;
    interval::Ivl (-2) 0
    > -c;
    interval::Ivl (-29) (-21)
    > s * a;
    interval::Ivl (-19) 19
    > a % 2;
    interval::Ivl (11L%2L) (19L%2L)
    > a / 2;
    interval::Ivl 5.5 9.5
    > reciprocal a;
    interval::Ivl (1L%19L) (1L%11L)
    > 2 % a;
    interval::Ivl (2L%19L) (2L%11L)
    > a % b;
    interval::Ivl (11L%24L) (19L%16L)
    > a % a; // notice that the intervals are mutually independent here
    interval::Ivl (11L%19L) (19L%11L)

There are also some relations defined for intervals. The standard relations
(==) and (\~=) are overloaded.

However, rather than overloading (&lt;), (&lt;=), (&gt;), (&gt;=), which could
be used for either ordering or containment with some ambiguity, the module
defines `(before)`, `(within)`, and so on. 'Strictness' refers to the
properties at the end-points.

<a name="interval::before"></a>`v1 interval::before v2`
:   returns whether v1 is entirely before v2.

<a name="interval::strictly_before"></a>`v1 interval::strictly_before v2`
:   returns whether v1 is strictly entirely before v2.

<a name="interval::after"></a>`v1 interval::after v2`
:   returns whether v1 is entirely after v2.

<a name="interval::strictly_after"></a>`v1 interval::strictly_after v2`
:   returns whether v1 is strictly entirely after v2.

<a name="interval::within"></a>`v1 interval::within v2`
:   returns whether v1 is entirely within v2; i.e. whether v1 is subinterval
    of v2.

<a name="interval::strictly_within"></a>`v1 interval::strictly_within v2`
:   returns whether v1 is strictly entirely within v2; i.e. whether v1 is
    proper subinterval of v2.

<a name="interval::without"></a>`v1 interval::without v2`
:   returns whether v1 entirely contains v2; i.e. whether v1 is superinterval
    of v2. 'Without' is used in the sense of outside or around.

<a name="interval::strictly_without"></a>`v1 interval::strictly_without v2`
:   returns whether v1 strictly entirely contains v2; i.e. whether v1 is
    proper superinterval of v2.

<a name="interval::disjoint"></a>`v1 interval::disjoint v2`
:   returns whether v1 and v2 are entirely disjoint.

<a name="interval::strictly_disjoint"></a>`v interval::strictly_disjoint v2`
:   returns whether v1 and v2 are entirely strictly disjoint.

<!-- -->
**Example 38** Interval relations:

    > a == b;
    0
    > a == a;
    1
    > a before b;
    0
    > a before c;
    1
    > c before a;
    0
    > a disjoint b;
    0
    > c disjoint a;
    1
    > a within b;
    0
    > a within c;
    0
    > d within c;
    1
    > c within d;
    0
    > a strictly_within a;
    0
    > a within a;
    1

(The symbols a through d were defined in [Example 36](#example-36).)

These may also be used with a simple (real) value, and in particular to test
membership.

**Example 39** Membership:

    > 10 within a;
    0
    > 11 within a;
    1
    > 11.0 within a;
    1
    > 12 within a;
    1
    > 12.0 within a;
    1
    > 10 strictly_within a;
    0
    > 11 strictly_within a;
    0
    > (11%1) strictly_within a;
    0
    > 12 strictly_within a;
    1
    > (12%1) strictly_within a;
    1

(The symbol a was defined in [Example 36](#example-36).)

#### Interval Maths

Some standard functions are overloaded for intervals; some new functions are
provided.

<a name="abs/rational"></a>`abs v`
:   returns the interval representing the range of (x) as x varies over v.

<!-- -->
**Example 40** Absolute interval:

    > abs (interval (1, 5));
    interval::Ivl 1 5
    > abs (interval (-1, 5));
    interval::Ivl 0 5
    > abs (interval (-5, -1));
    interval::Ivl 1 5

<a name="sgn/rational"></a>`sgn v`
:   returns the interval representing the range of sgn(x) as x varies over v.

<a name="#/rational"></a>`# v`
:   returns the length of an interval.

<!-- -->
**Example 41** Absolute interval:

    > #d;
    4

(The symbol d was defined in [Example 36](#example-36).)

### Least Complex Approximation within Epsilon

<a name="rational::rational_approx_epsilon"></a>`rational::rational_approx_epsilon ε r`
:   Find the least complex (see [Complexity Extrema](#complexity-extrema))
    rational approximation to r (usually a double) that is ε-close. That is
    find the q with the smallest possible denominator such that such that |q −
    r| ≤ ε. (ε &gt; 0.)

<!-- -->
**Example 42** Rational approximation to π ≈ 3.142 ≈ 22/7:

    > rational_approx_epsilon .01 pi;
    22L%7L
    > abs (ans - pi);
    0.00126448926734968

<a name="example-43"></a>

**Example 43** The golden ratio φ = (1 + √5) / 2 ≈ 1.618:

    > let phi = (1 + sqrt 5) / 2;
    > rational_approx_epsilon .001 phi;
    55L%34L
    > abs (ans - phi);
    0.000386929926365465

<a name="rational::rational_approxs_epsilon"></a>`rational::rational_approxs_epsilon ε r`
:   Produce a list of ever better rational approximations to r (usually a
    double) that is eventually ε-close. (ε &gt; 0.)

<!-- -->
**Example 44** Rational approximations to π:

    > rational_approxs_epsilon .0001 pi;
    [3L%1L,25L%8L,47L%15L,69L%22L,91L%29L,113L%36L,135L%43L,157L%50L,179L%57L,
    201L%64L,223L%71L,245L%78L,267L%85L,289L%92L,311L%99L,333L%106L]

**Example 45** Rational approximations to the golden ratio φ; these
approximations are always reverse consecutive Fibonacci numbers (from f1: 1,
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...):

    > rational_approxs_epsilon .0001 phi;
    [1L%1L,3L%2L,8L%5L,21L%13L,55L%34L,144L%89L]

(The symbol phi was defined in [Example 43](#example-43).)

<a name="rational::rational_interval_epsilon"></a>`rational::rational_interval_epsilon ε r`
:   Find the least complex (see [Complexity Extrema](#complexity-extrema))
    rational interval containing r (usually a double) that is ε-small. That is
    find the least complex (see [Complexity Extrema](#complexity-extrema)) q1
    ≤ q2 such that r ∈ \[q1, q2\] and q2 − q1 ≤ ε. (ε &gt; 0.)

<!-- -->
**Example 46** Rational interval surrounding π:

    > let i_Pi = rational_interval_epsilon .01 pi;
    > i_Pi;
    interval::Ivl (47L%15L) (22L%7L)
    > double (lower i_Pi); pi; double (upper i_Pi);
    3.13333333333333
    3.14159265358979
    3.14285714285714

(The functions lower and upper are described in [Interval Constructors and
'Deconstructors'](#interval-constructors-and-deconstructors).)

**Example 47** Rational interval surrounding the golden ratio φ:

    > rational_interval_epsilon .001 phi;
    interval::Ivl (55L%34L) (89L%55L)
    > #ans;
    1L%1870L

(The symbol phi was defined in [Example 43](#example-43). The function
[`#`](#rational::#/interval) is described in [Interval
Maths](#interval-maths).)

### Best Approximation with Bounded Denominator

<a name="rational::rational_approx_max_den"></a>`rational::rational_approx_max_den maxDen r`
:   Find the closest rational approximation to r (usually a double) that has a
    denominator no greater than maxDen. (maxDen &gt; 0).

<!-- -->
**Example 48** Rational approximation to π:

    > rational_approx_max_den 10 pi;
    22L%7L

**Example 49** Rational approximation to the golden ratio φ:

    > rational_approx_max_den 1000 phi;
    1597L%987L

(The symbol phi was defined in [Example 43](#example-43).)

<a name="rational::rational_approxs_max_den"></a>`rational::rational_approxs_max_den maxDen r`
:   Produce a list of ever better rational approximations to r (usually a
    double) while the denominator is bounded by maxDen (maxDen &gt; 0).

<!-- -->
**Example 50** Rational approximations to π:

    > rational_approxs_max_den 100 pi;
    [3L%1L,25L%8L,47L%15L,69L%22L,91L%29L,113L%36L,135L%43L,157L%50L,179L%57L,
    201L%64L,223L%71L,245L%78L,267L%85L,289L%92L,311L%99L]

**Example 51** Rational approximations to the golden ratio φ:

    > rational_approxs_max_den 100 phi;
    [1L%1L,3L%2L,8L%5L,21L%13L,55L%34L,144L%89L]

(The symbol phi was defined in [Example 43](#example-43).)

<a name="rational::rational_interval_max_den"></a>`rational::rational_interval_max_den maxDen r`
:   Find the smallest rational interval containing r (usually a double) that
    has endpoints with denominators no greater than maxDen (maxDen &gt; 0).

<!-- -->
**Example 52** Rational interval surrounding π:

    > let i_Pi = rational_interval_max_den 100 pi ; i_Pi;
    interval::Ivl (311L%99L) (22L%7L)
    > double (lower i_Pi); pi; double (upper i_Pi);
    3.14141414141414
    3.14159265358979
    3.14285714285714

**Example 53** Rational interval surrounding the golden ratio φ:

    > rational_interval_max_den 1000 phi;
    interval::Ivl (987L%610L) (1597L%987L)

(The symbol phi was defined in [Example 43](#example-43).)

To approximate for a particular (rather than bounded) denominator, use round
to multiple instead (see [Rounding to Multiples](#rounding-to-multiples)).

Decomposition
-------------

There is more than one way to 'decompose' a rational number into its
'components'. It might be split into an integer and a fraction part — see
[Integer and Fraction Parts](#integer-and-fraction-parts); or sign and
absolute value — see [Absolute Value and Sign](#absolute-value-and-sign); or
numerator and denominator — see ['Deconstructors'](#deconstructors).

Continued Fractions
-------------------

### Introduction

In "pure-rational", a continued fraction a~0~ + (1 / (a~1~ + (1 / (a~2~ + ···
+ 1 / a ~n~)))) where ∀i &gt; 0 • a~i~ ≠ 0, is represented by \[a~0~, a ~1~,
a~2~, ... , a~n~\].

A 'simple' continued fraction is one in which ∀i • a~i~ ∈ **Z** and ∀i &gt; 0
• a~i~ &gt; 0.

Simple continued fractions for rationals are not quite unique since \[a ~0~,
a~1~, ... , a~n~, 1\] = \[a~0~, a~1~, ... , a~n+1~\]. We will refer to these
as the 'non-standard' and 'standard' forms, respectively. The following
functions return the standard form.

### Generating Continued Fractions

#### Exact

<a name="rational::continued_fraction"></a>`rational::continued_fraction q`
:   Find 'the' (exact) continued fraction of a rational (including, trivially,
    integer) value q.

<!-- -->
**Example 54** The rational 1234/1001:

    > continued_fraction (1234%1001);
    [1L,4L,3L,2L,1L,1L,1L,8L]
    > evaluate_continued_fraction ans;
    1234L%1001L

#### Inexact

<a name="rational::continued_fraction_max_terms"></a>`rational::continued_fraction_max_terms n r`
:   Find up to n initial terms of continued fraction of the value r with the
    'remainder', if any, in the final element. (If
    continued\_fraction\_max\_terms n r returns a list of length n or less,
    then the result is exact.)

<!-- -->
**Example 55** First 5 terms of the continued fraction for the golden ratio φ:

    > continued_fraction_max_terms 5 phi;
      [1.0,1.0,1.0,1.0,1.0,1.61803398874989]
    > evaluate_continued_fraction ans;
    1.61803398874989

(The symbol phi was defined in [Example 43](#example-43).)

<a name="rational::continued_fraction_epsilon"></a>`rational::continued_fraction_epsilon ε r`
:   Find enough of the initial terms of a continued fraction to within ε of
    the value r with the 'remainder', if any, in the final element.

<!-- -->
**Example 56** First few terms of the value √2:

    > continued_fraction_epsilon .001 (sqrt 2);
    [1.0,2.0,2.0,2.0,2.0,2.41421356237241]
    > map double (convergents ans);
    [1.0,1.5,1.4,1.41666666666667,1.41379310344828,1.41421356237309]

### Evaluating Continued Fractions

<a name="rational::evaluate_continued_fraction"></a>`rational::evaluate_continued_fraction aa`
:   Fold a continued fraction aa into the value it represents. This function
    is not limited to simple continued fractions. (Exact simple continued
    fractions are folded into a rational.)

<!-- -->
**Example 57** The continued fraction \[1, 2, 3, 4\] and the non-standard form
\[4, 3, 2, 1\]:

    > evaluate_continued_fraction [1,2,3,4];
    43L%30L
    > continued_fraction ans;
    [1L,2L,3L,4L]
    > evaluate_continued_fraction [4,3,2,1];
    43L%10L
    > continued_fraction ans;
    [4L,3L,3L]

#### Convergents

<a name="rational::convergents"></a>`rational::convergents aa`
:   Calculate the convergents of the continued fraction aa. This function is
    not limited to simple continued fractions.

<!-- -->
**Example 58** Convergents of a continued fraction approximation of the value
√2:

    > continued_fraction_max_terms 5 (sqrt 2);
    [1.0,2.0,2.0,2.0,2.0,2.41421356237241]
    > convergents ans;
    [1.0,1.5,1.4,1.41666666666667,1.41379310344828,1.41421356237309]

Rational Complex Numbers
------------------------

Pure together with `rational.pure` provide various types of number, including
integers (**Z**), doubles (**R**, roughly), complex numbers (**C**) and
Gaussian integers (**Z**\[i\]), rationals (**Q**) and rational complex numbers
(**Q**\[i\]).

Functions concerning 'complex numbers' are named with 'comp', whereas
functions concerning 'complexity' (see [Complexity](#complexity)) are named
with 'cplx'.

### Rational Complex Constructors and 'Deconstructors'

Complex numbers can have rational parts.

**Example 59** Forming a rational complex:

    > 1 +: 1 * (1%2);
    1+:1L%2L
    > ans * ans;
    3L%4L+:1L%1L

And rational numbers can be given complex parts.

**Example 60** Complex rationals and complicated rationals:

    > (1 +: 2) % (3 +: 4);
    11L%25L+:2L%25L
    > ans * (3 +: 4);
    1L%1L+:2L%1L
    > ((4%1) * (0 +: 1)) % 2;
    0L%1L+:2L%1L
    > ((4%1) * (0 +: 1)) % (1%2);
    0L%1L+:8L%1L
    > ((4%1) * (0 +: 1)) % (1 + (1%2) * (0 +: 1));
    8L%5L+:16L%5L
    > ans * (1+(1%2) * (0 +: 1));
    0L%1L+:4L%1L
    > ((4%1) * (0 +: 1)) / (1 + (1%2) * (0 +: 1));
    1.6+:3.2

The various parts of a complex rational may be deconstructed using
combinations of num and den and the standard functions re and im.

Thus, taking real and imaginary parts first, a rational complex number may be
considered to be (x~n~ / x~d~) + (y~n~ / y~d~) \* i with x~n~, x~d~, y~n~,
y~d~ ∈ **Z**.

A rational complex number may also be decomposed into its 'numerator' and
'denominator', where these are both integral complex numbers, or 'Gaussian
integers', and the denominatoris a minimal choice in some sense.

One way to do this is so that the denominator is the minimum positive integer.
The denominator is a complex number with zero imaginary part.

Thus, taking numerator and denominator parts first, a rational complex number
may be considered to be (n~x~ + n~y~ \* i) / (d + 0 \* i) with n ~x~, n~y~, d
∈ **Z**.

Another way to do this is so that the denominator is a Gaussian integer with
minimal absolute value. Thus, taking numerator and denominator parts first, a
rational complex number may be considered to be (n~x~ + n~y~ \* i) / (d~x~ +
d~y~ \* i) with n~x~, n~y~, d~x~, d ~y~ ∈ **Z**.

The d~x~, d~y~ are not unique, but can be chosen such that d ~x~ &gt; 0 and
either |d~y~| &lt; d~x~ or d~y~ = d ~x~ &gt; 0.

<a name="rational::num_den_nat"></a>`rational::num_den_nat c`

:   given a complex rational or integer c, returns a pair (n, d) containing an
    integral complex (Gaussian integral) numerator n, and the smallest natural
    (i.e. positive integral real) complex denominator d, i.e. a complex number
    where ℜ(d) ∈ **Z**, ℜ(d) &gt; 0, ℑ(d) = 0; i.e. the numerator and
    denominator of one 'normalised' form of c.

    This is an inverse (up to equivalence) of rational as defined on integral
    complex pairs (see [Constructors](#constructors)).

<a name="rational::num_den_gauss"></a>`rational::num_den_gauss c`

:   given a complex rational or integer c, returns a pair (n, d) containing an
    integral complex (Gaussian integral) numerator n, and an absolutely
    smallest integral complex denominator d chosen s.t. ℜ(d),=(d) ∈ **Z**,
    ℜ(d) &gt; 0, and either |ℜ(d)| &lt; ℑ(d) or ℜ(d) = ℑ(d) &gt; 0; i.e. the
    numerator and denominator of another 'normalised' form of c.

    This is an inverse (up to equivalence) of rational as defined on integral
    complex pairs (see [Constructors](#constructors)).

<a name="rational::num_den"></a>`rational::num_den c`

:   synonymous with num\_den\_gauss.

    This is an inverse (up to equivalence) of rational as defined on integer
    pairs (see [Constructors](#constructors)).

<a name="num/rational"></a>`num c`
:   given a complex rational or integer c, returns just the numerator of the
    normalised form of c given by num\_den c.

<a name="den/rational"></a>`den c`
:   given a complex rational or integer c, returns just the denominator of the
    normalised form of c given by num\_den c.

<!-- -->
**Example 61** Rational complex number deconstruction:

    > let cq = (1+2*i)%(3+3*i); cq;
    1L%2L+:1L%6L
    > (re cq, im cq);
    1L%2L,1L%6L
    > (num . re) cq;
    1L
    > (den . re) cq;
    2L
    > (num . im) cq;
    1L
    > (den . im) cq;
    6L
    > let (n_nat,d_nat) = num_den_nat cq;
    > (n_nat, d_nat);
    3+:1,6+:0
    > n_nat % d_nat;
    1L%2L+:1L%6L
    > abs d_nat;
    6.0
    > let (n, d) = num_den_gauss cq; (n, d);
    1L+:2L,3L+:3L
    > let (n,d) = num_den cq; (n, d);
    1L+:2L,3L+:3L
    > n % d;
    1L%2L+:1L%6L
    > abs d;
    4.24264068711928
    > (re . num) cq;
    1L
    > (im . num) cq;
    2L
    > (re . den) cq; //always > 0
    3L
    > (im . den) cq; //always <= (re.den)
    3L

### Rational Complex Type and Value Tests

Beware that `intcompvalp` and `ratcompvapl` may return 1 even if the value is
of complex type with double parts. However, these functions may be combined
with `exactp`.

<a name="complexp/rational"></a>`complexp x`
:   standard function; returns whether x is of complex type.

<a name="compvalp/rational"></a>`compvalp x`
:   standard function; returns whether x has a complex value (∈ **C** =
    **R**\[i\]).

<a name="rational::ratcompvalp"></a>`rational::ratcompvalp x`
:   returns whether x has a rational complex value (∈ **Q**\[i\]).

<a name="rational::intcompvalp"></a>`rational::intcompvalp x`
:   returns whether x has an integral complex value (∈ **Z**\[i\]), i.e. a
    Gaussian integer value.

<!-- -->
**Example 62** Rational complex number value tests:

    > let l = [9, 9%1, 9%2, 4.5, sqrt 2, 1+:1, 1%2+:1, 0.5+:1, inf, nan];
    > map exactp l;
    [1,1,1,0,0,1,1,0,0,0]
    > map inexactp l;
    [0,0,0,1,1,0,0,1,1,1]
    > map complexp l;
    [0,0,0,0,0,1,1,1,0,0]
    > map compvalp l;
    [1,1,1,1,1,1,1,1,1,1]
    > map (\x -> (exactp x and compvalp x)) l; // "has exact complex value"
    [1,1,1,0,0,1,1,0,0,0]
    > map ratcompvalp l;
    [1,1,1,1,1,1,1,1,0,0]
    > map (\x -> (exactp x and ratcompvalp x)) l;
    [1,1,1,0,0,1,1,0,0,0]
    > map intcompvalp l;
    [1,1,0,0,0,1,0,0,0,0]
    > map (\x -> (exactp x and intcompvalp x)) l;
    [1,1,0,0,0,1,0,0,0,0]
    > map ratvalp l;
    [1,1,1,1,1,0,0,0,0,0]
    > map (\x -> (exactp x and ratvalp x)) l;
    [1,1,1,0,0,0,0,0,0,0]
    > map intvalp l; // for comparison
    [1,1,0,0,0,0,0,0,0,0]
    > map (\x -> (exactp x and intvalp x)) l;
    [1,1,0,0,0,0,0,0,0,0]

    See `Type and Value Tests`_ for some details of rational type and value tests.

### Rational Complex Arithmetic Operators and Relations

The standard arithmetic operators (+), (−), (\*), (/), (%), (), (==) and (\~=)
are overloaded to have at least one complex and/or rational operand, but
(&lt;), (&lt;=), (&gt;), (&gt;=) are not, as complex numbers are unordered.

**Example 63** Rational complex arithmetic:

    > let w = 1%2 +: 3%4;
    > let z = 5%6 +: 7%8;
    > w + z;
    4L%3L+:13L%8L
    > w % z;
    618L%841L+:108L%841L
    > w / z;
    0.734839476813318+:0.128418549346017
    > w ^ 2;
    -0.3125+:0.75
    > w == z;
    0
    > w == w;
    1

### Rational Complex Maths

The standard functions `re` and `im` work with rational complex numbers (see
[Rational Complex Constructors and
'Deconstructors'](#rational-complex-constructors-and-deconstructors)).

The standard functions `polar`, `abs` and `arg` work with rational complex
numbers, but the results are inexact.

**Example 64** Rational complex maths:

    > polar (1%2+:1%2);
    0.707106781186548<:0.785398163397448
    > abs (4%2+:3%2);
    2.5
    > arg (-1%1);
    3.14159265358979

There are some additional useful functions for calculating with rational
complex numbers and more general mathematical values.

<a name="rational::norm_gauss"></a>`rational::norm_gauss c`
:   returns the Gaussian norm ||c|| of any complex (or real) number c; this is
    the square of the absolute value, and is returned as an (exact) integer.

<a name="rational::div_mod_gauss"></a>`rational::div_mod_gauss n d`
:   performs Gaussian integer division, returning (q, r) where q is a (not
    always unique) quotient, and r is a (not always unique) remainder. q and r
    are such that n = q \* d + r and ||r|| &lt; ||d|| (equivalently, |r| &lt;
    |d|).

<a name="rational::n_div_gauss"></a>`rational::n_div_gauss d`
:   returns just a quotient from Gaussian integer division as produced by
    div\_mod\_gauss n d.

<a name="rational::n_mod_gauss"></a>`rational::n_mod_gauss d`
:   returns just a remainder from Gaussian integer division as produced by
    div\_mod\_gauss n d.

<a name="rational::gcd_gauss"></a>`rational::gcd_gauss c1 c2`
:   returns a GCD G of the Gaussian integers c1,c2. This is chosen so that
    s.t. ℜ(G) &gt; 0, and either |ℑ(G)| &lt; ℜ(G) or ℑ(G) = ℜ(G) &gt; 0;

<a name="rational::euclid_gcd"></a>`rational::euclid_gcd zerofun modfun x y`
:   returns a (non-unique) GCD calculated by performing the Euclidean
    algorithm on the values x and y (of any type) where zerofun is a predicate
    for equality to 0, and modfun is a binary modulus (remainder) function.

<a name="rational::euclid_alg"></a>`rational::euclid_alg zerofun divfun x y`
:   returns (g, a, b) where the g is a (non-unique) GCD and a, b are
    (arbitrary, non-unique) values such that a \* x + b \* y = g calculated by
    performing the generalised Euclidean algorithm on the values x and y (of
    any type) where zerofun is a predicate for equality to 0, and div is a
    binary quotient function.

<!-- -->
**Example 65** More rational complex and other maths:

    > norm_gauss (1 +: 3);
    10
    > abs (1 +: 3);
    3.16227766016838
    > norm_gauss (-5);
    25
    > let (q, r) = div_mod_gauss 100 (12 +: 5);
    > (q, r);
    7L+:-3L,1L+:1L
    > q * (12 +: 5) + r;
    100L+:0L
    > 100 div_gauss (12 +: 5);
    7L+:-3L
    > 100 mod_gauss (12 +: 5);
    1L+:1L
    > div_mod_gauss 23 5;
    5L+:0L,-2L+:0L
    > gcd_gauss (1 +: 2) (3 +: 4);
    1L+:0L
    > gcd_gauss 25 15;
    5L+:0L
    > euclid_gcd (==0) (mod_gauss) (1+: 2) (3 +: 4);
    1L+:0L
    > euclid_gcd (==0) (mod) 25 15;
    5
    > let (g, a, b) = euclid_alg (==0) (div_gauss) (1 +: 2) (3 +: 4); g;
    1L+:0L
    > (a, b);
    -2L+:0L,1L+:0L
    > a * (1 +: 2) + b * (3 +: 4);
    1L+:0L
    > let (g, a, b) = euclid_alg (==0) (div) 25 15; g;
    5
    > (a, b);
    -1,2
    > a * 25 + b * 15;
    5

### Rational Complex Type Simplification

<a name="rational::comp_simplify"></a>`rational::comp_simplify c`
:   returns q with complex numbers simplified to reals, if possible.

<a name="rational::ratcomp_simplify"></a>`rational::ratcomp_simplify c`
:   returns q with rationals simplified to integers, and complex numbers
    simplified to reals, if possible.

<!-- -->
**Example 66** Rational complex number type simplification:

    > let l = [9+:1, 9%1+:1, 9%2+:1, 4.5+:1, 9%1+:0, 9%2+:0, 4.5+:0.0];
    > l;
    [9+:1,9L%1L+:1,9L%2L+:1,4.5+:1,9L%1L+:0,9L%2L+:0,4.5+:0.0]
    > map comp_simplify l;
    [9+:1,9L%1L+:1,9L%2L+:1,4.5+:1,9L%1L,9L%2L,4.5+:0.0]
    > map ratcomp_simplify l;
    [9+:1,9+:1,9L%2L+:1,4.5+:1,9,9L%2L,4.5+:0.0]

    See `Rational Type Simplification`_ for some details of rational type
    simplification.

String Formatting and Evaluation
--------------------------------

### The Naming of the String Conversion Functions

There are several families of functions for converting between strings and
rationals.

The functions that convert from rationals to strings have names based on that
of the standard function `str`. The `str_*` functions convert to a formatted
string, and depend on a 'format structure' parameter (see
[Internationalisation and Format
Structures](#internationalisation-and-format-structures)). The `strs_*`
functions convert to a tuple of string fragments.

The functions that convert from strings to rationals have names based on that
of the standard function `eval` (`val` in Q). The `val_*` functions convert
from a formatted string, and depend on a format structure parameter. The
`sval_*` functions convert from a tuple of string fragments.

There are also `join_*` and `split_*` functions to join string fragments into
formatted strings, and to split formatted strings into string fragments,
respectively; these depend on a format structure parameter. These functions
are not always invertible, because some of the functions reduce an error term
to just a sign, e.g. [`str_real_approx_dp`](#rational::str_real_approx_dp) may
round a value. Thus sometimes the `join_*` and `split_*` pairs, and the
`str_*` and `val_*` pairs are not quite mutual inverses.

### Internationalisation and Format Structures

Many of the string formatting functions in the following sections are
parameterised by a 'format structure'. Throughout this document, the formal
parameter for the format structure will be `fmt`. This is simply a record
mapping some string 'codes' to functions as follows. The functions are mostly
from strings to a string, or from a string to a tuple of strings.

`"sm"`

:   a function mapping a sign and an unsigned mantissa (or integer) strings to
    a signed mantissa (or integer) string.

`"se"`

:   a function mapping a sign and an unsigned exponent string to a signed
    exponent string.

`"-s"`

:   a function mapping a signed number string to a pair containing a sign and
    the unsigned number string.

`"gi"`

:   a function mapping an integer representing the group size and an integer
    string to a grouped integer string.

`"gf"`

:   a function mapping an integer representing the group size and a
    fraction-part string to a grouped fraction-part string.

`"-g"`

:   a function mapping a grouped number string to an ungrouped number string.

`"zi"`

:   a function mapping an integer number string to a number string. The input
    string representing zero integer part is "", which should be mapped to the
    desired representation of zero. All other number strings should be
    returned unaltered.

`"zf"`

:   a function mapping a fraction-part number string to a number string. The
    input string representing zero fraction part is "", which should be mapped
    to the desired representation of zero. All other number strings should be
    returned unaltered.

`"ir"`

:   a function mapping initial and recurring parts of a fraction part to the
    desired format.

`"-ir"`

:   a function mapping a formatted fraction part to the component initial and
    recurring parts.

`"if"`

:   a function mapping an integer string and fraction part string to the
    radix-point formatted string.

`"-if"`

:   a function mapping a radix-point formatted string to the component integer
    fraction part strings

`"me"`

:   a function mapping a mantissa string and exponent string to the formatted
    exponential string.

`"-me"`

:   a function mapping a formatted exponential string to the component
    mantissa and exponent strings.

`"e"`

:   a function mapping an 'error' number (not string) and a number string to a
    formatted number string indicating the sign of the error.

`"-e"`

:   a function mapping a formatted number string indicating the sign of the
    error to the component 'error' string (not number) and number strings.

Depending upon the format structure, some parameters of some of the functions
taking a format structure may have no effect. For example, an `intGroup`
parameter specifying the size of the integer digit groups will have no effect
if the integer group separator is the empty string.

<a name="rational::create_format"></a>`rational::create_format options`
:   is a function that provides an easy way to prepare a 'format structure'
    from the simpler 'options structure'. The options structure is another
    record, but from more descriptive strings to a string or tuple of strings.

<!-- -->
For example, `format_uk` is generated from `options_uk` as follows:

    public options_uk;
    const options_uk =
      {
        "sign" => ("-","",""),             // alternative: ("-"," ","+")
        "exponent sign" => ("-","",""),    // alternative: ("-","","+")
        "group separator" => ",",          // might be " " or "." or "'" elsewhere
        "zero" => "0",
        "radix point" => ".",              // might be "," elsewhere
        "fraction group separator" => ",",
        "fraction zero" => "0",            // alternative: ""
        "recur brackets" => ("[","...]"),
        "exponent" => "*10^",              // (poor) alternative: "e"
        "error sign" => ("-","","+"),
        "error brackets" => ("(",")")
      };

    public format_uk;
    const format_uk = create_format options_uk;

The exponent string need not depend on the radix, as the numerals for the
number radix in that radix are always "10".

Beware of using "e" or "E" as an exponent string as these have the potential
of being treated as digits in, e.g., hexadecimal.

Format structures do not have to be generated via create format; they may also
be constructed directly.

### Digit Grouping

Some functions take `group` parameters. A value of 0 means "don’t group".

### Radices

The functions that produce a decimal expansion take a Radix argument. The
fraction parts are expanded in that radix (or 'base'), in addition to the
integer parts. The parameter Radix is not restricted to the usual {2, 8, 10,
16}, but may be any integer from 2 to 36; the numerals ('digits') are chosen
from \["0", ... , "9", "A", ... , "Z"\]. The letter-digits are always upper
case.

The functions do not attach a prefix (such as "0x" for hexadecimal) to the
resulting string.

### Error Terms

Some functions return a value including an 'error' term (in a tuple) or sign
(at the end of a string). Such an error is represents what the next digit
would be as a fraction of the radix.

**Example 67** Error term in the tuple of string 'fragments':

    > strs_real_approx_sf 10 floor 3 (234567%100000);
    "+","2","34",567L%1000L
    > strs_real_approx_sf 10 ceil 3 (234567%100000);
    "+","2","35",(-433L)%1000L

(See the function [`strs_real_approx_sf`](#rational::strs_real_approx_sf).)

In strings, only the sign of the error term is given. A “+” should be read as
“and a bit more”; “-” as “but a bit less”.

**Example 68** Error sign in the string:

    > str_real_approx_sf format_uk 10 0 0 floor 3 (234567%100000);
    "2.34(+)"
    > str_real_approx_sf format_uk 10 0 0 ceil 3 (234567%100000);
    "2.35(-)"

(See the function [`str_real_approx_sf`](#rational::str_real_approx_sf).)

**Q** &lt;-&gt; Fraction String (“i + n/d”)
-------------------------------------------

### Formatting to Fraction Strings

<a name="rational::str_vulgar"></a>`rational::str_vulgar q`

:   returns a String representing the rational (or integer) q in the form

    -   “\[−\]n/d”

<a name="rational::str_vulgar_or_int"></a>`rational::str_vulgar_or_int q`

:   returns a String representing the rational (or integer) q in one of the
    forms

    -   “\[−\]n/d”
    -   “\[−\]i”

<a name="rational::str_mixed"></a>`rational::str_mixed q`

:   returns a String representing the rational (or integer) q in one of the
    forms

    -   “i + n/d”
    -   “−(i + n/d)”
    -   “\[−\]n/d”
    -   “\[−\]i”

<!-- -->
**Example 69** The fraction string representations:

    > let l = iterwhile (<= 3%2) (+(1%2)) (-3%2);
    > l;
    [(-3L)%2L,(-1L)%1L,(-1L)%2L,0L%1L,1L%2L,1L%1L,3L%2L]
    > map str_vulgar l;
    ["-3L/2L","-1L/1L","-1L/2L","0L/1L","1L/2L","1L/1L","3L/2L"]
    > map str_vulgar_or_int l;
    ["-3L/2L","-1L","-1L/2L","0L","1L/2L","1L","3L/2L"]
    > map str_mixed l;
    ["-(1L+1L/2L)","-1L","-1L/2L","0L","1L/2L","1L","1L+1L/2L"]

These might be compared to the behaviour of the standard function `str`.

<a name="str/rational"></a>`str x`
:   returns a string representing the value x.

<!-- -->
**Example 70** The standard function str:

    > map str l;
    ["(-3L)%2L","(-1L)%1L","(-1L)%2L","0L%1L","1L%2L","1L%1L","3L%2L"]

### Evaluation of Fraction Strings

<a name="rational::val_vulgar"></a>`rational::val_vulgar strg`

:   returns a rational q represented by the string strg in the form

    -   “\[−\]n/d”

<!-- -->
Such strings can also be evaluated by the `val_mixed` function.

<a name="rational::val_mixed"></a>`rational::val_mixed strg`

:   returns a rational q represented by the string strg

    -   “i + n/d”
    -   “−(i + n/d)”
    -   “\[−\]n/d” — thus val\_mixed strictly extends val\_vulgar
    -   “\[−\]i”

<!-- -->
**Example 71** Evaluating fraction strings:

    > val_vulgar "-22/7";
    (-22L)%7L
    > val_mixed "1L+5L/6L";
    11L%6L

These might be compared to the behaviour of the standard function eval.

<a name="eval/rational"></a>`eval s`
:   evaluates the string s.

<!-- -->
**Example 72** The standard function eval:

    > eval "1+5%6";
    11L%6L
    > eval "1L+5L/6L";
    1.83333333333333

**Q** &lt;-&gt; Recurring Numeral Expansion String (“I.FR”)
-----------------------------------------------------------

See [Internationalisation and Format
Structures](#internationalisation-and-format-structures) for information about
the formatting structure to be supplied in the `fmt` parameter.

### Formatting to Recurring Expansion Strings

<a name="rational::str_real_recur"></a>`rational::str_real_recur fmt radix intGroup q`

:   returns a string (exactly) representing the rational (or integer) q as
    base-Radix expansion of one the forms

    -   “\[−\]int.frac”
    -   “\[−\]int.init frac part\[smallest recurring frac part ...\]”

<!-- -->
Note that there is no fracGroup parameter.

Beware that the string returned by this function can be very long. The length
of the recurring part of such a decimal expansion may be up to one less than
the simplest denominator of q.

**Example 73** The recurring radix expansion-type string representations:

    > str_real_recur format_uk 10 3 (4000001%4); // grouped with commas
    "1,000,000.25"
    > str_real_recur format_uk 10 0 (4000001%4); // no grouping
    "1000000.25"
    > str_real_recur format_uk 10 3 (1000000%3);
    "333,333.[3...]"
    > str_real_recur format_uk 10 3 (1000000%7);
    "142,857.[142857...]"
    > str_real_recur format_uk 10 3 (-1%700);
    "-0.00[142857...]"
    > str_real_recur format_uk 10 3 (127%128);
    "0.9921875"
    > str_real_recur format_uk 2 4 (-127%128);
    "-0.1111111"
    > str_real_recur format_uk 16 4 (127%128);
    "0.FE"
    > str_real_recur format_uk 10 0 (70057%350); // 1%7 + 10001%50;
    "200.16[285714...]"

The function allows expansion to different radices (bases).

**Example 74** The recurring radix expansion in decimal and hexadecimal:

    > str_real_recur format_uk 10 0 (1%100);
    "0.01"
    > str_real_recur format_uk 16 0 (1%100);
    "0.0[28F5C...]"

**Example 75** The recurring radix expansion in duodecimal:

    > str_real_recur format_uk 12 0 (1%100);
    "0.0[15343A0B62A68781B059...]"

Note that this bracket notation is not standard in the literature. Usually the
recurring numerals are indicated by a single dot over the initial and final
numerals of the recurring part, or an overline over the recurring part. For
example 1/70 = 0.0˙14285˙7 = 0.0142857 and 1/3 = 0.˙3 = 0.3.

<a name="rational::strs_real_recur"></a>`rational::strs_real_recur radix q`

:   returns a quadruple of the four strings:

    -   the sign,
    -   integer part (which is empty for 0),
    -   initial fraction part
    -   and recurring fraction part (either and both of which may be empty).

<!-- -->
**Example 76** The recurring radix expansion in decimal — the fragments:

    > strs_real_recur 10 (100%7);
    "+","14","","285714"
    > strs_real_recur 10 (-1%700);
    "-","","00","142857"
    > strs_real_recur 10 (70057%350);
    "+","200","16","285714"

This function may be used to also, e.g. format the integer part with
comma-separated groupings.

<a name="rational::join_str_real_recur"></a>`rational::join_str_real_recur fmt intGroup sign i fracInit fracRecur`
:   formats the parts in the quadruple returned by
    [`strs_real_recur`](#rational::strs_real_recur) to the sort of string as
    returned by [`str_real_recur`](#rational::str_real_recur).

<!-- -->
### Evaluation of Recurring Expansion Strings

The `str_*` and `val_*` functions depend on a 'format structure' parameter
(fmt) such as format uk. Conversions may be performed between rationals and
differently formatted strings if a suitable alternative format structure is
supplied. See [Internationalisation and Format
Structures](#internationalisation-and-format-structures) for information about
formatting structures.

<a name="rational::val_real_recur"></a>`rational::val_real_recur fmt radix strg`

:   returns the rational q represented by the base-radix expansion string strg
    of one the forms

    -   “\[−\]int.frac”
    -   “\[−\]int.init frac part\[recurring frac part ...\]”

<!-- -->
**Example 77** Conversion from the recurring radix expansion-type string
representations:

    > val_real_recur format_uk 10 "-12.345";
    (-2469L)%200L
    > val_real_recur format_uk 10 "0.3";
    3L%10L
    > val_real_recur format_uk 10 "0.[3...]";
    1L%3L
    > val_real_recur format_uk 10 ".333[33...]";
    1L%3L
    > val_real_recur format_uk 10 ".[9...]";
    1L%1L

<a name="rational::sval_real_recur"></a>`rational::sval_real_recur radix sign iStr fracStr recurPartStr`

:   returns the rational q represented by the parts

    -   sign
    -   integer part
    -   initial fraction part
    -   recurring fraction part

<a name="rational::split_str_real_recur"></a>`rational::split_str_real_recur Fmt strg`

:   returns a tuple containing the parts

    -   sign
    -   integer part
    -   initial fraction part
    -   recurring fraction part of one the forms
        -   “\[−\]int.frac”
        -   “\[−\]int.init frac part\[recurring frac part ...\]”

<!-- -->
**Q** &lt;-&gt; Numeral Expansion String (“I.F × 10E”)
------------------------------------------------------

See [Internationalisation and Format
Structures](#internationalisation-and-format-structures) for information about
the formatting structure to be supplied in the `fmt` parameter.

The exponent string "\*10\^" need not depend on the radix, as the numerals for
the number radix in that radix are always "10".

### Formatting to Expansion Strings

#### Functions for Fixed Decimal Places

<a name="rational::str_real_approx_dp"></a>`rational::str_real_approx_dp fmt radix intGroup fracGroup roundfun dp q`
:   returns a string representing a numeral expansion approximation of q to dp
    decimal places, using rounding mode `roundfun` (see [Rounding to
    Integer](#rounding-to-integer)) `roundfun` is usually [`round`](#round) or
    [`round_unbiased`](#rational::round_unbiased). (dp may be positive, zero
    or negative; non-positive dps may look misleading — use e.g. scientific
    notation instead.)

<!-- -->
**Example 78** Decimal places:

    > str_real_approx_dp format_uk 10 3 3 round 2 (22%7);
    "3.14(+)"
    > str_real_approx_dp format_uk 10 3 3 ceil 2 (22%7);
    "3.15(-)"

<a name="rational::strs_real_approx_dp"></a>`rational::strs_real_approx_dp radix roundfun do q`

:   returns a tuple of strings

    -   sign
    -   integer part
    -   fraction part

    representing an expansion to a number of decimal places, together with

    -   the rounding “error”: a fraction representing the next numerals.

<!-- -->
**Example 79** Decimal places — the fragments:

    > strs_real_approx_dp 10 round 2 (22%7);
    "+","3","14",2L%7L
    > strs_real_approx_dp 10 ceil 2 (22%7);
    "+","3","15",(-5L)%7L

<a name="rational::join_str_real_approx"></a>`rational::join_str_real_approx fmt intGroup fracGroup sign i frac err`
:   formats the parts in the quadruple returned by
    [`strs_real_approx_dp`](#rational::strs_real_approx_dp) or
    [`strs_real_approx_sf`](#rational::strs_real_approx_sf) to the sort of
    string as returned by
    [`str_real_approx_dp`](#rational::str_real_approx_dp) or
    [`str_real_approx_sf`](#rational::str_real_approx_sf).

<!-- -->
#### Functions for Significant Figures

<a name="rational::str_real_approx_sf"></a>`rational::str_real_approx_sf fmt radix intGroup fracGroup roundfun sf q`
:   returns a string representing a numeral expansion approximation of q to sf
    significant figures, using rounding mode `roundfun` (see [Rounding to
    Integer](#rounding-to-integer)).

<!-- -->
`roundfun` is usually [`round`](#round) or
[`round_unbiased`](#rational::round_unbiased). (sf must be positive.)

**Example 80** Significant figures:

    > str_real_approx_sf format_uk 10 3 3 floor 2 (22%7);
    "3.1(+)"
    > str_real_approx_sf format_uk 10 3 3 floor 2 ((-22)%7);
    "-3.2(+)"

<a name="rational::strs_real_approx_sf"></a>`rational::strs_real_approx_sf radix roundfun sf q`

:   returns a tuple of strings

    -   sign,
    -   integer part,
    -   fraction part, representing an expansion to a number of significant
        figures, together with
    -   the rounding “error”: a fraction representing the next numerals

<a name="join_str_real_approx"></a>`join_str_real_approx`
:   see [`join_str_real_approx`](#rational::join_str_real_approx).

<!-- -->
#### Functions for Scientific Notation and Engineering Notation

<a name="rational::str_real_approx_sci"></a>`rational::str_real_approx_sci fmt radix intGroup fracGroup roundfun sf q`

:   returns a string expansion with a number of significant figures in
    scientific notation, using rounding mode `roundfun` (see [Rounding to
    Integer](#rounding-to-integer)).

    (sf must be positive; expStep is usually 3, radix is usually 10,
    `roundfun` is usually [`round`](#round) or
    [`round_unbiased`](#rational::round_unbiased);
    [`str_real_approx_sci`](#rational::str_real_approx_sci) is equivalent to
    [`str_real_approx_eng`](#rational::str_real_approx_eng) (below) with
    expStep = 1.)

<a name="rational::strs_real_approx_sci"></a>`rational::strs_real_approx_sci radix roundfun sf q`

:   returns a tuple of strings:

    -   sign of mantissa,
    -   integer part of mantissa,
    -   fraction part of mantissa,
    -   sign of exponent,
    -   exponent magnitude

    representing an expansion to a number of significant figures in scientific
    notation together with

    -   the rounding "error": a fraction representing the next numerals.

<a name="rational::str_real_approx_eng"></a>`rational::str_real_approx_eng fmt expStep radix intGroup fracGroup round sf q`

:   returns a string expansion with a number of significant figures in
    engineering notation, using rounding mode roundfun.

    The ExpStep parameter specifies the granularity of the exponent;
    specifically, the exponent will always be divisible by expStep.

    (sf must be positive; expStep is usually 3 and must be positive, radix is
    usually 10, `roundfun` is usually [`round`](#round) or
    [`round_unbiased`](#rational::round_unbiased).)

<!-- -->
**Example 81** Engineering notation:

    > str_real_approx_eng format_uk 3 10 3 3 round 7 (rational 999950);
    "999.950,0*10^3"
    > str_real_approx_eng format_uk 3 10 3 3 round 4 999950;
    "1.000*10^6(-)"

<a name="rational::strs_real_approx_eng"></a>`rational::strs_real_approx_eng expStep radix roundfun sf q`

:   returns a tuple of strings:

    -   sign of mantissa,
    -   integer part of mantissa,
    -   fraction part of mantissa,
    -   sign of exponent,
    -   exponent magnitude

    representing an expansion to a number of significant figures in
    engineering notation together with

    -   the rounding “error”: a fraction representing the next numerals.

<!-- -->
**Example 82** Engineering notation — the fragments:

    > strs_real_approx_eng 3 10 round 7 (rational 999950);
    "+","999","9500","+","3",0L%1L
    > strs_real_approx_eng 3 10 round 4 999950;
    "+","1","000","+","6",(-1L)%20L

<a name="rational::join_str_real_eng"></a>`rational::join_str_real_eng fmt intGroup fracGroup mantSign mantI mantF rac expSign expI err`
:   formats the parts in the quadruple returned by `strs_real_approx_eng` or
    strs\_real\_approx\_sci to the sort of string as returned by
    [`str_real_approx_eng`](#rational::str_real_approx_eng) or
    [`str_real_approx_sci`](#rational::str_real_approx_sci).

<!-- -->
### Evaluation of Expansion Strings

The `str_*` and `val_*` functions depend on a 'format structure' parameter
(fmt) such as format uk. Conversions may be performed between rationals and
differently formatted strings if a suitable alternative format structure is
supplied. See [Internationalisation and Format
Structures](#internationalisation-and-format-structures) for information about
formatting structures.

<a name="rational::val_real_eng"></a>`rational::val_real_eng fmt radix strg`

:   returns the rational q represented by the base-radix expansion string strg
    of one the forms

    -   “\[−\]int.frac”
    -   “\[−\]int.frace\[−\]exponent”

<!-- -->
**Example 83** Conversion from the recurring radix expansion-type string
representations:

    > val_real_eng format_uk 10 "-12.345";
    (-2469L)%200L
    > val_real_eng format_uk 10 "-12.345*10^2";
    (-2469L)%2L

<a name="rational::sval_real_eng"></a>`rational::sval_real_eng radix signStr mantIStr mantF racStr expSignStr expStr`

:   returns the rational q represented by the parts

    -   sign
    -   integer part of mantissa
    -   fraction part of mantissa
    -   sign of exponent
    -   exponent

<a name="rational::split_str_real_eng"></a>`rational::split_str_real_eng fmt strg`

:   returns a tuple containing the string parts

    -   sign
    -   integer part of mantissa
    -   fraction part of mantissa
    -   sign of exponent
    -   exponent
    -   the “error” sign

    of one the forms

    -   “\[−\]int.frac”
    -   “\[−\]int.frac ×10\^\[−\]exponent”

<!-- -->
These functions can deal with the fixed decimal places, the significant
figures and the scientific notation in addition to the engineering notation.

Numeral String -&gt; **Q** — Approximation
------------------------------------------

This section describes functions to approximate by a rational a real number
represented by a string. See [R -&gt; Q —
Approximation](#r--gt-q-approximation) for approximation by a rational of a
double.

The `str_*` and `val_*` functions depend on a 'format structure' parameter
(fmt) such as format uk. Conversions may be performed between rationals and
differently formatted strings if a format structure is supplied. See
[Internationalisation and Format
Structures](#internationalisation-and-format-structures) for information about
formatting structures.

<a name="rational::val_eng_approx_epsilon"></a>`rational::val_eng_approx_epsilon fmt radix epsilon strg`

:   Find the least complex rational approximation q to the number represented
    by the base-radix expansion string str in one of the forms

    -   “\[−\]int.frac”
    -   “\[−\]int.frac ×10\^\[−\]exponent”

    that is ε-close. That is find a q such that |q − eval str| ≤ ε.

<!-- -->
**Example 84** Rational from a long string:

    > let strg = "123.456,789,876,543,212,345,678,987,654,321*10^27";
    > let x = val_real_eng format_uk 10 strg;
    > x;
    123456789876543212345678987654321L%1000L
    > let q = val_eng_approx_epsilon format_uk 10 (1%100) strg;
    > q;
    1975308638024691397530863802469L%16L
    > double (x - q);
    0.0085
    > str_real_approx_eng format_uk 3 10 3 3 round 30 q;
    "123.456,789,876,543,212,345,678,987,654*10^27(+)"
    > str_real_approx_eng format_uk 3 10 3 3 round 42 q;
    "123.456,789,876,543,212,345,678,987,654,312,500,000,000*10^27"
    > double q;
    1.23456789876543e+029

<a name="rational::val_eng_interval_epsilon"></a>`rational::val_eng_interval_epsilon fmt radix epsilon strg`

:   Find the least complex rational interval containing the number represented
    by the base-radix expansion string strg in one of the forms

    -   “\[−\]int.frac”
    -   “\[−\]int.frac ×10\^\[−\]exponent”

    that is "-small.

<a name="rational::val_eng_approx_max_den"></a>`rational::val_eng_approx_max_den fmt radix maxDen strg`

:   Find the closest rational approximation to the number represented by the
    base-rRadix expansion string strg in one of the forms

    -   “\[−\]int.frac”
    -   “\[−\]int.frac ×10\^\[−\]exponent”

    that has a denominator no greater than maxDen. (maxDen &gt; 0)

<a name="rational::val_eng_interval_max_den"></a>`rational::val_eng_interval_max_den fmt radix maxDen strg`

:   Find the smallest rational interval containing the number represented by
    the base-radix expansion string strg in one of the forms

    -   “\[−\]int.frac”
    -   “\[−\]int.frac ×10\^\[−\]exponent”

    that has endpoints with denominators no greater than maxDen.
    (maxDen &gt; 0)

<!-- -->
**Example 85** Other rationals from a long string:

    > val_eng_approx_epsilon format_uk 10 (1%100) strg;
    1975308638024691397530863802469L%16L
    > val_eng_interval_epsilon format_uk 10 (1%100) strg;
    interval::Ivl (3086419746913580308641974691358L%25L)
    (3456790116543209945679011654321L%28L)
    > val_eng_approx_max_den format_uk 10 100 strg;
    9999999980000000199999998000000L%81L
    > val_eng_interval_max_den format_uk 10 100 strg;
    interval::Ivl 9999999980000000199999998000000L%81L
    3456790116543209945679011654321L%28L
