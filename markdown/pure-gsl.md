<a name="doc-pure-gsl"></a>

pure-gsl - GNU Scientific Library Interface for Pure
====================================================

<a name="module-gsl"></a>

Version 0.12, March 06, 2017

| Albert Graef &lt;<aggraef@gmail.com>&gt;
| Eddie Rucker &lt;<erucker@bmc.edu>&gt;

License: GPL V3 or later, see the accompanying COPYING file

Building on Pure's GSL-compatible matrix support, this module aims to provide
a complete wrapper for the GNU Scientific Library which provides a wide range
of mathematical routines useful for scientific programming, number crunching
and signal processing applications.

This is still work in progress, only a small part of the interface is finished
right now. Here is a brief summary of the operations which are implemented:

-   Matrix-scalar and matrix-matrix arithmetic. This is fairly complete and
    includes matrix multiplication, as well as element-wise exponentiation
    (\^) and integer operations (div, mod, bit shifts and bitwise logical
    operations) which aren't actually in the GSL API.
-   SVD (singular value decomposition), as well as the corresponding solvers,
    pseudo inverses and left and right matrix division. This is only available
    for real matrices right now, as GSL doesn't implement complex SVD.
-   Random distributions (p.d.f. and c.d.f.) and statistic functions.
-   Polynomial evaluation and roots.
-   Linear least-squares fitting. Multi-fitting is not available yet.

Installation instructions: Get the latest source from
<https://bitbucket.org/purelang/pure-lang/downloads/pure-gsl-0.12.tar.gz>. Run
`make` to compile the module and `make install` (as root) to install it in the
Pure library directory. This requires GNU make, and of course you need to have
Pure and GSL installed. The `make install` step is only necessary for
system-wide installation.

`make` tries to guess your Pure installation directory and platform-specific
setup. If it gets this wrong, you can set some variables manually. In
particular, `make install prefix=/usr` sets the installation prefix, and
`make PIC=-fPIC` or some similar flag might be needed for compilation on 64
bit systems. Please see the Makefile for details.

The current release requires GSL 1.11 or later and Pure 0.45 or later. Older
GSL versions might still work, but then some operations may be missing. The
latest and greatest GSL version is always available from
<http://www.gnu.org/software/gsl>.

After installation, you can import the entire GSL interface as follows:

    using gsl;

For convenience, the different parts of the GSL interface are also available
as separate modules. E.g., if you only need the matrix operations:

    using gsl::matrix;

In either case, the global `gsl_version` variable reports the installed GSL
version:

    > show gsl_version
    let gsl_version = "1.11";

(This variable used to be defined by the Pure runtime but has been moved into
pure-gsl as of Pure 0.37.)

Most other operations are declared in separate namespaces which are in 1-1
correspondence with the module names. Thus, e.g., the `gsl_poly_eval` routine
is named `gsl::poly::eval` in Pure and can be found in the `gsl::poly` module
and namespace. The `using namespace` declaration can be used to facilitate
access to the operations in a given namespace, e.g.:

    > using gsl::poly;
    > using namespace gsl::poly;
    > eval {1,2,3} 2;
    17

See the `examples` folder in the sources for some examples.

If you'd like to contribute, please mail the authors or contact us at
<http://groups.google.com/group/pure-lang>.

Polynomials
-----------

<a name="module-gslpoly"></a>

This module provides Pure wrappers for the GSL polynomial routines. For detail
about the routines, see Chapter 6 of the GSL manual,

<http://www.gnu.org/software/gsl/manual/html_node/Polynomials.html>.

Polynomials are represented by vectors (one row matrices).

### Routines

<a name="gsl::poly::eval"></a>`gsl::poly::eval c::matrix  x`

:   implements `gsl_poly_eval`, `gsl_poly_complex_eval`, and
    `gsl_complex_poly_eval` without the `len` parameter.

    GSL does not supply an integer routine for evaluating polynomials with
    `int` or `bigint` coefficients. Therefore, an integer routine has been
    provided in pure-gsl using the Chinese Remainder Theorem.

<a name="gsl::poly::dd_init"></a>`gsl::poly::dd_init x::matrix  y::matrix`
:   implements `gsl_poly_dd_init` without the `size` parameter.

<a name="gsl::poly::dd_eval"></a>`gsl::poly::dd_eval dd::matrix  xa::matrix  x::double`
:   implements `gsl_poly_dd_eval` without the `size` parameter.

<a name="gsl::poly::dd_taylor"></a>`gsl::poly::dd_taylor xp::double  dd::matrix  xa::matrix`
:   implements `gsl_poly_dd_taylor` without the `size` and workspace `w`
    arguments.

<a name="gsl::poly::solve_quadratic"></a>`gsl::poly::solve_quadratic a  b  c`
:   implements `gsl_poly_solve_quadratic`. This function returns a list of
    roots instead of passing them through the parameters `x0` and `x1`.

<a name="gsl::poly::complex_solve_quadratic"></a>`gsl::poly::complex_solve_quadratic a  b  c`
:   implements `gsl_poly_complex_solve_quadratic`. This function returns a
    list of roots instead of passing trhough the parameters `z0` and `z1`.

<a name="gsl::poly::solve_cubic"></a>`gsl::poly::solve_cubic a  b  c`
:   implements `gsl_poly_solve_cubic`. This function returns a list of roots
    instead of passing them through the parameters `x0`, `x1`, and `x2`.

<a name="gsl::poly::complex_solve_cubic"></a>`gsl::poly::complex_solve_cubic a  b  c`
:   implements `gsl_poly_complex_colve_cubic`. This function returns a list of
    roots instead of passing them through the parameters `z0`, `z1`, and `z2`.

<a name="gsl::poly::complex_solve"></a>`gsl::poly::complex_solve c::matrix`
:   implements `gsl_poly_complex_solve` omitting the parametrs `n` and `w`.
    The GSL routines for creating and freeing the workspace are handled
    automatically.

<!-- -->
### Examples

Usage of each library routine is illustrated below.

    > using gsl::poly;
    > using namespace gsl::poly;
    > eval {1,2,3} 2;
    17
    > eval {1.0,2.0,3.0} (-2.0);
    9.0
    > eval {1, 2, 2} (1+:1);
    3.0+:6.0
    > eval {1+:2, 2+:3, 2+:3} (1+:1);
    -6.0+:11.0
    > let dd = dd_init {1,2,3} {2,4,6};
    > dd;
    {2.0,2.0,0.0}
    > dd_eval dd {1,2,3} 2;
    4.0
    > dd_taylor 0.0 dd {1,2,3};
    {0.0,2.0,0.0}
    > solve_quadratic 2 4 1;
    [-1.70710678118655,-0.292893218813452]
    > solve_quadratic 1 4 4;
    [-2.0,-2.0]
    > solve_quadratic 0 2 1;
    [-0.5]
    > solve_quadratic 1 2 8;
    []
    > complex_solve_quadratic 0 2 1;
    [-0.5+:0.0]
    > complex_solve_quadratic 2 2 3;
    [-0.5+:-1.11803398874989,-0.5+:1.11803398874989]   
    > solve_cubic 3 3 1;
    [-1.0,-1.0,-1.0]
    > solve_cubic 3 2 1;
    [-2.32471795724475]
    > complex_solve_cubic 2 2 1;
    [-1.0+:0.0,-0.5+:-0.866025403784439,-0.5+:0.866025403784439]
    > complex_solve {6,1,-7,-1,1};
    [1.0+:0.0,-1.0+:0.0,-2.0+:0.0,3.0+:0.0]

Special Functions
-----------------

<a name="module-gslsf"></a>

This module is loaded via the command `using gsl::sf` and provides Pure
wrappers for the GSL Special Functions. For details, see Chapter 7 of the GSL
manual,

<http://www.gnu.org/software/gsl/manual/html_node/Special-Functions.html>.

To load the library, use the Pure command `using gsl::sf`. Modes for the
functions must be one of:

    GSL_PREC_DOUBLE
    GSL_PREC_SINGLE
    GSL_PREC_APPROX

Results for some of the functions are returned as a Pure list instead of the
`gsl_sf_result` or `gsl_sf_result_e10` structures in C. In these cases, the
resulting list is one of the following forms.

> -   `[val, err]` for the `gsl_sf_result` struct and
> -   `[val, err, e10]` for the `gsl_sf_result_e10` struct.

### Airy Functions

<a name="gsl::sf::airy_Ai"></a>`gsl::sf::airy_Ai x`, <a name="gsl::sf::airy_Ai"></a>`gsl::sf::airy_Ai (x, mode::int)`
:   implements `gsl_sf_airy_Ai`. The first form computes the function with
    `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Ai_e"></a>`gsl::sf::airy_Ai_e x`, <a name="gsl::sf::airy_Ai_e"></a>`gsl::sf::airy_Ai_e (x, mode::int)`
:   implements `gsl_sf_airy_Ai_e`. The first form computes the function with
    `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Ai_scaled"></a>`gsl::sf::airy_Ai_scaled x`, <a name="gsl::sf::airy_Ai_scaled"></a>`gsl::sf::airy_Ai_scaled (x, mode::int)`
:   implements `gsl_sf_airy_Ai_scaled`. The first form computes the function
    with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Ai_scaled_e"></a>`gsl::sf::airy_Ai_scaled_e x`, <a name="gsl::sf::airy_Ai_scaled_e"></a>`gsl::sf::airy_Ai_scaled_e (x, mode::int)`
:   implements `gsl_sf_airy_Ai_scaled_e`. The first form computes the function
    with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Bi"></a>`gsl::sf::airy_Bi x`, <a name="gsl::sf::airy_Bi"></a>`gsl::sf::airy_Bi (x, mode::int)`
:   implements `gsl_sf_airy_Bi`. The first form computes the function with
    `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Bi_e"></a>`gsl::sf::airy_Bi_e x`, <a name="gsl::sf::airy_Bi_e"></a>`gsl::sf::airy_Bi_e (x, mode::int)`
:   implements `gsl_sf_airy_Bi_e`. The first form computes the function with
    `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Bi_scaled"></a>`gsl::sf::airy_Bi_scaled x`, <a name="gsl::sf::airy_Bi_scaled"></a>`gsl::sf::airy_Bi_scaled (x, mode::int)`
:   implements `gsl_sf_airy_Bi_scaled`. The first form computes the function
    with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Bi_scaled_e"></a>`gsl::sf::airy_Bi_scaled_e x`, <a name="gsl::sf::airy_Bi_scaled_e"></a>`gsl::sf::airy_Bi_scaled_e (x, mode::int)`
:   implements `gsl_sf_airy_Bi_scaled_e`. The first form computes the function
    with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Ai_deriv"></a>`gsl::sf::airy_Ai_deriv x`, <a name="gsl::sf::airy_Ai_deriv"></a>`gsl::sf::airy_Ai_deriv (x, mode::int)`
:   implements `gsl_sf_airy_Ai_deriv`. The first form computes the function
    with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Ai_deriv_e"></a>`gsl::sf::airy_Ai_deriv_e x`, <a name="gsl::sf::airy_Ai_deriv_e"></a>`gsl::sf::airy_Ai_deriv_e (x, mode::int)`
:   implements `gsl_sf_airy_Ai_deriv_e`. The first form computes the function
    with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Ai_deriv_scaled"></a>`gsl::sf::airy_Ai_deriv_scaled x`, <a name="gsl::sf::airy_Ai_deriv_scaled"></a>`gsl::sf::airy_Ai_deriv_scaled (x, mode::int)`
:   implements `gsl_sf_airy_Ai_deriv_scaled`. The first form computes the
    function with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Ai_deriv_scaled_e"></a>`gsl::sf::airy_Ai_deriv_scaled_e x`, <a name="gsl::sf::airy_Ai_deriv_scaled_e"></a>`gsl::sf::airy_Ai_deriv_scaled_e (x, mode::int)`
:   implements `gsl_sf_airy_Ai_deriv_scaled_e`. The first form computes the
    function with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Bi_deriv"></a>`gsl::sf::airy_Bi_deriv x`, <a name="gsl::sf::airy_Bi_deriv"></a>`gsl::sf::airy_Bi_deriv (x, mode::int)`
:   implements `gsl_sf_airy_Bi_deriv`. The first form computes the function
    with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Bi_deriv_e"></a>`gsl::sf::airy_Bi_deriv_e x`, <a name="gsl::sf::airy_Bi_deriv_e"></a>`gsl::sf::airy_Bi_deriv_e (x, mode::int)`
:   implements `gsl_sf_airy_Bi_deriv_e`. The first form computes the function
    with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Bi_deriv_scaled"></a>`gsl::sf::airy_Bi_deriv_scaled x`, <a name="gsl::sf::airy_Bi_deriv_scaled"></a>`gsl::sf::airy_Bi_deriv_scaled (x, mode::int)`
:   implements `gsl_sf_airy_Bi_deriv_scaled`. The first form computes the
    function with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_Bi_deriv_scaled_e"></a>`gsl::sf::airy_Bi_deriv_scaled_e x`, <a name="gsl::sf::airy_Bi_deriv_scaled_e"></a>`gsl::sf::airy_Bi_deriv_scaled_e (x, mode::int)`
:   implements `gsl_sf_airy_Bi_deriv_scaled_e`. The first form computes the
    function with `mode = GSL_PREC_DOUBLE`.

<a name="gsl::sf::airy_zero_Ai"></a>`gsl::sf::airy_zero_Ai s`
:   implements `gsl_sf_airy_zero_Ai`.

<a name="gsl::sf::airy_zero_Ai_e"></a>`gsl::sf::airy_zero_Ai_e s`
:   implements `gsl_sf_airy_zero_Ai_e`.

<a name="gsl::sf::airy_zero_Bi"></a>`gsl::sf::airy_zero_Bi s`
:   implements `gsl_sf_airy_zero_Bi`.

<a name="gsl::sf::airy_zero_Bi_e"></a>`gsl::sf::airy_zero_Bi_e s`
:   implements `gsl_sf_airy_zero_Bi_e`.

<a name="gsl::sf::airy_zero_Ai_deriv"></a>`gsl::sf::airy_zero_Ai_deriv s`
:   implements `gsl_sf_airy_zero_Ai_deriv`.

<a name="gsl::sf::airy_zero_Ai_deriv_e"></a>`gsl::sf::airy_zero_Ai_deriv_e s`
:   implements `gsl_sf_airy_zero_Ai_deriv_e`.

<a name="gsl::sf::airy_zero_Bi_deriv"></a>`gsl::sf::airy_zero_Bi_deriv s`
:   implements `gsl_sf_airy_zero_Bi_deriv`.

<a name="gsl::sf::airy_zero_Bi_deriv_e"></a>`gsl::sf::airy_zero_Bi_deriv_e s`
:   implements `gsl_sf_airy_zero_Bi_deriv_e`.

<!-- -->
### Examples

The following illustrate the Airy functions.

    > using gsl::sf;
    > using namespace gsl::sf;
    > airy_Ai (-1.2); // defaults to GSL_PREC_DOUBLE
    0.52619437480212
    > airy_Ai_scaled (-1.2);
    0.52619437480212
    > airy_Ai (-1.2,GSL_PREC_APPROX);
    0.526194374771687
    > airy_Ai_scaled (-1.2, GSL_PREC_SINGLE);
    0.526194374771687
    > airy_Ai_e (-1.2);
    [0.52619437480212,1.88330586480371e-15]
    > airy_Ai_e (-1.2,GSL_PREC_APPROX);
    [0.526194374771687,1.01942940819652e-08]
    > airy_Ai_scaled_e (-1.2);
    [0.52619437480212,1.88330586480371e-15]
    > airy_Ai_scaled_e (-1.2,GSL_PREC_APPROX);
    [0.526194374771687,1.01942940819652e-08]
    > airy_Bi (-1.2);
    -0.015821370184632
    > airy_Bi_scaled (-1.2);
    -0.015821370184632
    > airy_Bi (-1.2,GSL_PREC_APPROX);
    -0.0158213701898015
    > airy_Bi_scaled (-1.2, GSL_PREC_SINGLE);
    -0.0158213701898015
    > airy_Bi_e (-1.2);
    [-0.015821370184632,1.31448899295896e-16]
    > airy_Bi_e (-1.2,GSL_PREC_APPROX);
    [-0.0158213701898015,4.10638404843775e-10]
    > airy_Bi_scaled_e (-1.2);
    [-0.015821370184632,1.31448899295896e-16]
    > airy_Bi_scaled_e (-1.2,GSL_PREC_APPROX);
    [-0.0158213701898015,4.10638404843775e-10]
    > airy_Ai_deriv (-1.2); // defaults to GSL_PREC_DOUBLE
    0.107031569272281
    > airy_Ai_deriv_scaled (-1.2);
    0.107031569272281
    > airy_Ai_deriv (-1.2,GSL_PREC_APPROX);
    0.107031569264504
    > airy_Ai_deriv_scaled (-1.2, GSL_PREC_SINGLE);
    0.107031569264504
    > airy_Ai_deriv_e (-1.2);
    [0.107031569272281,3.02919983680384e-16]
    > airy_Ai_deriv_e (-1.2,GSL_PREC_APPROX);
    [0.107031569264504,9.25921017197604e-11]
    > airy_Ai_deriv_scaled_e (-1.2);
    [0.107031569272281,3.02919983680384e-16]
    > airy_Ai_deriv_scaled_e (-1.2,GSL_PREC_APPROX);
    [0.107031569264504,9.25921017197604e-11]
    > airy_Bi_deriv (-1.2);
    0.601710157437464
    > airy_Bi_deriv_scaled (-1.2);
    0.601710157437464
    > airy_Bi_deriv (-1.2,GSL_PREC_APPROX);
    0.601710157441937
    > airy_Bi_deriv_scaled (-1.2, GSL_PREC_SINGLE);
    0.601710157441937
    > airy_Bi_deriv_e (-1.2);
    [0.601710157437464,1.7029557943563e-15]
    > airy_Bi_deriv_e (-1.2,GSL_PREC_APPROX);
    [0.601710157441937,5.20534347823991e-10]
    > airy_Bi_deriv_scaled_e (-1.2);
    [0.601710157437464,1.7029557943563e-15]
    > airy_Bi_deriv_scaled_e (-1.2,GSL_PREC_APPROX);
    [0.601710157441937,5.20534347823991e-10]
    > airy_zero_Ai 2;
    -4.08794944413097
    > airy_zero_Ai_e 3;
    [-5.52055982809555,1.22581052599448e-15]
    > airy_zero_Bi 2;
    -3.27109330283635
    > airy_zero_Bi_e 3;
    [-4.83073784166202,1.07263927554824e-15]
    > airy_zero_Ai_deriv 2;
    -3.24819758217984
    > airy_zero_Ai_deriv_e 3;
    [-4.82009921117874,1.07027702504564e-15]
    > airy_zero_Bi_deriv 2;
    -4.07315508907183
    > airy_zero_Bi_deriv_e 3;
    [-5.5123957296636,1.22399773198358e-15]

### Bessel Functions

<a name="gsl::sf::bessel_J0"></a>`gsl::sf::bessel_J0 x`
:   implements `gsl_sf_bessel_J0`.

<a name="gsl::sf::bessel_J0_e"></a>`gsl::sf::bessel_J0_e x`
:   implements `gsl_sf_besselJ0_e`.

<a name="gsl::sf::bessel_J1"></a>`gsl::sf::bessel_J1 x`
:   implements `gsl_sf_bessel_J1`.

<a name="gsl::sf::bessel_J1_e"></a>`gsl::sf::bessel_J1_e x`
:   implements `gsl_sf_bessel_J1_e`.

<a name="gsl::sf::bessel_Jn"></a>`gsl::sf::bessel_Jn n  x`
:   implements `gsl_sf_bessel_Jn`.

<a name="gsl::sf::bessel_Jn_e"></a>`gsl::sf::bessel_Jn_e n  x`
:   implements `gsl_sf_bessel_Jn_e`.

<a name="gsl::sf::bessel_Jn_array"></a>`gsl::sf::bessel_Jn_array nmin::int  nmax::int  x`
:   implements `gsl_sf_bessel_Jn_array`.

<a name="gsl::sf::bessel_Y0"></a>`gsl::sf::bessel_Y0 x`
:   implements `gsl_sf_bessel_Y0`.

<a name="gsl::sf::bessel_Y0_e"></a>`gsl::sf::bessel_Y0_e x`
:   implements `gsl_sf_bessel_Y0_e`.

<a name="gsl::sf::bessel_Y1"></a>`gsl::sf::bessel_Y1 x`
:   implements `gsl_sf_bessel_Y1`.

<a name="gsl::sf::bessel_Y1_e"></a>`gsl::sf::bessel_Y1_e x`
:   implements `gsl_sf_bessel_Y1_e`.

<a name="gsl::sf::bessel_Yn"></a>`gsl::sf::bessel_Yn x`
:   implements `gsl_sf_bessel_Yn`.

<a name="gsl::sf::bessel_Yn_e"></a>`gsl::sf::bessel_Yn_e x`
:   implements `gsl_sf_bessel_Yn_e`.

<a name="gsl::sf::bessel_Yn_array"></a>`gsl::sf::bessel_Yn_array nmin::int  nmax::int  x`
:   implements `gsl_sf_bessel_Yn_array`.

<a name="gsl::sf::bessel_I0"></a>`gsl::sf::bessel_I0 x`
:   implements `gsl_sf_bessel_I0`.

<a name="gsl::sf::bessel_I0_e"></a>`gsl::sf::bessel_I0_e x`
:   implements `gsl_sf_bessel_I0_e`.

<a name="gsl::sf::bessel_I1"></a>`gsl::sf::bessel_I1 x`
:   implements `gsl_sf_bessel_I1`.

<a name="gsl::sf::bessel_I1_e"></a>`gsl::sf::bessel_I1_e x`
:   implements `gsl_sf_bessel_I1_e`.

<a name="gsl::sf::bessel_In"></a>`gsl::sf::bessel_In n::int  x`
:   implements `gsl_sf_bessel_In`.

<a name="gsl::sf::bessel_In_e"></a>`gsl::sf::bessel_In_e n::int  x`
:   implements `gsl_sf_bessel_In_e`

<a name="gsl::sf::bessel_In_array"></a>`gsl::sf::bessel_In_array nmin::int  nmax::int  x`
:   implements `gsl_sf_bessel_In_array`.

<a name="gsl::sf::bessel_I0_scaled"></a>`gsl::sf::bessel_I0_scaled x`
:   implements `gsl_sf_bessel_I0_scaled`.

<a name="gsl::sf::bessel_I0_scaled_e"></a>`gsl::sf::bessel_I0_scaled_e x`
:   implements `gsl_sf_bessel_I0_scaled_e`.

<a name="gsl::sf::bessel_I1_scaled"></a>`gsl::sf::bessel_I1_scaled x`
:   implements `gsl_sf_bessel_I1_scaled`.

<a name="gsl::sf::bessel_I1_scaled_e"></a>`gsl::sf::bessel_I1_scaled_e x`
:   implements `gsl_sf_bessel_I1_scaled_e`.

<a name="gsl::sf::bessel_In_scaled"></a>`gsl::sf::bessel_In_scaled n::int  x`
:   implements `gsl_sf_bessel_In_scaled`.

<a name="gsl::sf::bessel_In_scaled_e"></a>`gsl::sf::bessel_In_scaled_e n::int  x`
:   implements `gsl_sf_bessel_In_scaled_e`.

<a name="gsl::sf::bessel_In_scaled_array"></a>`gsl::sf::bessel_In_scaled_array nmin::int  nmax::int  x`
:   implements `gsl_sf_bessel_In_array`.

<a name="gsl::sf::bessel_K0"></a>`gsl::sf::bessel_K0 x`
:   implements `gsl_sf_bessel_K0`.

<a name="gsl::sf::bessel_K0_e"></a>`gsl::sf::bessel_K0_e x`
:   implements `gsl_sf_bessel_K0_e`.

<a name="gsl::sf::bessel_K1"></a>`gsl::sf::bessel_K1 x`
:   implements `gsl_sf_bessel_K1`.

<a name="gsl::sf::bessel_K1_e"></a>`gsl::sf::bessel_K1_e x`
:   implements `gsl_sf_bessel_K1_e`.

<a name="gsl::sf::bessel_Kn"></a>`gsl::sf::bessel_Kn n::int  x`
:   implements `gsl_sf_bessel_Kn`.

<a name="gsl::sf::bessel_Kn_e"></a>`gsl::sf::bessel_Kn_e n::int  x`
:   implements `gsl_sf_bessel_Kn_e`

<a name="gsl::sf::bessel_Kn_array"></a>`gsl::sf::bessel_Kn_array nmin::int  nmax::int  x`
:   implements `gsl_sf_bessel_Kn_array`.

<a name="gsl::sf::bessel_K0_scaled"></a>`gsl::sf::bessel_K0_scaled x`
:   implements `gsl_sf_bessel_K0_scaled`.

<a name="gsl::sf::bessel_K0_scaled_e"></a>`gsl::sf::bessel_K0_scaled_e x`
:   implements `gsl_sf_bessel_K0_scaled_e`.

<a name="gsl::sf::bessel_K1_scaled"></a>`gsl::sf::bessel_K1_scaled x`
:   implements `gsl_sf_bessel_K1_scaled`.

<a name="gsl::sf::bessel_K1_scaled_e"></a>`gsl::sf::bessel_K1_scaled_e x`
:   implements `gsl_sf_bessel_K1_scaled_e`.

<a name="gsl::sf::bessel_Kn_scaled"></a>`gsl::sf::bessel_Kn_scaled n::int  x`
:   implements `gsl_sf_bessel_Kn_scaled`.

<a name="gsl::sf::bessel_Kn_scaled_e"></a>`gsl::sf::bessel_Kn_scaled_e n::int  x`
:   implements `gsl_sf_bessel_Kn_scaled_e`.

<a name="gsl::sf::bessel_Kn_scaled_array"></a>`gsl::sf::bessel_Kn_scaled_array nmin::int  nmax::int  x`
:   implements `gsl_sf_bessel_Kn_array`.

<a name="gsl::sf::bessel_j0"></a>`gsl::sf::bessel_j0 x`
:   implements `gsl_sf_bessel_j0`.

<a name="gsl::sf::bessel_j0_e"></a>`gsl::sf::bessel_j0_e x`
:   implements `gsl_sf_bessel_j0_e`.

<a name="gsl::sf::bessel_j1"></a>`gsl::sf::bessel_j1 x`
:   implements `gsl_sf_bessel_j1`.

<a name="gsl::sf::bessel_j1_e"></a>`gsl::sf::bessel_j1_e x`
:   implements `gsl_sf_bessel_j1_e`.

<a name="gsl::sf::bessel_j2"></a>`gsl::sf::bessel_j2 x`
:   implements `gsl_sf_bessel_j2`.

<a name="gsl::sf::bessel_j2_e"></a>`gsl::sf::bessel_j2_e x`
:   implements `gsl_sf_bessel_j2_e`.

<a name="gsl::sf::bessel_jl"></a>`gsl::sf::bessel_jl l::int  x`
:   implements `gsl_sf_bessel_jl`.

<a name="gsl::sf::bessel_jl_e"></a>`gsl::sf::bessel_jl_e l::int  x`
:   implements `gsl_sf_bessel_jl_e`.

<a name="gsl::sf::bessel_jl_array"></a>`gsl::sf::bessel_jl_array lmax::int  x`
:   implements `gsl_sf_bessel_jl_array`.

<a name="gsl::sf::bessel_jl_steed_array"></a>`gsl::sf::bessel_jl_steed_array lmax::int  x`
:   implements `gsl_sf_bessel_jl_steed_array`.

<a name="gsl::sf::bessel_y0"></a>`gsl::sf::bessel_y0 x`
:   implements `gsl_sf_bessel_y0`.

<a name="gsl::sf::bessel_y0_e"></a>`gsl::sf::bessel_y0_e x`
:   implements `gsl_sf_bessel_y0_e`.

<a name="gsl::sf::bessel_y1"></a>`gsl::sf::bessel_y1 x`
:   implements `gsl_sf_bessel_y1`.

<a name="gsl::sf::bessel_y1_e"></a>`gsl::sf::bessel_y1_e x`
:   implements `gsl_sf_bessel_y1_e`.

<a name="gsl::sf::bessel_y2"></a>`gsl::sf::bessel_y2 x`
:   implements `gsl_sf_bessel_y2`.

<a name="gsl::sf::bessel_y2_e"></a>`gsl::sf::bessel_y2_e x`
:   implements `gsl_sf_bessel_y2_e`.

<a name="gsl::sf::bessel_yl"></a>`gsl::sf::bessel_yl l::int  x`
:   implements `gsl_sf_bessel_yl`.

<a name="gsl::sf::bessel_yl_e"></a>`gsl::sf::bessel_yl_e l::int  x`
:   implements `gsl_sf_bessel_yl_e`.

<a name="gsl::sf::bessel_yl_array"></a>`gsl::sf::bessel_yl_array lmax::int  x`
:   implements `gsl_sf_bessel_yl_array`.

<a name="gsl::sf::bessel_i0_scaled"></a>`gsl::sf::bessel_i0_scaled x`
:   implements `gsl_sf_bessel_i0_scaled`.

<a name="gsl::sf::bessel_i0_scaled_e"></a>`gsl::sf::bessel_i0_scaled_e x`
:   implements `gsl_sf_bessel_i0_scaled_e`.

<a name="gsl::sf::bessel_i1_scaled"></a>`gsl::sf::bessel_i1_scaled x`
:   implements `gsl_sf_bessel_i1_scaled`.

<a name="gsl::sf::bessel_i1_scaled_e"></a>`gsl::sf::bessel_i1_scaled_e x`
:   implements `gsl_sf_bessel_i1_scaled_e`.

<a name="gsl::sf::bessel_i2_scaled"></a>`gsl::sf::bessel_i2_scaled x`
:   implements `gsl_sf_bessel_i2_scaled`.

<a name="gsl::sf::bessel_i2_scaled_e"></a>`gsl::sf::bessel_i2_scaled_e x`
:   implements `gsl_sf_bessel_i2_scaled_e`.

<a name="gsl::sf::bessel_il_scaled"></a>`gsl::sf::bessel_il_scaled l::int  x`
:   implements `gsl_sf_bessel_il_scaled`.

<a name="gsl::sf::bessel_il_scaled_e"></a>`gsl::sf::bessel_il_scaled_e l::int  x`
:   implements `gsl_sf_bessel_il_scaled_e`.

<a name="gsl::sf::bessel_il_scaled_array"></a>`gsl::sf::bessel_il_scaled_array lmax::int  x`
:   implements `gsl_sf_bessel_il_scaled_array`.

<a name="gsl::sf::bessel_k0_scaled"></a>`gsl::sf::bessel_k0_scaled x`
:   implements `gsl_sf_bessel_k0_scaled`.

<a name="gsl::sf::bessel_k0_scaled_e"></a>`gsl::sf::bessel_k0_scaled_e x`
:   implements `gsl_sf_bessel_k0_scaled_e`.

<a name="gsl::sf::bessel_k1_scaled"></a>`gsl::sf::bessel_k1_scaled x`
:   implements `gsl_sf_bessel_k1_scaled`.

<a name="gsl::sf::bessel_k1_scaled_e"></a>`gsl::sf::bessel_k1_scaled_e x`
:   implements `gsl_sf_bessel_ik_scaled_e`.

<a name="gsl::sf::bessel_k2_scaled"></a>`gsl::sf::bessel_k2_scaled x`
:   implements `gsl_sf_bessel_k2_scaled`.

<a name="gsl::sf::bessel_k2_scaled_e"></a>`gsl::sf::bessel_k2_scaled_e x`
:   implements `gsl_sf_bessel_k2_scaled_e`.

<a name="gsl::sf::bessel_kl_scaled"></a>`gsl::sf::bessel_kl_scaled l::int  x`
:   implements `gsl_sf_bessel_kl_scaled`.

<a name="gsl::sf::bessel_kl_scaled_e"></a>`gsl::sf::bessel_kl_scaled_e l::int  x`
:   implements `gsl_sf_bessel_kl_scaled_e`.

<a name="gsl::sf::bessel_kl_scaled_array"></a>`gsl::sf::bessel_kl_scaled_array lmax::int  x`
:   implements `gsl_sf_bessel_il_scaled_array`.

<a name="gsl::sf::bessel_Jnu"></a>`gsl::sf::bessel_Jnu nu  x`
:   implements `gsl_sf_bessel_Jnu`.

<a name="gsl::sf::bessel_Jnu_e"></a>`gsl::sf::bessel_Jnu_e nu  x`
:   implements `gsl_sf_bessel_Jnu_e`.

<a name="gsl::sf::bessel_sequence_Jnu_e"></a>`gsl::sf::bessel_sequence_Jnu_e nu  v::matrix`
:   implements `gsl_sf_bessel_sequence_Jnu_e`.

<a name="gsl::sf::bessel_Ynu"></a>`gsl::sf::bessel_Ynu nu  x`
:   implements `gsl_sf_bessel_Ynu`.

<a name="gsl::sf::bessel_Ynu_e"></a>`gsl::sf::bessel_Ynu_e nu  x`
:   implements `gsl_sf_bessel_Ynu_e`.

<a name="gsl::sf::bessel_Inu"></a>`gsl::sf::bessel_Inu nu  x`
:   implements `gsl_sf_bessel_Inu`.

<a name="gsl::sf::bessel_Inu_e"></a>`gsl::sf::bessel_Inu_e nu  x`
:   implements `gsl_sf_bessel_Inu_e`.

<a name="gsl::sf::bessel_Inu_scaled"></a>`gsl::sf::bessel_Inu_scaled nu  x`
:   implements `gsl_sf_bessel_Inu_scaled`.

<a name="gsl::sf::bessel_Inu_scaled_e"></a>`gsl::sf::bessel_Inu_scaled_e nu  x`
:   implements `gsl_sf_bessel_Inu_scaled_e`.

<a name="gsl::sf::bessel_Knu"></a>`gsl::sf::bessel_Knu nu  x`
:   implements `gsl_sf_bessel_Knu`.

<a name="gsl::sf::bessel_Knu_e"></a>`gsl::sf::bessel_Knu_e nu  x`
:   implements `gsl_sf_bessel_Knu`.

<a name="gsl::sf::bessel_lnKnu"></a>`gsl::sf::bessel_lnKnu nu  x`
:   implements `gsl_sf_bessel_lnKnu`.

<a name="gsl::sf::bessel_lnKnu_e"></a>`gsl::sf::bessel_lnKnu_e nu  x`
:   implements `gsl_sf_bessel_lnKnu_e`.

<a name="gsl::sf::bessel_Knu_scaled"></a>`gsl::sf::bessel_Knu_scaled nu x`
:   implements `gsl_sf_bessel_Knu_scaled`.

<a name="gsl::sf::bessel_Knu_scaled_e"></a>`gsl::sf::bessel_Knu_scaled_e nu x`
:   implements `gsl_sf_bessel_Knu_scaled_e`.

<a name="gsl::sf::bessel_zero_J0"></a>`gsl::sf::bessel_zero_J0 s::int`
:   implements `gsl_sf_bessel_zero_J0`.

<a name="gsl::sf::bessel_zero_J0_e"></a>`gsl::sf::bessel_zero_J0_e s::int`
:   implements `gsl_sf_bessel_zero_J0_e`.

<a name="gsl::sf::bessel_zero_J1"></a>`gsl::sf::bessel_zero_J1 s::int`
:   implements `gsl_sf_bessel_zero_J1`.

<a name="gsl::sf::bessel_zero_J1_e"></a>`gsl::sf::bessel_zero_J1_e s::int`
:   implements `gsl_sf_bessel_zero_J1_e`.

<a name="gsl::sf::bessel_zero_Jnu"></a>`gsl::sf::bessel_zero_Jnu nu  s::int`
:   implements `gsl_sf_bessel_zero_Jnu`.

<a name="gsl::sf::bessel_zero_Jnu_e"></a>`gsl::sf::bessel_zero_Jnu_e nu  s::int`
:   implements `gsl_sf_bessel_zero_Jnu`.

<!-- -->
### Examples

The following illustrate the Bessel functions.

    > using gsl::sf;
    > using namespace gsl::sf;
    > bessel_J0 (-1.2);
    0.671132744264363
    > bessel_J0_e 0.75;
    [0.864242275166649,7.07329111491049e-16]
    > bessel_J1 1.2;
    0.498289057567216
    > bessel_J1_e (-0.2);
    [-0.099500832639236,5.00768737808415e-17]
    > bessel_Jn 0 (-1.2);
    0.671132744264363
    > bessel_Jn_e 2 0.75;
    [0.0670739972996506,5.48959386474892e-17]
    > bessel_Jn_array 0 4 0.5;
    [0.938469807240813,0.242268457674874,0.0306040234586826,
     0.00256372999458724,0.000160736476364288]
    > bessel_Y0 0.25;
    -0.931573024930059
    > bessel_Y0_e 0.25;
    [-0.931573024930059,6.4279898430593e-16]
    > bessel_Y1 0.125;
    -5.19993611253477
    > bessel_Y1_e 4.325;
    [0.343041276811844,2.74577716760089e-16]
    > bessel_Yn 3 4.325;
    -0.0684784962694202
    > bessel_Yn_e 3 4.325;
    [-0.0684784962694202,3.37764590906247e-16]
    > bessel_Yn_array 2 4 1.35;
    [-1.07379345815726,-2.66813016175689,-10.7845628163178]
    > bessel_I0 1.35;
    1.51022709775726
    > bessel_I0_e 1.35;
    [1.51022709775726,2.37852166449918e-15]
    > bessel_I1 0.35;
    0.177693400031422
    > bessel_I1_e 0.35;
    [0.177693400031422,1.55520651386126e-16]
    > bessel_In 2 3.0;
    2.24521244092995
    > bessel_In_e 2 3.0;
    2.24521244092995,5.98244771302867e-15]
    > bessel_In_array 3 5 (-0.1);
    [-2.08463574223272e-05,2.60546902129966e-07,-2.6052519298937e-09]
    > bessel_I0_scaled 1.05;
    0.453242541279856
    > bessel_I0_scaled_e 1.05;
    [0.453242541279856,4.10118141697477e-16]
    > bessel_I1_scaled 1.05;
    0.210226017612868
    > bessel_I1_scaled_e 1.05;
    [0.210226017612868,2.12903131803686e-16]
    > bessel_In_scaled 3 1.05;
    0.00903732602788281
    > bessel_In_scaled_e 3 1.05;
    [0.00903732602788281,2.00668948743994e-17]
    > bessel_In_scaled_array 3 5 1.05;
    [0.00903732602788281,0.0011701685245855,0.000121756316755217]
    > bessel_K0 2.3;
    0.0791399330020936
    > bessel_K0_e 2.3;
    [0.0791399330020936,1.15144454318261e-16]
    > bessel_K1 2.3;
    0.0949824438453627
    > bessel_K1_e 2.3;
    [0.0949824438453627,9.85583638959967e-17]
    > bessel_Kn 2 3.4;
    0.0366633035851529
    > bessel_Kn_e 2 3.4;
    [0.0366633035851529,2.01761856558251e-16]
    > bessel_Kn_array 1 3 2.5;
    [0.0738908163477471,0.121460206278564,0.268227146393449]
    > bessel_K0_scaled 1.5;
    0.367433609054158
    > bessel_K0_scaled_e 1.5;
    [0.958210053294896,1.25816573186951e-14]
    > bessel_K1_scaled 1.5;
    1.24316587355255
    > bessel_K1_scaled_e 1.5;
    [1.24316587355255,2.32370553362606e-15]
    > bessel_Kn_scaled 4 1.5;
    35.4899165934682
    > bessel_Kn_scaled_e 4 1.5;
    [35.4899165934682,3.89252285021454e-14]
    > bessel_Kn_scaled_array 4 6 1.5;
    [35.4899165934682,197.498093175689,1352.14387109806]
    > bessel_j0 0.01;
    0.999983333416666
    > bessel_j0_e 0.01;
    [0.999983333416666,4.44081808400239e-16]
    > bessel_j1 0.2;
    0.0664003806703222
    > bessel_j1_e 0.2;
    [0.0664003806703222,2.94876925856268e-17]
    > bessel_j2 0.3;
    0.00596152486862022
    > bessel_j2_e 0.3;
    [0.00596152486862022,2.64744886840705e-18]
    > bessel_jl 4 0.3;
    8.53642426502516e-06
    > bessel_jl_e 4 0.3;
    [8.53642426502516e-06,1.02355215483598e-19]
    > bessel_jl_array 2 1.2;
    [0.776699238306022,0.34528456985779,0.0865121863384538]
    > bessel_jl_steed_array 2 1.2;
    [0.776699238306022,0.34528456985779,0.0865121863384538]
    > bessel_y0 1;
    -0.54030230586814
    > bessel_y0_e 3;
    [0.329997498866815,2.93096657048522e-16]
    > bessel_y1 3;
    0.062959163602316
    > bessel_y1_e 3.0;
    [0.062959163602316,1.04609100698801e-16]
    > bessel_yl 3 5;
    -0.0154429099129942
    > bessel_yl_e 3 5;
    [-0.0154429099129942,2.87258769784673e-17]
    > bessel_i0_scaled 3;
    0.166253541303889
    > bessel_i0_scaled_e 3;
    [0.166253541303889,7.38314037924188e-17]
    > bessel_i1_scaled 3;
    0.111661944928148
    > bessel_i1_scaled_e 3;
    [0.111661944928148,4.95878648934625e-17]
    > bessel_i2_scaled 3;
    0.0545915963757409
    > bessel_i2_scaled_e 3;
    [0.0545915963757409,2.42435388989563e-17]
    > bessel_il_scaled 3 1;
    0.0037027398773348
    > bessel_il_scaled_e 3 1;
    [0.0037027398773348,8.46838615599053e-17]
    > bessel_il_scaled_array 3 1;
    [0.432332358381693,0.135335283236613,0.0263265086718556,0.0037027398773348]
    > bessel_k0_scaled 3;
    0.523598775598299
    > bessel_k0_scaled_e 3;
    [0.523598775598299,2.32524566533909e-16]
    > bessel_k1_scaled 4;
    0.490873852123405
    > bessel_k1_scaled_e 4;
    [0.490873852123405,2.17991781125539e-16]
    > bessel_k2_scaled 4;
    0.760854470791278
    > bessel_k2_scaled_e 4;
    [0.760854470791278,3.37887260744586e-16]
    > bessel_kl_scaled 2 4;
    0.760854470791278
    > bessel_kl_scaled_e 2 4;
    [0.760854470791278,3.37887260744586e-16]
    > bessel_kl_scaled_array 2 4;
    [0.392699081698724,0.490873852123405,0.760854470791278]
    > bessel_Jnu 2 2.3;
    0.413914591732062
    > bessel_Jnu_e 2 2.3;
    [0.413914591732062,6.43352513956959e-16]
    > bessel_sequence_Jnu_e 2 {.1,.2,.3};
    [0.00124895865879992,0.00498335415278356,0.011165861949064]
    > bessel_Ynu 1 0.5;
    -1.47147239267024
    > bessel_Ynu_e 1 0.5;
    [-1.47147239267024,8.49504515830242e-15]
    > bessel_Inu 1.2 3.4;
    5.25626563437082
    > bessel_Inu_e 1.2 3.4;
    [5.25626563437082,1.00839636820646e-13]
    > bessel_Inu_scaled 1.2 3.4;
    0.175418771999042
    > bessel_Inu_scaled_e 1.2 3.4;
    [0.175418771999042,3.15501414592188e-15]
    > bessel_Knu 3 3;
    0.122170375757184
    > bessel_Knu_e 3 3;
    [0.122170375757184,4.34036365096743e-16]
    > bessel_lnKnu 3 3;
    -2.10233868587978
    > bessel_lnKnu_e 3 3;
    [-2.10233868587978,4.24157124665032e-15]
    > bessel_Knu_scaled 3 3;
    2.45385759319062
    > bessel_Knu_scaled_e 3 3;
    [2.45385759319062,7.6281217575122e-15]
    > bessel_zero_J0 3;
    8.65372791291102
    > bessel_zero_J0_e 3;
    [8.65372791291102,2.59611837387331e-14]
    > bessel_zero_J1 3;
    10.1734681350627
    > bessel_zero_J1_e 3;
    [10.1734681350627,2.03469362701254e-13]
    > bessel_zero_Jnu 1.2 3;
    10.46769
    > bessel_zero_Jnu_e 1.2 3;
    [10.4676986203553,2.09353972407105e-14]86203553

### Clausen Functions

<a name="gsl::sf::clausen"></a>`gsl::sf::clausen x`
:   implements `gsl_sf_clausen`.

<a name="gsl::sf::clausen_e"></a>`gsl::sf::clausen_e x`
:   implements `gsl_sf_clausen_e`.

<!-- -->
### Examples

The following illustrate the Clausen functions.

    > using gsl::sf;
    > using namespace gsl::sf;
    > clausen 4.5;
    -0.831839220823219
    > clausen_e 4.5;
    [-0.831839220823219,8.60688668835964e-16]

### Colomb Functions

The results of the Coulomb wave functions are returned as a list whose
elements are ordered corresponding to the argument order of the corresponding
C functions in GSL library.

<a name="gsl::sf::hydrogenicR_1"></a>`gsl::sf::hydrogenicR_1 Z  r`
:   implements `gsl_sf_hydrogenicR_1`.

<a name="gsl::sf::hydrogenicR_1_e"></a>`gsl::sf::hydrogenicR_1_e Z  r`
:   implements `gsl_sf_hydrogenicR_1_e`.

<a name="gsl::sf::hydrogenicR"></a>`gsl::sf::hydrogenicR n::int  l::int  Z  r`
:   implements `gsl_sf_hydrogenicR_1`.

<a name="gsl::sf::hydrogenicR_e"></a>`gsl::sf::hydrogenicR_e n::int  l::int  Z  r`
:   implements `gsl_sf_hydrogenicR_1_e`.

<a name="gsl::sf::coulomb_wave_FG_e"></a>`gsl::sf::coulomb_wave_FG_e eta  x  L_F  k::int`
:   implements `gsl_sf_coulomb_wave_FG_e`.

<a name="gsl::sf::coulomb_wave_F_array"></a>`gsl::sf::coulomb_wave_F_array L_min  kmax::int  eta  x`
:   implements `gsl_sf_coulomb_wave_F_array`.

<a name="gsl::sf::coulomb_wave_FG_array"></a>`gsl::sf::coulomb_wave_FG_array L_min  kmax::int  eta  x`
:   implements `gsl_sf_coulomb_wave_FG_array`.

<a name="gsl::sf::coulomb_wave_FGp_array"></a>`gsl::sf::coulomb_wave_FGp_array L_min  kmax::int  eta x`
:   implements `gsl_sf_coulomb_wave_FGp_array`.

<a name="gsl::sf::coulomb_wave_sphF_array"></a>`gsl::sf::coulomb_wave_sphF_array L_min  kmax::int  eta  x`
:   implements `gsl_sf_coulomb_wave_sphF_array`.

<a name="gsl::sf::coulomb_CL_e"></a>`gsl::sf::coulomb_CL_e L  eta`
:   implements `gsl_sf_coulomb_wave_CL_e`.

<a name="gsl::sf::coulomb_CL_array"></a>`gsl::sf::coulomb_CL_array Lmin  kmax  eta`
:   implements `gsl_sf_coulomb_wave_CL_array`.

<!-- -->
### Examples

The following illustrate the Coulomb functions.

    > using gsl::sf;
    > using namespace gsl::sf;
    > hydrogenicR_1 0.2 4;
    0.0803784086420537
    > hydrogenicR_1_e 0.2 4;
    [0.0803784086420537,2.85561471862841e-17]
    > hydrogenicR 3 1 0.25 3.2;
    0.00802954301593587
    > hydrogenicR_e 3 1 0.25 3.2;
    [0.00802954301593587,3.90138748076797e-17]
    > coulomb_wave_F_array 1 2 0.5 0.5;
    [{0.0387503306520188,0.0038612830533923,0.000274978904710252},0.0]
    > coulomb_wave_FG_array 1 2 0.5 0.5;
    [{0.0387503306520188,0.0038612830533923,0.000274978904710252},
     {4.13731494044202,25.4479852847406,257.269816591168},0.0,0.0]
    > coulomb_wave_FGp_array 1 2 0.5 0.5;
    [{0.0387503306520188,0.0038612830533923,0.000274978904710252},
     {4.13731494044202,25.4479852847406,257.269816591168},0.0,0.0]
    > coulomb_wave_sphF_array 1 2 0.5 0.5;
    [{0.0775006613040376,0.0077225661067846,0.000549957809420504},0.0]
    > coulomb_CL_e (-0.5) 3;
    [0.000143036170217949,2.92195771135514e-18]
    > coulomb_CL_array (-0.5) 4 1.5;
    [0.0159218263353144,0.0251746178646226,0.00890057150292734,
     0.00172996014234001,0.000235267570111599]

### Coupling Coefficients

<a name="gsl::sf::coupling_3j"></a>`gsl::sf::coupling_3j m::matrix`
:   implements `gsl_sf_coupling_3j` except the input is a 2x3 (row by column)
    integer matrix instead of six integer arguments.

<a name="gsl::sf::coupling_3j_e"></a>`gsl::sf::coupling_3j_e m::matrix`
:   implements `gsl_sf_coupling_3j_e` except the input is a 2x3 (row by
    column) integer matrix instead of six integer arguments.

<a name="gsl::sf::coupling_6j"></a>`gsl::sf::coupling_6j m::matrix`
:   implements `gsl_sf_coupling_6j` except the input is a 2x3 (row by column)
    integer matrix instead of six integer arguments.

<a name="gsl::sf::coupling_6j_e"></a>`gsl::sf::coupling_6j_e m::matrix`
:   implements `gsl_sf_coupling_6j_e` except the input is a 2x3 (row by
    column) integer matrix instead of six integer arguments.

<a name="gsl::sf::coupling_9j"></a>`gsl::sf::coupling_9j m::matrix`
:   implements `gsl_sf_coupling_9j` except the input is a 3x3 integer matrix
    instead of six integer arguments.

<a name="gsl::sf::coupling_9j_e"></a>`gsl::sf::coupling_9j_e m::matrix`
:   implements `gsl_sf_coupling_9j_e` except the input is a 3x3 integer matrix
    instead of six integer arguments.

<!-- -->
### Examples

The following illustrate the coupling coefficient functions.

    > using gsl::sf;
    > using namespace gsl::sf;
    > coupling_3j {6,4,2;0,0,0};
    -0.29277002188456
    > coupling_3j_e {6,4,2;0,0,0};
    [-0.29277002188456,1.300160076865e-16]
    > coupling_6j {1,2,3;2,1,2};
    -0.166666666666667
    > coupling_6j_e {1,2,3;2,1,2};
    [-0.166666666666667,2.22044604925031e-16]
    > coupling_9j {1,2,3;2,1,2;1,1,1};
    -0.0962250448649376
    > coupling_9j_e {1,2,3;2,1,2;1,1,1};
    [-0.0962250448649376,4.84948508304183e-16]

### Dawson Function

<a name="gsl::sf::dawson"></a>`gsl::sf::dawson x`
:   implements `gsl_sf_dawson`.

<a name="gsl::sf::dawson_e"></a>`gsl::sf::dawson_e x`
:   implements `gsl_sf_dawson_e`.

<!-- -->
### Examples

The following illustrate the dawson functions.

    > dawson 3;/**-
    0.178271030610558
    > dawson_e 3;
    [0.178271030610558,8.9920386788099e-16]

### Debye Functions

<a name="gsl::sf::debye_1"></a>`gsl::sf::debye_1 x`
:   implements `gsl_sf_debye_1`.

<a name="gsl::sf::debye_1_e"></a>`gsl::sf::debye_1_e x`
:   implements `gsl_sf_debye_1_e`.

<a name="gsl::sf::debye_2"></a>`gsl::sf::debye_2 x`
:   implements `gsl_sf_debye_2`.

<a name="gsl::sf::debye_2_e"></a>`gsl::sf::debye_2_e x`
:   implements `gsl_sf_debye_2_e`.

<a name="gsl::sf::debye_3"></a>`gsl::sf::debye_3 x`
:   implements `gsl_sf_debye_3`.

<a name="gsl::sf::debye_3_e"></a>`gsl::sf::debye_3_e x`
:   implements `gsl_sf_debye_3_e`.

<a name="gsl::sf::debye_4"></a>`gsl::sf::debye_4 x`
:   implements `gsl_sf_debye_4`.

<a name="gsl::sf::debye_4_e"></a>`gsl::sf::debye_4_e x`
:   implements `gsl_sf_debye_4_e`.

<a name="gsl::sf::debye_5"></a>`gsl::sf::debye_5 x`
:   implements `gsl_sf_debye_5`.

<a name="gsl::sf::debye_5_e"></a>`gsl::sf::debye_5_e x`
:   implements `gsl_sf_debye_5_e`.

<a name="gsl::sf::debye_6"></a>`gsl::sf::debye_6 x`
:   implements `gsl_sf_debye_6`.

<a name="gsl::sf::debye_6_e"></a>`gsl::sf::debye_6_e x`
:   implements `gsl_sf_debye_6_e`.

<!-- -->
### Examples

The following illustrate the debye functions.

    > debye_1 0.4;
    0.904437352623294
    > debye_1_e 0.4;
    [0.904437352623294,3.84040456356756e-16]
    > debye_2 1.4;
    0.613281386045505
    > debye_2_e 1.4;
    [0.613281386045505,5.15090106564116e-16]
    > debye_3 2.4;
    0.370136882985216
    > debye_3_e 2.4;
    [0.370136882985216,6.0792125556598e-16]
    > debye_4 3.4;
    0.205914922541978
    > debye_4_e 3.4;
    [0.205914922541978,7.42872979584512e-16]
    > debye_5 4.4;
    0.107477287722471
    > debye_5_e 4.4;
    [0.107477287722471,2.38647518907499e-17]
    > debye_6 5.4;
    0.0533132925698824
    > debye_6_e 5.4;
    [0.0533132925698824,1.18379289859322e-17]

### Dilogarithm

<a name="gsl::sf::dilog"></a>`gsl::sf::dilog x`
:   implements `gsl_sf_dilog`.

<a name="gsl::sf::dilog"></a>`gsl::sf::dilog (r<:theta)`
:   implements `gsl_sf_complex_dilog_e` except that results are returned as
    the complex value `re+:im` and the error values are not returned.

<a name="gsl::sf::dilog_e"></a>`gsl::sf::dilog_e x`
:   implements `gsl_sf_dilog_e`.

<a name="gsl::sf::dilog_e"></a>`gsl::sf::dilog_e (r<:theta)`
:   implements `gsl_sf_complex_dilog_e` except the results are returned as the
    list `[re+:im, re_error, im_error]`.

<!-- -->
### Examples

The following illustrate the dilog functions.

    > dilog 1.0;
    1.64493406684823
    > dilog (1<:2);
    -0.496658586741567+:0.727146050863279
    > dilog_e (1%3);
    [0.366213229977064,8.22687466397711e-15]
    > dilog_e (1<:3);
    [-0.817454913536463+:0.0980262093913011,3.8224192909699e-15,
     1.47247478976757e-15]

<a name="gsl::sf::multiply_e"></a>`gsl::sf::multiply_e x  y`
:   implements `gsl_sf_multiply_e`.

<a name="gsl::sf::multiply_err_e"></a>`gsl::sf::multiply_err_e x  dx  y  dy`
:   implements `gsl_sf_multiply_err_e`.

<!-- -->
### Examples

The following illustrate the multiply functions.

    > multiply_e 10.0 11.0;
    [110.0,4.88498130835069e-14]
    > multiply_err_e 10.0 0.04 11.0 0.002;
    [110.0,0.460000000000049]

Matrices
--------

<a name="module-gslmatrix"></a>

This module is loaded via the command `using gsl::matrix` and provides
wrappers for many of the GSL matrix, BLAS, and linear algebra routines found
in Chapters 8, 12, and 13, respectively of the GSL Reference Manual:

-   [Vectors and
    Matrices](http://www.gnu.org/software/gsl/manual/html_node/Vectors-and-Matrices.html)
-   [BLAS
    Support](http://www.gnu.org/software/gsl/manual/html_node/BLAS-Support.html)
-   [Linear
    Algebra](http://www.gnu.org/software/gsl/manual/html_node/Linear-Algebra.html)

It also contains some general utility functions for creating various types of
matrices.

### Matrix Creation

The utility functions `zeros` and `ones` create matrices with all elements
zero or one, respectively, and `eye` creates identity matrices. These
functions can be invoked either with a pair *(n,m)* denoting the desired
number of rows or columns, or an integer *n* in which case a square *n* x *n*
matrix is created. The result is always a double matrix. Analogous functions
`izeros`, `czeros`, etc. are provided to create integer and complex matrices,
respectively.

<a name="gsl::matrix::zeros"></a>`gsl::matrix::zeros (n :: int, m :: int)`
:   creates an *n* x *m* double matrix with all of its entries being zero.

<a name="gsl::matrix::zeros"></a>`gsl::matrix::zeros n :: int`
:   creates an *n* x *n* double matrix with all of its entries being zero.

<a name="gsl::matrix::izeros"></a>`gsl::matrix::izeros (n :: int, m :: int)`
:   creates an *n* x *m* integer matrix with all of its entries being zero.

<a name="gsl::matrix::izeros"></a>`gsl::matrix::izeros n :: int`
:   creates an nx\`n\` integer matrix with all of its entries being zero.

<a name="gsl::matrix::czeros"></a>`gsl::matrix::czeros (n :: int, m :: int)`
:   creates an *n* x *m* complex matrix with all of its entries being zero.

<a name="gsl::matrix::czeros"></a>`gsl::matrix::czeros n :: int`
:   creates an *n* x *n* complex matrix with all of its entries being zero.

<a name="gsl::matrix::ones"></a>`gsl::matrix::ones (n :: int, m :: int)`
:   creates an *n* x *m* double matrix with all of its entries being one.

<a name="gsl::matrix::ones"></a>`gsl::matrix::ones n :: int`
:   creates an *n* x *n* double matrix with all of its entries being one.

<a name="gsl::matrix::iones"></a>`gsl::matrix::iones (n :: int, m :: int)`
:   creates an *n* x *m* integer matrix with all of its entries being one.

<a name="gsl::matrix::iones"></a>`gsl::matrix::iones n :: int`
:   creates an *n* x *n* integer matrix with all of its entries being one.

<a name="gsl::matrix::cones"></a>`gsl::matrix::cones (n :: int, m :: int)`
:   creates an *n* x *m* complex matrix with all of its entries being one.

<a name="gsl::matrix::cones"></a>`gsl::matrix::cones n :: int`
:   creates an *n* x *n* complex matrix with all of its entries being one.

<a name="gsl::matrix::eye"></a>`gsl::matrix::eye (n :: int, m :: int)`
:   creates an *n* x *m* identity matrix with double entries.

<a name="gsl::matrix::eye"></a>`gsl::matrix::eye n :: int`
:   creates an *n* x *n* identity matrix with double entries.

<a name="gsl::matrix::ieye"></a>`gsl::matrix::ieye (n :: int, m :: int)`
:   creates an *n* x *m* identity matrix with integer entries.

<a name="gsl::matrix::ieye"></a>`gsl::matrix::ieye n :: int`
:   creates an *n* x *n* identity matrix with integer entries.

<a name="gsl::matrix::ceye"></a>`gsl::matrix::ceye (n :: int, m :: int)`
:   creates an *n* x *m* identity matrix with complex entries.

<a name="gsl::matrix::ceye"></a>`gsl::matrix::ceye n :: int`
:   creates an *n* x *n* identity matrix with complex entries.

<!-- -->
### Matrix Operators and Functions

The following operations are defined for constant `a` and matrices `x` and
`y`. Some operators are not defined in the GSL library but are provided here
for convenience.

<a name="+/gsl_matrix"></a>`a + x`, <a name="+/gsl_matrix"></a>`x + a`
:   returns a matrix with entries `a + x!(i,j)`.

<a name="+/gsl_matrix"></a>`x + y`
:   adds matrix `x` to matrix `y`.

<a name="-/gsl_matrix"></a>`- x`
:   returns a matrix with entries `- x!(i,j)`. Note that `neg x` is equivalent
    to `- x`.

<a name="-/gsl_matrix"></a>`a - x`
:   returns a matrix with entries `a - x!(i,j)`.

<a name="-/gsl_matrix"></a>`x - a`
:   returns a matrix with entries `x!(i,j) - a`.

<a name="-/gsl_matrix"></a>`x - y`
:   subtracts matrix `y` from matrix `x`.

<a name="*/gsl_matrix"></a>`a * x`, <a name="*/gsl_matrix"></a>`x * a`
:   returns a matrix with entries `a * x!(i,j)`.

<a name=".*/gsl_matrix"></a>`x .* y`
:   multiplies, element-wise, matrix `x` to matrix `y`.

<a name="*/gsl_matrix"></a>`x * y`
:   multiplies matrix `x` to matrix `y`.

<a name="//gsl_matrix"></a>`a / x`
:   returns a matrix with entries `a / x!(i,j)`. Note that matrix `x` must not
    have any zero entries.

<a name="//gsl_matrix"></a>`x / a`
:   returns a matrix with entries `x!(i,j) / a`. Note that `a` must be
    nonzero.

<a name=".//gsl_matrix"></a>`x ./ y`
:   divides, element-wise, matrix *x* by matrix *y*.

<a name="//gsl_matrix"></a>`x / y`
:   right divides matrix *x* by matrix *y*.

<a name="\/gsl_matrix"></a>`x \ y`
:   left divides matrix *x* by matrix *y*.

<a name="div/gsl_matrix"></a>`a div x`
:   returns an integer matrix with entries `a div x!(i,j)`. Note that *a* must
    be an integer and matrix `x` must be an integer matrix with nonzero
    entries.

<a name="div/gsl_matrix"></a>`x div a`
:   returns an integer matrix with entries `x!(i,j) div a`. Note that `a` must
    be a nonzero integer and matrix `x` must have integer entries.

<a name="div/gsl_matrix"></a>`x div y`
:   computes the quotient integer matrix `x` by integer matrix `y`.

<a name="mod/gsl_matrix"></a>`a mod x`
:   returns an integer matrix with entries `a mod x!(i,j)`. Note that `a` must
    be an integer and matrix `x` must be an integer matrix with nonzero
    entries.

<a name="mod/gsl_matrix"></a>`x mod a`
:   returns an integer matrix with entries `a mod x!(i,j)`. Note that `a` must
    be an integer and matrix `x` must be an integer matrix with nonzero
    entries.

<a name="mod/gsl_matrix"></a>`x mod y`
:   returns the remainder integer matrix `x` mod integer matrix `y`.

<a name="not/gsl_matrix"></a>`not x`
:   returns a matrix with integer entries `not x!(i,j)`. Note that `x` must be
    a matrix with integer entries and `not` is the bitwise negation operation.

<a name="^/gsl_matrix"></a>`a ^ x`
:   returns a matrix with entries `a ^ x!(i,j)`. Note that `0^0` is defined
    as 1.

<a name="^/gsl_matrix"></a>`x ^ a`
:   returns a matrix with entries `x!(i,j) ^ a`. Note that `0^0` is defined
    as 1.

<a name=".^/gsl_matrix"></a>`x .^ y`
:   returns a matrix with entries `x!(i,j) ^ y!(i,j)`.

<a name="^/gsl_matrix"></a>`x ^ y`
:   returns a matrix with entries `x!(i,j) ^ y!(i,j)`.

<a name="<</gsl_matrix"></a>`x << a`
:   returns an integer matrix with entries `x!(i,j) << a`. Note that `a` must
    be an integer and matrix `x` must have integer entries.

<a name="<</gsl_matrix"></a>`x << y`
:   returns an integer matrix with entries `x!(i,j) << y!(i,j)`. Note that `x`
    and `y` must have integer entries.

<a name="&gt;&gt;/gsl_matrix"></a>`x >> a`
:   returns an integer matrix with entries `x!(i,j) >> a`. Note that `a` must
    be an integer and matrix `x` must have integer entries.

<a name="&gt;&gt;/gsl_matrix"></a>`x >> y`
:   returns an integer matrix with entries `x!(i,j) >> y!(i,j)`. Note that `x`
    and `y` must have integer entries.

<a name="and/gsl_matrix"></a>`x and a`, <a name="and/gsl_matrix"></a>`a and x`
:   returns an integer matrix with entries `a and x!(i,j)`. Note that `a` must
    be an integer, matrix `x` must have integer entries, and `and` is a
    bitwise operator.

<a name="and/gsl_matrix"></a>`x and y`
:   returns an integer matrix with entries `x!(i,j) and y!(i,j)`. Note that
    `x` and `y` must be matrices with integer entries.

<a name="or/gsl_matrix"></a>`x or a`, <a name="or/gsl_matrix"></a>`a or x`
:   returns an integer matrix with entries `a or x!(i,j)`. Note that `a` must
    be an integer, matrix `x` must have integer entries, and `or` is a bitwise
    operator.

<a name="or/gsl_matrix"></a>`x or y`
:   returns an integer matrix with entries `x!(i,j) or y!(i,j)`. Note that `x`
    and `y` must be matrices with integer entries.

<!-- -->
The `pow` function computes powers of matrices by repeated matrix
multiplication.

<a name="pow/gsl_matrix"></a>`pow x :: matrix  k :: int`, <a name="pow/gsl_matrix"></a>`pow x :: matrix  k :: bigint`
:   Raises matrix `x` to the `k` th power. Note `x` must be a square matrix
    and `k` a nonnegative integer.

<!-- -->
### Singular Value Decomposition

For a given *n* x *m* matrix `x`, these functions yield a singular-value
decomposition `u`, `s`, `v` of the matrix such that `x == u*s*transpose v`,
where `u` and `v` are orthogonal matrices of dimensions *n* x *m* and *n* x
*n*, respectively, and *s* is a *n* x *n* diagonal matrix which has the
singular values in its diagonal, in descending order. Note that GSL implements
this only for double matrices right now. Also, GSL only handles the case of
square or overdetermined systems, but we work around that in our wrapper
functions by just adding a suitable number of zero rows in the underdetermined
case.

<a name="gsl::matrix::svd"></a>`gsl::matrix::svd x`
:   singular-value decomposition of matrix `x`.

<a name="gsl::matrix::svd_mod"></a>`gsl::matrix::svd_mod x`
:   This uses the modified Golub-Reinsch algorithm, which is faster if `n > m`
    but needs *O(m\^2)* extra memory as internal workspace.

<a name="gsl::matrix::svd_jacobi"></a>`gsl::matrix::svd_jacobi x`
:   This uses one-sided Jacobi orthogonalization which provides better
    relative accuracy but is slower.

<a name="gsl::matrix::svd_solve"></a>`gsl::matrix::svd_solve (u, s, v)  b`
:   Solve the system `Ax=b`, using the SVD of `A`. `svd_solve` takes the
    result `(u,s,v)` of a `svd` call, and a column vector `b` of the
    appropriate dimension. The result is another column vector solving the
    system (possibly in the least-squares sense).

<a name="gsl::matrix::pinv"></a>`gsl::matrix::pinv x`
:   Computes the pseudo inverse of a matrix from its singular value
    decomposition.

<!-- -->
Least-Squares Fitting
---------------------

<a name="module-gslfit"></a>

This module is loaded via the command `using gsl::fit` and provides Pure
wrappers for the GSL least-squares fitting routines found in Chapter 36 of the
GSL manual,

<http://www.gnu.org/software/gsl/manual/html_node/Least_002dSquares-Fitting.html>.

### Routines

<a name="gsl::fit::linear"></a>`gsl::fit::linear x::matrix  y::matrix`
:   implements `gsl_fit_linear` without the `xstride`, `ystride`, and `n`
    parameters. Results are returned as a list
    `[c0, c1, cov00, cov01, cov11, sumsq]`.

<a name="gsl::fit::wlinear"></a>`gsl::fit::wlinear x::matrix  w::matrix  y::matrix`
:   implements `gsl_fit_wlinear` without the `xstride`, `wstride`, `ystride`,
    and `n` parameters. Results are given as a list
    `[c0, c1, cov00, cov01, cov11, chisq]`.

<a name="gsl::fit::linear_est"></a>`gsl::fit::linear_est x  c0::double  c1::double  cov00::double  cov01::double  cov11::double`
:   implements `gsl_fit_linear_est`. Results are returned as a list
    `[y, y_err]`.

<a name="gsl::fit::mul"></a>`gsl::fit::mul x::matrix  y::matrix`
:   implements `gsl_fit_mul` omitting the parameters `xstride`, `ystride`, and
    `n`. Results are returned as a list `[c1, cov11, sumsq]`.

<a name="gsl::fit::wmul"></a>`gsl::fit::wmul x::matrix  w::matrix  y::matrix`
:   implements `gsl_fit_wmul` omitting the parametrs `xstride`, `ystride`, and
    `n`. Results are returned as a list `[c1, cov11, sumsq]`.

<a name="gsl::fit::mul_est"></a>`gsl::fit::mul_est x  c1::double  cov11::double`
:   implements `gsl_fit_mul_est`. Results are returned as a list `[y, y_err]`.

<!-- -->
### Examples

Usage of each implemented library routine is illustrated below.

    > using gsl::fit;
    > using namespace gsl::fit;

The following code determines the equation for the least-squares line through
the points (1,0.01), (2,1.11), (3,1.9), (4,2.85), and (5,4.01).

    > Y x = '(a + b * x)
    > when
    >   a:b:_ = linear {1,2,3,4,5} {0.01,1.11,1.9,2.85,4.01}
    > end;
    > Y x;
    -0.946+0.974*x
    > eval $ Y 2;
    1.002

The following code illustrates estimating y-values without constructing an
equation for the least-squares line determined by the points
`{x1,x2,x3,...,xn}`, `{y1,y2,y3,...,yn}`. Here we estimate the *y*-value at
*x* = 1, *x* = 2, and *x* = 3. Compare the output above at *x* = 2 to the
output at *x* = 2 below.

    > let c0:c1:cov00:cov01:cov11:_ = linear {1,2,3,4,5}
    >   {0.01,1.11,1.9,2.85,4.01};
    > linear_est 1 c0 c1 cov00 cov01 cov11;
    [0.028,0.0838570211729465]
    > linear_est 2 c0 c1 cov00 cov01 cov11;
    [1.002,0.0592958683214944]
    > linear_est 3 c0 c1 cov00 cov01 cov11;
    [1.976,0.0484148737476408]

Next, we determine a least-squares line through the points (1,0.01), (2,1.11),
(3,1.9), (4,2.85), and (5,4.01) using weights 0.1, 0.2, 0.3, 0.4, and 0.5.

    > W x = '(a + b * x)
    > when
    >   a:b:_ = wlinear (matrix (1..5)) 
    >           {0.1, 0.2, 0.3, 0.4, 0.5}
    >           {0.01, 1.11, 1.9, 2.85, 4.01};
    > end;
    > W u;
    -0.99+0.986*u
    > eval $ W 2;
    0.982

The least-squares slope for `Y = c1 * X` using the points (1,3), (2,5), and
(3,7) is calculated below. Also, the *y*-values and standard error about *x* =
1, 2, and 3 are given.

    > let c1:cov11:sumsq:_ = mul {1,2,3} {3,5,7};
    > mul_est 1 c1 cov11;
    [2.42857142857143,0.123717914826348]
    > mul_est 2 c1 cov11;
    [4.85714285714286,0.247435829652697]
    > mul_est 3 c1 cov11;
    [7.28571428571428,0.371153744479045]

The least-squares slope for `Y = c1 * X` using the points (1,3), (2,5), and
(3,7), and weights 0.4, 0.9, and 0.4 is calculated below. The approximation of
y-values and standard error about *x* = 1, 2, and 3 follows.

    > let c1:cov11:sumsq:_ = wmul {1,2,3} {0.4,0.9,0.4} {3,5,7};
    > mul_est 1 c1 cov11;
    [2.44736842105263,0.362738125055006]
    > mul_est 2 c1 cov11;
    [4.89473684210526,0.725476250110012]
    > mul_est 3 c1 cov11;
    [7.34210526315789,1.08821437516502]

Statistics
----------

<a name="module-gslstats"></a>

This module is loaded via the command `using gsl::stats` and provides Pure
wrappers for the GSL Statistics routines found in Chapter 20 of the GSL
manual,

<http://www.gnu.org/software/gsl/manual/html_node/Statistics.html>.

### Routines

<a name="gsl::stats::mean"></a>`gsl::stats::mean data::matrix`
:   implements `gsl_stats_mean` without `stride` and `n` arguments.

<a name="gsl::stats::variance"></a>`gsl::stats::variance data::matrix`
:   implements `gsl_stats_variance` without `stride` and `n` arguments.

<a name="gsl::stats::variance"></a>`gsl::stats::variance data::matrix  mean`
:   implements `gsl_stats_variance_m` without `stride` and `n` arguments.

<a name="gsl::stats::sd"></a>`gsl::stats::sd data::matrix`
:   implements `gsl_stats_sd` without `stride` and `n` arguments.

<a name="gsl::stats::sd_m"></a>`gsl::stats::sd_m data::matrix  mean`
:   implements `gsl_stats_sd_m` without `stride` and `n` arguments.

<a name="gsl::stats::tss"></a>`gsl::stats::tss data::matrix`
:   implements `gsl_stats_tss` without `stride` and `n` arguments.

<a name="gsl::stats::tss_m"></a>`gsl::stats::tss_m data::matrix  mean`
:   implements `gsl_stats_tss_m` without `stride` and `n` arguments.

<a name="gsl::stats::variance_with_fixed_mean"></a>`gsl::stats::variance_with_fixed_mean data::matrix  mean`
:   implements `gsl_stats_variance_with_fixed_mean` without `stride` and `n`
    arguments.

<a name="gsl::stats::sd_with_fixed_mean"></a>`gsl::stats::sd_with_fixed_mean data::matrix  mean`
:   implements `gsl_stats_sd_with_fixed_mean` without `stride` and `n`
    arguments.

<a name="gsl::stats::absdev"></a>`gsl::stats::absdev data::matrix`
:   implements `gsl_stats_absdev` without `stride` and `n` arguments.

<a name="gsl::stats::absdev_m"></a>`gsl::stats::absdev_m data::matrix  mean`
:   implements `gsl_stats_absdev_m` without `stride` and `n` arguments.

<a name="gsl::stats::skew"></a>`gsl::stats::skew data::matrix  mean`
:   implements `gsl_stats_skew` without `stride` and `n` arguments.

<a name="gsl::stats::skew_m_sd"></a>`gsl::stats::skew_m_sd data::matrix  mean  sd`
:   implements `gsl_stats_skew_m_sd` without `stride` and `n` arguments.

<a name="gsl::stats::kurtosis"></a>`gsl::stats::kurtosis data::matrix`
:   implements `gsl_stats_kurtosis` without `stride` and `n` arguments.

<a name="gsl::stats::kurtosis_m_sd"></a>`gsl::stats::kurtosis_m_sd data::matrix  mean  sd`
:   implements `gsl_stats_kurtosis_m_sd` without `stride` and `n` arguments.

<a name="gsl::stats::lag1_autocorrelation"></a>`gsl::stats::lag1_autocorrelation data::matrix`
:   implements `gsl_stats_lag1_autocorrelation` without `stride` and `n`
    arguments.

<a name="gsl::stats::lag1_autocorrelation_m"></a>`gsl::stats::lag1_autocorrelation_m data::matrix  mean`
:   implements `gsl_stats_lag1_autocorrelation_m` without `stride` and `n`
    arguments.

<a name="gsl::stats::covariance"></a>`gsl::stats::covariance d1::matrix  d2::matrix`
:   implements `gsl_stats_covariance` without `stride1`, `stride2`, and `n`
    arguments.

<a name="gsl::stats::covariance_m"></a>`gsl::stats::covariance_m d1::matrix  d2::matrix  mean1  mean2`
:   implements `gsl_stats_covariance_m` without `stride1`, `stride2`, and `n`
    arguments.

<a name="gsl::stats::correlation"></a>`gsl::stats::correlation d1::matrix  d2::matrix`
:   implements `gsl_stats_correlation` without `stride1`, `stride2`, and `n`
    arguments.

<a name="gsl::stats::wmean"></a>`gsl::stats::wmean weight::matrix  data::matrix`
:   implements `gsl_stats_wmean` without `stride` and `n` arguments.

<a name="gsl::stats::wvariance"></a>`gsl::stats::wvariance weight::matrix  data::matrix`
:   implements `gsl_stats_wvariance` without `stride` and `n` arguments.

<a name="gsl::stats::wvariance_m"></a>`gsl::stats::wvariance_m weight::matrix  data::matrix  mean`
:   implements `gsl_stats_wvariance_m` without `stride` and `n` arguments.

<a name="gsl::stats::wsd"></a>`gsl::stats::wsd weight::matrix  data::matrix`
:   implements `gsl_stats_wsd` without `stride` and `n` arguments.

<a name="gsl::stats::wsd_m"></a>`gsl::stats::wsd_m weight::matrix  data::matrix  mean`
:   implements `gsl_stats_wsd_m` without `stride` and `n` arguments.

<a name="gsl::stats::wvariance_with_fixed_mean"></a>`gsl::stats::wvariance_with_fixed_mean weight::matrix  data::matrix  mean`
:   implements `gsl_stats_wvariance_with_fixed_mean` without `stride` and `n`
    arguments.

<a name="gsl::stats::wsd_with_fixed_mean"></a>`gsl::stats::wsd_with_fixed_mean weight::matrix  data::matrix  mean`
:   implements `gsl_stats_wsd_with_fixed_mean` without `stride` and `n`
    arguments.

<a name="gsl::stats::wtss"></a>`gsl::stats::wtss weight::matrix  data::matrix`
:   implements `gsl_stats_wtss` without `stride` and `n` arguments.

<a name="gsl::stats::wtss_m"></a>`gsl::stats::wtss_m weight::matrix  data::matrix  mean`
:   implements `gsl_stats_wtss_m` without `stride` and `n` arguments.

<a name="gsl::stats::wabsdev"></a>`gsl::stats::wabsdev weight::matrix  data::matrix`
:   implements `gsl_stats_wabsdev` without `stride` and `n` arguments.

<a name="gsl::stats::wabsdev_m"></a>`gsl::stats::wabsdev_m weight::matrix  data::matrix  mean`
:   implements `gsl_stats_wabsdev_m` without `stride` and `n` arguments.

<a name="gsl::stats::wskew"></a>`gsl::stats::wskew weight::matrix  data::matrix`
:   implements `gsl_stats_wskew` without `stride` and `n` arguments.

<a name="gsl::stats::wskew_m_sd"></a>`gsl::stats::wskew_m_sd weight::matrix  data::matrix  mean  sd`
:   implements `gsl_stats_wskew_m_sd` without `stride` and `n` arguments.

<a name="gsl::stats::wkurtosis"></a>`gsl::stats::wkurtosis weight::matrix  data::matrix`
:   implements `gsl_stats_wkurtosis` without `stride` and `n` arguments.

<a name="gsl::stats::wkurtosis_m_sd"></a>`gsl::stats::wkurtosis_m_sd weight::matrix  data::matrix`
:   implements `gsl_stats_wkurtosis_m_sd` without `stride` and `n` arguments.

<a name="gsl::stats::max"></a>`gsl::stats::max data::matrix`
:   implements `gsl_stats_max` without `stride` and `n` arguments.

<a name="gsl::stats::min"></a>`gsl::stats::min data::matrix`
:   implements `gsl_stats_min` without `stride` and `n` arguments.

<a name="gsl::stats::minmax"></a>`gsl::stats::minmax data::matrix`
:   implements `gsl_stats_minmax` without `stride` and `n` arguments. Results
    are returned as a list `[min, max]`.

<a name="gsl::stats::min_index"></a>`gsl::stats::min_index data::matrix`
:   implements `gsl_stats_min_index` without `stride` and `n` arguments.

<a name="gsl::stats::max_index"></a>`gsl::stats::max_index data::matrix`
:   implements `gsl_stats_max_index` without `stride` and `n` arguments.

<a name="gsl::stats::minmax_index"></a>`gsl::stats::minmax_index data::matrix`
:   implements `gsl_stats_minmax_index` without `stride` and `n` arguments.
    Results are returned as a list `[min_index, max_index]`.

<a name="gsl::stats::median_from_sorted_data"></a>`gsl::stats::median_from_sorted_data data::matrix`
:   implements `gsl_stats_median_from_sorted_data` without `stride` and `n`
    arguments.

<a name="gsl::stats::quantile_from_sorted_data"></a>`gsl::stats::quantile_from_sorted_data data::matrix  f::double`
:   implements `gsl_stats_quantile_from_sorted_data` without `stride` and `n`
    arguments.

<!-- -->
### Examples

The following illustrates the use of each function in the `stats` module.

    > using gsl::stats;
    > using namespace gsl::stats;
    > mean {1,2,3,4,5};
    3.0
    > variance {1,2,3,4,5};
    2.5
    > variance_m {1,2,3,4,5}  4;
    3.75
    > sd {1,2,3,4,5};
    1.58113883008419
    > sd_m {1,2,3,4,5} 4;
    1.93649167310371
    > tss {1,2,3,4,5};
    10.0
    > tss_m {1,2,3,4,5} 4;
    15.0
    > variance_with_fixed_mean {0.0,1.2,3.4,5.6,6.0} 4.1;
    6.314
    > sd_with_fixed_mean {0.0,1.2,3.4,5.6,6.0} 4.1;
    2.51276739870606
    > absdev {2,2,3,4,4};
    0.8
    > absdev_m {2,2,3,4,4} 4;
    1.0
    > skew {1,1,1,1,2,2,2,2,2,2,2,2,3,30};
    2.94796699504537
    > skew_m_sd {1,2,2,3,3,3,3,3,3,3,4,4,5} 3 1;
    0.0
    > kurtosis  {1,2,2,3,3,3,3,3,3,3,4,4,5};
    -0.230769230769231
    > kurtosis_m_sd {1,2,2,3,3,3,3,3,3,3,4,4,5} 3 1;
    -0.230769230769231
    > lag1_autocorrelation {1,2,3,4,5};
    0.4
    > lag1_autocorrelation_m {1,2,3,4,5} 2.5;
    0.444444444444444
    > covariance {1,2,3,4,5} {3.0,4.5,6.0,7.5,9.0};
    3.75
    > covariance_m {1,2,3,4,5} {3.0,4.5,6.0,7.5,9.0} 3 6;
    3.75
    > correlation {1,2,3,4} {2,3,4,5};
    1.0
    > wmean {0.4,0.2,0.3,0.3,0.3} {2,3,4,5,6};
    3.93333333333333
    > wvariance {0.4,0.2,0.3,0.3,0.3} {2,3,4,5,6};
    2.7752808988764
    > wvariance_m {0.4,0.2,0.3,0.3,0.3} {2,3,4,5,6} 3.0;
    3.87640449438202
    > wsd {0.4,0.2,0.3,0.3,0.3} {2,3,4,5,6};
    1.66591743459164
    > wsd_m {0.4,0.2,0.3,0.3,0.3} {2,3,4,5,6} 3.0;
    1.96885867811329
    > wvariance_with_fixed_mean {1,2,3,4} {1,2,3,4} 2.5;
    1.25
    > wsd_with_fixed_mean {1,2,3,4} {1,2,3,4} 2.5;
    1.11803398874989
    > wtss {1,1,2,2} {2,3,4,5};
    6.83333333333333
    > wtss_m {1,1,2,2} {2,3,4,5} 3.1;
    10.06
    > wabsdev {1,1,2,2} {2,3,4,5};
    0.888888888888889
    > wabsdev_m {1,1,2,2} {2,3,4,5} 3.1;
    1.13333333333333
    > wskew {1,1,2,2} {2,3,4,5};
    -0.299254338484713
    > wskew_m_sd {1,1,2,2} {2,3,4,5} 3.1 1.2;
    1.33526234567901
    > wkurtosis {1,1,2,2} {2,3,4,5};
    -1.96206512878137
    > wkurtosis_m_sd {1,1,2,2} {2,3,4,5} 3.1 1.2;
    -0.681921939300412
    > min {9,4,2,1,9};
    1
    > max {9.1,4.2,2.6,1.1,9.2};
    9.2
    > minmax {9.0,4.0,2.0,1.0,9.0};
    [1.0,9.0]
    > min_index {9.1,4.2,2.6,1.1,9.2};
    3
    > max_index {9,4,2,1,9};
    0
    > minmax_index {9,4,2,1,0,9};
    [4,0]
    > median_from_sorted_data {1.0,2.0,3.0};
    2.0
    > quantile_from_sorted_data {1.0,2.0,3.0} 0.25;
    1.5

Random Number Distributions
---------------------------

<a name="module-gslrandist"></a>

This module is loaded via the command `using gsl::randist` and provides Pure
wrappers for the GSL random distribution routines found in Chapter 19 of the
GSL manual,

<http://www.gnu.org/software/gsl/manual/html_node/Random-Number-Distributions.html>.

There are two namespaces provided by randist.pure, `gsl::ran` for probability
densitity functions and `gsl::cdf` for cumulative distribution functions. The
two namespaces minimize typing of the prefixes `gsl_ran_` and `gsl_cdf_`
respectively.

### Routines

<a name="gsl::ran::ugaussian_pdf"></a>`gsl::ran::ugaussian_pdf x`
:   implements `gsl_ran_ugaussian`.

<a name="gsl::ran::gaussian_pdf"></a>`gsl::ran::gaussian_pdf x  sigma`
:   implements `gsl_ran_gaussian_pdf`.

<a name="gsl::ran::gaussian_tail_pdf"></a>`gsl::ran::gaussian_tail_pdf x  a  sigma`
:   implements `gsl_ran_gaussian_tail_pdf`.

<a name="gsl::ran::ugaussian_tail_pdf"></a>`gsl::ran::ugaussian_tail_pdf x  a`
:   implements `gsl_ran_ugaussian_tail_pdf`.

<a name="gsl::ran::bivariate_gaussian_pdf"></a>`gsl::ran::bivariate_gaussian_pdf x  a`
:   implements `gsl_ran_bivariate_gaussian_pdf`.

<a name="gsl::ran::exponential_pdf"></a>`gsl::ran::exponential_pdf x  mu`
:   implements `gsl_ran_exponential_pdf`.

<a name="gsl::ran::laplace_pdf"></a>`gsl::ran::laplace_pdf x  a`
:   implements `gsl_ran_laplace_pdf`.

<a name="gsl::ran::exppow_pdf"></a>`gsl::ran::exppow_pdf x  a  b`
:   implements `gsl_ran_exppow_pdf`.

<a name="gsl::ran::cauchy_pdf"></a>`gsl::ran::cauchy_pdf x  a`
:   implements `gsl_ran_cauchy_pdf`.

<a name="gsl::ran::rayleigh_pdf"></a>`gsl::ran::rayleigh_pdf x  sigma`
:   implements `gsl_ran_rayleigh_pdf`.

<a name="gsl::ran::rayleigh_tail_pdf"></a>`gsl::ran::rayleigh_tail_pdf x  a  sigma`
:   implements `gsl_ran_rayleigh_tail_pdf`.

<a name="gsl::ran::landau_pdf"></a>`gsl::ran::landau_pdf x`
:   implements `gsl_ran_landau_pdf`.

<a name="gsl::ran::gamma_pdf"></a>`gsl::ran::gamma_pdf x  a  b`
:   implements `gsl_ran_gamma_pdf`.

<a name="gsl::ran::flat_pdf"></a>`gsl::ran::flat_pdf x  a  b`
:   implements `gsl_ran_flat_pdf`.

<a name="gsl::ran::lognormal_pdf"></a>`gsl::ran::lognormal_pdf x  zeta  sigma`
:   implements `gsl_ran_lognormal_pdf`.

<a name="gsl::ran::chisq_pdf"></a>`gsl::ran::chisq_pdf x  nu`
:   implements `gsl_ran_chisq_pdf`.

<a name="gsl::ran::fdist_pdf"></a>`gsl::ran::fdist_pdf x  nu1  nu2`
:   implements `gsl_ran_fdist_pdf`.

<a name="gsl::ran::tdist_pdf"></a>`gsl::ran::tdist_pdf x  nu`
:   implements `gsl_ran_tdist_pdf`.

<a name="gsl::ran::beta_pdf"></a>`gsl::ran::beta_pdf x  a  b`
:   implements `gsl_ran_beta_pdf`.

<a name="gsl::ran::logistic_pdf"></a>`gsl::ran::logistic_pdf x  a`
:   implements `gsl_ran_logistic_pdf`.

<a name="gsl::ran::pareto_pdf"></a>`gsl::ran::pareto_pdf x  a  b`
:   implements `gsl_ran_pareto_pdf`.

<a name="gsl::ran::weibull_pdf"></a>`gsl::ran::weibull_pdf x  a  b`
:   implements `gsl_ran_weibull_pdf`.

<a name="gsl::ran::gumbel1_pdf"></a>`gsl::ran::gumbel1_pdf x  a  b`
:   implements `gsl_ran_gumbel1_pdf`.

<a name="gsl::ran::gumbel2_pdf"></a>`gsl::ran::gumbel2_pdf x  a  b`
:   implements `gsl_ran_gumbel2_pdf`.

<a name="gsl::ran::dirichlet_pdf"></a>`gsl::ran::dirichlet_pdf alpha::matrix  theta::matrix`
:   implements `gsl_ran_dirichlet_pdf`.

<a name="gsl::ran::dirichlet_lnpdf"></a>`gsl::ran::dirichlet_lnpdf alpha::matrix  theta::matrix`
:   implements `gsl_ran_dirichlet_lnpdf`.

<a name="gsl::ran::discrete_preproc"></a>`gsl::ran::discrete_preproc p::matrix`
:   implements `gsl_ran_discrete_preproc` without the `K` parameter.

<a name="gsl::ran::discrete_pdf"></a>`gsl::ran::discrete_pdf k::int  p::pointer`
:   implements `gsl_ran_discrete_pdf` without the `K` parameter.

<a name="gsl::ran::discrete_free"></a>`gsl::ran::discrete_free p::pointer`
:   implements `gsl_ran_discrete_free`

<a name="gsl::ran::poisson_pdf"></a>`gsl::ran::poisson_pdf k::int  mu`
:   implements `gsl_ran_poisson_pdf`.

<a name="gsl::ran::bernoulli_pdf"></a>`gsl::ran::bernoulli_pdf k::int  p`
:   implements `gsl_ran_bernoulli_pdf`.

<a name="gsl::ran::binomial_pdf"></a>`gsl::ran::binomial_pdf k::int  p  n::int`
:   implements `gsl_ran_binomial_pdf`.

<a name="gsl::ran::multinomial_pdf"></a>`gsl::ran::multinomial_pdf p::matrix  n::matrix`
:   implements `gsl_ran_multinomial_pdf`.

<a name="gsl::ran::multinomial_lnpdf"></a>`gsl::ran::multinomial_lnpdf p::matrix  n::matrix`
:   implements `gsl_ran_multinomial_lnpdf`.

<a name="gsl::ran::negative_binomial_pdf"></a>`gsl::ran::negative_binomial_pdf k::int  p  n`
:   implements `gsl_ran_negative_binomial_pdf`.

<a name="gsl::ran::pascal_pdf"></a>`gsl::ran::pascal_pdf k::int  p  n::int`
:   implements `gsl_ran_pascal_pdf`.

<a name="gsl::ran::geometric_pdf"></a>`gsl::ran::geometric_pdf k::int  p`
:   implements `gsl_ran_geometric_pdf`.

<a name="gsl::ran::hypergeometric_pdf"></a>`gsl::ran::hypergeometric_pdf k::int  n1::int  n2::int  t::int`
:   implements `gsl_ran_hypergeometric_pdf`.

<a name="gsl::ran::logarithmic_pdf"></a>`gsl::ran::logarithmic_pdf k::int  p`
:   implements `gsl_ran_logarithmic_pdf`.

<!-- -->

<a name="gsl::cdf::ugaussian_P"></a>`gsl::cdf::ugaussian_P x`
:   implements `gsl_cdf_ugaussian_P`.

<a name="gsl::cdf::ugaussian_Q"></a>`gsl::cdf::ugaussian_Q x`
:   implements `gsl_cdf_ugaussian_Q`.

<a name="gsl::cdf::ugaussian_Pinv"></a>`gsl::cdf::ugaussian_Pinv p`
:   implements `gsl_cdf_ugaussian_Pinv`.

<a name="gsl::cdf::ugaussian_Qinv"></a>`gsl::cdf::ugaussian_Qinv q`
:   implements `gsl_cdf_ugaussian_Qinv`.

<a name="gsl::cdf::gaussian_P"></a>`gsl::cdf::gaussian_P x  sigma`
:   implements `gsl_cdf_gaussian_P`.

<a name="gsl::cdf::gaussian_Q"></a>`gsl::cdf::gaussian_Q x  sigma`
:   implements `gsl_cdf_gaussian_Q`.

<a name="gsl::cdf::gaussian_Pinv"></a>`gsl::cdf::gaussian_Pinv p  sigma`
:   implements `gsl_cdf_gaussian_Pinv`.

<a name="gsl::cdf::guassian_Qinv"></a>`gsl::cdf::guassian_Qinv q  sigma`
:   implements `gsl_cdf_gaussian_Qinv`.

<a name="gsl::cdf::exponential_P"></a>`gsl::cdf::exponential_P x  mu`
:   implements `gsl_cdf_exponential_P`.

<a name="gsl::cdf::exponential_Q"></a>`gsl::cdf::exponential_Q x  mu`
:   implements `gsl_cdf_exponential_Q`.

<a name="gsl::cdf::exponential_Pinv"></a>`gsl::cdf::exponential_Pinv p  mu`
:   implements `gsl_cdf_exponential_Pinv`.

<a name="gsl::cdf::exponential_Qinv"></a>`gsl::cdf::exponential_Qinv q  mu`
:   implements `gsl_cdf_exponential_Qinv`.

<a name="gsl::cdf::laplace_P"></a>`gsl::cdf::laplace_P x  a`
:   implements `gsl_cdf_laplace_P`.

<a name="gsl::cdf::laplace_Q"></a>`gsl::cdf::laplace_Q x  a`
:   implements `gsl_cdf_laplace_Q`.

<a name="gsl::cdf::laplace_Pinv"></a>`gsl::cdf::laplace_Pinv p  a`
:   implements `gsl_cdf_laplace_Pinv`.

<a name="gsl::cdf::laplace_Qinv"></a>`gsl::cdf::laplace_Qinv q  a`
:   implements `gsl_cdf_laplace_Qinv`.

<a name="gsl::cdf::exppow_P"></a>`gsl::cdf::exppow_P x  a  b`
:   implements `gsl_cdf_exppow_P`.

<a name="gsl::cdf::exppow_Q"></a>`gsl::cdf::exppow_Q x  a  b`
:   implements `gsl_cdf_exppow_Q`.

<a name="gsl::cdf::cauchy_P"></a>`gsl::cdf::cauchy_P x  a`
:   implements `gsl_cdf_cauchy_P`.

<a name="gsl::cdf::cauchy_Q"></a>`gsl::cdf::cauchy_Q x  a`
:   implements `gsl_cdf_cauchy_Q`.

<a name="gsl::cdf::cauchy_Pinv"></a>`gsl::cdf::cauchy_Pinv p  a`
:   implements `gsl_cdf_cauchy_Pinv`.

<a name="gsl::cdf::cauchy_Qinv"></a>`gsl::cdf::cauchy_Qinv q  a`
:   implements `gsl_cdf_cauchy_Qinv`.

<a name="gsl::cdf::rayleigh_P"></a>`gsl::cdf::rayleigh_P x  sigma`
:   implements `gsl_cdf_rayleigh_P`.

<a name="gsl::cdf::rayleigh_Q"></a>`gsl::cdf::rayleigh_Q x  sigma`
:   implements `gsl_cdf_rayleigh_Q`.

<a name="gsl::cdf::rayleigh_Pinv"></a>`gsl::cdf::rayleigh_Pinv p  sigma`
:   implements `gsl_cdf_rayleigh_Pinv`.

<a name="gsl::cdf::rayleigh_Qinv"></a>`gsl::cdf::rayleigh_Qinv q  sigma`
:   implements `gsl_cdf_rayleigh_Qinv`.

<a name="gsl::cdf::gamma_P"></a>`gsl::cdf::gamma_P x  a  b`
:   implements `gsl_cdf_gamma_P`.

<a name="gsl::cdf::gamma_Q"></a>`gsl::cdf::gamma_Q x  a  b`
:   implements `gsl_cdf_gamMa_Q`.

<a name="gsl::cdf::gamma_Pinv"></a>`gsl::cdf::gamma_Pinv p  a  b`
:   implements `gsl_cdf_gamma_Pinv`.

<a name="gsl::cdf::gamma_Qinv"></a>`gsl::cdf::gamma_Qinv q  a  b`
:   implements `gsl_cdf_gamma_Qinv`.

<a name="gsl::cdf::flat_P"></a>`gsl::cdf::flat_P x  a  b`
:   implements `gsl_cdf_flat_P`.

<a name="gsl::cdf::flat_Q"></a>`gsl::cdf::flat_Q x  a  b`
:   implements `gsl_cdf_flat_Q`.

<a name="gsl::cdf::flat_Pinv"></a>`gsl::cdf::flat_Pinv p  a  b`
:   implements `gsl_cdf_flat_Pinv`.

<a name="gsl::cdf::flat_Qinv"></a>`gsl::cdf::flat_Qinv q  a  b`
:   implements `gsl_cdf_flat_Qinv`.

<a name="gsl::cdf::lognormal_P"></a>`gsl::cdf::lognormal_P x  zeta  sigma`
:   implements `gsl_cdf_lognormal_P`.

<a name="gsl::cdf::lognormal_Q"></a>`gsl::cdf::lognormal_Q x  zeta  sigma`
:   implements `gsl_cdf_lognormal_Q`.

<a name="gsl::cdf::lognormal_Pinv"></a>`gsl::cdf::lognormal_Pinv p  zeta  sigma`
:   implements `gsl_cdf_lognormal_Pinv`.

<a name="gsl::cdf::lognormal_Qinv"></a>`gsl::cdf::lognormal_Qinv q  zeta  sigma`
:   implements `gsl_cdf_lognormal_Qinv`.

<a name="gsl::cdf::chisq_P"></a>`gsl::cdf::chisq_P x  nu`
:   implements `gsl_cdf_chisq_P`.

<a name="gsl::cdf::chisq_Q"></a>`gsl::cdf::chisq_Q x  nu`
:   implements `gsl_cdf_chisq_Q`.

<a name="gsl::cdf::chisq_Pinv"></a>`gsl::cdf::chisq_Pinv p  nu`
:   implements `gsl_cdf_chisq_Pinv`.

<a name="gsl::cdf::chisq_Qinv"></a>`gsl::cdf::chisq_Qinv q  nu`
:   implements `gsl_cdf_chisq_Qinv`.

<a name="gsl::cdf::fdist_P"></a>`gsl::cdf::fdist_P x  nu1  nu2`
:   implements `gsl_cdf_fdist_P`.

<a name="gsl::cdf::fdist_Q"></a>`gsl::cdf::fdist_Q x  nu1  nu2`
:   implements `gsl_cdf_fdist_Q`.

<a name="gsl::cdf::fdist_Pinv"></a>`gsl::cdf::fdist_Pinv p  nu1  nu2`
:   implements `gsl_cdf_fdist_Pinv`.

<a name="gsl::cdf::fdist_Qinv"></a>`gsl::cdf::fdist_Qinv q  nu1  nu2`
:   implements `gsl_cdf_fdist_Qinv`.

<a name="gsl::cdf::tdist_P"></a>`gsl::cdf::tdist_P x  nu`
:   implements `gsl_cdf_tdist_P`.

<a name="gsl::cdf::tdist_Q"></a>`gsl::cdf::tdist_Q x  nu`
:   implements `gsl_cdf_tdist_Q`.

<a name="gsl::cdf::tdist_Pinv"></a>`gsl::cdf::tdist_Pinv p  nu`
:   implements `gsl_cdf_tdist_Pinv`.

<a name="gsl::cdf::tdist_Qinv"></a>`gsl::cdf::tdist_Qinv q  nu`
:   implements `gsl_cdf_tdist_Qinv`.

<a name="gsl::cdf::beta_P"></a>`gsl::cdf::beta_P x  a  b`
:   implements `gsl_cdf_beta_P`.

<a name="gsl::cdf::beta_Q"></a>`gsl::cdf::beta_Q x  a  b`
:   implements `gsl_cdf_beta_Q`.

<a name="gsl::cdf::beta_Pinv"></a>`gsl::cdf::beta_Pinv p  a  b`
:   implements `gsl_cdf_beta_Pinv`.

<a name="gsl::cdf::beta_Qinv"></a>`gsl::cdf::beta_Qinv q  a  b`
:   implements `gsl_cdf_beta_Qinv`.

<a name="gsl::cdf::logistic_P"></a>`gsl::cdf::logistic_P x  a`
:   implements `gsl_cdf_logistic_P`.

<a name="gsl::cdf::logistic_Q"></a>`gsl::cdf::logistic_Q x  a`
:   implements `gsl_cdf_logistic_Q`.

<a name="gsl::cdf::logistic_Pinv"></a>`gsl::cdf::logistic_Pinv p  a`
:   implements `gsl_cdf_logistic_Pinv`.

<a name="gsl::cdf::logistic_Qinv"></a>`gsl::cdf::logistic_Qinv q  a`
:   implements `gsl_cdf_logistic_Qinv`.

<a name="gsl::cdf::pareto_P"></a>`gsl::cdf::pareto_P x  a  b`
:   implements `gsl_cdf_pareto_P`.

<a name="gsl::cdf::pareto_Q"></a>`gsl::cdf::pareto_Q x  a  b`
:   implements `gsl_cdf_pareto_Q`.

<a name="gsl::cdf::pareto_Pinv"></a>`gsl::cdf::pareto_Pinv p  a  b`
:   implements `gsl_cdf_pareto_Pinv`.

<a name="gsl::cdf::pareto_Qinv"></a>`gsl::cdf::pareto_Qinv q  a  b`
:   implements `gsl_cdf_pareto_Qinv`.

<a name="gsl::cdf::weibull_P"></a>`gsl::cdf::weibull_P x  a  b`
:   implements `gsl_cdf_weibull_P`.

<a name="gsl::cdf::weibull_Q"></a>`gsl::cdf::weibull_Q x  a  b`
:   implements `gsl_cdf_weibull_Q`.

<a name="gsl::cdf::weibull_Pinv"></a>`gsl::cdf::weibull_Pinv p  a  b`
:   implements `gsl_cdf_weibull_Pinv`.

<a name="gsl::cdf::weibull_Qinv"></a>`gsl::cdf::weibull_Qinv q  a  b`
:   implements `gsl_cdf_weibull_Qinv`.

<a name="gsl::cdf::gumbel1_P"></a>`gsl::cdf::gumbel1_P x  a  b`
:   implements `gsl_cdf_gumbel1_P`.

<a name="gsl::cdf::gumbel1_Q"></a>`gsl::cdf::gumbel1_Q x  a  b`
:   implements `gsl_cdf_gumbel1_Q`.

<a name="gsl::cdf::gumbel1_Pinv"></a>`gsl::cdf::gumbel1_Pinv p  a  b`
:   implements `gsl_cdf_gumbel1_Pinv`.

<a name="gsl::cdf::gumbel1_Qinv"></a>`gsl::cdf::gumbel1_Qinv q  a  b`
:   implements `gsl_cdf_gumbel1_Qinv`.

<a name="gsl::cdf::gumbel2_P"></a>`gsl::cdf::gumbel2_P x  a  b`
:   implements `gsl_cdf_gumbel2_P`.

<a name="gsl::cdf::gumbel2_Q"></a>`gsl::cdf::gumbel2_Q x  a  b`
:   implements `gsl_cdf_gumbel2_Q`.

<a name="gsl::cdf::gumbel2_Pinv"></a>`gsl::cdf::gumbel2_Pinv p  a  b`
:   implements `gsl_cdf_gumbel2_Pinv`.

<a name="gsl::cdf::gumbel2_Qinv"></a>`gsl::cdf::gumbel2_Qinv q  a  b`
:   implements `gsl_cdf_gumbel2_Qinv`.

<a name="gsl::cdf::poisson_P"></a>`gsl::cdf::poisson_P k::int  mu`
:   implements `gsl_cdf_poisson_P`.

<a name="gsl::cdf::poisson_Q"></a>`gsl::cdf::poisson_Q k::int  mu`
:   implements `gsl_cdf_poisson_Q`.

<a name="gsl::cdf::binomial_P"></a>`gsl::cdf::binomial_P k::int  p  n::int`
:   implements `gsl_cdf_binomial_P`.

<a name="gsl::cdf::binomial_Q"></a>`gsl::cdf::binomial_Q k::int  q  n::int`
:   implements `gsl_cdf_binomial_Q`.

<a name="gsl::cdf::negative_binomial_P"></a>`gsl::cdf::negative_binomial_P k::int  p  n`
:   implements `gsl_cdf_negative_binomial_P`.

<a name="gsl::cdf::negative_binomial_Q"></a>`gsl::cdf::negative_binomial_Q k::int  p  n`
:   implements `gsl_cdf_negative_binomial_Q`.

<a name="gsl::cdf::pascal_P"></a>`gsl::cdf::pascal_P k::int  p  n::int`
:   implements `gsl_cdf_pascal_P`.

<a name="gsl::cdf::pascal_Q"></a>`gsl::cdf::pascal_Q k::int  p  n::int`
:   implements `gsl_cdf_pascal_Q`.

<a name="gsl::cdf::geometric_P"></a>`gsl::cdf::geometric_P k::int  p`
:   implements `gsl_cdf_geometric_P`.

<a name="gsl::cdf::geometric_Q"></a>`gsl::cdf::geometric_Q k::int  p`
:   implements `gsl_cdf_geometric_Q`.

<a name="gsl::cdf::hypergeometric_P"></a>`gsl::cdf::hypergeometric_P k::int  n1::int  n2::int  t::int`
:   implements `gsl_cdf_hypergeometric_P`.

<a name="gsl::cdf::hypergeometric_Q"></a>`gsl::cdf::hypergeometric_Q k::int  n1::int  n2::int  t::int`
:   implements `gsl_cdf_hypergeometric_Q`.

<!-- -->
### Examples

The following illustrates the use of each function in the `randist` module.
The pdf functions are illustrated first.

    > using gsl::stats;
    > using namespace gsl::ran;
    > ugaussian_pdf 1.2;
    0.194186054983213
    > gaussian_pdf (-1.3) 1.5;
    0.182690978264686
    > gaussian_tail_pdf 2.0 1.0 1.5;
    0.433042698395299
    > ugaussian_tail_pdf 2.0 1.0;
    0.34030367841782
    > bivariate_gaussian_pdf 1.2 0.9 1.0 1.0 0.95;
    0.184646843689817
    > exponential_pdf 1.0 0.5;
    0.270670566473225
    > laplace_pdf 1.5 2.0;
    0.118091638185254
    > exppow_pdf 0.0 1.0 1.5;
    0.553866083716236
    > cauchy_pdf (-1.0) 1.0;
    0.159154943091895
    > rayleigh_pdf 2.5 1.0;
    0.109842334058519
    > rayleigh_tail_pdf 1.5 1.0 1.0;
    0.802892142778485
    > landau_pdf 1.1;
    0.140968737919623
    > gamma_pdf 1.0 1.0 1.5;
    0.342278079355061
    > flat_pdf 1.0 0.5 2.5;
    0.5
    > lognormal_pdf 0.01 0.0 1.0;
    0.000990238664959182
    > chisq_pdf 1.0 2.0;
    0.303265329856317
    > fdist_pdf 0.5 3.0 2.0;
    0.480970043785452
    > tdist_pdf 0.1 10.0;
    0.386975225815181
    > beta_pdf 0.5 4.0 1.0;
    0.499999999999999
    > logistic_pdf (-1.0) 2.0;
    0.117501856100797
    > pareto_pdf 0.01 3.0 2.0;
    0.0
    > weibull_pdf  0.01 1.0 1.0;
    0.990049833749168
    > gumbel1_pdf 0.01 1.0 1.0;
    0.367861108816436
    > gumbel2_pdf 0.01 1.0 1.0;
    3.72007597602084e-40
    > dirichlet_pdf {0.1,0.2,0.8} {2.0,2.0,2.0};
    0.00501316294425874
    > dirichlet_lnpdf {0.1,0.2,0.8} {2.0,2.0,2.0};
    -5.29568823688856
    > poisson_pdf 4 0.4;
    0.000715008049104682
    > bernoulli_pdf 1 0.7;
    0.7
    > binomial_pdf 3 0.5 9;
    0.1640625
    > multinomial_pdf {0.1,0.2,0.7} {2,2,2};
    0.0
    > multinomial_lnpdf {0.1,0.2,0.7} {2,2,2};
    -1728120799.71174
    > negative_binomial_pdf 10 0.5 3.5;
    0.0122430486923836
    > pascal_pdf 10 0.5 3;
    0.00805664062499999
    > geometric_pdf 5 0.4;
    0.05184
    > hypergeometric_pdf 1 5 20 3;
    0.413043478260872
    > logarithmic_pdf 10 0.7; 
    0.00234619293712492
    > test_discrete
    >   = v
    >     when
    >       px = discrete_preproc {0.1,0.3,0.4};
    >       v = discrete_pdf 0 px +
    >           discrete_pdf 1 px +
    >           discrete_pdf 2 px;
    >       _ = discrete_free px
    >     end;
    > test_discrete;
    1.0

The cumulative distribution functions are shown.

    > using namespace gsl::cdf;
    > ugaussian_P  (-1.3);
    0.0968004845856103
    > ugaussian_Q  (-1.3);
    0.90319951541439
    > ugaussian_Pinv  0.84;
    0.994457883209753
    > ugaussian_Qinv  0.84;
    -0.994457883209753
    > gaussian_P  (1.3)  1.5;
    0.806937662858093
    > gaussian_Q  (1.3)  1.5;
    0.193062337141907
    > gaussian_Pinv  0.4  5.0;
    -1.266735515679
    > gaussian_Qinv  0.4 5.0;
    1.266735515679
    > exponential_P  1.0  0.5;
    0.864664716763387
    > exponential_Q  1.0  0.5;
    0.135335283236613
    > exponential_Pinv  0.6  0.5;
    0.458145365937077
    > exponential_Qinv  0.6  0.5;
    0.255412811882995
    > laplace_P  1.5  2.0;
    0.763816723629493
    > laplace_Q  1.5  2.0;
    0.236183276370507
    > laplace_Pinv  0.6  2.0;
    0.446287102628419
    > laplace_Qinv  0.4  2.0;
    0.446287102628419
    > exppow_P  0.0  1.0  2.5;
    0.5
    > exppow_Q  0.0  1.0  0.5;
    0.5
    > cauchy_P  (-1.0)  1.0;
    0.25
    > cauchy_Q  (-1.0)  1.0;
    0.75
    > cauchy_Pinv  0.75  1.0;
    1.0
    > cauchy_Qinv  0.25  1.0;
    1.0
    > rayleigh_P  1.5  2.0;
    0.245160398010993
    > rayleigh_Q  0.5  1.0;
    0.882496902584595
    > rayleigh_Pinv  0.5  1.0;
    1.17741002251547
    > rayleigh_Qinv  0.5  1.0;
    1.17741002251547
    > gamma_P  1.0  1.0  3.0;
    0.283468689426211
    > gamma_Q  1.0  1.0  3.0;
    0.716531310573789
    > gamma_Pinv  0.5  1.0  1.0;
    0.693147180559945
    > gamma_Qinv  0.5  1.0  1.0;
    0.693147180559945
    > flat_P  2.0  1.2  4.8;
    0.222222222222222
    > flat_Q  2.0  1.2  4.8;
    0.777777777777778
    > flat_Pinv  0.2  0.5  2.5;
    0.9
    > flat_Qinv  0.2  0.5  2.5;
    2.1
    > lognormal_P  0.01  0.0  1.0;
    2.06064339597172e-06
    > lognormal_Q  0.01  0.0  1.0;
    0.999997939356604
    > lognormal_Pinv  0.1  0.0  1.0;
    0.27760624185201
    > lognormal_Qinv  0.1  0.0  1.0;
    3.60222447927916
    > chisq_P  1.0  2.0;
    0.393469340287367
    > chisq_Q  1.0  2.0;
    0.606530659712633
    > chisq_Pinv  0.5  2.0;
    0.221199216928595
    > chisq_Qinv  0.5  2.0;
    1.38629436111989
    > fdist_P  1.0  3.0  2.0;
    0.46475800154489
    > fdist_Q  1.0  3.0  2.0;
    0.53524199845511
    > fdist_Pinv  0.5  3.0  2.0;
    1.13494292261288
    > fdist_Qinv  0.5  3.0  2.0;
    1.13494292261288
    > tdist_P  2.1  10.0;
    0.968961377898891
    > tdist_Q  (-2.1)  10.0;
    0.968961377898891
    > tdist_Pinv  0.68  10.0;
    0.482264205919689
    > tdist_Qinv  0.68  10.0;
    -0.482264205919689
    > beta_P  0.75  2.0  2.0;
    0.84375
    > beta_Q  0.75  2.0  2.0;
    0.15625
    > beta_Pinv  0.75  2.0  2.0;
    0.673648177666931
    > beta_Qinv  0.25  2.0  2.0;
    0.673648177666931
    > logistic_P  (-1.0)  2.0;
    1
    > logistic_Q  (-1.0)  2.0;
    0.622459331201855
    > logistic_Pinv  0.75  1.0;
    1.09861228866811
    > logistic_Qinv  0.25  1.0;
    1.09861228866811
    > pareto_P  2.01  3.0  2.0;
    0.0148512406901899
    > pareto_Q  2.01  3.0  2.0;
    0.98514875930981
    > pareto_Pinv  0.1  3.0  2.0;
    2.07148833730257
    > pareto_Qinv  0.1  3.0  2.0;
    4.30886938006377
    > weibull_P  1.01  1.0  2.0;
    0.639441117518024
    > weibull_Q  1.01  2.0  3.0;
    0.879160657465162
    > weibull_Pinv  0.1  1.0  2.0;
    0.324592845974501
    > weibull_Qinv  0.1  1.0  2.0;
    1.51742712938515
    > gumbel1_P  1.01  1.0  1.0;
    0.694739044426344
    > gumbel1_Q  1.01  1.0  1.0;
    0.305260955573656
    > gumbel1_Pinv  0.1  1.0  1.0;
    -0.834032445247956
    > gumbel1_Qinv  0.1  1.0  1.0;
    2.25036732731245
    > gumbel2_P  1.01  1.0  1.0;
    0.371539903071873
    > gumbel2_Q  1.01  1.0  1.0;
    0.628460096928127
    > gumbel2_Pinv  0.1  1.0  1.0;
    0.434294481903252
    > gumbel2_Qinv  0.1  1.0  1.0;
    9.4912215810299
    > poisson_P  4  0.4;
    0.999938756672898
    > poisson_Q  4  0.6;
    0.000394486018340255
    > binomial_P  3  0.5  10;
    0.171874999999999
    > binomial_Q  3  0.5  10;
    0.828125000000001
    > negative_binomial_P  10  0.5  3.0;
    0.98876953125
    > negative_binomial_Q  10  0.5  3.0;
    0.01123046875
    > pascal_P  10  0.5  3;
    0.98876953125
    > pascal_Q  10  0.5  3;
    0.01123046875
    > geometric_P  5  0.4;
    0.92224
    > geometric_Q  5  0.6;
    0.01024
    > hypergeometric_P  1  5  20  3;
    0.908695652173913
    > hypergeometric_Q  1  5  20  3;
    0.0913043478260873

Sorting
-------

<a name="module-gslsort"></a>

This module is loaded via the command `using gsl::sort` and provides Pure
wrappers for the GSL sorting routines found in Chapter 11 of the GSL manual,

<http://www.gnu.org/software/gsl/manual/html_node/Sorting.html>.

### Routines

<a name="gsl::sort_vector"></a>`gsl::sort_vector m::matrix`
:   implements `gsl_sort` and `gsl_sort_int` without `stride` and `n`
    parameters.

<a name="gsl::sort_vector_index"></a>`gsl::sort_vector_index m::matrix`
:   implements `gsl_sort_index` and `gsl_sort_int_index` without `stride` and
    `n` parameters.

<!-- -->
### Examples

Usage of each library routine is illustrated below.

    > using gsl::sort;
    > using namespace gsl;
    > sort_vector {0,3,2,4,5};
    {0,2,3,4,5} 
    > sort_vector_index {0.0,1.0,5.0,2.0,8.0,0.0};
    {5,0,1,3,2,4}
