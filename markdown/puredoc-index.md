---
author:
- 'Albert Gräf (ed.)'
date: 'Last updated: March 06, 2017 (Pure version 0.66)'
title: Pure Language and Library Documentation
---

<a name="doc-index"></a>

This manual collects all of Pure's online documentation: [The Pure
Manual](#the-pure-manual) which covers the Pure language and the operation of
the Pure interpreter; the [Pure Library Manual](#pure-library-manual) which
describes the standard library modules included in the distribution of the
Pure interpreter; all available documentation for the various [addon
modules](#addon-modules) which can be downloaded as separate packages from the
Pure website; and an appendix with [installation
instructions](#installing-pure-and-llvm) and additional information for
[Windows users](#running-pure-on-windows).

Most of the Pure documentation is distributed under the [GNU Free
Documentation License](http://www.gnu.org/copyleft/fdl.html). The authors of
the current edition are listed below. (This just lists the primary section
authors in alphabetical order; please check the different parts of this manual
for additional authorship and licensing information.)

-   Albert Gräf ([The Pure Manual](#the-pure-manual); [Pure Library
    Manual](#pure-library-manual); various addon manuals)
-   Rob Hubbard ([Pure-Rational - Rational number library for the Pure
    programming
    language](#pure-rational---rational-number-library-for-the-pure-programming-language))
-   Kay-Uwe Kirstein ([Gnuplot bindings](#gnuplot-bindings))
-   Eddie Rucker ([Pure-CSV - Comma Separated Value Interface for the Pure
    Programming
    Language](#pure-csv---comma-separated-value-interface-for-the-pure-programming-language);
    [pure-gsl - GNU Scientific Library Interface for
    Pure](#pure-gsl---gnu-scientific-library-interface-for-pure))
-   Jiri Spitz ([Pure-GLPK - GLPK interface for the Pure programming
    language](#pure-glpk---glpk-interface-for-the-pure-programming-language))
-   Peter Summerland ([Pure-Sql3](#pure-sql3), [pure-stlmap](#pure-stlmap),
    [pure-stlvec](#pure-stlvec))

The Pure programming system is free and open source software. The interpreter
runtime, the standard library and most of the addon modules are distributed
under the [GNU Lesser General Public
License](http://www.gnu.org/copyleft/lgpl.html) or the 3-clause [BSD
License](http://www.opensource.org/licenses/bsd-license.php) which allow for
commercial applications. Some parts of the system also use the [GNU General
Public License](http://www.gnu.org/copyleft/gpl.html) (typically because they
interface to other GPL'ed software such as Gnumeric, GSL and Octave). Details
about authorship and license conditions can be found in the sources or in the
various manual sections.

For more information, discussions, feedback, questions, suggestions etc.
please see:

-   Pure website: <http://purelang.bitbucket.org>
-   Pure mailing list: <http://groups.google.com/group/pure-lang>

<a name="addon-modules"></a>

<a name="doc-index2"></a>

Below is a brief overview of the available documentation.

**Language and Standard Library:**

This part of the manual documents the Pure language and interpreter, as well
as the standard library distributed with the interpreter.

-   [The Pure Manual](#the-pure-manual)
-   [Pure Library Manual](#pure-library-manual)

**Basic Support Utilities and Libraries:**

This part covers general programming tools and libraries which are useful in
many Pure programs but don't come bundled with the interpreter.

-   [pure-avahi: Pure Avahi Interface](#pure-avahi-pure-avahi-interface)
-   [pure-bonjour: Pure Bonjour
    Interface](#pure-bonjour-pure-bonjour-interface)
-   [pure-doc](#pure-doc)
-   [pure-ffi](#pure-ffi)
-   [pure-gen: Pure interface generator](#pure-gen-pure-interface-generator)
-   [pure-readline](#pure-readline)
-   [pure-sockets: Pure Sockets
    Interface](#pure-sockets-pure-sockets-interface)
-   [pure-stldict](#pure-stldict)
-   [pure-stllib](#pure-stllib)
-   [pure-stlmap](#pure-stlmap)
-   [pure-stlvec](#pure-stlvec)

**Scientific Computing:**

Interfaces to various 3rd party mathematical software.

-   [Gnumeric/Pure: A Pure Plugin for
    Gnumeric](#gnumericpure-a-pure-plugin-for-gnumeric)
-   [Pure-GLPK - GLPK interface for the Pure programming
    language](#pure-glpk---glpk-interface-for-the-pure-programming-language)
-   [Gnuplot bindings](#gnuplot-bindings)
-   [pure-gsl - GNU Scientific Library Interface for
    Pure](#pure-gsl---gnu-scientific-library-interface-for-pure)
-   [pure-mpfr](#pure-mpfr)
-   [pure-octave](#pure-octave)
-   [Pure-Rational - Rational number library for the Pure programming
    language](#pure-rational---rational-number-library-for-the-pure-programming-language)
-   [Computer Algebra with Pure: A Reduce
    Interface](#computer-algebra-with-pure-a-reduce-interface)

**Database and Web Programming:**

Modules for dealing with data in CSV and XML format, interfacing to SQL
databases, and running Pure scripts in a web server using the FastCGI
protocol.

-   [Pure-CSV - Comma Separated Value Interface for the Pure Programming
    Language](#pure-csv---comma-separated-value-interface-for-the-pure-programming-language)
-   [pure-fastcgi: FastCGI module for
    Pure](#pure-fastcgi-fastcgi-module-for-pure)
-   [Pure-ODBC - ODBC interface for the Pure programming
    language](#pure-odbc---odbc-interface-for-the-pure-programming-language)
-   [Pure-Sql3](#pure-sql3)
-   [Pure-XML - XML/XSLT interface](#pure-xml---xmlxslt-interface)

**GUI and Graphics:**

Various interfaces to 3rd party GUI and graphics libraries.

-   [pure-g2](#pure-g2)
-   [Pure OpenGL Bindings](#pure-opengl-bindings)
-   [Pure GTK+ Bindings](#pure-gtk-bindings)
-   [pure-tk](#pure-tk)

**Multimedia:**

A collection of scripts and modules useful for programming media applications.
Currently, this covers digital audio, MIDI and OSC. Interfaces to Yann
Orlarey's functional DSP programming language Faust and Miller Puckette's
graphical computer music software PureData are also available.

-   [faust2pd: Pd Patch Generator for
    Faust](#faust2pd-pd-patch-generator-for-faust)
-   [pd-faust](#pd-faust)
-   [pd-pure: Pd loader for Pure scripts](#pd-pure-pd-loader-for-pure-scripts)
-   [pure-audio](#pure-audio)
-   [pure-faust](#pure-faust)
-   [pure-liblo](#pure-liblo)
-   [pure-lilv: Pure Lilv Interface](#pure-lilv-pure-lilv-interface)
-   [pure-lv2](#pure-lv2)
-   [pure-midi](#pure-midi)

**Installation and Usage:**

General information about installing and using Pure.

-   [Installing Pure (and LLVM)](#installing-pure-and-llvm)
-   [Running Pure on Windows](#running-pure-on-windows)
-   [Using PurePad](#using-purepad)
-   [Reporting Bugs](#reporting-bugs)
