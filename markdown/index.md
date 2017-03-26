---
author:
- 'Albert Gräf (ed.)'
date: 'Last updated: March 06, 2017 (Pure version 0.66)'
title: Pure Language and Library Documentation
---

<a name="doc-index"></a>

This manual collects all of Pure's online documentation: [The Pure
Manual](pure.html) which covers the Pure language and the operation of the
Pure interpreter; the [Pure Library Manual](purelib.html) which describes the
standard library modules included in the distribution of the Pure interpreter;
all available documentation for the various [addon modules](#addon-modules)
which can be downloaded as separate packages from the Pure website; and an
appendix with [installation instructions](install.html) and additional
information for [Windows users](windows.html).

Most of the Pure documentation is distributed under the [GNU Free
Documentation License](http://www.gnu.org/copyleft/fdl.html). The authors of
the current edition are listed below. (This just lists the primary section
authors in alphabetical order; please check the different parts of this manual
for additional authorship and licensing information.)

-   Albert Gräf ([The Pure Manual](pure.html); [Pure Library
    Manual](purelib.html); various addon manuals)
-   Rob Hubbard ([Pure-Rational - Rational number library for the Pure
    programming language](pure-rational.html))
-   Kay-Uwe Kirstein ([Gnuplot bindings](pure-gplot.html))
-   Eddie Rucker ([Pure-CSV - Comma Separated Value Interface for the Pure
    Programming Language](pure-csv.html); [pure-gsl - GNU Scientific Library
    Interface for Pure](pure-gsl.html))
-   Jiri Spitz ([Pure-GLPK - GLPK interface for the Pure programming
    language](pure-glpk.html))
-   Peter Summerland ([Pure-Sql3](pure-sql3.html),
    [pure-stlmap](pure-stlmap.html), [pure-stlvec](pure-stlvec.html))

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

-   [The Pure Manual](pure.html)
-   [Pure Library Manual](purelib.html)

**Basic Support Utilities and Libraries:**

This part covers general programming tools and libraries which are useful in
many Pure programs but don't come bundled with the interpreter.

-   [pure-avahi: Pure Avahi Interface](pure-avahi.html)
-   [pure-bonjour: Pure Bonjour Interface](pure-bonjour.html)
-   [pure-doc](pure-doc.html)
-   [pure-ffi](pure-ffi.html)
-   [pure-gen: Pure interface generator](pure-gen.html)
-   [pure-readline](pure-readline.html)
-   [pure-sockets: Pure Sockets Interface](pure-sockets.html)
-   [pure-stldict](pure-stldict.html)
-   [pure-stllib](pure-stllib.html)
-   [pure-stlmap](pure-stlmap.html)
-   [pure-stlvec](pure-stlvec.html)

**Scientific Computing:**

Interfaces to various 3rd party mathematical software.

-   [Gnumeric/Pure: A Pure Plugin for Gnumeric](gnumeric-pure.html)
-   [Pure-GLPK - GLPK interface for the Pure programming
    language](pure-glpk.html)
-   [Gnuplot bindings](pure-gplot.html)
-   [pure-gsl - GNU Scientific Library Interface for Pure](pure-gsl.html)
-   [pure-mpfr](pure-mpfr.html)
-   [pure-octave](pure-octave.html)
-   [Pure-Rational - Rational number library for the Pure programming
    language](pure-rational.html)
-   [Computer Algebra with Pure: A Reduce Interface](pure-reduce.html)

**Database and Web Programming:**

Modules for dealing with data in CSV and XML format, interfacing to SQL
databases, and running Pure scripts in a web server using the FastCGI
protocol.

-   [Pure-CSV - Comma Separated Value Interface for the Pure Programming
    Language](pure-csv.html)
-   [pure-fastcgi: FastCGI module for Pure](pure-fastcgi.html)
-   [Pure-ODBC - ODBC interface for the Pure programming
    language](pure-odbc.html)
-   [Pure-Sql3](pure-sql3.html)
-   [Pure-XML - XML/XSLT interface](pure-xml.html)

**GUI and Graphics:**

Various interfaces to 3rd party GUI and graphics libraries.

-   [pure-g2](pure-g2.html)
-   [Pure OpenGL Bindings](pure-gl.html)
-   [Pure GTK+ Bindings](pure-gtk.html)
-   [pure-tk](pure-tk.html)

**Multimedia:**

A collection of scripts and modules useful for programming media applications.
Currently, this covers digital audio, MIDI and OSC. Interfaces to Yann
Orlarey's functional DSP programming language Faust and Miller Puckette's
graphical computer music software PureData are also available.

-   [faust2pd: Pd Patch Generator for Faust](faust2pd.html)
-   [pd-faust](pd-faust.html)
-   [pd-pure: Pd loader for Pure scripts](pd-pure.html)
-   [pure-audio](pure-audio.html)
-   [pure-faust](pure-faust.html)
-   [pure-liblo](pure-liblo.html)
-   [pure-lilv: Pure Lilv Interface](pure-lilv.html)
-   [pure-lv2](pure-lv2.html)
-   [pure-midi](pure-midi.html)

**Installation and Usage:**

General information about installing and using Pure.

-   [Installing Pure (and LLVM)](install.html)
-   [Running Pure on Windows](windows.html)
-   [Using PurePad](purepad.html)
-   [Reporting Bugs](bugs.html)
