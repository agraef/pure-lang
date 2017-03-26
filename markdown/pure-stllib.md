<a name="doc-pure-stllib"></a>

pure-stllib
===========

Version 0.6, March 06, 2017

| Peter Summerland &lt;<p.summerland@gmail.com>&gt;

[pure-stllib](pure-stllib.html) is an "umbrella" package that contains a pair
of Pure addons, [pure-stlvec](pure-stlvec.html) and
[pure-stlmap](pure-stlmap.html). These addons provide
[Pure](http://purelang.bitbucket.org) interfaces to a selection of containers
provided by the [C++ Standard Library](http://en.cppreference.com/w/cpp),
specialized to hold pointers to arbitrary Pure expressions.
[pure-stlvec](pure-stlvec.html) is a Pure interface to C++'s vector and the
STL algorithms that act on them. [pure-stlmap](pure-stlmap.html) is an
interface to six (of the eight) of C++'s associative containers: map, set,
multimap, multiset, unordered\_map and unordered\_set.

Copying
-------

| Copyright (c) 2011-2012 by Peter Summerland
  &lt;<p.summerland@gmail.com>&gt;.

All rights reserved.

[pure-stllib](pure-stllib.html) is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

[pure-stllib](pure-stllib.html) is distributed under a BSD-style license, see
the COPYING file for details.

Installation
------------

pure-stllib-0.6 requires at least Pure 0.50. The latest version of Pure is
available at <https://bitbucket.org/purelang/pure-lang/downloads>.

The latest version of the source code for [pure-stllib](pure-stllib.html) can
be downloaded from
<https://bitbucket.org/purelang/pure-lang/downloads/pure-stllib-0.6.tar.gz>.

To install pure-stllib-0.6 (on Linux), extract the source code (e.g., tar -xzf
pure-stllib-0.6.tar.gz), cd to the pure-stllib-0.6 directory, and run `make`.
After this you can (and should) also run `make check` to run a few unit tests
to make sure that [pure-stlvec](pure-stlvec.html) and
[pure-stlmap](pure-stlmap.html) work properly on your system. If `make check`
works, run `sudo make install` to install [pure-stlvec](pure-stlvec.html) and
[pure-stlmap](pure-stlmap.html). Run `sudo make uninstall` to remove them.

`make` tries to guess your Pure installation directory and platform-specific
setup. If it gets this wrong, you can set some variables manually. In
particular, `make install prefix=/usr` sets the installation prefix. Please
see the Makefile for details.

Usage
-----

[pure-stlvec](pure-stlvec.html) provides functions that act on a single
mutable container, stlvec, which is a wrapper around C++'s vector, specialized
to hold Pure expressions. It also provides functions that correspond to C++'s
STL algorithms specialized to act on stlvecs.

[pure-stlmap](pure-stlmap.html) provides functions that act on six mutable
containers, "stlmap", "stlset", "stlmmap", "stlmset", "stlhmap" and "stlhset",
that are thin wrappers around the corresponding associative containers
provided by C++, map, set, multimap, multiset, unordered\_map and
unordered\_set, specialized to hold Pure expressions.

The functions provided by [pure-stlvec](pure-stlvec.html) and
[pure-stlmap](pure-stlmap.html) are made available by importing one or more of
the following modules.

> [stlvec](pure-stlvec.html#module-stlvec) - support for stlvecs
>
> [stlvec::algorithms](pure-stlvec.html#module-stlvec::algorithms) - STL
> algorithms specialized to act on stlvecs
>
> [stlmap](pure-stlmap.html#module-stlmap) - support for stlmap and stlset
>
> [stlmmap](pure-stlmap.html#module-stlmmap) - support for stlmmap and stlmset
>
> [stlhmap](pure-stlmap.html#module-stlhmap) - support for stlhmap and stlhset

Documentation
-------------

Please see the documentation for [pure-stlvec](pure-stlvec.html) and
[pure-stlmap](pure-stlmap.html).

For the impatient, the functions that act on containers provided by the
[stlmap](pure-stlmap.html#module-stlmap),
[stlmmap](pure-stlmap.html#module-stlmmap),
[stlhmap](pure-stlmap.html#module-stlhmap) and
[stlvec](pure-stlvec.html#module-stlvec) modules are summarized in a
rudimentary cheatsheet, pure-stllib-cheatsheet.pdf, which can be found in the
pure-stllib/doc directory.

Changes
-------

Version 0.1 - Bundle pure-stlvec-0.3 and pure-stlmap-0.1.

Version 0.2 - Bundle pure-stlvec-0.3 and pure-stlmap-0.2.

Version 0.3 - Bundle pure-stlvec-0.4 and pure-stlmap-0.3.
