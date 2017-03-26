<a name="doc-pure-liblo"></a>

pure-liblo
==========

<a name="module-lo"></a>

<a name="module-osc"></a>

Version 0.9, March 06, 2017

Albert Gräf &lt;<aggraef@gmail.com>&gt;

Copying
-------

Copyright (c) 2009 by Albert Graef.

pure-liblo is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

pure-liblo is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
details.

You should have received a copy of the GNU Lesser General Public License along
with this program. If not, see &lt;<http://www.gnu.org/licenses/>&gt;.

Description
-----------

This is a quick and dirty Pure wrapper for the
[liblo](http://liblo.sourceforge.net/) library by Steve Harris and others,
which implements Berkeley's [Open Sound Control](http://opensoundcontrol.org/)
(OSC) protocol.

OSC is a protocol for exchanging data between multimedia devices and software
across the network (TCP, UDP and UNIX domain sockets are supported as the
transport layer). It is also useful as a general communication mechanism for
both hard- and software. In difference to the plain socket interface (on which
it builds), OSC provides you with an efficient means to send around binary
data packets along with the corresponding type and timing information, which
makes it well-suited for both realtime and non-realtime applications.

The OSC protocol is [standardized](http://opensoundcontrol.org/spec-1_0) and
is supported by an abundance of different
[implementations](http://opensoundcontrol.org/implementations), which includes
controller hardware of all sorts and computer music software like CSound, Pd
and SuperCollider. Lots of implementations exist for different programming
languages. liblo aims to provide a lightweight and ubiquitous OSC
implementation for the C programming language.

The `lo.pure` module provides a fairly straight wrapper of the C library. A
more high-level and Purified interface is available in `osc.pure`. Most of the
time, you'll want to use the latter for convenience, but if you need utmost
flexibility then it is worth having a look at `lo.pure`, too.

-   Get the latest source from
    <https://bitbucket.org/purelang/pure-lang/downloads/pure-liblo-0.9.tar.gz>.
-   To install, run `make` and `sudo make install`. This will try to guess
    your Pure installation directory; if it guesses wrong, you can set the
    `prefix` variable accordingly, see the Makefile for details.
-   You can also regenerate the wrapper by running `make generate`; this
    requires the `pure-gen` utility and the liblo headers. The present version
    was generated from liblo 0.26. If your liblo version differs from that
    then it's always a good idea to run `make generate`.
-   Have a look at `lo.pure` and `osc.pure` for a description of the API
    provided to Pure programmers.
-   The `examples` folder contains some Pure code which illustrates how to use
    these modules.

