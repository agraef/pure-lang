<a name="doc-pure-g2"></a>

<a name="module-g2"></a>

pure-g2
=======

Version 0.3, March 06, 2017

Albert Graef &lt;<aggraef@gmail.com>&gt;

This is a straight wrapper of the g2 graphics library, see
<http://g2.sf.net/>.

License: BSD-style, see the COPYING file for details.

Get the latest source from
<https://bitbucket.org/purelang/pure-lang/downloads/pure-g2-0.3.tar.gz>.

g2 is a simple, no-frills 2D graphics library, distributed under the LGPL.
It's easy to use, portable and supports PostScript, X11, PNG and Win32. Just
the kind of thing that you need if you want to quickly knock out some basic
graphics, and whipping out the almighty OpenGL or GTK/Cairo seems overkill.

To use this module, you need to have libg2 installed as a shared library
(libg2.so, .dll etc.) in a place where the Pure interpreter can find it. The
g2 source does not include rules to properly build and install a shared
library on modern Linux systems, so we've included a little patch named
g2-0.72-sharedlib.patch in the pure-g2 package which helps with this. (You
still have to run ldconfig on Linux after installing g2. Also note that this
patch has only been tested on Linux, for other systems you'll probably have to
edit the g2 Makefile yourself.)

Documentation still needs to be written, so for the time being please see
g2.pure and have a look at the examples provided in the distribution.

Run `make install` to copy g2.pure to the Pure library directory. This tries
to guess the prefix under which Pure is installed; if this doesn't work,
you'll have to set the prefix variable in the Makefile accordingly.

The Makefile also provides the following targets:

-   `make examples` compiles the examples to native executables.
-   `make clean` deletes the native executables for the examples, as well as
    some graphics files which are produced by running g2\_test.pure.
-   `make generate` regenerates the g2.pure module. This requires that you
    have pure-gen installed, as well as the g2 header files (you can point
    pure-gen to the prefix under which g2 is installed with the g2prefix
    variable in the Makefile). This step shouldn't normally be necessary,
    unless you find that the provided wrapper doesn't work with your g2
    version. The g2.pure in this release has been generated from g2 0.72.

