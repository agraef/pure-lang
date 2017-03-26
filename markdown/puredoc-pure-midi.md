<a name="doc-pure-midi"></a>

pure-midi
=========

<a name="module-midi"></a>

<a name="module-midifile"></a>

Version 0.6, March 06, 2017

Albert Graef &lt;<aggraef@gmail.com>&gt;

This is a MIDI interface for the Pure programming language (Pure 0.45 or later
is required). It includes the following modules:

-   midi.pure: A PortMidi/PortTime wrapper which gives you portable access to
    realtime MIDI input and output. This uses PortMidi (by Roger B. Dannenberg
    et al) from the PortMedia project, see
    <http://portmedia.sourceforge.net/>.
-   midifile.pure: Reading and writing standard MIDI files. This is based on
    David G. Slomin's light-weight midifile library, which comes bundled with
    the pure-midi sources.

Documentation still needs to be written, so for the time being please read the
source modules listed above and have a look at the examples provided in the
distribution.

Installation
------------

Get the latest source from
<https://bitbucket.org/purelang/pure-lang/downloads/pure-midi-0.6.tar.gz>.

You need to have the PortMidi library installed on your system. This release
was tested with PortMidi 2.00 (I recommend using the svn version of PortMidi,
since it fixes some 64 bit compilation problems). If you have to use some
earlier PortMidi version then you may have to fiddle with portmidi.pure and/or
midi.pure to make it work. (You can also just regenerate the wrapper by
copying portmidi.h and porttime.h from your PortMidi installation to the
pure-midi source directory and running 'make generate'. This requires
pure-gen. See the toplevel Makefile for details.)

Run 'make' to compile the package. If you're lucky and everything compiles
smoothly, you can install with 'sudo make install'.

If you're not so lucky, you can get help on the Pure mailing list, see
<http://groups.google.com/group/pure-lang>.

NOTE: You may also want to install the related pure-audio package. In
particular, pure-audio also provides realtime.pure, a little utility module
which gives Pure programs access to realtime scheduling.

License
-------

pure-midi is Copyright (c) 2010 by Albert Graef, licensed under the 3-clause
BSD license, see the COPYING file for details.

For convenience, I've bundled some (BSD-licensed or compatible) source files
from other packages with this release. portmidi.h and porttime.h are from
PortMidi 2.00 (<http://portmedia.sourceforge.net/>) which is

Copyright (c) 1999-2000 Ross Bencina and Phil Burk Copyright (c) 2001-2006
Roger B. Dannenberg

midifile.c and midifile.h in the midifile subdirectory are from "Div's midi
utilities" (<http://public.sreal.com:8000/~div/midi-utilities/>) which is

Copyright (c) 2003-2006 David G. Slomin

Please see portmidi.h and midifile.h for the pertaining copyrights and license
conditions.
