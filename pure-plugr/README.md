pure-plugr
==========

Albert Gräf <aggraef@gmail.com>, 2014-12-13

PlugR (pronounced "plug-ar" with a rolling "r") is a research project which
aims at developing a universal plugin architecture for the Pure programming
language. The idea is quite similar to JUCE (but without all the extra GUI and
graphics baggage), in that the created plugins contain entry points for all
the supported architectures. The Pure core of the plugin architecture is
implemented as a separate base class, which provides all the stuff needed to
add support for new plugin architectures quite easily.

This code is still highly experimental and the only supported target
architecture is Steinberg's [VST][1] right now. Other target architectures
such as AU, LV2, Pd, Max et al will be added in the future, once we have the
basics right. So far, the code has been tested on Linux and Mac OS X, but
Windows support shouldn't be hard to do and is on our TODO list.

If you want to give PlugR a whirl in this early stage, make sure that you have
Pure installed and run `make && sudo make install`. This will install the
necessary helper scripts. Then run `make` in the `examples` subdir to compile
yourself some VST plugins. (You'll need Steinberg's VST SDK to make that
work. The SDK isn't redistributable, so you need to download it at their
[website][1].) Copy the resulting plugins over to your VST plugin dir, or just
run `sudo make install` in the `examples` subdir, and give them a whirl in VST
hosts such as Ardour, Qtractor, Reaper and Tracktion (these have been tested
already and are known to work).

Of course, you can also create your own plugin scripts and compile them with
the pure2plugr script included in the distribution. On Mac OS X the script
understands the `-u` option which lets you create universal (32+64 bit Intel)
plugin binaries which will work with both 32 and 64 bit VST hosts. To make
this work, you need to have a universal build of the Pure interpreter
installed (`sudo port install pure +universal` if you're using MacPorts), as
well as universal builds of all the Pure addon modules used by your plugin
scripts.

[1]: http://www.steinberg.net/en/company/developers.html
