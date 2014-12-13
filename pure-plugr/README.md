pure-plugr
==========

Albert Gräf <aggraef@gmail.com>, 2014-12-13

PlugR (pronounced "plug-ar" with a rolling "r") is a research project which
aims at developing a generic plugin architecture for the Pure programming
language. PlugR will let you run Pure scripts as plugins under various plugin
architectures. The idea is quite similar to JUCE (but without all the extra
GUI and graphics baggage), in that the Pure core of the plugin architecture is
implemented as a separate base class, which provides all the stuff needed to
add the entry points for the supported architectures quite easily.

This code is still highly experimental and the only supported target
architecture is Steinberg's [VST][1] right now. Other target architectures
such as AU, LV2, Pd, Max et al will be added in the future, once we have the
basics right. The code has only been tested on Linux so far, but Mac OS X and
Windows support shouldn't be hard to do and is on our TODO list.

If you want to give PlugR a whirl in this early stage, make sure that you have
Pure installed and run `make && sudo make install`. This will install the
necessary helper scripts. Then run `make` in the `examples` subdir to compile
yourself some VST plugins. (You'll need Steinberg's VST SDK to make that
work. The SDK isn't redistributable, so you need to download it at their
[website][1].) Copy the resulting plugins over to your VST plugin dir, or just
run `sudo make install` in the `examples` subdir, and give them a whirl in VST
hosts such as Ardour, Qtractor and Tracktion (these have been tested already
and are known to work).

[1]: http://www.steinberg.net/en/company/developers.html
