
# A sample plugin to run Faust dsps in Gnumeric. Run 'make && make install'
# and see dsps.gnumeric and organ.gnumeric for examples.

myplugin = dsps.pure

# Platform-specific defaults, you may have to edit this if the
# auto-configuration below doesn't properly handle your system.

PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

CXXFLAGS = -g -O2

# Try to guess the host system type and figure out compilation flags.
host = $(shell ./config.guess)
ifneq "$(findstring -mingw,$(host))" ""
# Windows
DLL = .dll
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX (untested)
DLL = .dylib
shared = -dynamiclib
endif
ifneq "$(findstring x86_64-,$(host))" ""
# 64 bit, needs -fPIC flag
PIC = -fPIC
endif

dspsrc  = $(wildcard *.dsp)
cppsrc  = $(dspsrc:.dsp=.cpp)
plugins	= $(dspsrc:%.dsp=%$(DLL))
svg 	= $(dspsrc:.dsp=-svg)
xml 	= $(dspsrc:.dsp=.dsp.xml)

all: $(plugins) plugin.xml

plugin.xml: $(myplugin)
	pure-gnm $< > $@

clean:
	rm -Rf *~ $(cppsrc) $(plugins) $(svg) $(xml) plugin.xml

# This target lets you install the plugin.xml file into your personal Gnumeric
# plugin folder, so that the plugin will be recognized by Gnumeric without
# having to modify the plugin search path. Note that we use an absolute
# pathname for the plugin script here so that it is found no matter where in
# the file system it is located.

# NOTE: This requires GNU make and that you have the Gnumeric development
# files installed.
gnmversion=$(shell pkg-config --modversion libspreadsheet-1.12)
plugindir=$(HOME)/.gnumeric/$(gnmversion)/plugins/faust

install: $(myplugin)
	test -d $(plugindir) || mkdir -p $(plugindir)
	pure-gnm $(CURDIR)/$< > $(plugindir)/plugin.xml

# Remove the plugin directory created with 'make install' from your personal
# Gnumeric plugin folder.

uninstall:
	rm -rf $(plugindir)

# Generic rules for compiling Pure-Faust plugins. #############################

# We offer some useful extra targets here:
# make cpp: make C++ sources for the dsps
# make svg: make SVG diagrams for the dsps
# make xml: make Faust xml (not plugin.xml!) files for the dsps

cpp: $(cppsrc)
svg: $(svg)
xml: $(xml)

%.cpp: %.dsp
	faust -a pure.cpp -double -cn $(<:.dsp=) $< -o $@

%$(DLL): %.cpp
	$(CXX) $(shared) $(PIC) $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@ $<

%-svg: %.dsp
	faust -svg $< -o /dev/null

%.dsp.xml: %.dsp
	faust -xml $< -o /dev/null
