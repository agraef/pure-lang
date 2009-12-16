
# Generic Makefile for Pure-Faust plugins.

# make, make all: make plugins
# make cpp: make C++ sources
# make svg: make SVG diagrams
# make clean: remove all generated stuff

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

CXXFLAGS = -g -O2

# Try to guess the host system type and figure out compilation flags.
host = $(shell ../config.guess)
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

dspsrc  := $(wildcard *.dsp)
cppsrc  := $(dspsrc:.dsp=.cpp)
plugins	:= $(dspsrc:%.dsp=%$(DLL))
svg 	:= $(dspsrc:.dsp=-svg)
xml 	:= $(dspsrc:.dsp=.dsp.xml)

all: $(plugins)
cpp: $(cppsrc)
svg: $(svg)
xml: $(xml)

clean:
	rm -Rf *~ $(cppsrc) $(plugins) $(svg) $(xml)

%.cpp: %.dsp
	faust -a pure.cpp -double $< -o $@

%$(DLL): %.cpp
	$(CXX) $(shared) $(PIC) $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@ $<

%-svg: %.dsp
	faust -svg $< -o /dev/null

%.dsp.xml: %.dsp
	faust -xml $< -o /dev/null
