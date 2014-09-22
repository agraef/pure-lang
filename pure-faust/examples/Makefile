
# Generic Makefile for Pure-Faust plugins.

# make, make all: make plugins
# make bitcode: make plugins in LLVM bitcode format
# make cpp: make C++ sources
# make svg: make SVG diagrams
# make clean: remove all generated stuff

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
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

dspsrc  := $(wildcard *.dsp)
cppsrc  := $(dspsrc:.dsp=.cpp)
plugins	:= $(dspsrc:.dsp=$(DLL))
bitcode	:= $(dspsrc:.dsp=.bc)
svg 	:= $(dspsrc:.dsp=-svg)
xml 	:= $(dspsrc:.dsp=.dsp.xml)

all: $(plugins)
cpp: $(cppsrc)
svg: $(svg)
xml: $(xml)

# Use this target to generate bitcode modules (this requires faust2).
bitcode: $(bitcode)

clean:
	rm -Rf *~ $(cppsrc) $(plugins) $(bitcode) $(svg) $(xml)

%.cpp: %.dsp
	faust -a pure.cpp -double $< -o $@

%$(DLL): %.cpp
	$(CXX) $(shared) $(PIC) $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) -o $@ $<

# Here's an alternative rule which shows how to create bitcode using the C
# architecture pure.c instead. You'll need an LLVM-capable C compiler like
# clang for this.
%.bc: %.dsp
	faust -lang c -a pure.c -double $< -o $(<:.dsp=.c)
	clang -emit-llvm -O3 -c $(<:.dsp=.c) -o $@
	rm -f $(<:.dsp=.c)

# Default rule for generating bitcode directly using Faust.
%.bc: %.dsp
	faust -lang llvm -double $< -o $@

%-svg: %.dsp
	faust -svg $< -o /dev/null

%.dsp.xml: %.dsp
	faust -xml $< -o /dev/null
