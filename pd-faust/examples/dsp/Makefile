
# GNU Makefile. Just run `make` to build the dsp modules in the native shared
# library format, or `make bitcode` for LLVM bitcode modules.

# This Makefile is self-contained, so you can just drop it into any directory
# with Faust dsp sources and run 'make' there to compile the modules to a
# format which is ready to be loaded with pd-faust.

CC = gcc
CXX = g++
#CC = clang
#CXX = clang++

# Default compilation flags. You may adjust these from the command line if
# necessary. The defaults here enable auto-vectorization and fast math on x86
# and similar cpus (this works with both gcc and clang). CPPFLAGS, LDFLAGS and
# LIBS are by default empty, but can also be set on the command line if you
# need them.
CFLAGS = -O3 -ffast-math -msse
CXXFLAGS = -O3 -ffast-math -msse

# Platform-specific setup. This uses the Pure pkg-config (pure.pc).
DLL = $(shell pkg-config pure --variable DLL)
PIC = $(shell pkg-config pure --variable PIC)
shared = $(shell pkg-config pure --variable shared)
PURE_INCLUDES = $(shell pkg-config pure --cflags)
PURE_LIBS = $(shell pkg-config pure --libs)

# LLVM setup. Make sure that we find the LLVM toolchain.
LLVM_CONFIG = $(word 1,$(wildcard /opt/local/bin/llvm-config*) $(wildcard /usr/local/bin/llvm-config*) $(wildcard /usr/bin/llvm-config*))
LLVM_PATH = $(shell $(LLVM_CONFIG) --bindir)
PATH := $(LLVM_PATH):$(PATH)
# clang bitcode compilation.
LLVMCC = clang -emit-llvm
LLVMCXX = clang++ -emit-llvm

# Faust (we only require mainline Faust, but Faust2 will work as well).
FAUST = faust -double -cn $(<:.dsp=)
#FAUST = faust -double

# This is the collection of flags which actually gets passed in the C/C++
# recipes. Normally you shouldn't have to edit or override these.
ALL_CFLAGS = $(PIC) $(CFLAGS) $(CPPFLAGS) $(PURE_INCLUDES) -I.
ALL_CXXFLAGS = $(PIC) $(CXXFLAGS) $(CPPFLAGS) -I.
ALL_LDFLAGS = $(LDFLAGS) $(LIBS)

# Generic compilation rules.

# compile Faust source to C++ module (Pure architecture)
%.cpp: %.dsp
	$(FAUST) -a pure.cpp $< -o $@

# compile Faust C++ module to shared lib
%$(DLL): %.cpp
	$(CXX) $(shared) $(ALL_CXXFLAGS) $< -o $@ $(ALL_LDFLAGS) $(PURE_LIBS)

# compile Faust C++ module to LLVM bitcode (needs clang) NB: need -std=c++11
# here to avoid a clang error on the __float128 type in newer libc versions
# (this type is only supported in clang 3.9+)
%.bc: %.cpp
	$(LLVMCXX) -std=c++11 $(ALL_CXXFLAGS) -c $< -o $@

# dsp sources
dspsource := $(sort $(wildcard *.dsp))

# target modules
cppsource := $(dspsource:%.dsp=%.cpp)
somodules := $(dspsource:.dsp=$(DLL))
bcmodules := $(dspsource:.dsp=.bc)
modules   := $(somodules) $(bcmodules)

# svg and xml/json targets
svg := $(dspsource:.dsp=-svg)
xml := $(dspsource:.dsp=.dsp.xml)
json := $(dspsource:.dsp=.dsp.json)

%-svg: %.dsp
	faust -svg -sd $< -o /dev/null >/dev/null

%.dsp.xml: %.dsp
	faust -xml $< -o /dev/null

%.dsp.json: %.dsp
	faust -json $< -o /dev/null

all: $(somodules)
bitcode: $(bcmodules)

# Faust-specific stuff
svg: $(svg)
xml: $(xml)
json: $(json)

clean:
	-rm -f $(modules)

distclean: clean
	rm -Rf $(cppsource) $(svg) $(xml) $(json)

realclean: distclean
