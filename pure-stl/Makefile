# Makefile for pure-stlvec
#
# Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.
# 
# All rights reserved.
# 
# This software is is part of pure-stlvec, an addon to the Pure Programming
# Language (http://code.google.com/p/pure-lang/).
# 
# This software is distributed under a BSD-style license in the hope that it
# will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
# included with the pure-stlvec distribution package for details.
# 
# 

### Usage:
#
# make dll       - updates production versions of stl dlls and copys them to .
# make debug     - does the same for Pure debug support versions
# make           - same as make dll
# make install   - copy dlls and Pure files to $(libdir)/pure/
# make test      - run unit test scripts
# make clean     - rm all dlls and executables - leave only source code
# make uninstall - rm dlls and Pure files from $(libdir)/pure/
# make dist      - make a release tarball
# make distcheck - check the release tarball

### Example dry run:
#
#	touch sv_nonmodifying.cpp
#	make -n dll
#	make -n debug
#	make -n


# Package name and version number:
dist = pure-stl-$(version)
version = 0.1

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure$(EXE),%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib


# Try to guess the host system type.
host = $(shell ./config.guess)

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows (untested)
DLL = .dll
EXE = .exe
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

### Parameter Macros

DLL_BASENAMES = stlcontainer stlalgorithm
CONTAINER_BASENAMES = stlbase stlvec
ALGO_BASENAMES = sv_modifying sv_nonmodifying sv_sort \
	         sv_merge sv_heap sv_minmax sv_numeric
DEBUG_DLL_DIR = ./debug_dll
PROD_DLL_DIR = ./dll
# CXX = clang++
DBG_FLAGS = -DSTL_DEBUG
PROD_FLAGS = 

### Derived Macros

DBG_SUPPORT = $(if $(findstring debug,$(MAKECMDGOALS)),ON)
FLAGS = $(if $(DBG_SUPPORT),$(DBG_FLAGS),$(PROD_FLAGS))
DLL_DIR = $(if $(DBG_SUPPORT),$(DEBUG_DLL_DIR),$(PROD_DLL_DIR))

PURE_BASENAMES = stlvec_algorithms $(CONTAINER_BASENAMES) $(ALGO_BASENAMES)
CONTAINER_HPPS = $(addsuffix .hpp, $(CONTAINER_BASENAMES))
CONTAINER_CPPS = $(addsuffix .cpp, $(CONTAINER_BASENAMES))
ALGO_HPPS = stlbase.hpp stlvec.hpp sv_algorithm.hpp
ALGO_CPPS = $(addsuffix .cpp, $(ALGO_BASENAMES))

DLLS = $(addsuffix $(DLL), $(DLL_BASENAMES))
DLL_TRGS = $(addprefix $(DLL_DIR)/, $(DLLS))

PUREFILES = $(addsuffix .pure, $(PURE_BASENAMES))
LIBFILES = $(PUREFILES) $(DLLS)
INSTALLEDFILES = $(addprefix $(DESTDIR)$(libdir)/pure/, $(LIBFILES))
DISTFILES = Makefile COPYING README config.guess *.pure *.hpp *.cpp ut/*

### Make dlls

dll debug: $(DLLS)

$(DLLS): $(DLL_TRGS)
	cp $(DLL_DIR)/*$(DLL) .

$(DLL_DIR)/stlcontainer$(DLL): $(CONTAINER_HPPS) $(CONTAINER_CPPS)
	@ test -d $(DLL_DIR) || mkdir $(DLL_DIR)
	$(CXX) $(shared) $(FLAGS) $(PIC) $(CONTAINER_CPPS) -o $@

$(DLL_DIR)/stlalgorithm$(DLL): $(ALGO_HPPS) $(ALGO_CPPS)
	@ test -d $(DLL_DIR) || mkdir $(DLL_DIR)
	$(CXX) $(shared) $(FLAGS) $(PIC) $(ALGO_CPPS) -o $@

### Compile unit tests

test:
	pure -I. -x ut/ut_all.pure


### Remove dlls

clean:	
	@ test -d $(DEBUG_DLL_DIR) || mkdir $(DEBUG_DLL_DIR)
	@ test -d $(PROD_DLL_DIR) || mkdir $(PROD_DLL_DIR)
	@ rm -f $(DEBUG_DLL_DIR)/* $(PROD_DLL_DIR)/*
	@ rm -f *.so

### Install and uninstall

install: dll
	@ test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	@ cp $(LIBFILES) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f $(INSTALLEDFILES)

### Make distribution tarball

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/ut
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)

.PHONY:	dll debug test clean install uninstall dist distcheck

