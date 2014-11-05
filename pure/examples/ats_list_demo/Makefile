
# This directory contains a prototypical Pure module written in a
# mixture of ATS and C, and a Makefile showing how to compile and
# install the module. You can use this as a starting point and
# template to create your own packages with Pure modules.

# Note that pkg-config is used to query the installed Pure for the
# necessary information required to build and install the module. (The
# original author of this demo actually uses the pkg-config clone from
# OpenBSD.)

modname = ats_list_demo
modversion = 1.0

# Get various platform-specific information from pure.pc. 'DLL' is the
# filename extension for shared libraries, 'PIC' the PIC flag (-fPIC on
# systems which need it) and 'shared' the option needed to create a shared
# library on the host system ('-shared' by default, '-dynamiclib' on OSX). The
# latter two options are gcc-specific, so you might have to fiddle with these
# if a different C compiler is used.

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

# Default installation directory. DESTDIR is supported for staging purposes to
# make package maintainers happy.

libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)

# Module compilation and linkage flags.

MOD_CFLAGS = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS)	\
	$(CPPFLAGS) $(AO_CFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

# Default CFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
# These can be set from the command line as usual. Use CFLAGS, CPPFLAGS and
# LDFLAGS for compiler (-O etc.), preprocessor (-I etc.) and linker (-L etc.)
# options, respectively. LIBS is to be used for additional libraries to be
# linked (-l etc.).

CFLAGS = -g -O2
LIBS = $(AO_LIBS)

# To use atomic_ops. Comment these out if you do not have atomic_ops
# and pthreads. (Building with atomic_ops may work without the
# -latomic_ops flag; usually the atomic_ops.h header file is
# sufficient.)

AO_CFLAGS = -DHAVE_ATOMIC_OPS=1 -DHAVE_PTHREAD=1
AO_LIBS = -latomic_ops

# ATS compiler.

ATSCC = patscc

# What should ATS use for allocation and freeing?

ATSCCFLAGS = -DATS_MEMALLOC_LIBC
ATSCCLIBS =

# To use the Boehm GC.
#ATSCCFLAGS = -DATS_MEMALLOC_GCBDW
#ATSCCLIBS = -lgc

# Basic rules to build the module, clean, install and uninstall.

all: $(modname)$(DLL)

$(modname)$(DLL): $(modname)/DATS/$(modname).dats	\
					$(modname)/SATS/$(modname).sats	\
					$(modname)/CATS/$(modname).cats
	$(ATSCC) $(shared) -o $@ $(ATSCCFLAGS) $(MOD_CFLAGS) \
		$(modname)/DATS/$(modname).dats \
		$(modname)/SATS/$(modname).sats \
		$(MOD_LDFLAGS) $(LIBS) $(ATSCCLIBS)

clean:
	rm -f $(modname)$(DLL) $(modname)_dats.c $(modname)_sats.c

install: $(modname).pure $(modname)$(DLL)
	test -d $(installdir) || mkdir -p $(installdir)
	cp $^ $(installdir)

uninstall:
	rm -f $(installdir)/$(modname).pure $(installdir)/$(modname)$(DLL)

# Roll a distribution tarball.

DISTFILES = Makefile $(modname).pure \
	$(modname)/DATS/$(modname).dats \
	$(modname)/SATS/$(modname).sats \
	$(modname)/CATS/$(modname).cats

dist = $(modname)-$(modversion)

dist: $(DISTFILES)
	rm -rf $(dist)
	mkdir -p $(dist)/$(modname)/DATS
	mkdir -p $(dist)/$(modname)/SATS
	mkdir -p $(dist)/$(modname)/CATS
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
