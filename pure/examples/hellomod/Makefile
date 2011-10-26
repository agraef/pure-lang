
# This directory contains a prototypical Pure module and a Makefile showing
# how to compile and install the module. You can use this as a starting point
# and template to create your own packages with Pure modules.

# Note that pkg-config is used to query the installed Pure for the necessary
# information required to build and install the module, so a fairly recent
# pkg-config version is required (pkg-config 0.23 has been tested).

modname = hello
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

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

# Default CFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
# These can be set from the command line as usual. Use CFLAGS, CPPFLAGS and
# LDFLAGS for compiler (-O etc.), preprocessor (-I etc.) and linker (-L etc.)
# options, respectively. LIBS is to be used for additional libraries to be
# linked (-l etc.).

CFLAGS = -g -O2

# Basic rules to build the module, clean, install and uninstall.

all: $(modname)$(DLL)

$(modname)$(DLL): $(modname).c
	$(CC) $(shared) -o $@ $(MOD_CFLAGS) $< $(MOD_LDFLAGS) $(LIBS)

clean:
	rm -f $(modname)$(DLL)

install: $(modname).pure $(modname)$(DLL)
	test -d $(installdir) || mkdir -p $(installdir)
	cp $^ $(installdir)

uninstall:
	rm -f $(installdir)/$(modname).pure $(installdir)/$(modname)$(DLL)

# Roll a distribution tarball.

DISTFILES = Makefile $(modname).c $(modname).pure

dist = $(modname)-$(modversion)

dist: $(DISTFILES)
	rm -rf $(dist)
	mkdir $(dist)
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
