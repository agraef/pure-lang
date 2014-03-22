
# Package name and version number:
pkgname = pure-sockets
version = 0.7

# compiler flags -- these can be overridden from the command line if needed,
# as well as CPPFLAGS, LDFLAGS and LIBS
CFLAGS = -O3
#CFLAGS = -g -O2

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

prefix      = $(shell pkg-config pure --variable prefix)
bindir      = $(prefix)/bin
libdir      = $(shell pkg-config pure --variable libdir)

ifeq ($(DLL),.dylib)
dllname = -install_name "$(libdir)/pure/$@"
endif

ifeq ($(DLL),.dll)
LIBS = -lws2_32
endif

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS) $(LIBS)

# Anything listed below will also be included in the tarball. Adjust as needed.
extradirs = examples debian
extradist = examples/*.pure debian/*

# Distribution files for dist and distcheck targets below.
distfiles = COPYING COPYING.LESSER Makefile README $(source) $(extradist)
sedfiles = README

# Installation files for install and uninstall targets.
instfiles = $(pure_source) $(objects)

# The stuff below is pretty generic.

pure_source := $(wildcard *.pure)
c_source    := $(wildcard *.c)
cc_source   := $(wildcard *.cc)
objects     := $(c_source:.c=$(DLL)) $(cc_source:.cc=$(DLL))
source      := $(c_source) $(cc_source) $(pure_source)

all: $(objects)

clean:
	rm -f $(objects)

# compile C source to shared library
%$(DLL): %.c
	$(CC) $(shared) $(dllname) $(MOD_CFLAGS) $< -o $@ $(MOD_LDFLAGS)

# compile C++ source to shared library
%$(DLL): %.cc
	$(CXX) $(shared) $(dllname) $(MOD_CFLAGS) $< -o $@ $(MOD_LDFLAGS)

# install and uninstall targets.

pkgdir = $(DESTDIR)$(libdir)/pure

install: $(instfiles)
	test -d "$(pkgdir)" || mkdir -p "$(pkgdir)"
	cp $(instfiles) "$(pkgdir)"

uninstall:
	for x in $(instfiles); do rm -f "$(pkgdir)/$$x"; done

# dist and distcheck targets.

dist = $(pkgname)-$(version)

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist: $(html)
	rm -rf $(dist)
	mkdir $(dist)
	for x in $(extradirs); do mkdir $(dist)/$$x; done
	for x in $(distfiles); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(sedfiles); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@
