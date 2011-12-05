
pkgname = pure-stlmap
version = 0.0

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)
stlbase	    = ../pure-stlvec


MOD_CXXFLAGS = $(PIC) $(shell pkg-config pure --cflags) -I$(pure-stlvec) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

CFLAGS = -g -O2

# Basic rules to build the module, clean, install and uninstall.

cppsource = stlmap.cpp stlmmap.cpp stlhmap.cpp
puresource = $(patsubst %.cpp, %.pure, $(cppsource))
objects = $(patsubst %.cpp, %$(DLL), $(cppsource))


all: $(objects)

%$(DLL): %.cpp
	g++ $(shared) -o $@ $(MOD_CXXFLAGS) -std=c++0x \
	$< $(MOD_LDFLAGS) $(LIBS) -I$(stlbase)

clean:
	rm -f *.o *$(DLL)

install: $(puresource) $(objects)
	test -d $(installdir) || mkdir -p $(installdir)
	cp $^ $(installdir)

uninstall:
	rm -f $(addprefix $(installdir)/, $(puresource) $(objects))

# Roll a distribution tarball.

DISTFILES = README Makefile $(cppsource) $(puresource)
#SEDFILES = README

dist = pure-$(pkgname)-$(version)

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist: $(DISTFILES)
	rm -rf $(dist)
	mkdir $(dist)
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(SEDFILES); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
