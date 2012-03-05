# Makfile for stllib

pkgname = stllib

version = 0.1

.PHONY: all stlvec stlmap dist check install uninstall clean dist distcheck

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)
libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)

MOD_CXXFLAGS = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

CFLAGS = -g -O2

# Basic rules to build the module, clean, check, install and uninstall.

cppsource = stlbase.cpp
puresource = $(patsubst %.cpp, %.pure, $(cppsource))
object = $(patsubst %.cpp, %$(DLL), $(cppsource))

all: $(object) stlmap stlvec

$(object): $(cppsource)
	$(CXX) $(shared) -o $@ $(MOD_CXXFLAGS) $(cppsource) \
        $(MOD_LDFLAGS) $(LIBS)

stlvec:
	$(MAKE) --directory=pure-stlvec

stlmap:
	$(MAKE) --directory=pure-stlmap

clean:
	rm -f *.o *$(DLL)
	rm -fr BUILD
	$(MAKE) clean --directory=pure-stlvec
	$(MAKE) clean --directory=pure-stlmap

install: $(puresource) $(object)
	test -d $(installdir) || mkdir -p $(installdir)
	cp $^ $(installdir)
	$(MAKE) install DESTDIR=$(DESTDIR) --directory=pure-stlvec
	$(MAKE) install DESTDIR=$(DESTDIR) --directory=pure-stlmap 

uninstall:
	rm -f $(addprefix $(installdir)/, $(puresource) $(object))
	$(MAKE) uninstall --directory=pure-stlvec
	$(MAKE) uninstall --directory=pure-stlmap

check:
	rm -rf BUILD
	$(MAKE) install DESTDIR=$(shell pwd)/BUILD
	./check_script $(shell pwd)/BUILD

# Roll a distribution tarball.

distfiles = Makefile README COPYING check_script *.pure *.hpp *.cpp *.pdf
sedfiles = README
dist = pure-$(pkgname)-$(version)
date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" \
	        -e "s?|today|?$(date)?g" < $(1) > $(2)
dist:
	rm -rf $(dist)
	mkdir $(dist)
	for x in $(distfiles); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(sedfiles); do rm -f $(dist)/$$x; \
                              $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	$(MAKE) dist DIST=../$(dist) --directory=pure-stlvec
	$(MAKE) dist DIST=../$(dist) --directory=pure-stlmap
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && $(MAKE) && $(MAKE) check
	rm -rf $(dist)
