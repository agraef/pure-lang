
pkgname = pure-stldict
version = 0.6

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)

MOD_CXXFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CXXFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

CXXFLAGS = -g -O2 -std=c++0x -Wall

# Basic rules to build the modules, clean, install and uninstall.

cppsource = hashdict.cc orddict.cc
puresource = stldict*.pure $(patsubst %.cc, %.pure, $(cppsource))
objects = $(patsubst %.cc, %$(DLL), $(cppsource))

all: $(objects)

%$(DLL): %.cc
	g++ $(shared) -o $@ $(MOD_CXXFLAGS) $< $(MOD_LDFLAGS) $(LIBS)

clean:
	rm -f *.o *$(DLL) examples/life *~

install: $(puresource) $(objects)
	test -d $(installdir) || mkdir -p $(installdir)
	cp $^ $(installdir)

uninstall:
	rm -f $(addprefix $(installdir)/, $(puresource) $(objects))

# Examples and tests.

testdir = examples
tests = $(testdir)/test.pure
logs = $(patsubst %.pure, %.log, $(tests))

check:
	./run-tests

cleanlogs:
	rm -f $(logs)

logs: $(logs)

%.log: %.pure
	pure -v -I. -L. < $< > $@ 2>&1

# compile the 'life' example
examples: examples/life

examples/life: examples/life.pure
	pure -I. -L. -c $< -o $@

# Roll a distribution tarball.

DISTFILES = COPYING COPYING.LESSER Makefile $(cppsource) $(puresource) \
config.guess run-tests debian/* examples/*.pure examples/*.log
SEDFILES = README

dist = $(pkgname)-$(version)

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist: $(DISTFILES)
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(SEDFILES); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make check && make install DESTDIR=./BUILD
	rm -rf $(dist)

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@
