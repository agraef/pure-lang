
modname = hashmap
version = 0.0

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)

MOD_CXXFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CXXFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

CXXFLAGS = -g -O2 -std=c++0x -Wall

# Basic rules to build the module, clean, install and uninstall.

all: hashmap$(DLL)

hashmap$(DLL): hashmap.cc
	g++ $(shared) -o $@ $(MOD_CXXFLAGS) $< $(MOD_LDFLAGS) $(LIBS)

check:
	(cd examples && (pure -v -I.. -L.. < hashmap_examp.pure 2>&1 | diff hashmap_examp.log -) && echo "test passed" || (echo "test FAILED"; exit 1))

log: examples/hashmap_examp.log

examples/hashmap_examp.log: hashmap.cc hashmap.pure hashmap$(DLL) examples/hashmap_examp.pure
	(cd examples && (pure -v -I.. -L.. < hashmap_examp.pure > hashmap_examp.log 2>&1))

clean:
	rm -f *.o *$(DLL) *~

install: $(modname).pure $(modname)$(DLL)
	test -d $(installdir) || mkdir -p $(installdir)
	cp $^ $(installdir)

uninstall:
	rm -f $(installdir)/$(modname).pure $(installdir)/$(modname)$(DLL)

# Roll a distribution tarball.

DISTFILES = COPYING COPYING.LESSER Makefile $(modname).cc $(modname).pure \
examples/*.pure
#SEDFILES = README

dist = pure-$(modname)-$(version)

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist: $(DISTFILES)
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(SEDFILES); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
