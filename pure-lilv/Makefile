
pkgname = pure-lilv
version = 0.4

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)

ifeq ($(DLL),.dylib)
dllname = -install_name "$(libdir)/pure/$@"
endif

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(shell pkg-config --cflags lilv-0) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

LIBS = $(shell pkg-config --libs lilv-0)

# Basic rules to build the module, clean, install and uninstall.

csource = lilv.c
puresource = $(patsubst %.c, %.pure, $(csource))
objects = $(patsubst %.c, %$(DLL), $(csource))

all: $(objects)

lilv$(DLL): lilv.c lv2_evbuf.c symap.c
	$(CC) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) -std=c99 $^ $(MOD_LDFLAGS) $(LIBS)

clean:
	rm -rf *$(DLL)*

install: $(puresource) $(objects)
	test -d $(installdir) || mkdir -p $(installdir)
	cp $^ $(installdir)

uninstall:
	rm -f $(addprefix $(installdir)/, $(puresource) $(objects))

# Roll a distribution tarball.

DISTFILES = COPYING Makefile $(puresource) $(csource) \
lv2_evbuf.c lv2_evbuf.h symap.c symap.h debian/* \
examples/Makefile examples/*.pure examples/*mid
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
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@
