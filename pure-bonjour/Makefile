
pkgname = pure-bonjour
version = 0.2

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

ifeq ($(DLL),.so)
# Linux: link against Avahi (default Zeroconf implementation)
LIBS = -ldns_sd
endif

ifeq ($(DLL),.dylib)
# OSX: hardcode installation path into the dll.
dllname = -install_name "$(installdir)/$@"
endif

# Basic rules to build the module, clean, install and uninstall.

csource = bonjour.c
puresource = $(patsubst %.c, %.pure, $(csource))
objects = $(patsubst %.c, %$(DLL), $(csource))

ifeq ($(DLL),.so)
# We don't build this module on Linux by default any more, you should use
# pure-avahi there instead. If you want to force the build anyway, run 'make
# all' instead.
default:
	@echo "WARNING: We don't build this module on Linux by default; use pure-avahi instead."
	@echo "WARNING: If you still want to try, run 'make all'."
else
default: all
endif

all: $(objects)

%$(DLL): %.c
	$(CC) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) $< $(MOD_LDFLAGS) $(LIBS)

clean:
	rm -f *$(DLL)

install: $(puresource) $(objects)
	test -d $(installdir) || mkdir -p $(installdir)
	cp $^ $(installdir)

uninstall:
	rm -f $(addprefix $(installdir)/, $(puresource) $(objects))

# Roll a distribution tarball.

DISTFILES = COPYING COPYING.LESSER Makefile $(csource) $(puresource) \
zeroconf.pure examples/*.pure
SEDFILES = README

dist = $(pkgname)-$(version)

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
