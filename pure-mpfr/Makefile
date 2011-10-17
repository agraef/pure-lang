
modname = mpfr
version = 0.1

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

CFLAGS = -g -O2

# Basic rules to build the module, clean, install and uninstall.

all: $(modname)$(DLL)

$(modname)$(DLL): $(modname).c
	$(CC) $(shared) -o $@ $(MOD_CFLAGS) $^ $(MOD_LDFLAGS) $(LIBS) -lmpfr -lgmp

clean:
	rm -f $(modname)$(DLL)

install: $(modname).pure $(modname)$(DLL)
	test -d $(installdir) || mkdir -p $(installdir)
	cp $^ $(installdir)

uninstall:
	rm -f $(installdir)/$(modname).pure $(installdir)/$(modname)$(DLL)

# Roll a distribution tarball.

DISTFILES = COPYING COPYING.LESSER README Makefile \
$(modname).c $(modname).pure examples/*.pure

dist = pure-$(modname)-$(version)

dist: $(DISTFILES)
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
