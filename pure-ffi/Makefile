
# Package name and version number:
dist = pure-ffi-$(version)
version = 0.16

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
LDFLAGS = -Wl,--enable-auto-import
endif

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS) $(LIBS)

# How to find the libffi includes.
FFI_CFLAGS = $(shell pkg-config libffi --cflags)
MOD_CFLAGS += $(FFI_CFLAGS)

DISTFILES = COPYING COPYING.LESSER Makefile README debian/* \
examples/*.pure ffi.c ffi.pure
SEDFILES = README

all: ffi$(DLL)

ffi$(DLL): ffi.c
	$(CC) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) $< $(MOD_LDFLAGS) -lffi -lgmp

clean:
	rm -f *$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp ffi.pure ffi$(DLL) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/ffi.pure" "$(DESTDIR)$(libdir)/pure/ffi$(DLL)"

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
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
	wget -nv https://github.com/agraef/pure-lang/releases/download/$(dist)/$(dist).tar.gz -O $@
