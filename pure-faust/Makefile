
# Package name and version number:
dist = pure-faust-$(version)
version = 0.11

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
# OSX: hardcode the installation path into the module, so that the module can
# be linked in by the batch compiler.
dllname = -install_name "$(libdir)/pure/$@"
endif

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS) $(LIBS)

DISTFILES = COPYING COPYING.LESSER Makefile README \
faust.cc faust.pure faust2.pure \
debian/* examples/Makefile examples/config.guess \
examples/pure.c examples/pure.cpp \
examples/*.pure examples/*.dsp
SEDFILES = README

all: faust$(DLL)

faust$(DLL): faust.o
	$(CXX) $(shared) $(dllname) -o $@ $< $(MOD_LDFLAGS) -lltdl

faust.o: faust.cc
	$(CXX) $(MOD_CFLAGS) -o $@ -c $<

clean:
	rm -Rf faust$(DLL)* *~ *.a *.o

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp faust$(DLL) faust.pure faust2.pure "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure"/faust$(DLL) "$(DESTDIR)$(libdir)/pure"/faust.pure "$(DESTDIR)$(libdir)/pure"/faust2.pure

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
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@
