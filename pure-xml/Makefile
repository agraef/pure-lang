
# Package name and version number:
dist = pure-xml-$(version)
version = 0.6

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure$(EXE),%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

pureinc = -I$(prefix)/include
purelib = -L$(prefix)/lib -lpure

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib
datadir = $(libdir)/pure

# Try to guess the host system type.
host = $(shell ./config.guess)

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows
EXE = .exe
DLL = .dll
XMLLIBS = -lxslt -lxml2
XMLFLAGS = 
# This is needed to link with GMP.
#LDFLAGS = -Wl,--enable-auto-import
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX (untested)
DLL = .dylib
shared = -dynamiclib
# OSX: hardcode installation path into the dll.
dllname = -install_name "$(datadir)/xml$(DLL)"
endif
ifneq "$(findstring x86_64-,$(host))" ""
# 64 bit, needs -fPIC flag
PIC = -fPIC
endif
ifeq "$(findstring -mingw,$(host))" ""
XMLLIBS = $(shell xml2-config --libs) $(shell xslt-config --libs)
XMLFLAGS = $(shell xml2-config --cflags)
endif

# Default CFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
# These can be set from the command line as usual. Use CFLAGS, CPPFLAGS and
# LDFLAGS for compiler (-O etc.), preprocessor (-I etc.) and linker (-L etc.) 
# options, respectively. LIBS is to be used for additional libraries to be
# linked (-l etc.).

CFLAGS = -g -O2

FLAGS = $(CPPFLAGS) $(CFLAGS) $(PIC) $(LDFLAGS)

DISTFILES = COPYING COPYING.LESSER Makefile README config.guess \
xml.c xml.pure debian/* examples/*.pure examples/*.xml examples/*.xsl
SEDFILES = README

all: xml$(DLL)

xml$(DLL): xml.c
	$(CC) $(shared) $(dllname) -o $@ $< $(XMLFLAGS) $(FLAGS) $(pureinc) $(XMLLIBS) $(purelib) $(LIBS)

clean:
	rm -f *$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(datadir)" || mkdir -p "$(DESTDIR)$(datadir)"
	cp xml.pure xml$(DLL) "$(DESTDIR)$(datadir)"

uninstall:
	rm -f "$(DESTDIR)$(datadir)/xml.pure" "$(DESTDIR)$(datadir)/xml$(DLL)"

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

# Generate the documentation.

sources = xml.pure

pure-xml.txt: README $(sources)
	pure-doc -is $(sources) | cat README - > $@
