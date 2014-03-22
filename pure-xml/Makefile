
# Package name and version number:
dist = pure-xml-$(version)
version = 0.6

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

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS) $(LIBS)

# Compilation options to link with XML/XSLT.
ifeq ($(DLL),.dll)
XMLLIBS = -lxslt -lxml2
XMLFLAGS = 
else
XMLLIBS = $(shell xml2-config --libs) $(shell xslt-config --libs)
XMLFLAGS = $(shell xml2-config --cflags)
endif

MOD_CFLAGS += $(XMLFLAGS)
MOD_LDFLAGS += $(XMLLIBS)

DISTFILES = COPYING COPYING.LESSER Makefile README \
xml.c xml.pure debian/* examples/*.pure examples/*.xml examples/*.xsl
SEDFILES = README

all: xml$(DLL)

xml$(DLL): xml.c
	$(CC) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) $< $(MOD_LDFLAGS)

clean:
	rm -Rf *$(DLL)* *~ *.a *.o

datadir = $(libdir)/pure

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
