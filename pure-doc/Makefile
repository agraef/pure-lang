
# Package name and version number:
dist = pure-doc-$(version)
version = 0.6

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(bindir) and $(datadir), you can also set this
# directly instead of $(prefix).
bindir = $(prefix)/bin
libdir = $(prefix)/lib
datadir = $(libdir)/pure

DISTFILES = COPYING Makefile README fixdoc pure-doc.ll pure-doc.cc

all: pure-doc

pure-doc: pure-doc.cc

pure-doc.cc: pure-doc.ll
	flex -o $@ $<

clean:
	rm -f pure-doc *.pdf *.tex *.aux *.log *.out

realclean: clean
	rm -f *.html

install:
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
	test -d "$(DESTDIR)$(datadir)" || mkdir -p "$(DESTDIR)$(datadir)"
	cp pure-doc "$(DESTDIR)$(bindir)"

uninstall:
	rm -f "$(DESTDIR)$(bindir)/pure-doc"

dist:
	rm -rf $(dist)
	mkdir $(dist)
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
