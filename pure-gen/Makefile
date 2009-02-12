
# Package name and version number:
dist = pure-gen-$(version)
version = 0.1

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(prefix)/bin and $(prefix)/lib/pure-gen-$(version),
# you can also set this directly instead of $(prefix). In addition, packagers
# may want to set the DESTDIR variable for a staged install.
bindir = $(prefix)/bin
pkgdatadir = $(prefix)/lib/pure-gen-$(version)

DISTFILES = COPYING Makefile README dump-ast.hs pure-gen.pure examples/*.templ

all: dump-ast

# NOTE: To rebuild this, you need ghc (http://www.haskell.org/ghc/) and
# language-c version 0.3.1 or later
# (http://hackage.haskell.org/cgi-bin/hackage-scripts/package/language-c).

dump-ast: dump-ast.hs
	ghc --make $< -o $@

clean:
	rm -f dump-ast *~ *.hi *.o

install:
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
	test -d "$(DESTDIR)$(pkgdatadir)" || mkdir -p "$(DESTDIR)$(pkgdatadir)"
	cp dump-ast "$(DESTDIR)$(pkgdatadir)"
	sed -e 's?/usr/local/bin/pure?$(bindir)/pure?' -e 's?@version@?$(version)?' -e 's?./dump-ast?$(pkgdatadir)/dump-ast?' < pure-gen.pure > "$(DESTDIR)$(pkgdatadir)/pure-gen.pure"
	chmod a+x "$(DESTDIR)$(pkgdatadir)/dump-ast" "$(DESTDIR)$(pkgdatadir)/pure-gen.pure"
	ln -sf "$(pkgdatadir)/pure-gen.pure" "$(DESTDIR)$(bindir)/pure-gen"

uninstall:
	rm -rf "$(DESTDIR)$(bindir)/pure-gen" "$(DESTDIR)$(pkgdatadir)"

dist:
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
