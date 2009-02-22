
# Package name and version number:
dist = pure-gen-$(version)
version = 0.2

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into the folloiwing directories. You can also set these
# directly instead of $(prefix). In addition, packagers may want to set the
# DESTDIR variable for a staged install.
bindir = $(prefix)/bin
pkgdatadir = $(prefix)/lib/pure-gen-$(version)
mandir = ${prefix}/share/man
man1dir = $(mandir)/man1

DISTFILES = COPYING Makefile README dump-ast.hs pure-gen.pure pure-gen.1 examples/*.templ

all: dump-ast

# NOTE: To rebuild this, you need ghc (http://www.haskell.org/ghc/) and
# language-c version 0.3.1 or later
# (http://hackage.haskell.org/cgi-bin/hackage-scripts/package/language-c).

dump-ast: dump-ast.hs
	ghc --make $< -o $@

# Documentation in various formats (this requires additional tools).

html: pure-gen.html
dvi: pure-gen.dvi
ps: pure-gen.ps
pdf: pure-gen.pdf

%.html: %.1
	rman -f HTML $< | sed -e 's/dq/\"/g' -e '/^<br>$$/d' > $@

#%.html: %.1
#	groff -man -Thtml $< > $@

%.dvi: %.1
	groff -man -Tdvi $< > $@

%.ps: %.1
	groff -man -Tps $< > $@

%.pdf: %.1
	groff -man -Tps $< | ps2pdf - $@

clean:
	rm -f dump-ast *~ *.hi *.o *.dvi *.pdf *.ps *.html

install:
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
	test -d "$(DESTDIR)$(pkgdatadir)" || mkdir -p "$(DESTDIR)$(pkgdatadir)"
	test -d "$(DESTDIR)$(man1dir)" || mkdir -p "$(DESTDIR)$(man1dir)"
	cp dump-ast "$(DESTDIR)$(pkgdatadir)"
	sed -e 's?/usr/local/bin/pure?$(bindir)/pure?' -e 's?@version@?$(version)?' -e 's?./dump-ast?$(pkgdatadir)/dump-ast?' < pure-gen.pure > "$(DESTDIR)$(pkgdatadir)/pure-gen.pure"
	chmod a+x "$(DESTDIR)$(pkgdatadir)/dump-ast" "$(DESTDIR)$(pkgdatadir)/pure-gen.pure"
	ln -sf "$(pkgdatadir)/pure-gen.pure" "$(DESTDIR)$(bindir)/pure-gen"
	cp pure-gen.1 "$(DESTDIR)$(man1dir)"

uninstall:
	rm -rf "$(DESTDIR)$(bindir)/pure-gen" "$(DESTDIR)$(pkgdatadir)" "$(DESTDIR)$(man1dir)/pure-gen.1"

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
