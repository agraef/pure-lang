
# Package name and version number:
dist = pure-rational-$(version)
version = 0.1

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

# Try to guess the host system type.
host = $(shell ./config.guess)

DISTFILES = COPYING Makefile rational.pure rat_interval.pure config.guess

all:

clean:

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp rational.pure rat_interval.pure "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/rational.pure" "$(DESTDIR)$(libdir)/pure/rat_interval.pure"

# Generate the documentation. This needs docutils.

rst2html = $(shell which rst2html 2>/dev/null || which rst2html.py 2>/dev/null || echo rst2html)
rst2latex = $(shell which rst2latex 2>/dev/null || which rst2latex.py 2>/dev/null || echo rst2latex)

html: pure-rational.html
tex: pure-rational.tex
pdf: pure-rational.pdf

pure-rational.html: README
	$(rst2html) $< $@

pure-rational.tex: README
	$(rst2latex) -i utf8 -o utf8 $< $@

# This also requires that you have TeX installed.

pure-rational.pdf: pure-rational.tex
	pdflatex $<
	rm -f *.aux *.log *.out

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

