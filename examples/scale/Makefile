
# Package name and version number:
dist = scale-$(version)
version = 0.1

# Default installation prefix.
prefix = /usr/local

bindir = $(prefix)/bin
docdir = $(prefix)/share/doc/scale

DISTFILES = COPYING Makefile README scale.html *.pure intnam.par *.m *.tcl \
scl/*.scl

GTK_LIBS = $(shell pkg-config --libs gtk+-2.0)

all: scale

scale: scale.pure scale.tcl scale.m factor.pure hrm.pure nomen.pure mds.pure intnam.par
	pure -c $< -o $@

clean:
	rm -f scale *~

# documentation (this needs docutils)

rst2html = $(shell which rst2html 2>/dev/null || which rst2html.py 2>/dev/null || echo rst2html)
rst2latex = $(shell which rst2latex 2>/dev/null || which rst2latex.py 2>/dev/null || echo rst2latex)

html: scale.html
latex: scale.tex
pdf: scale.pdf

scale.html: README
	$(rst2html) $< $@

scale.tex: README
	$(rst2latex) $< $@

scale.pdf: scale.tex
	pdflatex $<
	pdflatex $<
	rm -f *.aux *.log *.out

install:
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
	cp scale "$(DESTDIR)$(bindir)"
	test -d "$(DESTDIR)$(docdir)" || mkdir -p "$(DESTDIR)$(docdir)"
	cp scale.html "$(DESTDIR)$(docdir)"

uninstall:
	rm -rf "$(DESTDIR)$(bindir)/scale" "$(DESTDIR)$(docdir)"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/scl
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
