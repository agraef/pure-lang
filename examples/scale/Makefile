
# Package name and version number:
dist = scale-$(version)
version = 0.1

# Default installation prefix.
prefix = /usr/local

bindir = $(prefix)/bin

DISTFILES = COPYING Makefile README scale.html *.pure intnam.par *.m *.tcl \
scl/*.scl

GTK_LIBS = $(shell pkg-config --libs gtk+-2.0)

all: scale

scale: scale.pure scale.tcl scale.m factor.pure hrm.pure nomen.pure mds.pure intnam.par
	pure -c $< -o $@

clean:
	rm -f scale *~

# documentation (this needs docutils)

html: scale.html
latex: scale.tex
pdf: scale.pdf

scale.html: README
	rst2html.py $< $@

scale.tex: README
	rst2latex.py $< $@

scale.pdf: scale.tex
	pdflatex $<
	pdflatex $<
	rm -f *.aux *.log *.out

install:
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
	cp scale "$(DESTDIR)$(bindir)"

uninstall:
	rm -f "$(DESTDIR)$(bindir)/scale"

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
