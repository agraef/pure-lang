
# Package name and version number:
dist = scale-$(version)
version = 0.3

# Default installation prefix.
prefix = /usr/local

bindir = $(prefix)/bin
docdir = $(prefix)/share/doc/scale

DISTFILES = COPYING Makefile README.md scale.html *.pure intnam.par *.m *.tcl \
scl/*.scl

GTK_LIBS = $(shell pkg-config --libs-only-L --libs-only-l gtk+-2.0)

all: scale

scale: scale.pure scale.tcl scale.m factor.pure hrm.pure nomen.pure mds.pure intnam.par
	pure -c $< -o $@

clean:
	rm -f scale *~

# documentation (this needs pandoc)

PDF_FLAGS = -V "geometry:paperwidth=21cm" -V "geometry:paperheight=29.7cm" \
-V "geometry:vmargin=2cm" -V "geometry:hmargin=2cm" -V "fontsize:12pt"

html: scale.html
pdf: scale.pdf

scale.html: README.md
	pandoc -s -S $< -o $@

scale.pdf: README.md
	pandoc -s -S $< -o $@ $(PDF_FLAGS)

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
