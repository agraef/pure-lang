
# Package name and version number:
dist = pure-doc-$(version)
version = 0.1

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(bindir), you can also set this directly instead of
# $(prefix).
bindir = $(prefix)/bin

DISTFILES = COPYING Makefile README pure-doc.ll pure-doc.cc

all: pure-doc

pure-doc: pure-doc.cc

pure-doc.cc: pure-doc.ll
	flex -o $@ $<

clean:
	rm -f pure-doc *.html *.pdf *.tex

install:
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
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

html: pure-doc.html
latex: pure-doc.tex
pdf: pure-doc.pdf

pure-doc.html: README
	rst2html.py $< $@

pure-doc.tex: README
	rst2latex.py $< $@

pure-doc.pdf: pure-doc.tex
	pdflatex $<
	rm -f *.aux *.log *.out
