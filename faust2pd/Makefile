
# Package name and version number:
dist = faust2pd-$(version)
version = 2.0

# Try to guess the Pure installation prefix:
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Try to guess the Pd installation prefix:
pdprefix = $(patsubst %/bin/pd,%,$(shell which pd 2>/dev/null))
ifeq ($(strip $(pdprefix)),)
# Fall back to /usr/local.
pdprefix = /usr/local
endif

bindir = $(prefix)/bin
libdir = $(prefix)/lib
datadir = $(libdir)/pure
appdir = $(datadir)/faust2pd

# where to install the Pd stuff
pdlibdir = $(pdprefix)/lib
pddatadir = $(pdlibdir)/pd/extra

DESTDIR=

DISTFILES = Makefile COPYING ChangeLog README faust2pd.html config.guess *.pure *.pd examples/README examples/*/Makefile examples/*/*.dsp examples/*/*.pd examples/faust/*.xml examples/seqdemo/*.pure examples/seqdemo/*.xml examples/synth/*.xml

all:
	for x in basic faust synth seqdemo; do make -C examples/$$x all; done

clean:
	for x in basic faust synth seqdemo; do make -C examples/$$x clean; done

distclean:
	rm -f *.txt *.tex *.pdf
	for x in basic faust synth seqdemo; do make -C examples/$$x distclean; done

realclean:
	rm -f *.txt *.tex *.pdf
	for x in basic faust synth seqdemo; do make -C examples/$$x realclean; done

# install the faust2pd script and accompanying files
install:
	test -d $(DESTDIR)$(appdir) || mkdir -p $(DESTDIR)$(appdir)
	cp faustxml.pure $(DESTDIR)$(appdir)
	sed -e 's?#! /usr/local/bin/pure?#! $(bindir)/pure?g' < faust2pd.pure > $(DESTDIR)$(appdir)/faust2pd.pure
	chmod a+x $(DESTDIR)$(appdir)/faust2pd.pure
	test -d $(DESTDIR)$(bindir) || mkdir -p $(DESTDIR)$(bindir)
	ln -sf $(appdir)/faust2pd.pure $(DESTDIR)$(bindir)/faust2pd
	@echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
	@echo "Optionally, you can also run make install-pd to install the auxiliary Faust"
	@echo "abstractions in $(DESTDIR)$(pddatadir)."
	@echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

# install the auxiliary Faust abstractions (faust-*.pd)
install-pd:
	test -d $(DESTDIR)$(pddatadir) || mkdir -p $(DESTDIR)$(pddatadir)
	cp faust-*.pd $(DESTDIR)$(pddatadir)

#uninstall
uninstall:
	rm -rf $(DESTDIR)$(bindir)/faust2pd $(DESTDIR)$(appdir)
	rm -f $(DESTDIR)$(pddatadir)/faust-*.pd

# roll distribution tarball
dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples
	for x in basic faust seqdemo synth; do mkdir $(dist)/examples/$$x; done
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install install-pd DESTDIR=./BUILD
	rm -rf $(dist)

# Generate the documentation. This needs docutils.

sources = faustxml.pure
target = faust2pd

.PHONY: html tex pdf

html: $(target).html
tex: $(target).tex
pdf: $(target).pdf

$(target).txt: README $(sources)
	pure-doc $(sources) | cat README - > $@

%.html: %.txt
	rst2html.py $< $@

%.tex: %.txt
	rst2latex.py $< $@

# This also requires that you have TeX installed.

%.pdf: %.tex
	pdflatex $<
	rm -f *.aux *.log *.out
