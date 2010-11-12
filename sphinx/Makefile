
# This requires GNU make. No, really.

# Package name and version number:
dist = pure-docs-$(version)
version = 0.1

# Try to guess the installation prefix:
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

all: docs

help:
	@printf "\nJust 'make' grabs new rst sources and builds the html and pdf files.\n"
	@printf "Or use 'make rst' followed by 'make <target>' where <target> is one of:\n\n"
	@printf "  %-12s%s\n" \
	  html       "to make standalone HTML files" \
	  dirhtml    "to make HTML files named index.html in directories" \
	  singlehtml "to make a single large HTML file" \
	  pickle     "to make pickle files" \
	  json       "to make JSON files" \
	  htmlhelp   "to make HTML files and a HTML help project" \
	  qthelp     "to make HTML files and a qthelp project" \
	  devhelp    "to make HTML files and a Devhelp project" \
	  epub       "to make an epub" \
	  latex      "to make LaTeX files, you can set PAPER=a4 or PAPER=letter" \
	  latexpdf   "to make LaTeX files and run them through pdflatex" \
	  text       "to make text files" \
	  man        "to make manual pages" \
	  changes    "to make an overview of all changed/added/deprecated items" \
	  linkcheck  "to check all external links for integrity" \
	  doctest    "to run all doctests embedded in the documentation (if enabled)"
	@printf "\nOther targets of interest:\n\n"
	@printf "  %-12s%s\n" \
	  dist       "to create a distribution tarball" \
	  install    "to install the generated docs on the Pure library path" \
	  clean      "to remove the Sphinx-built files" \
	  cleanrst   "to remove intermediate rst files" \
	  realclean  "to remove all generated files"
	@printf "\nMaintainers only (this requires a checkout of the 'docs' repository):\n\n"
	@printf "  %-12s%s\n" \
	  update     "to copy the generated docs to a staging area for online viewing" \
	  update-html "to copy only the html docs to the staging area"

# The rst sources we want to use. This is pretty much the only definition that
# needs to be updated (along with the toctree in the master document
# puredoc.txt) when the set of documentation changes.

rst_sources = pure/pure.txt pure/purelib.txt faust2pd/faust2pd.txt gnumeric-pure/gnumeric-pure.txt pd-pure/pd-pure.txt pure-audio/pure-audio.txt pure-csv/pure-csv.txt pure-doc/pure-doc.txt pure-fastcgi/pure-fastcgi.txt pure-faust/pure-faust.txt pure-ffi/pure-ffi.txt pure-g2/pure-g2.txt pure-gen/pure-gen.txt pure-gl/pure-gl.txt pure-glpk/pure-glpk.txt pure-gplot/pure-gplot.txt pure-gsl/pure-gsl.txt pure-gtk/pure-gtk.txt pure-liblo/pure-liblo.txt pure-midi/pure-midi.txt pure-octave/pure-octave.txt pure-odbc/pure-odbc.txt pure-rational/pure-rational.txt pure-readline/pure-readline.txt pure-sockets/pure-sockets.txt pure-sql3/pure-sql3.txt pure-tk/pure-tk.txt pure-xml/pure-xml.txt

# Default target files. These are the ones we package. After generation, they
# are to be found in the appropriate subdirs (html, latex) of the $(BUILDDIR)
# directory.

htmlfiles = genindex.html puredoc.html $(patsubst %.txt,%.html,$(notdir $(rst_sources))) search.html searchindex.js
pdffiles = puredoc.pdf

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         =
BUILDDIR      = build

#HTMLOPTS  = -D pygments_style='pygments.styles.bw.BlackWhiteStyle'
LATEXOPTS = -D pygments_style='pygments.styles.bw.BlackWhiteStyle'

# Internal variables.
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

.PHONY: all docs rst alldocs allhtml dist install uninstall update update-html sources help clean cleanrst realclean html dirhtml singlehtml pickle json htmlhelp qthelp devhelp epub latex latexpdf text man changes linkcheck doctest

# Try to grab as many rst sources as we can, then go ahead and build the html
# and pdf targets if necessary. This requires an installation of docutils,
# Sphinx and LaTeX.

docs: alldocs
	@printf "The html and pdf docs in $(BUILDDIR)/html and $(BUILDDIR)/latex should be up to date now.\nPlease try 'make help' for a list of other available options.\n"

# This is also used internally for the 'dist' and 'update' targets.

alldocs:
	@rm -rf $(dir $(rst_sources))
	@$(MAKE) --no-print-directory sources
	@(manifest=`ls */*.txt 2>/dev/null || true`; test -n "$$manifest" && cp $$manifest .; rm -rf $(dir $(rst_sources)); true)
	@$(MAKE) --no-print-directory $(BUILDDIR)/html/puredoc.html $(BUILDDIR)/latex/puredoc.pdf

sources: conf.py $(rst_sources)

conf.py: conf.py.in Makefile
	sed -e 's/@version@/$(version)/g' < $< > $@

$(BUILDDIR)/html/puredoc.html: conf.py puredoc.txt $(notdir $(rst_sources))
	@$(MAKE) --no-print-directory html

$(BUILDDIR)/latex/puredoc.pdf: conf.py puredoc.txt $(notdir $(rst_sources))
	@$(MAKE) --no-print-directory latexpdf

# Same as above, but only update the html docs. This is used internally for
# the 'install' and 'update-html' targets.

allhtml:
	@rm -rf $(dir $(rst_sources))
	@$(MAKE) --no-print-directory sources
	@(manifest=`ls */*.txt 2>/dev/null || true`; test -n "$$manifest" && cp $$manifest .; rm -rf $(dir $(rst_sources)); true)
	@$(MAKE) --no-print-directory $(BUILDDIR)/html/puredoc.html

# Same as above, but don't run sphinx-builder yet, leave that to the user.
# This can be run without having any of the docutils/Sphinx tools installed,
# but then you won't be able to do anything with the rst files.

rst:
	@rm -rf $(dir $(rst_sources))
	@$(MAKE) --no-print-directory sources
	@(manifest=`ls */*.txt 2>/dev/null || true`; test -n "$$manifest" && cp $$manifest .; if test -n "$$manifest"; then echo "Updated rst sources; try 'make help' for a list of available options."; else echo "No updated rst sources; nothing to be done."; fi)
	@rm -rf $(dir $(rst_sources))

# A little helper function which patches up a rst document and checks whether
# it's different from what we already have (if so, then get rid of it
# immediately). $(1) is the original rst source from .., $(2) the target file
# to be built. If the directory of $(1) contains a Makefile which has a
# definition for the 'version' variable in it, we also patch up occurrences of
# '@version@' in the rst source with the appropriate version string.

check = (version=`(grep '^version[ \t]*=' $(dir $(1))/Makefile 2>/dev/null | sed -e 's/^version[ \t]*=[ \t]*\(.*\)$$/\1/' 2>/dev/null) || true`; ./fixdoc < $(1) | sed -e "s?@version@?$$version?g" > $(2)); if diff $(notdir $(2)) $(2) >/dev/null 2>&1; then rm $(2); else echo "creating $(notdir $(2)) from: $(1)"; fi

# This is actually a phony rule as the documentation we want to create might
# be up-to-date (in which case it won't be updated) or may be missing
# altogether. Note that we first create the rst source in a corresponding
# subdirectory, the doc generation targets above ('alldocs', 'allhtml' and
# 'rst') move them back up.

%.txt:
	@(test -d $(dir $@) || mkdir -p $(dir $@))
	@(test -f ../$@ && mv ../$@ $@.bak; if $(MAKE) -C ../$(dir $@) $(notdir $@) >/dev/null 2>&1; then $(call check,../$@,$@); test -f $@.bak && mv $@.bak ../$@ || rm ../$@; elif test -f $@.bak; then mv $@.bak ../$@; $(call check,../$@,$@); elif test -f ../$(dir $@)README; then $(call check,../$(dir $@)README,$@); else echo "no source for $(notdir $@)"; fi)

# Install/uninstall targets.

datadir = $(libdir)/pure/docs

# KLUDGE: Running 'make allhtml' under 'sudo' doesn't work if root doesn't
# have the required tools on its path. So we omit the 'allhtml' dependency
# here and instead assume that the user has run 'make' beforehand.

install: # allhtml
	rm -rf $(DESTDIR)$(datadir)
	test -d $(DESTDIR)$(datadir) || mkdir -p $(DESTDIR)$(datadir)
	cp -r $(addprefix $(BUILDDIR)/html/, _images _sources _static $(htmlfiles)) $(DESTDIR)$(datadir)

uninstall:
	rm -rf $(DESTDIR)$(datadir)

# Maintainers only: Copy the generated docs to a staging area for online
# viewing.

docsdir = ../../pure-lang-docs

$(docsdir)/Makefile: docs/Makefile.in Makefile
	sed -e 's/@version@/$(version)/g' < $< > $@

update: alldocs $(docsdir)/Makefile
	rm -rf $(addprefix $(docsdir)/, _images _sources _static $(htmlfiles) $(pdffiles))
	cp -r $(addprefix $(BUILDDIR)/, $(addprefix html/, _images _sources _static $(htmlfiles)) $(addprefix latex/, $(pdffiles))) $(docsdir)

# Same as above, but only update the html docs.

update-html: allhtml $(docsdir)/Makefile
	rm -rf $(addprefix $(docsdir)/, _images _sources _static $(htmlfiles))
	cp -r $(addprefix $(BUILDDIR)/html/, _images _sources _static $(htmlfiles)) $(docsdir)

# Dist target.

htmldist = _images/* _sources/* _static/* $(htmlfiles)
pdfdist = $(pdffiles)

docs/Makefile: docs/Makefile.in Makefile
	sed -e 's/@version@/$(version)/g' < $< > $@

dist: alldocs docs/Makefile
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/_images && mkdir $(dist)/_sources && mkdir $(dist)/_static
	ln -sf $$PWD/docs/Makefile $(dist)/Makefile
	(cd $(BUILDDIR)/html && for x in $(htmldist); do ln -sf $$PWD/$$x ../../$(dist)/$$x; done)
	(cd $(BUILDDIR)/latex && for x in $(pdfdist); do ln -sf $$PWD/$$x ../../$(dist)/$$x; done)
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

# Get rid of the rst source we created. You might want to run this to force a
# fresh build.

cleanrst:
	rm -rf $(notdir $(rst_sources))

# Get rid of really *everything* that we generated, and other unneccessary
# stuff that may be lying around.

realclean: clean cleanrst
	rm -rf *~ *.bak auto $(BUILDDIR) conf.py

# These are the targets generated by sphinx-quickstart, slightly massaged to
# be somewhat less verbose.

clean:
	-rm -rf $(BUILDDIR)/*

html:
	$(SPHINXBUILD) -b html $(HTMLOPTS) $(ALLSPHINXOPTS) $(BUILDDIR)/html

dirhtml:
	$(SPHINXBUILD) -b dirhtml $(HTMLOPTS) $(ALLSPHINXOPTS) $(BUILDDIR)/dirhtml

singlehtml:
	$(SPHINXBUILD) -b singlehtml $(HTMLOPTS) $(ALLSPHINXOPTS) $(BUILDDIR)/singlehtml

pickle:
	$(SPHINXBUILD) -b pickle $(ALLSPHINXOPTS) $(BUILDDIR)/pickle

json:
	$(SPHINXBUILD) -b json $(ALLSPHINXOPTS) $(BUILDDIR)/json

htmlhelp:
	$(SPHINXBUILD) -b htmlhelp $(HTMLOPTS) $(ALLSPHINXOPTS) $(BUILDDIR)/htmlhelp

qthelp:
	$(SPHINXBUILD) -b qthelp $(ALLSPHINXOPTS) $(BUILDDIR)/qthelp

devhelp:
	$(SPHINXBUILD) -b devhelp $(ALLSPHINXOPTS) $(BUILDDIR)/devhelp

epub:
	$(SPHINXBUILD) -b epub $(ALLSPHINXOPTS) $(BUILDDIR)/epub

# We have to fix a glitch in the Sphinx output here so that we can
# \usepackage[utf8x]{inputenc}. Moreover, add -Q to the build command to
# suppress the silly warnings about unknown references to local html files.

latex:
	$(SPHINXBUILD) -Q -b latex $(LATEXOPTS) $(ALLSPHINXOPTS) $(BUILDDIR)/latex
	@(cd $(BUILDDIR)/latex && sed -e 's/\\DeclareUnicodeCharacter{00A0}{\\nobreakspace}/%\\DeclareUnicodeCharacter{00A0}{\\nobreakspace}/g' < puredoc.tex > puredoc.fixed && rm -f puredoc.tex && mv puredoc.fixed puredoc.tex)

latexpdf: latex
	make -C $(BUILDDIR)/latex all-pdf

text:
	$(SPHINXBUILD) -b text $(ALLSPHINXOPTS) $(BUILDDIR)/text

man:
	$(SPHINXBUILD) -b man $(ALLSPHINXOPTS) $(BUILDDIR)/man

changes:
	$(SPHINXBUILD) -b changes $(ALLSPHINXOPTS) $(BUILDDIR)/changes

linkcheck:
	$(SPHINXBUILD) -b linkcheck $(ALLSPHINXOPTS) $(BUILDDIR)/linkcheck

doctest:
	$(SPHINXBUILD) -b doctest $(ALLSPHINXOPTS) $(BUILDDIR)/doctest
