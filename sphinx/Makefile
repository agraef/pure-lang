# Makefile for Sphinx documentation
#

rst_sources = pure/pure.txt pure/purelib.txt faust2pd/faust2pd.txt gnumeric-pure/gnumeric-pure.txt pd-pure/pd-pure.txt pure-audio/pure-audio.txt pure-csv/pure-csv.txt pure-doc/pure-doc.txt pure-fastcgi/pure-fastcgi.txt pure-faust/pure-faust.txt pure-ffi/pure-ffi.txt pure-g2/pure-g2.txt pure-gen/pure-gen.txt pure-gl/pure-gl.txt pure-gplot/pure-gplot.txt pure-gsl/pure-gsl.txt pure-gtk/pure-gtk.txt pure-liblo/pure-liblo.txt pure-midi/pure-midi.txt pure-octave/pure-octave.txt pure-odbc/pure-odbc.txt pure-readline/pure-readline.txt pure-sockets/pure-sockets.txt pure-sql3/pure-sql3.txt pure-tk/pure-tk.txt pure-xml/pure-xml.txt

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         =
BUILDDIR      = _build

# Internal variables.
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

.PHONY: all sources help clean cleandocs realclean html dirhtml singlehtml pickle json htmlhelp qthelp devhelp epub latex latexpdf text man changes linkcheck doctest

all:
	@rm -rf $(dir $(rst_sources)) $(notdir $(rst_sources)) 
	@make sources
	@cp */*.txt .
	@rm -rf $(dir $(rst_sources))

sources: $(rst_sources)

%.txt:
	@(test -d $(dir $@) || mkdir -p $(dir $@))
	@(test -f ../$@ && mv ../$@ $@.bak; if $(MAKE) -C ../$(dir $@) $(notdir $@) pure-doc="pure-doc -s" >/dev/null 2>&1; then echo "creating $(notdir $@) from: ../$@"; ./fixdoc < ../$@ > $@; test -f $@.bak && mv $@.bak ../$@ || (echo "deleting ../$@"; rm ../$@); elif test -f $@.bak; then mv $@.bak ../$@; echo "creating $(notdir $@) from: ../$@"; ./fixdoc < ../$@ > $@; elif test -f ../$(dir $@)/README; then echo "creating $(notdir $@) from: ../$(dir $@)README"; ./fixdoc < ../$(dir $@)README > $@; else echo "no source for $(notdir $@)"; fi)

cleandocs:
	rm -rf $(notdir $(rst_sources)) 

realclean: clean cleandocs
	rm -rf auto _build

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  html       to make standalone HTML files"
	@echo "  dirhtml    to make HTML files named index.html in directories"
	@echo "  singlehtml to make a single large HTML file"
	@echo "  pickle     to make pickle files"
	@echo "  json       to make JSON files"
	@echo "  htmlhelp   to make HTML files and a HTML help project"
	@echo "  qthelp     to make HTML files and a qthelp project"
	@echo "  devhelp    to make HTML files and a Devhelp project"
	@echo "  epub       to make an epub"
	@echo "  latex      to make LaTeX files, you can set PAPER=a4 or PAPER=letter"
	@echo "  latexpdf   to make LaTeX files and run them through pdflatex"
	@echo "  text       to make text files"
	@echo "  man        to make manual pages"
	@echo "  changes    to make an overview of all changed/added/deprecated items"
	@echo "  linkcheck  to check all external links for integrity"
	@echo "  doctest    to run all doctests embedded in the documentation (if enabled)"

clean:
	-rm -rf $(BUILDDIR)/*

html:
	$(SPHINXBUILD) -Q -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html

dirhtml:
	$(SPHINXBUILD) -Q -b dirhtml $(ALLSPHINXOPTS) $(BUILDDIR)/dirhtml

singlehtml:
	$(SPHINXBUILD) -Q -b singlehtml $(ALLSPHINXOPTS) $(BUILDDIR)/singlehtml

pickle:
	$(SPHINXBUILD) -Q -b pickle $(ALLSPHINXOPTS) $(BUILDDIR)/pickle

json:
	$(SPHINXBUILD) -Q -b json $(ALLSPHINXOPTS) $(BUILDDIR)/json

htmlhelp:
	$(SPHINXBUILD) -Q -b htmlhelp $(ALLSPHINXOPTS) $(BUILDDIR)/htmlhelp

qthelp:
	$(SPHINXBUILD) -Q -b qthelp $(ALLSPHINXOPTS) $(BUILDDIR)/qthelp

devhelp:
	$(SPHINXBUILD) -Q -b devhelp $(ALLSPHINXOPTS) $(BUILDDIR)/devhelp

epub:
	$(SPHINXBUILD) -Q -b epub $(ALLSPHINXOPTS) $(BUILDDIR)/epub

latex:
	$(SPHINXBUILD) -Q -b latex $(ALLSPHINXOPTS) $(BUILDDIR)/latex

latexpdf:
	$(SPHINXBUILD) -Q -b latex $(ALLSPHINXOPTS) $(BUILDDIR)/latex
	make -C $(BUILDDIR)/latex all-pdf

text:
	$(SPHINXBUILD) -Q -b text $(ALLSPHINXOPTS) $(BUILDDIR)/text

man:
	$(SPHINXBUILD) -Q -b man $(ALLSPHINXOPTS) $(BUILDDIR)/man

changes:
	$(SPHINXBUILD) -Q -b changes $(ALLSPHINXOPTS) $(BUILDDIR)/changes

linkcheck:
	$(SPHINXBUILD) -b linkcheck $(ALLSPHINXOPTS) $(BUILDDIR)/linkcheck

doctest:
	$(SPHINXBUILD) -b doctest $(ALLSPHINXOPTS) $(BUILDDIR)/doctest
