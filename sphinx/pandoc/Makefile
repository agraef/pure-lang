
dist = pure-pandoc-$(version)
version = 0.1
prefix=/usr/local

all: pure-pandoc pure-pandoc.1

# This includes the awk scripts in the shell script, to make the script
# self-contained.
pure-pandoc: pure-pandoc.in rst-pre.awk rst-post.awk
	cat $^ > $@
	chmod a+x $@

# Targets to build the documentation in Markdown, html and pdf format.
markdown: pure-pandoc.md
html: pure-pandoc.html
pdf: pure-pandoc.pdf

# If you need any special pandoc options to format the Markdown, html and pdf
# output then you can specify with the MD_FLAGS, HTML_FLAGS and PDF_FLAGS
# variables, respectively. E.g., to produce a MultiMarkdown document suitable
# for viewing with `Marked` on the Mac, the option `MD_FLAGS=-tmarkdown_mmd`
# might be used.
MD_FLAGS   = -tmarkdown
HTML_FLAGS = -thtml
PDF_FLAGS  = -V "geometry:paperwidth=21cm" -V "geometry:paperheight=29.7cm" -V "geometry:vmargin=2cm" -V "geometry:hmargin=2cm" -V "fontsize:12pt" --latex-engine=xelatex

pure-pandoc.md: README pure-pandoc
	./pure-pandoc -c -O "$(MD_FLAGS)" $< -o $@

pure-pandoc.1: README pure-pandoc
	./pure-pandoc -s -c -n -T -tman $< | sed -e 's/ä/ae/g' -e 's/<<aggraef@gmail.com>>/<aggraef@gmail.com>/g' > $@

%.html: %.md
	pandoc -s -S $(HTML_FLAGS) $< -o $@

%.pdf: %.md
	pandoc -s -S $(PDF_FLAGS) $< -o $@

clean:
	rm -f pure-pandoc *.md *.1 *.html *.pdf

install: pure-pandoc pure-pandoc.1
	cp pure-pandoc "$(DESTDIR)$(prefix)/bin"
	cp pure-pandoc.1 "$(DESTDIR)$(prefix)/share/man/man1"

uninstall:
	rm -f "$(DESTDIR)$(prefix)/bin/pure-pandoc"
	rm -f "$(DESTDIR)$(prefix)/share/man/man1/pure-pandoc.1"

DISTFILES = Makefile README pure-pandoc.in rst-post.awk rst-pre.awk
SEDFILES = README

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir $(dist)
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(SEDFILES); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)
