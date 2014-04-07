
dist = rst-markdown-$(version)
version = 0.1

prefix=/usr/local

pre = $(shell cat rst-pre.awk)
post = $(shell cat rst-post.awk)

# This includes the awk scripts in the shell script, to make the script
# self-contained.
rst-markdown: rst-markdown.in rst-pre.awk rst-post.awk
	cat $^ > $@
	chmod a+x $@

# Targets to build the documentation in Markdown, html and pdf format.
markdown: README.md
html: README.html
pdf: README.pdf

README.md: README rst-markdown
	./rst-markdown -C -c $< -o $@

%.html: %.md
	pandoc -s -S $< -o $@

PDF_FORMAT = -V "geometry:paperwidth=21cm" -V "geometry:paperheight=29.7cm" -V "geometry:vmargin=2cm" -V "geometry:hmargin=2cm" -V "fontsize:12pt" --latex-engine=xelatex

%.pdf: %.md
	pandoc -s -S $(PDF_FORMAT) $< -o $@

clean:
	rm -f rst-markdown *.md *.html *.pdf

install:
	cp rst-markdown "$(DESTDIR)$(prefix)/bin"

uninstall:
	rm -f "$(DESTDIR)$(prefix)/bin/rst-markdown"

DISTFILES = Makefile README rst-markdown.in rst-post.awk rst-pre.awk
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
