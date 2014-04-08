
dist = rst-markdown-$(version)
version = 0.1
prefix=/usr/local

all: rst-markdown rst-markdown.1

# This includes the awk scripts in the shell script, to make the script
# self-contained.
rst-markdown: rst-markdown.in rst-pre.awk rst-post.awk
	cat $^ > $@
	chmod a+x $@

# Targets to build the documentation in Markdown, html and pdf format.
markdown: rst-markdown.md
html: rst-markdown.html
pdf: rst-markdown.pdf

rst-markdown.md: README rst-markdown
	./rst-markdown -c $< -o $@

rst-markdown.1: README rst-markdown
	./rst-markdown -s -c -n -T -tman $< | sed -e 's/ä/ae/g' -e 's/<<aggraef@gmail.com>>/<aggraef@gmail.com>/g' > $@

%.html: %.md
	pandoc -s -S $< -o $@

PDF_FORMAT = -V "geometry:paperwidth=21cm" -V "geometry:paperheight=29.7cm" -V "geometry:vmargin=2cm" -V "geometry:hmargin=2cm" -V "fontsize:12pt" --latex-engine=xelatex

%.pdf: %.md
	pandoc -s -S $(PDF_FORMAT) $< -o $@

clean:
	rm -f rst-markdown *.md *.1 *.html *.pdf

install: rst-markdown rst-markdown.1
	cp rst-markdown "$(DESTDIR)$(prefix)/bin"
	cp rst-markdown.1 "$(DESTDIR)$(prefix)/share/man/man1"

uninstall:
	rm -f "$(DESTDIR)$(prefix)/bin/rst-markdown"
	rm -f "$(DESTDIR)$(prefix)/share/man/man1/rst-markdown.1"

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
