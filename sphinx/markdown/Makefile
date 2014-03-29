
prefix=/usr/local

pre = $(shell cat rst-pre.awk)
post = $(shell cat rst-post.awk)

# This includes the awk scripts in the shell script, to make the script
# self-contained.
rst-markdown: rst-markdown.in rst-pre.awk rst-post.awk
	cat $^ > $@
	chmod a+x $@

clean:
	rm -f rst-markdown

install:
	cp rst-markdown "$(DESTDIR)$(prefix)/bin"

uninstall:
	rm -f "$(DESTDIR)$(prefix)/bin/rst-markdown"
