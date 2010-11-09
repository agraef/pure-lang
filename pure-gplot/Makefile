
# Package name and version number:
dist = pure-gplot-$(version)
version = 0.1

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

distfiles = COPYING COPYING.LESSER Makefile gplot.pure gplot_test.pure

all:
	@echo "Try 'make install' to install, 'make uninstall' to uninstall the package."

clean:

pure-doc = pure-doc

pure-gplot.txt: gplot.pure
	$(pure-doc) $< > $@

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp gplot.pure "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/gplot.pure"

dist:
	rm -rf $(dist)
	mkdir $(dist)
	for x in $(distfiles); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)
