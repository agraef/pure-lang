
# Package name and version number:
dist = pure-g2-$(version)
version = 0.3

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

# This must be set to the prefix under which g2 is installed. (You'll only
# need this to compile the examples and regenerate the wrapper, however.)
g2prefix = /usr
g2libdir = $(g2prefix)/lib
includedir = $(g2prefix)/include
includefile = $(includedir)/g2.h

DISTFILES = COPYING Makefile README g2.pure g2_*.pure penguin.pure debian/* g2-0.72-sharedlib.patch
SEDFILES = README

%: %.pure
	pure -L$(g2libdir) -c $^ -o $@ -fPIC

examples = g2_test g2_life

all:
	@echo "Run 'make install' to install, 'make examples' to compile the"
	@echo "the examples, and 'make generate' to regenerate the wrapper."
	@echo "See the README file for details."

examples: $(examples)

clean:
	rm -f $(examples) *.ps *.eps *.emf *.fig *.jpg *.png

# Run this to regenerate the wrapper.

generate:
	rm -f g2.pure
	pure-gen -s "$(includefile);" -x "^(linux|unix)$$" -o g2.tmp "$(includefile)" -llibg2
	for x in $(includedir)/g2_*.h; do pure-gen -s "$$x;" -x "^(linux|unix)$$" -o g2.tmp -N "$$x"; done
# This prototype needs cosmetic changes.
	sed -e 's/g2_open_X11X(int, int, int, int, char\*, char\*, char\*, int, int)/g2_open_X11X(int, int, int, int, char\*, char\*, void\*, int, int)/' < g2.tmp > g2.pure
	rm -f g2.tmp

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp g2.pure "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/g2.pure"

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(SEDFILES); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make generate examples && make install DESTDIR=./BUILD
	rm -rf $(dist)

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@
