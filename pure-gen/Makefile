
# Package name and version number:
dist = pure-gen-$(version)
version = 0.20

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure$(EXE),%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into the following directories. You can also set these
# directly instead of $(prefix). In addition, packagers may want to set the
# DESTDIR variable for a staged install.

# Note that if you want to change the installation prefix, you'll have to set
# set the 'prefix' variable *both* at build and installation time, because the
# path to dump-ast gets hard-coded into the pure-gen executable.

# Alternatively, you can also set the DUMP_AST shell environment variable at
# runtime to tell pure-gen about the right path to the dump-ast executable.

bindir = $(prefix)/bin
libdir = $(prefix)/lib
pkgdatadir = $(libdir)/pure-gen
mandir = ${prefix}/share/man
man1dir = $(mandir)/man1

# Try to guess the host system type and do platform-specific setup.
host = $(shell ./config.guess)
gcc = gcc
ifneq "$(findstring -mingw,$(host))" ""
# Windows
EXE = .exe
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX: Try to find a suitable gcc version to be used as the C preprocessor.
# The following assumes that a recent gcc version from MacPorts is installed.
# Note that we *really* need gcc (4.3+) here, pure-gen does *not* work when
# using clang (the system compiler on newer OS X versions) instead.
gcc = $(lastword gcc $(sort $(wildcard /opt/local/bin/gcc-mp-*)))
endif

DISTFILES = COPYING ChangeLog Makefile config.guess README README.dump-ast \
dump-ast.hs dump-ast-old.hs pure-gen.pure pure-gen.1 pure-gen.txt run-tests \
debian/* examples/*.templ

all: pure-gen dump-ast

# NOTE: This requires Pure 0.21 or later.

dump_ast = $(pkgdatadir)/dump-ast

pure-gen: pure-gen.pure
	sed -e 's?@version@?$(version)?' -e 's?@gcc@?$(gcc)?' -e 's?./dump-ast?$(dump_ast)?' < $< > xx$<
	pure $(PUREC_FLAGS) -c xx$< -o $@
	rm -f xx$<

# NOTE: This requires ghc (http://www.haskell.org/ghc/) and language-c
# (http://hackage.haskell.org/cgi-bin/hackage-scripts/package/language-c),
# version 0.4.2 or later. If the compilation fails, it's probably because
# Language.C 0.3.x is used; in this case we patch up the source accordingly
# and try again. If you know that you have 0.3.x and want to avoid this extra
# step then you can patch up the source beforehand.

# FIXME: As of Language.C 0.4.2, -XTypeSynonymInstances is needed to make the
# dump-ast source compile. We should figure out which source changes are
# needed to do without this kludge.

GHCFLAGS = -XTypeSynonymInstances -XFlexibleInstances
dump-ast: dump-ast.hs dump-ast-old.hs
	ghc $(GHCFLAGS) --make $< -o $@ || \
	ghc $(GHCFLAGS) --make dump-ast-old.hs -o $@

# Minimal test suite:

check:
	./run-tests $(gcc)

clean:
	rm -f pure-gen$(EXE) dump-ast$(EXE) *~ *.hi *.o

install:
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
	test -d "$(DESTDIR)$(pkgdatadir)" || mkdir -p "$(DESTDIR)$(pkgdatadir)"
	test -d "$(DESTDIR)$(man1dir)" || mkdir -p "$(DESTDIR)$(man1dir)"
	cp dump-ast$(EXE) "$(DESTDIR)$(pkgdatadir)"
	cp pure-gen$(EXE) "$(DESTDIR)$(bindir)"
# The binaries are huge, so better strip them.
	strip "$(DESTDIR)$(pkgdatadir)/dump-ast$(EXE)" "$(DESTDIR)$(bindir)/pure-gen$(EXE)"
ifeq "$(findstring -mingw,$(host))" ""
	chmod a+x "$(DESTDIR)$(pkgdatadir)/dump-ast$(EXE)" "$(DESTDIR)$(bindir)/pure-gen$(EXE)"
endif
	cp pure-gen.1 "$(DESTDIR)$(man1dir)"

uninstall:
	rm -rf "$(DESTDIR)$(bindir)/pure-gen$(EXE)" "$(DESTDIR)$(pkgdatadir)" "$(DESTDIR)$(man1dir)/pure-gen.1"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make check && make install DESTDIR=./BUILD
	rm -rf $(dist)

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@
