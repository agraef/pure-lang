
# Package name and version number:
dist = pure-gen-$(version)
version = 0.15

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
ifneq "$(findstring -mingw,$(host))" ""
# Windows
EXE = .exe
endif

DISTFILES = COPYING ChangeLog Makefile config.guess README README.dump-ast \
dump-ast.hs pure-gen.pure pure-gen.1 pure-gen.txt run-tests examples/*.templ

all: pure-gen dump-ast

# NOTE: This requires Pure 0.21 or later.

dump_ast = $(pkgdatadir)/dump-ast

pure-gen: pure-gen.pure
	sed -e 's?@version@?$(version)?' -e 's?./dump-ast?$(dump_ast)?' < $< > xx$<
	pure -c xx$< -o $@
	rm -f xx$<

# NOTE: This requires ghc (http://www.haskell.org/ghc/) and language-c
# (http://hackage.haskell.org/cgi-bin/hackage-scripts/package/language-c),
# version 0.4.2 or later.

# FIXME: As of Language.C 0.4.2, -XTypeSynonymInstances is needed to make the
# dump-ast source compile. We should figure out which source changes are
# needed to do without this kludge.

dump-ast: dump-ast.hs
	ghc -XTypeSynonymInstances -XFlexibleInstances --make $< -o $@

# Minimal test suite:

check:
	./run-tests

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
	mkdir $(dist) && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make check && make install DESTDIR=./BUILD
	rm -rf $(dist)
