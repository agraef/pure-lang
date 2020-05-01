
# Package name and version number:
dist = pure-gen-$(version)
version = 0.23

# Installation goes into the following directories. You can also set these
# directly instead of $(prefix). In addition, packagers may want to set the
# DESTDIR variable for a staged install.

# Note that if you want to change the installation prefix, you'll have to set
# set the 'prefix' variable *both* at build and installation time, because the
# path to dump-ast gets hard-coded into the pure-gen executable.

# Alternatively, you can also set the DUMP_AST shell environment variable at
# runtime to tell pure-gen about the right path to the dump-ast executable.

prefix = $(shell pkg-config pure --variable prefix)
bindir = $(prefix)/bin
libdir = $(shell pkg-config pure --variable libdir)

pkgdatadir = $(libdir)/pure-gen
mandir = ${prefix}/share/man
man1dir = $(mandir)/man1

# Set GHCFLAGS to whatever additional ghc compilation flags are required on
# your platform. In particular, you'll want to set the linkage type to either
# -dynamic or -static, or leave it empty for the default (usually -static).
# Note that some systems (Arch) only ship with dynamic Haskell libraries these
# days. (But see below for static linkage on Arch.)
GHCFLAGS = -dynamic
#GHCFLAGS = -static

# Try to guess the host system type and do platform-specific setup.
host = $(shell ./config.guess)
gcc = gcc
ifneq "$(findstring -mingw,$(host))" ""
# Windows
EXE = .exe
# Windows uses the official packages which use static linking by default.
GHCFLAGS =
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX: Try to find a suitable gcc version to be used as the C preprocessor.
# The following assumes that a recent gcc version from MacPorts is installed.
# Note that we *really* need gcc (4.3+) here, pure-gen does *not* work when
# using clang (the system compiler on newer OS X versions) instead.
gcc = $(lastword gcc $(sort $(wildcard /opt/local/bin/gcc-mp-*)))
endif
ifneq "$(findstring x86_64-,$(host))" ""
# 64 bit, needs -fPIC flag
PIC = -fPIC
endif

DISTFILES = COPYING ChangeLog Makefile config.guess README README.dump-ast \
dump-ast.hs dump-ast-old.hs pure-gen.pure pure-gen.1 pure-gen.txt run-tests \
debian/* examples/*.templ language-c/*

# Always just do the maintainer build by default now. If you want the old
# default build target, run 'make all' instead.
default: maintainer-build

all: pure-gen dump-ast

# NOTE: This requires Pure 0.21 or later.

dump_ast = $(pkgdatadir)/dump-ast

pure-gen: pure-gen.pure
	sed -e 's?@version@?$(version)?' -e 's?@gcc@?$(gcc)?' -e 's?./dump-ast?$(dump_ast)?' < $< > xx$<
	pure $(PIC) $(PUREC_FLAGS) -c xx$< -o $@
	rm -f xx$<

# NB: This requires ghc (http://www.haskell.org/ghc/, version 6 or later) and
# language-c (https://hackage.haskell.org/package/language-c, version 0.4 or
# 0.5), please check the README for details. Any language-c versions later
# than 0.5 most likely won't work because of backward-incompatible changes in
# the library. (If you can figure out which changes are needed in dump-ast to
# compile against the latest version, please let me know!)

# If a suitable language-c version is not readily available, you can run this
# to install the included language-c 0.5.0 locally using cabal.
cabal-install:
	cd language-c && cabal v1-update && cabal v1-install

# These options work around quirks with various ghc versions.
GHCXFLAGS += -XTypeSynonymInstances -XFlexibleInstances
dump-ast: dump-ast.hs
	ghc $(GHCFLAGS) $(GHCXFLAGS) --make $< -o $@

# NB: If you're still running a really old language-c version like 0.3, then
# try compiling dump-ast-old.hs instead, it might work.

# dump-ast: dump-ast.hs dump-ast-old.hs
# 	ghc $(GHCFLAGS) $(GHCXFLAGS) --make $< -o $@ || \
# 	ghc $(GHCFLAGS) $(GHCXFLAGS) --make dump-ast-old.hs -o $@

# Alternative self-contained static build using the included language-c
# source, to make things easier for package maintainers. This requires that
# your ghc supports static linkage. NOTE: On Arch you'll have to install
# ghc-static and ghc-pristine (AUR) and make sure that
# /usr/share/ghc-pristine/bin comes first on PATH to make this work.
# Cf. https://wiki.archlinux.org/index.php/haskell#Static_linking.

maintainer-build:
# Install locally into the build directory.
	cd language-c && HOME=$(shell pwd)/cabal cabal v1-update && HOME=$(shell pwd)/cabal cabal v1-install
# Make sure to do a static build here, since the cabal-installed language-c
# will be gone once we're finished.
	$(MAKE) all GHCFLAGS=-static HOME=$(shell pwd)/cabal
# Clean up.
	HOME=$(shell pwd)/cabal ghc-pkg unregister language-c
	rm -rf cabal language-c/dist

# Same as above, but without using cabal, which won't work on Launchpad where
# we don't have a working Internet connection.
deb-maintainer-build:
# Install locally into the build directory.
	cd language-c && mkdir -p $(shell pwd)/cabal && HOME=$(shell pwd)/cabal runhaskell Setup.hs configure --ghc --prefix=$(shell pwd)/cabal && HOME=$(shell pwd)/cabal runhaskell Setup.hs build && HOME=$(shell pwd)/cabal runhaskell Setup.hs install --user
# Make sure to do a static build here, since the locally installed language-c
# will be gone once we're finished.
	$(MAKE) all GHCFLAGS=-static HOME=$(shell pwd)/cabal
# Clean up.
	HOME=$(shell pwd)/cabal ghc-pkg unregister language-c
	rm -rf cabal language-c/dist

# Minimal test suite:

check:
	./run-tests $(gcc)

clean:
	rm -f pure-gen$(EXE) dump-ast$(EXE) *~ *.hi *.o
	rm -rf cabal language-c/dist

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
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/examples && mkdir $(dist)/language-c
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist)/language-c/dist
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
	wget -nv https://github.com/agraef/pure-lang/releases/download/pure-gen-$(version)/$(dist).tar.gz -O $@
