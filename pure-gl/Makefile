
# This needs GNU make. Really.

# Package name and version number:
dist = pure-gl-$(version)
version = 0.8

# Try to guess the installation prefix:
prefix = $(patsubst %/bin/pure$(EXE),%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

# Try to guess the host system type.
host = $(shell ./config.guess)

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared
LinkGL = -lGL -lglut -ldl

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows
EXE = .exe
DLL = .dll
LinkGL = -lopengl32 -lglu32
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX (untested)
DLL = .dylib
shared = -dynamiclib
LinkGL = -framework GLUT -framework OpenGL
endif
ifneq "$(findstring x86_64-,$(host))" ""
# 64 bit, needs -fPIC flag
PIC = -fPIC
endif

# Default CFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
# These can be set from the command line as usual. Use CFLAGS, CPPFLAGS and
# LDFLAGS for compiler (-O etc.), preprocessor (-I etc.) and linker (-L etc.) 
# options, respectively. LIBS is to be used for additional libraries to be
# linked (-l etc.).

CFLAGS = -g -O2

# No need to edit below this line, usually.

# Generated stuff:
modules = GL.pure GL_ARB.pure GL_EXT.pure GL_NV.pure GL_ATI.pure \
GLU.pure GLUT.pure
c-modules = $(patsubst %.pure,%.c,$(modules))

DISTFILES = COPYING Makefile README config.guess gl.templ debian/* \
examples/*.pure examples/*.jpg examples/flexi-line/*.pure \
GL/all_gl.h GL/all_gl_freeglut.h $(modules) $(c-modules)
SEDFILES = README

.PHONY: all clean realclean generate install uninstall dist distcheck

all: pure-gl$(DLL)

pure-gl$(DLL): $(c-modules)
	$(CC) $(shared) $(CPPFLAGS) $(CFLAGS) $(PIC) $(c-modules) -o $@ $(LDFLAGS) $(LinkGL) -lpure $(LIBS)

# Clean, but keep generated modules.

clean:
	rm -f *$(DLL) *~ *.a *.o

# Clean all, including generated modules. NOTE: This will delete stuff that
# needs special tools to rebuild (see below), so only do this if you know what
# you're doing.

realclean: clean
	rm -f $(modules) $(c-modules)

# Regenerate the Pure and C modules. The same caveats apply as for 'make
# realclean'. Note that we don't do this automatically in case of changes, to
# avoid regenerating stuff (which needs special tools) if the timestamps are
# botched, which might be the case when using svn sources.

generate: gl.templ $(wildcard GL/*.h)
	rm -f pure-gl$(DLL) *.o $(modules) $(c-modules)
	$(MAKE) all

# Install targets.

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp $(modules) pure-gl$(DLL) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/"GL*.pure "$(DESTDIR)$(libdir)/pure/pure-gl$(DLL)"

# Roll a distribution tarball.

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/examples $(dist)/examples/flexi-line $(dist)/GL
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(SEDFILES); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)

# Debian packaging

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@

#############################################################################
# Generator stuff. You only need this if you want to regenerate the wrappers.
# You need pure-gen and the OpenGL headers to do this.
#############################################################################

# Path to the installed OpenGL headers (gl.h, glext.h, glu.h, glut.h):
# Note: You can also use your own custom headers. Either adjust the path below
# accordingly, or place them into the GL subdirectory of the pure-gl sources.
glpath = /usr/include

# Use this if you have freeglut and want the freeglut extensions (default):
source = GL/all_gl_freeglut.h
# Use this if you only want the vanilla GLUT stuff:
#source = GL/all_gl.h

# OpenGL proper
# Note: NVidia extensions must be filtered out using '[_a-z]NV', just 'NV'
# will also catch the GL_TEXTURE_ENV constant.
GL.pure: GL.c
GL.c:
	pure-gen -I. -I$(glpath) $(source) -fc -t gl.templ -p GL -m GL -s './GL/*.h,$(glpath)/GL/*.h::^(GL_[A-Z_0-9]+|gl[A-Z].*)$$' -x '(EXT|ARB|[_a-z]NV|ATI|APPLE|SGI|SGIS|SGIX|3DFX|SUN|SUNX|MESA|INTEL|IBM|GREMEDY|HP|PGI|INGR|S3TC|OML|OES)$$' -o $(@:.c=.pure) -c $@ -lpure-gl

# ARB extensions
GL_ARB.pure: GL_ARB.c
GL_ARB.c:
	pure-gen -I. -I$(glpath) $(source) -fc -t gl.templ -p GL -m GL -s './GL/*.h,$(glpath)/GL/*.h::^(GL_[A-Z_0-9]+ARB|gl[A-Z].*ARB)$$' -o $(@:.c=.pure) -c $@ -lpure-gl

# EXT extensions
GL_EXT.pure: GL_EXT.c
GL_EXT.c:
	pure-gen -I. -I$(glpath) $(source) -fc -t gl.templ -p GL -m GL -s './GL/*.h,$(glpath)/GL/*.h::^(GL_[A-Z_0-9]+EXT|gl[A-Z].*EXT)$$' -o $(@:.c=.pure) -c $@ -lpure-gl

# NV extensions
GL_NV.pure: GL_NV.c
GL_NV.c:
	pure-gen -I. -I$(glpath) $(source) -fc -t gl.templ -p GL -m GL -s './GL/*.h,$(glpath)/GL/*.h::^(GL_[A-Z_0-9]+_NV|gl[A-Z].*NV)$$' -o $(@:.c=.pure) -c $@ -lpure-gl

# ATI extensions
GL_ATI.pure: GL_ATI.c
GL_ATI.c:
	pure-gen -I. -I$(glpath) $(source) -fc -t gl.templ -p GL -m GL -s './GL/*.h,$(glpath)/GL/*.h::^(GL_[A-Z_0-9]+ATI|gl[A-Z].*ATI)$$' -o $(@:.c=.pure) -c $@ -lpure-gl

# GLU
GLU.pure: GLU.c
GLU.c:
	pure-gen -I. -I$(glpath) $(source) -fc -t gl.templ -p GLU -m GLU -s './GL/*.h,$(glpath)/GL/*.h::^(GLU_[A-Z_0-9]+|glu[A-Z].*)$$' -o $(@:.c=.pure) -c $@ -lpure-gl

# GLUT
GLUT.pure: GLUT.c
GLUT.c:
	pure-gen -I. -I$(glpath) $(source) -fc -t gl.templ -p GLUT -m GLUT -s './GL/*.h,$(glpath)/GL/*.h::^(GLUT_[A-Z_0-9]+|glut[A-Z].*)$$' -o $(@:.c=.pure) -c $@ -lpure-gl
