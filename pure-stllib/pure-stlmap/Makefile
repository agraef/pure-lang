
pkgname = pure-stlmap
# version = 0.1

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)
libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)

MOD_CXXFLAGS = $(PIC) $(shell pkg-config pure --cflags) -I.. \
	       $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

CFLAGS = -g -O2

# Basic rules to build the module, clean, install and uninstall.

cppsource = stlmap.cpp stlmmap.cpp stlhmap.cpp
puresource = $(patsubst %.cpp, %.pure, $(cppsource))
objects = stlmap$(DLL)

all: $(objects)

$(objects): $(cppsource) 
	$(CXX) $(shared) -std=c++0x -o $@ $(MOD_CXXFLAGS) \
	$(cppsource) $(MOD_LDFLAGS) $(LIBS) -L$(installdir)
#        -Wl,-rpath="$(installdir)"\

clean:
	rm -f *.o *$(DLL)

install: $(puresource) $(objects)
	test -d $(installdir) || mkdir -p $(installdir)
	cp $^ $(installdir)

uninstall:
	rm -f $(addprefix $(installdir)/, $(puresource) $(objects))

check:
	cd uts; pure -I$(installdir) -L$(installdir) -x check_uts.pure uts_*

# Make distribution files in $(DIST)/$(pkgname)

distfiles = README Makefile *.hpp *.cpp *.pure uts/* examples/*
sedfiles = README
dist = $(DIST)/$(pkgname)
date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@stlmapversion@?$(stlmap_version)?g" \
                -e "s?@stllibversion@?$(stllib_version)?g" \
                -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir -p $(dist)/uts
	mkdir -p $(dist)/examples
	for x in $(distfiles); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(sedfiles); do rm -f $(dist)/$$x; \
                 $(call datesubst,$$PWD/$$x,$(dist)/$$x); done

