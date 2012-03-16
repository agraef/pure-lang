
pkgname = pure-stlvec
version = 0.3

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)
libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)
stlbaselib  = $(installdir)/stlbase$(DLL)
stlveclib   = $(installdir)/stlvec$(DLL)

CFLAGS = -g -O2
MOD_CXXFLAGS = $(PIC) $(shell pkg-config pure --cflags) -I.. \
	       $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

# Basic rules to build the module, clean, install and uninstall.

stlvec_source = stlvec.pure
algo_basenames = sv_modifying sv_nonmodifying sv_sort \
	         sv_merge sv_heap sv_minmax sv_numeric
algo_source = $(addsuffix .cpp, $(algo_basenames))
objects = stlvec$(DLL) stlalgorithm$(DLL)

all: $(objects)

stlvec$(DLL): stlvec.cpp
	$(CXX) $(shared) -o $@ $(MOD_CXXFLAGS) \
           stlvec.cpp $(MOD_LDFLAGS) $(LIBS) -L$(installdir) \
           $(stlbaselib)

stlalgorithm$(DLL): $(algo_source) stlvec$(DLL)
	test -d $(installdir) || mkdir -p $(installdir)
	cp -p stlvec$(DLL) $(installdir) 
	$(CXX) $(shared) -o $@ $(MOD_CXXFLAGS) \
           $(algo_source) $(MOD_LDFLAGS) $(LIBS) -L$(installdir) \
           $(stlbaselib) $(stlveclib)

clean:
	rm -f *.o *$(DLL)

install: $(stlvec_source) $(objects)
	test -d $(installdir) || mkdir -p $(installdir)
	cp -r $^ stlvec $(installdir) 

uninstall:
	rm -rf $(addprefix $(installdir)/, $(stlvec_source) $(objects) stlvec)


# Make distribution files in $(DIST)/$(pkgname)

distfiles = README Makefile *.hpp *.cpp *.pure stlvec/* ut/* examples/*
sedfiles = README
dist = $(DIST)/$(pkgname)
date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" \
                -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir -p $(dist)/stlvec
	mkdir -p $(dist)/ut
	mkdir -p $(dist)/examples
	for x in $(distfiles); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(sedfiles); do rm -f $(dist)/$$x; \
                 $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
