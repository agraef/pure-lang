
pkgname = pure-stlvec
version = 0.4

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)
libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)


MOD_CXXFLAGS = $(PIC) $(shell pkg-config pure --cflags) -I.. \
	       $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

CFLAGS = -g -O2 -Wall -Wextra
ifneq "$(DLL)" ".so"
# additional libraries to be linked (Mac/Windows only)
EXTRA_LIBS = ../stlbase$(DLL)
STLVECLIB = stlvec$(DLL)
endif
ifeq "$(DLL)" ".dylib"
DLL_FLAGS = -install_name "$(libdir)/pure/$@"
endif

# Basic rules to build the module, clean, install and uninstall.

puresource = stlvec.pure
algo_basenames = sv_modifying sv_nonmodifying sv_sort \
	         sv_merge sv_heap sv_minmax sv_numeric
algo_source = $(addsuffix .cpp, $(algo_basenames))
objects = stlvec$(DLL) stlalgorithm$(DLL)

all: $(objects)

stlvec$(DLL): stlvec.cpp 
	$(CXX) $(shared) $(DLL_FLAGS) -o $@ $(MOD_CXXFLAGS) \
        stlvec.cpp $(MOD_LDFLAGS) $(EXTRA_LIBS) $(LIBS)

stlalgorithm$(DLL): $(algo_source) 
	$(CXX) $(shared) -o $@ $(MOD_CXXFLAGS) \
        $(algo_source) $(MOD_LDFLAGS) $(EXTRA_LIBS) $(STLVECLIB) $(LIBS)

clean:
	rm -f *.o *$(DLL)

install: $(puresource) $(objects)
	test -d $(installdir) || mkdir -p $(installdir)
	cp -r $^ stlvec $(installdir) 

uninstall:
	rm -rf $(addprefix $(installdir)/, $(puresource) $(objects) stlvec)

check:
	cd ut; pure -I$(installdir) -L$(installdir) -x ut_all.pure


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
