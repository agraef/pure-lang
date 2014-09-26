
pkgname = pure-stlmap
version = 0.4

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)
libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)

MOD_CXXFLAGS = $(PIC) $(shell pkg-config pure --cflags) -I.. \
	       $(CXXFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

CXXFLAGS = -g -O2 -Wall -Wextra
ifneq "$(DLL)" ".so"
# additional libraries to be linked (Mac/Windows only)
EXTRA_LIBS = ../stlbase$(DLL)
endif
ifeq "$(DLL)" ".dylib"
DLL_FLAGS = -install_name "$(libdir)/pure/$@"
endif

# Basic rules to build the modules, clean, install and uninstall.

cppsource = stlmap.cpp stlmmap.cpp stlhmap.cpp
puresource = $(patsubst %.cpp, %.pure, $(cppsource))
objects = $(patsubst %.cpp, %$(DLL), $(cppsource))

all: $(objects)

%$(DLL) : %.cpp
	$(CXX) $(shared) $(DLL_FLAGS) -std=c++0x $< -o $@ $(MOD_CXXFLAGS) \
	$(MOD_LDFLAGS) $(EXTRA_LIBS) $(LIBS)

clean:
	rm -Rf *.o *$(DLL)*

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
datesubst = sed -e "s?@version@?$(version)?g" \
                -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir -p $(dist)/uts
	mkdir -p $(dist)/examples
	for x in $(distfiles); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(sedfiles); do rm -f $(dist)/$$x; \
                 $(call datesubst,$$PWD/$$x,$(dist)/$$x); done

