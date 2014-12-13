
dist = pure-plugr-$(version)
version = 0.1

# compiler flags (these can be overridden)
CFLAGS = -O3
#CFLAGS = -g -O2

# Check for some common locations of the VST SDK files. This falls back to
# /usr/local/src/vstsdk if none of these are found. In that case, or if make
# picks the wrong location, you can also set the SDK variable explicitly.
sdkpaths = /usr/local/include /usr/local/src /usr/include /usr/src
sdkpat = vst* VST*
#SDK = /usr/local/src/vstsdk
SDK = $(firstword $(wildcard $(foreach path,$(sdkpaths),$(addprefix $(path)/,$(sdkpat)))) /usr/src/vstsdk)
# Steinberg's distribution zip has the SDK source files in the
# public.sdk/source/vst2.x subdirectory, while some Linux packages (e.g.,
# steinberg-vst on the AUR) keep them directly under $(SDK).
#SDKSRC = $(SDK)/public.sdk/source/vst2.x
SDKSRC = $(firstword $(patsubst %/,%,$(dir $(wildcard $(addsuffix vstplugmain.cpp,$(SDK)/ $(SDK)/public.sdk/source/vst2.x/)))) $(SDK)/public.sdk/source/vst2.x)

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

prefix      = $(shell pkg-config pure --variable prefix)
bindir      = $(prefix)/bin
libdir      = $(shell pkg-config pure --variable libdir)
purelibdir  = $(libdir)/pure

ifeq ($(DLL),.dylib)
dllname = -install_name "$(purelibdir)/$@"
endif

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS)

all: pure2plugr plugr$(DLL)

# This sets the proper SDK paths in the pure2plugr script, detected at
# build time.
pure2plugr: pure2plugr.in
	sed -e 's?@SDK@?$(SDK)?g;s?@SDKSRC@?$(SDKSRC)?g' < $< > $@
	chmod a+x $@

plugr$(DLL): plugr.cc
	$(CXX) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) $< $(MOD_LDFLAGS) $(LIBS)

clean:
	rm -rf *$(DLL)* *.o pure2plugr

install:
	test -d "$(DESTDIR)$(purelibdir)" || mkdir -p "$(DESTDIR)$(purelibdir)"
	cp plugr$(DLL) plugr.pure pureplugin.cc pureplugin.hh "$(DESTDIR)$(purelibdir)"
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
	cp pure2plugr "$(DESTDIR)$(bindir)"

uninstall:
	rm -f $(addprefix "$(DESTDIR)$(purelibdir)/", plugr$(DLL) plugr.pure pureplugin.cc pureplugin.hh) "$(DESTDIR)$(bindir)/pure2plugr"

DISTFILES = COPYING COPYING.LESSER Makefile README.md plugr.cc plugr.pure \
pureplugin.cc pureplugin.hh pure2plugr.in examples/Makefile examples/*.pure

dist:
	rm -rf $(dist)
	for x in $(dist) $(dist)/examples; do mkdir $$x; done
	for x in $(DISTFILES); do ln -sf "$$PWD"/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar -czhf $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar -xzf $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
