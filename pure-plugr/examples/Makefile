
# Generic Makefile for VST PlugR plugins.

# Use this variable to pass extra options to pure2plugr.
#PURE2PLUGR_FLAGS =

# Installation directory. Use /usr/lib/vst or /usr/local/lib/vst for
# system-wide, ~/.vst for personal installation.
prefix = /usr/local
vstlibdir = $(prefix)/lib/vst

DLL = $(shell pkg-config pure --variable DLL)

sources = $(wildcard *.pure)
targets = $(sources:%.pure=%$(DLL))

all: $(targets)

%$(DLL): %.pure
	pure2plugr $(PURE2PLUGR_FLAGS) $<

clean:
	rm -Rf $(targets)

install: $(targets)
	rm -Rf $(addprefix $(DESTDIR)$(vstlibdir)/, $(targets))
	test -d $(DESTDIR)$(vstlibdir) || mkdir -p $(DESTDIR)$(vstlibdir)
	cp -R $(targets) $(DESTDIR)$(vstlibdir)

uninstall:
	rm -Rf $(addprefix $(DESTDIR)$(vstlibdir)/, $(targets))
