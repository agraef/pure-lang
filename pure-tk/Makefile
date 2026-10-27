
# Package name and version number:
dist = pure-tk-$(version)
version = 0.5

# compiler flags -- these can be overridden from the command line if needed,
# as well as CPPFLAGS, LDFLAGS and LIBS
CFLAGS = -O3
#CFLAGS = -g -O2

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

prefix      = $(shell pkg-config pure --variable prefix)
bindir      = $(prefix)/bin
libdir      = $(shell pkg-config pure --variable libdir)

ifeq ($(DLL),.dylib)
dllname = -install_name "$(libdir)/pure/$@"
endif

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS) $(LIBS)

# Try to guess the Tcl version.
tclvers = $(shell echo 'puts $$tcl_version' | $(firstword $(foreach t,tclsh tclsh8.6 tclsh8.7 tclsh9.0,$(shell command -v $(t) 2>/dev/null)) tclsh))
# OS X: Use this instead to link against the native (Apple) Tcl/Tk.
#tclvers = $(shell echo 'puts $$tcl_version' | /usr/bin/tclsh)
ifeq ($(strip $(tclvers)),)
tclvers = 8.6
endif

# Try to figure out where the Tcl/Tk includes and libraries are. FreeBSD
# names the packages tk86 and tcl86, and installs the headers in versioned
# directories.
tk_pc = $(firstword $(foreach p,tk tk$(subst .,,$(tclvers)) tk$(tclvers),$(shell pkg-config --exists $(p) 2>/dev/null && echo $(p))))
tk_includes = $(shell pkg-config $(or $(tk_pc),tcl) --cflags-only-I 2>/dev/null)
tk_libs = $(shell pkg-config $(or $(tk_pc),tcl) --libs-only-L 2>/dev/null)
# The libraries themselves: from pkg-config if it knows Tk, else by version.
tk_link = $(if $(tk_pc),$(shell pkg-config $(tk_pc) --libs-only-l),-ltcl$(tclvers) -ltk$(tclvers))
# OS X: Use this instead to link against the native (Apple) Tcl/Tk.
#tk_includes = -I/System/Library/Frameworks/Tcl.framework/Headers -I/System/Library/Frameworks/Tk.framework/Headers
#tk_libs = -framework tcl -framework tk

MOD_CFLAGS += $(tk_includes)
MOD_LDFLAGS += $(tk_libs)

DISTFILES = COPYING Makefile README tk.c tk.pure gnocl.pure \
debian/* examples/Makefile examples/*.pure examples/*.tcl examples/*.glade
SEDFILES = README

all: tk$(DLL)

tk$(DLL): tk.c
	$(CC) $(shared) $(dllname) -o $@ -Wall $(MOD_CFLAGS) $< $(MOD_LDFLAGS) $(tk_link)

clean:
	rm -f *$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp gnocl.pure tk.pure tk$(DLL) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/gnocl.pure" "$(DESTDIR)$(libdir)/pure/tk.pure" "$(DESTDIR)$(libdir)/pure/tk$(DLL)"

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(SEDFILES); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@
