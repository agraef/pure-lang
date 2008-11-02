
# Package name and version number:
dist = pd-pure-$(version)
version = 0.1

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pd,%,$(shell which pd 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Install dir for the external and accompanying stuff.
pdextradir = $(prefix)/lib/pd/extra/pure

# Platform-specific stuff, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .pd_linux # .dll on Windows

DISTFILES = COPYING Makefile README pure.c examples/*.pure examples/*.pd examples/*.el

all: pure$(DLL)

pure$(DLL): pure.c
	gcc -shared -o $@ $< $(PIC) -lpure

clean:
	rm -f pure$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(pdextradir)" || mkdir -p "$(DESTDIR)$(pdextradir)"
	cp COPYING README Makefile pure.c pure$(DLL) examples/*.pure examples/*.pd "$(DESTDIR)$(pdextradir)"

uninstall:
	rm -rf "$(DESTDIR)$(pdextradir)"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)
