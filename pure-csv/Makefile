
# Package name and version number:
dist = pure-csv-$(version)
version = 0.1

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Platform-specific stuff, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so # .dll on Windows

DISTFILES = COPYING Makefile README examples/*.pure examples/*.csv \
csv.c csv.pure

all: csv$(DLL)

csv$(DLL): csv.c
	gcc -shared -o $@ $< $(PIC) -lpure

clean:
	rm -f *$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(prefix)/lib/pure" || mkdir -p "$(DESTDIR)$(prefix)/lib/pure"
	cp csv.pure csv$(DLL) "$(DESTDIR)$(prefix)/lib/pure"

uninstall:
	rm -f "$(DESTDIR)$(prefix)/lib/pure/csv.pure" "$(DESTDIR)$(prefix)/lib/pure/csv$(DLL)"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)
