
# Try to guess the installation prefix (this needs GNU make). NOTE: The EXE
# and PIC variables are imported from the main Makefile.
prefix = $(patsubst %/bin/pure$(EXE),%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

bindir = $(prefix)/bin
libdir = $(prefix)/lib
pkgdatadir = $(libdir)/pure-gnm

all: pure-gnm

template = $(pkgdatadir)/plugin.xml

pure-gnm$(EXE): pure-gnm.pure
	sed -e 's?plugin.xml?$(template)?' < $< > xx$<
	pure $(PIC) $(PUREC_FLAGS) -c xx$< -o $@
	rm -f xx$<

clean:
	rm -f pure-gnm$(EXE) *~ *.o

install:
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
	test -d "$(DESTDIR)$(pkgdatadir)" || mkdir -p "$(DESTDIR)$(pkgdatadir)"
	cp pure-gnm$(EXE) "$(DESTDIR)$(bindir)"
	cp plugin.xml "$(DESTDIR)$(pkgdatadir)"

uninstall:
	rm -rf "$(DESTDIR)$(bindir)/pure-gnm$(EXE)" "$(DESTDIR)$(pkgdatadir)"
