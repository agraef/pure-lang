
dist = pure-lv2-$(version)
version = 0.1

# compiler flags (these can be overridden)
CFLAGS = -O3
#CFLAGS = -g -O2

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

all: lv2$(DLL)

lv2$(DLL): lv2.c
	gcc $(shared) $(dllname) -o $@ $(MOD_CFLAGS) -std=c99 $< $(MOD_LDFLAGS) $(LIBS)

clean:
	rm -rf *$(DLL)*

install:
	test -d "$(DESTDIR)$(purelibdir)" || mkdir -p "$(DESTDIR)$(purelibdir)"
	cp lv2$(DLL) lv2.pure lv2pure.c lv2pure.h "$(DESTDIR)$(purelibdir)"
	test -d "$(DESTDIR)$(bindir)" || mkdir -p "$(DESTDIR)$(bindir)"
	cp pure2lv2 "$(DESTDIR)$(bindir)"

uninstall:
	rm -f $(addprefix "$(DESTDIR)$(purelibdir)/", lv2$(DLL) lv2.pure lv2pure.c lv2pure.h) "$(DESTDIR)$(bindir)/pure2lv2"

DISTFILES = COPYING Makefile README lv2.c lv2.pure lv2pure.c lv2pure.h \
pure2lv2 *.ttl examples/Makefile examples/*.pure

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
