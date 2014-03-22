
# Package name and version number:
dist = pure-csv-$(version)
version = 1.5

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
# OSX: hardcode the installation path into the module, so that the module can
# be linked in by the batch compiler.
dllname = -install_name "$(libdir)/pure/$@"
endif

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS) $(LIBS)

DISTFILES = COPYING Makefile README config.guess csv.c csv.pure run-tests debian/* test/*
SEDFILES = README

all: csv$(DLL)

csv$(DLL): csv.c
	$(CC) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) $< $(MOD_LDFLAGS)

clean:
	rm -Rf *$(DLL)* *~ *.a *.o

# documentation (this needs docutils)

rst2html = $(shell which rst2html 2>/dev/null || which rst2html.py 2>/dev/null || echo rst2html)
rst2latex = $(shell which rst2latex 2>/dev/null || which rst2latex.py 2>/dev/null || echo rst2latex)

html: pure-csv.html
latex: pure-csv.tex
pdf: pure-csv.pdf

pure-csv.html: README
	$(rst2html) $< $@

pure-csv.tex: README
	$(rst2latex) $< $@

pure-csv.pdf: pure-csv.tex
	pdflatex $<
	pdflatex $<
	rm -r *.aux *.log *.out

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp csv.pure csv$(DLL) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/csv.pure" "$(DESTDIR)$(libdir)/pure/csv$(DLL)"

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/test
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(SEDFILES); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make check && make install DESTDIR=./BUILD
	rm -rf $(dist)

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@

# Examples and tests.

testdir = test
tests = $(wildcard $(testdir)/*.pure)
logs = $(patsubst %.pure, %.log, $(tests))

check:
	./run-tests

cleanlogs:
	rm -f $(logs)

logs: $(logs)

%.log: %.pure csv.pure csv$(DLL)
	(cd $(testdir) && pure -v -I.. -L.. < $(notdir $<) > $(notdir $@) 2>&1)
