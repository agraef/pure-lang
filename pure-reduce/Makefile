
modname = pure-reduce
modversion = 0.1

# Platform-specific setup.

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

# Default installation directory. DESTDIR is supported for staging purposes to
# make package maintainers happy.

libdir      = $(shell pkg-config pure --variable libdir)
installdir  = $(addprefix $(DESTDIR), $(libdir)/pure)

# Special flags and objects needed to build the reduce dll pilfered from
# reduce-algebra/csl/new-embedded/procedural/.

CFLAGS=-O2 -I. -DPAGE_BITS=19 -DHAVE_CONFIG_H=1 -DEMBEDDED=1 $(PIC)

objects = arith01.o arith02.o arith03.o arith04.o arith05.o arith06.o arith07.o arith08.o arith09.o arith10.o arith11.o arith12.o bytes.o char.o driver.o embedcsl.o cslmpi.o cslread.o eval1.o eval2.o eval3.o eval4.o fasl.o fns1.o fns2.o fns3.o fwin.o gc.o preserve.o print.o restart.o sysfwin.o termed.o stubs.o

all: reduce.img reduce$(DLL)

reduce.img: reduce-algebra/csl/new-embedded/reduce.img
	cp $< .

reduce-algebra/csl/new-embedded/reduce.img:
	$(MAKE) -C reduce-algebra/csl/new-embedded

reduce$(DLL): $(addprefix reduce-algebra/csl/new-embedded/procedural/, $(objects))
	$(CC) $(shared) $(PIC) -o reduce$(DLL) $^

$(addprefix reduce-algebra/csl/new-embedded/procedural/, $(objects)):
	$(MAKE) -C reduce-algebra/csl/new-embedded/procedural CFLAGS="$(CFLAGS)"

logs: tests.log

tests.log: tests.pure
	pure tests.pure | tail -n +2 > tests.log

check:
	pure tests.pure | tail -n +2 | diff tests.log -

clean:
	$(MAKE) -C reduce-algebra/csl/new-embedded/procedural clean
	$(MAKE) -C reduce-algebra/csl/new-embedded clean
	rm -f reduce.img reduce$(DLL)

install:
	test -d $(installdir) || mkdir -p $(installdir)
	cp reduce.pure $(installdir)
	for x in reduce$(DLL) reduce.img; do test -f $$x && cp $$x $(installdir) || true; done

uninstall:
	rm -f $(addprefix $(installdir)/, reduce.pure reduce$(DLL) reduce.img)

# Roll a distribution tarball.

DISTFILES = README Makefile reduce.pure reduce_examp.pure tests.pure tests.log pure_reduce.txt

dist = $(modname)-$(modversion)

dist: $(DISTFILES)
	rm -rf $(dist)
	mkdir $(dist)
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)
