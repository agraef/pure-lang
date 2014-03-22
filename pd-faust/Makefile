
# This is a GNU makefile. Please also check Makefile.config for customizable
# setup information and Makefile.common for the actual compilation rules.

# Normally you just run 'make' and then 'make install' to build and install
# the package. The usual standard targets to clean (clean, distclean,
# realclean) and roll a distribution tarball (dist, distcheck) are supported
# as well. 'make install' also honors DESTDIR so that staged installs can be
# done conveniently by package maintainers.

# Installation goes to extra/faust in your Pd library directory by default.
# The Makefile tries to detect the location of your Pd installation. You can
# change this by setting any of the prefix, libdir, pdlibdir and pdextradir
# variables on the command line, see below for the defaults.

# If you'd like to experiment with Faust bitcode modules which can be loaded
# natively by the Pure interpreter, you also need to run 'make bitcode' before
# 'make install'. Please check pdfaust.pure for details.

include Makefile.config

# Package name and version:
dist = pd-faust-$(version)
version = 0.7

DISTFILES = COPYING COPYING.LESSER Makefile Makefile.config.in Makefile.config \
Makefile.common README TODO config.guess *.pure etc/*.el etc/*.pd \
debian/* examples/*.pd examples/midi/*.mid examples/osc/*.osc \
examples/dsp/README* examples/dsp/Makefile \
examples/dsp/*.dsp examples/dsp/*.lib examples/dsp/*.h \
lib/Makefile lib/*.c lib/*.dsp lib/*.pure
SEDFILES = README

subdirs = lib examples/dsp

all:
	for x in $(subdirs); do $(MAKE) -C $$x all; done

bitcode:
	for x in $(subdirs); do $(MAKE) -C $$x bitcode; done

clean:
	for x in $(subdirs); do $(MAKE) -C $$x clean; done

distclean:
	for x in $(subdirs); do $(MAKE) -C $$x distclean; done

realclean:
	for x in $(subdirs); do $(MAKE) -C $$x realclean; done

install:
	test -d "$(DESTDIR)$(pdextradir)" || mkdir -p "$(DESTDIR)$(pdextradir)"
	cp COPYING COPYING.LESSER README TODO *.pure examples/*.pd etc/*.el lib/pdfaust*$(PDDLL) "$(DESTDIR)$(pdextradir)"
	cp -R examples/dsp examples/midi examples/osc "$(DESTDIR)$(pdextradir)"
	test -d "$(DESTDIR)$(pdextradir)/lib" || mkdir -p "$(DESTDIR)$(pdextradir)/lib"
	cp lib/util.* lib/xfade.* "$(DESTDIR)$(pdextradir)/lib"

uninstall:
	rm -rf "$(DESTDIR)$(pdextradir)"

Makefile.config: Makefile.config.in Makefile
	sed -e "s?@version@?$(version)?g" < $< > $@

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	for x in $(dist) $(addprefix $(dist)/, debian etc lib examples $(addprefix examples/, dsp midi osc)); do mkdir $$x; done
	for x in $(DISTFILES); do ln -sf "$$PWD"/$$x $(dist)/$$x; done
	for x in $(SEDFILES); do rm -f $(dist)/$$x; $(call datesubst,"$$PWD"/$$x,$(dist)/$$x); done
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
