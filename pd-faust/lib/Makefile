
override srcdir = ..
include $(srcdir)/Makefile.common

override CPPFLAGS += -I$(pdincludedir)

solibs     := util$(DLL) pdstub$(DLL)
somodules  := $(dsp_source:.dsp=.so)
bcmodules  := $(dsp_source:.dsp=.bc)

# The xfade dsp is needed by all effect plugins, preallocate some units.
$(somodules): CXXFLAGS += -DNVOICES=16

all: $(solibs) $(somodules) pdfaust$(PDDLL)
bitcode: $(bclibs) $(bcmodules) pdfaust2$(PDDLL)

# We need the location of some of the installed Pure library modules.
purelib = $(shell pkg-config pure --variable libdir)/pure

# pure-faust precompiled module.

# NOTE: This requires a linker that handles the -rpath option. Otherwise you
# may have to add the Pure library directory to your system dll search path so
# that the linked library modules are found when the pdfaust module is loaded
# by Pd.

extralibs = -Wl,-rpath=$(purelib) $(addprefix $(purelib)/, orddict$(DLL) faust$(DLL) midifile$(DLL))

pdfaust$(PDDLL): pdfaust.o pdloader.o util.o
	rm -f pdfaust$(PDDLL)
	gcc $(PIC) $(shared) $^ $(extralibs) -o pdfaust$(DLL)
	test "$(DLL)" = "$(PDDLL)" || mv pdfaust$(DLL) pdfaust$(PDDLL)

pdfaust2$(PDDLL): pdfaust2.o pdloader2.o util.o
	rm -f pdfaust2$(PDDLL)
	gcc $(PIC) $(shared) $^ $(extralibs) -o pdfaust2$(DLL)
	test "$(DLL)" = "$(PDDLL)" || mv pdfaust2$(DLL) pdfaust2$(PDDLL)

pdfaustsrc = $(addprefix ../, fdsp~.pure fsynth~.pure midiseq.pure oscseq.pure)
pdfaustdeps = $(wildcard ../*.pure)

pdfaust.o: $(pdfaustdeps)
	pure $(PIC) -I.. -L.. -c $(pdfaustsrc) -o $@ --main __pdfaust_main__

pdfaust2.o: $(pdfaustdeps)
	pure $(PIC) -I.. -L.. -c $(pdfaustsrc) -o $@ --main __pdfaust_main__ --enable faust2

pdloader.o: pdloader.c
	gcc $(ALL_CFLAGS) -DVERSION="\"$(version)\"" -c $< -o $@

pdloader2.o: pdloader.c
	gcc $(ALL_CFLAGS) -DVERSION="\"$(version)\"" -DFAUST2 -c $< -o $@

util.o: util.c
	gcc $(ALL_CFLAGS) -c $< -o $@

# Faust-specific stuff
svg: $(svg)
xml: $(xml)

clean:
	rm -f $(solibs) $(somodules) $(bclibs) $(bcmodules) *.o pdfaust$(PDDLL) pdfaust2$(PDDLL)

distclean: clean
	rm -Rf $(csource) $(cppsource) $(asmsource) $(svg) $(xml)

realclean: distclean
