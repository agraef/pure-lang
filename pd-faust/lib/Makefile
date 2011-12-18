
override srcdir = ..
include $(srcdir)/Makefile.common

solibs     := mix$(DLL) pdstub$(DLL)
somodules  := $(dsp_source:.dsp=.so)
bcmodules  := $(dsp_source:.dsp=.bc)

# The xfade dsp is needed by all effect plugins, preallocate some units.
$(somodules): CXXFLAGS += -DNVOICES=16

all: $(solibs) $(somodules) pdfaust$(PDDLL)
bitcode: $(bclibs) $(bcmodules)

# We need the location of some of the installed Pure library modules.
purelib = $(shell pkg-config pure --variable libdir)/pure

# pure-faust precompiled module.

# NOTE: This requires a linker that handles the -rpath option. Otherwise you
# may have to add the Pure library directory and the lib subdir of the
# pd-faust installation directory to your system dll search path so that the
# linked library modules are found when the pdfaust module is loaded by Pd.

pdfaust$(PDDLL): pdfaust.o pdloader.o
	rm -f pdfaust$(PDDLL)
	gcc $(PIC) $(shared) $^ -Wl,-rpath=$(purelib) $(addprefix $(purelib)/, orddict$(DLL) faust$(DLL) midifile$(DLL)) -Wl,-rpath=$(pdextradir)/lib mix$(DLL) -o pdfaust$(DLL)
	test "$(DLL)" = "$(PDDLL)" || mv pdfaust$(DLL) pdfaust$(PDDLL)

pdfaust.o: $(addprefix ../, fdsp~.pure fsynth~.pure) \
$(addprefix ../examples/, midiseq.pure oscseq.pure)
	pure $(PIC) -I.. -L.. -c $^ -o $@ --main __pdfaust_main__

pdloader.o: pdloader.c
	gcc $(ALL_CFLAGS) -DVERSION="\"$(version)\"" -c $< -o $@

# Faust-specific stuff
svg: $(svg)
xml: $(xml)

clean:
	rm -f $(solibs) $(somodules) $(bclibs) $(bcmodules) *.o pdfaust$(PDDLL)

distclean: clean
	rm -Rf $(csource) $(cppsource) $(asmsource) $(svg) $(xml)

realclean: distclean
