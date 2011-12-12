
override srcdir = ..
include $(srcdir)/Makefile.common

solibs     := mix.so
somodules  := $(dsp_source:.dsp=.so)
bcmodules  := $(dsp_source:.dsp=.bc)

# The xfade dsp is needed by all effect plugins, preallocate some units.
$(somodules): CXXFLAGS += -DNVOICES=16

all: $(solibs) $(somodules)
bitcode: $(bclibs) $(bcmodules)

# Faust-specific stuff
svg: $(svg)
xml: $(xml)

clean:
	-rm -f $(solibs) $(somodules) $(bclibs) $(bcmodules)

distclean: clean
	rm -Rf $(csource) $(cppsource) $(asmsource) $(svg) $(xml)

realclean: distclean
