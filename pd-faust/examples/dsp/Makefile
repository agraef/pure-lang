
override srcdir = ../..
include $(srcdir)/Makefile.common

somodules  := $(dsp_source:.dsp=$(DLL))
bcmodules  := $(dsp_source:.dsp=.bc)
modules    := $(somodules) $(bcmodules)

fx = amp.dsp chorus.dsp compressor.dsp echo.dsp expander.dsp flanger.dsp \
freeverb.dsp fuzz.dsp

fxmodules = $(fx:.dsp=.so)
synthmodules = $(filter-out $(fxmodules), $(somodules))

# uncomment this to debug voice allocation
#CXXFLAGS += -DDEBUG
# preallocate a suitable number of voices for the synths
$(synthmodules): CXXFLAGS += -DNVOICES=16

all: $(somodules)
bitcode: $(bcmodules)

$(synthmodules): instrument.lib

# Faust-specific stuff
svg: $(svg)
xml: $(xml)

clean:
	-rm -f $(modules)

distclean: clean
	rm -Rf $(csource) $(cppsource) $(asmsource) $(svg) $(xml)

realclean: distclean
