dspsrc  := $(wildcard *.dsp)
cppsrc  := $(dspsrc:.dsp=.cpp)
mods	:= $(dspsrc:%.dsp=%~.pd_linux)
svg 	:= $(dspsrc:.dsp=.svg)
xml 	:= $(dspsrc:.dsp=.dsp.xml)
pd 	:= $(dspsrc:.dsp=.pd)
libs    := $(wildcard *.lib)

# Try to guess the host system type and figure out compilation flags.
host = $(shell ../../config.guess)
ifneq "$(findstring x86_64-,$(host))" ""
# 64 bit, needs -fPIC flag
PIC = -fPIC
endif

CFLAGS = $(PIC)

ARCH     = puredata.cpp
#FAUST2PD = faust2pd
FAUST2PD = pure -x ../../faust2pd.pure
# faust2pd options (the -s forces additional number controls for each slider)
# a number of other options are supported, run faust2pd -h for help
F2PDFLAGS = -s

all: $(mods) $(pd)

cpp: $(cppsrc)

svg: $(svg)

xml: $(xml)

%~.pd_linux: %.cpp
	$(CXX) $(CFLAGS) -shared -Dmydsp=$(@:%~.pd_linux=%) $< -o $@

%.cpp: %.dsp
	faust $(VEC) -a $(ARCH) $< -o $@

%.svg: %.dsp
	faust -svg $< -o /dev/null >/dev/null

%.dsp.xml: %.dsp
	faust -xml $< -o /dev/null

# synths

NVOICES = 2

organ.pd: organ.dsp.xml
	$(FAUST2PD) $(F2PDFLAGS) -n $(NVOICES) $<

subtractive.pd: subtractive.dsp.xml
	$(FAUST2PD) $(F2PDFLAGS) -n $(NVOICES) $<

karplusplus.pd: karplusplus.dsp.xml
	$(FAUST2PD) $(F2PDFLAGS) -n $(NVOICES) $<

# other dsps (effect units)

%.pd: %.dsp.xml
	$(FAUST2PD) $(F2PDFLAGS) $<

clean:
	rm -f $(mods) *~ *.a *.o

distclean:
	rm -f $(mods) $(cppsrc) *~ *.a *.o

clean-cpp:
	rm -f $(cppsrc)

clean-xml:
	rm -f $(xml)

clean-pd:
	rm -f $(pd)

clean-svg:
	rm -rf *-svg

realclean:
	rm -f $(mods) $(cppsrc) $(xml) $(pd) *~ *.a *.o
	rm -rf *-svg
