
# This Makefile illustrates how to compile a collection of pd-pure objects to
# a shared library which can be loaded with Pd's -lib option. To give this a
# try, run 'make' and then 'pd -lib test test.pd'. (Make sure that you've
# properly installed the pd-pure loader first.)

# The filename extension for Pd object libraries depends on your operating
# system. Edit this as needed.
PDEXT       = .pd_linux

# Other platform-specific information can be obtained from Pure's pkg-config.
DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

all: test$(PDEXT)

# This links the compiled Pure code and loader to a shared library object with
# the proper extension required by Pd.
test$(PDEXT): test.o loader.o
	gcc $(PIC) $(shared) $^ -o test$(DLL)
	test "$(DLL)" = "$(PDEXT)" || mv test$(DLL) test$(PDEXT)

# This uses the Pure interpreter to compile our pd-pure objects to native code.
# Note that the --main option is necessary to prevent name clashes and allow
# the module to coexist with other modules of its kind.
test.o: add.pure counter.pure
	pure $(PIC) -c $^ -o $@ --main=__test_main__

# Compile a minimal loader module which is needed to interface to Pd and
# register the object classes with pd-pure.
loader.o: loader.c
	gcc $(PIC) -c $< -o $@

clean:
	rm -f *.o *$(DLL) *$(PDEXT)
