
# GNU Makefile to compile the C, Fortran and Faust examples to LLVM bitcode.

# Edit these as needed. CC is the C compiler (llvm-gcc by default), FC the
# Fortran compiler (llvm-fortran), FAUST the Faust compiler, including any
# options for bitcode generation that they may need.

CC = llvm-gcc -emit-llvm
#CC = clang -emit-llvm
FC = llvm-gfortran -emit-llvm
FAUST = faust -double -lang llvm

# Prerequisites:

# For the C and Fortran examples, you need to have a version of llvm-gcc with
# both C and Fortran support installed. (Clang can be used instead of llvm-gcc
# if you have it.) Also, the headers and libraries of the GNU Scientific
# Library (GSL) are needed to compile and run the gslsort.c example.

# For the Faust signal processing examples (.dsp programs), you'll need a
# special version of Grame's Faust compiler (http://faust.grame.fr). Until
# Faust's new LLVM backend becomes part of the mainline Faust compiler, you
# can find this on the special 'faust2' branch in Faust's git repository at:
# git://faudiostream.git.sourceforge.net/gitroot/faudiostream/faudiostream

# If your system lacks any of the above then some of the examples will refuse
# to build. Compilation will proceed anyway, so that you should be able to
# build at least the examples for which you have the required utilities.

# Usage:

# 'make' compiles all the examples.

# 'make asm' can be used to generate the LLVM assembler code for the examples.
# (These are for your own perusal, they aren't needed for running the examples
# in Pure.)

# 'make clean' removes all the generated bitcode and assembler files.

# Examples showing how to use the bitcode interface in Pure 0.44 and later can
# be found in the corresponding *.pure scripts.

c_source   := $(wildcard *.c)
f_source   := $(wildcard *.f90)
dsp_source := $(wildcard *.dsp)
bitcode    := $(c_source:.c=.bc) $(f_source:.f90=.bc) $(dsp_source:.dsp=.bc)
assembler  := $(c_source:.c=.ll) $(f_source:.f90=.ll) $(dsp_source:.dsp=.ll)
source     := $(c_source) $(f_source) $(dsp_source)

all: $(bitcode)

asm: $(assembler)

clean:
	-rm -f $(bitcode)
	-rm -f $(assembler)

# compile C source to LLVM bitcode
%.bc: %.c
	-$(CC) -c $< -o $@

# compile C source to LLVM assembler
%.ll: %.c
	-$(CC) -S $< -o $@

# compile Fortran source to LLVM bitcode
%.bc: %.f90
	-$(FC) -c $< -o $@

# compile Fortran source to LLVM assembler
%.ll: %.f90
	-$(FC) -S $< -o $@

# compile Faust source to LLVM bitcode
%.bc: %.dsp
	-$(FAUST) $< -o $@

# compile Faust source to LLVM assembler
%.ll: %.dsp
	-$(FAUST) $< 2> $@
