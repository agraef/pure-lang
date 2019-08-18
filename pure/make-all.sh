#! /bin/bash

# This is supposed to be run from the toplevel directory in a clone of the
# pure-lang git repository. It will iterate over all addon modules and run
# 'make' with the arguments supplied on the command line there. So you can
# use, e.g., just 'make-all.sh' to build all the modules, 'make-all.sh clean'
# to clean them, etc. All 3rd party dependencies required by the addon modules
# must be installed first to make this work.

# CAVEAT: If you're running this on a fresh checkout, you'll have to do things
# in the right order, since some of the add-on modules depend on others, and
# will fail to compile if those dependencies are not built and installed
# first. Thus you will want to proceed in these stages:

# - First install LLVM. Then build and install pure and pure-gen, in that
#   order.

# - Build and install the following modules in any order: pure-ffi,
#   pure-stldict, pure-audio, pure-faust, pd-pure, pure-midi, pure-xml.

# - Now you can build and install the remaining add-ons by running
#   make-all.sh.

# Once you've done the first and second stage, stage 3 can just be repeated
# after changes without doing the first two stages again.

for x in pure-* *-pure; do
    test -f $x/Makefile && make -C $x "$@"
done
