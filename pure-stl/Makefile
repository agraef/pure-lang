### Usage:
#
# make dll   - updates production versions of stl dlls and copys them to .
# make debug - does the same for Pure debug support versions
# make tests - compile some Pure unit test files
# make clean - rm all dlls and executables - leave only source code
# make       - same as make dll

### Example dry run:
#
#	touch sv_nonmodifying.cpp
#	make -n dll
#	make -n debug
#	make -n

# Try to guess the host system type.
host = $(shell ./config.guess)

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows (untested)
DLL = .dll
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX (untested)
DLL = .dylib
shared = -dynamiclib
endif
ifneq "$(findstring x86_64-,$(host))" ""
# 64 bit, needs -fPIC flag
PIC = -fPIC
endif

### Macros

DEBUG_DLL_DIR = ./debug_dll
PROD_DLL_DIR = ./dll
# CXX = clang++
DBG_FLAGS = -DSTL_DEBUG
PROD_FLAGS = 

DBG_SUPPORT = $(if $(findstring debug,$(MAKECMDGOALS)),ON)
FLAGS = $(if $(DBG_SUPPORT),$(DBG_FLAGS),$(PROD_FLAGS))
DLL_DIR = $(if $(DBG_SUPPORT),$(DEBUG_DLL_DIR),$(PROD_DLL_DIR))

MODULES = stlcontainer stlalgorithm
CONTAINER_BASENAMES = stlbase stlvec
ALGO_BASENAMES = sv_modifying sv_nonmodifying sv_sort \
	         sv_merge sv_heap sv_minmax sv_numeric

CONTAINER_HPPS = $(addsuffix .hpp, $(CONTAINER_BASENAMES))
CONTAINER_CPPS = $(addsuffix .cpp, $(CONTAINER_BASENAMES))
ALGO_HPPS = stlbase.hpp stlvec.hpp sv_algorithm.hpp
ALGO_CPPS = $(addsuffix .cpp, $(ALGO_BASENAMES))

DLLS = $(addsuffix $(DLL), $(MODULES))
DLL_TRGS = $(addprefix $(DLL_DIR)/, $(DLLS))

### Make dlls

dll debug: $(DLLS)

$(DLLS): $(DLL_TRGS)
	cp $(DLL_DIR)/*$(DLL) .

$(DLL_DIR)/stlcontainer$(DLL): $(CONTAINER_HPPS) $(CONTAINER_CPPS)
	@ test -d $(DLL_DIR) || mkdir $(DLL_DIR)
	$(CXX) $(shared) $(FLAGS) $(PIC) $(CONTAINER_CPPS) -o $@

$(DLL_DIR)/stlalgorithm$(DLL): $(ALGO_HPPS) $(ALGO_CPPS)
	@ test -d $(DLL_DIR) || mkdir $(DLL_DIR)
	$(CXX) $(shared) $(FLAGS) $(PIC) $(ALGO_CPPS) -o $@

### Compile unit tests

test:	x_sv_all

x_sv_%:	ut_x_sv_%.pure stl*.pure sv_*.pure $(DLLS)
	pure -c ut_$@.pure -o $@

### Remove executables and dlls

clean:	
	@ rm -f $(DEBUG_DLL_DIR)/* $(PROD_DLL_DIR)/*
	@ rm -f x_sv_*

.PHONY:	dll debug tests clean

