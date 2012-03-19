# Makefile for the pure-stlmap/dev directory

# It compiles pure scripts that load up stlmaps, dicts, ordicts, etc with
# random words and then do $(num_lookups) lookups $(num_repeats) time and print out
# how long it took.
#
# A "lookup" is "if member cont key then cont!key else ();
#

# all	    compile the scripts
# run	    do the lookups using normal calls (member and !)
# run_dcc   do the lookups using direct C calls where available (e.g., hashdict_get, stl::sm_get)
# clean     remove the executables

# For run and run_dcc you can specify NUMWORDS the number of words to be in
# the containers and WORDFILE, a text file that contains a random list of
# words, one per line. The entries in the containers are (word=>int). A
# different key is used for each lookup. The keys are the first $(num_lookups)
# words in WORDFILE

ifndef NUMWORDS
  NUMWORDS = 10000
endif

ifndef WORDFILE
  WORDFILE = random_words.txt
endif

# These parameters are fixed in timer_template.pure - do not change
containers  = dict orddict hashdict stlmap stlhmap 

containers_dcc = dict_dcc orddict_dcc hashdict_dcc stlmap_dcc stlhmap_dcc 
plain_tests = $(addprefix time_, $(containers))
dcc_tests = $(addprefix time_, $(containers_dcc))
all_tests = $(plain_tests) $(dcc_tests)
num_repeats = 1000
num_lookups = 100

all	: $(plain_tests) $(dcc_tests)

time_dict : 
	pure -c timer_template.pure -o time_dict --enable DICT

time_orddict : 
	pure -c timer_template.pure -o time_orddict --enable ORDDICT

time_hashdict : 
	pure -c timer_template.pure -o time_hashdict --enable HASHDICT

time_stlmap : 
	pure -c timer_template.pure -o time_stlmap --enable STLMAP

time_stlhmap : 
	pure -c timer_template.pure -o time_stlhmap --enable STLHMAP

time_orddict_dcc : 
	pure -c timer_template.pure -o time_orddict_dcc --enable ORDDICT --enable DCC

time_dict_dcc : 
	pure -c timer_template.pure -o time_dict_dcc --enable DICT --enable DCC

time_hashdict_dcc : 
	pure -c timer_template.pure -o time_hashdict_dcc --enable HASHDICT --enable DCC

time_stlmap_dcc : 
	pure -c timer_template.pure -o time_stlmap_dcc --enable STLMAP --enable DCC

time_stlhmap_dcc : 
	pure -c timer_template.pure -o time_stlhmap_dcc --enable STLHMAP --enable DCC

clean   :
	rm -f $(all_tests)

run     : $(plain_tests)
	@ echo ----------------------------------------------------------------
	@ echo test - $(NUMWORDS) words in container, $(num_lookups) lookups repeated $(num_repeats) times
	@ echo ----------------------------------------------------------------
	@ for x in $(plain_tests) ; do ./$$x 0 $(NUMWORDS) $(WORDFILE) ; done

run_dcc : $(dcc_tests)
	@ echo ----------------------------------------------------------------
	@ echo test - $(NUMWORDS) words in container, $(num_lookups) lookups repeated $(num_repeats) times
	@ echo ----------------------------------------------------------------
	@ for x in $(dcc_tests) ; do ./$$x 0 $(NUMWORDS) $(WORDFILE) ; done

.PHONY	: all clean run run_plain run_dcc

