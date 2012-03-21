# Makefile for the pure-stlmap/dev directory

# Compiles pure scripts that load up stlmaps, dicts, ordicts, etc with random
# words then do repeated lookups with random keys lookup.
#
# A "lookup" is "if member cont key then cont!key else ();
#

# all	    compile the scripts

# run	    do the lookups using normal calls (member and !)

# run_dcc   do the lookups using direct C calls where available (e.g., 
#           hashdict_get, stl::sm_get)

# words     make a new random word file from WORDSOURCE based on 
#           NUMWORDS and WORDSEED

# keys      build a new random key file from WORDFILE based on 
#           NUMKEYS and KEYSEED 

# clean     remove the executables

# For run and run_dcc you can specify NUMWORDS the number of words to be in
# the containers and WORDFILE, a text file that contains a random list of
# words, one per line. The entries in the containers are (word=>int). A
# different key is used for each lookup. 

ifndef NUMWORDS
  NUMWORDS = 10000
endif

ifndef NUMKEYS
  NUMKEYS = 100
endif

ifndef WORDSOURCE
  WORDSOURCE = random_words.txt
endif

ifndef WORDSEED
  WORDSEED = 1
endif

ifndef KEYSEED
  KEYSEED = 1
endif


WORDFILE = x_words.txt

KEYFILE = x_keys.txt

# These parameters are fixed in timer_template.pure - do not change

containers  = dict orddict hashdict stlmap stlhmap 
containers_dcc = dict_dcc orddict_dcc hashdict_dcc stlmap_dcc stlhmap_dcc 
plain_tests = $(addprefix time_, $(containers))
dcc_tests = $(addprefix time_, $(containers_dcc))
all_tests = $(plain_tests) $(dcc_tests)
num_repeats = 1000

all	: $(plain_tests) $(dcc_tests) shuffle

shuffle : shuffle.pure
	pure -c shuffle.pure -o shuffle

words   : $(WORDSOURCE) shuffle
	./shuffle $(WORDSEED) $(NUMWORDS) $(WORDSOURCE) > $(WORDFILE)

$(WORDFILE) : $(WORDSOURCE)
	./shuffle $(WORDSEED) $(NUMWORDS) $(WORDSOURCE) > $(WORDFILE)

keys    : $(WORDFILE) shuffle
	./shuffle $(KEYSEED) $(NUMKEYS) $(WORDFILE) > $(KEYFILE)	

$(KEYFILE) : $(WORDFILE) shuffle
	./shuffle $(KEYSEED) $(NUMKEYS) $(WORDFILE) > $(KEYFILE)

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
	rm -f $(all_tests) $(WORDFILE) $(KEYFILE)

run     : $(plain_tests) $(KEYFILE) 
	@ ./print_test_title $(num_repeats) $(KEYFILE) $(WORDFILE)
	@ for x in $(plain_tests) ; do ./$$x 0 $(KEYFILE) $(WORDFILE) ; done

run_dcc : $(dcc_tests) $(KEYFILE)
	@ ./print_test_title $(num_repeats) $(KEYFILE) $(WORDFILE)
	@ for x in $(dcc_tests) ; do ./$$x 0 $(KEYFILE) $(WORDFILE) ; done

.PHONY	: all clean run run_plain run_dcc ensure_shuffle

