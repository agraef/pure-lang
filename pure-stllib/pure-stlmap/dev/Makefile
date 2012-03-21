# Makefile for the pure-stlmap/dev directory

# Compiles pure scripts that load up stlmaps, dicts, ordicts, etc with random
# words then do repeated lookups with random keys lookup.
#
# A "lookup" is "if member cont key then cont!key else ();"
#

# all	    compile the test scripts and setup_wk

# run	    do the lookups using normal calls (member and !)

# run_dcc   do the lookups using direct C calls where available (e.g., 
#           hashdict_get, stl::sm_get)

# clean     remove the compiled test scripts

# For run and run_dcc work with  two files: $(WORDS) and $(KEYS). The first is
# a list  of words that are  inserted int the  containers and the second  is a
# list of keys that are used to look up entries.

# setup_wk can be used to make a shuffled word_file and a shuffled
# key_file that contains keys selected from the word_file. Usage:
#
#   setup_wk seed num_words num_keys [source_file [word_file key_file]]
#
# Defaults are zz_wordlist.txt zx_words.txt zx_keys.txt

WORDS = zx_words.txt
KEYS = zx_keys.txt

# These parameters are fixed in timer_template.pure - do not change

containers  = dict orddict hashdict stlmap stlhmap 
containers_dcc = dict_dcc orddict_dcc hashdict_dcc stlmap_dcc stlhmap_dcc 
plain_tests = $(addprefix time_, $(containers))
dcc_tests = $(addprefix time_, $(containers_dcc))
all_tests = $(plain_tests) $(dcc_tests)
num_repeats = 1000

all	: $(plain_tests) $(dcc_tests) setup_wk

setup_wk : setup_wk_source.pure
	pure -c setup_wk_source.pure -o setup_wk

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
	rm -f $(all_tests) setup_wk $(WORDS) $(KEYS)

run     : $(plain_tests) $(KEYS) 
	@ ./print_test_title $(num_repeats) $(KEYS) $(WORDS)
	@ for x in $(plain_tests) ; do ./$$x 0 $(KEYS) $(WORDS) ; done

run_dcc : $(dcc_tests) $(KEYS)
	@ ./print_test_title $(num_repeats) $(KEYS) $(WORDS)
	@ for x in $(dcc_tests) ; do ./$$x 0 $(KEYS) $(WORDS) ; done

# Generate a default sample if setup_wk wasn't run manually.

$(WORDS) $(KEYS): zz_wordlist.txt setup_wk
	./setup_wk 7 10000 100 $< $(WORDS) $(KEYS)

.PHONY	: all clean run run_dcc

