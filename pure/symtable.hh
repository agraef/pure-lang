
#ifndef SYMTABLE_HH
#define SYMTABLE_HH

#include <string>
#include <map>
#include <vector>
#include <stdint.h>
#include "expr.hh"
#include "printer.hh"

using namespace std;

/* Symbol table entries. */

class symbol {
public:
  expr x; // cached expression node
  int32_t f; // symbol tag
  string s; // print name
  prec_t prec; // precedence level
  fix_t fix; // fixity
  int32_t modno; // module key for private symbol, -1 for global symbol
  symbol() : // constructor for dummy entries
    f(0), s(""), prec(10), fix(infix), modno(-1) { }
  symbol(const string& _s, int _f, int32_t _modno = -1) :
    f(_f), s(_s), prec(10), fix(infix), modno(_modno) { x = expr(f); }
  symbol(const string& _s, int _f, prec_t _prec, fix_t _fix,
	 int32_t _modno = -1) :
    f(_f), s(_s), prec(_prec), fix(_fix), modno(_modno) { x = expr(f); }
};

/* Symbol table. */

typedef map<string, symbol> sym_map;
typedef map<int32_t, sym_map> sym_tab;

class symtable {
  int32_t fno;
  sym_tab tab;
  vector<symbol*> rtab;
public:
  symtable();
  // add default declarations for the builtin constants and operators (to be
  // invoked *after* possibly reading the prelude)
  void init_builtins();
  // get current number of symbols in table (symbols are always numbered
  // consecutively from 1 to nsyms())
  int32_t nsyms() { return fno; }
  /* The following routines first search for a symbol in the given module,
     failing that they will also search for a global symbol. (If modno==-1
     then only global symbols will be searched.) */
  // look up an existing symbol in given module (return 0 if not in table)
  symbol* lookup(const string& s, int32_t modno = -1);
  // get a symbol by its name (create if necessary)
  symbol& sym(const string& s, int32_t modno = -1);
  symbol& sym(const string& s, prec_t prec, fix_t fix, int32_t modno = -1);
  /* These work like the above, but will only return exact matches in the
     given module. */
  symbol* xlookup(const string& s, int32_t modno = -1);
  symbol& xsym(const string& s, int32_t modno = -1);
  symbol& xsym(const string& s, prec_t prec, fix_t fix, int32_t modno = -1);
  // get a symbol by its number
  symbol& sym(int32_t f);
  // retrieve various builtin symbols (create when necessary)
  int32_t __show__sym; // This is cached here to improve performance.
  symbol& nil_sym();
  symbol& cons_sym();
  symbol& void_sym();
  symbol& pair_sym();
  symbol& seq_sym();
  symbol& neg_sym() { return sym("neg"); }
  symbol& not_sym();
  symbol& bitnot_sym();
  symbol& or_sym();
  symbol& and_sym();
  symbol& bitor_sym();
  symbol& bitand_sym();
  symbol& shl_sym();
  symbol& shr_sym();
  symbol& less_sym();
  symbol& greater_sym();
  symbol& lesseq_sym();
  symbol& greatereq_sym();
  symbol& equal_sym();
  symbol& notequal_sym();
  symbol& plus_sym();
  symbol& minus_sym();
  symbol& mult_sym();
  symbol& fdiv_sym();
  symbol& div_sym();
  symbol& mod_sym();
  symbol& catch_sym() { return sym("catch"); }
  symbol& catmap_sym() { return sym("catmap"); }
  symbol& rowcatmap_sym() { return sym("rowcatmap"); }
  symbol& colcatmap_sym() { return sym("colcatmap"); }
  symbol& failed_match_sym() { return sym("failed_match"); }
  symbol& failed_cond_sym() { return sym("failed_cond"); }
  symbol& signal_sym() { return sym("signal"); }
  symbol& segfault_sym() { return sym("stack_fault"); }
  symbol& bad_matrix_sym() { return sym("bad_matrix_value"); }
  symbol& amp_sym();
  // These aren't predefined and aren't in the prelude either, so they may be
  // undefined in which case a null pointer is returned. Pass force=true to
  // forcibly create these symbols.
  symbol* complex_rect_sym(bool force = false);
  symbol* complex_polar_sym(bool force = false);
};

#endif // ! SYMTABLE_HH
