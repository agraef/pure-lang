
/* Copyright (c) 2008-2012 by Albert Graef <Dr.Graef@t-online.de>.

   This file is part of the Pure runtime.

   The Pure runtime is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at your
   option) any later version.

   Pure is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
   more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef SYMTABLE_HH
#define SYMTABLE_HH

#include <string>
#include <set>
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
  int32_t g; // right paren for outfix symbol
  string s; // print name
  string *ns; // non-null indicates namespace bracket
  prec_t prec; // precedence level
  fix_t fix; // fixity
  bool priv; // private attribute
  bool unresolved; // unresolved symbol
  symbol *bminus; // pointer to corresponding binary symbol (unary minus)
  symbol() // constructor for dummy entries
    : f(0), g(0), s(""), ns(0), prec(PREC_MAX), fix(infix), priv(false),
      unresolved(false), bminus(0)
  {}
  symbol(const string& _s, int _f, bool _priv = false)
    : f(_f), g(0), s(_s), ns(0), prec(PREC_MAX), fix(infix), priv(_priv),
      unresolved(false), bminus(0)
  { x = expr(f); }
  symbol(const string& _s, int _f, prec_t _prec, fix_t _fix,
	 bool _priv = false)
    : f(_f), g(0), s(_s), ns(0), prec(_prec), fix(_fix), priv(_priv),
      unresolved(false), bminus(0)
  { x = expr(f); }
};

/* Symbol table. */

class symtable {
  int32_t fno;
  map<string, symbol> tab;
  vector<symbol*> rtab;
  symbol* lookup_p(const char *s)
  {
    int count;
    return lookup_p(s, count);
  }
  symbol* lookup_p(const char *s, symbol*& cache)
  {
    if (!cache) cache = lookup_p(s);
    return cache;
  }
  symbol* lookup_p(const char *s, int& count)
  {
    map<string, symbol>::iterator it = tab.find(s);
    count = it != tab.end();
    if (!count || !visible(it->second))
      return 0;
    else
      return &it->second;
  }
  symbol* sym_p(const char *s, symbol*& cache, bool priv = false);
  symbol* sym_p(const char *s, symbol*& cache,
		prec_t prec, fix_t fix, bool priv = false);
  // these are cached here to speed up predefined symbol lookups
  symbol* __gensym_sym;
  symbol* __namespace_sym;
  symbol* __dir_sym;
  symbol* __file_sym;
  symbol* __locals_sym;
  symbol* __func_sym;
  symbol* __list_sym;
  symbol* __nil_sym;
  symbol* __cons_sym;
  symbol* __void_sym;
  symbol* __pair_sym;
  symbol* __mapsto_sym;
  symbol* __seq_sym;
  symbol* __flip_sym;
  symbol* __neg_sym;
  symbol* __not_sym;
  symbol* __bitnot_sym;
  symbol* __or_sym;
  symbol* __and_sym;
  symbol* __bitor_sym;
  symbol* __bitand_sym;
  symbol* __shl_sym;
  symbol* __shr_sym;
  symbol* __less_sym;
  symbol* __greater_sym;
  symbol* __lesseq_sym;
  symbol* __greatereq_sym;
  symbol* __equal_sym;
  symbol* __notequal_sym;
  symbol* __plus_sym;
  symbol* __minus_sym;
  symbol* __mult_sym;
  symbol* __fdiv_sym;
  symbol* __div_sym;
  symbol* __mod_sym;
  symbol* __quote_sym;
  symbol* __catch_sym;
  symbol* __catmap_sym;
  symbol* __rowcatmap_sym;
  symbol* __colcatmap_sym;
  symbol* __listmap_sym;
  symbol* __rowmap_sym;
  symbol* __colmap_sym;
  symbol* __failed_match_sym;
  symbol* __failed_cond_sym;
  symbol* __signal_sym;
  symbol* __segfault_sym;
  symbol* __bad_matrix_sym;
  symbol* __amp_sym;
  symbol* __quoteop_sym;
  symbol* __complex_rect_sym;
  symbol* __complex_polar_sym;
  symbol* __rational_xdiv_sym;
  symbol* __int_sym;
  symbol* __bigint_sym;
  symbol* __double_sym;
  symbol* __string_sym;
  symbol* __pointer_sym;
  symbol* __matrix_sym;
  symbol* __if_sym;
  symbol* __ifelse_sym;
  symbol* __lambda_sym;
  symbol* __case_sym;
  symbol* __when_sym;
  symbol* __with_sym;
  symbol* __eqn_sym;
  symbol* __ttag_sym;
  symbol* __astag_sym;
  symbol* __eval_sym;
public:
  symtable();
  ~symtable();
  // the current namespace
  string *current_namespace;
  // additional namespaces to be searched for unqualified symbols
  map< string, set<int32_t> > *search_namespaces;
  // stack of temporary namespaces used to handle namespace brackets
  list<string*> temp_namespaces;
  void push_namespace(const string& ns)
  {
    temp_namespaces.push_front(current_namespace);
    current_namespace = new string(ns);
  }
  void pop_namespace()
  {
    assert(!temp_namespaces.empty());
    delete current_namespace;
    current_namespace = temp_namespaces.front();
    temp_namespaces.pop_front();
  }
  void clean_namespaces()
  {
    while (!temp_namespaces.empty())
      pop_namespace();
  }
  // these are for internal use only
  void dump(string& s);
  void restore(const string& s);
  // add default declarations for the builtin constants and operators (to be
  // invoked *after* possibly reading the prelude)
  void init_builtins();
  // get current number of symbols in table (symbols are always numbered
  // consecutively from 1 to nsyms())
  int32_t nsyms() { return fno; }
  /* The following operations look up a symbol in the table and possibly
     create it if necessary. Unqualified symbols are first searched for in the
     current namespace and all search namespaces (if any), and finally also in
     the default namespace. If the symbol is found (or could be created), a
     pointer to the symbol table entry is returned. Otherwise a null pointer
     is returned, in which case you can inspect the 'count' variable to
     determine the cause of the error. If 'count' is 0, then the symbol wasn't
     found; this can only happen with lookup(), as the sym() methods always
     try to create the symbol if it doesn't exist already. If 'count' is 1,
     then the symbol was found but is not visible in the current namespace
     because it is a 'private' symbol in another namespace. If 'count' is >1,
     then an unqualified symbol couldn't be resolved because it exists in
     multiple (current or search) namespaces. */
  int count;
  // look up an existing symbol
  symbol* lookup(const char *s);
  symbol* lookup(const string& s) { return lookup(s.c_str()); }
  template <class T>
  /* This is basically the same as above, but looks for a symbol in a specific
     class. Note that stupid C++ forces us to include this right here in the
     header file, instead of symtable.cc where it belongs, so that the
     template method is properly instantiated. */
  symbol* lookup_restricted(const char *s, const map<int32_t,T>& syms)
  {
    if (strncmp(s, "::", 2) == 0) {
      // absolute qualifier
      symbol *sym = lookup_p(s+2, count);
      if (sym && syms.find(sym->f) == syms.end())
	sym = 0;
      return sym;
    }
    int priv;
    symbol *default_sym = lookup_p(s, priv), *search_sym = 0;
    if (default_sym && syms.find(default_sym->f) == syms.end())
      default_sym = 0;
    count = 0;
    if (strcmp(s, "_") == 0) {
      // anonymous variable is always taken as is
      count = default_sym!=0;
      return default_sym;
    }
    // first look for a symbol in the current namespace
    if (!current_namespace->empty() || !temp_namespaces.empty()) {
      string id = current_namespace->empty()?s:(*current_namespace)+"::"+s;
      symbol *sym = lookup_p(id.c_str());
      if (sym && syms.find(sym->f) != syms.end()) {
	count = 1;
	return sym;
      }
    }
    // next scan the search namespaces; if the symbol is ambiguous, bail out
    // with an error here
    for (map< string, set<int32_t> >::iterator it = search_namespaces->begin(),
	   end = search_namespaces->end(); it != end; it++) {
      string id = it->first+"::"+s;
      int priv2;
      symbol *sym = lookup_p(id.c_str(), priv2);
      if (sym && syms.find(sym->f) == syms.end())
	sym = 0;
      if (sym && !it->second.empty() &&
	  it->second.find(sym->f) == it->second.end())
	continue;
      if (!sym) priv |= priv2;
      if (sym && ++count > 1) return 0;
      if (!search_sym) search_sym = sym;
    }
    if (search_sym) return search_sym;
    // fall back to a symbol in the default namespace, if any
    count = (default_sym!=0) | priv;
    return default_sym;
  }
  template <class T>
  symbol* lookup_restricted(const string& s, const map<int32_t,T>& syms)
  { return lookup_restricted<T>(s.c_str(), syms); }
  // get a symbol by its name, create if necessary
  symbol* sym(const char *s, bool priv = false);
  symbol* sym(const char *s, prec_t prec, fix_t fix, bool priv = false);
  symbol* sym(const string& s, bool priv = false)
  { return sym(s.c_str(), priv); }
  symbol* sym(const string& s, prec_t prec, fix_t fix, bool priv = false)
  { return sym(s.c_str(), prec, fix, priv); }
  // get/create a symbol, throw an appropriate error if this fails
  symbol& checksym(const char *s, bool priv = false);
  symbol& checksym(const string& s, bool priv = false)
  { return checksym(s.c_str(), priv); }
  // get a symbol by its number
  symbol& sym(int32_t f);
  // check whether a symbol is currently visible
  bool visible(const symbol& sym);
  bool visible(int32_t f)
  { assert(f > 0 && (uint32_t)f < rtab.size());
    return visible(*rtab[f]); }
  // retrieve various builtin symbols (create when necessary)
  int32_t anon_sym, __show__sym; // Cached here to improve performance.
  symbol& gensym_sym()
  { return *sym_p("__gensym__", __gensym_sym); }
  symbol& namespace_sym()
  { return *sym_p("__namespace__", __namespace_sym); }
  symbol& dir_sym()
  { return *sym_p("__dir__", __dir_sym); }
  symbol& file_sym()
  { return *sym_p("__file__", __file_sym); }
  symbol& locals_sym()
  { return *sym_p("__locals__", __locals_sym); }
  symbol& func_sym()
  { return *sym_p("__func__", __func_sym); }
  symbol& list_sym()
  { return *sym_p("__list__", __list_sym); }
  symbol& nil_sym();
  symbol& cons_sym();
  symbol& void_sym();
  symbol& pair_sym();
  symbol& mapsto_sym();
  symbol& seq_sym();
  symbol& flip_sym()
  { return *sym_p("flip", __flip_sym); }
  symbol& neg_sym()
  { return *sym_p("neg", __neg_sym); }
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
  symbol& quote_sym()
  { return *sym_p("quote", __quote_sym); }
  symbol& catch_sym()
  { return *sym_p("catch", __catch_sym); }
  symbol& catmap_sym()
  { return *sym_p("catmap", __catmap_sym); }
  symbol& rowcatmap_sym()
  { return *sym_p("rowcatmap", __rowcatmap_sym); }
  symbol& colcatmap_sym()
  { return *sym_p("colcatmap", __colcatmap_sym); }
  symbol& listmap_sym()
  { return *sym_p("listmap", __listmap_sym); }
  symbol& rowmap_sym()
  { return *sym_p("rowmap", __rowmap_sym); }
  symbol& colmap_sym()
  { return *sym_p("colmap", __colmap_sym); }
  symbol& failed_match_sym()
  { return *sym_p("failed_match", __failed_match_sym); }
  symbol& failed_cond_sym()
  { return *sym_p("failed_cond", __failed_cond_sym); }
  symbol& signal_sym()
  { return *sym_p("signal", __signal_sym); }
  symbol& segfault_sym()
  { return *sym_p("stack_fault", __segfault_sym); }
  symbol& bad_matrix_sym()
  { return *sym_p("bad_matrix_value", __bad_matrix_sym); }
  symbol& amp_sym();
  symbol& quoteop_sym();
  symbol& complex_rect_sym();
  symbol& complex_polar_sym();
  symbol& rational_xdiv_sym();
  symbol& int_sym()
  { return *sym_p("int", __int_sym); }
  symbol& bigint_sym()
  { return *sym_p("bigint", __bigint_sym); }
  symbol& double_sym()
  { return *sym_p("double", __double_sym); }
  symbol& string_sym()
  { return *sym_p("string", __string_sym); }
  symbol& pointer_sym()
  { return *sym_p("pointer", __pointer_sym); }
  symbol& matrix_sym()
  { return *sym_p("matrix", __matrix_sym); }
  symbol& if_sym();
  symbol& ifelse_sym()
  { return *sym_p("__ifelse__", __ifelse_sym); }
  symbol& lambda_sym()
  { return *sym_p("__lambda__", __lambda_sym); }
  symbol& case_sym()
  { return *sym_p("__case__", __case_sym); }
  symbol& when_sym();
  symbol& with_sym();
  symbol& eqn_sym();
  symbol& ttag_sym();
  symbol& astag_sym();
  symbol& eval_sym()
  { return *sym_p("__eval__", __eval_sym); }
  // the minus symbol; this plays an awkward role in the syntax because it can
  // be *both* a unary and a binary symbol
  bool check_minus_sym(int32_t f);
  symbol& neg_sym_of(int32_t f);
  bool is_neg_sym(int32_t f);
  string neg_sym_pname(int32_t f);
  prec_t neg_sym_nprec(int32_t f);
};

#endif // ! SYMTABLE_HH
