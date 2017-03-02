
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

#include "symtable.hh"
#include "util.hh"
#include <assert.h>

symtable::symtable()
  : fno(0), rtab(1024),
    __gensym_sym(0),
    __namespace_sym(0),
    __dir_sym(0),
    __file_sym(0),
    __locals_sym(0),
    __func_sym(0),
    __list_sym(0),
    __nil_sym(0),
    __cons_sym(0),
    __void_sym(0),
    __pair_sym(0),
    __mapsto_sym(0),
    __seq_sym(0),
    __flip_sym(0),
    __neg_sym(0),
    __not_sym(0),
    __bitnot_sym(0),
    __or_sym(0),
    __and_sym(0),
    __bitor_sym(0),
    __bitand_sym(0),
    __shl_sym(0),
    __shr_sym(0),
    __less_sym(0),
    __greater_sym(0),
    __lesseq_sym(0),
    __greatereq_sym(0),
    __equal_sym(0),
    __notequal_sym(0),
    __plus_sym(0),
    __minus_sym(0),
    __mult_sym(0),
    __fdiv_sym(0),
    __div_sym(0),
    __mod_sym(0),
    __quote_sym(0),
    __catch_sym(0),
    __catmap_sym(0),
    __rowcatmap_sym(0),
    __colcatmap_sym(0),
    __listmap_sym(0),
    __rowmap_sym(0),
    __colmap_sym(0),
    __failed_match_sym(0),
    __failed_cond_sym(0),
    __signal_sym(0),
    __segfault_sym(0),
    __bad_matrix_sym(0),
    __amp_sym(0),
    __quoteop_sym(0),
    __complex_rect_sym(0),
    __complex_polar_sym(0),
    __rational_xdiv_sym(0),
    __int_sym(0),
    __bigint_sym(0),
    __double_sym(0),
    __string_sym(0),
    __pointer_sym(0),
    __matrix_sym(0),
    __if_sym(0),
    __ifelse_sym(0),
    __lambda_sym(0),
    __case_sym(0),
    __when_sym(0),
    __with_sym(0),
    __eqn_sym(0),
    __ttag_sym(0),
    __astag_sym(0),
    __eval_sym(0),
    current_namespace(new string),
    search_namespaces(new map< string, set<int32_t> >),
    __show__sym(0)
{
  // enter any additional predefined symbols here, e.g.:
  //sym("-", 2100, infixl);
  anon_sym = sym("_")->f; // anonymous variable
}

symtable::~symtable()
{
  delete current_namespace;
  delete search_namespaces;
}

void symtable::init_builtins()
{
  gensym_sym();
  namespace_sym();
  dir_sym();
  file_sym();
  locals_sym();
  func_sym();
  list_sym();
  nil_sym();
  cons_sym();
  void_sym();
  pair_sym();
  mapsto_sym();
  flip_sym();
  neg_sym();
  not_sym();
  bitnot_sym();
  or_sym();
  and_sym();
  bitor_sym();
  bitand_sym();
  shl_sym();
  shr_sym();
  less_sym();
  greater_sym();
  lesseq_sym();
  greatereq_sym();
  equal_sym();
  notequal_sym();
  plus_sym();
  minus_sym();
  mult_sym();
  fdiv_sym();
  div_sym();
  mod_sym();
  quote_sym();
  catch_sym();
  catmap_sym();
  rowcatmap_sym();
  colcatmap_sym();
  listmap_sym();
  rowmap_sym();
  colmap_sym();
  failed_match_sym();
  failed_cond_sym();
  signal_sym();
  segfault_sym();
  bad_matrix_sym();
  amp_sym();
  quoteop_sym();
  complex_rect_sym();
  complex_polar_sym();
  rational_xdiv_sym();
  int_sym();
  bigint_sym();
  double_sym();
  string_sym();
  pointer_sym();
  matrix_sym();
  if_sym();
  ifelse_sym();
  lambda_sym();
  case_sym();
  when_sym();
  with_sym();
  eqn_sym();
  ttag_sym();
  astag_sym();
  eval_sym();
}

/* These are used internally to dump the entire symbol table from a string,
   and later restore it when running on the bare metal. */

#include <sstream>

void symtable::dump(string& s)
{
  ostringstream sout(s);
  for (int32_t i = 2; i <= fno; i++) {
    symbol *sym = rtab[i];
    if (sym)
      sout << sym->s << " " << sym->f << " " << (int)sym->prec << " "
	   << sym->fix << " " << sym->priv << '\n';
  }
  s = sout.str();
}

void symtable::restore(const string& s)
{
  istringstream sin(s);
  char buf[1024];
  int f, prec, fix;
  bool priv;
  sin.width(1024);
  while (1) {
    sin >> buf >> f >> prec >> fix >> priv;
    if (sin.fail() || sin.eof()) break;
    string id = buf;
    symbol &sym = tab[id];
    if (f > fno) fno = f;
    if ((uint32_t)fno >= rtab.size())
      rtab.resize(rtab.size()+1024);
    sym = symbol(id, fno, prec, (fix_t)fix, priv);
    //cout << "new symbol " << sym.f << ": " << sym.s << endl;
    rtab[fno] = &sym;
    if (__show__sym == 0 && strcmp(buf, "__show__") == 0) __show__sym = fno;
  }
}

/* These operations are used internally to look up and create symbols exactly
   as specified (no namespace search). */

symbol* symtable::sym_p(const char *s, symbol*& cache, bool priv)
{
  if (cache) return cache;
  symbol *_sym = lookup_p(s, cache);
  if (_sym)
    return _sym;
  string id = s;
  _sym = &tab[id];
  if (_sym->f == 0) {
    if ((uint32_t)++fno >= rtab.size())
      rtab.resize(rtab.size()+1024);
    *_sym = symbol(id, fno, priv);
    //cout << "new symbol " << _sym->f << ": " << _sym->s << endl;
    rtab[fno] = _sym;
    if (__show__sym == 0 && strcmp(s, "__show__") == 0) __show__sym = fno;
    cache = _sym;
    return _sym;
  } else
    return 0;
}

symbol* symtable::sym_p(const char *s, symbol*& cache,
			prec_t prec, fix_t fix, bool priv)
{
  if (cache) return cache;
  assert(prec <= PREC_MAX);
  symbol *_sym = lookup_p(s, cache);
  if (_sym)
    return _sym;
  string id = s;
  _sym = &tab[id];
  if (_sym->f == 0) {
    if ((uint32_t)++fno >= rtab.size())
      rtab.resize(rtab.size()+1024);
    *_sym = symbol(id, fno, prec, fix, priv);
    //cout << "new symbol " << _sym->f << ": " << _sym->s << endl;
    rtab[fno] = _sym;
    if (__show__sym == 0 && strcmp(s, "__show__") == 0) __show__sym = fno;
    cache = _sym;
    return _sym;
  } else
    return 0;
}

symbol* symtable::lookup(const char *s)
{
  if (strncmp(s, "::", 2) == 0) {
    // absolute qualifier
    symbol *sym = lookup_p(s+2, count);
    return sym;
  }
  int priv;
  symbol *default_sym = lookup_p(s, priv), *search_sym = 0;
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
    if (sym) {
      count = 1;
      return sym;
    }
  }
  // next scan the search namespaces; if the symbol is ambiguous, bail out
  // with an error here
  for (map< string, set<int32_t> >::iterator it = search_namespaces->begin(),
	 end = search_namespaces->end(); it != end; it++) {
    string id = it->first.empty()?s:it->first+"::"+s;
    int priv2;
    symbol *sym = lookup_p(id.c_str(), priv2);
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

symbol* symtable::sym(const char *s, bool priv)
{
  symbol *_sym = lookup(s);
  if (_sym)
    return _sym;
  else if (count > 1)
    return 0;
  if (s[0] == ':' && s[1] == ':') s+=2;
  string id = s;
  _sym = &tab[id];
  if (_sym->f == 0) {
    if ((uint32_t)++fno >= rtab.size())
      rtab.resize(rtab.size()+1024);
    *_sym = symbol(id, fno, priv);
    //cout << "new symbol " << _sym->f << ": " << _sym->s << endl;
    rtab[fno] = _sym;
    if (__show__sym == 0 && strcmp(s, "__show__") == 0) __show__sym = fno;
    count = 1;
    return _sym;
  } else
    // the symbol already exists, but isn't visible in the current namespace
    return 0;
}

symbol* symtable::sym(const char *s, prec_t prec, fix_t fix, bool priv)
{
  assert(prec <= PREC_MAX);
  symbol *_sym = lookup(s);
  if (_sym)
    return _sym;
  else if (count > 1)
    return 0;
  if (s[0] == ':' && s[1] == ':') s+=2;
  string id = s;
  _sym = &tab[id];
  if (_sym->f == 0) {
    if ((uint32_t)++fno >= rtab.size())
      rtab.resize(rtab.size()+1024);
    *_sym = symbol(id, fno, prec, fix, priv);
    //cout << "new symbol " << _sym->f << ": " << _sym->s << endl;
    rtab[fno] = _sym;
    if (__show__sym == 0 && strcmp(s, "__show__") == 0) __show__sym = fno;
    count = 1;
    return _sym;
  } else
    // the symbol already exists, but isn't visible in the current namespace
    return 0;
}

symbol& symtable::checksym(const char *s, bool priv)
{
  symbol *_sym;
  bool exists = strstr(s, "::") || ((_sym = lookup(s)) && !_sym->unresolved);
  _sym = sym(s, priv);
  if (_sym) {
    if (!exists)
      // symbol was generated on the fly, we need to keep track of these
      _sym->unresolved = true;
    else if (_sym->unresolved)
      // symbol was previously marked as unresolved, a qualified instance
      // overrides this
      _sym->unresolved = false;
    return *_sym;
  } else if (count > 1)
    throw err("symbol '"+string(s)+"' is ambiguous here");
  else
    throw err("symbol '"+string(s)+"' is private here");
}

symbol& symtable::sym(int32_t f)
{
  assert(f > 0 && f <= fno);
  return *rtab[f];
}

bool symtable::visible(const symbol& sym)
{
  if (sym.priv) {
    size_t k = symsplit(sym.s);
    if (k == string::npos) k = 0;
    string qual = sym.s.substr(0, k);
    // A private symbol is always visible if it is in the default namespace.
    // Other private symbols are only visible in their home namespace.
    // Temporary namespaces (inside a namespace bracket) don't count here.
    return qual.empty() ||
      (qual == *current_namespace && temp_namespaces.empty());
  } else
    return true;
}

symbol& symtable::nil_sym()
{
  lookup_p("[]", __nil_sym);
  if (__nil_sym)
    return *__nil_sym;
  else
    return *sym_p("[]", __nil_sym, PREC_MAX, nonfix);
}

symbol& symtable::cons_sym()
{
  lookup_p(":", __cons_sym);
  if (__cons_sym)
    return *__cons_sym;
  else
    return *sym_p(":", __cons_sym, 1900, infixr);
}

symbol& symtable::void_sym()
{
  lookup_p("()", __void_sym);
  if (__void_sym)
    return *__void_sym;
  else
    return *sym_p("()", __void_sym, PREC_MAX, nonfix);
}

symbol& symtable::pair_sym()
{
  lookup_p(",", __pair_sym);
  if (__pair_sym)
    return *__pair_sym;
  else
    return *sym_p(",", __pair_sym, 1200, infixr);
}

symbol& symtable::mapsto_sym()
{
  lookup_p("=>", __mapsto_sym);
  if (__mapsto_sym)
    return *__mapsto_sym;
  else
    return *sym_p("=>", __mapsto_sym, 1300, infix);
}

symbol& symtable::seq_sym()
{
  lookup_p("$$", __seq_sym);
  if (__seq_sym)
    return *__seq_sym;
  else
    return *sym_p("$$", __seq_sym, 1000, infixl);
}

symbol& symtable::not_sym()
{
  lookup_p("~", __not_sym);
  if (__not_sym)
    return *__not_sym;
  else
    return *sym_p("~", __not_sym, 1700, prefix);
}

symbol& symtable::bitnot_sym()
{
  lookup_p("not", __bitnot_sym);
  if (__bitnot_sym)
    return *__bitnot_sym;
  else
    return *sym_p("not", __bitnot_sym, 2400, prefix);
}

symbol& symtable::or_sym()
{
  lookup_p("||", __or_sym);
  if (__or_sym)
    return *__or_sym;
  else
    return *sym_p("||", __or_sym, 1500, infixr);
}

symbol& symtable::and_sym()
{
  lookup_p("&&", __and_sym);
  if (__and_sym)
    return *__and_sym;
  else
    return *sym_p("&&", __and_sym, 1600, infixr);
}

symbol& symtable::bitor_sym()
{
  lookup_p("or", __bitor_sym);
  if (__bitor_sym)
    return *__bitor_sym;
  else
    return *sym_p("or", __bitor_sym, 2200, infixl);
}

symbol& symtable::bitand_sym()
{
  lookup_p("and", __bitand_sym);
  if (__bitand_sym)
    return *__bitand_sym;
  else
    return *sym_p("and", __bitand_sym, 2300, infixl);
}

symbol& symtable::shl_sym()
{
  lookup_p("<<", __shl_sym);
  if (__shl_sym)
    return *__shl_sym;
  else
    return *sym_p("<<", __shl_sym, 2100, infixl);
}

symbol& symtable::shr_sym()
{
  lookup_p(">>", __shr_sym);
  if (__shr_sym)
    return *__shr_sym;
  else
    return *sym_p(">>", __shr_sym, 2100, infixl);
}

symbol& symtable::less_sym()
{
  lookup_p("<", __less_sym);
  if (__less_sym)
    return *__less_sym;
  else
    return *sym_p("<", __less_sym, 1800, infix);
}

symbol& symtable::greater_sym()
{
  lookup_p(">", __greater_sym);
  if (__greater_sym)
    return *__greater_sym;
  else
    return *sym_p(">", __greater_sym, 1800, infix);
}

symbol& symtable::lesseq_sym()
{
  lookup_p("<=", __lesseq_sym);
  if (__lesseq_sym)
    return *__lesseq_sym;
  else
    return *sym_p("<=", __lesseq_sym, 1800, infix);
}

symbol& symtable::greatereq_sym()
{
  lookup_p(">=", __greatereq_sym);
  if (__greatereq_sym)
    return *__greatereq_sym;
  else
    return *sym_p(">=", __greatereq_sym, 1800, infix);
}

symbol& symtable::equal_sym()
{
  lookup_p("==", __equal_sym);
  if (__equal_sym)
    return *__equal_sym;
  else
    return *sym_p("==", __equal_sym, 1800, infix);
}

symbol& symtable::notequal_sym()
{
  lookup_p("~=", __notequal_sym);
  if (__notequal_sym)
    return *__notequal_sym;
  else
    return *sym_p("~=", __notequal_sym, 1800, infix);
}

symbol& symtable::plus_sym()
{
  lookup_p("+", __plus_sym);
  if (__plus_sym)
    return *__plus_sym;
  else
    return *sym_p("+", __plus_sym, 2200, infixl);
}

symbol& symtable::minus_sym()
{
  lookup_p("-", __minus_sym);
  if (__minus_sym)
    return *__minus_sym;
  else
    return *sym_p("-", __minus_sym, 2200, infixl);
}

symbol& symtable::mult_sym()
{
  lookup_p("*", __mult_sym);
  if (__mult_sym)
    return *__mult_sym;
  else
    return *sym_p("*", __mult_sym, 2300, infixl);
}

symbol& symtable::fdiv_sym()
{
  lookup_p("/", __fdiv_sym);
  if (__fdiv_sym)
    return *__fdiv_sym;
  else
    return *sym_p("/", __fdiv_sym, 2300, infixl);
}

symbol& symtable::div_sym()
{
  lookup_p("div", __div_sym);
  if (__div_sym)
    return *__div_sym;
  else
    return *sym_p("div", __div_sym, 2300, infixl);
}

symbol& symtable::mod_sym()
{
  lookup_p("mod", __mod_sym);
  if (__mod_sym)
    return *__mod_sym;
  else
    return *sym_p("mod", __mod_sym, 2300, infixl);
}

symbol& symtable::amp_sym()
{
  lookup_p("&", __amp_sym);
  if (__amp_sym)
    return *__amp_sym;
  else
    return *sym_p("&", __amp_sym, 3000, postfix);
}

symbol& symtable::quoteop_sym()
{
  lookup_p("'", __quoteop_sym);
  if (__quoteop_sym)
    return *__quoteop_sym;
  else
    return *sym_p("'", __quoteop_sym, 2900, prefix);
}

symbol& symtable::complex_rect_sym()
{
  lookup_p("+:", __complex_rect_sym);
  if (__complex_rect_sym)
    return *__complex_rect_sym;
  else
    return *sym_p("+:", __complex_rect_sym, 2000, infix);
}

symbol& symtable::complex_polar_sym()
{
  lookup_p("<:", __complex_polar_sym);
  if (__complex_polar_sym)
    return *__complex_polar_sym;
  else
    return *sym_p("<:", __complex_polar_sym, 2000, infix);
}

symbol& symtable::rational_xdiv_sym()
{
  lookup_p("%", __rational_xdiv_sym);
  if (__rational_xdiv_sym)
    return *__rational_xdiv_sym;
  else
    return *sym_p("%", __rational_xdiv_sym, 2300, infixl);
}

symbol& symtable::ttag_sym()
{
  lookup_p("__type__", __ttag_sym);
  if (__ttag_sym)
    return *__ttag_sym;
  else
    return *sym_p("__type__", __ttag_sym, 10000, infixl);
}

symbol& symtable::astag_sym()
{
  lookup_p("__as__", __astag_sym);
  if (__astag_sym)
    return *__astag_sym;
  else
    return *sym_p("__as__", __astag_sym, 10000, infixl);
}

symbol& symtable::eqn_sym()
{
  lookup_p("-->", __eqn_sym);
  if (__eqn_sym)
    return *__eqn_sym;
  else
    return *sym_p("-->", __eqn_sym, 0, infix);
}

symbol& symtable::if_sym()
{
  lookup_p("__if__", __if_sym);
  if (__if_sym)
    return *__if_sym;
  else
    return *sym_p("__if__", __if_sym, 0, infixl);
}

symbol& symtable::when_sym()
{
  lookup_p("__when__", __when_sym);
  if (__when_sym)
    return *__when_sym;
  else
    return *sym_p("__when__", __when_sym, 0, infixl);
}

symbol& symtable::with_sym()
{
  lookup_p("__with__", __with_sym);
  if (__with_sym)
    return *__with_sym;
  else
    return *sym_p("__with__", __with_sym, 0, infixl);
}

bool symtable::check_minus_sym(int32_t f)
{
  if (f == minus_sym().f) return true;
  symbol* sym = (f>0)?&symtable::sym(f):0;
  if (sym && sym->prec >= 0 && sym->prec < PREC_MAX && sym->fix <= infixr) {
    size_t k = symsplit(sym->s);
    if (k == string::npos) return false;
    return sym->s.substr(k).compare("::-") == 0;
  } else
    return false;
}

symbol& symtable::neg_sym_of(int32_t f)
{
  assert(check_minus_sym(f));
  if (f == minus_sym().f) return neg_sym();
  symbol* sym = (f>0)?&symtable::sym(f):0;
  assert(sym && sym->prec >= 0 && sym->prec < PREC_MAX && sym->fix <= infixr);
  size_t k = symsplit(sym->s);
  assert(k != string::npos && sym->s.substr(k).compare("::-") == 0);
  string s = sym->s.substr(0, k);
  symbol* neg_sym = symtable::sym(s.append("::neg"));
  neg_sym->bminus = sym;
  return *neg_sym;
}

bool symtable::is_neg_sym(int32_t f)
{
  if (f == neg_sym().f) return true;
  symbol* sym = (f>0)?&symtable::sym(f):0;
  return sym && sym->bminus;
}

string symtable::neg_sym_pname(int32_t f)
{
  if (f == neg_sym().f) return "-";
  symbol* sym = (f>0)?&symtable::sym(f):0;
  assert(sym && sym->bminus);
  return sym->bminus->s;
}

prec_t symtable::neg_sym_nprec(int32_t f)
{
  prec_t p;
  if (f == neg_sym().f)
    p = minus_sym().prec;
  else {
    symbol* sym = (f>0)?&symtable::sym(f):0;
    assert(sym && sym->bminus);
    p = sym->bminus->prec;
  }
  p = nprec(p);
  if (p < NPREC_MAX) p += 3; // precedence of unary minus
  return p;
}
