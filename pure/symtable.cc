
#include "symtable.hh"
#include "util.hh"
#include <assert.h>

symtable::symtable()
  : fno(0), rtab(1024),
    __nil_sym(0),
    __cons_sym(0),
    __void_sym(0),
    __pair_sym(0),
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
    current_namespace(new string),
    search_namespaces(new set<string>),
    __show__sym(0)
{
  // enter any additional predefined symbols here, e.g.:
  //sym("-", 6, infixl);
  sym("_"); // anonymous variable
}

symtable::~symtable()
{
  delete current_namespace;
  delete search_namespaces;
}

void symtable::init_builtins()
{
  nil_sym();
  cons_sym();
  void_sym();
  pair_sym();
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
	   << sym->fix << " " << sym->priv << endl;
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

inline symbol* symtable::lookup_p(const char *s, int& count)
{
  map<string, symbol>::iterator it = tab.find(s);
  count = it != tab.end();
  if (!count || !visible(it->second))
    return 0;
  else
    return &it->second;
}

inline symbol* symtable::lookup_p(const char *s)
{
  int count;
  return lookup_p(s, count);
}

inline symbol* symtable::lookup_p(const char *s, symbol*& cache)
{
  if (!cache)
    cache = lookup_p(s);
  return cache;
}

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
  assert(prec <= 10);
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
  const char *t = strstr(s, "::");
  if (t == s) {
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
  if (!current_namespace->empty()) {
    string id = (*current_namespace)+"::"+s;
    symbol *sym = lookup_p(id.c_str());
    if (sym) {
      count = 1;
      return sym;
    }
  }
  // next scan the search namespaces; if the symbol is ambiguous, bail out
  // with an error here
  for (set<string>::iterator it = search_namespaces->begin(),
	 end = search_namespaces->end(); it != end; it++) {
    string id = (*it)+"::"+s;
    int priv2;
    symbol *sym = lookup_p(id.c_str(), priv2);
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
  assert(prec <= 10);
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
  symbol *_sym = sym(s, priv);
  if (_sym)
    return *_sym;
  else if (count > 1)
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
    size_t k = sym.s.find("::");
    if (k == string::npos) k = 0;
    string qual = sym.s.substr(0, k);
    return qual.empty() || qual == *current_namespace;
  } else
    return true;
}

symbol& symtable::nil_sym()
{
  lookup_p("[]", __nil_sym);
  if (__nil_sym)
    return *__nil_sym;
  else
    return *sym_p("[]", __nil_sym, 10, nullary);
}

symbol& symtable::cons_sym()
{
  lookup_p(":", __cons_sym);
  if (__cons_sym)
    return *__cons_sym;
  else
    return *sym_p(":", __cons_sym, 4, infixr);
}

symbol& symtable::void_sym()
{
  lookup_p("()", __void_sym);
  if (__void_sym)
    return *__void_sym;
  else
    return *sym_p("()", __void_sym, 10, nullary);
}

symbol& symtable::pair_sym()
{
  lookup_p(",", __pair_sym);
  if (__pair_sym)
    return *__pair_sym;
  else
    return *sym_p(",", __pair_sym, 1, infixr);
}

symbol& symtable::seq_sym()
{
  lookup_p("$$", __seq_sym);
  if (__seq_sym)
    return *__seq_sym;
  else
    return *sym_p("$$", __seq_sym, 0, infixl);
}

symbol& symtable::not_sym()
{
  lookup_p("~", __not_sym);
  if (__not_sym)
    return *__not_sym;
  else
    return *sym_p("~", __not_sym, 3, prefix);
}

symbol& symtable::bitnot_sym()
{
  lookup_p("not", __bitnot_sym);
  if (__bitnot_sym)
    return *__bitnot_sym;
  else
    return *sym_p("not", __bitnot_sym, 7, prefix);
}

symbol& symtable::or_sym()
{
  lookup_p("||", __or_sym);
  if (__or_sym)
    return *__or_sym;
  else
    return *sym_p("||", __or_sym, 2, infixr);
}

symbol& symtable::and_sym()
{
  lookup_p("&&", __and_sym);
  if (__and_sym)
    return *__and_sym;
  else
    return *sym_p("&&", __and_sym, 3, infixr);
}

symbol& symtable::bitor_sym()
{
  lookup_p("or", __bitor_sym);
  if (__bitor_sym)
    return *__bitor_sym;
  else
    return *sym_p("or", __bitor_sym, 6, infixl);
}

symbol& symtable::bitand_sym()
{
  lookup_p("and", __bitand_sym);
  if (__bitand_sym)
    return *__bitand_sym;
  else
    return *sym_p("and", __bitand_sym, 7, infixl);
}

symbol& symtable::shl_sym()
{
  lookup_p("<<", __shl_sym);
  if (__shl_sym)
    return *__shl_sym;
  else
    return *sym_p("<<", __shl_sym, 5, infixl);
}

symbol& symtable::shr_sym()
{
  lookup_p(">>", __shr_sym);
  if (__shr_sym)
    return *__shr_sym;
  else
    return *sym_p(">>", __shr_sym, 5, infixl);
}

symbol& symtable::less_sym()
{
  lookup_p("<", __less_sym);
  if (__less_sym)
    return *__less_sym;
  else
    return *sym_p("<", __less_sym, 4, infix);
}

symbol& symtable::greater_sym()
{
  lookup_p(">", __greater_sym);
  if (__greater_sym)
    return *__greater_sym;
  else
    return *sym_p(">", __greater_sym, 4, infix);
}

symbol& symtable::lesseq_sym()
{
  lookup_p("<=", __lesseq_sym);
  if (__lesseq_sym)
    return *__lesseq_sym;
  else
    return *sym_p("<=", __lesseq_sym, 4, infix);
}

symbol& symtable::greatereq_sym()
{
  lookup_p(">=", __greatereq_sym);
  if (__greatereq_sym)
    return *__greatereq_sym;
  else
    return *sym_p(">=", __greatereq_sym, 4, infix);
}

symbol& symtable::equal_sym()
{
  lookup_p("==", __equal_sym);
  if (__equal_sym)
    return *__equal_sym;
  else
    return *sym_p("==", __equal_sym, 4, infix);
}

symbol& symtable::notequal_sym()
{
  lookup_p("~=", __notequal_sym);
  if (__notequal_sym)
    return *__notequal_sym;
  else
    return *sym_p("~=", __notequal_sym, 4, infix);
}

symbol& symtable::plus_sym()
{
  lookup_p("+", __plus_sym);
  if (__plus_sym)
    return *__plus_sym;
  else
    return *sym_p("+", __plus_sym, 6, infixl);
}

symbol& symtable::minus_sym()
{
  lookup_p("-", __minus_sym);
  if (__minus_sym)
    return *__minus_sym;
  else
    return *sym_p("-", __minus_sym, 6, infixl);
}

symbol& symtable::mult_sym()
{
  lookup_p("*", __mult_sym);
  if (__mult_sym)
    return *__mult_sym;
  else
    return *sym_p("*", __mult_sym, 7, infixl);
}

symbol& symtable::fdiv_sym()
{
  lookup_p("/", __fdiv_sym);
  if (__fdiv_sym)
    return *__fdiv_sym;
  else
    return *sym_p("/", __fdiv_sym, 7, infixl);
}

symbol& symtable::div_sym()
{
  lookup_p("div", __div_sym);
  if (__div_sym)
    return *__div_sym;
  else
    return *sym_p("div", __div_sym, 7, infixl);
}

symbol& symtable::mod_sym()
{
  lookup_p("mod", __mod_sym);
  if (__mod_sym)
    return *__mod_sym;
  else
    return *sym_p("mod", __mod_sym, 7, infixl);
}

symbol& symtable::amp_sym()
{
  lookup_p("&", __amp_sym);
  if (__amp_sym)
    return *__amp_sym;
  else
    return *sym_p("&", __amp_sym, 9, postfix);
}

symbol& symtable::quoteop_sym()
{
  lookup_p("'", __quoteop_sym);
  if (__quoteop_sym)
    return *__quoteop_sym;
  else
    return *sym_p("'", __quoteop_sym, 9, prefix);
}

symbol& symtable::complex_rect_sym()
{
  lookup_p("+:", __complex_rect_sym);
  if (__complex_rect_sym)
    return *__complex_rect_sym;
  else
    return *sym_p("+:", __complex_rect_sym, 5, infix);
}

symbol& symtable::complex_polar_sym()
{
  lookup_p("<:", __complex_polar_sym);
  if (__complex_polar_sym)
    return *__complex_polar_sym;
  else
    return *sym_p("<:", __complex_polar_sym, 5, infix);
}

symbol& symtable::rational_xdiv_sym()
{
  lookup_p("%", __rational_xdiv_sym);
  if (__rational_xdiv_sym)
    return *__rational_xdiv_sym;
  else
    return *sym_p("+:", __rational_xdiv_sym, 7, infixl);
}
