
#include "printer.hh"
#include "interpreter.hh"
#include "expr.hh"
#include "util.hh"

#include <sstream>

#include "config.h"

#ifdef HAVE_GSL
#include <gsl/gsl_matrix.h>
#endif

static inline const string& pname(int32_t f)
{
  assert(f > 0);
  if (f == interpreter::g_interp->symtab.neg_sym().f) {
    static string uminus = "-";
    return uminus;
  } else {
    const symbol& sym = interpreter::g_interp->symtab.sym(f);
    return sym.s;
  }
}

static string pnamex(expr x)
{
  if (x.tag() > 0) return pname(x.tag());
  assert(x.tag() == EXPR::FVAR);
  if ((interpreter::g_verbose&verbosity::envs) != 0) {
    ostringstream os;
    os << pname(x.vtag()) << "/*" << (unsigned)x.vidx() << "*/";
    return os.str();
  } else
    return pname(x.vtag());
}

static inline const string sym_padding(int32_t f)
{
  assert(f > 0);
  const symbol& sym = interpreter::g_interp->symtab.sym(f);
  if (ispunct(sym.s[0]) || f == interpreter::g_interp->symtab.neg_sym().f)
    return "";
  else
    return " ";
}

static prec_t sym_nprec(int32_t f)
{
  assert(f > 0);
  if (f == interpreter::g_interp->symtab.neg_sym().f) {
    prec_t p = interpreter::g_interp->symtab.sym("-").prec*10;
    if (p < 100) p += 3; // precedence of unary minus
    return p;
  } else {
    const symbol& sym = interpreter::g_interp->symtab.sym(f);
    return nprec(sym.prec, sym.fix);
  }
}

static prec_t expr_nprec(expr x, bool aspat = true)
{
  if (x.is_null() || aspat && x.astag()>0) return 100;
  switch (x.tag()) {
  case EXPR::VAR:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::MATRIX:
    return 100;
  case EXPR::FVAR:
    return sym_nprec(x.vtag());
  case EXPR::INT:
    if (x.ival() < 0)
      // precedence of unary minus:
      return sym_nprec(interpreter::g_interp->symtab.neg_sym().f);
    else
      return 100;
  case EXPR::BIGINT:
    if (mpz_sgn(x.zval()) < 0)
      // precedence of unary minus:
      return sym_nprec(interpreter::g_interp->symtab.neg_sym().f);
    else
      return 100;
  case EXPR::DBL:
    /* NOTE: The check for negative zero really needs IEEE 754 floating point
       numbers, otherwise we'll divide by zero here. */
    if (x.dval() < 0.0 || x.dval() == 0.0 && 1.0/x.dval() < 0.0)
      // precedence of unary minus:
      return sym_nprec(interpreter::g_interp->symtab.neg_sym().f);
    else
      return 100;
  case EXPR::APP: {
    expr u, v, w;
    prec_t p;
    if (x.is_list())
      return 100;
    else if (x.is_app(u, v))
      if (u.tag() > 0 && (p = sym_nprec(u.tag())) < 100 && p%10 >= 3)
	// unary (prefix, postfix)
	return p;
      else if (u.is_app(v, w) && v.tag() > 0 &&
	       (p = sym_nprec(v.tag())) < 100 && p%10 < 3)
	// binary (infix, infixl, infixr)
	return p;
      else
	return 95;
    else
      return 95;
  }
  case EXPR::LAMBDA:
    return -30;
  case EXPR::COND:
    return -10;
  case EXPR::CASE:
  case EXPR::WHEN:
  case EXPR::WITH:
    return -20;
  default:
    return 100;
  }
}

struct pattern {
  expr x;
  bool pat;
  pattern(expr _x, bool _pat = true)
    : x(_x), pat(_pat) { }
};

static ostream& printx(ostream& os, const expr& x, bool pat,
		       bool aspat = true);

ostream& operator << (ostream& os, const pattern& p)
{
  return printx(os, p.x, p.pat);
}

struct paren {
  expr x;
  bool have, pat;
  paren(prec_t p, expr _x, bool _pat = false)
    : x(_x), have(expr_nprec(_x) < p), pat(_pat) { }
};

ostream& operator << (ostream& os, const paren& p)
{
  if (p.have)
    return os << '(' << pattern(p.x, p.pat) << ')';
  else
    return os << pattern(p.x, p.pat);
}

ostream& operator << (ostream& os, const expr& x)
{
  return printx(os, x, false);
}

static inline ostream& print_ttag(ostream& os, int8_t ttag)
{
  switch (ttag) {
  case EXPR::INT:
    return os << "::int";
  case EXPR::BIGINT:
    return os << "::bigint";
  case EXPR::DBL:
    return os << "::double";
  case EXPR::STR:
    return os << "::string";
  case EXPR::MATRIX:
    return os << "::matrix";
  case EXPR::DMATRIX:
    return os << "::dmatrix";
  case EXPR::CMATRIX:
    return os << "::cmatrix";
  case EXPR::IMATRIX:
    return os << "::imatrix";
  default:
    return os;
  }
}

static ostream& printx(ostream& os, const expr& x, bool pat, bool aspat)
{
  char buf[64];
  if (x.is_null()) return os << "#<NULL>";
  //os << "{" << x.refc() << "}";
  // handle "as" patterns
  if (aspat && x.astag()>0) {
    const symbol& sym = interpreter::g_interp->symtab.sym(x.astag());
    if (expr_nprec(x, false) < 100) {
      os << sym.s << "@(";
      printx(os, x, pat, false);
      return os << ")";
    } else {
      os << sym.s << "@";
      return printx(os, x, pat, false);
    }
  }
  switch (x.tag()) {
  case EXPR::VAR: {
    const symbol& sym = interpreter::g_interp->symtab.sym(x.vtag());
    os << sym.s;
    if ((interpreter::g_verbose&verbosity::envs) != 0) {
      os << "/*" << (unsigned)x.vidx() << ":";
      const path& p = x.vpath();
      for (size_t i = 0; i < p.len(); i++)
	os << p[i];
      os << "*/";
    }
    if (pat) print_ttag(os, x.ttag());
    return os;
  }
  case EXPR::FVAR: {
    assert(x.vtag() > 0);
    const symbol& sym = interpreter::g_interp->symtab.sym(x.vtag());
    if (sym.prec < 10) {
      os << '(' << sym.s;
      if ((interpreter::g_verbose&verbosity::envs) != 0) {
	os << "/*" << (unsigned)x.vidx() << "*/";
      }
      os << ')';
    } else {
      os << sym.s;
      if ((interpreter::g_verbose&verbosity::envs) != 0) {
	os << "/*" << (unsigned)x.vidx() << "*/";
      }
    }
    return os;
  }
  case EXPR::INT:
    return os << x.ival();
  case EXPR::BIGINT: {
    char *s = mpz_get_str(NULL, 10, x.zval());
    os << s << "L"; free(s);
    return os;
  }
  case EXPR::DBL: {
    double d = x.dval();
    if (is_inf(d))
      if (d > 0)
	strcpy(buf, "inf");
      else
	strcpy(buf, "-inf");
    else if (is_nan(d))
      strcpy(buf, "nan");
    else
      my_formatd(buf, "%0.15g", d);
    // make sure that the output conforms to Pure syntax
    os << buf;
    if (strchr("0123456789", buf[buf[0]=='-'?1:0]) &&
	!strchr(buf, '.') && !strchr(buf, 'e') && !strchr(buf, 'E'))
      os << ".0";
    return os;
  }
  case EXPR::STR: {
    char *s = printstr(x.sval());
    os << '"' << s << '"';
    free(s);
    return os;
  }
  case EXPR::PTR:
    return os << "#<pointer " << x.pval() << ">";
  case EXPR::MATRIX: {
    os << "{";
    for (exprll::const_iterator xs = x.xvals()->begin(),
	   end = x.xvals()->end(); xs != end; ) {
      size_t n = xs->size();
      if (n>1 || n==1 && xs->front().is_pair()) {
	// matrix elements at a precedence not larger than ',' have to be
	// parenthesized
	prec_t p = sym_nprec(interpreter::g_interp->symtab.pair_sym().f) + 1;
	for (exprl::const_iterator it = xs->begin(), end = xs->end();
	     it != end; ) {
	  os << paren(p, *it, pat);
	  if (++it != end) os << ",";
	}
      } else
	for (exprl::const_iterator it = xs->begin(), end = xs->end();
	     it != end; ) {
	  printx(os, *it, pat);
	  if (++it != end) os << ",";
	}
      if (++xs != end) os << ";";
    }
    return os << "}";
  }
  case EXPR::APP: {
    expr u, v, w, y;
    exprl xs;
    prec_t p;
    if (x.is_list(xs)) {
      // proper list value
      size_t n = xs.size();
      os << "[";
      if (n>1 || n==1 && xs.front().is_pair()) {
	// list elements at a precedence not larger than ',' have to be
	// parenthesized
	p = sym_nprec(interpreter::g_interp->symtab.pair_sym().f) + 1;
	for (exprl::const_iterator it = xs.begin(); it != xs.end(); ) {
	  os << paren(p, *it, pat);
	  if (++it != xs.end()) os << ",";
	}
      } else
	for (exprl::const_iterator it = xs.begin(); it != xs.end(); ) {
	  printx(os, *it, pat);
	  if (++it != xs.end()) os << ",";
	}
      return os << "]";
    } else if (x.is_app(u, v)) {
      if (u.ftag() > 0 && (p = sym_nprec(u.ftag())) < 100 && p%10 >= 3) {
	// unary operator
	string blank = sym_padding(u.ftag());
	prec_t q = expr_nprec(v);
	if (// both prefix/postfix => add parens for clarity if we don't do
	    // padding anyway:
	    q%10 == p%10 && blank.empty() ||
	    // mixed operators where subexpr has lower precedence => parens
	    // required:
	    q%10 != p%10 && q < p)
	  if (p%10 == 3)
	    // prefix
	    return os << pnamex(u) << blank << "(" << pattern(v, pat)
		      << ")";
	  else
	    // postfix
	    return os << "(" << pattern(v, pat) << ")" << blank
		      << pnamex(u);
	else
	  // no parens needed, just add padding
	  if (p%10 == 3)
	    // prefix
	    return os << pnamex(u) << blank << pattern(v, pat);
	  else
	    // postfix
	    return os << pattern(v, pat) << blank << pnamex(u);
      } else if (u.is_app(y, w) && y.ftag() > 0 &&
		 (p = sym_nprec(y.ftag())) < 100 && p%10 < 3) {
	// binary operator (infix, infixl, infixr)
	u = y; // u is the operator now, w the left, v the right operand
	string blank = sym_padding(u.ftag());
	prec_t l = p, r = p;
	// check left subexpr
	prec_t q = expr_nprec(w);
	if (p == q) {
	  // operators of same precedence, associativity decides
	  switch (p%10) {
	  case 0:
	    // infix (non-associative) will give a syntax error, use a plain
	    // application instead
	    u = x.xval1(); v = x.xval2();
	    return os << paren(95, u, pat) << " " << paren(100, v, pat);
	  case 2:
	    // infixr, need parens
	    l++;
	  }
	}
	// check right subexpr
	q = expr_nprec(v);
	if (p == q) {
	  // operators of same precedence, associativity decides
	  switch (p%10) {
	  case 0:
	    // infix (non-associative) will give a syntax error, use a plain
	    // application instead
	    u = x.xval1(); v = x.xval2();
	    return os << paren(95, u, pat) << " " << paren(100, v, pat);
	  case 1:
	    // infixl, need parens
	    r++;
	  }
	}
	return os << paren(l, w, pat) << blank << pnamex(u)
		  << blank << paren(r, v, pat);
      } else {
	u = x.xval1(); v = x.xval2();
	return os << paren(95, u, pat) << " " << paren(100, v, pat);
      }
    } else {
      u = x.xval1(); v = x.xval2();
      return os << paren(95, u, pat) << " " << paren(100, v, pat);
    }
  }
  case EXPR::LAMBDA: {
    expr u = x.xval1(), v = x.xval2();
    os << '\\' << paren(100, u, true) << " -> " << v;
    if ((interpreter::g_verbose&verbosity::code) && x.pm())
      os << " " << *x.pm();
    return os;
  }
  case EXPR::COND: {
    expr u = x.xval1(), v = x.xval2(), w = x.xval3();
    return os << "if " << paren(0, u) << " then " << v << " else " << w;
  }
  case EXPR::CASE: {
    expr u = x.xval();
    os << "case " << paren(-10, u) << " of " << *x.rules();
    if ((interpreter::g_verbose&verbosity::code) && x.pm())
      os << " " << *x.pm();
    os << " end";
    return os;
  }
  case EXPR::WHEN:
    os << x.xval() << " when " << *x.rules();
    if ((interpreter::g_verbose&verbosity::code) && x.pm()) {
      size_t n = x.rules()->size();
      matcher *pm = x.pm();
      // we print multiple matchers in 'when' clauses in reverse, so that they
      // look equivalent to a nested 'when' (which, in fact, they are)
      for (size_t i = 0; i < n; i++)
	os << " " << pm[n-i-1];
    }
    os << " end";
    return os;
  case EXPR::WITH:
    return os << x.xval() << " with " << *x.fenv() << " end";
  default: {
    assert(x.tag() > 0);
    const symbol& sym = interpreter::g_interp->symtab.sym(x.tag());
    if (sym.prec < 10)
      return os << '(' << sym.s << ')';
    else
      return os << sym.s;
  }
  }
}

ostream& operator << (ostream& os, const exprl& xl)
{
  for (exprl::const_iterator it = xl.begin(); it != xl.end(); ) {
    os << *it;
    if (++it != xl.end()) os << " ";
  }
  return os;
}

ostream& operator << (ostream& os, const rule& r)
{
  printx(os, r.lhs, true); os << " = " << r.rhs;
  if (!r.qual.is_null()) os << " if " << r.qual;
  return os;
}

ostream& operator << (ostream& os, const rulel& rl)
{
  for (rulel::const_iterator it = rl.begin(); it != rl.end(); ) {
    os << *it;
    if (++it != rl.end()) os << "; ";
  }
  return os;
}

ostream& operator << (ostream& os, const env& e)
{
  for (env::const_iterator it = e.begin(); it != e.end(); ) {
    int32_t f = it->first;
    assert(f>0);
    const symbol& sym = interpreter::g_interp->symtab.sym(f);
    const env_info& info = it->second;
    switch (info.t) {
    case env_info::none:
      break;
    case env_info::lvar: {
      os << sym.s;
      print_ttag(os, info.ttag);
      if ((interpreter::g_verbose&verbosity::envs) != 0) {
	os << " = /*";
	const path& p = *info.p;
	for (size_t i = 0; i < p.len(); i++)
	  os << p[i];
	os << "*/";
      }
      break;
    }
    case env_info::cvar:
      os << "const " << sym.s << " = " << *info.cval;
      break;
    case env_info::fvar:
      os << "let " << sym.s << " = " << *(pure_expr**)info.val;
      break;
    case env_info::fun:
      os << *info.rules;
      if ((interpreter::g_verbose&verbosity::code) && info.m)
	os << " " << *info.m;
      break;
    }
    if (++it != e.end()) os << "; ";
  }
  return os;
}

/* Print a pattern matching automaton. Useful for debugging purposes. */

ostream& operator << (ostream& os, const trans& tr)
{
  switch (tr.tag) {
  case EXPR::APP:
    return os << "\t<app> state " << tr.st->s << endl;
  case EXPR::VAR:
    os << "\t<var>";
    print_ttag(os, tr.ttag);
    return os << " state " << tr.st->s << endl;
  case EXPR::INT:
    os << "\t" << tr.i;
    print_ttag(os, tr.ttag);
    return os << " state " << tr.st->s << endl;
  case EXPR::BIGINT: {
    char *s = mpz_get_str(NULL, 10, tr.z);
    os << "\t" << s << "L";
    print_ttag(os, tr.ttag);
    os << " state " << tr.st->s << endl;
    free(s);
    return os;
  }
  case EXPR::DBL:
    os << "\t" << tr.d;
    print_ttag(os, tr.ttag);
    return os << " state " << tr.st->s << endl;
  case EXPR::STR: {
    char *s = printstr(tr.s);
    os << "\t" << '"' << s << '"';
    print_ttag(os, tr.ttag);
    os << " state " << tr.st->s << endl;
    free(s);
    return os;
  }
  default: {
    assert(tr.tag > 0);
    const symbol& sym = interpreter::g_interp->symtab.sym(tr.tag);
    return os << "\t" << sym.s << " state " << tr.st->s << endl;
  }
  }
}

ostream& operator << (ostream& os, const state& st)
{
  os << "  state " << st.s << ":";
  ruleml::const_iterator r;
  for (r = st.r.begin(); r != st.r.end(); r++)
    os << " #" << *r;
  os << endl;
  transl::const_iterator t;
  for (t = st.tr.begin(); t != st.tr.end(); t++)
    os << *t;
  return os;
}

ostream& operator << (ostream& os, const matcher& m)
{
  uint8_t s_verbose = interpreter::g_verbose;
  interpreter::g_verbose = 0;
  os << "{\n";
  size_t n = m.r.size();
  for (size_t i = 0; i < n; i++)
    os << "  rule #" << i << ": " << m.r[i] << endl;
  n = m.st.size();
  for (size_t i = 0; i < n; i++)
    os << *m.st[i];
  os << "}";
  interpreter::g_verbose = s_verbose;
  return os;
}

static bool pure_is_nil(const pure_expr *x)
{
  return x->tag == interpreter::g_interp->symtab.nil_sym().f;
}

static bool pure_is_cons(const pure_expr *x)
{
  if (x->tag == EXPR::APP && x->data.x[0]->tag == EXPR::APP)
    return x->data.x[0]->data.x[0]->tag == interpreter::g_interp->symtab.cons_sym().f;
  else
    return false;
}

static bool pure_is_pair(const pure_expr *x)
{
  if (x->tag == EXPR::APP && x->data.x[0]->tag == EXPR::APP)
    return x->data.x[0]->data.x[0]->tag == interpreter::g_interp->symtab.pair_sym().f;
  else
    return false;
}

static bool pure_is_list(const pure_expr *x)
{
  while (pure_is_cons(x))
    x = x->data.x[1];
  return pure_is_nil(x);
}

static bool pure_is_list(const pure_expr *x, list<const pure_expr*>& xs)
{
  while (pure_is_cons(x)) {
    xs.push_back(x->data.x[0]->data.x[1]);
    x = x->data.x[1];
  }
  return pure_is_nil(x);
}

static prec_t pure_expr_nprec(const pure_expr *x)
{
  assert(x);
  switch (x->tag) {
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::MATRIX:
  case EXPR::DMATRIX:
  case EXPR::CMATRIX:
  case EXPR::IMATRIX:
    return 100;
  case EXPR::INT:
    if (x->data.i < 0)
      // precedence of unary minus:
      return sym_nprec(interpreter::g_interp->symtab.neg_sym().f);
    else
      return 100;
  case EXPR::BIGINT:
    if (mpz_sgn(x->data.z) < 0)
      // precedence of unary minus:
      return sym_nprec(interpreter::g_interp->symtab.neg_sym().f);
    else
      return 100;
  case EXPR::DBL:
    /* NOTE: The check for negative zero really needs IEEE 754 floating point
       numbers, otherwise we'll divide by zero here. */
    if (x->data.d < 0.0 || x->data.d == 0.0 && 1.0/x->data.d < 0.0)
      // precedence of unary minus:
      return sym_nprec(interpreter::g_interp->symtab.neg_sym().f);
    else
      return 100;
  case EXPR::APP:
    if (pure_is_list(x))
      return 100;
    else {
      const pure_expr *u = x->data.x[0], *v = x->data.x[1], *w;
      prec_t p;
      if (u->tag > 0 && (p = sym_nprec(u->tag)) < 100 && p%10 >= 3)
	// unary (prefix, postfix)
	return p;
      else if (u->tag == EXPR::APP) {
	v = u->data.x[0]; w = u->data.x[1];
	if (v->tag > 0 && (p = sym_nprec(v->tag)) < 100 && p%10 < 3)
	  // binary (infix, infixl, infixr)
	  return p;
	else
	  return 95;
      } else
	return 95;
    }
  default:
    assert(x->tag >= 0);
    return 100;
  }
}

struct pure_paren {
  const pure_expr *x;
  bool have;
  pure_paren(prec_t p, const pure_expr *_x)
    : x(_x), have(pure_expr_nprec(_x) < p) { }
};

ostream& operator << (ostream& os, const pure_paren& p)
{
  if (p.have)
    return os << '(' << p.x << ')';
  else
    return os << p.x;
}

static inline bool pstr(ostream& os, pure_expr *x)
{
  static bool recursive = false;
  if (recursive ||
      // We don't want to force a thunk here. Unfortunately, this means that
      // currently you can't define a print representation for a thunk, at
      // least not directly. :(
      x->tag == 0 && x->data.clos && x->data.clos->n == 0)
    return false;
  interpreter& interp = *interpreter::g_interp;
  int32_t f = interp.symtab.__show__sym;
  if (f > 0 && interp.globenv.find(f) != interp.globenv.end()) {
    assert(x->refc > 0);
    pure_exception ex; ex.e = 0; ex.sz = interp.sstk_sz;
    interp.estk.push_front(ex);
    if (setjmp(interp.estk.front().jmp)) {
      // caught an exception
      size_t sz = interp.estk.front().sz;
      pure_expr* e = interp.estk.front().e;
      interp.estk.pop_front();
      if (e) pure_freenew(e);
      for (size_t i = interp.sstk_sz; i-- > sz; )
	if (interp.sstk[i] && interp.sstk[i]->refc > 0)
	  pure_free(interp.sstk[i]);
      interp.sstk_sz = sz;
      return false;
    } else {
      recursive = true;
      pure_expr *y = pure_app(pure_symbol(f), x);
      interp.estk.pop_front();
      recursive = false;
      assert(y);
      if (y->tag == EXPR::STR) {
	char *s = fromutf8(y->data.s);
	pure_freenew(y);
	if (s) {
	  os << s; free(s);
	  return true;
	} else
	  return false;
      } else
	return false;
    }
  } else
    return false;
}

static inline ostream& print_double(ostream& os, double d)
{
  char buf[64];
  if (is_inf(d))
    if (d > 0)
      strcpy(buf, "inf");
    else
      strcpy(buf, "-inf");
  else if (is_nan(d))
    strcpy(buf, "nan");
  else
    my_formatd(buf, "%0.15g", d);
  // make sure that the output conforms to Pure syntax
  os << buf;
  if (strchr("0123456789", buf[buf[0]=='-'?1:0]) &&
      !strchr(buf, '.') && !strchr(buf, 'e') && !strchr(buf, 'E'))
    os << ".0";
  return os;
}

ostream& operator << (ostream& os, const pure_expr *x)
{
  char test;
  if (interpreter::stackmax > 0 &&
      interpreter::stackdir*(&test - interpreter::baseptr) >=
      interpreter::stackmax)
    throw err("stack overflow in printer");
  assert(x);
  if (pstr(os, (pure_expr*)x)) return os;
  //os << "{" << x->refc << "}";
  switch (x->tag) {
  case EXPR::INT:
    return os << x->data.i;
  case EXPR::BIGINT: {
    char *s = mpz_get_str(NULL, 10, x->data.z);
    os << s << "L"; free(s);
    return os;
  }
  case EXPR::DBL:
    return print_double(os, x->data.d);
  case EXPR::STR: {
    char *s = printstr(x->data.s);
    os << '"' << s << '"';
    free(s);
    return os;
  }
  case EXPR::PTR:
    return os << "#<pointer " << x->data.p << ">";
  /* NOTE: For performance reasons, we don't do any custom representations for
     matrix elements. As a workaround, you can define __show__ on matrices as
     a whole. */
  case EXPR::MATRIX:
    os << "{";
    if (x->data.mat.p) {
      gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
      if (m->size1>0 && m->size2>0) {
	prec_t p = sym_nprec(interpreter::g_interp->symtab.pair_sym().f) + 1;
	for (size_t i = 0; i < m->size1; i++) {
	  if (i > 0) os << ";";
	  for (size_t j = 0; j < m->size2; j++) {
	    if (j > 0) os << ",";
	    os << pure_paren(p, m->data[i * m->tda + j]);
	  }
	}
      }
    }
    return os << "}";
#ifdef HAVE_GSL
  case EXPR::DMATRIX:
    os << "{";
    if (x->data.mat.p) {
      gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
      if (m->size1>0 && m->size2>0) {
	for (size_t i = 0; i < m->size1; i++) {
	  if (i > 0) os << ";";
	  for (size_t j = 0; j < m->size2; j++) {
	    if (j > 0) os << ",";
	    print_double(os, m->data[i * m->tda + j]);
	  }
	}
      }
    }
    return os << "}";
  case EXPR::IMATRIX:
    os << "{";
    if (x->data.mat.p) {
      gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
      if (m->size1>0 && m->size2>0) {
	if (m->size1>0 && m->size2>0) {
	  for (size_t i = 0; i < m->size1; i++) {
	    if (i > 0) os << ";";
	    for (size_t j = 0; j < m->size2; j++) {
	      if (j > 0) os << ",";
	      os << m->data[i * m->tda + j];
	    }
	  }
	}
      }
    }
    return os << "}";
  case EXPR::CMATRIX:
    /* Print complex values in rectangular format using the infix notation
       defined in math.pure. FIXME: We require the +: symbol to be predefined
       no matter whether math.pure has actually been loaded. */
    os << "{";
    if (x->data.mat.p) {
      interpreter& interp = *interpreter::g_interp;
      symbol *rect = interp.symtab.complex_rect_sym(true);
      string& rectsym = rect->s;
      gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
      if (m->size1>0 && m->size2>0) {
	for (size_t i = 0; i < m->size1; i++) {
	  if (i > 0) os << ";";
	  for (size_t j = 0; j < m->size2; j++) {
	    if (j > 0) os << ",";
	    print_double(os, m->data[2*(i * m->tda + j)]);
	    os << rectsym;
	    print_double(os, m->data[2*(i * m->tda + j) + 1]);
	  }
	}
      }
    }
    return os << "}";
#else
  case EXPR::DMATRIX:
    return os << "#<dmatrix " << x->data.mat.p << ">";
  case EXPR::IMATRIX:
    return os << "#<imatrix " << x->data.mat.p << ">";
  case EXPR::CMATRIX:
    return os << "#<cmatrix " << x->data.mat.p << ">";
#endif
  case EXPR::APP: {
    list<const pure_expr*> xs;
    prec_t p;
    if (pure_is_list(x, xs)) {
      // proper list value
      size_t n = xs.size();
      os << "[";
      if (n>1 || n==1 && pure_is_pair(xs.front())) {
	// list elements at a precedence not larger than ',' have to be
	// parenthesized
	p = sym_nprec(interpreter::g_interp->symtab.pair_sym().f) + 1;
	for (list<const pure_expr*>::const_iterator it = xs.begin();
	     it != xs.end(); ) {
	  os << pure_paren(p, *it);
	  if (++it != xs.end()) os << ",";
	}
      } else
	for (list<const pure_expr*>::const_iterator it = xs.begin();
	     it != xs.end(); ) {
	  os << *it;
	  if (++it != xs.end()) os << ",";
	}
      return os << "]";
    }
    const pure_expr *u = x->data.x[0], *v = x->data.x[1], *w, *y;
    if (u->tag > 0 && (p = sym_nprec(u->tag)) < 100 && p%10 >= 3) {
      // unary operator
      string blank = sym_padding(u->tag);
      prec_t q = pure_expr_nprec(v);
      if (// both prefix/postfix => add parens for clarity if we don't do
	  // padding anyway:
	  q%10 == p%10 && blank.empty() ||
	  // mixed operators where subexpr has lower precedence => parens
	  // required:
	  q%10 != p%10 && q < p)
	if (p%10 == 3)
	  // prefix
	  return os << pname(u->tag) << blank << "(" << v << ")";
	else
	  // postfix
	  return os << "(" << v << ")" << blank << pname(u->tag);
      else
	// no parens needed, just add padding
	if (p%10 == 3)
	  // prefix
	  return os << pname(u->tag) << blank << v;
	else
	  // postfix
	  return os << v << blank << pname(u->tag);
    } else if (u->tag == EXPR::APP && (y = u->data.x[0])->tag > 0 &&
	       (p = sym_nprec(y->tag)) < 100 && p%10 < 3) {
      // binary operator (infix, infixl, infixr)
      w = u->data.x[1]; u = y;
      // u is the operator now, w the left, v the right operand
      string blank = sym_padding(u->tag);
      prec_t l = p, r = p;
      // check left subexpr
      prec_t q = pure_expr_nprec(w);
      if (p == q) {
	// operators of same precedence, associativity decides
	switch (p%10) {
	case 0:
	  // infix (non-associative) will give a syntax error, use a plain
	  // application instead
	  u = x->data.x[0]; v = x->data.x[1];
	  return os << pure_paren(95, u) << " " << pure_paren(100, v);
	case 2:
	  // infixr, need parens
	  l++;
	}
      }
      // check right subexpr
      q = pure_expr_nprec(v);
      if (p == q) {
	// operators of same precedence, associativity decides
	switch (p%10) {
	case 0:
	  // infix (non-associative) will give a syntax error, use a plain
	  // application instead
	  u = x->data.x[0]; v = x->data.x[1];
	  return os << pure_paren(95, u) << " " << pure_paren(100, v);
	case 1:
	  // infixl, need parens
	  r++;
	}
      }
      return os << pure_paren(l, w) << blank << pname(u->tag)
		<< blank << pure_paren(r, v);
    } else
      return os << pure_paren(95, u) << " " << pure_paren(100, v);
  }
  default: {
    if (x->tag == 0) {
      const char *s = (x->data.clos && x->data.clos->n==0)?"thunk":"closure";
      return os << "#<" << s << " " << (void*)x << ">";
    }
    const symbol& sym = interpreter::g_interp->symtab.sym(x->tag);
    if (x->data.clos && x->data.clos->local)
      return os << "#<closure " << sym.s << ">";
    if (sym.prec < 10)
      return os << '(' << sym.s << ')';
    else
      return os << sym.s;
  }
  }
}
