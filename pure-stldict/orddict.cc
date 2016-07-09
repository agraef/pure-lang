
// This is completely analogous to hashdict.cc (which see).

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <pure/runtime.h>
#include <map>
#include <algorithm>

/* Define this for C++ TR1 extensions support. This is needed to get tuple and
   unordered_map for some older C++ libraries which don't provide full C++11
   support yet. */
//#define HAVE_TR1
#ifdef HAVE_TR1
#include <tr1/tuple>
using namespace std;
using namespace std::tr1;
#else
#include <tuple>
using namespace std;
#endif

// Enable this for some additional (possibly costly) assertions in the code.
//#define DEBUG 1

// Enable this if your C++ library has the std::is_permutation function.
//#define HAVE_STD_IS_PERMUTATION 1

// This should really be in the runtime.

typedef struct _gsl_block_symbolic
{
  size_t size;
  pure_expr **data;
} gsl_block_symbolic;

typedef struct _gsl_matrix_symbolic
{
  size_t size1;
  size_t size2;
  size_t tda;
  pure_expr **data;
  gsl_block_symbolic *block;
  int owner;
} gsl_matrix_symbolic;

static gsl_matrix_symbolic*
gsl_matrix_symbolic_alloc(const size_t n1, const size_t n2)
{
  gsl_block_symbolic* block;
  gsl_matrix_symbolic* m;
  if (n1 == 0 || n2 == 0)
    return 0;
  m = (gsl_matrix_symbolic*)malloc(sizeof(gsl_matrix_symbolic));
  if (m == 0)
    return 0;
  block = (gsl_block_symbolic*)malloc(sizeof(gsl_block_symbolic));
  if (block == 0) {
    free(m);
    return 0;
  }
  block->size = n1*n2;
  block->data = (pure_expr**)malloc(block->size*sizeof(pure_expr*)) ;
  if (block->data == 0) {
    free(m);
    free(block);
    return 0;
  }
  m->data = block->data;
  m->size1 = n1;
  m->size2 = n2;
  m->tda = n2;
  m->block = block;
  m->owner = 1;
  return m;
}

static gsl_matrix_symbolic*
gsl_matrix_symbolic_calloc(const size_t n1, const size_t n2)
{
  gsl_matrix_symbolic* m = gsl_matrix_symbolic_alloc(n1, n2);
  if (m == 0) return 0;
  memset(m->data, 0, m->block->size*sizeof(pure_expr*));
  return m;
}

static inline gsl_matrix_symbolic*
create_symbolic_matrix(size_t nrows, size_t ncols)
{
  if (nrows == 0 || ncols == 0 ) {
    size_t nrows1 = (nrows>0)?nrows:1;
    size_t ncols1 = (ncols>0)?ncols:1;
    gsl_matrix_symbolic *m = gsl_matrix_symbolic_calloc(nrows1, ncols1);
    if (!m) return 0;
    m->size1 = nrows; m->size2 = ncols;
    return m;
  } else
    return gsl_matrix_symbolic_alloc(nrows, ncols);
}

static pure_expr *pure_symbolic_vectorv(size_t n, pure_expr **xv)
{
  gsl_matrix_symbolic *mat = create_symbolic_matrix(1, n);
  if (!mat) return 0;
  pure_expr **data = mat->data;
  for (size_t i = 0; i < n; i++)
    data[i] = xv[i];
  return pure_symbolic_matrix(mat);
}

static bool pure_is_symbolic_vectorv(pure_expr *x, size_t *n, pure_expr ***xv)
{
  gsl_matrix_symbolic *mat;
  if (!pure_is_symbolic_matrix(x, (void**)&mat) || !mat ||
      (mat->size1 > 1 && mat->size2 > 1))
    return false;
  size_t sz = mat->size1*mat->size2, tda = mat->tda;
  pure_expr **data = mat->data;
  if (n) *n = sz;
  if (xv) {
    *xv = 0; if (sz == 0) return true;
    pure_expr **xs = (pure_expr**)malloc(sz*sizeof(pure_expr**));
    assert(xs);
    for (size_t i = 0, k = 0; i < mat->size1; i++)
      for (size_t j = 0; j < mat->size2; j++)
	xs[k++] = data[i*tda+j];
    *xv = xs;
  }
  return true;
}

using namespace std;

// A little helper class to keep track of interpreter-local data.

template <class T>
struct ILS {
  pure_interp_key_t key;
  T val;
  /* This is safe to invoke at any time. */
  ILS() : key(pure_interp_key(free)), val(T()) {}
  ILS(T const& x) : key(pure_interp_key(free)), val(x) {}
  /* This must only be invoked after an interpreter instance is available. It
     will return a different reference to an object of type T (initialized to
     the default value, if given) for each interpreter. */
  T& operator()();
};

template <class T>
T& ILS<T>::operator()()
{
  T *ptr = (T*)pure_interp_get(key);
  if (!ptr) {
    ptr = (T*)malloc(sizeof(T)); assert(ptr);
    pure_interp_set(key, ptr);
    *ptr = val;
  }
  return *ptr;
}

// Comparing Pure expressions. This is done using Pure's standard '<'
// predicate, so this needs to be defined on all key values used in an orddict
// (if not, a failed_cond Pure exception will be thrown).

enum {
  INT		= -3,	// 32 bit signed integer
  BIGINT	= -4,	// bigint (mpz_t)
  DBL		= -5,	// double precision floating point number
  STR		= -6,	// utf-8 string (char*)
};

static bool less_than(pure_expr *x, pure_expr *y)
{
  // Optimize some common cases of POD (plain old data).
  if (x->tag == y->tag && x->tag < 0) {
    switch (x->tag) {
    case INT:
      return x->data.i < y->data.i;
    case DBL:
      return x->data.d < y->data.d;
    case BIGINT:
      return bigint_cmp(x->data.z, y->data.z) < 0;
    case STR:
      return strcmp(x->data.s, y->data.s) < 0;
    }
  }
  static ILS<int32_t> _lt_sym = 0, _failed_cond_sym = 0;
  int32_t &lt_sym = _lt_sym(), &failed_cond_sym = _failed_cond_sym();
  if (!lt_sym) lt_sym = pure_getsym("<");
  if (!failed_cond_sym)
    failed_cond_sym = pure_getsym("failed_cond");
  assert(lt_sym > 0);
  pure_expr *res = pure_appl(pure_symbol(lt_sym), 2, x, y);
  int32_t rc;
  if (!pure_is_int(res, &rc)) {
    pure_freenew(res);
    pure_throw((failed_cond_sym>0)?
	       pure_symbol(failed_cond_sym):0);
    return false;
  }
  pure_freenew(res);
  return rc!=0;
}

namespace std {
  template<>
  struct less<pure_expr*> {
    bool operator()(pure_expr* x, pure_expr* y) const
    { return less_than(x, y); }
  };
}

typedef map<pure_expr*,pure_expr*> myorddict;

// Runtime-configurable pretty-printing support.

static ILS<int32_t> omsym = 0;

extern "C" void orddict_symbol(pure_expr *x)
{
  int32_t f;
  if (pure_is_symbol(x, &f) && f>0) omsym() = f;
}

extern "C" pure_expr *orddict_list(myorddict *m);

static const char *orddict_str(myorddict *m)
{
  static char *buf0 = 0; // TLD
  /* Instead of building the string representation directly, it's much easier
     to just construct the real term on the fly and have str() do all the hard
     work for us. */
  int32_t fsym = omsym()?omsym():pure_sym("orddict");
  pure_expr *f = pure_const(fsym), *xs = orddict_list(m),
    *x = pure_applc(pure_new(f), pure_new(xs));
  char *buf = str(x);
  pure_freenew(x);
  /* Note that in the case of an outfix symbol we now have something like LEFT
     [...] RIGHT, but we actually want that to be just LEFT ... RIGHT. We fix
     this here on the fly by removing the list brackets. */
  if (omsym() && pure_sym_other(omsym())) {
    const char *s = pure_sym_pname(omsym()),
      *t = pure_sym_pname(pure_sym_other(omsym()));
    size_t k = strlen(s), l = strlen(t), m = strlen(buf);
    // sanity check
    if (strncmp(buf, s, k) || strncmp(buf+m-l, t, l)) {
      free(buf);
      return 0;
    }
    char *p = buf+k, *q = buf+m-l;
    while (p < buf+m && *p == ' ') p++;
    while (q > buf && *--q == ' ') ;
    if (p >= q || *p != '[' || *q != ']') {
      free(buf);
      return 0;
    }
    memmove(q, q+1, buf+m-q);
    memmove(p, p+1, buf+m-p);
  }
  if (buf0) free(buf0);
  buf0 = buf;
  return buf;
}

#define NPREC_APP 167772155 // this comes from expr.hh

static int orddict_prec(myorddict *m)
{
  if (omsym()) {
    int32_t p = pure_sym_nprec(omsym());
    if (p%10 == OP_PREFIX || p%10 == OP_POSTFIX || pure_sym_other(omsym()))
      return p;
    else
      return NPREC_APP;
  } else
    return NPREC_APP;
}

// Syntactic equality. This hooks into same().

static inline bool samechk(pure_expr *x, pure_expr *y)
{
  if (x == y)
    return true;
  else if (!x || !y)
    return false;
  else
    return same(x, y);
}

static bool orddict_same(myorddict *x, myorddict *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myorddict::iterator it = x->begin(), jt = y->begin(); it != x->end();
       ++it, ++jt) {
#ifdef DEBUG
    assert(jt != y->end());
#endif
    if (!same(it->first, jt->first) || !samechk(it->second, jt->second))
      return false;
  }
  return true;
}

// Custom hashing. This hooks into hash().

static uint32_t orddict_hash(myorddict *x)
{
  int h = omsym()?omsym():pure_sym("orddict");
  for (myorddict::iterator it = x->begin(); it != x->end(); ++it) {
    h = (h<<1) | (h<0 ? 1 : 0);
    h ^= ::hash(it->first);
    if (it->second) {
      h = (h<<1) | (h<0 ? 1 : 0);
      h ^= ::hash(it->second);
    }
  }
  return h;
}

// Value comparisons. Note that, as of Pure 0.49, the stdlib dict checks
// values for semantic equality and falls back to syntactic equality if it
// isn't defined. We mimic this behaviour here.

static bool eqsame(pure_expr *x, pure_expr *y)
{
  // Optimize some common cases of POD (plain old data).
  if (x->tag == y->tag && x->tag < 0) {
    switch (x->tag) {
    case INT:
      return x->data.i == y->data.i;
    case DBL:
      return x->data.d == y->data.d;
    case BIGINT:
      return bigint_cmp(x->data.z, y->data.z) == 0;
    case STR:
      return strcmp(x->data.s, y->data.s) == 0;
    }
  }
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("==");
  assert(fno > 0);
  pure_expr *e = 0, *res = pure_appxl(pure_symbol(fno), &e, 2, x, y);
  int32_t rc;
  if (res && pure_is_int(res, &rc)) {
    pure_freenew(res);
    return rc!=0;
  }
  if (res)
    pure_freenew(res);
  else if (e)
    pure_freenew(e);
  return same(x, y);
}

static inline bool eqchk(pure_expr *x, pure_expr *y)
{
  if (!x || !y)
    return x==y;
  else
    return eqsame(x, y);
}

// Pointer type tag.

extern "C" int orddict_tag(void)
{
  static ILS<int> t = 0;
  if (!t()) {
    t() = pure_pointer_tag("orddict*");
    pure_pointer_add_equal(t(), (pure_equal_fun)orddict_same);
    pure_pointer_add_hash(t(), (pure_hash_fun)orddict_hash);
    pure_pointer_add_printer(t(), (pure_printer_fun)orddict_str,
			     (pure_printer_prec_fun)orddict_prec);
  }
  return t();
}

// Basic interface functions.

extern "C" void orddict_free(myorddict *m)
{
  for (myorddict::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
  }
  delete m;
}

static pure_expr *make_orddict(myorddict *m)
{
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_sym("orddict_free");
  return pure_sentry(pure_symbol(fno),
		     pure_tag(orddict_tag(), pure_pointer(m)));
}

extern "C" void orddict_add(myorddict *m, pure_expr *key);
extern "C" void orddict_add2(myorddict *m, pure_expr *key, pure_expr *val);

extern "C" pure_expr *orddict(pure_expr *xs)
{
  size_t n;
  pure_expr **xv;
  if (!pure_is_listv(xs, &n, &xv) &&
      !pure_is_symbolic_vectorv(xs, &n, &xv) &&
      !(pure_is_tuplev(xs, &n, 0) && n != 1 &&
	pure_is_tuplev(xs, &n, &xv)))
    return 0;
  int32_t fno = pure_getsym("=>"), gno;
  assert(fno > 0);
  myorddict *m = new myorddict;
  for (size_t i = 0; i < n; i++) {
    pure_expr *f, *g, *key, *val;
    if (pure_is_app(xv[i], &f, &val) && pure_is_app(f, &g, &key) &&
	pure_is_symbol(g, &gno) && gno == fno)
      orddict_add2(m, key, val);
    else
      orddict_add(m, xv[i]);
  }
  if (xv) free(xv);
  return make_orddict(m);
}

extern "C" void orddict_add(myorddict *m, pure_expr *key)
{
  myorddict::iterator it = m->find(key);
  if (it != m->end()) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
    m->erase(it);
  }
  pure_new(key);
  (*m)[key] = 0;
}

extern "C" void orddict_add2(myorddict *m, pure_expr *key, pure_expr *val)
{
  myorddict::iterator it = m->find(key);
  if (it != m->end()) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
    m->erase(it);
  }
  pure_new(key);
  (*m)[key] = pure_new(val);
}

extern "C" void orddict_del(myorddict *m, pure_expr *key)
{
  myorddict::iterator it = m->find(key);
  if (it != m->end()) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
    m->erase(it);
  }
}

extern "C" void orddict_del2(myorddict *m, pure_expr *key, pure_expr *val)
{
  myorddict::iterator it = m->find(key);
  if (it != m->end() && it->second && eqsame(it->second, val)) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
    m->erase(it);
  }
}

extern "C" pure_expr *orddict_get(myorddict *m, pure_expr *key)
{
  myorddict::iterator it = m->find(key);
  return (it != m->end())?(it->second?it->second:it->first):0;
}

extern "C" bool orddict_member(myorddict *m, pure_expr *key)
{
  myorddict::iterator it = m->find(key);
  return it != m->end();
}

extern "C" bool orddict_member2(myorddict *m, pure_expr *key, pure_expr *val)
{
  myorddict::iterator it = m->find(key);
  return it != m->end() && it->second && eqsame(it->second, val);
}

extern "C" bool orddict_empty(myorddict *m)
{
  return m->empty();
}

extern "C" int orddict_size(myorddict *m)
{
  return m->size();
}

extern "C" pure_expr *orddict_keys(myorddict *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myorddict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *orddict_vals(myorddict *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myorddict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?it->second:it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *orddict_list(myorddict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myorddict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *orddict_tuple(myorddict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myorddict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_tuplev(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *orddict_vector(myorddict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myorddict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_symbolic_vectorv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" myorddict *orddict_copy(myorddict *m)
{
  myorddict *m2 = new myorddict(*m);
  for (myorddict::iterator it = m2->begin(); it != m2->end(); ++it) {
    pure_new(it->first); if (it->second) pure_new(it->second);
  }
  return m2;
}

extern "C" void orddict_clear(myorddict *m)
{
  for (myorddict::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
  }
  m->clear();
}

// Equality. We do our own version here so that we can compare the associated
// values using eqchk() instead of ==. This might not be the most efficient
// implementation, so suggestions for improvement are welcome.

static bool myequal(pair<pure_expr*,pure_expr*> x,
		    pair<pure_expr*,pure_expr*> y)
{
#ifdef DEBUG
  // It should be enough to just compare the values here, as the keys are
  // supposed to be equal anyway.
  assert(!less_than(x.first, y.first) && !less_than(y.first, x.first));
#endif
  return eqchk(x.second, y.second);
}

#ifdef HAVE_STD_IS_PERMUTATION
#define my_is_permutation is_permutation
#else
// My gcc 4.5 doesn't have this function in its C++ library, use the reference
// implementation instead.

template<class ForwardIterator1, class ForwardIterator2, class BinaryPredicate>
static bool my_is_permutation(ForwardIterator1 first, ForwardIterator1 last,
			      ForwardIterator2 d_first, BinaryPredicate p)
{
  // skip common prefix
  tie(first, d_first) = mismatch(first, last, d_first, p);
  // iterate over the rest, counting how many times each element
  // from [first, last) appears in [d_first, d_last)
  if (first != last) {
    ForwardIterator2 d_last = d_first;
    advance(d_last, distance(first, last));
    for (ForwardIterator1 i = first; i != last; ++i) {
      if (i != find(first, i, *i)) continue; // already counted this *i
      auto m = count(d_first, d_last, *i);
      if (m==0 || count(i, last, *i) != m) {
	return false;
      }
    }
  }
  return true;
}
#endif

extern "C" bool orddict_equal(myorddict *x, myorddict *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myorddict::iterator it = x->begin(); it != x->end(); ) {
    /* This is probably overkill here, as map is guaranteed to have only one
       entry per key, so that the equal ranges should all have size 1. But we
       do it that way for safety, and so that the same implementation can be
       used in the multidict case (see ordmdict_equal below). */
    pair<myorddict::iterator, myorddict::iterator>
      r1 = x->equal_range(it->first),
      r2 = y->equal_range(it->first);
    if (distance(r1.first, r1.second) != distance(r2.first, r2.second))
      return false;
    if (!my_is_permutation(r1.first, r1.second, r2.first, myequal))
      return false;
#ifdef DEBUG
    assert(it == r1.first);
#endif
    it = r1.second;
  }
  return true;
}

// Iterator API.

struct myorddict_iterator {
  // The iterator itself. This should always be the first member so that a
  // pointer to the iterator object can also be passed as a pointer to an
  // ordinary C++ iterator.
  myorddict::iterator it;
  // We also keep a reference to the original Pure expression holding the
  // container, so that it doesn't get garbage-collected while the iterator is
  // still in use.
  pure_expr *x;
  myorddict_iterator(pure_expr *_x) : x(pure_new(_x)) {}
  myorddict_iterator(const myorddict_iterator& y)
    : it(y.it), x(pure_new(y.x)) {}
  ~myorddict_iterator() { pure_free(x); }
};

extern "C" int orddict_iterator_tag(void)
{
  static ILS<int> _t = 0; int &t = _t();
  if (!t) t = pure_pointer_tag("orddict_iterator*");
  return t;
}

static pure_expr *make_orddict_iterator(myorddict_iterator *it)
{
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_sym("orddict_iterator_free");
  return pure_sentry(pure_symbol(fno),
		     pure_tag(orddict_iterator_tag(), pure_pointer(it)));
}

extern "C" pure_expr *orddict_begin(pure_expr *x)
{
  myorddict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(orddict_tag(), x)) {
    myorddict_iterator *it = new myorddict_iterator(x);
    it->it = m->begin();
    return make_orddict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *orddict_end(pure_expr *x)
{
  myorddict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(orddict_tag(), x)) {
    myorddict_iterator *it = new myorddict_iterator(x);
    it->it = m->end();
    return make_orddict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *orddict_find(pure_expr *x, pure_expr *y)
{
  myorddict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(orddict_tag(), x)) {
    myorddict_iterator *it = new myorddict_iterator(x);
    it->it = m->find(y);
    return make_orddict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *orddict_find2(pure_expr *x, pure_expr *key,
				    pure_expr *val)
{
  myorddict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(orddict_tag(), x)) {
    myorddict_iterator *it = new myorddict_iterator(x);
    it->it = m->find(key);
    if (it->it != m->end() && !eqchk(it->it->second, val))
      it->it = m->end();
    return make_orddict_iterator(it);
  } else
    return 0;
}

extern "C" void orddict_iterator_free(myorddict_iterator *it)
{
  delete it;
}

extern "C" pure_expr *orddict_iterator_dict(myorddict_iterator *it)
{
  return it->x;
}

extern "C" pure_expr *orddict_iterator_next(myorddict_iterator *it)
{
  myorddict *m = (myorddict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  myorddict_iterator *jt = new myorddict_iterator(*it);
  jt->it++;
  return make_orddict_iterator(jt);
}

extern "C" pure_expr *orddict_iterator_get(myorddict_iterator *it)
{
  myorddict *m = (myorddict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  if (it->it->second) {
    static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
    if (!fno) fno = pure_getsym("=>");
    assert(fno > 0);
    return pure_appl(pure_symbol(fno), 2, it->it->first, it->it->second);
  } else
    return it->it->first;
}

extern "C" pure_expr *orddict_iterator_put(myorddict_iterator *it,
					   pure_expr *val)
{
  myorddict *m = (myorddict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  if (it->it->second) pure_free(it->it->second);
  it->it->second = pure_new(val);
  return val;
}

extern "C" void orddict_iterator_erase(myorddict_iterator *it)
{
  myorddict *m = (myorddict*)it->x->data.p;
  if (it->it == m->end()) return;
  pure_free(it->it->first);
  if (it->it->second) pure_free(it->it->second);
  m->erase(it->it);
}

extern "C" bool orddict_iterator_endp(myorddict_iterator *it)
{
  myorddict *m = (myorddict*)it->x->data.p;
  return it->it == m->end();
}

extern "C" bool orddict_iterator_equal(myorddict_iterator *it,
				       myorddict_iterator *jt)
{
  myorddict *mi = (myorddict*)it->x->data.p;
  myorddict *mj = (myorddict*)jt->x->data.p;
  return mi == mj && it->it == jt->it;
}

//////////////////////////////////////////////////////////////////////////////

// multidicts: This is basically the same as above, with some minor
// adjustments for the multidict implementation.

//////////////////////////////////////////////////////////////////////////////

typedef multimap<pure_expr*,pure_expr*> myordmdict;

static ILS<int32_t> ommsym = 0;

extern "C" void ordmdict_symbol(pure_expr *x)
{
  int32_t f;
  if (pure_is_symbol(x, &f) && f>0) ommsym() = f;
}

extern "C" pure_expr *ordmdict_list(myordmdict *m);

static const char *ordmdict_str(myordmdict *m)
{
  static char *buf0 = 0; // TLD
  int32_t fsym = ommsym()?ommsym():pure_sym("ordmdict");
  pure_expr *f = pure_const(fsym), *xs = ordmdict_list(m),
    *x = pure_applc(pure_new(f), pure_new(xs));
  char *buf = str(x);
  pure_freenew(x);
  if (ommsym() && pure_sym_other(ommsym())) {
    const char *s = pure_sym_pname(ommsym()),
      *t = pure_sym_pname(pure_sym_other(ommsym()));
    size_t k = strlen(s), l = strlen(t), m = strlen(buf);
    if (strncmp(buf, s, k) || strncmp(buf+m-l, t, l)) {
      free(buf);
      return 0;
    }
    char *p = buf+k, *q = buf+m-l;
    while (p < buf+m && *p == ' ') p++;
    while (q > buf && *--q == ' ') ;
    if (p >= q || *p != '[' || *q != ']') {
      free(buf);
      return 0;
    }
    memmove(q, q+1, buf+m-q);
    memmove(p, p+1, buf+m-p);
  }
  if (buf0) free(buf0);
  buf0 = buf;
  return buf;
}

static int ordmdict_prec(myordmdict *m)
{
  if (ommsym()) {
    int32_t p = pure_sym_nprec(ommsym());
    if (p%10 == OP_PREFIX || p%10 == OP_POSTFIX || pure_sym_other(ommsym()))
      return p;
    else
      return NPREC_APP;
  } else
    return NPREC_APP;
}

static bool ordmdict_same(myordmdict *x, myordmdict *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myordmdict::iterator it = x->begin(), jt = y->begin(); it != x->end();
       ++it, ++jt) {
#ifdef DEBUG
    assert(jt != y->end());
#endif
    if (!same(it->first, jt->first) || !samechk(it->second, jt->second))
      return false;
  }
  return true;
}

static uint32_t ordmdict_hash(myordmdict *x)
{
  int h = ommsym()?ommsym():pure_sym("ordmdict");
  for (myordmdict::iterator it = x->begin(); it != x->end(); ++it) {
    h = (h<<1) | (h<0 ? 1 : 0);
    h ^= ::hash(it->first);
    if (it->second) {
      h = (h<<1) | (h<0 ? 1 : 0);
      h ^= ::hash(it->second);
    }
  }
  return h;
}

extern "C" int ordmdict_tag(void)
{
  static ILS<int> t = 0;
  if (!t()) {
    t() = pure_pointer_tag("ordmdict*");
    pure_pointer_add_equal(t(), (pure_equal_fun)ordmdict_same);
    pure_pointer_add_hash(t(), (pure_hash_fun)ordmdict_hash);
    pure_pointer_add_printer(t(), (pure_printer_fun)ordmdict_str,
			     (pure_printer_prec_fun)ordmdict_prec);
  }
  return t();
}

extern "C" void ordmdict_free(myordmdict *m)
{
  for (myordmdict::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
  }
  delete m;
}

static pure_expr *make_ordmdict(myordmdict *m)
{
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_sym("ordmdict_free");
  return pure_sentry(pure_symbol(fno),
		     pure_tag(ordmdict_tag(), pure_pointer(m)));
}

extern "C" void ordmdict_add(myordmdict *m, pure_expr *key);
extern "C" void ordmdict_add2(myordmdict *m, pure_expr *key, pure_expr *val);

extern "C" pure_expr *ordmdict(pure_expr *xs)
{
  size_t n;
  pure_expr **xv;
  if (!pure_is_listv(xs, &n, &xv) &&
      !pure_is_symbolic_vectorv(xs, &n, &xv) &&
      !(pure_is_tuplev(xs, &n, 0) && n != 1 &&
	pure_is_tuplev(xs, &n, &xv)))
    return 0;
  int32_t fno = pure_getsym("=>"), gno;
  assert(fno > 0);
  myordmdict *m = new myordmdict;
  for (size_t i = 0; i < n; i++) {
    pure_expr *f, *g, *key, *val;
    if (pure_is_app(xv[i], &f, &val) && pure_is_app(f, &g, &key) &&
	pure_is_symbol(g, &gno) && gno == fno)
      ordmdict_add2(m, key, val);
    else
      ordmdict_add(m, xv[i]);
  }
  if (xv) free(xv);
  return make_ordmdict(m);
}

extern "C" void ordmdict_add(myordmdict *m, pure_expr *key)
{
  m->insert(make_pair(pure_new(key), (pure_expr*)0));
}

extern "C" void ordmdict_add2(myordmdict *m, pure_expr *key, pure_expr *val)
{
  m->insert(make_pair(pure_new(key), pure_new(val)));
}

extern "C" void ordmdict_del(myordmdict *m, pure_expr *key)
{
  myordmdict::iterator it = m->find(key);
  if (it != m->end()) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
    m->erase(it);
  }
}

extern "C" void ordmdict_del2(myordmdict *m, pure_expr *key, pure_expr *val)
{
  pair<myordmdict::iterator, myordmdict::iterator> r = m->equal_range(key);
  for (myordmdict::iterator it = r.first; it != r.second; ++it)
    if (it->second && eqsame(it->second, val)) {
      pure_free(it->first);
      if (it->second) pure_free(it->second);
      m->erase(it);
      return;
    }
}

extern "C" pure_expr *ordmdict_get(myordmdict *m, pure_expr *key)
{
  pair<myordmdict::iterator, myordmdict::iterator> r = m->equal_range(key);
  size_t i = 0, n = distance(r.first, r.second);
  pure_expr **xs = new pure_expr*[n];
  for (myordmdict::iterator it = r.first; it != r.second; ++it)
    xs[i++] = it->second?it->second:it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" bool ordmdict_member(myordmdict *m, pure_expr *key)
{
  myordmdict::iterator it = m->find(key);
  return it != m->end();
}

extern "C" bool ordmdict_member2(myordmdict *m, pure_expr *key, pure_expr *val)
{
  pair<myordmdict::iterator, myordmdict::iterator> r = m->equal_range(key);
  for (myordmdict::iterator it = r.first; it != r.second; ++it)
    if (it->second && eqsame(it->second, val))
      return true;
  return false;
}

extern "C" bool ordmdict_empty(myordmdict *m)
{
  return m->empty();
}

extern "C" int ordmdict_size(myordmdict *m)
{
  return m->size();
}

extern "C" pure_expr *ordmdict_keys(myordmdict *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myordmdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *ordmdict_vals(myordmdict *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myordmdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?it->second:it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *ordmdict_list(myordmdict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myordmdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *ordmdict_tuple(myordmdict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myordmdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_tuplev(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *ordmdict_vector(myordmdict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myordmdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_symbolic_vectorv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" myordmdict *ordmdict_copy(myordmdict *m)
{
  myordmdict *m2 = new myordmdict(*m);
  for (myordmdict::iterator it = m2->begin(); it != m2->end(); ++it) {
    pure_new(it->first); if (it->second) pure_new(it->second);
  }
  return m2;
}

extern "C" void ordmdict_clear(myordmdict *m)
{
  for (myordmdict::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
  }
  m->clear();
}

extern "C" bool ordmdict_equal(myordmdict *x, myordmdict *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myordmdict::iterator it = x->begin(); it != x->end(); ) {
    pair<myordmdict::iterator, myordmdict::iterator>
      r1 = x->equal_range(it->first),
      r2 = y->equal_range(it->first);
    if (distance(r1.first, r1.second) != distance(r2.first, r2.second))
      return false;
    // Note that entries for the same key may be listed in an apparently
    // random order, so we have to consider all possible permutations here.
    if (!my_is_permutation(r1.first, r1.second, r2.first, myequal))
      return false;
#ifdef DEBUG
    // We assume that equal_range always yields the first element for a given
    // key in the global traversal order, so that we can just advance to the
    // end of the range afterwards. I couldn't find anything about this in the
    // standard, but the assumption seems reasonable and makes the traversal
    // more efficient.
    assert(it == r1.first);
#endif
    it = r1.second;
  }
  return true;
}

struct myordmdict_iterator {
  // The iterator itself. This should always be the first member so that a
  // pointer to the iterator object can also be passed as a pointer to an
  // ordinary C++ iterator.
  myordmdict::iterator it;
  // We also keep a reference to the original Pure expression holding the
  // container, so that it doesn't get garbage-collected while the iterator is
  // still in use.
  pure_expr *x;
  myordmdict_iterator(pure_expr *_x) : x(pure_new(_x)) {}
  myordmdict_iterator(const myordmdict_iterator& y)
    : it(y.it), x(pure_new(y.x)) {}
  ~myordmdict_iterator() { pure_free(x); }
};

extern "C" int ordmdict_iterator_tag(void)
{
  static ILS<int> _t = 0; int &t = _t();
  if (!t) t = pure_pointer_tag("ordmdict_iterator*");
  return t;
}

static pure_expr *make_ordmdict_iterator(myordmdict_iterator *it)
{
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_sym("ordmdict_iterator_free");
  return pure_sentry(pure_symbol(fno),
		     pure_tag(ordmdict_iterator_tag(), pure_pointer(it)));
}

extern "C" pure_expr *ordmdict_begin(pure_expr *x)
{
  myordmdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(ordmdict_tag(), x)) {
    myordmdict_iterator *it = new myordmdict_iterator(x);
    it->it = m->begin();
    return make_ordmdict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *ordmdict_end(pure_expr *x)
{
  myordmdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(ordmdict_tag(), x)) {
    myordmdict_iterator *it = new myordmdict_iterator(x);
    it->it = m->end();
    return make_ordmdict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *ordmdict_find(pure_expr *x, pure_expr *y)
{
  myordmdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(ordmdict_tag(), x)) {
    myordmdict_iterator *it = new myordmdict_iterator(x);
    it->it = m->find(y);
    return make_ordmdict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *ordmdict_find2(pure_expr *x, pure_expr *key,
				     pure_expr *val)
{
  myordmdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(ordmdict_tag(), x)) {
    myordmdict_iterator *it = new myordmdict_iterator(x);
    pair<myordmdict::iterator, myordmdict::iterator> r = m->equal_range(key);
    it->it = m->end();
    for (myordmdict::iterator jt = r.first; jt != r.second; ++jt)
      if (jt->second && eqsame(jt->second, val)) {
	it->it = jt;
	break;
      }
    return make_ordmdict_iterator(it);
  } else
    return 0;
}

extern "C" void ordmdict_iterator_free(myordmdict_iterator *it)
{
  delete it;
}

extern "C" pure_expr *ordmdict_iterator_dict(myordmdict_iterator *it)
{
  return it->x;
}

extern "C" pure_expr *ordmdict_iterator_next(myordmdict_iterator *it)
{
  myordmdict *m = (myordmdict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  myordmdict_iterator *jt = new myordmdict_iterator(*it);
  jt->it++;
  return make_ordmdict_iterator(jt);
}

extern "C" pure_expr *ordmdict_iterator_get(myordmdict_iterator *it)
{
  myordmdict *m = (myordmdict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  if (it->it->second) {
    static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
    if (!fno) fno = pure_getsym("=>");
    assert(fno > 0);
    return pure_appl(pure_symbol(fno), 2, it->it->first, it->it->second);
  } else
    return it->it->first;
}

extern "C" pure_expr *ordmdict_iterator_put(myordmdict_iterator *it,
					    pure_expr *val)
{
  myordmdict *m = (myordmdict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  if (it->it->second) pure_free(it->it->second);
  it->it->second = pure_new(val);
  return val;
}

extern "C" void ordmdict_iterator_erase(myordmdict_iterator *it)
{
  myordmdict *m = (myordmdict*)it->x->data.p;
  if (it->it == m->end()) return;
  pure_free(it->it->first);
  if (it->it->second) pure_free(it->it->second);
  m->erase(it->it);
}

extern "C" bool ordmdict_iterator_endp(myordmdict_iterator *it)
{
  myordmdict *m = (myordmdict*)it->x->data.p;
  return it->it == m->end();
}

extern "C" bool ordmdict_iterator_equal(myordmdict_iterator *it,
					myordmdict_iterator *jt)
{
  myordmdict *mi = (myordmdict*)it->x->data.p;
  myordmdict *mj = (myordmdict*)jt->x->data.p;
  return mi == mj && it->it == jt->it;
}
