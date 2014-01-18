
// This uses unordered_map, so a recent C++ library is required for now.

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <pure/runtime.h>
#include <unordered_map>
#include <algorithm>

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

// Hashing and comparing Pure expressions. The required functionality is in
// the Pure runtime (hash() and same() functions, see pure/runtime.h).

namespace std {
  template<>
  struct hash<pure_expr*>
  {
    size_t operator()(pure_expr* x) const
    { return ::hash(x); };
  };
  template<>
  struct equal_to<pure_expr*> {
    bool operator()(pure_expr* x, pure_expr* y) const
    { return same(x, y); }
  };
}

typedef unordered_map<pure_expr*,pure_expr*> myhashdict;

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

// Runtime-configurable pretty-printing support.

static ILS<int32_t> hmsym = 0;

extern "C" void hashdict_symbol(pure_expr *x)
{
  int32_t f;
  if (pure_is_symbol(x, &f) && f>0) hmsym() = f;
}

extern "C" pure_expr *hashdict_list(myhashdict *m);

static const char *hashdict_str(myhashdict *m)
{
  static char *buf0 = 0; // TLD
  /* Instead of building the string representation directly, it's much easier
     to just construct the real term on the fly and have str() do all the hard
     work for us. */
  int32_t fsym = hmsym()?hmsym():pure_sym("hashdict");
  pure_expr *f = pure_const(fsym), *xs = hashdict_list(m),
    *x = pure_applc(pure_new(f), pure_new(xs));
  char *buf = str(x);
  pure_freenew(x);
  /* Note that in the case of an outfix symbol we now have something like LEFT
     [...] RIGHT, but we actually want that to be just LEFT ... RIGHT. We fix
     this here on the fly by removing the list brackets. */
  if (hmsym() && pure_sym_other(hmsym())) {
    const char *s = pure_sym_pname(hmsym()),
      *t = pure_sym_pname(pure_sym_other(hmsym()));
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
  /* We use a static variable to keep track of memory allocations and get rid
     of previously allocated buffers in order to prevent memory leaks. This is
     ugly, but it works. Note that updating the static buffer must be the last
     thing here in order to make the routine reentrant, as the call to the
     str() routine may invoke hashdict_str() recursively if components of the
     dictionary are themselves dictionaries. */
  if (buf0) free(buf0); buf0 = buf;
  return buf;
}

#define NPREC_APP 167772155 // this comes from expr.hh

static int hashdict_prec(myhashdict *m)
{
  if (hmsym()) {
    int32_t p = pure_sym_nprec(hmsym());
    if (p%10 == OP_PREFIX || p%10 == OP_POSTFIX || pure_sym_other(hmsym()))
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

static bool hashdict_same(myhashdict *x, myhashdict *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myhashdict::iterator it = x->begin(), jt = y->begin(); it != x->end();
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

static uint32_t hashdict_hash(myhashdict *x)
{
  int h = hmsym()?hmsym():pure_sym("hashdict");
  for (myhashdict::iterator it = x->begin(); it != x->end(); ++it) {
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

enum {
  INT		= -3,	// 32 bit signed integer
  BIGINT	= -4,	// bigint (mpz_t)
  DBL		= -5,	// double precision floating point number
  STR		= -6,	// utf-8 string (char*)
};

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
  pure_expr *res = pure_appl(pure_symbol(fno), 2, x, y);
  int32_t rc;
  if (pure_is_int(res, &rc)) {
    pure_freenew(res);
    return rc!=0;
  }
  pure_freenew(res);
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

extern "C" int hashdict_tag(void)
{
  static ILS<int> t = 0;
  if (!t()) {
    t() = pure_pointer_tag("hashdict*");
    pure_pointer_add_equal(t(), (pure_equal_fun)hashdict_same);
    pure_pointer_add_hash(t(), (pure_hash_fun)hashdict_hash);
    pure_pointer_add_printer(t(), (pure_printer_fun)hashdict_str,
			     (pure_printer_prec_fun)hashdict_prec);
  }
  return t();
}

// Basic interface functions.

extern "C" void hashdict_free(myhashdict *m)
{
  for (myhashdict::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
  }
  delete m;
}

static pure_expr *make_hashdict(myhashdict *m)
{
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_sym("hashdict_free");
  return pure_sentry(pure_symbol(fno),
		     pure_tag(hashdict_tag(), pure_pointer(m)));
}

extern "C" void hashdict_add(myhashdict *m, pure_expr *key);
extern "C" void hashdict_add2(myhashdict *m, pure_expr *key, pure_expr *val);

extern "C" pure_expr *hashdict(pure_expr *xs)
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
  myhashdict *m = new myhashdict;
  for (size_t i = 0; i < n; i++) {
    pure_expr *f, *g, *key, *val;
    if (pure_is_app(xv[i], &f, &val) && pure_is_app(f, &g, &key) &&
	pure_is_symbol(g, &gno) && gno == fno)
      hashdict_add2(m, key, val);
    else
      hashdict_add(m, xv[i]);
  }
  if (xv) free(xv);
  return make_hashdict(m);
}

extern "C" void hashdict_add(myhashdict *m, pure_expr *key)
{
  myhashdict::iterator it = m->find(key);
  if (it != m->end()) {
    if (it->second) pure_free(it->second);
    it->second = 0;
  } else {
    pure_new(key);
    (*m)[key] = 0;
  }
}

extern "C" void hashdict_add2(myhashdict *m, pure_expr *key, pure_expr *val)
{
  myhashdict::iterator it = m->find(key);
  if (it != m->end()) {
    if (it->second) pure_free(it->second);
    it->second = pure_new(val);
  } else {
    pure_new(key);
    (*m)[key] = pure_new(val);
  }
}

extern "C" void hashdict_del(myhashdict *m, pure_expr *key)
{
  myhashdict::iterator it = m->find(key);
  if (it != m->end()) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
    m->erase(it);
  }
}

extern "C" void hashdict_del2(myhashdict *m, pure_expr *key, pure_expr *val)
{
  myhashdict::iterator it = m->find(key);
  if (it != m->end() && it->second && eqsame(it->second, val)) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
    m->erase(it);
  }
}

extern "C" pure_expr *hashdict_get(myhashdict *m, pure_expr *key)
{
  myhashdict::iterator it = m->find(key);
  return (it != m->end())?(it->second?it->second:it->first):0;
}

extern "C" bool hashdict_member(myhashdict *m, pure_expr *key)
{
  myhashdict::iterator it = m->find(key);
  return it != m->end();
}

extern "C" bool hashdict_member2(myhashdict *m, pure_expr *key, pure_expr *val)
{
  myhashdict::iterator it = m->find(key);
  return it != m->end() && it->second && eqsame(it->second, val);
}

extern "C" bool hashdict_empty(myhashdict *m)
{
  return m->empty();
}

extern "C" int hashdict_size(myhashdict *m)
{
  return m->size();
}

extern "C" pure_expr *hashdict_keys(myhashdict *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myhashdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *hashdict_vals(myhashdict *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myhashdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?it->second:it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *hashdict_list(myhashdict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *hashdict_tuple(myhashdict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_tuplev(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *hashdict_vector(myhashdict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_symbolic_vectorv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" myhashdict *hashdict_copy(myhashdict *m)
{
  myhashdict *m2 = new myhashdict(*m);
  for (myhashdict::iterator it = m2->begin(); it != m2->end(); ++it) {
    pure_new(it->first); if (it->second) pure_new(it->second);
  }
  return m2;
}

extern "C" void hashdict_clear(myhashdict *m)
{
  for (myhashdict::iterator it = m->begin(); it != m->end(); ++it) {
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
  assert(same(x.first, y.first));
#endif
  return eqchk(x.second, y.second);
}

#include <tuple>

#ifdef HAVE_STD_IS_PERMUTATION
#define my_is_permutation is_permutation
#else
// My gcc 4.5 doesn't have this function in its C++ library, use the reference
// implementation instead.

#include <functional>

template<class ForwardIterator1, class ForwardIterator2, class BinaryPredicate>
static bool my_is_permutation(ForwardIterator1 first, ForwardIterator1 last,
			      ForwardIterator2 d_first, BinaryPredicate p)
{
  // skip common prefix
  std::tie(first, d_first) = std::mismatch(first, last, d_first, p);
  // iterate over the rest, counting how many times each element
  // from [first, last) appears in [d_first, d_last)
  if (first != last) {
    ForwardIterator2 d_last = d_first;
    std::advance(d_last, std::distance(first, last));
    for (ForwardIterator1 i = first; i != last; ++i) {
      using std::placeholders::_1;
      if (i != std::find_if(first, i, std::bind(p, _1, *i)))
	continue; // already counted this *i
      auto m = std::count_if(d_first, d_last, std::bind(p, _1, *i));
      if (m==0 || std::count_if(i, last, std::bind(p, _1, *i)) != m) {
	return false;
      }
    }
  }
  return true;
}
#endif

extern "C" bool hashdict_equal(myhashdict *x, myhashdict *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myhashdict::iterator it = x->begin(); it != x->end(); ) {
    /* This is probably overkill here, as unordered_map is guaranteed to have
       only one entry per key, so that the equal ranges should all have size 1.
       But we do it that way for safety, and so that the same implementation
       can be used in the multidict case (see hashmdict_equal below). */
    pair<myhashdict::iterator, myhashdict::iterator>
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

struct myhashdict_iterator {
  // The iterator itself. This should always be the first member so that a
  // pointer to the iterator object can also be passed as a pointer to an
  // ordinary C++ iterator.
  myhashdict::iterator it;
  // We also keep a reference to the original Pure expression holding the
  // container, so that it doesn't get garbage-collected while the iterator is
  // still in use.
  pure_expr *x;
  myhashdict_iterator(pure_expr *_x) : x(pure_new(_x)) {}
  myhashdict_iterator(const myhashdict_iterator& y)
    : it(y.it), x(pure_new(y.x)) {}
  ~myhashdict_iterator() { pure_free(x); }
};

extern "C" int hashdict_iterator_tag(void)
{
  static ILS<int> _t = 0; int &t = _t();
  if (!t) t = pure_pointer_tag("hashdict_iterator*");
  return t;
}

static pure_expr *make_hashdict_iterator(myhashdict_iterator *it)
{
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_sym("hashdict_iterator_free");
  return pure_sentry(pure_symbol(fno),
		     pure_tag(hashdict_iterator_tag(), pure_pointer(it)));
}

extern "C" pure_expr *hashdict_begin(pure_expr *x)
{
  myhashdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(hashdict_tag(), x)) {
    myhashdict_iterator *it = new myhashdict_iterator(x);
    it->it = m->begin();
    return make_hashdict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *hashdict_end(pure_expr *x)
{
  myhashdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(hashdict_tag(), x)) {
    myhashdict_iterator *it = new myhashdict_iterator(x);
    it->it = m->end();
    return make_hashdict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *hashdict_find(pure_expr *x, pure_expr *y)
{
  myhashdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(hashdict_tag(), x)) {
    myhashdict_iterator *it = new myhashdict_iterator(x);
    it->it = m->find(y);
    return make_hashdict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *hashdict_find2(pure_expr *x, pure_expr *key,
				     pure_expr *val)
{
  myhashdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(hashdict_tag(), x)) {
    myhashdict_iterator *it = new myhashdict_iterator(x);
    it->it = m->find(key);
    if (it->it != m->end() && !eqchk(it->it->second, val))
      it->it = m->end();
    return make_hashdict_iterator(it);
  } else
    return 0;
}

extern "C" void hashdict_iterator_free(myhashdict_iterator *it)
{
  delete it;
}

extern "C" pure_expr *hashdict_iterator_dict(myhashdict_iterator *it)
{
  return it->x;
}

extern "C" pure_expr *hashdict_iterator_next(myhashdict_iterator *it)
{
  myhashdict *m = (myhashdict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  myhashdict_iterator *jt = new myhashdict_iterator(*it);
  jt->it++;
  return make_hashdict_iterator(jt);
}

extern "C" pure_expr *hashdict_iterator_get(myhashdict_iterator *it)
{
  myhashdict *m = (myhashdict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  if (it->it->second) {
    static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
    if (!fno) fno = pure_getsym("=>");
    assert(fno > 0);
    return pure_appl(pure_symbol(fno), 2, it->it->first, it->it->second);
  } else
    return it->it->first;
}

extern "C" pure_expr *hashdict_iterator_put(myhashdict_iterator *it,
					    pure_expr *val)
{
  myhashdict *m = (myhashdict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  if (it->it->second) pure_free(it->it->second);
  it->it->second = pure_new(val);
  return val;
}

extern "C" void hashdict_iterator_erase(myhashdict_iterator *it)
{
  myhashdict *m = (myhashdict*)it->x->data.p;
  if (it->it == m->end()) return;
  pure_free(it->it->first);
  if (it->it->second) pure_free(it->it->second);
  m->erase(it->it);
}

extern "C" bool hashdict_iterator_endp(myhashdict_iterator *it)
{
  myhashdict *m = (myhashdict*)it->x->data.p;
  return it->it == m->end();
}

extern "C" bool hashdict_iterator_equal(myhashdict_iterator *it,
					myhashdict_iterator *jt)
{
  myhashdict *mi = (myhashdict*)it->x->data.p;
  myhashdict *mj = (myhashdict*)jt->x->data.p;
  return mi == mj && it->it == jt->it;
}

// Bucket and hash policy API.

extern "C" float hashdict_load_factor(myhashdict *m)
{
  return m->load_factor();
}

extern "C" float hashdict_max_load_factor(myhashdict *m)
{
  return m->max_load_factor();
}

extern "C" void hashdict_set_max_load_factor(myhashdict *m, float x)
{
  m->max_load_factor(x);
}

extern "C" void hashdict_rehash(myhashdict *m, unsigned count)
{
  m->rehash(count);
}

extern "C" void hashdict_reserve(myhashdict *m, unsigned count)
{
  // This requires g++ >= 4.5. (Add other compilers as needed.)
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 5)
  m->reserve(count);
#endif
}

extern "C" unsigned hashdict_bucket_count(myhashdict *m)
{
  return m->bucket_count();
}

extern "C" unsigned hashdict_bucket_size(myhashdict *m, unsigned i)
{
  return m->bucket_size(i);
}

//////////////////////////////////////////////////////////////////////////////

// hashed multidicts: This is basically the same as above, with some minor
// adjustments for the multidict implementation.

//////////////////////////////////////////////////////////////////////////////

typedef unordered_multimap<pure_expr*,pure_expr*> myhashmdict;

static ILS<int32_t> hmmsym = 0;

extern "C" void hashmdict_symbol(pure_expr *x)
{
  int32_t f;
  if (pure_is_symbol(x, &f) && f>0) hmmsym() = f;
}

extern "C" pure_expr *hashmdict_list(myhashmdict *m);

static const char *hashmdict_str(myhashmdict *m)
{
  static char *buf0 = 0; // TLD
  int32_t fsym = hmmsym()?hmmsym():pure_sym("hashmdict");
  pure_expr *f = pure_const(fsym), *xs = hashmdict_list(m),
    *x = pure_applc(pure_new(f), pure_new(xs));
  char *buf = str(x);
  pure_freenew(x);
  if (hmmsym() && pure_sym_other(hmmsym())) {
    const char *s = pure_sym_pname(hmmsym()),
      *t = pure_sym_pname(pure_sym_other(hmmsym()));
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
  if (buf0) free(buf0); buf0 = buf;
  return buf;
}

static int hashmdict_prec(myhashmdict *m)
{
  if (hmmsym()) {
    int32_t p = pure_sym_nprec(hmmsym());
    if (p%10 == OP_PREFIX || p%10 == OP_POSTFIX || pure_sym_other(hmmsym()))
      return p;
    else
      return NPREC_APP;
  } else
    return NPREC_APP;
}

static bool hashmdict_same(myhashmdict *x, myhashmdict *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myhashmdict::iterator it = x->begin(), jt = y->begin(); it != x->end();
       ++it, ++jt) {
#ifdef DEBUG
    assert(jt != y->end());
#endif
    if (!same(it->first, jt->first) || !samechk(it->second, jt->second))
      return false;
  }
  return true;
}

static uint32_t hashmdict_hash(myhashmdict *x)
{
  int h = hmmsym()?hmmsym():pure_sym("hashmdict");
  for (myhashmdict::iterator it = x->begin(); it != x->end(); ++it) {
    h = (h<<1) | (h<0 ? 1 : 0);
    h ^= ::hash(it->first);
    if (it->second) {
      h = (h<<1) | (h<0 ? 1 : 0);
      h ^= ::hash(it->second);
    }
  }
  return h;
}

extern "C" int hashmdict_tag(void)
{
  static ILS<int> t = 0;
  if (!t()) {
    t() = pure_pointer_tag("hashmdict*");
    pure_pointer_add_equal(t(), (pure_equal_fun)hashmdict_same);
    pure_pointer_add_hash(t(), (pure_hash_fun)hashmdict_hash);
    pure_pointer_add_printer(t(), (pure_printer_fun)hashmdict_str,
			     (pure_printer_prec_fun)hashmdict_prec);
  }
  return t();
}

extern "C" void hashmdict_free(myhashmdict *m)
{
  for (myhashmdict::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
  }
  delete m;
}

static pure_expr *make_hashmdict(myhashmdict *m)
{
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_sym("hashmdict_free");
  return pure_sentry(pure_symbol(fno),
		     pure_tag(hashmdict_tag(), pure_pointer(m)));
}

extern "C" void hashmdict_add(myhashmdict *m, pure_expr *key);
extern "C" void hashmdict_add2(myhashmdict *m, pure_expr *key, pure_expr *val);

extern "C" pure_expr *hashmdict(pure_expr *xs)
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
  myhashmdict *m = new myhashmdict;
  for (size_t i = 0; i < n; i++) {
    pure_expr *f, *g, *key, *val;
    if (pure_is_app(xv[i], &f, &val) && pure_is_app(f, &g, &key) &&
	pure_is_symbol(g, &gno) && gno == fno)
      hashmdict_add2(m, key, val);
    else
      hashmdict_add(m, xv[i]);
  }
  if (xv) free(xv);
  return make_hashmdict(m);
}

extern "C" void hashmdict_add(myhashmdict *m, pure_expr *key)
{
  m->insert(make_pair(pure_new(key), (pure_expr*)0));
}

extern "C" void hashmdict_add2(myhashmdict *m, pure_expr *key, pure_expr *val)
{
  m->insert(make_pair(pure_new(key), pure_new(val)));
}

extern "C" void hashmdict_del(myhashmdict *m, pure_expr *key)
{
  myhashmdict::iterator it = m->find(key);
  if (it != m->end()) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
    m->erase(it);
  }
}

extern "C" void hashmdict_del2(myhashmdict *m, pure_expr *key, pure_expr *val)
{
  pair<myhashmdict::iterator, myhashmdict::iterator> r = m->equal_range(key);
  for (myhashmdict::iterator it = r.first; it != r.second; ++it)
    if (it->second && eqsame(it->second, val)) {
      pure_free(it->first);
      if (it->second) pure_free(it->second);
      m->erase(it);
      return;
    }
}

extern "C" pure_expr *hashmdict_get(myhashmdict *m, pure_expr *key)
{
  pair<myhashmdict::iterator, myhashmdict::iterator> r = m->equal_range(key);
  size_t i = 0, n = distance(r.first, r.second);
  pure_expr **xs = new pure_expr*[n];
  for (myhashmdict::iterator it = r.first; it != r.second; ++it)
    xs[i++] = it->second?it->second:it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" bool hashmdict_member(myhashmdict *m, pure_expr *key)
{
  myhashmdict::iterator it = m->find(key);
  return it != m->end();
}

extern "C" bool hashmdict_member2(myhashmdict *m, pure_expr *key, pure_expr *val)
{
  pair<myhashmdict::iterator, myhashmdict::iterator> r = m->equal_range(key);
  for (myhashmdict::iterator it = r.first; it != r.second; ++it)
    if (it->second && eqsame(it->second, val))
      return true;
  return false;
}

extern "C" bool hashmdict_empty(myhashmdict *m)
{
  return m->empty();
}

extern "C" int hashmdict_size(myhashmdict *m)
{
  return m->size();
}

extern "C" pure_expr *hashmdict_keys(myhashmdict *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myhashmdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *hashmdict_vals(myhashmdict *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myhashmdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?it->second:it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *hashmdict_list(myhashmdict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashmdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *hashmdict_tuple(myhashmdict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashmdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_tuplev(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *hashmdict_vector(myhashmdict *m)
{
  size_t i = 0, n = m->size();
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashmdict::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_symbolic_vectorv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" myhashmdict *hashmdict_copy(myhashmdict *m)
{
  myhashmdict *m2 = new myhashmdict(*m);
  for (myhashmdict::iterator it = m2->begin(); it != m2->end(); ++it) {
    pure_new(it->first); if (it->second) pure_new(it->second);
  }
  return m2;
}

extern "C" void hashmdict_clear(myhashmdict *m)
{
  for (myhashmdict::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
  }
  m->clear();
}

extern "C" bool hashmdict_equal(myhashmdict *x, myhashmdict *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myhashmdict::iterator it = x->begin(); it != x->end(); ) {
    pair<myhashmdict::iterator, myhashmdict::iterator>
      r1 = x->equal_range(it->first),
      r2 = y->equal_range(it->first);
    if (distance(r1.first, r1.second) != distance(r2.first, r2.second))
      return false;
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

struct myhashmdict_iterator {
  myhashmdict::iterator it;
  pure_expr *x;
  myhashmdict_iterator(pure_expr *_x) : x(pure_new(_x)) {}
  myhashmdict_iterator(const myhashmdict_iterator& y)
    : it(y.it), x(pure_new(y.x)) {}
  ~myhashmdict_iterator() { pure_free(x); }
};

extern "C" int hashmdict_iterator_tag(void)
{
  static ILS<int> _t = 0; int &t = _t();
  if (!t) t = pure_pointer_tag("hashmdict_iterator*");
  return t;
}

static pure_expr *make_hashmdict_iterator(myhashmdict_iterator *it)
{
  static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
  if (!fno) fno = pure_sym("hashmdict_iterator_free");
  return pure_sentry(pure_symbol(fno),
		     pure_tag(hashmdict_iterator_tag(), pure_pointer(it)));
}

extern "C" pure_expr *hashmdict_begin(pure_expr *x)
{
  myhashmdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(hashmdict_tag(), x)) {
    myhashmdict_iterator *it = new myhashmdict_iterator(x);
    it->it = m->begin();
    return make_hashmdict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *hashmdict_end(pure_expr *x)
{
  myhashmdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(hashmdict_tag(), x)) {
    myhashmdict_iterator *it = new myhashmdict_iterator(x);
    it->it = m->end();
    return make_hashmdict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *hashmdict_find(pure_expr *x, pure_expr *y)
{
  myhashmdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(hashmdict_tag(), x)) {
    myhashmdict_iterator *it = new myhashmdict_iterator(x);
    it->it = m->find(y);
    return make_hashmdict_iterator(it);
  } else
    return 0;
}

extern "C" pure_expr *hashmdict_find2(pure_expr *x, pure_expr *key,
				      pure_expr *val)
{
  myhashmdict *m;
  if (pure_is_pointer(x, (void**)&m) &&
      pure_check_tag(hashmdict_tag(), x)) {
    myhashmdict_iterator *it = new myhashmdict_iterator(x);
    pair<myhashmdict::iterator, myhashmdict::iterator> r = m->equal_range(key);
    it->it = m->end();
    for (myhashmdict::iterator jt = r.first; jt != r.second; ++jt)
      if (jt->second && eqsame(jt->second, val)) {
	it->it = jt;
	break;
      }
    return make_hashmdict_iterator(it);
  } else
    return 0;
}

extern "C" void hashmdict_iterator_free(myhashmdict_iterator *it)
{
  delete it;
}

extern "C" pure_expr *hashmdict_iterator_dict(myhashmdict_iterator *it)
{
  return it->x;
}

extern "C" pure_expr *hashmdict_iterator_next(myhashmdict_iterator *it)
{
  myhashmdict *m = (myhashmdict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  myhashmdict_iterator *jt = new myhashmdict_iterator(*it);
  jt->it++;
  return make_hashmdict_iterator(jt);
}

extern "C" pure_expr *hashmdict_iterator_get(myhashmdict_iterator *it)
{
  myhashmdict *m = (myhashmdict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  if (it->it->second) {
    static ILS<int32_t> _fno = 0; int32_t &fno = _fno();
    if (!fno) fno = pure_getsym("=>");
    assert(fno > 0);
    return pure_appl(pure_symbol(fno), 2, it->it->first, it->it->second);
  } else
    return it->it->first;
}

extern "C" pure_expr *hashmdict_iterator_put(myhashmdict_iterator *it,
					     pure_expr *val)
{
  myhashmdict *m = (myhashmdict*)it->x->data.p;
  if (it->it == m->end()) return 0;
  if (it->it->second) pure_free(it->it->second);
  it->it->second = pure_new(val);
  return val;
}

extern "C" void hashmdict_iterator_erase(myhashmdict_iterator *it)
{
  myhashmdict *m = (myhashmdict*)it->x->data.p;
  if (it->it == m->end()) return;
  pure_free(it->it->first);
  if (it->it->second) pure_free(it->it->second);
  m->erase(it->it);
}

extern "C" bool hashmdict_iterator_endp(myhashmdict_iterator *it)
{
  myhashmdict *m = (myhashmdict*)it->x->data.p;
  return it->it == m->end();
}

extern "C" bool hashmdict_iterator_equal(myhashmdict_iterator *it,
					 myhashmdict_iterator *jt)
{
  myhashmdict *mi = (myhashmdict*)it->x->data.p;
  myhashmdict *mj = (myhashmdict*)jt->x->data.p;
  return mi == mj && it->it == jt->it;
}

extern "C" float hashmdict_load_factor(myhashmdict *m)
{
  return m->load_factor();
}

extern "C" float hashmdict_max_load_factor(myhashmdict *m)
{
  return m->max_load_factor();
}

extern "C" void hashmdict_set_max_load_factor(myhashmdict *m, float x)
{
  m->max_load_factor(x);
}

extern "C" void hashmdict_rehash(myhashmdict *m, unsigned count)
{
  m->rehash(count);
}

extern "C" void hashmdict_reserve(myhashmdict *m, unsigned count)
{
  // This requires g++ >= 4.5. (Add other compilers as needed.)
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 5)
  m->reserve(count);
#endif
}

extern "C" unsigned hashmdict_bucket_count(myhashmdict *m)
{
  return m->bucket_count();
}

extern "C" unsigned hashmdict_bucket_size(myhashmdict *m, unsigned i)
{
  return m->bucket_size(i);
}
