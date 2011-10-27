
// This uses unordered_map, so a recent STL is required for now.

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

typedef unordered_map<pure_expr*,pure_expr*> myhashmap;

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
    ptr = (T*)malloc(sizeof(int)); assert(ptr);
    pure_interp_set(key, ptr);
    *ptr = val;
  }
  return *ptr;
}

// Runtime-configurable pretty-printing support.

static ILS<int32_t> hmsym = 0;

extern "C" void hashmap_symbol(pure_expr *x)
{
  int32_t f;
  if (pure_is_symbol(x, &f) && f>0) hmsym() = f;
}

extern "C" pure_expr *hashmap_list(myhashmap *m);

static const char *hashmap_str(myhashmap *m)
{
  static char *buf = 0; // TLD
  if (buf) free(buf);
  /* Instead of building the string representation directly, it's much easier
     to just construct the real term on the fly and have str() do all the hard
     work for us. */
  int32_t fsym = hmsym()?hmsym():pure_sym("hashmap");
  pure_expr *f = pure_const(fsym), *x = pure_applc(f, hashmap_list(m));
  buf = str(x);
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
      free(buf); buf = 0;
      return 0;
    }
    char *p = buf+k, *q = buf+m-l;
    while (p < buf+m && *p == ' ') p++;
    while (q > buf && *--q == ' ') ;
    if (p >= q || *p != '[' || *q != ']') {
      free(buf); buf = 0;
      return 0;
    }
    memmove(q, q+1, buf+m-q);
    memmove(p, p+1, buf+m-p);
  }
  return buf;
}

#define NPREC_APP 167772155 // this comes from expr.hh

static int hashmap_prec(myhashmap *m)
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

static bool hashmap_same(myhashmap *x, myhashmap *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myhashmap::iterator it = x->begin(), jt = y->begin(); it != x->end();
       ++it, ++jt) {
#ifdef DEBUG
    assert(jt != y->end());
#endif
    if (!same(it->first, jt->first) || !samechk(it->second, jt->second))
      return false;
  }
  return true;
}

// Pointer type tag.

extern "C" int hashmap_tag(void)
{
  static ILS<int> t = 0;
  if (!t()) {
    t() = pure_pointer_tag("hashmap*");
    pure_pointer_add_equal(t(), (pure_equal_fun)hashmap_same);
    pure_pointer_add_printer(t(), (pure_printer_fun)hashmap_str,
			     (pure_printer_prec_fun)hashmap_prec);
  }
  return t();
}

// Basic interface functions.

extern "C" void hashmap_free(myhashmap *m)
{
  for (myhashmap::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
  }
  delete m;
}

static pure_expr *make_hashmap(myhashmap *m)
{
  return pure_sentry(pure_symbol(pure_sym("hashmap_free")),
                     pure_tag(hashmap_tag(), pure_pointer(m)));
}

extern "C" void hashmap_add(myhashmap *m, pure_expr *key);
extern "C" void hashmap_add2(myhashmap *m, pure_expr *key, pure_expr *val);

extern "C" pure_expr *hashmap(pure_expr *xs)
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
  myhashmap *m = new myhashmap;
  for (size_t i = 0; i < n; i++) {
    pure_expr *f, *g, *key, *val;
    if (pure_is_app(xv[i], &f, &val) && pure_is_app(f, &g, &key) &&
	pure_is_symbol(g, &gno) && gno == fno)
      hashmap_add2(m, key, val);
    else
      hashmap_add(m, xv[i]);
  }
  if (xv) free(xv);
  return make_hashmap(m);
}

extern "C" void hashmap_add(myhashmap *m, pure_expr *key)
{
  myhashmap::iterator it = m->find(key);
  if (it != m->end()) {
    if (it->second) pure_free(it->second);
  } else
    pure_new(key);
  (*m)[key] = 0;
}

extern "C" void hashmap_add2(myhashmap *m, pure_expr *key, pure_expr *val)
{
  myhashmap::iterator it = m->find(key);
  if (it != m->end()) {
    if (it->second) pure_free(it->second);
  } else
    pure_new(key);
  (*m)[key] = pure_new(val);
}

extern "C" void hashmap_del(myhashmap *m, pure_expr *key)
{
  myhashmap::iterator it = m->find(key);
  if (it != m->end()) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
    m->erase(it);
  }
}

extern "C" void hashmap_del2(myhashmap *m, pure_expr *key, pure_expr *val)
{
  myhashmap::iterator it = m->find(key);
  if (it != m->end() && it->second && same(it->second, val)) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
    m->erase(it);
  }
}

extern "C" pure_expr *hashmap_get(myhashmap *m, pure_expr *key)
{
  myhashmap::iterator it = m->find(key);
  return (it != m->end())?(it->second?it->second:it->first):0;
}

extern "C" bool hashmap_member(myhashmap *m, pure_expr *key)
{
  myhashmap::iterator it = m->find(key);
  return it != m->end();
}

extern "C" bool hashmap_member2(myhashmap *m, pure_expr *key, pure_expr *val)
{
  myhashmap::iterator it = m->find(key);
  return it != m->end() && it->second && same(it->second, val);
}

extern "C" bool hashmap_empty(myhashmap *m)
{
  return m->empty();
}

extern "C" int hashmap_size(myhashmap *m)
{
  return m->size();
}

extern "C" pure_expr *hashmap_keys(myhashmap *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myhashmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *hashmap_vals(myhashmap *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myhashmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?it->second:it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *hashmap_list(myhashmap *m)
{
  size_t i = 0, n = m->size();
  int32_t fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *hashmap_tuple(myhashmap *m)
{
  size_t i = 0, n = m->size();
  int32_t fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_tuplev(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *hashmap_vector(myhashmap *m)
{
  size_t i = 0, n = m->size();
  int32_t fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_symbolic_vectorv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" myhashmap *hashmap_copy(myhashmap *m)
{
  myhashmap *m2 = new myhashmap(*m);
  for (myhashmap::iterator it = m2->begin(); it != m2->end(); ++it) {
    pure_new(it->first); if (it->second) pure_new(it->second);
  }
  return m2;
}

extern "C" void hashmap_clear(myhashmap *m)
{
  for (myhashmap::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
  }
  m->clear();
}

// Equality. We do our own version here so that we can compare the associated
// values using same() instead of ==. This might not be the most efficient
// implementation, so suggestions for improvement are welcome.

static bool myequal(pair<pure_expr*,pure_expr*> x,
		    pair<pure_expr*,pure_expr*> y)
{
#ifdef DEBUG
  // It should be enough to just compare the values here, as the keys are
  // supposed to be equal anyway.
  assert(same(x.first, y.first));
#endif
  return samechk(x.second, y.second);
}

#include <tuple>

#ifndef HAVE_STD_IS_PERMUTATION
// My gcc 4.5 doesn't have this function in its C++ library, use the reference
// implementation instead.

template<class ForwardIterator1, class ForwardIterator2, class BinaryPredicate>
static bool is_permutation(ForwardIterator1 first, ForwardIterator1 last,
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
      if (i != std::find(first, i, *i)) continue; // already counted this *i
      auto m = std::count(d_first, d_last, *i);
      if (m==0 || std::count(i, last, *i) != m) {
	return false;
      }
    }
  }
  return true;
}
#endif

extern "C" bool hashmap_equal(myhashmap *x, myhashmap *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myhashmap::iterator it = x->begin(); it != x->end(); ) {
    /* This is probably overkill here, as unordered_map is guaranteed to have
       only one entry per key, so that the equal ranges should all have size 1.
       But we do it that way for safety, and so that the same implementation
       can be used in the multimap case (see hashmmap_equal below). */
    pair<myhashmap::iterator, myhashmap::iterator>
      r1 = x->equal_range(it->first),
      r2 = y->equal_range(it->first);
    if (distance(r1.first, r1.second) != distance(r2.first, r2.second))
      return false;
    if (!is_permutation(r1.first, r1.second, r2.first, myequal))
      return false;
#ifdef DEBUG
    assert(it == r1.first);
#endif
    it = r1.second;
  }
  return true;
}

extern "C" float hashmap_load_factor(myhashmap *m)
{
  return m->load_factor();
}

extern "C" float hashmap_max_load_factor(myhashmap *m)
{
  return m->max_load_factor();
}

extern "C" void hashmap_set_max_load_factor(myhashmap *m, float x)
{
  m->max_load_factor(x);
}

extern "C" void hashmap_rehash(myhashmap *m, unsigned count)
{
  m->rehash(count);
}

extern "C" void hashmap_reserve(myhashmap *m, unsigned count)
{
  m->reserve(count);
}

extern "C" unsigned hashmap_bucket_count(myhashmap *m)
{
  return m->bucket_count();
}

extern "C" unsigned hashmap_bucket_size(myhashmap *m, unsigned i)
{
  return m->bucket_size(i);
}

//////////////////////////////////////////////////////////////////////////////

// hashed multimaps: This is basically the same as above, with some minor
// adjustments for the multimap implementation.

//////////////////////////////////////////////////////////////////////////////

typedef unordered_multimap<pure_expr*,pure_expr*> myhashmmap;

static ILS<int32_t> hmmsym = 0;

extern "C" void hashmmap_symbol(pure_expr *x)
{
  int32_t f;
  if (pure_is_symbol(x, &f) && f>0) hmmsym() = f;
}

extern "C" pure_expr *hashmmap_list(myhashmmap *m);

static const char *hashmmap_str(myhashmmap *m)
{
  static char *buf = 0; // TLD
  if (buf) free(buf);
  int32_t fsym = hmmsym()?hmmsym():pure_sym("hashmmap");
  pure_expr *f = pure_const(fsym), *x = pure_applc(f, hashmmap_list(m));
  buf = str(x);
  pure_freenew(x);
  if (hmmsym() && pure_sym_other(hmmsym())) {
    const char *s = pure_sym_pname(hmmsym()),
      *t = pure_sym_pname(pure_sym_other(hmmsym()));
    size_t k = strlen(s), l = strlen(t), m = strlen(buf);
    if (strncmp(buf, s, k) || strncmp(buf+m-l, t, l)) {
      free(buf); buf = 0;
      return 0;
    }
    char *p = buf+k, *q = buf+m-l;
    while (p < buf+m && *p == ' ') p++;
    while (q > buf && *--q == ' ') ;
    if (p >= q || *p != '[' || *q != ']') {
      free(buf); buf = 0;
      return 0;
    }
    memmove(q, q+1, buf+m-q);
    memmove(p, p+1, buf+m-p);
  }
  return buf;
}

static int hashmmap_prec(myhashmmap *m)
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

static bool hashmmap_same(myhashmmap *x, myhashmmap *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myhashmmap::iterator it = x->begin(), jt = y->begin(); it != x->end();
       ++it, ++jt) {
#ifdef DEBUG
    assert(jt != y->end());
#endif
    if (!same(it->first, jt->first) || !samechk(it->second, jt->second))
      return false;
  }
  return true;
}

extern "C" int hashmmap_tag(void)
{
  static ILS<int> t = 0;
  if (!t()) {
    t() = pure_pointer_tag("hashmmap*");
    pure_pointer_add_equal(t(), (pure_equal_fun)hashmmap_same);
    pure_pointer_add_printer(t(), (pure_printer_fun)hashmmap_str,
			     (pure_printer_prec_fun)hashmmap_prec);
  }
  return t();
}

extern "C" void hashmmap_free(myhashmmap *m)
{
  for (myhashmmap::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
  }
  delete m;
}

static pure_expr *make_hashmmap(myhashmmap *m)
{
  return pure_sentry(pure_symbol(pure_sym("hashmmap_free")),
                     pure_tag(hashmmap_tag(), pure_pointer(m)));
}

extern "C" void hashmmap_add(myhashmmap *m, pure_expr *key);
extern "C" void hashmmap_add2(myhashmmap *m, pure_expr *key, pure_expr *val);

extern "C" pure_expr *hashmmap(pure_expr *xs)
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
  myhashmmap *m = new myhashmmap;
  for (size_t i = 0; i < n; i++) {
    pure_expr *f, *g, *key, *val;
    if (pure_is_app(xv[i], &f, &val) && pure_is_app(f, &g, &key) &&
	pure_is_symbol(g, &gno) && gno == fno)
      hashmmap_add2(m, key, val);
    else
      hashmmap_add(m, xv[i]);
  }
  if (xv) free(xv);
  return make_hashmmap(m);
}

extern "C" void hashmmap_add(myhashmmap *m, pure_expr *key)
{
  m->insert(make_pair(pure_new(key), (pure_expr*)0));
}

extern "C" void hashmmap_add2(myhashmmap *m, pure_expr *key, pure_expr *val)
{
  m->insert(make_pair(pure_new(key), pure_new(val)));
}

extern "C" void hashmmap_del(myhashmmap *m, pure_expr *key)
{
  myhashmmap::iterator it = m->find(key);
  if (it != m->end()) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
    m->erase(it);
  }
}

extern "C" void hashmmap_del2(myhashmmap *m, pure_expr *key, pure_expr *val)
{
  pair<myhashmmap::iterator, myhashmmap::iterator> r = m->equal_range(key);
  for (myhashmmap::iterator it = r.first; it != r.second; ++it)
    if (it->second && same(it->second, val)) {
      pure_free(it->first);
      if (it->second) pure_free(it->second);
      m->erase(it);
      return;
    }
}

extern "C" pure_expr *hashmmap_get(myhashmmap *m, pure_expr *key)
{
  pair<myhashmmap::iterator, myhashmmap::iterator> r = m->equal_range(key);
  size_t i = 0, n = distance(r.first, r.second);
  pure_expr **xs = new pure_expr*[n];
  for (myhashmmap::iterator it = r.first; it != r.second; ++it)
    xs[i++] = it->second?it->second:it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" bool hashmmap_member(myhashmmap *m, pure_expr *key)
{
  myhashmmap::iterator it = m->find(key);
  return it != m->end();
}

extern "C" bool hashmmap_member2(myhashmmap *m, pure_expr *key, pure_expr *val)
{
  pair<myhashmmap::iterator, myhashmmap::iterator> r = m->equal_range(key);
  for (myhashmmap::iterator it = r.first; it != r.second; ++it)
    if (it->second && same(it->second, val))
      return true;
  return false;
}

extern "C" bool hashmmap_empty(myhashmmap *m)
{
  return m->empty();
}

extern "C" int hashmmap_size(myhashmmap *m)
{
  return m->size();
}

extern "C" pure_expr *hashmmap_keys(myhashmmap *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myhashmmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *hashmmap_vals(myhashmmap *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (myhashmmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?it->second:it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *hashmmap_list(myhashmmap *m)
{
  size_t i = 0, n = m->size();
  int32_t fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashmmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *hashmmap_tuple(myhashmmap *m)
{
  size_t i = 0, n = m->size();
  int32_t fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashmmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_tuplev(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" pure_expr *hashmmap_vector(myhashmmap *m)
{
  size_t i = 0, n = m->size();
  int32_t fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (myhashmmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second?pure_appl(f, 2, it->first, it->second):it->first;
  pure_expr *x = pure_symbolic_vectorv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" myhashmmap *hashmmap_copy(myhashmmap *m)
{
  myhashmmap *m2 = new myhashmmap(*m);
  for (myhashmmap::iterator it = m2->begin(); it != m2->end(); ++it) {
    pure_new(it->first); if (it->second) pure_new(it->second);
  }
  return m2;
}

extern "C" void hashmmap_clear(myhashmmap *m)
{
  for (myhashmmap::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    if (it->second) pure_free(it->second);
  }
  m->clear();
}

extern "C" bool hashmmap_equal(myhashmmap *x, myhashmmap *y)
{
  if (x == y) return true;
  if (x->size() != y->size()) return false;
  for (myhashmmap::iterator it = x->begin(); it != x->end(); ) {
    pair<myhashmmap::iterator, myhashmmap::iterator>
      r1 = x->equal_range(it->first),
      r2 = y->equal_range(it->first);
    if (distance(r1.first, r1.second) != distance(r2.first, r2.second))
      return false;
    if (!is_permutation(r1.first, r1.second, r2.first, myequal))
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

extern "C" float hashmmap_load_factor(myhashmmap *m)
{
  return m->load_factor();
}

extern "C" float hashmmap_max_load_factor(myhashmmap *m)
{
  return m->max_load_factor();
}

extern "C" void hashmmap_set_max_load_factor(myhashmmap *m, float x)
{
  m->max_load_factor(x);
}

extern "C" void hashmmap_rehash(myhashmmap *m, unsigned count)
{
  m->rehash(count);
}

extern "C" void hashmmap_reserve(myhashmmap *m, unsigned count)
{
  m->reserve(count);
}

extern "C" unsigned hashmmap_bucket_count(myhashmmap *m)
{
  return m->bucket_count();
}

extern "C" unsigned hashmmap_bucket_size(myhashmmap *m, unsigned i)
{
  return m->bucket_size(i);
}
