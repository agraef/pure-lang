
// This uses unordered_map, so a recent STL is required for now.
// TODO: hash policy operations, multimap interface.

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <pure/runtime.h>
#include <unordered_map>

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

// Runtime-configurable pretty-printing support.

// FIXME: This should actually be interpreter- and/or thread-local data.
static int32_t hmsym = 0;

extern "C" void hashmap_symbol(pure_expr *x)
{
  int32_t f;
  if (pure_is_symbol(x, &f) && f>0) hmsym = f;
}

extern "C" pure_expr *hashmap_list(myhashmap *m);

static const char *hashmap_str(myhashmap *m)
{
  static char *buf = 0; // TLD
  if (buf) free(buf);
  /* Instead of building the string representation directly, it's much easier
     to just construct the real term on the fly and have str() do all the hard
     work for us. */
  int32_t fsym = hmsym?hmsym:pure_sym("hashmap");
  pure_expr *f = pure_const(fsym), *x = pure_applc(f, hashmap_list(m));
  buf = str(x);
  pure_freenew(x);
  /* Note that in the case of an outfix symbol we now have something like LEFT
     [...] RIGHT, but we actually want that to be just LEFT ... RIGHT. We fix
     this here on the fly by removing the list brackets. */
  if (hmsym && pure_sym_other(hmsym)) {
    const char *s = pure_sym_pname(hmsym),
      *t = pure_sym_pname(pure_sym_other(hmsym));
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
  if (hmsym) {
    int32_t p = pure_sym_nprec(hmsym);
    if (p%10 == OP_PREFIX || p%10 == OP_POSTFIX || pure_sym_other(hmsym))
      return p;
    else
      return NPREC_APP;
  } else
    return NPREC_APP;
}

// Pointer type tag.

extern "C" int hashmap_tag(void)
{
  static int t = 0;
  if (!t) {
    t = pure_pointer_tag("hashmap*");
    pure_pointer_add_printer(t, (pure_printer_fun)hashmap_str,
			     (pure_printer_prec_fun)hashmap_prec);
  }
  return t;
}

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
