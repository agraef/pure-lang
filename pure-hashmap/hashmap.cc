
// This uses unordered_map, so a recent STL is required for now.
// TODO: hash policy operations, multimap interface, sets and bags.

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

typedef unordered_map<pure_expr*,pure_expr*> exprmap;

// Pretty-printing support.

#include <sstream>

static const char *hashmap_str(exprmap *m)
{
  ostringstream os;
  static char *buf = 0;
  size_t i = 0, n = m->size();
  int32_t fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr *f = pure_new(pure_symbol(fno));
  bool init = true;
  if (buf) free(buf);
  os << "{$";
  for (exprmap::iterator it = m->begin(); it != m->end(); ++it) {
    pure_expr *x = pure_appl(f, 2, it->first, it->second);
    char *s = str(x);
    pure_freenew(x);
    if (init)
      init = false;
    else
      os << ",";
    os << s;
    free(s);
  }
  os << "$}";
  pure_free(f);
  buf = strdup(os.str().c_str());
  return buf;
}

// Pointer type tag.

extern "C" int hashmap_tag(void)
{
  static int t = 0;
  if (!t) {
    t = pure_pointer_tag("hashmap*");
    pure_pointer_add_printer(t, (pure_printer_fun)hashmap_str, 0);
  }
  return t;
}

// Basic interface functions.

extern "C" void hashmap_free(exprmap *m)
{
  for (exprmap::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    pure_free(it->second);
  }
  delete m;
}

static pure_expr *make_hashmap(exprmap *m)
{
  return pure_sentry(pure_symbol(pure_sym("hashmap_free")),
                     pure_tag(hashmap_tag(), pure_pointer(m)));
}

extern "C" void hashmap_add(exprmap *m, pure_expr *key, pure_expr *val);

extern "C" pure_expr *hashmap(pure_expr *xs)
{
  exprmap *m = new exprmap;
  size_t n;
  pure_expr **xv;
  int32_t fno = pure_getsym("=>"), gno;
  if (!pure_is_listv(xs, &n, &xv)) goto fail;
  for (size_t i = 0; i < n; i++) {
    pure_expr *f, *g, *key, *val;
    if (pure_is_app(xv[i], &f, &val) && pure_is_app(f, &g, &key) &&
	pure_is_symbol(g, &gno) && gno == fno)
      hashmap_add(m, key, val);
    else {
      free(xv);
      goto fail;
    }
  }
  if (xv) free(xv);
  return make_hashmap(m);
 fail:
  hashmap_free(m);
  return 0;
}

extern "C" void hashmap_add(exprmap *m, pure_expr *key, pure_expr *val)
{
  exprmap::iterator it = m->find(key);
  if (it != m->end())
    pure_free(it->second);
  else
    pure_new(key);
  (*m)[key] = pure_new(val);
}

extern "C" void hashmap_del(exprmap *m, pure_expr *key)
{
  exprmap::iterator it = m->find(key);
  if (it != m->end()) {
    pure_free(it->first);
    pure_free(it->second);
    m->erase(it);
  }
}

extern "C" pure_expr *hashmap_get(exprmap *m, pure_expr *key)
{
  exprmap::iterator it = m->find(key);
  return (it != m->end())?it->second:0;
}

extern "C" bool hashmap_member(exprmap *m, pure_expr *key)
{
  exprmap::iterator it = m->find(key);
  return it != m->end();
}

extern "C" bool hashmap_empty(exprmap *m)
{
  return m->empty();
}

extern "C" int hashmap_size(exprmap *m)
{
  return m->size();
}

extern "C" pure_expr *hashmap_keys(exprmap *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (exprmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->first;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *hashmap_vals(exprmap *m)
{
  size_t i = 0, n = m->size();
  pure_expr **xs = new pure_expr*[n];
  for (exprmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = it->second;
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

extern "C" pure_expr *hashmap_list(exprmap *m)
{
  size_t i = 0, n = m->size();
  int32_t fno = pure_getsym("=>");
  assert(fno > 0);
  pure_expr **xs = new pure_expr*[n], *f = pure_new(pure_symbol(fno));
  for (exprmap::iterator it = m->begin(); it != m->end(); ++it)
    xs[i++] = pure_appl(f, 2, it->first, it->second);
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  pure_free(f);
  return x;
}

extern "C" exprmap *hashmap_copy(exprmap *m)
{
  exprmap *m2 = new exprmap;
  for (exprmap::iterator it = m->begin(); it != m->end(); ++it)
    (*m2)[pure_new(it->first)] = pure_new(it->second);
  return m2;
}

extern "C" void hashmap_clear(exprmap *m)
{
  for (exprmap::iterator it = m->begin(); it != m->end(); ++it) {
    pure_free(it->first);
    pure_free(it->second);
  }
  m->clear();
}
