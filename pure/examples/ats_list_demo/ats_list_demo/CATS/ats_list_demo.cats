/* -*- coding: utf-8; c-file-style: "gnu"; indent-tabs-mode: nil; -*- */

/*--------------------------------------------------------------------*/

#ifndef ATS_LIST_DEMO_CATS
#define ATS_LIST_DEMO_CATS

#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>
#include <pure/runtime.h>

#if HAVE_ATOMIC_OPS && HAVE_PTHREAD
#include <atomic_ops.h>
#include <pthread.h>
#endif

void pure_throw (pure_expr *);

static inline atstype_void
atslistdemo__pure_throw (pure_expr *expr)
{
  pure_throw (expr);
}

/*--------------------------------------------------------------------*/

/* A demonstration of thread-safe one-time initialization using
   atomic_ops. See
   http://www.hpl.hp.com/research/linux/atomic_ops/example.php4

   Atomic_ops often is available on platforms that support the
   Boehm–Demers–Weiser garbage collector (Boehm GC). */

#if HAVE_ATOMIC_OPS && HAVE_PTHREAD

static volatile AO_t _ats_list_demo_is_initialized = false;
static pthread_mutex_t _ats_list_demo_mutex = PTHREAD_MUTEX_INITIALIZER;
static pure_expr *_pure_symbol_cons;
static int32_t _pure_tag_cons;
static pure_expr *_pure_nil;
static int32_t _pure_tag_nil;

static void
_initialize_ats_list_demo (void)
{
  /* Get a lock. */
  pthread_mutex_lock (&_ats_list_demo_mutex);

  /* Did some other thread do the initialization before the lock was
     obtained? */
  if (!_ats_list_demo_is_initialized)
    {
      /* If not, do the initialization. */
      _pure_tag_cons = pure_getsym (":");
      _pure_symbol_cons = pure_symbol (_pure_tag_cons);
      _pure_nil = pure_listv (0, NULL);
      _pure_tag_nil = _pure_nil->tag;
      AO_store_release_write (&_ats_list_demo_is_initialized, true);
    }

  /* Release the lock. */
  pthread_mutex_unlock (&_ats_list_demo_mutex);
}

/* Return the symbol for list cons. */
static inline pure_expr *
atslistdemo__pure_symbol_cons (void)
{
  if (!AO_load_acquire_read (&_ats_list_demo_is_initialized))
    _initialize_ats_list_demo ();
  return _pure_symbol_cons;
}

/* Return the tag for list cons. */
static inline atstype_int32
atslistdemo__pure_tag_cons (void)
{
  if (!AO_load_acquire_read (&_ats_list_demo_is_initialized))
    _initialize_ats_list_demo ();
  return _pure_tag_cons;
}

/* Return an empty Pure list. */
static inline pure_expr *
atslistdemo__pure_nil (void)
{
  if (!AO_load_acquire_read (&_ats_list_demo_is_initialized))
    _initialize_ats_list_demo ();
  return _pure_nil;
}

/* Return the tag of an empty Pure list. */
static inline atstype_int32
atslistdemo__pure_tag_nil (void)
{
  if (!AO_load_acquire_read (&_ats_list_demo_is_initialized))
    _initialize_ats_list_demo ();
  return _pure_tag_nil;
}

#else

/* Return the symbol for list cons. */
static inline pure_expr *
atslistdemo__pure_symbol_cons (void)
{
  return pure_symbol (pure_getsym (":"));
}

/* Return the tag for list cons. */
static inline atstype_int32
atslistdemo__pure_tag_cons (void)
{
  return pure_getsym (":");
}

/* Return an empty Pure list. */
static inline pure_expr *
atslistdemo__pure_nil (void)
{
  return pure_listv (0, NULL);
}

/* Return the tag of an empty Pure list. */
static inline atstype_int32
atslistdemo__pure_tag_nil (void)
{
  return pure_listv (0, NULL)->tag;
}

#endif

/*--------------------------------------------------------------------*/

static inline pure_expr *
pure_apply2 (pure_expr *x, pure_expr *y)
{
  /* Count references and construct a function application. */
  pure_new_args (2, x, y);
  return pure_apply (x, y);
}

static pure_expr *
atslistdemo__pure_not_a_cons_exception (pure_expr *expr)
{
  assert (expr != NULL);
  pure_expr *sym = pure_const (pure_sym ("not_a_cons"));
  return pure_apply2 (sym, expr);
}

static pure_expr *
atslistdemo__pure_not_a_list_exception (pure_expr *expr)
{
  assert (expr != NULL);
  pure_expr *sym = pure_const (pure_sym ("not_a_list"));
  return pure_apply2 (sym, expr);
}

static pure_expr *
atslistdemo__pure_incompatible_list_length_exception (pure_expr *expr,
                                                      size_t n)
{
  assert (expr != NULL);
  pure_expr *sym = pure_const (pure_sym ("incompatible_list_length"));
  return pure_apply2 (sym, pure_tuplel (2, expr, pure_int (n)));
}

/* A wrapper around free(3), for freeing arrays of (pure_expr *). */
static inline atstype_void
atslistdemo__free_purexp_array (atstype_void *exprs)
{
  free (exprs);
}

/* A wrapper around pure_listv that casts the pointer type of
   elems. */
static inline pure_expr *
atslistdemo__pure_listv (size_t size, atstype_void *elems)
{
  return pure_listv (size, elems);
}

/* A wrapper around pure_is_int that converts <stdbool.h> bool to ATS
   bool and which always initializes (*value). */
static inline atstype_bool
atslistdemo__pure_is_int (const pure_expr *expr, int32_t *value)
{
  *value = 0;
  return pure_is_int (expr, value);
}

/* A wrapper around pure_is_listv that converts <stdbool.h> bool to
   ATS bool and which always initializes (*size). The elems argument
   may be a null pointer. */
static inline atstype_bool
atslistdemo__pure_is_listv (atstype_void *expr, atstype_size *size,
                            atstype_void *elems)
{
  *size = 0;
  return pure_is_listv (expr, size, elems);
}

/* Is the expression an empty Pure list? */
static inline int
atslistdemo__pure_is_nil (pure_expr *expr)
{
  return (expr->tag == atslistdemo__pure_tag_nil ());
}

/* Return the cons of two Pure expressions. */
static inline pure_expr *
atslistdemo__pure_cons (pure_expr *head, pure_expr *tail)
{
  return pure_apply2 (pure_apply2 (atslistdemo__pure_symbol_cons (), head),
                      tail);
}

/* Is the expression a Pure cons cell? */
static int
atslistdemo__pure_is_cons (pure_expr *expr)
{
  pure_expr *fun1;
  pure_expr *arg1;
  pure_expr *fun2;
  pure_expr *arg2;
  bool is;
  bool result;

  result = false;
  is = pure_is_app (expr, &fun1, &arg1);
  if (is)
    {
      is = pure_is_app (fun1, &fun2, &arg2);
      if (is && fun2->tag == atslistdemo__pure_tag_cons ())
        result = true;
    }
  return result;
}

/* Get the head and tail of a Pure cons cell. */
static atstype_void
atslistdemo__pure_get_head_tail (pure_expr *lst, pure_expr **head,
                                 pure_expr **tail)
{
  pure_expr *fun1;
  pure_expr *arg1;
  pure_expr *fun2;
  pure_expr *arg2;
  bool is;

  is = pure_is_app (lst, &fun1, &arg1);
  if (is)
    {
      is = pure_is_app (fun1, &fun2, &arg2);
      is = (is && fun2->tag == atslistdemo__pure_tag_cons ());
    }

  if (is)
    {
      *head = arg2;
      *tail = arg1;
    }
  else
    pure_throw (atslistdemo__pure_not_a_cons_exception (lst));
}

/* Return the head of a Pure cons cell. */
static inline pure_expr *
atslistdemo__pure_head (pure_expr *lst)
{
  pure_expr *head;
  pure_expr *tail;
  atslistdemo__pure_get_head_tail (lst, &head, &tail);
  return head;
}

/* Return the tail of a Pure cons cell. */
static inline pure_expr *
atslistdemo__pure_tail (pure_expr *lst)
{
  pure_expr *head;
  pure_expr *tail;
  atslistdemo__pure_get_head_tail (lst, &head, &tail);
  return tail;
}

/*--------------------------------------------------------------------*/

#endif /* ATS_LIST_DEMO_CATS */
