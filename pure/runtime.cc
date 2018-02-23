// vim: set shiftwidth=2 softtabstop=2:

/* Copyright (c) 2008-2013 by Albert Graef <Dr.Graef@t-online.de>.
   Copyright (c) 2008, 2009 by Scott E. Dillard.

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

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
#pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif
#ifdef __MINGW32__
#include <malloc.h>
#endif

#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <iostream>
#include <sstream>

#include "interpreter.hh"
#include "util.hh"

#include "funcall.h"

#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifdef __MINGW32__
#include <process.h>
#endif

#ifndef P_WAIT
#define P_WAIT 0
#define P_NOWAIT 1
#define P_OVERLAY 2
#endif

#ifndef WORDS_BIGENDIAN
#define WORDS_BIGENDIAN 0
#endif

#include <complex> //only the type is used, not any methods
typedef std::complex<double> Complex;

#if HAVE__LONGJMP && HAVE__SETJMP
/* On some systems, setjmp/longjmp can be slow. Use _setjmp/_longjmp if we
   have it. */
#undef setjmp
#undef longjmp
#define setjmp  _setjmp
#define longjmp  _longjmp
#endif

/* Implement the basic GSL-like operations on the matrix types that we need.
   Note that this code doesn't depend on GSL in any way, so that the operations
   work all the time even though the runtime isn't linked against GSL. */

#include "gsl_structs.h"

static gsl_matrix*
gsl_matrix_alloc(const size_t n1, const size_t n2)
{
  gsl_block* block;
  gsl_matrix* m;
  if (n1 == 0 || n2 == 0)
    return 0;
  m = (gsl_matrix*)malloc(sizeof(gsl_matrix));
  if (m == 0)
    return 0;
  block = (gsl_block*)malloc(sizeof(gsl_block));
  if (block == 0) {
    free(m);
    return 0;
  }
  block->size = n1*n2;
  block->data = (double*)malloc(block->size*sizeof(double));
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

static gsl_matrix*
gsl_matrix_calloc(const size_t n1, const size_t n2)
{
  gsl_matrix* m = gsl_matrix_alloc(n1, n2);
  if (m == 0) return 0;
  memset(m->data, 0, m->block->size*sizeof(double));
  return m;
}

static void gsl_matrix_free(gsl_matrix *m)
{
  if (m->owner) {
    if (m->block) free(m->block->data);
    free(m->block);
  }
  free(m);
}

static gsl_matrix_view
gsl_matrix_submatrix(gsl_matrix *m,
		     const size_t i, const size_t j,
		     const size_t n1, const size_t n2)
{
  gsl_matrix_view view = {{0, 0, 0, 0, 0, 0}};
  if (i >= m->size1 || j >= m->size2 || n1 == 0 || n2 == 0 ||
      i + n1 > m->size1 || j + n2 > m->size2)
    return view;
  else {
     gsl_matrix s = {0, 0, 0, 0, 0, 0};
     s.data = m->data + i * m->tda + j;
     s.size1 = n1;
     s.size2 = n2;
     s.tda = m->tda;
     s.block = m->block;
     s.owner = 0;
     view.matrix = s;
     return view;
  }
}

static gsl_matrix_view gsl_matrix_view_array(double* p, size_t n1, size_t n2)
{
  gsl_matrix_view view = {{0, 0, 0, 0, 0, 0}};
  if (n1 == 0 || n2 == 0)
    return view;
  else {
     gsl_matrix s = {0, 0, 0, 0, 0, 0};
     s.data = p;
     s.size1 = n1;
     s.size2 = n2;
     s.tda = n2;
     s.block = 0;
     s.owner = 0;
     view.matrix = s;
     return view;
  }
}

static int
gsl_matrix_memcpy(gsl_matrix *dest,
		  const gsl_matrix *src)
{
  const size_t src_size1 = src->size1;
  const size_t src_size2 = src->size2;
  const size_t dest_size1 = dest->size1;
  const size_t dest_size2 = dest->size2;
  if (src_size1 != dest_size1 || src_size2 != dest_size2)
    return -1;
  else {
    const size_t src_tda = src->tda ;
    const size_t dest_tda = dest->tda ;
    size_t i;
    for (i = 0; i < src_size1 ; i++)
      memcpy(dest->data+dest_tda*i, src->data+src_tda*i,
	     src_size2*sizeof(double));
    return 0;
  }
}

static gsl_matrix_complex*
gsl_matrix_complex_alloc(const size_t n1, const size_t n2)
{
  gsl_block_complex* block;
  gsl_matrix_complex* m;
  if (n1 == 0 || n2 == 0)
    return 0;
  m = (gsl_matrix_complex*)malloc(sizeof(gsl_matrix_complex));
  if (m == 0)
    return 0;
  block = (gsl_block_complex*)malloc(sizeof(gsl_block_complex));
  if (block == 0) {
    free(m);
    return 0;
  }
  block->size = n1*n2;
  block->data = (double*)malloc(2*block->size*sizeof(double));
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

static gsl_matrix_complex*
gsl_matrix_complex_calloc(const size_t n1, const size_t n2)
{
  gsl_matrix_complex* m = gsl_matrix_complex_alloc(n1, n2);
  if (m == 0) return 0;
  memset(m->data, 0, 2*m->block->size*sizeof(double));
  return m;
}

static void gsl_matrix_complex_free(gsl_matrix_complex *m)
{
  if (m->owner) {
    if (m->block) free(m->block->data);
    free(m->block);
  }
  free(m);
}

static gsl_matrix_complex_view
gsl_matrix_complex_submatrix(gsl_matrix_complex *m,
			     const size_t i, const size_t j,
			     const size_t n1, const size_t n2)
{
  gsl_matrix_complex_view view = {{0, 0, 0, 0, 0, 0}};
  if (i >= m->size1 || j >= m->size2 || n1 == 0 || n2 == 0 ||
      i + n1 > m->size1 || j + n2 > m->size2)
    return view;
  else {
     gsl_matrix_complex s = {0, 0, 0, 0, 0, 0};
     s.data = m->data + 2 * (i * m->tda + j);
     s.size1 = n1;
     s.size2 = n2;
     s.tda = m->tda;
     s.block = m->block;
     s.owner = 0;
     view.matrix = s;
     return view;
  }
}

static gsl_matrix_complex_view gsl_matrix_complex_view_array
(double* p, size_t n1, size_t n2)
{
  gsl_matrix_complex_view view = {{0, 0, 0, 0, 0, 0}};
  if (n1 == 0 || n2 == 0)
    return view;
  else {
     gsl_matrix_complex s = {0, 0, 0, 0, 0, 0};
     s.data = p;
     s.size1 = n1;
     s.size2 = n2;
     s.tda = n2;
     s.block = 0;
     s.owner = 0;
     view.matrix = s;
     return view;
  }
}

static int
gsl_matrix_complex_memcpy(gsl_matrix_complex *dest,
			  const gsl_matrix_complex *src)
{
  const size_t src_size1 = src->size1;
  const size_t src_size2 = src->size2;
  const size_t dest_size1 = dest->size1;
  const size_t dest_size2 = dest->size2;
  if (src_size1 != dest_size1 || src_size2 != dest_size2)
    return -1;
  else {
    const size_t src_tda = src->tda ;
    const size_t dest_tda = dest->tda ;
    size_t i;
    for (i = 0; i < src_size1 ; i++)
      memcpy(dest->data+2*dest_tda*i, src->data+2*src_tda*i,
	     2*src_size2*sizeof(double));
    return 0;
  }
}

static gsl_matrix_int*
gsl_matrix_int_alloc(const size_t n1, const size_t n2)
{
  gsl_block_int* block;
  gsl_matrix_int* m;
  if (n1 == 0 || n2 == 0)
    return 0;
  m = (gsl_matrix_int*)malloc(sizeof(gsl_matrix_int));
  if (m == 0)
    return 0;
  block = (gsl_block_int*)malloc(sizeof(gsl_block_int));
  if (block == 0) {
    free(m);
    return 0;
  }
  block->size = n1*n2;
  block->data = (int*)malloc(block->size*sizeof(int));
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

static gsl_matrix_int*
gsl_matrix_int_calloc(const size_t n1, const size_t n2)
{
  gsl_matrix_int* m = gsl_matrix_int_alloc(n1, n2);
  if (m == 0) return 0;
  memset(m->data, 0, m->block->size*sizeof(int));
  return m;
}

static void gsl_matrix_int_free(gsl_matrix_int *m)
{
  if (m->owner) {
    if (m->block) free(m->block->data);
    free(m->block);
  }
  free(m);
}

static gsl_matrix_int_view
gsl_matrix_int_submatrix(gsl_matrix_int *m,
			 const size_t i, const size_t j,
			 const size_t n1, const size_t n2)
{
  gsl_matrix_int_view view = {{0, 0, 0, 0, 0, 0}};
  if (i >= m->size1 || j >= m->size2 || n1 == 0 || n2 == 0 ||
      i + n1 > m->size1 || j + n2 > m->size2)
    return view;
  else {
     gsl_matrix_int s = {0, 0, 0, 0, 0, 0};
     s.data = m->data + i * m->tda + j;
     s.size1 = n1;
     s.size2 = n2;
     s.tda = m->tda;
     s.block = m->block;
     s.owner = 0;
     view.matrix = s;
     return view;
  }
}

static gsl_matrix_int_view gsl_matrix_int_view_array
(int* p, size_t n1, size_t n2)
{
  gsl_matrix_int_view view = {{0, 0, 0, 0, 0, 0}};
  if (n1 == 0 || n2 == 0)
    return view;
  else {
     gsl_matrix_int s = {0, 0, 0, 0, 0, 0};
     s.data = p;
     s.size1 = n1;
     s.size2 = n2;
     s.tda = n2;
     s.block = 0;
     s.owner = 0;
     view.matrix = s;
     return view;
  }
}

static int
gsl_matrix_int_memcpy(gsl_matrix_int *dest,
		      const gsl_matrix_int *src)
{
  const size_t src_size1 = src->size1;
  const size_t src_size2 = src->size2;
  const size_t dest_size1 = dest->size1;
  const size_t dest_size2 = dest->size2;
  if (src_size1 != dest_size1 || src_size2 != dest_size2)
    return -1;
  else {
    const size_t src_tda = src->tda ;
    const size_t dest_tda = dest->tda ;
    size_t i;
    for (i = 0; i < src_size1 ; i++)
      memcpy(dest->data+dest_tda*i, src->data+src_tda*i,
	     src_size2*sizeof(int));
    return 0;
  }
}

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
  m->q = 0;
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

static void free_record(void *q);

static void gsl_matrix_symbolic_free(gsl_matrix_symbolic *m)
{
  if (m->owner) {
    if (m->block) free(m->block->data);
    free(m->block);
    if (m->q) free_record(m->q);
  }
  free(m);
}

static gsl_matrix_symbolic_view
gsl_matrix_symbolic_submatrix(gsl_matrix_symbolic *m,
			      const size_t i, const size_t j,
			      const size_t n1, const size_t n2)
{
  gsl_matrix_symbolic_view view = {{0, 0, 0, 0, 0, 0, 0}};
  if (i >= m->size1 || j >= m->size2 || n1 == 0 || n2 == 0 ||
      i + n1 > m->size1 || j + n2 > m->size2)
    return view;
  else {
     gsl_matrix_symbolic s = {0, 0, 0, 0, 0, 0, 0};
     s.data = m->data + i * m->tda + j;
     s.size1 = n1;
     s.size2 = n2;
     s.tda = m->tda;
     s.block = m->block;
     s.owner = 0;
     s.q = 0;
     view.matrix = s;
     return view;
  }
}

static int
gsl_matrix_symbolic_memcpy(gsl_matrix_symbolic *dest,
			   const gsl_matrix_symbolic *src)
{
  const size_t src_size1 = src->size1;
  const size_t src_size2 = src->size2;
  const size_t dest_size1 = dest->size1;
  const size_t dest_size2 = dest->size2;
  if (src_size1 != dest_size1 || src_size2 != dest_size2)
    return -1;
  else {
    const size_t src_tda = src->tda ;
    const size_t dest_tda = dest->tda ;
    size_t i;
    for (i = 0; i < src_size1 ; i++)
      memcpy(dest->data+dest_tda*i, src->data+src_tda*i,
	     src_size2*sizeof(pure_expr*));
    return 0;
  }
}

// Hooks to report stack overflows and other kinds of hard errors.
#define checkstk(test) if (interpreter::stackmax > 0 &&			\
      interpreter::stackdir*(&test - interpreter::baseptr)		\
      >= interpreter::stackmax)						\
    pure_throw(stack_exception())
#define checkall(test) if (interpreter::stackmax > 0 &&			\
	interpreter::stackdir*(&test - interpreter::baseptr)		\
	>= interpreter::stackmax)					\
    pure_throw(stack_exception());					\
  else if (interpreter::brkmask) {					\
    if (interpreter::brkmask == 2) interpreter::brkmask = 0;		\
  } else if (interpreter::brkflag)					\
    pure_throw(signal_exception(interpreter::brkflag))

// Debug expression allocations. Warns about expression memory leaks.
// NOTE: Bookkeeping starts and ends at each toplevel pure_invoke call.
// Enabling this code will make the interpreter *much* slower.
#if MEMDEBUG
#if MEMDEBUG>1 // enable this to print each and every expression (de)allocation
#define MEMDEBUG_NEW(x)  interpreter::g_interp->mem_allocations.insert(x); \
  cerr << "NEW:  " << (void*)x << ": " << x << endl;
#define MEMDEBUG_FREE(x) interpreter::g_interp->mem_allocations.erase(x); \
  cerr << "FREE: " << (void*)x << ": " << x << endl;
#else
#define MEMDEBUG_NEW(x)  interpreter::g_interp->mem_allocations.insert(x);
#define MEMDEBUG_FREE(x) interpreter::g_interp->mem_allocations.erase(x);
#endif
#define MEMDEBUG_INIT if (!interpreter::g_interp->astk)	\
    interpreter::g_interp->mem_allocations.clear();
#define MEMDEBUG_SUMMARY(ret) if (!interpreter::g_interp->astk) {\
    mem_mark(ret);							\
    if (!interpreter::g_interp->mem_allocations.empty()) {		\
      cerr << "** WARNING: leaked expressions:\n";			\
      for (set<pure_expr*>::iterator x =				\
	     interpreter::g_interp->mem_allocations.begin();		\
	   x != interpreter::g_interp->mem_allocations.end(); x++)	\
	cerr << (void*)(*x) << " (refc = " << (*x)->refc << "): "	\
	     << (*x) << endl;						\
      interpreter::g_interp->mem_allocations.clear();			\
    }									\
  }
static void mem_mark(pure_expr *x)
{
  interpreter::g_interp->mem_allocations.erase(x);
  if (x->tag == EXPR::APP) {
    mem_mark(x->data.x[0]);
    mem_mark(x->data.x[1]);
  } else if (x->tag >= 0 && x->data.clos) {
    for (size_t i = 0; i < x->data.clos->m; i++)
      mem_mark(x->data.clos->env[i]);
  }
}
#else
#define MEMDEBUG_NEW(x)
#define MEMDEBUG_FREE(x)
#define MEMDEBUG_INIT
#define MEMDEBUG_SUMMARY(ret)
#endif

// Debug shadow stack manipulations. Prints pushes and pops of stack frames.
// NOTE: Enabling this code generates *lots* of debugging output.
#if DEBUG>2
#define SSTK_DEBUG 1
#else
#define SSTK_DEBUG 0
#endif

static inline pure_expr* pure_apply2(pure_expr *x, pure_expr *y)
{
  // Count references and construct a function application.
  pure_new_args(2, x, y);
  return pure_apply(x, y);
}

static inline pure_expr* pure_applc2(pure_expr *x, pure_expr *y)
{
  // Count references and construct a function application.
  pure_new_args(2, x, y);
  return pure_applc(x, y);
}

static inline pure_expr* signal_exception(int sig)
{
  if (!interpreter::g_interp) return 0;
  pure_expr *f = pure_const(interpreter::g_interp->symtab.signal_sym().f);
  pure_expr *x = pure_int(sig);
  return pure_apply2(f, x);
}

static inline pure_expr* unresolved_exception()
{
  return pure_const(pure_sym("unresolved_external"));
}

static inline pure_expr* stack_exception()
{
  if (!interpreter::g_interp) return 0;
  return pure_const(interpreter::g_interp->symtab.segfault_sym().f);
}

static inline pure_expr* bad_matrix_exception(pure_expr *x)
{
  if (!interpreter::g_interp) return 0;
  pure_expr *f = pure_const(interpreter::g_interp->symtab.bad_matrix_sym().f);
  if (x)
    return pure_apply2(f, x);
  else
    return f;
}

static inline pure_expr *get_sentry(pure_expr *x)
{
  if (x==0)
    return 0;
  else
    return x->sy;
}

static inline void call_sentry(pure_expr *x)
{
  pure_expr *s = x->sy;
  if (s) {
    ++x->refc;
    pure_freenew(pure_apply2(s, x));
    --x->refc;
  }
}

static inline void free_sentry(pure_expr *x)
{
  pure_expr *s = x->sy;
  if (s) pure_free(s);
}

// Expression pointers are allocated in larger chunks for better performance.
// NOTE: Only internal fields get initialized by new_expr(), the remaining
// fields *must* be initialized as appropriate by the caller.

static inline pure_expr *new_expr()
{
  interpreter& interp = *interpreter::g_interp;
  pure_expr *x = interp.exps;
  if (x) {
    interp.exps = x->xp;
    interp.freectr--;
    if (interp.stats_mem && interp.freectr < interp.memctr)
      interp.memctr = interp.freectr;
  } else if (interp.mem && interp.mem->p-interp.mem->x < MEMSIZE) {
    x = interp.mem->p++;
    if (interp.stats_mem) interp.memctr = 0;
  } else {
    pure_mem *mem = interp.mem;
    interp.mem = new pure_mem;
    interp.mem->next = mem;
    interp.mem->p = interp.mem->x;
    x = interp.mem->p++;
    if (interp.stats_mem) interp.memctr = 0;
  }
  x->refc = 0;
  x->xp = interp.tmps;
  x->sy = 0; // initialize the sentry
  x->data.x[1] = 0; // initialize the pointer tag
  interp.tmps = x;
  return x;
}

static inline pure_expr *new_ref_expr()
{
  interpreter& interp = *interpreter::g_interp;
  pure_expr *x = interp.exps;
  if (x) {
    interp.exps = x->xp;
    interp.freectr--;
    if (interp.stats_mem && interp.freectr < interp.memctr)
      interp.memctr = interp.freectr;
  } else if (interp.mem && interp.mem->p-interp.mem->x < MEMSIZE) {
    x = interp.mem->p++;
    if (interp.stats_mem) interp.memctr = 0;
  } else {
    pure_mem *mem = interp.mem;
    interp.mem = new pure_mem;
    interp.mem->next = mem;
    interp.mem->p = interp.mem->x;
    x = interp.mem->p++;
    if (interp.stats_mem) interp.memctr = 0;
  }
  x->refc = 1;
  x->xp = 0;
  x->sy = 0; // initialize the sentry
  return x;
}

static inline void free_expr(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  x->xp = interp.exps;
  interp.exps = x;
  interp.freectr++;
  MEMDEBUG_FREE(x)
}

static inline
pure_expr *pure_new_internal(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  assert(x && "pure_new: null expression");
  assert((x->refc==0 || !x->xp) && "pure_new: corrupt expression data");
#if DEBUG>2
  if (x->tag >= 0 && x->data.clos)
    cerr << "pure_new: " << (x->data.clos->local?"local":"global")
	 << " closure " << x << " (" << (void*)x << "), refc = "
	 << x->refc << endl;
#endif
  if (x->refc++ == 0) {
    // remove x from the list of temporaries
    if (interp.tmps == x)
      interp.tmps = x->xp;
    else {
      // walk the list to find the place where x has to be unlinked
      pure_expr *tmps = interp.tmps;
      while (tmps && tmps->xp != x) tmps = tmps->xp;
      assert(tmps);
      tmps->xp = x->xp;
    }
    x->xp = 0;
  }
  return x;
}

/* Mass updates of refcounts. We need to optimize this case to avoid quadratic
   complexity due to repeated searches of the tmps list. This prevents some
   serious performance issues, in particular in symbolic matrix primitives.
   Thanks to Scott E. Dillard for pointing this out. */

/* To make this fast, we currently employ a rather simple heuristic which only
   optimizes the most common cases (however, these seem to hold in most
   primitives where it matters):

   - The vector elements have a nonzero reference count already (as determined
     by looking at the first element). In this case updates don't refer to the
     tmps list and will be fast anyway (no optimization needed).

   - The *last* vector element is near the beginning of the tmps list (as
     determined by looking at the first three elements of the tmps list).
     This indicates that the elements have been created in order and should
     be updated in reverse.

   - The *first* vector element is near the beginning of the tmps list (as
     determined by looking at the first three elements of the tmps list).
     This indicates that the elements have been created in reverse and should
     be updated in order.

   If a primitive falls outside this scheme then it may still suffer from
   quadratic complexity. In such a case it is best if you create the elements
   with a nonzero reference count instead, and decrement them later. */

static inline short peek(pure_expr *const tmps,
			 pure_expr *const first, pure_expr *const last)
{
  pure_expr *t = tmps;
  short count = 0;
  while (t && count < 3)
    if (t == last)
      return -1;
    else if (t == first)
      return 1;
    else {
      t = t->xp; count++;
    }
  return 0;
}

static inline void pure_new_vect(const size_t n, pure_expr **xs)
{
  short rc;
  if (n == 0)
    return;
  else if (xs[0]->refc > 0)
    rc = 0;
  else {
    interpreter& interp = *interpreter::g_interp;
    pure_expr *tmps = interp.tmps;
    pure_expr *first = xs[0], *last = xs[n-1];
    rc = peek(tmps, first, last);
  }
  if (rc <= 0)
    for (size_t i = n; i > 0; )
      pure_new_internal(xs[--i]);
  else
    for (size_t i = 0; i < n; i++)
      pure_new_internal(xs[i]);
}

/* Also handle the case of non-contiguous expression vectors which might arise
   in symbolic matrix operations. */

static inline void pure_new_vect2(const size_t n, const size_t m,
				  const size_t stride, pure_expr **xs)
{
  if (stride == m) {
    pure_new_vect(n*m, xs);
    return;
  }
  short rc;
  if (n == 0 || m == 0)
    return;
  else if (xs[0]->refc > 0)
    rc = 0;
  else {
    interpreter& interp = *interpreter::g_interp;
    pure_expr *tmps = interp.tmps;
    pure_expr *first = xs[0], *last = xs[(n-1)*stride+m-1];
    rc = peek(tmps, first, last);
  }
  if (rc <= 0)
    for (size_t i = n; i > 0; ) {
      const size_t k = (--i)*stride;
      for (size_t j = m; j > 0; )
	pure_new_internal(xs[k+(--j)]);
    }
  else
    for (size_t i = 0; i < n; i++) {
      const size_t k = i*stride;
      for (size_t j = 0; j < m; j++)
	pure_new_internal(xs[k+j]);
    }
}

static void pure_free_clos(pure_expr *x)
{
#if DEBUG>3
  cerr << "pure_free, freeing environment: "
       << (x->data.clos->local?"local":"global") << " closure "
       << x << " (" << (void*)x << "), refc = " << x->refc << endl;
#endif
  if (x->data.clos->ep) {
    Env *env = (Env*)x->data.clos->ep;
    assert(env->refc > 0);
    if (--env->refc == 0) delete env;
  }
  if (x->data.clos->refp) {
    uint32_t *refp = (uint32_t*)x->data.clos->refp;
    assert(*refp > 0);
    --(*refp);
  }
  if (x->data.clos->env) {
    for (size_t i = 0; i < x->data.clos->m; i++)
      pure_free(x->data.clos->env[i]);
    delete[] x->data.clos->env;
  }
  delete x->data.clos;
}

static pure_closure *pure_copy_clos(pure_closure *clos)
{
  assert(clos);
  pure_closure *ret = new pure_closure;
  ret->local = clos->local;
  ret->key = clos->key;
  ret->n = clos->n;
  ret->m = clos->m;
  ret->fp = clos->fp;
  ret->ep = clos->ep;
  if (clos->ep) ((Env*)clos->ep)->refc++;
  ret->refp = clos->refp;
  if (clos->refp) (*(uint32_t*)clos->refp)++;
  if (clos->m == 0)
    ret->env = 0;
  else {
    ret->env = new pure_expr*[clos->m];
    for (size_t i = 0; i < clos->m; i++) {
      ret->env[i] = clos->env[i];
      assert(clos->env[i]->refc > 0);
      clos->env[i]->refc++;
    }
  }
  return ret;
}

static void pure_free_matrix(pure_expr *x)
{
  if (!x->data.mat.p) return;
  assert(x->data.mat.refc && "pure_free_matrix: corrupt data");
  assert(*x->data.mat.refc > 0 && "pure_free_matrix: unreferenced data");
  bool owner = --*x->data.mat.refc == 0;
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    if (owner) {
      const size_t k = m->size1, l = m->size2, tda = m->tda;
      for (size_t i = 0; i < k; i++)
	for (size_t j = 0; j < l; j++)
	  pure_free(m->data[i*tda+j]);
    }
    m->owner = owner;
    gsl_matrix_symbolic_free(m);
    break;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    m->owner = owner && m->block;
    gsl_matrix_free(m);
    break;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    m->owner = owner && m->block;
    gsl_matrix_complex_free(m);
    break;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    m->owner = owner && m->block;
    gsl_matrix_int_free(m);
    break;
  }
  default:
    break;
  }
  if (owner) delete x->data.mat.refc;
}

#if 1

/* This is implemented (mostly) non-recursively to prevent stack overflows,
   using the xp field to form a stack on the fly. */

static inline
void pure_free_internal(pure_expr *x)
{
  assert(x && "pure_free: null expression");
  assert(x->refc > 0 && "pure_free: unreferenced expression");
  assert(!x->xp && "pure_free: corrupt expression data");
  pure_expr *xp = 0, *y;
 loop:
  if (--x->refc == 0) {
    call_sentry(x);
    switch (x->tag) {
    case EXPR::APP:
      y = x->data.x[0];
      assert(!x->xp);
      x->xp = xp; xp = x; x = y;
      goto loop;
    case EXPR::INT:
    case EXPR::DBL:
    case EXPR::PTR:
      // nothing to do
      break;
    case EXPR::BIGINT:
      mpz_clear(x->data.z);
      break;
    case EXPR::STR:
      my_strfree(x->data.s);
      break;
    case EXPR::MATRIX:
    case EXPR::DMATRIX:
    case EXPR::CMATRIX:
    case EXPR::IMATRIX:
      pure_free_matrix(x);
      break;
    default:
      assert(x->tag >= 0);
      if (x->data.clos) pure_free_clos(x);
      break;
    }
  }
  while (xp && x == xp->data.x[1]) {
    if (x->refc == 0) {
      free_sentry(x);
      free_expr(x);
    }
    x = xp; xp = x->xp;
  }
  if (x->refc == 0) {
    free_sentry(x);
    free_expr(x);
  }
  if (xp) {
    x = xp->data.x[1];
    goto loop;
  }
}

#else

/* This version is only included here for reference and debugging purposes,
   normal builds should use the non-recursive version above. */

static
void pure_free_internal(pure_expr *x)
{
  if (--x->refc == 0) {
    call_sentry(x);
    switch (x->tag) {
    case EXPR::APP:
      pure_free_internal(x->data.x[0]);
      pure_free_internal(x->data.x[1]);
      break;
    case EXPR::INT:
    case EXPR::DBL:
      break;
    case EXPR::BIGINT:
      mpz_clear(x->data.z);
      break;
    case EXPR::STR:
      free(x->data.s);
      break;
    case EXPR::PTR:
      break;
    default:
      assert(x->tag >= 0);
      if (x->data.clos) pure_free_clos(x);
      break;
    }
    free_sentry(x);
    free_expr(x);
  }
}

#endif

static void inline
pure_unref_internal(pure_expr *x)
{
  assert(x && "pure_unref: null expression");
  assert(x->refc > 0 && "pure_unref: unreferenced expression");
  if (--x->refc == 0 && !x->xp) {
    interpreter& interp = *interpreter::g_interp;
    // check whether x is already on the tmps list
    pure_expr *tmps = interp.tmps;
    while (tmps && tmps != x) tmps = tmps->xp;
    if (!tmps) {
      // put x on the tmps list again
      x->xp = interp.tmps;
      interp.tmps = x;
    }
  }
}

/* PUBLIC API. **************************************************************/

extern "C"
int32_t pure_sym(const char *s)
{
  assert(s);
  interpreter& interp = *interpreter::g_interp;
  string id = (strncmp(s, "::", 2)==0)?s:"::"+string(s);
  size_t pos = symsplit(id);
  string qual = (pos>2)?id.substr(2, pos-2):"";
  const symbol* sym;
  if (qual != *interp.symtab.current_namespace) {
    /* Switch to the desired namespace so that we also get the private
       symbols. */
    string *save_namespace = interp.symtab.current_namespace;
    interp.symtab.current_namespace = &qual;
    sym = interp.symtab.sym(id);
    interp.symtab.current_namespace = save_namespace;
  } else
    sym = interp.symtab.sym(id);
  if (sym)
    return sym->f;
  else
    return 0;
}

extern "C"
int32_t pure_getsym(const char *s)
{
  assert(s);
  interpreter& interp = *interpreter::g_interp;
  string id = (strncmp(s, "::", 2)==0)?s:"::"+string(s);
  size_t pos = symsplit(id);
  string qual = (pos>2)?id.substr(2, pos-2):"";
  const symbol* sym;
  if (qual != *interp.symtab.current_namespace) {
    /* Switch to the desired namespace so that we also get the private
       symbols. */
    string *save_namespace = interp.symtab.current_namespace;
    interp.symtab.current_namespace = &qual;
    sym = interp.symtab.lookup(id);
    interp.symtab.current_namespace = save_namespace;
  } else
    sym = interp.symtab.lookup(id);
  if (sym)
    return sym->f;
  else
    return 0;
}

extern "C"
const char *pure_sym_pname(int32_t tag)
{
  assert(tag>0);
  interpreter& interp = *interpreter::g_interp;
  const symbol& sym = interp.symtab.sym(tag);
  return sym.s.c_str();
}

extern "C"
int32_t pure_sym_other(int32_t tag)
{
  assert(tag>0);
  interpreter& interp = *interpreter::g_interp;
  const symbol& sym = interp.symtab.sym(tag);
  return (sym.fix == outfix)?sym.g:0;
}

extern "C"
int32_t pure_sym_nprec(int32_t tag)
{
  assert(tag>0);
  interpreter& interp = *interpreter::g_interp;
  const symbol& sym = interp.symtab.sym(tag);
  return nprec(sym.prec, sym.fix);
}

extern "C"
pure_expr *pure_symbol(int32_t tag)
{
  assert(tag>0);
  interpreter& interp = *interpreter::g_interp;
  const symbol& sym = interp.symtab.sym(tag);
  // There might be some functions still waiting to be compiled, do that now.
  interp.compile();
  // Check for an existing global variable for this symbol.
  GlobalVar& v = interp.globalvars[tag];
  if (!v.v) {
    // The variable doesn't exist yet (we have a new symbol), create it.
    string lab;
    // Create a name for the variable (cf. interpreter::mkvarlabel).
    if (sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix)
      if (sym.fix == outfix && sym.g)
	lab = "$("+sym.s+" "+interp.symtab.sym(sym.g).s+")";
      else
	lab = "$("+sym.s+")";
    else
      lab = "$"+sym.s;
    // Create a global variable bound to the symbol for now.
    v.v = interpreter::global_variable
      (interp.module, interp.ExprPtrTy, false,
       llvm::GlobalVariable::InternalLinkage,
       llvm::ConstantPointerNull::get(interp.ExprPtrTy),
       lab.c_str());
    interp.JIT->addGlobalMapping(v.v, &v.x);
    v.x = pure_new_internal(pure_const(tag));
    // Since we just created this variable, it doesn't have any closure bound
    // to it yet, so it's safe to just return the symbol as is.
    return v.x;
  } else {
    // The symbol already has a definition, so we might have to evaluate it on
    // the fly.
    map<int32_t,ExternInfo>::const_iterator it = interp.externals.find(tag);
    if (it != interp.externals.end()) {
      // We have an external. This case must be treated separately, since v
      // just points to a box for the Pure function symbol rather than the
      // external wrapper function itself.
      const ExternInfo& info = it->second;
      size_t n = info.argtypes.size();
      void *f = interp.JIT->getPointerToFunction(info.f);
      if (f) {
	if (n == 0) {
	  // Parameterless external, do a direct call.
	  if (!interp.debugging) pure_push_args(0, 0);
	  return ((pure_expr *(*)(void))f) ();
	} else {
	  // External with parameters. Build an fbox for the external, return
	  // this as the value of the symbol.
	  return pure_clos(false, tag, 0, n, f, 0, 0);
	}
      }
      // If we come here, the external wrapper failed to compile for some
      // reason, just proceed as if it was an ordinary Pure function.
    }
    // We have an ordinary Pure symbol. There might be a parameterless closure
    // bound to it, pure_call takes care of that case.
    return pure_call(v.x);
  }
}

// This *must* be executed in the stack frame of the caller, so we implement
// this as a macro.

#define pure_try_call(x)				\
  assert(_e);						\
  pure_expr*& e = *_e;					\
  interpreter& interp = *interpreter::g_interp;		\
  pure_aframe *ex = interp.push_aframe(interp.sstk_sz);	\
  pure_expr *old_tmps = interp.tmps; interp.tmps = 0;	\
  if (setjmp(ex->jmp)) {				\
    size_t sz = ex->sz;					\
    e = ex->e;						\
    interp.pop_aframe();				\
    if (e) pure_new_internal(e);			\
    pure_expr *tmps = interp.tmps;			\
    while (tmps) {					\
      pure_expr *next = tmps->xp;			\
      pure_freenew(tmps);				\
      tmps = next;					\
    }							\
    interp.tmps = old_tmps;				\
    for (size_t i = interp.sstk_sz; i-- > sz; )		\
      if (interp.sstk[i] && interp.sstk[i]->refc > 0)	\
	pure_free_internal(interp.sstk[i]);		\
    interp.sstk_sz = sz;				\
    pure_unref_internal(e);				\
    return 0;						\
  } else {						\
    pure_expr *res = x;					\
    interp.pop_aframe();				\
    pure_expr *tmps = interp.tmps;			\
    while (tmps) {					\
      pure_expr *next = tmps->xp;			\
      if (tmps != res) pure_freenew(tmps);		\
      tmps = next;					\
    }							\
    interp.tmps = old_tmps;				\
    res->refc++; pure_unref_internal(res);		\
    e = NULL;						\
    return res;						\
  }

extern "C"
pure_expr *pure_symbolx(int32_t tag, pure_expr **_e)
{
  pure_try_call(pure_symbol(tag));
}

extern "C"
pure_expr *pure_quoted_symbol(int32_t tag)
{
  assert(tag>0);
  return pure_const(tag);
}

extern "C"
pure_expr *pure_int(int32_t i)
{
  pure_expr *x = new_expr();
  x->tag = EXPR::INT;
  x->data.i = i;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_mpz(const mpz_t z)
{
  pure_expr *x = new_expr();
  x->tag = EXPR::BIGINT;
  mpz_init_set(x->data.z, z);
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_double(double d)
{
  pure_expr *x = new_expr();
  x->tag = EXPR::DBL;
  x->data.d = d;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_pointer(void *p)
{
  pure_expr *x = new_expr();
  x->tag = EXPR::PTR;
  x->data.p = p;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_expr_pointer(void)
{
  pure_expr **p = (pure_expr**)malloc(sizeof(pure_expr*));
  if (p) {
    *p = 0;
    return pure_pointer(p);
  } else
    return 0;
}

extern "C"
pure_expr *pure_string_dup(const char *s)
{
  if (!s) return pure_pointer(0);
  pure_expr *x = new_expr();
  x->tag = EXPR::STR;
  x->data.s = my_strdup(s);
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_cstring_dup(const char *s)
{
  if (!s) return pure_pointer(0);
  pure_expr *x = new_expr();
  x->tag = EXPR::STR;
  x->data.s = my_toutf8(s, 0);
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_string(char *s)
{
  if (!s) return pure_pointer(0);
  pure_expr *x = new_expr();
  x->tag = EXPR::STR;
  x->data.s = s;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_cstring(char *s)
{
  if (!s) return pure_pointer(0);
  pure_expr *x = pure_cstring_dup(s);
  free(s);
  return x;
}

/* GSL doesn't really support empty matrices, so we fake them by allocating 1
   dummy row or column if the corresponding dimension is actually zero. */
static inline gsl_matrix*
create_double_matrix(size_t nrows, size_t ncols)
{
  if (nrows == 0 || ncols == 0 ) {
    size_t nrows1 = (nrows>0)?nrows:1;
    size_t ncols1 = (ncols>0)?ncols:1;
    gsl_matrix *m = gsl_matrix_calloc(nrows1, ncols1);
    if (!m) return 0;
    m->size1 = nrows; m->size2 = ncols;
    return m;
  } else
    return gsl_matrix_alloc(nrows, ncols);
}

static inline gsl_matrix_complex*
create_complex_matrix(size_t nrows, size_t ncols)
{
  if (nrows == 0 || ncols == 0 ) {
    size_t nrows1 = (nrows>0)?nrows:1;
    size_t ncols1 = (ncols>0)?ncols:1;
    gsl_matrix_complex *m = gsl_matrix_complex_calloc(nrows1, ncols1);
    if (!m) return 0;
    m->size1 = nrows; m->size2 = ncols;
    return m;
  } else
    return gsl_matrix_complex_alloc(nrows, ncols);
}

static inline gsl_matrix_int*
create_int_matrix(size_t nrows, size_t ncols)
{
  if (nrows == 0 || ncols == 0 ) {
    size_t nrows1 = (nrows>0)?nrows:1;
    size_t ncols1 = (ncols>0)?ncols:1;
    gsl_matrix_int *m = gsl_matrix_int_calloc(nrows1, ncols1);
    if (!m) return 0;
    m->size1 = nrows; m->size2 = ncols;
    return m;
  } else
    return gsl_matrix_int_alloc(nrows, ncols);
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

extern "C"
pure_expr *pure_symbolic_matrix(void *p)
{
  gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)p;
  if (!m || !m->owner) return 0;
  m->owner = 0;
  pure_expr *x = new_expr();
  x->tag = EXPR::MATRIX;
  x->data.mat.p = p;
  // count references
  const size_t k = m->size1, l = m->size2, tda = m->tda;
  pure_new_vect2(k, l, tda, m->data);
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_double_matrix(void *p)
{
  gsl_matrix *m = (gsl_matrix*)p;
  if (!m || !m->owner) return 0;
  m->owner = 0;
  pure_expr *x = new_expr();
  x->tag = EXPR::DMATRIX;
  x->data.mat.p = p;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_complex_matrix(void *p)
{
  gsl_matrix_complex *m = (gsl_matrix_complex*)p;
  if (!m || !m->owner) return 0;
  m->owner = 0;
  pure_expr *x = new_expr();
  x->tag = EXPR::CMATRIX;
  x->data.mat.p = p;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_int_matrix(void *p)
{
  gsl_matrix_int *m = (gsl_matrix_int*)p;
  if (!m || !m->owner) return 0;
  m->owner = 0;
  pure_expr *x = new_expr();
  x->tag = EXPR::IMATRIX;
  x->data.mat.p = p;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_symbolic_matrix_dup(const void *p)
{
  gsl_matrix_symbolic *m1 = (gsl_matrix_symbolic*)p;
  if (!m1) return 0;
  gsl_matrix_symbolic *m2 = create_symbolic_matrix(m1->size1, m1->size2);
  if (!m2) return 0;
  if (m1->size1 > 0 && m1->size2 > 0)
    gsl_matrix_symbolic_memcpy(m2, m1);
  return pure_symbolic_matrix(m2);
}

extern "C"
pure_expr *pure_double_matrix_dup(const void *p)
{
  gsl_matrix *m1 = (gsl_matrix*)p;
  if (!m1) return 0;
  gsl_matrix *m2 = create_double_matrix(m1->size1, m1->size2);
  if (!m2) return 0;
  if (m1->size1 > 0 && m1->size2 > 0)
    gsl_matrix_memcpy(m2, m1);
  return pure_double_matrix(m2);
}

extern "C"
pure_expr *pure_complex_matrix_dup(const void *p)
{
  gsl_matrix_complex *m1 = (gsl_matrix_complex*)p;
  if (!m1) return 0;
  gsl_matrix_complex *m2 = create_complex_matrix(m1->size1, m1->size2);
  if (!m2) return 0;
  if (m1->size1 > 0 && m1->size2 > 0)
    gsl_matrix_complex_memcpy(m2, m1);
  return pure_complex_matrix(m2);
}

extern "C"
pure_expr *pure_int_matrix_dup(const void *p)
{
  gsl_matrix_int *m1 = (gsl_matrix_int*)p;
  if (!m1) return 0;
  gsl_matrix_int *m2 = create_int_matrix(m1->size1, m1->size2);
  if (!m2) return 0;
  if (m1->size1 > 0 && m1->size2 > 0)
    gsl_matrix_int_memcpy(m2, m1);
  return pure_int_matrix(m2);
}

static inline bool is_complex(pure_expr *x)
{
  if (x->tag != EXPR::APP) return false;
  pure_expr *u = x->data.x[0], *v = x->data.x[1];
  if (u->tag == EXPR::APP) {
    interpreter& interp = *interpreter::g_interp;
    pure_expr *f = u->data.x[0];
    symbol &rect = interp.symtab.complex_rect_sym(),
      &polar = interp.symtab.complex_polar_sym();
    if (f->tag != rect.f && f->tag != polar.f)
      return false;
    u = u->data.x[1];
    if ((u->tag != EXPR::INT && u->tag != EXPR::DBL) ||
	(v->tag != EXPR::INT && v->tag != EXPR::DBL))
      return false;
    else
      return true;
  } else
    return false;
}

static inline bool get_complex(pure_expr *x, double& a, double& b)
{
  if (x->tag != EXPR::APP) return false;
  pure_expr *u = x->data.x[0], *v = x->data.x[1];
  if (u->tag == EXPR::APP) {
    interpreter& interp = *interpreter::g_interp;
    pure_expr *f = u->data.x[0];
    symbol &rect = interp.symtab.complex_rect_sym(),
      &polar = interp.symtab.complex_polar_sym();
    if (f->tag != rect.f && f->tag != polar.f)
      return false;
    u = u->data.x[1];
    switch (u->tag) {
    case EXPR::INT:
      a = (double)u->data.i;
      break;
    case EXPR::DBL:
      a = u->data.d;
      break;
    default:
      return false;
    }
    switch (v->tag) {
    case EXPR::INT:
      b = (double)v->data.i;
      break;
    case EXPR::DBL:
      b = v->data.d;
      break;
    default:
      return false;
    }
    if (f->tag == polar.f) {
      double r = a, t = b;
      a = r*cos(t); b = r*sin(t);
    }
    return true;
  } else
    return false;
}

static inline pure_expr *make_complex2(symbol& rect, double a, double b)
{
  return pure_appl(pure_symbol(rect.f), 2, pure_double(a), pure_double(b));
}

static inline pure_expr *make_complex(double a, double b)
{
  interpreter& interp = *interpreter::g_interp;
  symbol &rect = interp.symtab.complex_rect_sym();
  return make_complex2(rect, a, b);
}

static pure_expr*
double_matrix_rows(size_t nrows, size_t ncols, size_t n, pure_expr **xs)
{
  gsl_matrix *mat = create_double_matrix(nrows, ncols);
  if (!mat) return 0;
  double *data = mat->data;
  size_t tda = mat->tda;
  pure_new_vect(n, xs);
  for (size_t count = 0, i = 0; count < n; count++) {
    pure_expr *x = xs[count];
    switch (x->tag) {
    case EXPR::INT:
      data[i++*tda] = (double)x->data.i;
      break;
    case EXPR::BIGINT:
      data[i++*tda] = mpz_get_d(x->data.z);
      break;
    case EXPR::DBL:
      data[i++*tda] = x->data.d;
      break;
    case EXPR::DMATRIX: {
      gsl_matrix *mat1 = (gsl_matrix*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; i++, j++)
	  memcpy(data+i*tda, mat1->data+j*mat1->tda, ncols*sizeof(double));
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mat1 = (gsl_matrix_int*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; i++, j++)
	  for (size_t k = 0; k < mat1->size2; k++)
	    data[i*tda+k] = (double)mat1->data[j*mat1->tda+k];
      break;
    }
    case EXPR::CMATRIX:
    case EXPR::MATRIX:
      // empty matrix, skip
      break;
    default:
      assert(0 && "bad matrix element");
      break;
    }
  }
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  return pure_double_matrix(mat);
}

static pure_expr*
double_matrix_columns(size_t nrows, size_t ncols, size_t n, pure_expr **xs)
{
  gsl_matrix *mat = create_double_matrix(nrows, ncols);
  if (!mat) return 0;
  double *data = mat->data;
  size_t tda = mat->tda;
  pure_new_vect(n, xs);
  for (size_t count = 0, i = 0; count < n; count++) {
    pure_expr *x = xs[count];
    switch (x->tag) {
    case EXPR::INT:
      data[i++] = (double)x->data.i;
      break;
    case EXPR::BIGINT:
      data[i++] = mpz_get_d(x->data.z);
      break;
    case EXPR::DBL:
      data[i++] = x->data.d;
      break;
    case EXPR::DMATRIX: {
      gsl_matrix *mat1 = (gsl_matrix*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; j++)
	  memcpy(data+j*tda+i, mat1->data+j*mat1->tda,
		 mat1->size2*sizeof(double));
      i += mat1->size2;
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mat1 = (gsl_matrix_int*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; j++)
	  for (size_t k = 0; k < mat1->size2; k++)
	    data[j*tda+k+i] = (double)mat1->data[j*mat1->tda+k];
      i += mat1->size2;
      break;
    }
    case EXPR::CMATRIX:
    case EXPR::MATRIX:
      // empty matrix, skip
      break;
    default:
      assert(0 && "bad matrix element");
      break;
    }
  }
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  return pure_double_matrix(mat);
}

static pure_expr*
complex_matrix_rows(size_t nrows, size_t ncols, size_t n, pure_expr **xs)
{
  gsl_matrix_complex *mat = create_complex_matrix(nrows, ncols);
  if (!mat) return 0;
  double *data = mat->data;
  size_t tda = mat->tda;
  pure_new_vect(n, xs);
  for (size_t count = 0, i = 0; count < n; count++) {
    pure_expr *x = xs[count];
    switch (x->tag) {
    case EXPR::INT:
      data[2*i*tda] = (double)x->data.i;
      data[2*i*tda+1] = 0.0;
      i++;
      break;
    case EXPR::BIGINT:
      data[2*i*tda] = mpz_get_d(x->data.z);
      data[2*i*tda+1] = 0.0;
      i++;
      break;
    case EXPR::DBL:
      data[2*i*tda] = x->data.d;
      data[2*i*tda+1] = 0.0;
      i++;
      break;
    case EXPR::APP: {
      double a, b;
      if (get_complex(x, a, b)) {
	data[2*i*tda] = a;
	data[2*i*tda+1] = b;
	i++;
      } else {
	assert(0 && "bad matrix element");
      }
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *mat1 = (gsl_matrix*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; i++, j++)
	  for (size_t k = 0; k < mat1->size2; k++) {
	    data[2*(i*tda+k)] = mat1->data[j*mat1->tda+k];
	    data[2*(i*tda+k)+1] = 0.0;
	  }
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mat1 = (gsl_matrix_int*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; i++, j++)
	  for (size_t k = 0; k < mat1->size2; k++) {
	    data[2*(i*tda+k)] = (double)mat1->data[j*mat1->tda+k];
	    data[2*(i*tda+k)+1] = 0.0;
	  }
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *mat1 = (gsl_matrix_complex*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; i++, j++)
	  memcpy(data+2*i*tda, mat1->data+2*j*mat1->tda,
		 ncols*2*sizeof(double));
      break;
    }
    case EXPR::MATRIX:
      // empty matrix, skip
      break;
    default:
      assert(0 && "bad matrix element");
      break;
    }
  }
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  return pure_complex_matrix(mat);
}

static pure_expr*
complex_matrix_columns(size_t nrows, size_t ncols, size_t n, pure_expr **xs)
{
  gsl_matrix_complex *mat = create_complex_matrix(nrows, ncols);
  if (!mat) return 0;
  double *data = mat->data;
  size_t tda = mat->tda;
  pure_new_vect(n, xs);
  for (size_t count = 0, i = 0; count < n; count++) {
    pure_expr *x = xs[count];
    switch (x->tag) {
    case EXPR::INT:
      data[2*i] = (double)x->data.i;
      data[2*i+1] = 0.0;
      i++;
      break;
    case EXPR::BIGINT:
      data[2*i] = mpz_get_d(x->data.z);
      data[2*i+1] = 0.0;
      i++;
      break;
    case EXPR::DBL:
      data[2*i] = x->data.d;
      data[2*i+1] = 0.0;
      i++;
      break;
    case EXPR::APP: {
      double a, b;
      if (get_complex(x, a, b)) {
	data[2*i] = a;
	data[2*i+1] = b;
	i++;
      } else {
	assert(0 && "bad matrix element");
      }
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *mat1 = (gsl_matrix*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; j++)
	  for (size_t k = 0; k < mat1->size2; k++) {
	    data[2*(j*tda+k+i)] = mat1->data[j*mat1->tda+k];
	    data[2*(j*tda+k+i)+1] = 0.0;
	  }
      i += mat1->size2;
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mat1 = (gsl_matrix_int*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; j++)
	  for (size_t k = 0; k < mat1->size2; k++) {
	    data[2*(j*tda+k+i)] = (double)mat1->data[j*mat1->tda+k];
	    data[2*(j*tda+k+i)+1] = 0.0;
	  }
      i += mat1->size2;
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *mat1 = (gsl_matrix_complex*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; j++)
	  memcpy(data+2*(j*tda+i), mat1->data+2*j*mat1->tda,
		 mat1->size2*2*sizeof(double));
      i += mat1->size2;
      break;
    }
    case EXPR::MATRIX:
      // empty matrix, skip
      break;
    default:
      assert(0 && "bad matrix element");
      break;
    }
  }
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  return pure_complex_matrix(mat);
}

static pure_expr*
int_matrix_rows(size_t nrows, size_t ncols, size_t n, pure_expr **xs)
{
  gsl_matrix_int *mat = create_int_matrix(nrows, ncols);
  if (!mat) return 0;
  int *data = mat->data;
  size_t tda = mat->tda;
  pure_new_vect(n, xs);
  for (size_t count = 0, i = 0; count < n; count++) {
    pure_expr *x = xs[count];
    switch (x->tag) {
    case EXPR::INT:
      data[i++*tda] = x->data.i;
      break;
    case EXPR::BIGINT:
      data[i++*tda] = pure_get_int(x);
      break;
    case EXPR::DBL:
      data[i++*tda] = (int)x->data.d;
      break;
    case EXPR::DMATRIX: {
      gsl_matrix *mat1 = (gsl_matrix*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; i++, j++)
	  for (size_t k = 0; k < mat1->size2; k++)
	    data[i*tda+k] = (int)mat1->data[j*mat1->tda+k];
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mat1 = (gsl_matrix_int*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; i++, j++)
	  memcpy(data+i*tda, mat1->data+j*mat1->tda, ncols*sizeof(int));
      break;
    }
    case EXPR::CMATRIX:
    case EXPR::MATRIX:
      // empty matrix, skip
      break;
    default:
      assert(0 && "bad matrix element");
      break;
    }
  }
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  return pure_int_matrix(mat);
}

static pure_expr*
int_matrix_columns(size_t nrows, size_t ncols, size_t n, pure_expr **xs)
{
  gsl_matrix_int *mat = create_int_matrix(nrows, ncols);
  if (!mat) return 0;
  int *data = mat->data;
  size_t tda = mat->tda;
  pure_new_vect(n, xs);
  for (size_t count = 0, i = 0; count < n; count++) {
    pure_expr *x = xs[count];
    switch (x->tag) {
    case EXPR::INT:
      data[i++] = x->data.i;
      break;
    case EXPR::BIGINT:
      data[i++] = pure_get_int(x);
      break;
    case EXPR::DBL:
      data[i++] = (int)x->data.d;
      break;
    case EXPR::DMATRIX: {
      gsl_matrix *mat1 = (gsl_matrix*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; j++)
	  for (size_t k = 0; k < mat1->size2; k++)
	    data[j*tda+k+i] = (int)mat1->data[j*mat1->tda+k];
      i += mat1->size2;
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mat1 = (gsl_matrix_int*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; j++)
	  memcpy(data+j*tda+i, mat1->data+j*mat1->tda,
		 mat1->size2*sizeof(int));
      i += mat1->size2;
      break;
    }
    case EXPR::CMATRIX:
    case EXPR::MATRIX:
      // empty matrix, skip
      break;
    default:
      assert(0 && "bad matrix element");
      break;
    }
  }
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  return pure_int_matrix(mat);
}

static pure_expr*
symbolic_matrix_rows(size_t nrows, size_t ncols, size_t n, pure_expr **xs)
{
  gsl_matrix_symbolic *mat = create_symbolic_matrix(nrows, ncols);
  if (!mat) return 0;
  pure_expr **data = mat->data;
  size_t tda = mat->tda;
  pure_new_vect(n, xs);
  for (size_t count = 0, i = 0; count < n; count++) {
    pure_expr *x = xs[count];
    switch (x->tag) {
    case EXPR::MATRIX: {
      gsl_matrix_symbolic *mat1 = (gsl_matrix_symbolic*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; i++, j++)
	  memcpy(data+i*tda, mat1->data+j*mat1->tda, ncols*sizeof(pure_expr*));
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *mat1 = (gsl_matrix*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; i++, j++)
	  for (size_t k = 0; k < mat1->size2; k++)
	    data[i*tda+k] = pure_double(mat1->data[j*mat1->tda+k]);
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mat1 = (gsl_matrix_int*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; i++, j++)
	  for (size_t k = 0; k < mat1->size2; k++)
	    data[i*tda+k] = pure_int(mat1->data[j*mat1->tda+k]);
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *mat1 = (gsl_matrix_complex*)x->data.mat.p;
      if (mat1) {
	interpreter& interp = *interpreter::g_interp;
	symbol& rect = interp.symtab.complex_rect_sym();
	for (size_t j = 0; j < mat1->size1; i++, j++)
	  for (size_t k = 0; k < mat1->size2; k++) {
	    size_t l = 2*(j*mat1->tda+k);
	    data[i*tda+k] =
	      make_complex2(rect, mat1->data[l], mat1->data[l+1]);
	  }
      }
      break;
    }
    default:
      data[i++*tda] = x;
      break;
    }
  }
  pure_expr *ret = pure_symbolic_matrix(mat);
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  return ret;
}

static pure_expr*
symbolic_matrix_columns(size_t nrows, size_t ncols, size_t n, pure_expr **xs)
{
  gsl_matrix_symbolic *mat = create_symbolic_matrix(nrows, ncols);
  if (!mat) return 0;
  pure_expr **data = mat->data;
  size_t tda = mat->tda;
  pure_new_vect(n, xs);
  for (size_t count = 0, i = 0; count < n; count++) {
    pure_expr *x = xs[count];
    switch (x->tag) {
    case EXPR::MATRIX: {
      gsl_matrix_symbolic *mat1 = (gsl_matrix_symbolic*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; j++)
	  memcpy(data+j*tda+i, mat1->data+j*mat1->tda,
		 mat1->size2*sizeof(pure_expr*));
      i += mat1->size2;
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *mat1 = (gsl_matrix*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; j++)
	  for (size_t k = 0; k < mat1->size2; k++)
	    data[j*tda+k+i] = pure_double(mat1->data[j*mat1->tda+k]);
      i += mat1->size2;
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mat1 = (gsl_matrix_int*)x->data.mat.p;
      if (mat1)
	for (size_t j = 0; j < mat1->size1; j++)
	  for (size_t k = 0; k < mat1->size2; k++)
	    data[j*tda+k+i] = pure_int(mat1->data[j*mat1->tda+k]);
      i += mat1->size2;
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *mat1 = (gsl_matrix_complex*)x->data.mat.p;
      if (mat1) {
	interpreter& interp = *interpreter::g_interp;
	symbol& rect = interp.symtab.complex_rect_sym();
	for (size_t j = 0; j < mat1->size1; j++)
	  for (size_t k = 0; k < mat1->size2; k++) {
	    size_t l = 2*(j*mat1->tda+k);
	    data[j*tda+k+i] =
	      make_complex2(rect, mat1->data[l], mat1->data[l+1]);
	  }
      }
      i += mat1->size2;
      break;
    }
    default:
      data[i++] = x;
      break;
    }
  }
  pure_expr *ret = pure_symbolic_matrix(mat);
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  return ret;
}

static inline void set_target_type(int32_t& target, int32_t t)
{
  if (target == 0)
    target = t;
  else if (target != t)
    target = EXPR::MATRIX;
}

extern "C"
pure_expr *pure_matrix_rowsl(uint32_t n, ...)
{
  va_list ap;
  pure_expr **xs = (pure_expr**)alloca(n*sizeof(pure_expr*));
  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    xs[i] = va_arg(ap, pure_expr*);
  va_end(ap);
  return pure_matrix_rowsv(n, xs);
}

extern "C"
pure_expr *pure_matrix_rowsv(uint32_t n, pure_expr **xs)
{
  int k = -1;
  size_t nrows = 0, ncols = 0;
  int32_t target = 0;
  bool have_matrix = false;
  for (size_t i = 0; i < n; i++) {
    pure_expr *x = xs[i];
    switch (x->tag) {
    case EXPR::MATRIX: {
      gsl_matrix_symbolic *mp = (gsl_matrix_symbolic*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  return 0;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::MATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::DBL:
      set_target_type(target, EXPR::DMATRIX);
      if (k >= 0 && k != 1) return 0;
      nrows++; k = 1;
      break;
    case EXPR::INT:
      set_target_type(target, EXPR::IMATRIX);
      if (k >= 0 && k != 1) return 0;
      nrows++; k = 1;
      break;
    case EXPR::APP: {
      double a, b;
      if (k >= 0 && k != 1) return 0;
      nrows++; k = 1;
      if (get_complex(x, a, b))
	set_target_type(target, EXPR::CMATRIX);
      else
	set_target_type(target, EXPR::MATRIX);
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *mp = (gsl_matrix*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  return 0;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::DMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *mp = (gsl_matrix_complex*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  return 0;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::CMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mp = (gsl_matrix_int*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  return 0;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::IMATRIX);
	have_matrix = true;
      }
      break;
    }
    default:
      if (k >= 0 && k != 1) return 0;
      nrows++; k = 1;
      set_target_type(target, EXPR::MATRIX);
      break;
    }
  }
  if (n == 1 && have_matrix) return xs[0];
  if (k < 0) k = 0;
  ncols = k;
  if (target == 0) target = EXPR::MATRIX;
  switch (target) {
  case EXPR::MATRIX:
    return symbolic_matrix_rows(nrows, ncols, n, xs);
  case EXPR::DMATRIX:
    return double_matrix_rows(nrows, ncols, n, xs);
  case EXPR::CMATRIX:
    return complex_matrix_rows(nrows, ncols, n, xs);
  case EXPR::IMATRIX:
    return int_matrix_rows(nrows, ncols, n, xs);
  default:
    return 0;
  }
}

extern "C"
pure_expr *pure_matrix_columnsl(uint32_t n, ...)
{
  va_list ap;
  pure_expr **xs = (pure_expr**)alloca(n*sizeof(pure_expr*));
  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    xs[i] = va_arg(ap, pure_expr*);
  va_end(ap);
  return pure_matrix_columnsv(n, xs);
}

extern "C"
pure_expr *pure_matrix_columnsv(uint32_t n, pure_expr **xs)
{
  int k = -1;
  size_t nrows = 0, ncols = 0;
  int32_t target = 0;
  bool have_matrix = false;
  for (size_t i = 0; i < n; i++) {
    pure_expr *x = xs[i];
    switch (x->tag) {
    case EXPR::MATRIX: {
      gsl_matrix_symbolic *mp = (gsl_matrix_symbolic*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  return 0;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::MATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::DBL:
      set_target_type(target, EXPR::DMATRIX);
      if (k >= 0 && k != 1) return 0;
      ncols++; k = 1;
      break;
    case EXPR::INT:
      set_target_type(target, EXPR::IMATRIX);
      if (k >= 0 && k != 1) return 0;
      ncols++; k = 1;
      break;
    case EXPR::APP: {
      double a, b;
      if (k >= 0 && k != 1) return 0;
      ncols++; k = 1;
      if (get_complex(x, a, b))
	set_target_type(target, EXPR::CMATRIX);
      else
	set_target_type(target, EXPR::MATRIX);
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *mp = (gsl_matrix*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  return 0;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::DMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *mp = (gsl_matrix_complex*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  return 0;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::CMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mp = (gsl_matrix_int*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  return 0;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::IMATRIX);
	have_matrix = true;
      }
      break;
    }
    default:
      if (k >= 0 && k != 1) return 0;
      ncols++; k = 1;
      set_target_type(target, EXPR::MATRIX);
      break;
    }
  }
  if (n == 1 && have_matrix) return xs[0];
  if (k < 0) k = 0;
  nrows = k;
  if (target == 0) target = EXPR::MATRIX;
  switch (target) {
  case EXPR::MATRIX:
    return symbolic_matrix_columns(nrows, ncols, n, xs);
  case EXPR::DMATRIX:
    return double_matrix_columns(nrows, ncols, n, xs);
  case EXPR::CMATRIX:
    return complex_matrix_columns(nrows, ncols, n, xs);
  case EXPR::IMATRIX:
    return int_matrix_columns(nrows, ncols, n, xs);
  default:
    return 0;
  }
}

static uint32_t pure_push_argv(uint32_t n, uint32_t m, pure_expr **args);

static pure_expr *myfuncall(void *f, uint32_t n, pure_expr **argv)
{
  pure_expr *ret;
  funcall(ret, f, n, argv)
  return ret;
}

extern "C"
pure_expr *pure_funcall(void *f, uint32_t n, ...)
{
  va_list ap;
  pure_expr **argv = (pure_expr**)alloca((n+1)*sizeof(pure_expr*));
  argv[n] = 0;
  va_start(ap, n);
  for (uint32_t i = 0; i < n; i++) {
    pure_expr *x = va_arg(ap, pure_expr*);
    argv[i] = x;
  };
  va_end(ap);
  if (n>0 || !interpreter::g_interp->debugging) {
    if (n == 1)
      pure_push_arg(argv[0]);
    else
      pure_push_argv(n, 0, argv);
  }
  return myfuncall(f, n, argv);
}

extern "C"
pure_expr *pure_funcallx(void *f, pure_expr **_e, uint32_t n, ...)
{
  va_list ap;
  pure_expr **argv = (pure_expr**)alloca((n+1)*sizeof(pure_expr*));
  argv[n] = 0;
  va_start(ap, n);
  for (uint32_t i = 0; i < n; i++) {
    pure_expr *x = va_arg(ap, pure_expr*);
    argv[i] = x;
  };
  va_end(ap);
  if (n>0 || !interpreter::g_interp->debugging) {
    if (n == 1)
      pure_push_arg(argv[0]);
    else
      pure_push_argv(n, 0, argv);
  }
  pure_try_call(myfuncall(f, n, argv));
}

extern "C"
pure_expr *pure_app(pure_expr *fun, pure_expr *arg)
{
  return pure_apply2(fun, arg);
}

extern "C"
pure_expr *pure_appl(pure_expr *fun, size_t argc, ...)
{
  if (argc == 0) return fun;
  va_list ap;
  va_start(ap, argc);
  pure_expr **args = (pure_expr**)alloca(argc*sizeof(pure_expr*));
  for (size_t i = 0; i < argc; i++)
    args[i] = va_arg(ap, pure_expr*);
  return pure_appv(fun, argc, args);
}

extern "C"
pure_expr *pure_appv(pure_expr *fun, size_t argc, pure_expr **args)
{
  pure_expr *y = fun;
  for (size_t i = 0; i < argc; i++)
    y = pure_apply2(y, args[i]);
  return y;
}

extern "C"
pure_expr *pure_appx(pure_expr *fun, pure_expr *arg, pure_expr **_e)
{
  pure_new_args(2, fun, arg);
  pure_try_call(pure_apply(fun, arg));
}

extern "C"
pure_expr *pure_appxl(pure_expr *fun, pure_expr **e, size_t argc, ...)
{
  if (argc == 0) return fun;
  va_list ap;
  va_start(ap, argc);
  pure_expr **args = (pure_expr**)alloca(argc*sizeof(pure_expr*));
  for (size_t i = 0; i < argc; i++)
    args[i] = va_arg(ap, pure_expr*);
  return pure_appxv(fun, argc, args, e);
}

extern "C"
pure_expr *pure_appxv(pure_expr *fun, size_t argc, pure_expr **args,
		      pure_expr **e)
{
  pure_expr *y = fun;
  *e = NULL;
  for (size_t i = 0; i < argc; i++)
    if (!(y = pure_appx(y, args[i], e))) break;
  return y;
}

static inline pure_expr *mk_nil()
{
  interpreter& interp = *interpreter::g_interp;
  return pure_symbol(interp.symtab.nil_sym().f);
}

static inline pure_expr *mk_void()
{
  interpreter& interp = *interpreter::g_interp;
  return pure_symbol(interp.symtab.void_sym().f);
}

static inline pure_expr *mk_cons(pure_expr *f, pure_expr *x, pure_expr *y)
{
  return pure_apply2(pure_apply2(f, x), y);
}

extern "C"
pure_expr *pure_listl(size_t size, ...)
{
  if (size == 0) return mk_nil();
  va_list ap;
  va_start(ap, size);
  pure_expr **elems = (pure_expr**)alloca(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    elems[i] = va_arg(ap, pure_expr*);
  return pure_listv(size, elems);
}

extern "C"
pure_expr *pure_listv(size_t size, pure_expr **elems)
{
  interpreter& interp = *interpreter::g_interp;
  pure_expr *f = pure_symbol(interp.symtab.cons_sym().f);
  pure_expr *y = mk_nil();
  for (size_t i = size; i-- > 0; )
    y = mk_cons(f, elems[i], y);
  return y;
}

extern "C"
pure_expr *pure_listv2(size_t size, pure_expr **elems, pure_expr *tail)
{
  interpreter& interp = *interpreter::g_interp;
  pure_expr *f = pure_symbol(interp.symtab.cons_sym().f);
  pure_expr *y = tail;
  for (size_t i = size; i-- > 0; )
    y = mk_cons(f, elems[i], y);
  return y;
}

extern "C"
pure_expr *pure_tuplel(size_t size, ...)
{
  if (size == 0) return mk_void();
  va_list ap;
  va_start(ap, size);
  pure_expr **elems = (pure_expr**)alloca(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    elems[i] = va_arg(ap, pure_expr*);
  return pure_tuplev(size, elems);
}

extern "C"
pure_expr *pure_tuplev(size_t size, pure_expr **elems)
{
  if (size == 0) return mk_void();
  interpreter& interp = *interpreter::g_interp;
  pure_expr *f = pure_symbol(interp.symtab.pair_sym().f);
  pure_expr *y = elems[--size];
  for (size_t i = size; i-- > 0; )
    y = mk_cons(f, elems[i], y);
  return y;
}

static inline pure_expr *mk_nilq()
{
  interpreter& interp = *interpreter::g_interp;
  return pure_const(interp.symtab.nil_sym().f);
}

static inline pure_expr *mk_voidq()
{
  interpreter& interp = *interpreter::g_interp;
  return pure_const(interp.symtab.void_sym().f);
}

static inline pure_expr *mk_consq(pure_expr *f, pure_expr *x, pure_expr *y)
{
  return pure_applc2(pure_applc2(f, x), y);
}

extern "C"
pure_expr *pure_listlq(size_t size, ...)
{
  if (size == 0) return mk_nilq();
  va_list ap;
  va_start(ap, size);
  pure_expr **elems = (pure_expr**)alloca(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    elems[i] = va_arg(ap, pure_expr*);
  return pure_listvq(size, elems);
}

extern "C"
pure_expr *pure_listvq(size_t size, pure_expr **elems)
{
  interpreter& interp = *interpreter::g_interp;
  pure_expr *f = pure_const(interp.symtab.cons_sym().f);
  pure_expr *y = mk_nilq();
  for (size_t i = size; i-- > 0; )
    y = mk_consq(f, elems[i], y);
  return y;
}

extern "C"
pure_expr *pure_listv2q(size_t size, pure_expr **elems, pure_expr *tail)
{
  interpreter& interp = *interpreter::g_interp;
  pure_expr *f = pure_const(interp.symtab.cons_sym().f);
  pure_expr *y = tail;
  for (size_t i = size; i-- > 0; )
    y = mk_consq(f, elems[i], y);
  return y;
}

extern "C"
pure_expr *pure_tuplelq(size_t size, ...)
{
  if (size == 0) return mk_voidq();
  va_list ap;
  va_start(ap, size);
  pure_expr **elems = (pure_expr**)alloca(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    elems[i] = va_arg(ap, pure_expr*);
  return pure_tuplevq(size, elems);
}

extern "C"
pure_expr *pure_tuplevq(size_t size, pure_expr **elems)
{
  if (size == 0) return mk_voidq();
  interpreter& interp = *interpreter::g_interp;
  pure_expr *f = pure_const(interp.symtab.pair_sym().f);
  pure_expr *y = elems[--size];
  for (size_t i = size; i-- > 0; )
    y = mk_consq(f, elems[i], y);
  return y;
}

pure_expr *pure_intlistv(size_t size, int32_t *elems)
{
  if (size == 0) return mk_nil();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_int(elems[i]);
  pure_expr *res = pure_listv(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_intlistv2(size_t size, int32_t *elems, pure_expr *tail)
{
  if (size == 0) return tail;
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_int(elems[i]);
  pure_expr *res = pure_listv2(size, myelems, tail);
  free(myelems);
  return res;
}

pure_expr *pure_inttuplev(size_t size, int32_t *elems)
{
  if (size == 0) return mk_void();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_int(elems[i]);
  pure_expr *res = pure_tuplev(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_intlistvq(size_t size, int32_t *elems)
{
  if (size == 0) return mk_nil();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_int(elems[i]);
  pure_expr *res = pure_listvq(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_intlistv2q(size_t size, int32_t *elems, pure_expr *tail)
{
  if (size == 0) return tail;
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_int(elems[i]);
  pure_expr *res = pure_listv2q(size, myelems, tail);
  free(myelems);
  return res;
}

pure_expr *pure_inttuplevq(size_t size, int32_t *elems)
{
  if (size == 0) return mk_void();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_int(elems[i]);
  pure_expr *res = pure_tuplevq(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_doublelistv(size_t size, double *elems)
{
  if (size == 0) return mk_nil();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_double(elems[i]);
  pure_expr *res = pure_listv(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_doublelistv2(size_t size, double *elems, pure_expr *tail)
{
  if (size == 0) return tail;
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_double(elems[i]);
  pure_expr *res = pure_listv2(size, myelems, tail);
  free(myelems);
  return res;
}

pure_expr *pure_doubletuplev(size_t size, double *elems)
{
  if (size == 0) return mk_void();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_double(elems[i]);
  pure_expr *res = pure_tuplev(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_doublelistvq(size_t size, double *elems)
{
  if (size == 0) return mk_nil();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_double(elems[i]);
  pure_expr *res = pure_listvq(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_doublelistv2q(size_t size, double *elems, pure_expr *tail)
{
  if (size == 0) return tail;
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_double(elems[i]);
  pure_expr *res = pure_listv2q(size, myelems, tail);
  free(myelems);
  return res;
}

pure_expr *pure_doubletuplevq(size_t size, double *elems)
{
  if (size == 0) return mk_void();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_double(elems[i]);
  pure_expr *res = pure_tuplevq(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_bigintlistv(size_t size, limb_t *limbs,
			    uint32_t *offs, int32_t *sz)
{
  if (size == 0) return mk_nil();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_bigint(sz[i], limbs+offs[i]);
  pure_expr *res = pure_listv(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_bigintlistv2(size_t size, limb_t *limbs,
			     uint32_t *offs, int32_t *sz, pure_expr *tail)
{
  if (size == 0) return tail;
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_bigint(sz[i], limbs+offs[i]);
  pure_expr *res = pure_listv2(size, myelems, tail);
  free(myelems);
  return res;
}

pure_expr *pure_biginttuplev(size_t size, limb_t *limbs,
			     uint32_t *offs, int32_t *sz)
{
  if (size == 0) return mk_void();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_bigint(sz[i], limbs+offs[i]);
  pure_expr *res = pure_tuplev(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_bigintlistvq(size_t size, limb_t *limbs,
			     uint32_t *offs, int32_t *sz)
{
  if (size == 0) return mk_nil();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_bigint(sz[i], limbs+offs[i]);
  pure_expr *res = pure_listvq(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_bigintlistv2q(size_t size, limb_t *limbs,
			      uint32_t *offs, int32_t *sz, pure_expr *tail)
{
  if (size == 0) return tail;
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_bigint(sz[i], limbs+offs[i]);
  pure_expr *res = pure_listv2q(size, myelems, tail);
  free(myelems);
  return res;
}

pure_expr *pure_biginttuplevq(size_t size, limb_t *limbs,
			      uint32_t *offs, int32_t *sz)
{
  if (size == 0) return mk_void();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_bigint(sz[i], limbs+offs[i]);
  pure_expr *res = pure_tuplevq(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_bigintmatrixv(size_t nrows, size_t ncols, limb_t *limbs,
			      uint32_t *offs, int32_t *sz)
{
  gsl_matrix_symbolic *mat = create_symbolic_matrix(nrows, ncols);
  if (!mat) return 0;
  pure_expr **data = mat->data;
  size_t tda = mat->tda, k = 0;
  for (size_t i = 0; i < nrows; i++)
    for (size_t j = 0; j < ncols; j++) {
      data[i*tda+j] = pure_bigint(sz[k], limbs+offs[k]);
      k++;
    }
  return pure_symbolic_matrix(mat);
}

pure_expr *pure_strlistv(size_t size, char *chars, uint32_t *offs)
{
  if (size == 0) return mk_nil();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_string_dup(chars+offs[i]);
  pure_expr *res = pure_listv(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_strlistv2(size_t size, char *chars, uint32_t *offs,
			  pure_expr *tail)
{
  if (size == 0) return tail;
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_string_dup(chars+offs[i]);
  pure_expr *res = pure_listv2(size, myelems, tail);
  free(myelems);
  return res;
}

pure_expr *pure_strtuplev(size_t size, char *chars, uint32_t *offs)
{
  if (size == 0) return mk_void();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_string_dup(chars+offs[i]);
  pure_expr *res = pure_tuplev(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_strlistvq(size_t size, char *chars, uint32_t *offs)
{
  if (size == 0) return mk_nil();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_string_dup(chars+offs[i]);
  pure_expr *res = pure_listvq(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_strlistv2q(size_t size, char *chars, uint32_t *offs,
			   pure_expr *tail)
{
  if (size == 0) return tail;
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_string_dup(chars+offs[i]);
  pure_expr *res = pure_listv2q(size, myelems, tail);
  free(myelems);
  return res;
}

pure_expr *pure_strtuplevq(size_t size, char *chars, uint32_t *offs)
{
  if (size == 0) return mk_void();
  pure_expr **myelems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  for (size_t i = 0; i < size; i++)
    myelems[i] = pure_string_dup(chars+offs[i]);
  pure_expr *res = pure_tuplevq(size, myelems);
  free(myelems);
  return res;
}

pure_expr *pure_strmatrixv(size_t nrows, size_t ncols, char *chars,
			   uint32_t *offs)
{
  gsl_matrix_symbolic *mat = create_symbolic_matrix(nrows, ncols);
  if (!mat) return 0;
  pure_expr **data = mat->data;
  size_t tda = mat->tda, k = 0;
  for (size_t i = 0; i < nrows; i++)
    for (size_t j = 0; j < ncols; j++) {
      data[i*tda+j] = pure_string_dup(chars+offs[k++]);
    }
  return pure_symbolic_matrix(mat);
}

extern "C"
bool pure_is_symbol(const pure_expr *x, int32_t *sym)
{
  assert(x);
  if (x->tag >= 0) {
    if (sym) *sym = x->tag;
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_int(const pure_expr *x, int32_t *i)
{
  assert(x);
  if (x->tag == EXPR::INT) {
    if (i) *i = x->data.i;
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_mpz(const pure_expr *x, mpz_t *z)
{
  assert(x);
  if (x->tag == EXPR::BIGINT) {
    if (z) mpz_init_set(*z, x->data.z);
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_double(const pure_expr *x, double *d)
{
  assert(x);
  if (x->tag == EXPR::DBL) {
    if (d) *d = x->data.d;
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_pointer(const pure_expr *x, void **p)
{
  assert(x);
  if (x->tag == EXPR::PTR) {
    if (p) *p = x->data.p;
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_string(const pure_expr *x, const char **s)
{
  assert(x);
  if (x->tag == EXPR::STR) {
    if (s) *s = x->data.s;
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_string_dup(const pure_expr *x, char **s)
{
  assert(x);
  if (x->tag == EXPR::STR) {
    if (s) *s = strdup(x->data.s);
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_cstring_dup(const pure_expr *x, char **s)
{
  assert(x);
  if (x->tag == EXPR::STR) {
    if (s) *s = fromutf8(x->data.s);
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_symbolic_matrix(const pure_expr *x, void **p)
{
  if (x->tag == EXPR::MATRIX) {
    *p = x->data.mat.p;
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_double_matrix(const pure_expr *x, void **p)
{
  if (x->tag == EXPR::DMATRIX) {
    *p = x->data.mat.p;
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_complex_matrix(const pure_expr *x, void **p)
{
  if (x->tag == EXPR::CMATRIX) {
    *p = x->data.mat.p;
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_int_matrix(const pure_expr *x, void **p)
{
  if (x->tag == EXPR::IMATRIX) {
    *p = x->data.mat.p;
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_app(const pure_expr *x, pure_expr **fun, pure_expr **arg)
{
  assert(x);
  if (x->tag == EXPR::APP) {
    if (fun) *fun = x->data.x[0];
    if (arg) *arg = x->data.x[1];
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_appv(pure_expr *x, pure_expr **_fun,
		  size_t *_argc, pure_expr ***_args)
{
  assert(x);
  pure_expr *u = x, *y, *z;
  size_t argc = 0;
  while (pure_is_app(u, &y, &z)) {
    argc++;
    u = y;
  }
  if (_fun) *_fun = u;
  if (_argc) *_argc = argc;
  if (_args) {
    if (argc > 0) {
      pure_expr **args = (pure_expr**)malloc(argc*sizeof(pure_expr*));
      assert(args);
      size_t i = argc;
      u = x;
      while (pure_is_app(u, &y, &z)) {
	args[--i] = z;
	u = y;
      }
      *_args = args;
    } else
      *_args = 0;
  }
  return true;
}

static inline bool is_nil(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  return x->tag == interp.symtab.nil_sym().f;
}

static inline bool is_cons(pure_expr *x, pure_expr*& y, pure_expr*& z)
{
  interpreter& interp = *interpreter::g_interp;
  if (x->tag == EXPR::APP && x->data.x[0]->tag == EXPR::APP &&
      x->data.x[0]->data.x[0]->tag == interp.symtab.cons_sym().f) {
    y = x->data.x[0]->data.x[1];
    z = x->data.x[1];
    return true;
  } else
    return false;
}

static inline bool is_void(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  return x->tag == interp.symtab.void_sym().f;
}

static inline bool is_pair(pure_expr *x, pure_expr*& y, pure_expr*& z)
{
  interpreter& interp = *interpreter::g_interp;
  if (x->tag == EXPR::APP && x->data.x[0]->tag == EXPR::APP &&
      x->data.x[0]->data.x[0]->tag == interp.symtab.pair_sym().f) {
    y = x->data.x[0]->data.x[1];
    z = x->data.x[1];
    return true;
  } else
    return false;
}

extern "C"
bool pure_is_listv(pure_expr *x, size_t *_size, pure_expr ***_elems)
{
  assert(x);
  pure_expr *u = x, *y, *z;
  size_t size = 0;
  while (is_cons(u, y, z)) {
    size++;
    u = z;
  }
  if (!is_nil(u)) return false;
  if (_size) *_size = size;
  if (_elems) {
    if (size > 0) {
      pure_expr **elems = (pure_expr**)malloc(size*sizeof(pure_expr*));
      assert(elems);
      size_t i = 0;
      u = x;
      while (is_cons(u, y, z)) {
	elems[i++] = y;
	u = z;
      }
      *_elems = elems;
    } else
      *_elems = 0;
  }
  return true;
}

extern "C"
bool pure_is_tuplev(pure_expr *x, size_t *_size, pure_expr ***_elems)
{
  assert(x);
  /* FIXME: This implementation assumes that tuples are right-recursive. If we
     change the tuple implementation in the prelude then this code has to be
     adapted accordingly. */
  pure_expr *u = x, *y, *z;
  size_t size = 1;
  if (::is_void(x)) {
    if (_size) *_size = 0;
    if (_elems) *_elems = 0;
    return true;
  }
  while (is_pair(u, y, z)) {
    size++;
    u = z;
  }
  if (_size) *_size = size;
  if (_elems) {
    pure_expr **elems = (pure_expr**)malloc(size*sizeof(pure_expr*));
    assert(elems);
    size_t i = 0;
    u = x;
    while (is_pair(u, y, z)) {
      elems[i++] = y;
      u = z;
    }
    elems[i++] = u;
    *_elems = elems;
  }
  return true;
}

extern "C"
pure_expr *pure_complex(double c[2])
{
  return make_complex(c[0], c[1]);
}

extern "C"
bool pure_is_complex(pure_expr *x, double *c)
{
  double a, b;
  if (get_complex(x, a, b)) {
    if (c) {
      c[0] = a;
      c[1] = b;
    }
    return true;
  } else
    return false;
}

extern "C"
pure_expr *pure_rationalz(const mpz_t z[2])
{
  interpreter& interp = *interpreter::g_interp;
  symbol &xdiv = interp.symtab.rational_xdiv_sym();
  return pure_appl(pure_symbol(xdiv.f), 2, pure_mpz(z[0]), pure_mpz(z[1]));
}

extern "C"
bool pure_is_rationalz(const pure_expr *x, mpz_t *z)
{
  assert(x);
  if (x->tag != EXPR::APP) return false;
  pure_expr *u = x->data.x[0], *v = x->data.x[1];
  if (u->tag == EXPR::APP) {
    interpreter& interp = *interpreter::g_interp;
    pure_expr *f = u->data.x[0];
    symbol &xdiv = interp.symtab.rational_xdiv_sym();
    if (f->tag != xdiv.f)
      return false;
    u = u->data.x[1];
    if (u->tag == EXPR::BIGINT && v->tag == EXPR::BIGINT)
      return pure_is_mpz(u, z) && pure_is_mpz(v, z+1);
    else
      return false;
  } else
    return false;
}

extern "C"
pure_expr *pure_new(pure_expr *x)
{
  return pure_new_internal(x);
}

extern "C"
void pure_free(pure_expr *x)
{
  pure_free_internal(x);
}

extern "C"
void pure_freenew(pure_expr *x)
{
  if (x->refc == 0)
    pure_free_internal(pure_new_internal(x));
}

extern "C"
void pure_ref(pure_expr *x)
{
  x->refc++;
}

extern "C"
void pure_unref(pure_expr *x)
{
  pure_unref_internal(x);
}

extern "C"
pure_expr *pure_sentry(pure_expr *sentry, pure_expr *x)
{
  if (x==0)
    return 0;
  else {
    if (x->sy)
      pure_free_internal(x->sy);
    x->sy = sentry?pure_new_internal(sentry):0;
    return x;
  }
}

extern "C"
pure_expr *pure_get_sentry(pure_expr *x)
{
  return get_sentry(x);
}

extern "C"
bool pure_has_sentry(pure_expr *x)
{
  return get_sentry(x) != 0;
}

extern "C"
pure_expr *pure_clear_sentry(pure_expr *x)
{
  return pure_sentry(0, x);
}

extern "C"
pure_expr *pure_tag(int tag, pure_expr *x)
{
  if (pure_is_pointer(x, 0)) {
#if SIZEOF_VOID_P==8
    x->data.x[1] = (pure_expr*)(int64_t)tag;
#else
    x->data.x[1] = (pure_expr*)tag;
#endif
    return x;
  } else
    return 0;
}

extern "C"
int pure_get_tag(const pure_expr *x)
{
  if (pure_is_pointer(x, 0)) {
#if SIZEOF_VOID_P==8
    int tag = (int)(int64_t)x->data.x[1];
#else
    int tag = (int)x->data.x[1];
#endif
    return tag;
  } else
    return 0;
}

extern "C"
bool pure_check_tag(int tag, const pure_expr *x)
{
  void *p;
  if (pure_is_pointer(x, &p)) {
#if SIZEOF_VOID_P==8
    int mytag = (int)(int64_t)x->data.x[1];
#else
    int mytag = (int)x->data.x[1];
#endif
    /* (void*)NULL matches any pointer type, like it is in C. Otherwise the
       type tags must match literally. */
    return mytag==tag || (p==NULL && mytag==0);
  } else
    return tag==0;
}

extern "C"
int pure_make_tag()
{
  interpreter& interp = *interpreter::g_interp;
  return ++interp.last_tag;
}

extern "C"
void pure_add_rtti(const char *name, int tag)
{
  interpreter& interp = *interpreter::g_interp;
  map<string,int>::iterator it = interp.pointer_tags.find(name);
  if (it == interp.pointer_tags.end()) {
    interp.pointer_tags[name] = tag;
    it = interp.pointer_tags.find(name);
    if (it != interp.pointer_tags.end())
      interp.pointer_type_with_tag[tag] = it;
  }
}

extern "C"
int pure_pointer_tag(const char *s)
{
  if (!s) return 0;
  interpreter& interp = *interpreter::g_interp;
  string name = s;
  if (name.find_first_of(" \t\n\r\f\v") != string::npos) {
    // We allow whitespace in the type name, remove it here.
    name.clear();
    for (; *s; ++s) if (!isspace(*s)) name.append(1, *s);
  }
  // If the type is valid Pure syntax, normalize it a bit.
  try {
    llvm_const_Type *ty = interp.named_type(name);
    assert(ty);
    name = interp.type_name(ty);
  } catch (err &e) {
  }
  if (name == "void*") return 0; // generic pointer
  map<string,int>::iterator it = interp.pointer_tags.find(name);
  if (it != interp.pointer_tags.end())
    return it->second;
  else {
    // unknown type, create a new one on the fly
    int tag = pure_make_tag();
    pure_add_rtti(name.c_str(), tag);
    return tag;
  }
}

extern "C"
const char *pure_pointer_type(int tag)
{
  if (tag == 0) return "void*";
  interpreter& interp = *interpreter::g_interp;
  map<int,map<string,int>::iterator>::iterator it =
    interp.pointer_type_with_tag.find(tag);
  if (it != interp.pointer_type_with_tag.end())
    return it->second->first.c_str();
  else
    // unknown type
    return 0;
}

extern "C"
pure_expr *pure_pointer_cast(int tag, pure_expr *x)
{
  void *p;
  if (pure_is_pointer(x, &p)) {
    int old_tag = pure_get_tag(x);
    if (tag == old_tag)
      // nothing to do
      return x;
    else if (x->refc <= 1)
      // we can safely modify this object in-place
      return pure_tag(tag, x);
    else
      // create a copy of the pointer with the new tag on it
      return pure_tag(tag, pure_pointer(p));
  } else
    return 0;
}

extern "C"
void pure_pointer_add_equal(int tag, pure_equal_fun eq)
{
  interpreter& interp = *interpreter::g_interp;
  interp.pointer_type_info[tag].equal_cb = eq;
}

extern "C"
pure_equal_fun pure_pointer_equal(int tag)
{
  interpreter& interp = *interpreter::g_interp;
  map<int,pointer_type_extra_info>::iterator it =
    interp.pointer_type_info.find(tag);
  if (it != interp.pointer_type_info.end())
    return it->second.equal_cb;
  else
    return NULL;
}

extern "C"
void pure_pointer_add_hash(int tag, pure_hash_fun hash)
{
  interpreter& interp = *interpreter::g_interp;
  interp.pointer_type_info[tag].hash_cb = hash;
}

extern "C"
pure_hash_fun pure_pointer_hash(int tag)
{
  interpreter& interp = *interpreter::g_interp;
  map<int,pointer_type_extra_info>::iterator it =
    interp.pointer_type_info.find(tag);
  if (it != interp.pointer_type_info.end())
    return it->second.hash_cb;
  else
    return NULL;
}

extern "C"
void pure_pointer_add_printer(int tag, pure_printer_fun printer,
			      pure_printer_prec_fun prec)
{
  interpreter& interp = *interpreter::g_interp;
  interp.pointer_type_info[tag].printer_cb = printer;
  interp.pointer_type_info[tag].prec_cb = prec;
}

extern "C"
pure_printer_fun pure_pointer_printer(int tag)
{
  interpreter& interp = *interpreter::g_interp;
  map<int,pointer_type_extra_info>::iterator it =
    interp.pointer_type_info.find(tag);
  if (it != interp.pointer_type_info.end())
    return it->second.printer_cb;
  else
    return NULL;
}

extern "C"
pure_printer_prec_fun pure_pointer_printer_prec(int tag)
{
  interpreter& interp = *interpreter::g_interp;
  map<int,pointer_type_extra_info>::iterator it =
    interp.pointer_type_info.find(tag);
  if (it != interp.pointer_type_info.end())
    return it->second.prec_cb;
  else
    return NULL;
}

extern "C"
bool pure_let(int32_t sym, pure_expr *x)
{
  if (sym <= 0 || !x) return false;
  try {
    interpreter& interp = *interpreter::g_interp;
    interp.defn(sym, x);
    return true;
  } catch (err &e) {
    return false;
  }
}

extern "C"
bool pure_def(int32_t sym, pure_expr *x)
{
  if (sym <= 0 || !x) return false;
  try {
    interpreter& interp = *interpreter::g_interp;
    interp.const_defn(sym, x);
    return true;
  } catch (err &e) {
    return false;
  }
}

extern "C"
bool pure_clear(int32_t sym)
{
  if (sym > 0) {
    interpreter& interp = *interpreter::g_interp;
    interp.clear();
    return true;
  } else
    return false;
}

extern "C"
uint32_t pure_save()
{
  interpreter& interp = *interpreter::g_interp;
  if (interp.temp < 0xffffffffU)
    return ++interp.temp;
  else
    return 0;
}

extern "C"
uint32_t pure_restore()
{
  interpreter& interp = *interpreter::g_interp;
  uint32_t level = interp.temp;
  interp.clear();
  if (level > 0 && interp.temp > level-1) --interp.temp;
  return interp.temp;
}

extern "C"
uint32_t pure_savelevel()
{
  interpreter& interp = *interpreter::g_interp;
  return interp.temp;
}

extern "C"
pure_expr *pure_val(const char *s)
{
  assert(s);
  interpreter& interp = *interpreter::g_interp;
  interp.errmsg.clear(); interp.errpos.clear();
  pure_expr *res = interp.parsestr(string(s));
  interp.result = 0;
  if (res) {
    if (interp.errmsg.empty()) {
      pure_unref_internal(res);
      return res;
    } else {
      pure_free_internal(res);
      return 0;
    }
  } else if (interp.errmsg.empty())
    return mk_void();
  else
    return 0;
}

extern "C"
pure_expr *pure_eval(const char *s)
{
  assert(s);
  interpreter& interp = *interpreter::g_interp;
  interp.errmsg.clear(); interp.errpos.clear();
  string src = s;
  pure_expr *res = interp.runstr(src+";");
  interp.result = 0;
  // The extra ';' we placed at the end of the string may cause column
  // positions in the last line to extend past the end of the line, fix up
  // the error locations accordingly.
  size_t nlines = 1, lastpos = 0, pos;
  while ((pos = src.find('\n', lastpos)) != string::npos) {
    nlines++;
    lastpos = pos+1;
  }
  size_t lastlen = strlen(src.c_str()+lastpos);
  for (list<errinfo>::iterator it = interp.errpos.begin(),
	 end = interp.errpos.end(); it != end; ++it) {
    if (it->line1 == (int)nlines && it->col1 > (int)lastlen+1)
      it->col1 = lastlen+1;
    if (it->line2 == (int)nlines && it->col2 > (int)lastlen+1)
      it->col2 = lastlen+1;
  }
  if (res) {
    if (interp.errmsg.empty()) {
      pure_unref_internal(res);
      return res;
    } else {
      pure_free_internal(res);
      return 0;
    }
  } else if (interp.errmsg.empty())
    return mk_void();
  else
    return 0;
}

extern "C"
pure_expr *pure_evalx(pure_expr *x, pure_expr** e)
{
  assert(x);
  pure_expr *res = 0;
  interpreter& interp = *interpreter::g_interp;
  *e = 0;
  try {
    expr y = interp.pure_expr_to_expr(x);
    res = interp.eval(y, *e, false);
  } catch (err &e) {
    return x;
  }
  if (res) {
    assert(!*e);
    return res;
  } else if (*e) {
    pure_unref_internal(*e);
    return 0;
  } else
    return 0;
}

extern "C"
char *pure_evalcmd(const char *s)
{
  assert(s);
  interpreter& interp = *interpreter::g_interp;
  ostream *l_output = interp.output;
  ostringstream sout;
  interp.errmsg.clear(); interp.errpos.clear();
  interp.output = &sout;
  pure_expr *res = interp.runstr(string(s));
  interp.result = 0;
  interp.output = l_output;
  if (res)
    pure_free_internal(res);
  if (!interp.errmsg.empty() || sout.str().empty())
    return 0;
  else
    return strdup(sout.str().c_str());
}

extern "C"
void pure_start_logging()
{
  if (interpreter::g_interp) {
    interpreter& interp = *interpreter::g_interp;
    interp.errmsg.clear(); interp.errpos.clear();
    interp.logging = true;
  }
}

extern "C"
void pure_stop_logging()
{
  if (interpreter::g_interp) {
    interpreter& interp = *interpreter::g_interp;
    interp.logging = false;
  }
}

#ifndef HOST
#define HOST "unknown"
#endif
#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION "0.0"
#endif
#ifndef PURELIB
#define PURELIB "/usr/local/lib/pure-" PACKAGE_VERSION
#endif

#include <llvm/Target/TargetOptions.h>

static inline bool chkfile(const string& s)
{
  struct stat st;
  return !stat(s.c_str(), &st) && !S_ISDIR(st.st_mode);
}

static void add_path(list<string>& dirs, const string& path)
{
  size_t pos = 0;
  while (pos != string::npos) {
#ifdef _WIN32
    size_t end = path.find(';', pos);
#else
    size_t end = path.find(':', pos);
#endif
    string s;
    if (end == string::npos) {
      s = path.substr(pos);
      pos = end;
    } else {
      s = path.substr(pos, end-pos);
      pos = end+1;
    }
    if (!s.empty()) {
      if (s[s.size()-1] != '/') s.append("/");
      dirs.push_back(s);
    }
  }
}

static string unixize(const string& s)
{
  string t = s;
#ifdef _WIN32
  for (size_t i = 0, n = t.size(); i<n; i++)
    if (t[i] == '\\')
      t[i] = '/';
#endif
  return t;
}

extern "C"
pure_interp *pure_create_interp(int argc, char *argv[])
{
  if (interpreter::g_interp) interpreter::g_interp->save_context();
  // This is pretty much the same as pure.cc:main(), except that some options
  // are ignored and there's no user interaction.
  char base;
  interpreter *_interp = new interpreter(0, 0), &interp = *_interp;
  int count = 0;
  bool batch = false, want_prelude = true;
  // We use some stuff which is not safe to call while another interpreter is
  // active, so we temporarily switch to the new interpreter now.
  interpreter *s_interp = interpreter::g_interp;
  interpreter::g_interp = _interp;
  // This is used in advisory stack checks.
  interpreter::baseptr = &base;
  // get some settings from the environment
  const char *env;
  if ((env = getenv("HOME")))
    interp.histfile = string(env)+"/.pure_history";
  if ((env = getenv("PURE_PS")))
    interp.ps = string(env);
  if ((env = getenv("PURE_STACK"))) {
    char *end;
    size_t n = strtoul(env, &end, 0);
    if (!*end) interpreter::stackmax = n*1024;
  }
  if ((env = getenv("PURE_NOSYMBOLIC"))) interp.symbolic = false;
  if ((env = getenv("PURE_NOCHECKS"))) interp.checks = false;
  if ((env = getenv("PURE_NOCONST"))) interp.consts = false;
  if ((env = getenv("PURE_NOFOLD"))) interp.folding = false;
  if ((env = getenv("PURE_NOTC"))) interp.use_fastcc = false;
  if ((env = getenv("PURE_EAGER_JIT"))) interp.eager_jit = true;
  if ((env = getenv("PURE_ESCAPE"))) {
    string s = string(env);
    string prefixes = ESCAPECHARS;
    if (!s.empty()) {
      if (prefixes.find(s[0]) != string::npos)
	interp.escape_mode = s[0];
      else
	interp.warning("pure_create_interp: warning: invalid escape prefix '" +
		       s.substr(0, 1) + "'");
    }
  }
  if ((env = getenv("PURELIB"))) {
    string s = unixize(env);
    if (!s.empty() && s[s.size()-1] != '/') s.append("/");
    interp.libdir = s;
  } else
    interp.libdir = string(PURELIB)+"/";
  string prelude = interp.libdir+string("prelude.pure");
  // scan the command line options
  list<string> myargs;
  if (argv && *argv) for (char **args = ++argv; *args; ++args) {
    if (**args == '-') {
      char *arg = *args;
      if ((arg[1] && arg[2] == 0 && strchr("hbcgiqsu", arg[1])) ||
	  strcmp(arg, "--help") == 0 || strcmp(arg, "--version") == 0 ||
	  strcmp(arg, "-fPIC") == 0 || strcmp(arg, "-fpic") == 0 ||
	  strcmp(arg, "--norc") == 0 || strcmp(arg, "--noediting") == 0)
	batch = arg[2] == 0 && strchr("bci", arg[1]);
      else if (strcmp(arg, "-n") == 0 || strcmp(arg, "--noprelude") == 0)
	want_prelude = false;
      else if (strcmp(arg, "--nosymbolic") == 0)
	interp.symbolic = false;
      else if (strcmp(arg, "--symbolic") == 0)
	interp.symbolic = true;
      else if (strcmp(arg, "--nochecks") == 0)
	interp.checks = false;
      else if (strcmp(arg, "--checks") == 0)
	interp.checks = true;
      else if (strcmp(arg, "--noconst") == 0)
	interp.consts = false;
      else if (strcmp(arg, "--const") == 0)
	interp.consts = true;
      else if (strcmp(arg, "--nofold") == 0)
	interp.folding = false;
      else if (strcmp(arg, "--fold") == 0)
	interp.folding = true;
      else if (strcmp(arg, "--notc") == 0)
	interp.use_fastcc = false;
      else if (strcmp(arg, "--tc") == 0)
	interp.use_fastcc = true;
      else if (strcmp(arg, "--eager-jit") == 0)
	interp.eager_jit = true;
      else if (strcmp(arg, "-w") == 0)
	interp.compat = true;
      else if (strcmp(*args, "--escape") == 0 ||
	       strncmp(*args, "--escape=", 9) == 0) {
	string s = string(*args).substr(8);
	if (s.empty()) {
	  if (!*++args) {
	    cerr << "pure_create_interp: --escape lacks option argument\n";
	    delete _interp;
	    return 0;
	  }
	  s = *args;
	} else
	  s.erase(0, 1);
	string prefixes = ESCAPECHARS;
	if (s.empty())
	  interp.escape_mode = 0;
	else if (prefixes.find(s[0]) != string::npos)
	  interp.escape_mode = s[0];
	else
	  interp.warning("pure_create_interp: warning: invalid escape prefix '" +
			 s.substr(0, 1) + "'");
      } else if (strcmp(*args, "--enable") == 0 ||
	       strncmp(*args, "--enable=", 9) == 0) {
	string s = string(*args).substr(8);
	if (s.empty()) {
	  if (!*++args) {
	    cerr << "pure_create_interp: --enable lacks option argument\n";
	    delete _interp;
	    return 0;
	  }
	  s = *args;
	} else
	  s.erase(0, 1);
	interp.enable(s, true);
      } else if (strcmp(*args, "--disable") == 0 ||
		 strncmp(*args, "--disable=", 10) == 0) {
	string s = string(*args).substr(9);
	if (s.empty()) {
	  if (!*++args) {
	    cerr << "pure_create_interp: --disable lacks option argument\n";
	    delete _interp;
	    return 0;
	  }
	  s = *args;
	} else
	  s.erase(0, 1);
	interp.enable(s, false);
      } else if (strcmp(*args, "--main") == 0 ||
		 strncmp(*args, "--main=", 7) == 0) {
	string s = string(*args).substr(6);
	if (s.empty()) {
	  if (!*++args) {
	    cerr << "pure_create_interp: --main lacks name argument\n";
	    delete _interp;
	    return 0;
	  }
	}
	/* ignored */
      } else if (strncmp(*args, "-o", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) {
	  if (!*++args) {
	    cerr << "pure_create_interp: -o lacks filename argument\n";
	    delete _interp;
	    return 0;
	  }
	}
	/* ignored */
      } else if (strncmp(*args, "-l", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) {
	  if (!*++args) {
	    cerr << "pure_create_interp: -l lacks libname argument\n";
	    delete _interp;
	    return 0;
	  }
	}
	/* ignored */
      } else if (strncmp(*args, "-m", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) {
	  cerr << "pure_create_interp: -m lacks option argument\n";
	  delete _interp;
	  return 0;
	}
	/* ignored */
      } else if (strncmp(*args, "-I", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) {
	  if (!*++args) {
	    cerr << "pure_create_interp: -I lacks directory argument\n";
	    delete _interp;
	    return 0;
	  }
	  s = *args;
	}
	s = unixize(s);
	if (!s.empty()) {
	  if (s[s.size()-1] != '/') s.append("/");
	  interp.includedirs.push_back(s);
	}
      } else if (strncmp(*args, "-L", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) {
	  if (!*++args) {
	    cerr << "pure_create_interp: -L lacks directory argument\n";
	    delete _interp;
	    return 0;
	  }
	  s = *args;
	}
	s = unixize(s);
	if (!s.empty()) {
	  if (s[s.size()-1] != '/') s.append("/");
	  interp.librarydirs.push_back(s);
	}
      } else if (strncmp(*args, "-v", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) continue;
	char *end;
	if (strtoul(s.c_str(), &end, 0)) {}
	if (*end) {
	  cerr << "pure_create_interp: invalid option " << *args << endl;
	  delete _interp;
	  return 0;
	}
      } else if (strcmp(arg, "-x") == 0) {
	while (*++args) myargs.push_back(*args);
	break;
      } else if (strcmp(arg, "--") == 0) {
	while (*++args) myargs.push_back(*args);
	break;
      } else {
	cerr << "pure_create_interp: invalid option " << *args << endl;
	delete _interp;
	return 0;
      }
    } else if (!batch) {
      while (*args) myargs.push_back(*args++);
      break;
    }
  }
#if USE_FASTCC && !LLVM31
  // This global option is needed to get tail call optimization (you'll also
  // need to have USE_FASTCC in interpreter.hh enabled).
  if (interp.use_fastcc) llvm::GuaranteedTailCallOpt = true;
#endif
  interp.init_jit_mode();
  if ((env = getenv("PURE_INCLUDE")))
    add_path(interp.includedirs, unixize(env));
  if ((env = getenv("PURE_LIBRARY")))
    add_path(interp.librarydirs, unixize(env));
  interp.init_sys_vars(PACKAGE_VERSION, HOST, myargs);
  if (want_prelude) {
    // load the prelude if we can find it
    if (chkfile(prelude)) {
      try { interp.run(prelude, false); } catch (err &e) {
	cerr << "pure_create_interp: " << e.what() << endl;
	delete _interp;
	return 0;
      }
      interp.compile();
    }
  }
  // load scripts specified on the command line
  if (argv) {
    for (; *argv; ++argv) {
      if (string(*argv).substr(0,2) == "-v") {
	uint8_t level = 1;
	string s = string(*argv).substr(2);
	if (!s.empty()) level = (uint8_t)strtoul(s.c_str(), 0, 0);
	interp.verbose = level;
      } else if (*argv == string("-x")) {
	if (*++argv) {
	  count++; interp.modname = *argv;
	  try { interp.run(*argv, false); } catch (err &e) {
	    cerr << "pure_create_interp: " << e.what() << endl;
	    delete _interp;
	    return 0;
	  }
	} else {
	  cerr << "pure_create_interp: missing script name\n";
	  delete _interp;
	  return 0;
	}
	break;
      } else if (*argv == string("--"))
	break;
      else if (string(*argv).substr(0,2) == "-o" ||
	       string(*argv).substr(0,2) == "-l" ||
	       string(*argv).substr(0,2) == "-I" ||
	       string(*argv).substr(0,2) == "-L") {
	string s = string(*argv).substr(2);
	if (s.empty()) ++argv;
      } else if (string(*argv).substr(0,8) == "--escape") {
	string s = string(*argv).substr(8);
	if (s.empty()) ++argv;
      } else if (string(*argv).substr(0,8) == "--enable") {
	string s = string(*argv).substr(8);
	if (s.empty()) ++argv;
      } else if (string(*argv).substr(0,9) == "--disable") {
	string s = string(*argv).substr(9);
	if (s.empty()) ++argv;
      } else if (string(*argv).substr(0,6) == "--main") {
	string s = string(*argv).substr(6);
	if (s.empty()) ++argv;
      } else if (**argv == '-')
	;
      else if (**argv) {
	if (count++ == 0) interp.modname = *argv;
	try { interp.run(*argv, false); } catch (err &e) {
	  cerr << "pure_create_interp: " << e.what() << endl;
	  delete _interp;
	  return 0;
	}
	if (!batch) break;
      }
    }
  }
  interp.symtab.init_builtins();
  interp.temp = 1;
  interp.modno = interp.modctr++;
  if (s_interp) {
    if (interpreter::g_interp) interpreter::g_interp->save_context();
    interpreter::g_interp = s_interp;
    if (interpreter::g_interp) interpreter::g_interp->restore_context();
  }
  return (pure_interp*)_interp;
}

extern "C"
void pure_delete_interp(pure_interp *interp)
{
  assert(interp);
  interpreter *_interp = (interpreter*)interp;
  delete _interp;
}

extern "C"
void pure_switch_interp(pure_interp *interp)
{
  char base;
  if (interpreter::g_interp != (interpreter*)interp) {
    if (interpreter::g_interp) interpreter::g_interp->save_context();
    interpreter::g_interp = (interpreter*)interp;
    if (interpreter::g_interp) interpreter::g_interp->restore_context();
  }
  /* Check our base pointer to see whether it still looks valid. If we're
     wildly off from the calculated base pointer, chances are that we're being
     invoked from a new context with its own stack. In this case we update the
     base pointer and proceed with fingers crossed. XXXFIXME: This is only
     guesswork. A better way to deal with this situation would be to turn
     interpreter::baseptr into a thread-local variable, but this isn't cheap
     and we want the switching between interpreters to be as efficient as
     possible. */
  long d = &base - interpreter::baseptr;
  if (d < -100000 || d > 100000) interpreter::baseptr = &base;
}

#include <pthread.h>

// Global interpreter lock. This is evil, but may be needed in multithreaded
// code as long as the internals of the Pure interpreter are not thread-safe.
static pthread_mutex_t GIL = PTHREAD_MUTEX_INITIALIZER;

extern "C"
pure_interp *pure_lock_interp(pure_interp *interp)
{
  pthread_mutex_lock(&GIL);
  pure_interp *s_interp = pure_current_interp();
  pure_switch_interp(interp);
  return s_interp;
}

extern "C"
pure_interp *pure_unlock_interp(pure_interp *interp)
{
  pure_interp *s_interp = pure_current_interp();
  pure_switch_interp(interp);
  pthread_mutex_unlock(&GIL);
  return s_interp;
}

extern "C"
pure_interp *pure_current_interp()
{
  return (pure_interp*)interpreter::g_interp;
}

extern "C"
void pure_interp_compile(pure_interp *interp, int32_t fno)
{
  assert(interp);
  interpreter *_interp = (interpreter*)interp;
  _interp->jit_now(fno);
}

extern "C"
pure_interp_key_t pure_interp_key(void (*destroy)(void*))
{
  static pure_interp_key_t act_key = 0;
  ++act_key;
  if (destroy) interpreter::locals_destroy_cb[act_key] = destroy;
  return act_key;
}

extern "C"
void pure_interp_set(pure_interp_key_t key, void *ptr)
{
  interpreter *interp = interpreter::g_interp;
  assert(interp && key);
  interp->locals[key] = ptr;
}

extern "C"
void *pure_interp_get(pure_interp_key_t key)
{
  interpreter *interp = interpreter::g_interp;
  assert(interp && key);
  map<pure_interp_key_t,void*>::iterator it = interp->locals.find(key);
  return (it != interp->locals.end())?it->second:NULL;
}

static list<void (*)(void)> exit_funs;

extern "C"
int pure_atexit(void (*function)(void))
{
  exit_funs.push_front(function);
  return 0;
}

extern "C"
void pure_finalize(void)
{
  for (list<void (*)(void)>::const_iterator it = exit_funs.begin(),
	 end = exit_funs.end(); it != end; ++it)
    (*it)();
}

/* END OF PUBLIC API. *******************************************************/

extern "C"
pure_interp *pure_interp_main(int argc, char *argv[],
			      int32_t nsyms, char *syms,
			      pure_expr ***vars, void **vals,
			      int32_t *arities, void **externs,
			      pure_expr ***sstk, void **fptr)
{
  char base;
#if 0
  std::cerr << "\n** argc = " << argc << '\n';
  for (int i = 0; i < argc; i++)
    std::cerr << "argv[" << i << "] = '"
	      << (argv[i]?argv[i]:"(null)") << "'\n";
  std::cerr << "\n** " << nsyms << " symbols\n";
  std::cerr << syms;
  std::cerr << "\n** vars table " << vars << '\n';
  for (int32_t f = 1; f <= nsyms; f++)
    if (vars[f])
      std::cerr << f << " = " << vars[f] << '\n';
  std::cerr << "\n** vals table " << vals << '\n';
  for (int32_t f = 1; f <= nsyms; f++)
    if (vals[f])
      std::cerr << f << " = " << vals[f] << " (" << arities[f] << ")\n";
  std::cerr << "\n** externs table " << externs << '\n';
  for (int32_t f = 1; f <= nsyms; f++)
    if (externs[f])
      std::cerr << f << " = " << externs[f] << '\n';
  std::cerr << "\n** sstk = " << sstk << ", fptr = " << fptr << endl;
#endif
  if (interpreter::g_interp) interpreter::g_interp->save_context();
  interpreter *_interp =
    new interpreter(nsyms, syms, vars, vals, arities, externs, sstk, fptr),
    &interp = *_interp;
  interpreter::g_interp = _interp;
  interpreter::baseptr = &base;
  // get some settings from the environment
  const char *env;
  if ((env = getenv("PURE_STACK"))) {
    char *end;
    size_t n = strtoul(env, &end, 0);
    if (!*end) interpreter::stackmax = n*1024;
  }
  if ((env = getenv("PURE_NOSYMBOLIC"))) interp.symbolic = false;
  if ((env = getenv("PURE_NOCHECKS"))) interp.checks = false;
  if ((env = getenv("PURE_NOCONST"))) interp.consts = false;
  if ((env = getenv("PURE_NOFOLD"))) interp.folding = false;
  if ((env = getenv("PURE_NOTC"))) interp.use_fastcc = false;
  if ((env = getenv("PURELIB"))) {
    string s = unixize(env);
    if (!s.empty() && s[s.size()-1] != '/') s.append("/");
    interp.libdir = s;
  } else
    interp.libdir = string(PURELIB)+"/";
  if ((env = getenv("PURE_INCLUDE")))
    add_path(interp.includedirs, unixize(env));
  if ((env = getenv("PURE_LIBRARY")))
    add_path(interp.librarydirs, unixize(env));
#if USE_FASTCC && !LLVM31
  // This global option is needed to get tail call optimization (you'll also
  // need to have USE_FASTCC in interpreter.hh enabled).
  llvm::GuaranteedTailCallOpt = true;
#endif
  // scan the command line options
  list<string> myargs;
  if (argv && argc>0)
    for (; *argv && argc>0; ++argv, --argc)
      myargs.push_back(*argv);
  interp.symtab.init_builtins();
  interp.init_sys_vars(PACKAGE_VERSION, HOST, myargs);
  return (pure_interp*)_interp;
}

extern "C"
pure_expr *pure_const(int32_t tag)
{
  // XXXFIXME: We should cache these on a per interpreter basis, so that only
  // a single expression node exists for each symbol.
  pure_expr *x = new_expr();
  x->tag = tag;
  x->data.clos = 0;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_clos(bool local, int32_t tag, uint32_t key, uint32_t n,
		     void *f, void *e, uint32_t m, /* m x pure_expr* */ ...)
{
  pure_expr *x = new_expr();
  x->tag = tag;
  x->data.clos = new pure_closure;
  x->data.clos->local = local;
  x->data.clos->key = key;
  x->data.clos->n = n;
  x->data.clos->m = m;
  x->data.clos->fp = f;
  x->data.clos->ep = e;
  if (e) ((Env*)e)->refc++;
  x->data.clos->refp = interpreter::g_interp->get_refp(key);
  if (x->data.clos->refp) (*(uint32_t*)x->data.clos->refp)++;
  if (m == 0)
    x->data.clos->env = 0;
  else {
    x->data.clos->env = new pure_expr*[m];
    va_list ap;
    va_start(ap, m);
    for (size_t i = 0; i < m; i++) {
      x->data.clos->env[i] = va_arg(ap, pure_expr*);
      assert(x->data.clos->env[i]->refc > 0);
    }
    va_end(ap);
  }
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *pure_int64(int64_t l)
{
  int sgn = (l>0)?1:(l<0)?-1:0;
  uint64_t v = (uint64_t)(l>=0?l:-l);
  if (sizeof(mp_limb_t) == 8) {
    // 8 byte limbs, value fits in a single limb.
    limb_t u[1] = { (limb_t)v };
    return pure_bigint(sgn, u);
  } else {
    // 4 byte limbs, put least significant word in the first limb.
    limb_t u[2] = { (uint32_t)v, (uint32_t)(v>>32) };
    return pure_bigint(sgn+sgn, u);
  }
}

extern "C"
pure_expr *pure_uint64(uint64_t l)
{
  int sgn = (l!=0);
  if (sizeof(mp_limb_t) == 8) {
    // 8 byte limbs, value fits in a single limb.
    limb_t u[1] = { (limb_t)l };
    return pure_bigint(sgn, u);
  } else {
    // 4 byte limbs, put least significant word in the first limb.
    limb_t u[2] = { (uint32_t)l, (uint32_t)(l>>32) };
    return pure_bigint(sgn+sgn, u);
  }
}

static void make_bigint(mpz_t z, int32_t size, const limb_t *limbs)
{
  // FIXME: For efficiency, we poke directly into the mpz struct here, this
  // might need to be reviewed for future GMP revisions.
  int sz = size>=0?size:-size, sgn = size>0?1:size<0?-1:0, sz0 = 0;
  // normalize: the most significant limb should be nonzero
  for (int i = 0; i < sz; i++) if (limbs[i] != 0) sz0 = i+1;
  sz = sz0; size = sgn*sz;
  mpz_init(z);
  if (sz > 0) _mpz_realloc(z, sz);
  assert(sz == 0 || z->_mp_d);
  for (int i = 0; i < sz; i++) z->_mp_d[i] = limbs[i];
  z->_mp_size = size;
}

extern "C"
pure_expr *pure_bigint(int32_t size, const limb_t *limbs)
{
  pure_expr *x = new_expr();
  x->tag = EXPR::BIGINT;
  make_bigint(x->data.z, size, limbs);
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
int32_t pure_cmp_bigint(pure_expr *x, int32_t size, const limb_t *limbs)
{
  assert(x && x->tag == EXPR::BIGINT);
  mpz_t z;
  make_bigint(z, size, limbs);
  int res = mpz_cmp(x->data.z, z);
  mpz_clear(z);
  return res;
}

extern "C"
int32_t pure_cmp_string(pure_expr *x, const char *s)
{
  assert(x && x->tag == EXPR::STR);
  return strcmp(x->data.s, s);
}

extern "C"
void pure_add_rtty(int32_t tag, int argc, void *fp)
{
  interpreter& interp = *interpreter::g_interp;
  if (fp)
    interp.rtty[tag] = rtty_info(argc, fp);
  else
    interp.rtty.erase(tag);
}

static inline int builtin_typecheck(interpreter& interp,
				    int32_t tag, pure_expr *x)
{
  if (tag == interp.symtab.int_sym().f)
    return x->tag==EXPR::INT;
  else if (tag == interp.symtab.bigint_sym().f)
    return x->tag==EXPR::BIGINT;
  else if (tag == interp.symtab.double_sym().f)
    return x->tag==EXPR::DBL;
  else if (tag == interp.symtab.string_sym().f)
    return x->tag==EXPR::STR;
  else if (tag == interp.symtab.pointer_sym().f)
    return x->tag==EXPR::PTR;
  else if (tag == interp.symtab.matrix_sym().f)
    return x->tag>=EXPR::MATRIX && x->tag<=EXPR::IMATRIX;
  else
    return -1;
}

extern "C"
bool pure_typecheck(int32_t tag, pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  // Check for built-in types.
  int res = builtin_typecheck(interp, tag, x);
  if (res>=0) return res;
  if (tag<0) return false;
  // Check that the predicate actually exists.
  map<int32_t,rtty_info>::iterator info = interp.rtty.find(tag);
  if (info == interp.rtty.end()) return false;
  // Get the function pointer and number of arguments.
  void *fp = info->second.fp;
  int argc = info->second.argc;
  // An argument count of zero indicates an alias, pointing either to another
  // type predicate or a normal function (presumably a unary predicate).
  while (argc == 0) {
    // Resolve an alias. Note that this may loop if you define the predicate
    // in a circular way, so don't do this.
    pure_expr *y = pure_funcall(fp, 0);
    if (!y) return false;
    if (tag == y->tag) {
      // We're just looping here because the alias failed. Bail out.
      pure_freenew(y);
      return false;
    }
    tag = y->tag;
    if (tag > 0) {
      if ((res = builtin_typecheck(interp, tag, x)) >= 0) {
	pure_freenew(y);
	return res;
      } else if ((info = interp.rtty.find(tag)) != interp.rtty.end()) {
	// Alias pointing to yet another defined type predicate. Wash, rinse,
	// repeat.
	pure_freenew(y);
	fp = info->second.fp;
	argc = info->second.argc;
      } else
	goto eval;
    } else {
    eval:
      // Alias presumably is an ordinary predicate. Apply it to the argument
      // expression and return the resulting truth value.
      y = pure_apply2(y, x);
      int32_t iv;
      bool res = pure_is_int(y, &iv) && iv!=0;
      pure_freenew(y);
      return res;
    }
  }
  // Call the predicate.
  pure_expr *y = pure_funcall(fp, 1, x);
  if (y) {
    int32_t iv;
    bool res = pure_is_int(y, &iv) && iv!=0;
    pure_freenew(y);
    return res;
  } else
    return false;
}

extern "C"
bool pure_safe_typecheck(int32_t tag, pure_expr *x)
{
  if (x->refc == 0) {
    // Protect x from being garbage-collected.
    pure_new(x);
    bool res = pure_typecheck(tag, x);
    pure_unref(x);
    return res;
  } else
    return pure_typecheck(tag, x);
}

extern "C"
pure_expr *typep(pure_expr *ty, pure_expr *x)
{
  if (ty->tag > 0) {
    bool res = pure_typecheck(ty->tag, x);
    return pure_int(res);
  } else
    return 0;
}

char *pure_get_cstring(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<char*>& temps = interp.temps;
  assert(x && x->tag == EXPR::STR);
  char *s = my_fromutf8(x->data.s, 0);
  assert(s);
  temps.push_back(s);
  return s;
}

extern "C"
void pure_free_cstrings()
{
  interpreter& interp = *interpreter::g_interp;
  list<char*>& temps = interp.temps;
  for (list<char*>::iterator t = temps.begin(); t != temps.end(); t++)
    if (*t) my_strfree(*t);
  temps.clear();
}

extern "C"
int64_t pure_get_int64(pure_expr *x)
{
  uint64_t v =
    (sizeof(mp_limb_t) == 8) ? (uint64_t)mpz_getlimbn(x->data.z, 0) :
    (mpz_getlimbn(x->data.z, 0) +
     (((uint64_t)mpz_getlimbn(x->data.z, 1))<<32));
  return (mpz_sgn(x->data.z) < 0) ? -(int64_t)v : (int64_t)v;
}

extern "C"
int32_t pure_get_int(pure_expr *x)
{
  uint32_t v =
    (sizeof(mp_limb_t) == 8) ? (uint32_t)(uint64_t)mpz_getlimbn(x->data.z, 0) :
    mpz_getlimbn(x->data.z, 0);
  return (mpz_sgn(x->data.z) < 0) ? -(int32_t)v : (int32_t)v;
}

extern "C"
void *pure_get_bigint(pure_expr *x)
{
  assert(x && x->tag == EXPR::BIGINT);
  return &x->data.z;
}

extern "C"
void *pure_get_matrix(pure_expr *x)
{
  assert((x && x->tag == EXPR::MATRIX) || x->tag == EXPR::DMATRIX ||
	 x->tag == EXPR::CMATRIX || x->tag == EXPR::IMATRIX);
  return x->data.mat.p;
}

extern "C"
void *pure_get_matrix_data(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    return m->data;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    return m->data;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    return m->data;
  }
  case EXPR::IMATRIX:{
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    return m->data;
  }
  default:
    assert(0 && "not a matrix");
    return 0;
  }
}

bool is_contiguous(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    return m->size2 == m->tda;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    return m->size2 == m->tda;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    return m->size2 == m->tda;
  }
  case EXPR::IMATRIX:{
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    return m->size2 == m->tda;
  }
  default:
    assert(0 && "not a matrix");
    return false;
  }
}

static void *byte_array_to_matrix(pure_expr *x, void *q)
{
  char *p = (char*)q;
  if (!p) return 0;
  switch (x->tag) {
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < m; j++)
	m1->data[i*m1->tda+j] = (int)p[i*m+j];
    }
    return p;
  }
  default:
    return 0;
  }
}

static void *short_array_to_matrix(pure_expr *x, void *q)
{
  short *p = (short*)q;
  if (!p) return 0;
  switch (x->tag) {
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < m; j++)
	m1->data[i*m1->tda+j] = (int)p[i*m+j];
    }
    return p;
  }
  default:
    return 0;
  }
}

static void *int_array_to_matrix(pure_expr *x, void *q)
{
  int *p = (int*)q;
  if (!p) return 0;
  switch (x->tag) {
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    if (m1->data != p) {
      size_t n = m1->size1, m = m1->size2;
      if (n==0 || m==0) return p;
      for (size_t i = 0; i < n; i++) {
	for (size_t j = 0; j < m; j++)
	  m1->data[i*m1->tda+j] = (int)p[i*m+j];
      }
    }
    return p;
  }
  default:
    return 0;
  }
}

static void *int64_array_to_matrix(pure_expr *x, void *q)
{
  int64_t *p = (int64_t*)q;
  if (!p) return 0;
  switch (x->tag) {
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < m; j++)
	m1->data[i*m1->tda+j] = (int)p[i*m+j];
    }
    return p;
  }
  default:
    return 0;
  }
}

static void *float_array_to_matrix(pure_expr *x, void *q)
{
  float *p = (float*)q;
  if (!p) return 0;
  switch (x->tag) {
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < m; j++)
	m1->data[i*m1->tda+j] = (double)p[i*m+j];
    }
    return p;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < 2*m; j++)
	m1->data[2*i*m1->tda+j] = (double)p[2*i*m+j];
    }
    return p;
  }
  default:
    return 0;
  }
}

static void *double_array_to_matrix(pure_expr *x, void *q)
{
  double *p = (double*)q;
  if (!p) return 0;
  switch (x->tag) {
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    if (m1->data != p) {
      size_t n = m1->size1, m = m1->size2;
      if (n==0 || m==0) return p;
      for (size_t i = 0; i < n; i++) {
	for (size_t j = 0; j < m; j++)
	  m1->data[i*m1->tda+j] = (double)p[i*m+j];
      }
    }
    return p;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    if (m1->data != p) {
      size_t n = m1->size1, m = m1->size2;
      if (n==0 || m==0) return p;
      for (size_t i = 0; i < n; i++) {
	for (size_t j = 0; j < 2*m; j++)
	  m1->data[2*i*m1->tda+j] = (double)p[2*i*m+j];
      }
    }
    return p;
  }
  default:
    return 0;
  }
}

void *pure_get_matrix_data_byte(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_byte_array(0, x);
  cvector_temps.push_back(cvector_data(x, v, cvd_byte));
  return v;
}

void *pure_get_matrix_data_short(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_short_array(0, x);
  cvector_temps.push_back(cvector_data(x, v, cvd_short));
  return v;
}

void *pure_get_matrix_data_int(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = is_contiguous(x)?pure_get_matrix_data(x):
    matrix_to_int_array(0, x);
  cvector_temps.push_back(cvector_data(x, v, cvd_int));
  return v;
}

void *pure_get_matrix_data_int64(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_int64_array(0, x);
  cvector_temps.push_back(cvector_data(x, v, cvd_int64));
  return v;
}

void *pure_get_matrix_data_float(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_float_array(0, x);
  cvector_temps.push_back(cvector_data(x, v, cvd_float));
  return v;
}

void *pure_get_matrix_data_double(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = is_contiguous(x)?pure_get_matrix_data(x):
    matrix_to_double_array(0, x);
  cvector_temps.push_back(cvector_data(x, v, cvd_double));
  return v;
}

static void *matrix_to_void_vector(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m1 = (gsl_matrix_symbolic*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2, l = n*m;
    if (l==0) return 0;
    void **p = (void**)malloc((l+1)*sizeof(void*));
    if (!p) return 0;
    for (size_t k = 0, i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)  {
	void *ptr;
	const char *s;
	if (pure_is_pointer(m1->data[i*m1->tda+j], &ptr))
	  p[k++] = ptr;
	else if (pure_is_string(m1->data[i*m1->tda+j], &s))
	  p[k++] = (void*)s;
	else {
	  free(p);
	  return 0;
	}
      }
    p[l] = 0;
    return p;
  }
  default:
    return 0;
  }
}

static void *matrix_to_char_vector(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m1 = (gsl_matrix_symbolic*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2, l = n*m;
    if (l==0) return 0;
    void **p = (void**)malloc((l+1)*sizeof(char*));
    if (!p) return 0;
    for (size_t k = 0, i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)  {
	char *s;
	if (pure_is_cstring_dup(m1->data[i*m1->tda+j], &s))
	  p[k++] = (void*)s;
	else {
	  free(p);
	  return 0;
	}
      }
    p[l] = 0;
    return p;
  }
  default:
    return 0;
  }
}

static void *matrix_to_int_vector(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return 0;
    int **p = (int**)malloc(n*sizeof(int*));
    if (!p) return 0;
    for (size_t i = 0; i < n; i++)
      p[i] = m1->data+(i*m1->tda);
    return p;
  }
  default:
    return 0;
  }
}

static void *matrix_to_int64_vector(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return 0;
    int64_t **p = (int64_t**)malloc(n*sizeof(int64_t*));
    if (!p) return 0;
    for (size_t i = 0; i < n; i++) {
      p[i] = (int64_t*)malloc(m*sizeof(int64_t));
      if (!p[i]) {
	for (size_t j = 0; j < i; j++) free(p[j]);
	free(p); p = 0;
	break;
      }
    }
    if (!p) return 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	p[i][j] = (int64_t)m1->data[i*m1->tda+j];
    return p;
  }
  default:
    return 0;
  }
}

static void *matrix_to_short_vector(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return 0;
    short **p = (short**)malloc(n*sizeof(short*));
    if (!p) return 0;
    for (size_t i = 0; i < n; i++) {
      p[i] = (short*)malloc(m*sizeof(short));
      if (!p[i]) {
	for (size_t j = 0; j < i; j++) free(p[j]);
	free(p); p = 0;
	break;
      }
    }
    if (!p) return 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	p[i][j] = (short)m1->data[i*m1->tda+j];
    return p;
  }
  default:
    return 0;
  }
}

static void *matrix_to_byte_vector(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return 0;
    char **p = (char**)malloc(n*sizeof(char*));
    if (!p) return 0;
    for (size_t i = 0; i < n; i++) {
      p[i] = (char*)malloc(m);
      if (!p[i]) {
	for (size_t j = 0; j < i; j++) free(p[j]);
	free(p); p = 0;
	break;
      }
    }
    if (!p) return 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	p[i][j] = (char)m1->data[i*m1->tda+j];
    return p;
  }
  default:
    return 0;
  }
}

static void *matrix_to_double_vector(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return 0;
    double **p = (double**)malloc(n*sizeof(double*));
    if (!p) return 0;
    for (size_t i = 0; i < n; i++)
      p[i] = m1->data+(i*m1->tda);
    return p;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return 0;
    double **p = (double**)malloc(n*sizeof(double*));
    if (!p) return 0;
    for (size_t i = 0; i < n; i++)
      p[i] = m1->data+(2*i*m1->tda);
    return p;
  }
  default:
    return 0;
  }
}

static void *matrix_to_float_vector(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return 0;
    float **p = (float**)malloc(n*sizeof(float*));
    if (!p) return 0;
    for (size_t i = 0; i < n; i++) {
      p[i] = (float*)malloc(m*sizeof(float));
      if (!p[i]) {
	for (size_t j = 0; j < i; j++) free(p[j]);
	free(p); p = 0;
	break;
      }
    }
    if (!p) return 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	p[i][j] = (float)m1->data[i*m1->tda+j];
    return p;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return 0;
    float **p = (float**)malloc(n*sizeof(float*));
    if (!p) return 0;
    for (size_t i = 0; i < n; i++) {
      p[i] = (float*)malloc(2*m*sizeof(float));
      if (!p[i]) {
	for (size_t j = 0; j < i; j++) free(p[j]);
	free(p); p = 0;
	break;
      }
    }
    if (!p) return 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < 2*m; j++)
	p[i][j] = (float)m1->data[2*i*m1->tda+j];
    return p;
  }
  default:
    return 0;
  }
}

static void *char_vector_to_matrix(pure_expr *x, void *v, void *w)
{
  // We can't modify the matrix in place here, but we still need to collect
  // the converted strings.
  char **p = (char**)w;
  if (!p) return v;
  for (size_t i = 0; p[i]; i++) free(p[i]);
  free(w);
  return v;
}

static void *byte_vector_to_matrix(pure_expr *x, void *q)
{
  char **p = (char**)q;
  if (!p) return 0;
  switch (x->tag) {
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < m; j++)
	m1->data[i*m1->tda+j] = (int)p[i][j];
      free(p[i]);
    }
    return p;
  }
  default:
    return 0;
  }
}

static void *short_vector_to_matrix(pure_expr *x, void *q)
{
  short **p = (short**)q;
  if (!p) return 0;
  switch (x->tag) {
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < m; j++)
	m1->data[i*m1->tda+j] = (int)p[i][j];
      free(p[i]);
    }
    return p;
  }
  default:
    return 0;
  }
}

static void *int64_vector_to_matrix(pure_expr *x, void *q)
{
  int64_t **p = (int64_t**)q;
  if (!p) return 0;
  switch (x->tag) {
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < m; j++)
	m1->data[i*m1->tda+j] = (int)p[i][j];
      free(p[i]);
    }
    return p;
  }
  default:
    return 0;
  }
}

static void *float_vector_to_matrix(pure_expr *x, void *q)
{
  float **p = (float**)q;
  if (!p) return 0;
  switch (x->tag) {
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < m; j++)
	m1->data[i*m1->tda+j] = (double)p[i][j];
      free(p[i]);
    }
    return p;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < 2*m; j++)
	m1->data[2*i*m1->tda+j] = (double)p[i][j];
      free(p[i]);
    }
    return p;
  }
  default:
    return 0;
  }
}

void *pure_get_matrix_vector_void(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_void_vector(x);
  cvector_temps.push_back(cvector_data(x, v, cvd_void, true));
  return v;
}

void *pure_get_matrix_vector_char(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_char_vector(x);
  cvector_temps.push_back(cvector_data(x, v, cvd_char, true));
  return v;
}

void *pure_get_matrix_vector_byte(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_byte_vector(x);
  cvector_temps.push_back(cvector_data(x, v, cvd_byte, true));
  return v;
}

void *pure_get_matrix_vector_short(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_short_vector(x);
  cvector_temps.push_back(cvector_data(x, v, cvd_short, true));
  return v;
}

void *pure_get_matrix_vector_int(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_int_vector(x);
  cvector_temps.push_back(cvector_data(x, v, cvd_int, true));
  return v;
}

void *pure_get_matrix_vector_int64(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_int64_vector(x);
  cvector_temps.push_back(cvector_data(x, v, cvd_int64, true));
  return v;
}

void *pure_get_matrix_vector_float(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_float_vector(x);
  cvector_temps.push_back(cvector_data(x, v, cvd_float, true));
  return v;
}

void *pure_get_matrix_vector_double(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  void *v = matrix_to_double_vector(x);
  cvector_temps.push_back(cvector_data(x, v, cvd_double, true));
  return v;
}

extern "C"
void pure_free_cvectors()
{
  interpreter& interp = *interpreter::g_interp;
  list<cvector_data>& cvector_temps = interp.cvector_temps;
  for (list<cvector_data>::iterator t = cvector_temps.begin();
       t != cvector_temps.end(); t++) {
    if (t->v) {
      if (t->x && t->ty) {
	if (t->vdata) {
	  switch (t->ty) {
	  case cvd_char: char_vector_to_matrix(t->x, t->v, t->w); break;
	  case cvd_byte: byte_vector_to_matrix(t->x, t->v); break;
	  case cvd_short: short_vector_to_matrix(t->x, t->v); break;
	  case cvd_int64: int64_vector_to_matrix(t->x, t->v); break;
	  case cvd_float: float_vector_to_matrix(t->x, t->v); break;
	  default: break;
	  }
	} else {
	  switch (t->ty) {
	  case cvd_byte: byte_array_to_matrix(t->x, t->v); break;
	  case cvd_short: short_array_to_matrix(t->x, t->v); break;
	  case cvd_int: int_array_to_matrix(t->x, t->v); break;
	  case cvd_int64: int64_array_to_matrix(t->x, t->v); break;
	  case cvd_float: float_array_to_matrix(t->x, t->v); break;
	  case cvd_double: double_array_to_matrix(t->x, t->v); break;
	  default: break;
	  }
	}
      }
      if (!t->x || pure_get_matrix_data(t->x) != t->v) free(t->v);
    }
  }
  cvector_temps.clear();
}

extern "C"
pure_expr *pure_matrix_rows(uint32_t n, ...)
{
  va_list ap;
  pure_expr **xs = (pure_expr**)alloca(n*sizeof(pure_expr*));
  int k = -1;
  size_t nrows = 0, ncols = 0;
  int32_t target = 0;
  bool have_matrix = false;
  pure_expr *x = 0;
  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    xs[i] = va_arg(ap, pure_expr*);
  va_end(ap);
  for (size_t i = 0; i < n; i++) {
    x = xs[i];
    switch (x->tag) {
    case EXPR::MATRIX: {
      gsl_matrix_symbolic *mp = (gsl_matrix_symbolic*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  goto err;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::MATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::DBL:
      set_target_type(target, EXPR::DMATRIX);
      if (k >= 0 && k != 1) goto err;
      nrows++; k = 1;
      break;
    case EXPR::INT:
      set_target_type(target, EXPR::IMATRIX);
      if (k >= 0 && k != 1) goto err;
      nrows++; k = 1;
      break;
    case EXPR::APP: {
      double a, b;
      if (k >= 0 && k != 1) goto err;
      nrows++; k = 1;
      if (get_complex(x, a, b))
	set_target_type(target, EXPR::CMATRIX);
      else
	set_target_type(target, EXPR::MATRIX);
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *mp = (gsl_matrix*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  goto err;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::DMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *mp = (gsl_matrix_complex*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  goto err;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::CMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mp = (gsl_matrix_int*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  goto err;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::IMATRIX);
	have_matrix = true;
      }
      break;
    }
    default:
      if (k >= 0 && k != 1) goto err;
      nrows++; k = 1;
      set_target_type(target, EXPR::MATRIX);
      break;
    }
  }
  if (n == 1 && have_matrix) return xs[0];
  if (k < 0) k = 0;
  ncols = k;
  if (target == 0) target = EXPR::MATRIX;
  switch (target) {
  case EXPR::MATRIX:
    return symbolic_matrix_rows(nrows, ncols, n, xs);
  case EXPR::DMATRIX:
    return double_matrix_rows(nrows, ncols, n, xs);
  case EXPR::CMATRIX:
    return complex_matrix_rows(nrows, ncols, n, xs);
  case EXPR::IMATRIX:
    return int_matrix_rows(nrows, ncols, n, xs);
  default:
    assert(0 && "this can't happen");
    return 0;
  }
 err:
  /* This is called without a shadow stack frame, so we do our own cleanup
     here to avoid having temporaries hanging around indefinitely. */
  if (x) pure_new_internal(x);
  pure_new_vect(n, xs);
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  if (x) {
    pure_unref_internal(x);
    pure_throw(bad_matrix_exception(x));
  }
  return 0;
}

extern "C"
pure_expr *pure_matrix_columns(uint32_t n, ...)
{
  va_list ap;
  pure_expr **xs = (pure_expr**)alloca(n*sizeof(pure_expr*));
  int k = -1;
  size_t nrows = 0, ncols = 0;
  int32_t target = 0;
  bool have_matrix = false;
  pure_expr *x = 0;
  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    xs[i] = va_arg(ap, pure_expr*);
  va_end(ap);
  for (size_t i = 0; i < n; i++) {
    x = xs[i];
    switch (x->tag) {
    case EXPR::MATRIX: {
      gsl_matrix_symbolic *mp = (gsl_matrix_symbolic*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  goto err;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::MATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::DBL:
      set_target_type(target, EXPR::DMATRIX);
      if (k >= 0 && k != 1) goto err;
      ncols++; k = 1;
      break;
    case EXPR::INT:
      set_target_type(target, EXPR::IMATRIX);
      if (k >= 0 && k != 1) goto err;
      ncols++; k = 1;
      break;
    case EXPR::APP: {
      double a, b;
      if (k >= 0 && k != 1) goto err;
      ncols++; k = 1;
      if (get_complex(x, a, b))
	set_target_type(target, EXPR::CMATRIX);
      else
	set_target_type(target, EXPR::MATRIX);
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *mp = (gsl_matrix*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  goto err;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::DMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *mp = (gsl_matrix_complex*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  goto err;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::CMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mp = (gsl_matrix_int*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  goto err;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::IMATRIX);
	have_matrix = true;
      }
      break;
    }
    default:
      if (k >= 0 && k != 1) goto err;
      ncols++; k = 1;
      set_target_type(target, EXPR::MATRIX);
      break;
    }
  }
  if (n == 1 && have_matrix) return xs[0];
  if (k < 0) k = 0;
  nrows = k;
  if (target == 0) target = EXPR::MATRIX;
  switch (target) {
  case EXPR::MATRIX:
    return symbolic_matrix_columns(nrows, ncols, n, xs);
  case EXPR::DMATRIX:
    return double_matrix_columns(nrows, ncols, n, xs);
  case EXPR::CMATRIX:
    return complex_matrix_columns(nrows, ncols, n, xs);
  case EXPR::IMATRIX:
    return int_matrix_columns(nrows, ncols, n, xs);
  default:
    assert(0 && "this can't happen");
    return 0;
  }
 err:
  /* This is called without a shadow stack frame, so we do our own cleanup
     here to avoid having temporaries hanging around indefinitely. */
  if (x) pure_new_internal(x);
  pure_new_vect(n, xs);
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  if (x) {
    pure_unref_internal(x);
    pure_throw(bad_matrix_exception(x));
  }
  return 0;
}

extern "C"
pure_expr *pure_matrix_rowsq(uint32_t n, ...)
{
  va_list ap;
  pure_expr **xs = (pure_expr**)alloca(n*sizeof(pure_expr*));
  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    xs[i] = va_arg(ap, pure_expr*);
  va_end(ap);
  return pure_matrix_rowsvq(n, xs);
}

extern "C"
pure_expr *pure_matrix_rowsvq(uint32_t n, pure_expr **xs)
{
  int k = -1;
  int32_t target = 0;
  pure_expr *x = 0;
  for (size_t i = 0; i < n; i++) {
    x = xs[i];
    switch (x->tag) {
    case EXPR::MATRIX: {
      gsl_matrix_symbolic *mp = (gsl_matrix_symbolic*)x->data.mat.p;
      assert(mp->size1==1);
      if (k >= 0 && mp->size2 != (size_t)k)
	goto err;
      k = mp->size2;
      set_target_type(target, EXPR::MATRIX);
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *mp = (gsl_matrix*)x->data.mat.p;
      assert(mp->size1==1);
      if (k >= 0 && mp->size2 != (size_t)k)
	goto err;
      k = mp->size2;
      set_target_type(target, EXPR::DMATRIX);
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *mp = (gsl_matrix_complex*)x->data.mat.p;
      assert(mp->size1==1);
      if (k >= 0 && mp->size2 != (size_t)k)
	goto err;
      k = mp->size2;
      set_target_type(target, EXPR::CMATRIX);
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mp = (gsl_matrix_int*)x->data.mat.p;
      assert(mp->size1==1);
      if (k >= 0 && mp->size2 != (size_t)k)
	goto err;
      k = mp->size2;
      set_target_type(target, EXPR::IMATRIX);
      break;
    }
    default:
      assert(0 && "this can't happen");
      return 0;
    }
  }
  if (n == 1 && k >= 0) return xs[0];
  if (k < 0) k = 0;
  if (target == 0) target = EXPR::MATRIX;
  switch (target) {
  case EXPR::MATRIX:
    return symbolic_matrix_rows(n, k, n, xs);
  case EXPR::DMATRIX:
    return double_matrix_rows(n, k, n, xs);
  case EXPR::CMATRIX:
    return complex_matrix_rows(n, k, n, xs);
  case EXPR::IMATRIX:
    return int_matrix_rows(n, k, n, xs);
  default:
    assert(0 && "this can't happen");
    return 0;
  }
 err:
  /* This is called without a shadow stack frame, so we do our own cleanup
     here to avoid having temporaries hanging around indefinitely. */
  if (x) pure_new_internal(x);
  pure_new_vect(n, xs);
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  if (x) {
    pure_unref_internal(x);
    pure_throw(bad_matrix_exception(x));
  }
  return 0;
}

static double get_matrix_double_value(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::INT:
    return (double)x->data.i;
  case EXPR::BIGINT:
    return mpz_get_d(x->data.z);
  case EXPR::DBL:
    return x->data.d;
  default:
    assert(0 && "bad matrix element");
    return 0.0;
  }
}

static int32_t get_matrix_int_value(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::INT:
    return x->data.i;
  case EXPR::BIGINT:
    return mpz_get_si(x->data.z);
  case EXPR::DBL:
    return (int32_t)x->data.d;
  default:
    assert(0 && "bad matrix element");
    return 0;
  }
}

static void get_matrix_complex_value(pure_expr *x, double& a, double& b)
{
  a = b = 0.0;
  switch (x->tag) {
  case EXPR::INT:
    a = (double)x->data.i;
    break;
  case EXPR::BIGINT:
    a = mpz_get_d(x->data.z);
    break;
  case EXPR::DBL:
    a = x->data.d;
    break;
  case EXPR::APP:
    if (!get_complex(x, a, b)) {
      assert(0 && "bad matrix element");
    }
    break;
  default:
    assert(0 && "bad matrix element");
    break;
  }
}

extern "C"
pure_expr *pure_matrix_columnsq(uint32_t n, ...)
{
  va_list ap;
  pure_expr **xs = (pure_expr**)alloca(n*sizeof(pure_expr*));
  va_start(ap, n);
  for (size_t i = 0; i < n; i++)
    xs[i] = va_arg(ap, pure_expr*);
  va_end(ap);
  return pure_matrix_columnsvq(n, xs);
}

extern "C"
pure_expr *pure_matrix_columnsvq(uint32_t n, pure_expr **xs)
{
  int32_t target = 0;
  pure_expr *x = 0;
  for (size_t i = 0; i < n; i++) {
    x = xs[i];
    switch (x->tag) {
    case EXPR::DBL:
      set_target_type(target, EXPR::DMATRIX);
      break;
    case EXPR::INT:
      set_target_type(target, EXPR::IMATRIX);
      break;
    case EXPR::APP: {
      double a, b;
      if (get_complex(x, a, b))
	set_target_type(target, EXPR::CMATRIX);
      else
	set_target_type(target, EXPR::MATRIX);
      break;
    }
    default:
      set_target_type(target, EXPR::MATRIX);
      break;
    }
  }
  if (target == 0) target = EXPR::MATRIX;
  pure_expr *ret = 0;
  pure_new_vect(n, xs);
  switch (target) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *mat = create_symbolic_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      mat->data[i] = xs[i];
    ret = pure_symbolic_matrix(mat);
    break;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *mat = create_double_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      mat->data[i] = get_matrix_double_value(xs[i]);
    ret = pure_double_matrix(mat);
    break;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *mat = create_complex_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      get_matrix_complex_value(xs[i], mat->data[2*i], mat->data[2*i+1]);
    ret = pure_complex_matrix(mat);
    break;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *mat = create_int_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      mat->data[i] = get_matrix_int_value(xs[i]);
    ret = pure_int_matrix(mat);
    break;
  }
  default:
    assert(0 && "this can't happen");
    ret = 0;
    break;
  }
  for (size_t i = 0; i < n; i++)
    pure_free_internal(xs[i]);
  return ret;
}

#if DEFER_GLOBALS
static inline void *get_funptr(pure_expr *x)
{
  if (!x->data.clos->fp) {
    // The function hasn't been JITed yet. Do it now.
    interpreter& interp = *interpreter::g_interp;
    map<int32_t,Env>::iterator g = interp.globalfuns.find(x->tag);
    // Make sure that the function wasn't purged in the meantime.
    if (g != interp.globalfuns.end()) {
      llvm::Function *f = g->second.f, *h = g->second.h;
      assert(h);
      if (f != h) interp.JIT->getPointerToFunction(f);
      x->data.clos->fp = interp.JIT->getPointerToFunction(h);
#if DEBUG>1
      std::cerr << "JIT " << h->getNameStr() << " -> " << x->data.clos->fp << '\n';
#endif
    }
  }
  return x->data.clos->fp;
}
#else
#define get_funptr(x) x->data.clos->fp
#endif

static inline void resize_sstk(pure_expr**& sstk, size_t& cap,
			       size_t sz, size_t n)
{
  size_t newsz = sz+n;
  if (newsz > cap) {
    while (newsz > cap) {
      assert((cap << 1) > cap);
      cap = cap << 1;
    }
    sstk = (pure_expr**)realloc(sstk, cap*sizeof(pure_expr*));
    assert(sstk);
  }
}

/* Extended trampoline for doing indirect function calls to prevent tail calls
   from overflowing the stack. The same technique is also used in Chicken
   Scheme, cf. http://en.wikipedia.org/wiki/Tail_recursion, except that we use
   an extended trampoline of fixed size, given below. Smaller sizes use less
   stack space, larger sizes are faster. Please note that this is only used in
   indirect calls. Most direct function calls in Pure are translated to
   corresponding function calls in LLVM IR, which rely on LLVM's built-in TCO
   capabilities to opimize away tail calls. */

#define TRAMPOLINE_SIZE 1024

static pure_expr *trampoline(interpreter& interp,
			     pure_expr *x, void *fp, uint32_t n, uint32_t m,
			     pure_expr **argv)
{
  pure_expr *ret;
  pure_aframe *ex = interp.astk;
  uint32_t env = 0;
  bool fini = false;
  if (ex && ex->count && ex->sz == interp.sstk_sz) {
    // tail call
    if (ex->count++ >= TRAMPOLINE_SIZE) {
      // exceeded the size of the trampoline, so rewind the stack (this is
      // only done at regular intervals to make things more efficient)
      ex->count = 1;
      ex->e = x; ex->fp = fp;
      ex->n = n; ex->m = m; ex->argv = argv;
      longjmp(ex->jmp, 1);
    } else
      goto call;
  }
  // create a new trampoline for handling tail calls
  fini = true;
  ex = interp.push_aframe(interp.sstk_sz);
  ex->count = 1;
  if (setjmp(ex->jmp)) {
    // landing pad of tail call
    x = ex->e; fp = ex->fp;
    n = ex->n; m = ex->m; argv = ex->argv;
  }
 call:
  if (n+m == 0) {
    // parameterless call
    pure_push_args(0, 0);
    ret = ((pure_expr*(*)())fp)();
  } else {
    // construct a stack frame for a function call with parameters
    {
      size_t sz = interp.sstk_sz;
      resize_sstk(interp.sstk, interp.sstk_cap, sz, n+m+1);
      pure_expr **sstk = interp.sstk;
      if (m>0) env = sz+n+1;
      sstk[sz++] = 0;
      for (size_t j = 0; j < n; j++)
	sstk[sz++] = (pure_expr*)argv[j];
      for (size_t j = 0; j < m; j++) {
	sstk[sz++] = x->data.clos->env[j];
	assert(x->data.clos->env[j]->refc > 0);
	x->data.clos->env[j]->refc++;
      }
      interp.sstk_sz = sz;
    }
    // also keep track of the function object on the activation stack so that
    // it can be garbage-collected if we hit an exception
    ex->e = x;
    if (m>0)
      xfuncall(ret, fp, n, env, argv)
    else
      funcall(ret, fp, n, argv)
    {
      // cleanup of the function object
      bool keep = m>0 && ret->refc>0;
      if (keep) ret->refc++;
      pure_free_internal(x);
      if (keep) pure_unref_internal(ret);
    }
    ex->e = 0;
  }
  if (fini) interp.pop_aframe();
  return ret;
}

extern "C"
pure_expr *pure_call(pure_expr *x)
{
  char test;
  void *fp = 0;
  assert(x);
  if (x->tag <= 0 || !x->data.clos || x->data.clos->n != 0 ||
      (fp = get_funptr(x)) == 0) {
#if DEBUG>2
    cerr << "pure_call: returning " << x << endl;
#endif
    checkstk(test);
    return x;
  }
#if DEBUG>1
  cerr << "pure_call: calling " << x << " -> " << fp << endl;
#endif
  assert(fp && x->refc > 0 && !x->data.clos->local);
  // parameterless call
  interpreter& interp = *interpreter::g_interp;
  if (!interp.checks) { checkall(test); }
  if (interp.debugging)
    return ((pure_expr*(*)())fp)();
  else
    return trampoline(interp, 0, fp, 0, 0, 0);
}

#define is_thunk(x) ((x)->tag == 0 && (x)->data.clos && (x)->data.clos->n == 0)

extern "C"
pure_expr *pure_force(pure_expr *x)
{
  char test;
  assert(x);
  if (is_thunk(x)) {
    // parameterless anonymous closure (thunk)
    pure_expr *ret;
    interpreter& interp = *interpreter::g_interp;
    void *fp = x->data.clos->fp;
    size_t m = x->data.clos->m;
    uint32_t env = 0;
    assert(fp && x->refc > 0);
    // construct a stack frame for the function call
    if (m>0 || !interp.debugging) {
      size_t sz = interp.sstk_sz;
      resize_sstk(interp.sstk, interp.sstk_cap, sz, m+1);
      pure_expr **sstk = interp.sstk;
      env = sz+1;
      sstk[sz++] = 0;
      for (size_t j = 0; j < m; j++) {
	sstk[sz++] = x->data.clos->env[j];
	assert(x->data.clos->env[j]->refc > 0);
	x->data.clos->env[j]->refc++;
      }
#if SSTK_DEBUG
      cerr << "++ stack: (sz = " << sz << ")\n";
      for (size_t i = 0; i < sz; i++) {
	pure_expr *x = sstk[i];
	if (i == interp.sstk_sz) cerr << "** pushed:\n";
	if (x)
	  cerr << i << ": " << (void*)x << ": " << x << '\n';
	else
	  cerr << i << ": " << "** frame **\n";
      }
#endif
      interp.sstk_sz = sz;
    }
#if DEBUG>1
    cerr << "pure_force: calling " << x << " -> " << fp << endl;
    for (size_t j = 0; j < m; j++)
      cerr << "env#" << j << " = " << x->data.clos->env[j] << " -> " << (void*)x->data.clos->env[j] << ", refc = " << x->data.clos->env[j]->refc << '\n';
#endif
    // parameterless call
    if (!interp.checks) { checkall(test); }
    if (m>0)
      ret = ((pure_expr*(*)(uint32_t))fp)(env);
    else
      ret = ((pure_expr*(*)())fp)();
#if DEBUG>1
    cerr << "pure_force: result " << x << " = " << ret << " -> " << (void*)ret << ", refc = " << ret->refc << endl;
#endif
    // check whether the result is again a thunk, then we have to evaluate
    // that recursively
    pure_new_internal(ret);
    if (is_thunk(ret)) pure_force(ret);
    // memoize the result
    assert(x!=ret);
    if (x->data.clos->ep)
      /* XXXKLUDGE: This thunk was created inside a global variable
	 definition. Unlink the thunk from its parent environment (effectively
	 making the environment permanent), so that we don't leave dangling
	 pointers to the thunk procedure, which are bound to cause segfaults
	 later. (This leaks memory on the parent environment if the variable
	 is cleared later, but since this can only happen on the interactive
	 command line, that's a minor issue.) */
      x->data.clos->ep = 0;
    pure_free_clos(x);
    if (x->sy) {
      pure_free_internal(x->sy); x->sy = 0;
    }
    x->tag = ret->tag;
    x->data = ret->data;
    /* XXXFIXME: It's hard to decide what to do with the sentry here, since
       the overwritten thunk and the value being memoized are separate
       objects, so we can't tell which will go the way of the dodo first. In
       principle, the two objects should be linked so that either may call the
       sentry if it's the last one to survive, but that would make things
       awfully complicated. So for the moment we just assume that if the
       return value is a temporary then we take over its sentry, otherwise it
       remains with the original expression. */
    if (ret->refc <= 1) {
      x->sy = ret->sy; ret->sy = 0;
    }
    switch (x->tag) {
    case EXPR::BIGINT:
      mpz_init_set(x->data.z, ret->data.z);
      break;
    case EXPR::APP:
      pure_new_internal(x->data.x[0]);
      pure_new_internal(x->data.x[1]);
      break;
    case EXPR::STR:
      x->data.s = strdup(x->data.s);
      break;
    case EXPR::MATRIX: {
      // Create a new view of the matrix.
      gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
      gsl_matrix_symbolic *m1 =
	(gsl_matrix_symbolic*)malloc(sizeof(gsl_matrix_symbolic));
      assert(m1);
      *m1 = *m; m1->q = 0;
      x->data.mat.p = m1;
      (*x->data.mat.refc)++;
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
      gsl_matrix *m1 = (gsl_matrix*)malloc(sizeof(gsl_matrix));
      assert(m1);
      *m1 = *m;
      x->data.mat.p = m1;
      (*x->data.mat.refc)++;
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
      gsl_matrix_complex *m1 =
	(gsl_matrix_complex*)malloc(sizeof(gsl_matrix_complex));
      assert(m1);
      *m1 = *m;
      x->data.mat.p = m1;
      (*x->data.mat.refc)++;
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
      gsl_matrix_int *m1 =
	(gsl_matrix_int*)malloc(sizeof(gsl_matrix_int));
      assert(m1);
      *m1 = *m;
      x->data.mat.p = m1;
      (*x->data.mat.refc)++;
      break;
    }
    default:
      if (x->tag >= 0) {
	if (x->data.clos)
	  x->data.clos = pure_copy_clos(x->data.clos);
      }
      break;
    }
    pure_free_internal(ret);
    return x;
  } else {
#if DEBUG>2
    if (x->tag >= 0 && x->data.clos)
      cerr << "pure_force: returning " << x << " -> " << x->data.clos->fp
	   << " (" << x->data.clos->n << " args)" << endl;
    else
      cerr << "pure_force: returning " << x << endl;
#endif
    return x;
  }
}

extern "C"
pure_expr *pure_apply(pure_expr *x, pure_expr *y)
{
  char test;
  void *fp = 0;
  assert(x && y && x->refc > 0 && y->refc > 0);
  // if the function in this call is a thunk, evaluate it now
  if (is_thunk(x)) pure_force(x);
  // travel down the spine, count arguments
  pure_expr *f = x, *f0, *ret;
  uint32_t n = 1;
  while (f->tag == EXPR::APP) { f = f->data.x[0]; n++; }
  f0 = f;
  if (f->tag < 0 || !f->data.clos || f->data.clos->n != n ||
      (fp = get_funptr(f)) == 0) {
    // construct a literal application node
    f = new_expr();
    f->tag = EXPR::APP;
    f->data.x[0] = x;
    f->data.x[1] = y;
    MEMDEBUG_NEW(f)
    return f;
  }
  // saturated call; execute it now
  interpreter& interp = *interpreter::g_interp;
  size_t m = f->data.clos->m;
  uint32_t env = 0;
  static pure_expr *argv[MAXARGS];
  assert(n <= MAXARGS && "pure_apply: function call exceeds maximum #args");
  assert(fp && (f->data.clos->local || m == 0));
  // collect arguments
  f = x;
  for (size_t j = 1; f->tag == EXPR::APP; j++, f = f->data.x[0]) {
    assert(f->data.x[1]->refc > 0);
    argv[n-1-j] = f->data.x[1]; f->data.x[1]->refc++;
  }
  argv[n-1] = y;
  // make sure that we do not gc the function before calling it
  f0->refc++; pure_free_internal(x);
  // if we're not debugging, call the function using a trampoline to prevent
  // tail calls from overflowing the stack
  if (!interp.debugging) return trampoline(interp, f0, fp, n, m, argv);
  // no TCO, call the function directly
  if (!interp.checks) { checkall(test); }
  // first push the function object on the shadow stack so that it's
  // garbage-collected in case of an exception
  resize_sstk(interp.sstk, interp.sstk_cap, interp.sstk_sz, n+m+2);
  interp.sstk[interp.sstk_sz++] = f0;
  // construct a stack frame for the function call
  {
    size_t sz = interp.sstk_sz;
    resize_sstk(interp.sstk, interp.sstk_cap, sz, n+m+1);
    pure_expr **sstk = interp.sstk;
    if (m>0) env = sz+n+1;
    sstk[sz++] = 0;
    for (size_t j = 0; j < n; j++)
      sstk[sz++] = (pure_expr*)argv[j];
    for (size_t j = 0; j < m; j++) {
      sstk[sz++] = f0->data.clos->env[j];
      assert(f0->data.clos->env[j]->refc > 0);
      f0->data.clos->env[j]->refc++;
    }
#if SSTK_DEBUG
    cerr << "++ stack: (sz = " << sz << ")\n";
    for (size_t i = 0; i < sz; i++) {
      pure_expr *x = sstk[i];
      if (i == interp.sstk_sz) cerr << "** pushed:\n";
      if (x)
	cerr << i << ": " << (void*)x << ": " << x << '\n';
      else
	cerr << i << ": " << "** frame **\n";
    }
#endif
    interp.sstk_sz = sz;
  }
#if DEBUG>1
  cerr << "pure_apply: calling " << f0 << " -> " << fp << endl;
  for (size_t j = 0; j < n; j++)
    cerr << "arg#" << j << " = " << (pure_expr*)argv[j] << " -> " << argv[j] << ", refc = " << ((pure_expr*)argv[j])->refc << '\n';
  for (size_t j = 0; j < m; j++)
    cerr << "env#" << j << " = " << f0->data.clos->env[j] << " -> " << (void*)f0->data.clos->env[j] << ", refc = " << f0->data.clos->env[j]->refc << '\n';
#endif
  if (m>0)
    xfuncall(ret, fp, n, env, argv)
    else
      funcall(ret, fp, n, argv)
#if DEBUG>1
	cerr << "pure_apply: result " << f0 << " = " << ret << " -> " << (void*)ret << ", refc = " << ret->refc << endl;
#endif
  /* Temporarily increase the reference count on the result in case it comes
     from the environment, so that we don't gc it with the function object.
     We only need to do this if the result isn't a temporary and we actually
     have an environment. */
  {
    bool keep = m>0 && ret->refc>0;
    if (keep) ret->refc++;
    // pop the function object from the shadow stack
    pure_free_internal(interp.sstk[--interp.sstk_sz]);
    if (keep) pure_unref_internal(ret);
  }
  return ret;
}

extern "C"
pure_expr *pure_applc(pure_expr *x, pure_expr *y)
{
  pure_expr *f = new_expr();
  f->tag = EXPR::APP;
  f->data.x[0] = x;
  f->data.x[1] = y;
  MEMDEBUG_NEW(f)
  return f;
}

extern "C"
void pure_break(void)
{
  interpreter& interp = *interpreter::g_interp;
  if (interp.debugging && !interp.debug_skip && interp.stoplevel >= 0)
    interp.stoplevel = interp.debug_info.size();
}

extern "C"
void pure_trace(void)
{
  interpreter& interp = *interpreter::g_interp;
  if (interp.debugging && interp.tracelevel < 0 &&
      interp.debug_info.size() > 0)
    interp.tracelevel = interp.debug_info.size()-1;
}

extern "C"
void pure_throw(pure_expr* e)
{
  interpreter::brkflag = 0;
  interpreter& interp = *interpreter::g_interp;
  // get rid of indirect call frames
  while (interp.astk && interp.astk->count) {
    if (interp.astk->e) pure_free_internal(interp.astk->e);
    interp.pop_aframe();
  }
  if (!interp.astk) {
    if (e)
      cerr << "throw: unhandled exception '" << e << "'\n";
    else
      // No exception value. This most likely indicates a failed match. Report
      // that to the user.
      cerr << "throw: unhandled exception (failed match)\n";
    // Let's be a little more verbose here for the benefit of the user who may
    // not even be aware that he's running a Pure program.
    cerr << "\n\
Your Pure program has died with an unhandled exception. Running the program\n\
with the debugger (pure -g) may provide additional information.\n\n\
For further help and information about Pure please try the 'help' command in\n\
the interpreter or visit https://agraef.github.io/pure-lang.\n\n";
    abort(); // no exception handler, bail out
  } else {
    interp.astk->e = e;
    longjmp(interp.astk->jmp, 1);
  }
}

extern "C"
void pure_unresolved()
{
  pure_throw(unresolved_exception());
}

#include <signal.h>

extern "C"
void pure_sigfpe(void)
{
  pure_throw(signal_exception(SIGFPE));
}

static void sig_handler(int sig)
{
#ifdef MUST_REINSTALL_SIGHANDLERS
  signal(sig, sig_handler);
#endif
  interpreter::brkflag = sig;
}

extern "C"
void pure_trap(int32_t action, int32_t sig)
{
  if (action > 0)
    signal(sig, sig_handler);
  else if (action < 0)
    signal(sig, SIG_IGN);
  else
    signal(sig, SIG_DFL);
}

// Debugging routines.
static string pname(interpreter& interp, Env *e);
static string printx(pure_expr *x, size_t n = 30);

extern "C"
pure_expr *pure_catch(pure_expr *h, pure_expr *x)
{
  char test;
  void *fp = 0;
  assert(h && x);
  if (x->tag >= 0 && x->data.clos && x->data.clos->n == 0 &&
      (fp = get_funptr(x)) != 0) {
    interpreter& interp = *interpreter::g_interp;
#if DEBUG>1
    cerr << "pure_catch: calling " << x << " -> " << fp << endl;
#endif
    assert(h->refc > 0 && x->refc > 0);
    size_t m = x->data.clos->m;
    assert(fp && (x->data.clos->local || m == 0));
    pure_expr **env = 0;
    size_t oldsz = interp.sstk_sz, old_di_sz = 0;
    if (interp.debugging) old_di_sz = interp.debug_info.size();
    if (m>0 || !interp.debugging) {
      // construct a stack frame
      size_t sz = oldsz;
      resize_sstk(interp.sstk, interp.sstk_cap, sz, m+1);
      pure_expr **sstk = interp.sstk; env = sstk+sz+1;
      sstk[sz++] = 0;
      for (size_t j = 0; j < m; j++) {
	sstk[sz++] = x->data.clos->env[j];
	assert(env[j]->refc > 0); env[j]->refc++;
      }
#if SSTK_DEBUG
      cerr << "++ stack: (sz = " << sz << ")\n";
      for (size_t i = 0; i < sz; i++) {
	pure_expr *x = sstk[i];
	if (i == interp.sstk_sz) cerr << "** pushed:\n";
	if (x)
	  cerr << i << ": " << (void*)x << ": " << x << '\n';
	else
	  cerr << i << ": " << "** frame **\n";
      }
#endif
      interp.sstk_sz = sz;
    }
    checkstk(test);
    // Push an exception.
    pure_aframe *ex = interp.push_aframe(oldsz);
    // Save old temporaries.
    pure_expr *old_tmps = interp.tmps; interp.tmps = 0;
    // Call the function now. Catch exceptions generated by the runtime.
    if (setjmp(ex->jmp)) {
      // caught an exception
      size_t sz = ex->sz;
      pure_expr *e = ex->e;
      interp.pop_aframe();
      if (e) pure_new_internal(e);
      // collect new garbage
      pure_expr *tmps = interp.tmps;
      while (tmps) {
	pure_expr *next = tmps->xp;
#if DEBUG>2
	cerr << "collecting new garbage: " << tmps << '\n';
#endif
	pure_freenew(tmps);
	tmps = next;
      }
      // restore the old list of temporaries
      interp.tmps = old_tmps;
      for (size_t i = interp.sstk_sz; i-- > sz; )
	if (interp.sstk[i] && interp.sstk[i]->refc > 0)
	  pure_free_internal(interp.sstk[i]);
      interp.sstk_sz = sz;
      if (interp.debugging) {
	if (!interp.debug_info.empty()) {
	  DebugInfo& d = interp.debug_info.back();
	  if (interp.stopped(d.e) || interp.traced(d.e)) {
	    /* We're running in debugging mode. Print a little debugging
	       message so that the user knows what's going on. */
	    cout << "++ [" << d.n << "] " << pname(interp, d.e)
		 << ": *** caught exception ***\n";
	    if (e)
	      cout << "     --> " << printx(e, 68) << endl;
	  }
	}
	/* Unwind the debug info. */
	size_t new_di_sz = interp.debug_info.size();
	while (new_di_sz-- > old_di_sz)
	  interp.debug_info.pop_back();
	if (interp.tracelevel >= 0 &&
	    (uint32_t)interp.tracelevel > interp.debug_info.size())
	  interp.tracelevel = -1;
      }
      if (!e)
	e = pure_new_internal(mk_void());
      assert(e && e->refc > 0);
#if DEBUG>1
      cerr << "pure_catch: exception " << (void*)e << " (refc = " << e->refc
	   << "): " << e << endl;
#endif
      pure_free_internal(x);
      /* Mask further breaks while the handler is executing. */
      interp.brkmask = 1;
      pure_expr *res = pure_apply(h, e);
      /* This value indicates that we're done handling the signal. Further
	 breaks will still be masked until we do the next signal check. */
      interp.brkmask = 2;
      return res;
    } else {
      pure_expr *res;
      interp.brkmask = 0;
      if (env)
	// pass environment
	res = ((pure_expr*(*)(uint32_t))fp)(env-interp.sstk);
      else
	// parameterless call
	res = ((pure_expr*(*)())fp)();
      // check for pending signals
      checkall(test);
      // normal return
      interp.pop_aframe();
      /* Collect any new garbage that might have accumulated during the
	 call. Normally there shouldn't be any, but it might be that some
	 badly written external function does this. */
      pure_expr *tmps = interp.tmps;
      while (tmps) {
	pure_expr *next = tmps->xp;
	if (tmps != res) {
#if DEBUG>2
	  cerr << "uncollected temporary: " << tmps << '\n';
#endif
	  pure_freenew(tmps);
	}
	tmps = next;
      }
      // restore the old list of temporaries
      interp.tmps = old_tmps;
      assert(res);
      res->refc++;
      pure_free_internal(h); pure_free_internal(x);
      pure_unref_internal(res);
      return res;
    }
  } else {
    pure_free_internal(h);
    pure_unref_internal(x);
    return x;
  }
}

static void pure_debug_backtrace(ostream& out);

extern "C"
pure_expr *pure_invoke(void *f, pure_expr** _e)
{
  assert(_e);
  pure_expr*& e = *_e;
  interpreter& interp = *interpreter::g_interp;
  // Cast the function pointer to the right type (takes no arguments, returns
  // a pure_expr*), so we can call it as a native function.
  pure_expr *(*fp)() = (pure_expr*(*)())f;
#if DEBUG>1
  cerr << "pure_invoke: calling " << f << endl;
#endif
  MEMDEBUG_INIT
  // Push an exception.
  pure_aframe *ex = interp.push_aframe(interp.sstk_sz);
  // Save old temporaries.
  pure_expr *old_tmps = interp.tmps; interp.tmps = 0;
  // Call the function now. Catch exceptions generated by the runtime.
  size_t oldsz = interp.sstk_sz;
  if (!interp.debugging) pure_push_args(0, 0);
  if (setjmp(ex->jmp)) {
    // caught an exception
    size_t sz = ex->sz;
    e = ex->e;
    if (!interp.astk->prev && interp.debugging) {
      if (interp.debug_info.empty())
	interp.bt.clear();
      else {
	// record a backtrace
	ostringstream out;
	pure_debug_backtrace(out);
	interp.bt = out.str();
      }
    }
    interp.pop_aframe();
    if (e) pure_new_internal(e);
    // collect new garbage
    pure_expr *tmps = interp.tmps;
    while (tmps) {
      pure_expr *next = tmps->xp;
#if DEBUG>2
      cerr << "collecting new garbage: " << tmps << '\n';
#endif
      pure_freenew(tmps);
      tmps = next;
    }
    // restore the old list of temporaries
    interp.tmps = old_tmps;
    for (size_t i = interp.sstk_sz; i-- > sz; )
      if (interp.sstk[i] && interp.sstk[i]->refc > 0)
	pure_free_internal(interp.sstk[i]);
    interp.sstk_sz = sz;
#if DEBUG>1
    if (e)
      cerr << "pure_invoke: exception " << (void*)e << " (refc = " << e->refc
	   << "): " << e << endl;
#endif
    MEMDEBUG_SUMMARY(e)
    return 0;
  } else {
    pure_expr *res = fp();
    // normal return
    interp.pop_aframe();
    MEMDEBUG_SUMMARY(res)
    assert(res);
    res->refc++;
    /* Collect any new garbage that might have accumulated during the
       call. Normally there shouldn't be any, but it might be that some badly
       written external function does this. */
    pure_expr *tmps = interp.tmps;
    while (tmps) {
      pure_expr *next = tmps->xp;
      if (tmps != res) {
#if DEBUG>2
	cerr << "uncollected temporary: " << tmps << '\n';
#endif
	pure_freenew(tmps);
      }
      tmps = next;
    }
    // restore the old list of temporaries
    interp.tmps = old_tmps;
    pure_unref_internal(res);
    if (interp.sstk_sz > oldsz) {
      // The called function didn't clean up our stack frame, do it now.
      assert(interp.sstk_sz == oldsz+1);
      interp.sstk_sz = oldsz;
    }
    return res;
  }
}

extern "C"
void pure_new_args(uint32_t n, ...)
{
  va_list ap;
  va_start(ap, n);
  while (n-- > 0) {
    pure_expr *x = va_arg(ap, pure_expr*);
    if (x->refc > 0)
      x->refc++;
    else
      pure_new_internal(x);
  };
  va_end(ap);
}

extern "C"
void pure_free_args(pure_expr *x, uint32_t n, ...)
{
  va_list ap;
  if (x) x->refc++;
  va_start(ap, n);
  while (n-- > 0) {
    pure_expr *x = va_arg(ap, pure_expr*);
    if (x->refc > 1)
      x->refc--;
    else
      pure_free_internal(x);
  };
  va_end(ap);
  if (x) pure_unref_internal(x);
}

extern "C"
uint32_t pure_push_args(uint32_t n, uint32_t m, ...)
{
  va_list ap;
  interpreter& interp = *interpreter::g_interp;
  size_t sz = interp.sstk_sz;
  resize_sstk(interp.sstk, interp.sstk_cap, sz, n+m+1);
  pure_expr **sstk = interp.sstk; uint32_t env = (m>0)?sz+n+1:0;
  // mark the beginning of this frame
  sstk[sz++] = 0;
  pure_expr **frame = sstk+sz;
  const uint32_t k = n+m;
  va_start(ap, m);
  for (uint32_t i = 0; i < k; i++) {
    pure_expr *x = va_arg(ap, pure_expr*);
    sstk[sz++] = x;
  };
  va_end(ap);
  /* The reference counts are updated in reverse which is faster in the common
     case that temporary arguments have been generated in order. */
  for (uint32_t i = k; i > 0; ) {
    pure_expr *x = frame[--i];
    if (x->refc > 0)
      x->refc++;
    else
      pure_new_internal(x);
  }
#if SSTK_DEBUG
  cerr << "++ stack: (sz = " << sz << ")\n";
  for (size_t i = 0; i < sz; i++) {
    pure_expr *x = sstk[i];
    if (i == interp.sstk_sz) cerr << "** pushed:\n";
    if (x)
      cerr << i << ": " << (void*)x << ": " << x << '\n';
    else
      cerr << i << ": " << "** frame **\n";
  }
#endif
  interp.sstk_sz = sz;
  // return a pointer to the environment:
  return env;
}

// This is used by pure_funcall().
static uint32_t pure_push_argv(uint32_t n, uint32_t m, pure_expr **args)
{
  interpreter& interp = *interpreter::g_interp;
  size_t sz = interp.sstk_sz;
  resize_sstk(interp.sstk, interp.sstk_cap, sz, n+m+1);
  pure_expr **sstk = interp.sstk; uint32_t env = (m>0)?sz+n+1:0;
  // mark the beginning of this frame
  sstk[sz++] = 0;
  pure_expr **frame = sstk+sz;
  const uint32_t k = n+m;
  for (uint32_t i = 0; i < k; i++) {
    pure_expr *x = args[i];
    sstk[sz++] = x;
  };
  /* The reference counts are updated in reverse which is faster in the common
     case that temporary arguments have been generated in order. */
  for (uint32_t i = k; i > 0; ) {
    pure_expr *x = frame[--i];
    if (x->refc > 0)
      x->refc++;
    else
      pure_new_internal(x);
  }
#if SSTK_DEBUG
  cerr << "++ stack: (sz = " << sz << ")\n";
  for (size_t i = 0; i < sz; i++) {
    pure_expr *x = sstk[i];
    if (i == interp.sstk_sz) cerr << "** pushed:\n";
    if (x)
      cerr << i << ": " << (void*)x << ": " << x << '\n';
    else
      cerr << i << ": " << "** frame **\n";
  }
#endif
  interp.sstk_sz = sz;
  // return a pointer to the environment:
  return env;
}

extern "C"
void pure_pop_args(pure_expr *x, uint32_t n, uint32_t m)
{
  interpreter& interp = *interpreter::g_interp;
  pure_expr **sstk = interp.sstk;
  size_t sz = interp.sstk_sz;
#if !defined(NDEBUG) || SSTK_DEBUG
  size_t oldsz = sz;
#endif
  sz -= n+m+1;
  assert(sz < oldsz && !sstk[sz]);
#if SSTK_DEBUG
  cerr << "++ stack: (oldsz = " << oldsz << ")\n";
  for (size_t i = 0; i < oldsz; i++) {
    pure_expr *x = sstk[i];
    if (i == sz) cerr << "** popped:\n";
    if (x)
      cerr << i << ": " << (void*)x << ": " << x << '\n';
    else
      cerr << i << ": " << "** frame **\n";
  }
#endif
  if (x) x->refc++;
  for (size_t i = 0; i < n+m; i++) {
    pure_expr *x = sstk[sz+1+i];
    assert(x);
    if (x->refc > 1)
      x->refc--;
    else
      pure_free_internal(x);
  };
  if (x) pure_unref_internal(x);
  interp.sstk_sz = sz;
}

extern "C"
void pure_pop_tail_args(pure_expr *x, uint32_t n, uint32_t m)
{
  interpreter& interp = *interpreter::g_interp;
  pure_expr **sstk = interp.sstk;
  size_t sz, lastsz = interp.sstk_sz, oldsz = lastsz;
  while (lastsz > 0 && sstk[--lastsz]) ;
  assert(lastsz < oldsz && !sstk[lastsz]);
  sz = lastsz-(n+m+1);
  assert(sz < lastsz && !sstk[sz]);
#if SSTK_DEBUG
  cerr << "++ stack: (oldsz = " << oldsz << ", lastsz = " << lastsz << ")\n";
  for (size_t i = 0; i < oldsz; i++) {
    pure_expr *x = sstk[i];
    if (i == sz) cerr << "** popped:\n";
    if (i == lastsz) cerr << "** moved:\n";
    if (x)
      cerr << i << ": " << (void*)x << ": " << x << '\n';
    else
      cerr << i << ": " << "** frame **\n";
  }
#endif
  if (x) x->refc++;
  for (size_t i = 0; i < n+m; i++) {
    pure_expr *x = sstk[sz+1+i];
    assert(x);
    if (x->refc > 1)
      x->refc--;
    else
      pure_free_internal(x);
  };
  if (x) pure_unref_internal(x);
  memmove(sstk+sz, sstk+lastsz, (oldsz-lastsz)*sizeof(pure_expr*));
  interp.sstk_sz -= n+m+1;
}

extern "C"
void pure_push_arg(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  size_t sz = interp.sstk_sz;
  resize_sstk(interp.sstk, interp.sstk_cap, sz, 2);
  pure_expr** sstk = interp.sstk;
  sstk[sz++] = 0; sstk[sz++] = x;
  if (x->refc > 0)
    x->refc++;
  else
    pure_new_internal(x);
#if SSTK_DEBUG
  cerr << "++ stack: (sz = " << sz << ")\n";
  for (size_t i = 0; i < sz; i++) {
    pure_expr *x = sstk[i];
    if (i == interp.sstk_sz) cerr << "** pushed:\n";
    if (x)
      cerr << i << ": " << (void*)x << ": " << x << '\n';
    else
      cerr << i << ": " << "** frame **\n";
  }
#endif
  interp.sstk_sz = sz;
}

extern "C"
void pure_pop_arg(pure_expr *y)
{
#if SSTK_DEBUG
  pure_pop_args(y, 1, 0);
#else
  interpreter& interp = *interpreter::g_interp;
  pure_expr *x = interp.sstk[interp.sstk_sz-1];
  if (y) y->refc++;
  if (x->refc > 1)
    x->refc--;
  else
    pure_free_internal(x);
  if (y) pure_unref_internal(y);
  interp.sstk_sz -= 2;
#endif
}

extern "C"
void pure_pop_tail_arg(pure_expr *y)
{
#if SSTK_DEBUG
  pure_pop_tail_args(y, 1, 0);
#else
  interpreter& interp = *interpreter::g_interp;
  pure_expr **sstk = interp.sstk;
  size_t lastsz = interp.sstk_sz, oldsz = lastsz;
  while (lastsz > 0 && sstk[--lastsz]) ;
  pure_expr *x = interp.sstk[lastsz-1];
  if (y) y->refc++;
  if (x->refc > 1)
    x->refc--;
  else
    pure_free_internal(x);
  if (y) pure_unref_internal(y);
  memmove(sstk+lastsz-2, sstk+lastsz, (oldsz-lastsz)*sizeof(pure_expr*));
  interp.sstk_sz -= 2;
#endif
}

extern "C"
void pure_checks(void)
{
  char test;
  checkall(test);
}

extern "C"
void pure_debug(int32_t tag, const char *format, ...)
{
  cout << "break at ";
  if (tag > 0)
    cout << interpreter::g_interp->symtab.sym(tag).s;
  else
    cout << "<<anonymous closure>>";
  cout << ": ";
  va_list ap;
  va_start(ap, format);
  vprintf(format, ap);
  va_end(ap);
  static bool init = false;
  if (!init) {
    cout << "\n(Press 'x' to exit the interpreter, <cr> to continue, <eof> to run unattended.)";
    init = true;
  }
  cout << "\n: ";
  char ans;
  cin >> noskipws >> ans;
  bool bail_out = ans=='x';
  while (cin.good() && ans != '\n') cin >> noskipws >> ans;
  if (!cin.good()) cout << "\n";
  if (bail_out) exit(0);
}

static size_t argno(size_t n, path &p)
{
  size_t m = p.len(), k = n-1;
  size_t i;
  for (i = 0; i < m && p[i] == 0; i++) {
    assert(k>0); --k;
  }
  assert(i < m && p[i] == 1);
  path q(m-++i);
  for (size_t j = 0; i < m; i++, j++) {
    q.set(j, p[i]);
    q.setmsk(j, p.msk(i));
  }
  p = q;
  return k;
}

static uint32_t argidx(const path &p, size_t &i)
{
  size_t i0 = i;
  while (i < p.len() && p[i]) ++i;
  assert(i < p.len() && !p[i]);
  return (i++)-i0;
}

static pure_expr *subterm(size_t n, pure_expr **xs, path p, bool b)
{
  pure_expr *tmp = 0;
  size_t k = 0;
  if (b)
    // pattern binding
    assert(n==1);
  else
    k = argno(n, p);
  pure_expr *x = xs[k];
  for (size_t i = 0, m = p.len(); i < m; ) {
    if (p.msk(i)) {
      // matrix path
      uint32_t r = argidx(p, i), c = argidx(p, i);
      switch (x->tag) {
      case EXPR::MATRIX: {
	gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
	assert(r < m->size1 && c < m->size2);
	x = m->data[r * m->tda + c];
	break;
      }
      case EXPR::DMATRIX: {
	gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
	assert(r < m->size1 && c < m->size2);
	x = pure_double(m->data[r * m->tda + c]);
	break;
      }
      case EXPR::IMATRIX: {
	gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
	assert(r < m->size1 && c < m->size2);
	x = pure_int(m->data[r * m->tda + c]);
	break;
      }
      case EXPR::CMATRIX: {
	gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
	assert(r < m->size1 && c < m->size2);
	size_t k = 2*(r * m->tda + c);
	x = make_complex(m->data[k], m->data[k+1]);
	if (i < p.len()) tmp = x;
	break;
      }
      default:
	assert(0);
	break;
      }
    } else {
      assert(x->tag == EXPR::APP);
      x = x->data.x[p[i++]?1:0];
    }
  }
  if (tmp) {
    pure_new(x); pure_freenew(tmp); pure_unref(x);
  }
  return x;
}

static string printx(pure_expr *x, size_t n)
{
  static const string dots = "...";
  static const size_t l = dots.size();
  ostringstream sout;
  sout << x;
  string s = sout.str();
  if (s.size() > n+l) {
    s.erase(n);
    s += dots;
  }
  return s;
}

static void get_vars(interpreter& interp, list<DebugInfo>::reverse_iterator kt)
{
  // find the arguments and environment of this call and all subsequent calls
  // on the shadow stack
  pure_expr **sstk = interp.sstk;
  size_t sz = interp.sstk_sz;
  list<DebugInfo>::reverse_iterator it = interp.debug_info.rbegin(),
    end = interp.debug_info.rend();
  do {
    DebugInfo& d = *it;
#if 0
    cout << d.n << "@" << sz << ": " << d.e->name
	 << "(" << d.e->n << ", " << d.e->m << ")\n";
#endif
    if (d.e->n+d.e->m > 0) {
      assert(d.e->n+d.e->m < sz);
      sz -= d.e->n+d.e->m+1;
      while (sz > 0 && sstk[sz]) --sz;
      d.args = sstk+sz+1;
      d.envs = d.args+d.e->n;
    } else
      d.args = d.envs = 0;
  } while (it++ != kt && it != end);
}

static void print_vars(ostream& out, interpreter& interp, DebugInfo& d)
{
  map<string,pure_expr*> vals;
  if (d.r) {
    for (env::iterator it = d.vars.begin(); it != d.vars.end(); ++it) {
      int32_t vno = it->first;
      symbol& sym = interp.symtab.sym(vno);
      env_info& info = it->second;
      assert(info.t == env_info::lvar);
      vals[sym.s] = subterm(d.e->n, d.args, *info.p, d.e->b);
    }
    for (list<VarInfo>::iterator it = d.e->xtab.begin(); it != d.e->xtab.end();
	 ++it) {
      VarInfo& info = *it;
      symbol& sym = interp.symtab.sym(info.vtag);
      assert(info.v < d.e->m);
      vals[sym.s] = d.envs[info.v];
    }
  } else {
    // This is an external. There are no variable symbols for the arguments
    // right now, so we need to make up our own.
    for (uint32_t i = 0; i < d.e->n; i++) {
      char buf[100];
      sprintf(buf, "x%u", i+1);
      assert(d.args[i]);
      vals[buf] = d.args[i];
    }
  }
  if (!vals.empty()) out << "     ";
  size_t count = 0;
  for (map<string,pure_expr*>::iterator it = vals.begin(); it != vals.end();
       ++it, ++count) {
    const string& id = it->first;
    pure_expr *x = it->second;
    if (count > 0) out << "; ";
    out << id << " = " << printx(x);
  }
  if (count > 0) out << endl;
  // get rid of temporaries
  for (map<string,pure_expr*>::iterator it = vals.begin(); it != vals.end();
       ++it) {
    pure_expr *x = it->second;
    if (x->refc == 0) pure_freenew(x);
  }
}

static expr localvars(interpreter& interp, DebugInfo& d, pure_expr *x)
{
  // convert to compile time expression
  expr y = interp.pure_expr_to_expr(x);
  map<int32_t,pure_expr*> vals;
  if (d.r) {
    for (env::iterator it = d.vars.begin(); it != d.vars.end(); ++it) {
      int32_t vno = it->first;
      env_info& info = it->second;
      assert(info.t == env_info::lvar);
      vals[vno] = subterm(d.e->n, d.args, *info.p, d.e->b);
    }
    for (list<VarInfo>::iterator it = d.e->xtab.begin(); it != d.e->xtab.end();
	 ++it) {
      VarInfo& info = *it;
      assert(info.v < d.e->m);
      vals[info.vtag] = d.envs[info.v];
    }
  } else {
    // This is an external. There are no variable symbols for the arguments
    // right now, so we need to make up our own.
    for (uint32_t i = 0; i < d.e->n; i++) {
      char buf[100];
      sprintf(buf, "::x%u", i+1);
      symbol *sym = interp.symtab.sym(buf);
      if (sym) vals[sym->f] = d.args[i];
    }
  }
  // build the rule list
  rulel *r = new rulel;
  // at present, the compiler can't build 'when' expressions with more than
  // 256 definitions, so we enforce that limit here
  size_t count = 0;
  for (map<int32_t,pure_expr*>::iterator it = vals.begin();
       it != vals.end() && count < 0x100; ++it, ++count) {
    int32_t v = it->first;
    pure_expr *x = it->second;
    r->push_back(rule(expr(v), interp.pure_expr_to_expr(x)));
  }
  // get rid of temporaries
  for (map<int32_t,pure_expr*>::iterator it = vals.begin(); it != vals.end();
       ++it) {
    pure_expr *x = it->second;
    if (x->refc == 0) pure_freenew(x);
  }
  if (r->empty()) {
    // no locals, return y as it is
    delete r;
    return y;
  } else {
    try {
      // build a 'when' expression with all the locals
      expr *z = interp.mkwhen_expr(new expr(y), r);
      expr ret = *z;
      delete z;
      return ret;
    } catch (err &e) {
      // error in constructing the 'when' expression, pretend that we
      // succeeded anyway (this should never happen)
      return y;
    }
  }
}

static string psym(const string& s, bool local = false)
{
  if (local) {
    size_t pos = symsplit(s);
    if (pos != string::npos)
      return s.substr(pos+2);
    else
      return s;
  } else
    return s;
}

static string pname(interpreter& interp, Env *e)
{
  if (e->tag > 0) {
    ostringstream sout;
    sout << interp.symtab.sym(e->tag).x;
    string s = sout.str();
    return (s[0]!='(' && e->local)?psym(s, true):s;
  } else if (e->descr)
    return "#<"+string(e->descr)+">";
  else
    return "#<closure>";
}

static void parse_cmd(string& cmdline, string& cmd, string& arg, char escape_mode)
{
  // strip trailing whitespace
  size_t p = cmdline.find_last_not_of(" \t");
  if (p != string::npos)
    cmdline.erase(p+1);
  else
    cmdline.clear();
  // strip leading whitespace
  p = cmdline.find_first_not_of(" \t");
  if (p != string::npos)
    cmdline.erase(0, p);
  else
    cmdline.clear();
  // tokenize
  if (!cmdline.empty()) {
    if (cmdline[0] == '?' || cmdline[0] == '.') {
      cmd = cmdline.substr(0, 1);
      arg = cmdline.substr(1);
      return;
    }
    if (!escape_mode || escape_mode == '!') {
      if (cmdline[0] == '!') {
        cmd = "!";
        arg = cmdline.substr(1);
        return;
      }
    } else if (cmdline[0] == escape_mode) {
        cmd = "!";
        arg = cmdline.substr(1);
        return;
    } else if (cmdline[0] == '!') {
        cmd = "!";
        arg = cmdline;
        return;
    }
  }
  if (cmdline.substr(0, 2) == "//") {
    cmd = cmdline.substr(0, 2);
    arg = cmdline.substr(2);
    return;
  }
  p = cmdline.find_first_of(" \t");
  if (p != string::npos) {
    cmd = cmdline.substr(0, p);
    p = cmdline.find_first_not_of(" \t", p);
    assert(p != string::npos);
    arg = cmdline.substr(p);
  } else {
    cmd = cmdline; arg = "";
  }
}

static const char *stacklab(interpreter& interp,
			    list<DebugInfo>::reverse_iterator it,
			    list<DebugInfo>::reverse_iterator kt)
{
  return (it == interp.debug_info.rbegin())?"**":(it == kt)?">>":"  ";
}

static const char *stacklab(interpreter& interp,
			    list<DebugInfo>::iterator it)
{
  list<DebugInfo>::iterator jt = it;
  return (++jt == interp.debug_info.end())?">>":"  ";
}

static const bool yes_or_no(const string& msg)
{
  char ans;
  cout << TEXMACS_BEGIN_PROMPT << msg << " " << TEXMACS_END_PROMPT;
  cout.flush();
  cin >> noskipws >> ans;
  bool res = cin.good() && ans == 'y';
  while (cin.good() && ans != '\n') cin >> noskipws >> ans;
  if (!cin.good()) cout << endl;
  cin.clear();
  return res;
}

static void pure_debug_backtrace(ostream& out)
{
  interpreter& interp = *interpreter::g_interp;
  list<DebugInfo>::iterator it = interp.debug_info.begin(),
    end = interp.debug_info.end();
  if (it == end) return;
  size_t save_sz = interp.sstk_sz;
  interp.sstk_sz = interp.debug_info.back().sz;
  get_vars(interp, interp.debug_info.rend());
  for (; it != end; ++it) {
    DebugInfo& d = *it;
    if (d.r) {
      out << stacklab(interp, it) << " [" << d.n << "] "
	  << pname(interp, d.e) << ": " << *d.r << ";\n";
    } else if (d.e->tag > 0 &&
	       interp.externals.find(d.e->tag) != interp.externals.end()) {
      ExternInfo &info = interp.externals[d.e->tag];
      out << stacklab(interp, it) << " [" << d.n << "] "
	  << pname(interp, d.e) << ": " << info << ";\n";
    } else {
      out << stacklab(interp, it) << " [" << d.n << "] "
	  << pname(interp, d.e) << ":\n";
    }
    print_vars(out, interp, d);
  }
  interp.sstk_sz = save_sz;
}

extern "C"
void pure_debug_rule(void *_e, void *_r)
{
  Env *e = (Env*)_e;
  rule *r = (rule*)_r;
  interpreter& interp = *interpreter::g_interp;
  if (!interp.interactive) return;
  if (!r) {
    // push a new activation record
    interp.debug_info.push_back(DebugInfo(interp.debug_info.size()+1,
					  interp.sstk_sz, e));
    if (e->tag <= 0 ||
	interp.externals.find(e->tag) == interp.externals.end())
      return;
  }
  assert(!interp.debug_info.empty());
  DebugInfo& d = interp.debug_info.back();
#if 0
  if (d.e != e) {
    cerr << "unexpected environment: stacktop " << d.e << " ("
	 << pname(interp, d.e) << ") != " << e << endl;
    assert(r);
    cerr << "** [" << d.n << "] "
	 << pname(interp, e) << ": " << *r << ";\n";
  }
#endif
  assert(d.e == e);
  d.r = r;
  if (r) {
    // build the lhs variable table
    d.vars.clear();
    vinfo vi; // ignored
    interp.bind(d.vars, vi, r->lhs, e->b);
  }
  bool stp = interp.stopped(e);
  if (!stp && !interp.traced(e)) return;
  if (r) {
    cout << "** [" << d.n << "] "
	 << pname(interp, e) << ": " << *r << ";\n";
  } else if (e->tag > 0 &&
	     interp.externals.find(e->tag) != interp.externals.end()) {
    ExternInfo &info = interp.externals[e->tag];
    cout << "** [" << d.n << "] "
	 << pname(interp, e) << ": " << info << ";\n";
  } else {
    cout << "** [" << d.n << "] "
	 << pname(interp, e) << ":\n";
  }
  get_vars(interp, interp.debug_info.rbegin());
  print_vars(cout, interp, d);
  if (!stp) return;
  interp.stoplevel = -1;
  interp.debug_skip = false;
  static bool init = false;
  if (!init) {
    cout << "(Type 'h' for help.)\n";
    init = true;
  }
  list<DebugInfo>::reverse_iterator kt = interp.debug_info.rbegin();
  bool done = false;
  while (!done) {
    string cmdline, cmd, arg;
    extern char *(*command_input2)(const char *prompt);
    if (!cin.eof() && command_input2) {
      char *s = command_input2(": ");
      if (s)
	cmdline = s;
      else {
	cmdline = "";
	cin.clear(ios_base::eofbit);
      }
    } else {
      cout << TEXMACS_BEGIN_PROMPT << ": " << TEXMACS_END_PROMPT;
      cout.flush();
      getline(cin, cmdline);
    }
    parse_cmd(cmdline, cmd, arg, interp.escape_mode);
    if (!cin.good()) cout << endl;
    if (cmd=="//")
      // comment line
      ;
    else if (cmd=="!") {
      // command escape
      char *res = pure_evalcmd(arg.c_str());
      if (res) {
	cout << res;
	free(res);
      } else if (!interp.errmsg.empty()) {
	cerr << interp.errmsg;
	interp.errmsg.clear(); interp.errpos.clear();
      }
    } else if (cmd=="?" || cmd.size()>1 ||
	       (!cmd.empty() && !isalpha(cmd[0]) && cmd[0] != '.')) {
      // eval
      if (cmd != "?") arg = cmdline;
      size_t p = arg.find_first_not_of(" \t");
      if (p != string::npos)
	arg.erase(0, p);
      else
	arg.clear();
      p = arg.find_last_not_of(";");
      if (p != string::npos)
	arg.erase(p+1);
      else
	arg.clear();
      if (!arg.empty()) {
	DebugInfo& d = *kt;
	// make sure we don't invoke the debugger recursively
	interp.interactive = false;
	//interp.restricted = true;
	string s = "'("+arg+");";
	pure_expr *x = interp.runstr(s), *e;
	if (x) {
	  // supply the environment
	  get_vars(interp, kt);
	  expr y = localvars(interp, d, x);
	  // evaluate
	  x = interp.eval(y, e, false);
	  if (x) {
	    cout << x << endl;
	    pure_freenew(x);
	  } else if (e) {
	    cerr << "unhandled exception '" << e << "' while evaluating '"
		 << arg << "'\n";
	    pure_free(e);
	  } else
	    cerr << "unhandled exception while evaluating '" << arg << "'\n";
	} else {
	  // Only print the first error message if any.
	  string msg = interp.errmsg;
	  size_t p = msg.find('\n');
	  if (p != string::npos) msg.erase(p);
	  // Do some more cosmetic surgery.
	  p = msg.find(", unexpected ");
	  if (p != string::npos) msg.erase(p);
	  if (!msg.empty()) cerr << msg << endl;
	  interp.errmsg.clear(); interp.errpos.clear();
	}
	interp.interactive = true;
	//interp.restricted = false;
      }
    } else if (cmdline == "" || cmd == "s") {
      // single step
      done = true;
    } else if (cmd == "a") {
      // auto
      done = true;
      cin.clear(ios_base::eofbit);
    } else switch (cmd[0]) {
    case 'n': {
      // next step
      DebugInfo& d = *kt;
      interp.stoplevel = d.n;
      interp.debug_skip = true;
      done = true;
      break;
    }
    case 'r':
      // run unattended
      interp.stoplevel = 0;
      interp.debug_skip = true;
      done = true;
      break;
    case 'c': {
      // continue until breakpoint
      if (!arg.empty()) {
	int32_t f = pure_getsym(arg.c_str());
	if (f > 0) {
	  env::const_iterator jt = interp.globenv.find(f);
	  if ((jt != interp.globenv.end() && jt->second.t == env_info::fun) ||
	      interp.externals.find(f) != interp.externals.end()) {
	    if (interp.breakpoints.find(f) == interp.breakpoints.end())
	      interp.tmp_breakpoints.insert(f);
	  } else
	    f = 0;
	}
	if (f == 0)
	  cerr << "unknown function symbol '" << arg << "'\n";
	else {
	  interp.stoplevel = 0;
	  done = true;
	}
      } else {
	interp.stoplevel = 0;
	done = true;
      }
      break;
    }
    case '.': {
      // print current rule
      DebugInfo& d = *kt;
      if (d.r) {
	cout << stacklab(interp, kt, kt) << " [" << d.n << "] "
	     << pname(interp, d.e) << ": " << *d.r << ";\n";
      } else if (d.e->tag > 0 &&
		 interp.externals.find(d.e->tag) != interp.externals.end()) {
	ExternInfo &info = interp.externals[d.e->tag];
	cout << stacklab(interp, kt, kt) << " [" << d.n << "] "
	     << pname(interp, d.e) << ": " << info << ";\n";
      } else {
	cout << stacklab(interp, kt, kt) << " [" << d.n << "] "
	     << pname(interp, d.e) << ":\n";
      }
      get_vars(interp, kt);
      print_vars(cout, interp, d);
      break;
    }
    case 'p': {
      // print rule stack
      static size_t last_count = 5;
      size_t count = atoi(arg.c_str());
      if (count)
	last_count = count;
      else
	count = last_count;
      list<DebugInfo>::reverse_iterator it = kt;
      for (size_t i = 0; it != interp.debug_info.rbegin() && i < last_count/2;
	   it--, i++)
	;
      for (; count>0 && it != interp.debug_info.rend(); ++it, --count)
	;
      count = last_count;
      get_vars(interp, --it);
      while (count-- > 0) {
	DebugInfo& d = *it;
	if (d.r) {
	  cout << stacklab(interp, it, kt) << " [" << d.n << "] "
	       << pname(interp, d.e) << ": " << *d.r << ";\n";
	} else if (d.e->tag > 0 &&
		   interp.externals.find(d.e->tag) != interp.externals.end()) {
	  ExternInfo &info = interp.externals[d.e->tag];
	  cout << stacklab(interp, it, kt) << " [" << d.n << "] "
	       << pname(interp, d.e) << ": " << info << ";\n";
	} else {
	  cout << stacklab(interp, it, kt) << " [" << d.n << "] "
	       << pname(interp, d.e) << ":\n";
	}
	print_vars(cout, interp, d);
	if (it == interp.debug_info.rbegin())
	  break;
	else
	  --it;
      }
      break;
    }
    case 'd': case 'u': case 'b': case 't': {
      // stack navigation
      switch (cmd[0]) {
      case 'd':
	if (kt == interp.debug_info.rbegin()) {
	  cerr << "already at bottom\n";
	  goto errexit;
	} else
	  kt--;
	break;
      case 'u': {
	list<DebugInfo>::reverse_iterator lt = kt; lt++;
	if (lt == interp.debug_info.rend()) {
	  cerr << "already at top\n";
	  goto errexit;
	} else
	  kt = lt;
	break;
      }
      case 'b':
	if (kt == interp.debug_info.rbegin()) {
	  cerr << "already at bottom\n";
	  goto errexit;
	} else
	  kt = interp.debug_info.rbegin();
	break;
      case 't': {
	list<DebugInfo>::reverse_iterator lt = interp.debug_info.rend(); lt--;
	if (kt == lt) {
	  cerr << "already at top\n";
	  goto errexit;
	} else
	  kt = lt;
	break;
      }
      }
      {
	DebugInfo& d = *kt;
	if (d.r) {
	  cout << stacklab(interp, kt, kt) << " [" << d.n << "] "
	       << pname(interp, d.e) << ": " << *d.r << ";\n";
	} else if (d.e->tag > 0 &&
		   interp.externals.find(d.e->tag) != interp.externals.end()) {
	  ExternInfo &info = interp.externals[d.e->tag];
	  cout << stacklab(interp, kt, kt) << " [" << d.n << "] "
	       << pname(interp, d.e) << ": " << info << ";\n";
	} else {
	  cout << stacklab(interp, kt, kt) << " [" << d.n << "] "
	       << pname(interp, d.e) << ":\n";
	}
	get_vars(interp, kt);
	print_vars(cout, interp, d);
      }
      errexit:
      break;
    }
    case 'x':
      // bail out
      if (yes_or_no("This will exit the interpreter. Proceed (y/n)?"))
	exit(0);
      break;
    case 'h':
      // help
      cout << "Debugger commands:\n\
a       auto: step through the entire program, run unattended\n\
c [f]   continue until next breakpoint, or given function f\n\
h       help: print this list\n\
n       next step: step over reduction\n\
p [n]   print rule stack (n = number of frames)\n\
r       run: finish evaluation without debugger\n\
s       single step: step into reduction\n\
t, b    move to the top or bottom of the rule stack\n\
u, d    move up or down one level in the rule stack\n\
x       exit the interpreter (after confirmation)\n\
.       reprint current rule\n";
      if (!interp.escape_mode || interp.escape_mode == '!')
        cout.put('!');
      else {
        cout << "! cmd   execute shell command\n";
        cout.put(interp.escape_mode);
      }
      cout << "\
 cmd   execute interpreter command\n\
? expr  evaluate expression\n\
<cr>    single step (same as 's')\n\
<eof>   step through program, run unattended (same as 'a')\n";
      break;
    default:
      cerr << "unknown command '" << cmdline << "', type 'h' for help\n";
      break;
    }
  }
  if (!interp.dirty.empty() || !interp.dirty_types.empty())
    /* It seems that the user changed the program while in the debugger.
       Recompile and proceed with fingers crossed. XXXFIXME: If the user
       modified a function which is currently executing, this will most likely
       cause the program to segfault. Oh well. */
    interp.compile();
}

extern "C"
void pure_debug_redn(void *_e, void *_r, pure_expr *x)
{
  Env *e = (Env*)_e;
  rule *r = (rule*)_r;
  interpreter& interp = *interpreter::g_interp;
  if (!interp.interactive) return;
  assert(!interp.debug_info.empty());
  if (x && (interp.stopped(e) || interp.traced(e))) {
    DebugInfo& d = interp.debug_info.back();
    assert(d.e == e);
    if (r) {
      cout << "++ [" << d.n << "] "
	   << pname(interp, e) << ": " << *r << ";\n";
    } else if (e->tag > 0 &&
	       interp.externals.find(e->tag) != interp.externals.end()) {
      ExternInfo &info = interp.externals[e->tag];
      cout << "++ [" << d.n << "] "
	   << pname(interp, e) << ": " << info << ";\n";
    } else
      goto pop;
    get_vars(interp, interp.debug_info.rbegin());
    print_vars(cout, interp, d);
    cout << "     --> " << printx(x, 68) << endl;
  }
 pop:
  // pop an activation record
  interp.debug_info.pop_back();
  if (interp.tracelevel >= 0 &&
      (uint32_t)interp.tracelevel > interp.debug_info.size())
    interp.tracelevel = -1;
}

/* LIBRARY API. *************************************************************/

extern "C"
pure_expr *pure_byte_string(const char *s)
{
  if (!s) return pure_pointer(0);
  return pure_pointer(strdup(s));
}

extern "C"
pure_expr *pure_byte_cstring(const char *s)
{
  if (!s) return pure_pointer(0);
  return pure_pointer(fromutf8(s));
}

extern "C"
pure_expr *pure_int_seq(int32_t from, int32_t to, int32_t step)
{
  if (step == 0)
    return 0;
  if ((step > 0 && from > to) || (step < 0 && from < to))
    return mk_nil();
  int32_t count = (to-from)/step;
  if (count < 0) count = 0;
  count++;
  pure_expr **xs = (pure_expr**)malloc(count*sizeof(pure_expr*));
  if (!xs) {
    pure_throw(pure_symbol(pure_sym("malloc_error")));
    return 0;
  }
  for (int32_t i = 0; i < count; i++) {
    xs[i] = pure_int(from);
    from += step;
  }
  pure_expr *x = pure_listv(count, xs);
  free(xs);
  return x;
}

extern "C"
pure_expr *pure_double_seq(double from, double to, double step)
{
  if (step == 0.0)
    return 0;
  double to2 = to+0.5*step;
  if ((step > 0.0 && from > to2) || (step < 0.0 && from < to2) ||
      is_nan(from) || is_nan(to))
    return mk_nil();
  else if (is_nan(step))
    return pure_listl(1, pure_double(from));
  int32_t count = (int32_t)((to-from)/step+0.5);
  if (count < 0) count = 0;
  count++;
  pure_expr **xs = (pure_expr**)malloc(count*sizeof(pure_expr*));
  if (!xs) {
    pure_throw(pure_symbol(pure_sym("malloc_error")));
    return 0;
  }
  for (int32_t i = 0; i < count; i++) {
    if ((step > 0.0 && from > to2) || (step < 0.0 && from < to2)) {
      count = i;
      break;
    }
    xs[i] = pure_double(from);
    from += step;
  }
  pure_expr *x = pure_listv(count, xs);
  free(xs);
  return x;
}

extern "C"
pure_expr *pure_int_rowvect(int32_t from, int32_t to, int32_t step)
{
  if (step == 0)
    return 0;
  if ((step > 0 && from > to) || (step < 0 && from < to))
    return pure_int_matrix(create_int_matrix(1, 0));
  int32_t count = (to-from)/step;
  if (count < 0) count = 0;
  count++;
  size_t n = count;
  gsl_matrix_int *mat = create_int_matrix(1, n);
  for (size_t i = 0; i < n; i++) {
    mat->data[i] = from;
    from += step;
  }
  return pure_int_matrix(mat);
}

extern "C"
pure_expr *pure_double_rowvect(double from, double to, double step)
{
  if (step == 0.0)
    return 0;
  double to2 = to+0.5*step;
  if ((step > 0.0 && from > to2) || (step < 0.0 && from < to2) ||
      is_nan(from) || is_nan(to))
    return pure_double_matrix(create_double_matrix(1, 0));
  else if (is_nan(step)) {
    gsl_matrix *mat = create_double_matrix(1, 1);
    mat->data[0] = from;
    return pure_double_matrix(mat);
  }
  int32_t count = (int32_t)((to-from)/step+0.5);
  if (count < 0) count = 0;
  count++;
  double from1 = from;
  for (int32_t i = 0; i < count; i++) {
    if ((step > 0.0 && from1 > to2) || (step < 0.0 && from1 < to2)) {
      count = i;
      break;
    }
    from1 += step;
  }
  size_t n = count;
  gsl_matrix *mat = create_double_matrix(1, n);
  for (size_t i = 0; i < n; i++) {
    mat->data[i] = from;
    from += step;
  }
  return pure_double_matrix(mat);
}

extern "C"
pure_expr *pure_int_colvect(int32_t from, int32_t to, int32_t step)
{
  if (step == 0)
    return 0;
  if ((step > 0 && from > to) || (step < 0 && from < to))
    return pure_int_matrix(create_int_matrix(0, 1));
  int32_t count = (to-from)/step;
  if (count < 0) count = 0;
  count++;
  size_t n = count;
  gsl_matrix_int *mat = create_int_matrix(n, 1);
  for (size_t i = 0; i < n; i++) {
    mat->data[i] = from;
    from += step;
  }
  return pure_int_matrix(mat);
}

extern "C"
pure_expr *pure_double_colvect(double from, double to, double step)
{
  if (step == 0.0)
    return 0;
  double to2 = to+0.5*step;
  if ((step > 0.0 && from > to2) || (step < 0.0 && from < to2) ||
      is_nan(from) || is_nan(to))
    return pure_double_matrix(create_double_matrix(0, 1));
  else if (is_nan(step)) {
    gsl_matrix *mat = create_double_matrix(1, 1);
    mat->data[0] = from;
    return pure_double_matrix(mat);
  }
  int32_t count = (int32_t)((to-from)/step+0.5);
  if (count < 0) count = 0;
  count++;
  double from1 = from;
  for (int32_t i = 0; i < count; i++) {
    if ((step > 0.0 && from1 > to2) || (step < 0.0 && from1 < to2)) {
      count = i;
      break;
    }
    from1 += step;
  }
  size_t n = count;
  gsl_matrix *mat = create_double_matrix(n, 1);
  for (size_t i = 0; i < n; i++) {
    mat->data[i] = from;
    from += step;
  }
  return pure_double_matrix(mat);
}

/* C qsort() interface. This is pretty much the same as the implementation in
   examples/sort.c, massaged slightly to be reentrant, to handle exceptions,
   and to also support Pure matrices. */

static pure_expr* cmp_p; // TLD
static unsigned cmp_pno; // TLD

static inline int sgn(int x)
{
  return x<0?-1:x>0?1:0;
}

static inline int sgn(double x)
{
  return x<0?-1:x>0?1:0;
}

// Optimize comparisons for some common cases of POD (plain old data).

enum { CMP_P, CMP_LE, CMP_GR };

static int cmp(const void *xp, const void *yp)
{
  int res = 0;
  pure_expr *x = *(pure_expr**)xp, *y = *(pure_expr**)yp;
  if (cmp_pno != CMP_P) {
    if (x->tag == y->tag && x->tag < 0) {
      int res = 0;
      switch (x->tag) {
      case EXPR::INT:
	res = sgn(x->data.i - y->data.i); break;
      case EXPR::DBL:
	res = sgn(x->data.d - y->data.d); break;
      case EXPR::BIGINT:
	res = sgn(bigint_cmp(x->data.z, y->data.z)); break;
      case EXPR::STR:
	res = sgn(strcmp(x->data.s, y->data.s)); break;
      default:
	goto generic;
      }
      return cmp_pno==CMP_LE?res:-res;
    }
  }
 generic:
  pure_expr *p = pure_appl(cmp_p, 2, x, y); /* x<y? */
  if (!pure_is_int(p, &res))
    pure_throw(pure_symbol(interpreter::g_interp->
			   symtab.failed_cond_sym().f));
  pure_freenew(p); /* collect temporary */
  if (res)
    res = -1;
  else {
    /* Invoke cmp_p another time to perform the reverse comparison. */
    p = pure_appl(cmp_p, 2, y, x); /* y<x? */
    if (!pure_is_int(p, &res))
      pure_throw(pure_symbol(interpreter::g_interp->
			     symtab.failed_cond_sym().f));
    pure_freenew(p); /* collect temporary */
    res = res!=0;
    /* If both tests failed then the elements are either equal or
       incomparable, in which case res==0. */
  }
  return res;
}

/* These wrapper routines are needed to catch exceptions while qsort() is
   executing, so that we can perform proper cleanup when something nasty
   happens. */

static pure_expr *sort_worker(size_t size, pure_expr **elems,
			      gsl_matrix_symbolic *m)
{
  qsort(elems, size, sizeof(pure_expr*), cmp);
  if (m)
    return pure_symbolic_matrix(m);
  else
    return pure_listv(size, elems);
}

static pure_expr *sort_wrapper(size_t size, pure_expr **elems,
			       gsl_matrix_symbolic *m, pure_expr **_e)
{
  pure_try_call(sort_worker(size, elems, m));
}

extern "C"
pure_expr *pure_sort(pure_expr *p, pure_expr *x)
{
  size_t size;
  pure_expr **elems;
  interpreter& interp = *interpreter::g_interp;
  /* Deconstruct the list argument which is passed as a pure_expr* value.
     This yields a vector of pure_expr* elements which can be passed to the
     qsort() routine. */
  if (pure_is_listv(x, &size, &elems)) {
    /* Save the current predicate, so that we can be invoked recursively. */
    pure_expr *save_cmp_p = cmp_p;
    unsigned save_cmp_pno = cmp_pno;
    pure_expr *y = 0, *e = 0;
    /* Invoke qsort() to sort the elems vector. */
    cmp_p = p;
    cmp_pno = (p->tag==interp.symtab.less_sym().f)?CMP_LE:
      (p->tag==interp.symtab.greater_sym().f)?CMP_GR:CMP_P;
    y = sort_wrapper(size, elems, 0, &e);
    /* Clean up. */
    cmp_p = save_cmp_p; cmp_pno = save_cmp_pno;
    free(elems);
    /* If we got an exception, throw it again, otherwise return the sorted
       list. */
    if (e) pure_throw(e);
    return y;
  } else if (x->tag == EXPR::MATRIX) {
    if (x->data.mat.p) {
      gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
      size_t n1 = m->size1, n2 = m->size2;
      if (n1 == 0 || n2 == 0) // empty matrix
	return x;
      /* Create a copy of the original matrix in contiguous storage. */
      gsl_matrix_symbolic *m1 = create_symbolic_matrix(n1, n2);
      gsl_matrix_symbolic_memcpy(m1, m);
      elems = m1->data; size = n1*n2;
      /* Sort the new matrix using qsort(), handle exceptions. Analogous to
	 the list case. */
      pure_expr *save_cmp_p = cmp_p;
      unsigned save_cmp_pno = cmp_pno;
      pure_expr *y = 0, *e = 0;
      cmp_p = p;
      cmp_pno = (p->tag==interp.symtab.less_sym().f)?CMP_LE:
	(p->tag==interp.symtab.greater_sym().f)?CMP_GR:CMP_P;
      y = sort_wrapper(size, elems, m1, &e);
      cmp_p = save_cmp_p; cmp_pno = save_cmp_pno;
      if (e) {
	gsl_matrix_symbolic_free(m1);
	pure_throw(e);
      }
      return y;
    } else
      return x;
  } else
    return 0;
}

extern "C"
pure_expr *pure_intval(pure_expr *x)
{
  assert(x);
  if (is_thunk(x)) pure_force(x);
  switch (x->tag) {
  case EXPR::INT:	return x;
  case EXPR::BIGINT:	return pure_int(pure_get_int(x));
  case EXPR::DBL:	return pure_int((int32_t)x->data.d);
#if SIZEOF_VOID_P==8
    // Must cast to 64 bit here first, since on 64 bit systems g++ gives an
    // error when directly casting a 64 bit pointer to a 32 bit integer.
  case EXPR::PTR:	return pure_int((uint32_t)(uint64_t)x->data.p);
#else
  case EXPR::PTR:	return pure_int((uint32_t)x->data.p);
#endif
  default:		return 0;
  }
}

extern "C"
pure_expr *pure_dblval(pure_expr *x)
{
  assert(x);
  if (is_thunk(x)) pure_force(x);
  switch (x->tag) {
  case EXPR::INT:	return pure_double((double)x->data.i);
  case EXPR::BIGINT:	return pure_double(mpz_get_d(x->data.z));
  case EXPR::DBL:	return x;
  default:		return 0;
  }
}

extern "C"
pure_expr *pure_pointerval(pure_expr *x)
{
  assert(x);
  if (is_thunk(x)) pure_force(x);
  switch (x->tag) {
  case EXPR::PTR:	return x;
  case EXPR::STR:	return pure_pointer(x->data.s);
  case EXPR::INT:	return pure_pointer((void*)(intptr_t)x->data.i);
  case EXPR::BIGINT:
    if (sizeof(mp_limb_t) == 8)
#if SIZEOF_VOID_P==8
      return pure_pointer((void*)mpz_getlimbn(x->data.z, 0));
#else
      return pure_pointer((void*)(uint32_t)mpz_getlimbn(x->data.z, 0));
#endif
    else
#if SIZEOF_VOID_P==8
      return pure_pointer((void*)(uint64_t)pure_get_int64(x));
#else
      return pure_pointer((void*)(uint32_t)pure_get_int(x));
#endif
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    return pure_pointer(m->data);
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    return pure_pointer(m->data);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    return pure_pointer(m->data);
  }
  case EXPR::IMATRIX:{
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    return pure_pointer(m->data);
  }
  default:		return 0;
  }
}

static pure_expr *pointer_to_bigint(void *p)
{
  if (sizeof(mp_limb_t) == 8) {
    // In this case the pointer value ought to fit into a single limb.
#if SIZEOF_VOID_P==8
    limb_t u[1] = { (limb_t)(uint64_t)p };
#else
    limb_t u[1] = { (limb_t)(uint64_t)(uint32_t)p };
#endif
    return pure_bigint(1, u);
  }
  // 4 byte limbs.
#if SIZEOF_VOID_P==8
  // 8 byte pointers, put least significant word in the first limb.
  assert(sizeof(void*) == 8);
  limb_t u[2] = { (uint32_t)(uint64_t)p, (uint32_t)(((uint64_t)p)>>32) };
  return pure_bigint(2, u);
#else
  // 4 byte pointers.
  limb_t u[1] = { (uint32_t)p };
  return pure_bigint(1, u);
#endif
}

extern "C"
pure_expr *pure_bigintval(pure_expr *x)
{
  assert(x);
  if (is_thunk(x)) pure_force(x);
  if (x->tag == EXPR::BIGINT)
    return x;
  else if (x->tag == EXPR::PTR)
    return pointer_to_bigint(x->data.p);
  else if (x->tag != EXPR::INT && x->tag != EXPR::DBL)
    return 0;
  else if (x->tag == EXPR::DBL &&
	   (is_nan(x->data.d) || is_nan(x->data.d-x->data.d)))
    pure_sigfpe();
  pure_expr *y = pure_bigint(0, 0);
  mpz_t& z = y->data.z;
  if (x->tag == EXPR::INT)
    mpz_set_si(z, x->data.i);
  else if (x->tag == EXPR::DBL)
    mpz_set_d(z, x->data.d);
  return y;
}

extern "C"
pure_expr *pure_rational(double d)
{
  pure_expr *u = pure_bigint(0, 0);
  pure_expr *v = pure_bigint(0, 0);
  mpz_t& x = u->data.z;
  mpz_t& y = v->data.z;
  mpq_t q;
  mpq_init(q);
  mpq_set_d(q, d);
  mpq_get_num(x, q);
  mpq_get_den(y, q);
  mpq_clear(q);
  return pure_tuplel(2, u, v);
}

/* The Mersenne Twister (corrected version from 2002), slightly massaged for
   use in Pure. The original source is available here:
   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/CODES/mt19937ar.c */

/*
   A C-program for MT19937, with initialization improved 2002/1/26.
   Coded by Takuji Nishimura and Makoto Matsumoto.

   Before using, initialize the state by using init_genrand(seed)
   or init_by_array(init_key, key_length).

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote
        products derived from this software without specific prior written
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)
*/

/* Period parameters */
#define N 624
#define M 397
#define MATRIX_A 0x9908b0dfU   /* constant vector a */
#define UPPER_MASK 0x80000000U /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffU /* least significant r bits */

static unsigned mt[N]; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

/* initializes mt[N] with a seed */
extern "C"
void init_genrand(unsigned s)
{
    mt[0]= s & 0xffffffffU;
    for (mti=1; mti<N; mti++) {
        mt[mti] =
	    (1812433253U * (mt[mti-1] ^ (mt[mti-1] >> 30)) + mti);
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        mt[mti] &= 0xffffffffU;
        /* for >32 bit machines */
    }
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
/* slight change for C++, 2004/2/26 */
extern "C"
void init_genrand_array(unsigned *init_key, int key_length)
{
    int i, j, k;
    init_genrand(19650218U);
    i=1; j=0;
    k = (N>key_length ? N : key_length);
    for (; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525U))
          + init_key[j] + j; /* non linear */
        mt[i] &= 0xffffffffU; /* for WORDSIZE > 32 machines */
        i++; j++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941U))
          - i; /* non linear */
        mt[i] &= 0xffffffffU; /* for WORDSIZE > 32 machines */
        i++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
    }

    mt[0] = 0x80000000U; /* MSB is 1; assuring non-zero initial array */
}

/* generates a random number on [0,0xffffffff]-interval */
extern "C"
unsigned genrand_int32(void)
{
    unsigned y;
    static unsigned mag01[2]={0x0U, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if init_genrand() has not been called, */
            init_genrand(5489U); /* a default initial seed is used */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1U];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1U];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1U];

        mti = 0;
    }

    y = mt[mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680U;
    y ^= (y << 15) & 0xefc60000U;
    y ^= (y >> 18);

    return y;
}

/* generates a random number on [0,0x7fffffff]-interval */
extern "C"
int genrand_int31(void)
{
    return (int)(genrand_int32()>>1);
}

/* generates a random number on [0,1]-real-interval */
extern "C"
double genrand_real1(void)
{
    return genrand_int32()*(1.0/4294967295.0);
    /* divided by 2^32-1 */
}

/* generates a random number on [0,1)-real-interval */
extern "C"
double genrand_real2(void)
{
    return genrand_int32()*(1.0/4294967296.0);
    /* divided by 2^32 */
}

/* generates a random number on (0,1)-real-interval */
extern "C"
double genrand_real3(void)
{
    return (((double)genrand_int32()) + 0.5)*(1.0/4294967296.0);
    /* divided by 2^32 */
}

/* generates a random number on [0,1) with 53-bit resolution */
extern "C"
double genrand_res53(void)
{
    unsigned a=genrand_int32()>>5, b=genrand_int32()>>6;
    return(a*67108864.0+b)*(1.0/9007199254740992.0);
}

#undef N
#undef M

extern "C"
pure_expr *bigint_neg(mpz_t x)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_neg(z, x);
  return u;
}

extern "C"
pure_expr *bigint_add(mpz_t x, mpz_t y)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_add(z, x, y);
  return u;
}

extern "C"
pure_expr *bigint_sub(mpz_t x, mpz_t y)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_sub(z, x, y);
  return u;
}

extern "C"
pure_expr *bigint_mul(mpz_t x, mpz_t y)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_mul(z, x, y);
  return u;
}

// These raise a SIGFPE signal exception for division by zero.

extern "C"
pure_expr *bigint_div(mpz_t x, mpz_t y)
{
  if (mpz_sgn(y) == 0) pure_sigfpe();
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_tdiv_q(z, x, y);
  return u;
}

extern "C"
pure_expr *bigint_mod(mpz_t x, mpz_t y)
{
  if (mpz_sgn(y) == 0) pure_sigfpe();
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_tdiv_r(z, x, y);
  return u;
}

extern "C"
pure_expr *bigint_pow(mpz_t x, uint32_t y)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_pow_ui(z, x, y);
  return u;
}

extern "C"
pure_expr *bigint_shl(mpz_t x, int32_t y)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_mul_2exp(z, x, y);
  return u;
}

extern "C"
pure_expr *bigint_shr(mpz_t x, int32_t y)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_fdiv_q_2exp(z, x, y);
  return u;
}

extern "C"
pure_expr *bigint_not(mpz_t x)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_com(z, x);
  return u;
}

extern "C"
pure_expr *bigint_and(mpz_t x, mpz_t y)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_and(z, x, y);
  return u;
}

extern "C"
pure_expr *bigint_or(mpz_t x, mpz_t y)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_ior(z, x, y);
  return u;
}

extern "C"
pure_expr *bigint_gcd(mpz_t x, mpz_t y)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_gcd(z, x, y);
  return u;
}

extern "C"
pure_expr *bigint_lcm(mpz_t x, mpz_t y)
{
  pure_expr *u = pure_bigint(0, 0);
  mpz_t& z = u->data.z;
  mpz_lcm(z, x, y);
  return u;
}

extern "C"
int32_t bigint_cmp(mpz_t x, mpz_t y)
{
  return mpz_cmp(x, y);
}

extern "C"
bool string_null(const char *s)
{
  assert(s);
  return *s==0;
}

extern "C"
uint32_t string_size(const char *s)
{
  assert(s);
  return u8strlen(s);
}

extern "C"
pure_expr *string_char_at(const char *s, uint32_t n)
{
  assert(s);
  unsigned long c = u8strchar(s, n);
  if (c == 0) return 0;
  char buf[5];
  return pure_string_dup(u8char(buf, c));
}

static inline long
u8decode(const char *s, const char **t)
{
  size_t n = 0;
  unsigned p = 0, q = 0;
  unsigned long c = 0;
  if (s[0] == 0)
    return -1;
  else if (s[1] == 0) {
    *t = s+1;
    return (unsigned char)s[0];
  }
  for (; n == 0 && *s; s++) {
    unsigned char uc = (unsigned char)*s;
    if (q == 0) {
      if (((signed char)uc) < 0) {
	switch (uc & 0xf0) {
	case 0xc0: case 0xd0:
	  q = 1;
	  c = uc & 0x1f;
	  break;
	case 0xe0:
	  q = 2;
	  c = uc & 0xf;
	  break;
	case 0xf0:
	  if ((uc & 0x8) == 0) {
	    q = 3;
	    c = uc & 0x7;
	  } else
	    c = uc;
	  break;
	default:
	  c = uc;
	  break;
	}
      } else
	c = uc;
      p = 0; if (q == 0) n++;
    } else if ((uc & 0xc0) == 0x80) {
      /* continuation byte */
      c = c << 6 | (uc & 0x3f);
      if (--q == 0)
	n++;
      else
	p++;
    } else {
      /* malformed char */
      return -1;
    }
  }
  if (n == 1) {
    *t = s;
    return c;
  } else
    return -1;
}

extern "C"
pure_expr *string_chars(const char *s)
{
  assert(s);
  interpreter& interp = *interpreter::g_interp;
  pure_expr *f = pure_new_internal(pure_const(interp.symtab.cons_sym().f));
  pure_expr *xs, **x = &xs;
  const char *t;
  for (; *s; s = t) {
    long c = u8decode(s, &t);
    if (c < 0) {
      c = (unsigned char)*s;
      t = s+1;
    }
    char buf[5];
    pure_expr *y = new_ref_expr(),
      *z = pure_new_internal(pure_string_dup(u8char(buf, c)));
    y->tag = EXPR::APP;
    y->data.x[0] = pure_new_internal(f);
    y->data.x[1] = z;
    *x = new_ref_expr();
    (*x)->tag = EXPR::APP;
    (*x)->data.x[0] = y;
    x = (*x)->data.x+1;
  }
  *x = pure_new_internal(mk_nil());
  pure_unref_internal(xs);
  pure_free_internal(f);
  return xs;
}

extern "C"
pure_expr *string_chr(uint32_t n)
{
  char buf[5];
  u8char(buf, n);
  if (u8charcode(buf) > 0)
    return pure_string_dup(buf);
  else
    return 0;
}

extern "C"
pure_expr *string_ord(const char *c)
{
  assert(c);
  long n = u8charcode(c);
  if (n > 0)
    return pure_int(n);
  else
    return 0;
}

extern "C"
pure_expr *string_concat(const char* s, const char *t)
{
  assert(s && t);
  size_t p = strlen(s), q = strlen(t);
  char *buf = new char[p+q+1];
  strcpy(buf, s); strcpy(buf+p, t);
  pure_expr *x = new_expr();
  x->tag = EXPR::STR;
  x->data.s = buf;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *string_concat_list(pure_expr *xs)
{
  // linear-time concatenation of a list of strings
  assert(xs);
  if (is_thunk(xs)) pure_force(xs);
  // calculate the size of the result string
  pure_expr *ys = xs, *z, *zs;
  size_t n = 0;
  while (is_cons(ys, z, zs)) {
    if (is_thunk(z)) pure_force(z);
    if (z->tag != EXPR::STR) break;
    n += strlen(z->data.s);
    ys = zs;
    if (is_thunk(ys)) pure_force(ys);
  }
  if (!is_nil(ys)) return 0;
  // allocate the result string
  char *buf = new char[n+1]; buf[0] = 0;
  // concatenate
  ys = xs; n = 0;
  while (is_cons(ys, z, zs) && z->tag == EXPR::STR) {
    strcpy(buf+n, z->data.s);
    n += strlen(z->data.s);
    ys = zs;
  }
  // return the result
  pure_expr *x = new_expr();
  x->tag = EXPR::STR;
  x->data.s = buf;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *string_join(const char *delim, pure_expr *xs)
{
  // linear-time concatenation of a list of strings
  assert(xs);
  if (is_thunk(xs)) pure_force(xs);
  // calculate the size of the result string
  pure_expr *ys = xs, *z, *zs;
  size_t n = 0, k = 0, l = strlen(delim);
  while (is_cons(ys, z, zs)) {
    if (is_thunk(z)) pure_force(z);
    if (z->tag != EXPR::STR) break;
    n += k+strlen(z->data.s); k = l;
    ys = zs;
    if (is_thunk(ys)) pure_force(ys);
  }
  if (!is_nil(ys)) return 0;
  // allocate the result string
  char *buf = new char[n+1]; buf[0] = 0;
  // concatenate
  ys = xs; n = k = 0;
  while (is_cons(ys, z, zs) && z->tag == EXPR::STR) {
    if (k>0) strcpy(buf+n, delim);
    strcpy(buf+n+k, z->data.s);
    n += k+strlen(z->data.s); k = l;
    ys = zs;
  }
  // return the result
  pure_expr *x = new_expr();
  x->tag = EXPR::STR;
  x->data.s = buf;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *string_split(const char *delim, const char *s)
{
  assert(delim && s);
  if (!*s) return mk_nil();
  const size_t k = strlen(delim);
  if (k == 0) return string_chars(s);
  interpreter& interp = *interpreter::g_interp;
  pure_expr *f = pure_new_internal(pure_const(interp.symtab.cons_sym().f));
  pure_expr *xs, **x = &xs;
  const char *t = strstr(s, delim);
  while (t) {
    size_t n = t-s;
    char *buf = (char*)malloc(n+1);
    assert(buf);
    strncpy(buf, s, n); buf[n] = 0;
    pure_expr *y = new_ref_expr(), *z = pure_new_internal(pure_string(buf));
    y->tag = EXPR::APP;
    y->data.x[0] = pure_new_internal(f);
    y->data.x[1] = z;
    *x = new_ref_expr();
    (*x)->tag = EXPR::APP;
    (*x)->data.x[0] = y;
    x = (*x)->data.x+1;
    s = t+k; t = strstr(s, delim);
  }
  pure_expr *y = new_ref_expr(), *z = pure_new_internal(pure_string_dup(s));
  y->tag = EXPR::APP;
  y->data.x[0] = f;
  y->data.x[1] = z;
  *x = new_ref_expr();
  (*x)->tag = EXPR::APP;
  (*x)->data.x[0] = y;
  (*x)->data.x[1] = pure_new_internal(mk_nil());
  pure_unref_internal(xs);
  return xs;
}

extern "C"
pure_expr *string_substr(const char* s, uint32_t pos, uint32_t size)
{
  assert(s);
  const char *p = u8strcharpos(s, pos), *q = u8strcharpos(p, size);
  size_t n = q-p;
  char *buf = new char[n+1];
  strncpy(buf, p, n); buf[n] = 0;
  pure_expr *x = new_expr();
  x->tag = EXPR::STR;
  x->data.s = buf;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
int32_t string_index(const char* s, const char *t)
{
  assert(s && t);
  const char *p = strstr(s, t);
  if (p)
    return u8strpos(s, p);
  else
    return -1;
}

extern "C"
char *str(const pure_expr *x)
{
  assert(x);
  ostringstream os;
  try {
    os << x;
    return strdup(os.str().c_str());
  } catch (err &e) {
    return 0;
  }
}

extern "C"
pure_expr *pure_str(const pure_expr *x)
{
  assert(x);
  ostringstream os;
  try {
    os << x;
    return pure_cstring_dup(os.str().c_str());
  } catch (err &e) {
    return 0;
  }
}

extern bool __print_pretty__;

extern "C"
pure_expr *__str__(pure_expr *x)
{
  assert(x);
  try {
    interpreter& interp = *interpreter::g_interp;
    expr y = interp.macsval(x);
    ostringstream os;
    os << y;
    string s = os.str();
    return pure_cstring_dup(s.c_str());
  } catch (err &e) {
    return 0;
  }
}

extern "C"
pure_expr *eval(pure_expr *x)
{
  assert(x);
  char *s;
  if (pure_is_cstring_dup(x, &s)) {
    interpreter& interp = *interpreter::g_interp;
    interp.errmsg.clear(); interp.errpos.clear();
    string src = s;
    pure_expr *res = interp.runstr(src+";");
    free(s);
    interp.result = 0;
    // The extra ';' we placed at the end of the string may cause column
    // positions in the last line to extend past the end of the line, fix up
    // the error locations accordingly.
    size_t nlines = 1, lastpos = 0, pos;
    while ((pos = src.find('\n', lastpos)) != string::npos) {
      nlines++;
      lastpos = pos+1;
    }
    size_t lastlen = strlen(src.c_str()+lastpos);
    for (list<errinfo>::iterator it = interp.errpos.begin(),
	   end = interp.errpos.end(); it != end; ++it) {
      if (it->line1 == (int)nlines && it->col1 > (int)lastlen+1)
	it->col1 = lastlen+1;
      if (it->line2 == (int)nlines && it->col2 > (int)lastlen+1)
	it->col2 = lastlen+1;
    }
    if (res) {
      if (interp.errmsg.empty()) {
	pure_unref_internal(res);
	return res;
      } else {
	pure_free_internal(res);
	return 0;
      }
    } else if (interp.errmsg.empty())
      return mk_void();
    else
      return 0;
  } else {
    pure_expr *res = 0, *e = 0;
    interpreter& interp = *interpreter::g_interp;
    try {
      expr y = interp.pure_expr_to_expr(x);
      res = interp.eval(y, e, false);
    } catch (err &e) {
      return x;
    }
    if (res) {
      assert(!e);
      return res;
    } else if (e) {
      pure_unref_internal(e);
      pure_throw(e);
      return 0;
    } else
      return 0;
  }
}

extern "C"
pure_expr *evalcmd(pure_expr *x)
{
  assert(x);
  char *s;
  if (pure_is_cstring_dup(x, &s)) {
    interpreter& interp = *interpreter::g_interp;
    ostream *l_output = interp.output;
    ostringstream sout;
    interp.errmsg.clear(); interp.errpos.clear();
    interp.output = &sout;
    pure_expr *res = interp.runstr(string(s));
    free(s);
    interp.result = 0;
    interp.output = l_output;
    if (res)
      pure_free_internal(res);
    else if (!interp.errmsg.empty())
      return 0;
    return pure_cstring_dup(sout.str().c_str());
  } else
    return 0;
}

static inline bool is_mapsto(pure_expr *x, pure_expr*& y, pure_expr*& z)
{
  interpreter& interp = *interpreter::g_interp;
  if (x->tag == EXPR::APP && x->data.x[0]->tag == EXPR::APP &&
      x->data.x[0]->data.x[0]->tag == interp.symtab.mapsto_sym().f) {
    y = x->data.x[0]->data.x[1];
    z = x->data.x[1];
    return true;
  } else
    return false;
}

static bool is_list2(pure_expr *x, size_t& size,
		     pure_expr**& elems, pure_expr*& tl)
{
  assert(x);
  pure_expr *u = x, *y, *z;
  size = 0;
  while (is_cons(u, y, z)) {
    size++;
    u = z;
  }
  if (size == 0) return false;
  tl = u;
  elems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  assert(elems);
  size_t i = 0;
  u = x;
  while (is_cons(u, y, z)) {
    elems[i++] = y;
    u = z;
  }
  return true;
}

static bool is_tuple(pure_expr *x, size_t& size,
		     pure_expr**& elems)
{
  assert(x);
  pure_expr *u = x, *y, *z;
  size = 0;
  while (is_pair(u, y, z)) {
    size++;
    u = z;
  }
  if (size == 0) return false;
  size++;
  elems = (pure_expr**)malloc(size*sizeof(pure_expr*));
  assert(elems);
  size_t i = 0;
  u = x;
  while (is_pair(u, y, z)) {
    elems[i++] = y;
    u = z;
  }
  elems[i++] = u;
  return true;
}

static pure_expr *subst(map<int32_t,pure_expr*> &env,
			map<uint32_t,bool> &force,
			pure_expr *cons, pure_expr *pair,
			pure_expr *x)
{
  char test;
  switch (x->tag) {
  case EXPR::APP: {
    checkstk(test);
    size_t size;
    pure_expr **elems, *tl;
    interpreter& interp = *interpreter::g_interp;
    if (is_list2(x, size, elems, tl)) {
      /* Optimize the list case, so that we don't run out of stack space. */
      pure_expr *f = cons?cons:pure_symbol(interp.symtab.cons_sym().f);
      pure_expr *x = subst(env, force, cons, pair, tl);
      while (size > 0) {
	pure_expr *y = subst(env, force, cons, pair, elems[--size]);
	x = mk_cons(f, y, x);
      }
      free(elems);
      return x;
    } else if (is_tuple(x, size, elems)) {
      /* Optimize the tuple case. */
      pure_expr *f = pair?pair:pure_symbol(interp.symtab.pair_sym().f);
      pure_expr *x = subst(env, force, cons, pair, elems[--size]);
      while (size > 0) {
	pure_expr *y = subst(env, force, cons, pair, elems[--size]);
	x = mk_cons(f, y, x);
      }
      free(elems);
      return x;
    } else
      return pure_apply2(subst(env, force, cons, pair, x->data.x[0]),
			 subst(env, force, cons, pair, x->data.x[1]));
  }
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::DMATRIX:
  case EXPR::IMATRIX:
  case EXPR::CMATRIX:
    return x;
  case EXPR::MATRIX:
    if (x->data.mat.p) {
      gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
      size_t n1 = m->size1, n2 = m->size2;
      if (n1 == 0 || n2 == 0) // empty matrix
	return x;
      checkstk(test);
      gsl_matrix_symbolic *m1 = create_symbolic_matrix(n1, n2);
      for (size_t i = 0; i < n1; i++)
	for (size_t j = 0; j < n2; j++)
	  m1->data[i*m1->tda+j] =
	    subst(env, force, cons, pair, m->data[i*m->tda+j]);
      return pure_symbolic_matrix(m1);
    } else
      return x;
  default: {
    assert(x->tag >= 0);
    map<int32_t,pure_expr*>::iterator it;
    if (x->tag > 0 && (!x->data.clos || !x->data.clos->local) &&
	(it = env.find(x->tag)) != env.end()) {
      /* Substitute a local closure from the environment. */
      pure_expr *y = it->second;
      if (x->data.clos && y->data.clos && y->data.clos->local &&
	  force.find(y->data.clos->key) != force.end())
	force[y->data.clos->key] = true;
      return y;
    } else if (x->tag > 0)
      /* NOTE: Don't even think of just returning x here! In the (rather
	 obscure) case that x is a global symbol which has just been
	 recompiled, the function pointer in x will have become invalid so
	 that an application of this symbol will try to execute code which has
	 gone non-existing already. So we effectively quote the symbol here to
	 prevent its reevaluation; rsubst() then takes care of reconstructing
	 the closure. */
      return pure_const(x->tag);
    else
      return x;
  }
  }
}

static pure_expr *rsubst(map<uint32_t,pure_expr*> &env,
			 map<uint32_t,bool> &force,
			 pure_expr *x)
{
  char test;
  switch (x->tag) {
  case EXPR::APP: {
    checkstk(test);
    size_t size;
    pure_expr **elems, *tl;
    interpreter& interp = *interpreter::g_interp;
    if (is_list2(x, size, elems, tl)) {
      /* Optimize the list case, so that we don't run out of stack space. */
      pure_expr *f = pure_symbol(interp.symtab.cons_sym().f);
      pure_expr *x = rsubst(env, force, tl);
      while (size > 0) {
	pure_expr *y = rsubst(env, force, elems[--size]);
	x = mk_cons(f, y, x);
      }
      free(elems);
      return x;
    } else if (is_tuple(x, size, elems)) {
      /* Optimize the tuple case. */
      pure_expr *f = pure_symbol(interp.symtab.pair_sym().f);
      pure_expr *x = rsubst(env, force, elems[--size]);
      while (size > 0) {
	pure_expr *y = rsubst(env, force, elems[--size]);
	x = mk_cons(f, y, x);
      }
      free(elems);
      return x;
    } else
      return pure_apply2(rsubst(env, force, x->data.x[0]),
			 rsubst(env, force, x->data.x[1]));
  }
  case EXPR::INT:
  case EXPR::BIGINT:
  case EXPR::DBL:
  case EXPR::STR:
  case EXPR::PTR:
  case EXPR::DMATRIX:
  case EXPR::IMATRIX:
  case EXPR::CMATRIX:
    return x;
  case EXPR::MATRIX:
    if (x->data.mat.p) {
      gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
      size_t n1 = m->size1, n2 = m->size2;
      if (n1 == 0 || n2 == 0) // empty matrix
	return x;
      checkstk(test);
      gsl_matrix_symbolic *m1 = create_symbolic_matrix(n1, n2);
      for (size_t i = 0; i < n1; i++)
	for (size_t j = 0; j < n2; j++)
	  m1->data[i*m1->tda+j] =
	    rsubst(env, force, m->data[i*m->tda+j]);
      return pure_symbolic_matrix(m1);
    } else
      return x;
  default: {
    assert(x->tag >= 0);
    map<uint32_t,pure_expr*>::iterator it;
    if (x->tag > 0 && x->data.clos && x->data.clos->local &&
	(it = env.find(x->data.clos->key)) != env.end()) {
      /* Resubstitute a global for a local closure from the environment. */
      pure_expr *y = it->second;
      if (force[x->data.clos->key]) {
	/* We actually reconstruct the closure here since its code might
	   already have become invalid. See the corresponding note in
	   subst(). */
	assert(y->tag > 0);
	return pure_symbol(y->tag);
      } else
	return pure_const(y->tag);
    } else
      return x;
  }
  }
}

extern "C"
pure_expr *reduce(pure_expr *locals, pure_expr *x)
{
  size_t n;
  pure_expr **xs;
  if (pure_is_listv(locals, &n, &xs)) {
    map<int32_t,pure_expr*> env;
    map<uint32_t,pure_expr*> renv;
    map<uint32_t,bool> force;
    pure_expr *cons = 0, *pair = 0;
    interpreter& interp = *interpreter::g_interp;
    for (size_t i = 0; i < n; i++) {
      pure_expr *x, *y;
      int32_t f;
      if (is_mapsto(xs[i], x, y) && pure_is_symbol(x, &f) && f>0) {
	env[f] = y;
	if (f == interp.symtab.cons_sym().f)
	  cons = y;
	else if (f == interp.symtab.pair_sym().f)
	  pair = y;
	if (y->tag == f && y->data.clos && y->data.clos->local) {
	  renv[y->data.clos->key] = x;
	  force[y->data.clos->key] = false;
	}
      } else {
	free(xs);
	return 0;
      }
    }
    free(xs);
    pure_expr *u = pure_new(subst(env, force, cons, pair, x));
    pure_expr *v = pure_new(rsubst(renv, force, u));
    pure_free(u);
    pure_unref(v);
    return v;
  } else
    return 0;
}

extern "C"
const char *lasterr()
{
  interpreter& interp = *interpreter::g_interp;
  return interp.errmsg.c_str();
}

extern "C"
pure_expr *lasterrpos()
{
  interpreter& interp = *interpreter::g_interp;
  size_t n = interp.errpos.size();
  if (n == 0) return pure_listl(0);
  pure_expr **xv = (pure_expr**)malloc(n*sizeof(pure_expr*));
  if (!xv) return 0;
  n = 0;
  for (list<errinfo>::const_iterator it = interp.errpos.begin(),
	 end = interp.errpos.end(); it != end; ++it) {
    // remove trailing newline in message, if any
    char *s = strdup(it->msg.c_str());
    assert(s);
    size_t l = strlen(s);
    if (l > 0 && s[l-1] == '\n') s[--l] = 0;
    if (it->line1 > 0 && it->col1 > 0 && it->line2 > 0 && it->col2 > 0)
      xv[n++] = pure_tuplel(6, pure_cstring(s),
			    pure_cstring_dup(it->filename.c_str()),
			    pure_int(it->line1-1), pure_int(it->col1-1),
			    pure_int(it->line2-1), pure_int(it->col2-1));
    else if (l > 0)
      xv[n++] = pure_cstring(s);
  }
  pure_expr *x = pure_listv(n, xv);
  free(xv);
  return x;
}

extern "C"
void clear_lasterr()
{
  interpreter& interp = *interpreter::g_interp;
  interp.errmsg.clear(); interp.errpos.clear();
}

extern "C"
pure_expr *lastres()
{
  interpreter& interp = *interpreter::g_interp;
  return interp.lastres;
}

extern "C"
pure_expr *get_fundef(pure_expr *f)
{
  if (f->tag > 0) {
    interpreter& interp = *interpreter::g_interp;
    return interp.fun_rules(f->tag);
  } else
    return 0;
}

extern "C"
pure_expr *get_typedef(pure_expr *f)
{
  if (f->tag > 0) {
    interpreter& interp = *interpreter::g_interp;
    return interp.type_rules(f->tag);
  } else
    return 0;
}

extern "C"
pure_expr *get_interface(pure_expr *f)
{
  if (f->tag > 0) {
    interpreter& interp = *interpreter::g_interp;
    return interp.interface_patterns(f->tag);
  } else
    return 0;
}

extern "C"
pure_expr *get_interface_typedef(pure_expr *f)
{
  if (f->tag > 0) {
    interpreter& interp = *interpreter::g_interp;
    return interp.interface_rules(f->tag);
  } else
    return 0;
}

extern "C"
pure_expr *get_macdef(pure_expr *f)
{
  if (f->tag > 0) {
    interpreter& interp = *interpreter::g_interp;
    return interp.mac_rules(f->tag);
  } else
    return 0;
}

extern "C"
pure_expr *get_vardef(pure_expr *f)
{
  if (f->tag > 0) {
    interpreter& interp = *interpreter::g_interp;
    env::const_iterator it = interp.globenv.find(f->tag);
    if (it != interp.globenv.end() && it->second.t == env_info::fvar)
      return pure_listl
	(1, pure_appl(pure_symbol(interp.symtab.eqn_sym().f), 2, f, eval(f)));
    else
      return pure_listl(0);
  } else
    return 0;
}

extern "C"
pure_expr *get_constdef(pure_expr *f)
{
  if (f->tag > 0) {
    interpreter& interp = *interpreter::g_interp;
    env::const_iterator it = interp.globenv.find(f->tag);
    if (it != interp.globenv.end() && it->second.t == env_info::cvar)
      return pure_listl
	(1, pure_appl(pure_symbol(interp.symtab.eqn_sym().f), 2, f, eval(f)));
    else
      return pure_listl(0);
  } else
    return 0;
}

extern "C"
pure_expr *add_fundef(pure_expr *x)
{
  if (pure_is_listv(x, 0, 0)) {
    interpreter& interp = *interpreter::g_interp;
    bool res = interp.add_fun_rules(x);
    return res?pure_tuplel(0):0;
  } else
    return 0;
}

extern "C"
pure_expr *add_typedef(pure_expr *x)
{
  if (pure_is_listv(x, 0, 0)) {
    interpreter& interp = *interpreter::g_interp;
    bool res = interp.add_type_rules(x);
    return res?pure_tuplel(0):0;
  } else
    return 0;
}

extern "C"
pure_expr *add_interface(pure_expr *f, pure_expr *x)
{
  if (f->tag > 0 && pure_is_listv(x, 0, 0)) {
    interpreter& interp = *interpreter::g_interp;
    bool res = interp.add_interface_rules(f->tag, x);
    return res?pure_tuplel(0):0;
  } else
    return 0;
}

extern "C"
pure_expr *add_macdef(pure_expr *x)
{
  if (pure_is_listv(x, 0, 0)) {
    interpreter& interp = *interpreter::g_interp;
    bool res = interp.add_mac_rules(x);
    return res?pure_tuplel(0):0;
  } else
    return 0;
}

extern "C"
pure_expr *add_fundef_at(pure_expr *y, pure_expr *x)
{
  if (pure_is_listv(x, 0, 0)) {
    interpreter& interp = *interpreter::g_interp;
    bool res = interp.add_fun_rules_at(y, x);
    return res?pure_tuplel(0):0;
  } else
    return 0;
}

extern "C"
pure_expr *add_typedef_at(pure_expr *y, pure_expr *x)
{
  if (pure_is_listv(x, 0, 0)) {
    interpreter& interp = *interpreter::g_interp;
    bool res = interp.add_type_rules_at(y, x);
    return res?pure_tuplel(0):0;
  } else
    return 0;
}

extern "C"
pure_expr *add_interface_at(pure_expr *f, pure_expr *y, pure_expr *x)
{
  if (f->tag > 0 && pure_is_listv(x, 0, 0)) {
    interpreter& interp = *interpreter::g_interp;
    bool res = interp.add_interface_rules_at(f->tag, y, x);
    return res?pure_tuplel(0):0;
  } else
    return 0;
}

extern "C"
pure_expr *add_macdef_at(pure_expr *y, pure_expr *x)
{
  if (pure_is_listv(x, 0, 0)) {
    interpreter& interp = *interpreter::g_interp;
    bool res = interp.add_mac_rules_at(y, x);
    return res?pure_tuplel(0):0;
  } else
    return 0;
}

extern "C"
pure_expr *add_vardef(pure_expr *x)
{
  pure_expr **xv;
  size_t n;
  interpreter& interp = *interpreter::g_interp;
  interp.errmsg.clear(); interp.errpos.clear();
  if (pure_is_listv(x, &n, &xv)) {
    bool res = true;
    for (size_t i = 0; i < n; i++) {
      pure_expr *f, **yv;
      size_t m;
      if (pure_is_appv(xv[i], &f, &m, 0) && m == 2 &&
	  f->tag == interp.symtab.eqn_sym().f) {
	res = pure_is_appv(xv[i], &f, &m, &yv) && yv[0]->tag > 0 &&
	  interp.add_var(yv[0]->tag, yv[1]);
	free(yv);
	if (!res) break;
      }
    }
    free(xv);
    return res?pure_tuplel(0):0;
  } else
    return 0;
}

extern "C"
pure_expr *add_constdef(pure_expr *x)
{
  pure_expr **xv;
  size_t n;
  interpreter& interp = *interpreter::g_interp;
  interp.errmsg.clear(); interp.errpos.clear();
  if (pure_is_listv(x, &n, &xv)) {
    bool res = true;
    for (size_t i = 0; i < n; i++) {
      pure_expr *f, **yv;
      size_t m;
      if (pure_is_appv(xv[i], &f, &m, 0) && m == 2 &&
	  f->tag == interp.symtab.eqn_sym().f) {
	res = pure_is_appv(xv[i], &f, &m, &yv) && yv[0]->tag > 0 &&
	  interp.add_const(yv[0]->tag, yv[1]);
	free(yv);
	if (!res) break;
      }
    }
    free(xv);
    return res?pure_tuplel(0):0;
  } else
    return 0;
}

extern "C"
pure_expr *del_fundef(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  bool res = interp.del_fun_rule(x);
  return res?pure_tuplel(0):0;
}

extern "C"
pure_expr *del_typedef(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  bool res = interp.del_type_rule(x);
  return res?pure_tuplel(0):0;
}

extern "C"
pure_expr *del_interface(pure_expr *f, pure_expr *x)
{
  if (f->tag > 0) {
    interpreter& interp = *interpreter::g_interp;
    bool res = interp.del_interface_rule(f->tag, x);
    return res?pure_tuplel(0):0;
  } else
    return 0;
}

extern "C"
pure_expr *del_macdef(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  bool res = interp.del_mac_rule(x);
  return res?pure_tuplel(0):0;
}

extern "C"
pure_expr *del_vardef(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  bool res = x->tag>0 && interp.del_var(x->tag);
  return res?pure_tuplel(0):0;
}

extern "C"
pure_expr *del_constdef(pure_expr *x)
{
  interpreter& interp = *interpreter::g_interp;
  bool res = x->tag>0 && interp.del_const(x->tag);
  return res?pure_tuplel(0):0;
}

/* Helper functions to swap byte for on-the-fly endianness conversion. */

static char *swap_endian(char* s, const int nbytes)
{
  static char t[16];
  switch (nbytes) {
  case 2:
    t[0]=*(s+1);
    t[1]=*(s  );
    break;
  case 3:
    t[0]=*(s+2);
    t[1]=*(s+1);
    t[2]=*(s  );
    break;
  case 4:
    t[0]=*(s+3);
    t[1]=*(s+2);
    t[2]=*(s+1);
    t[3]=*(s  );
    break;
  case 8:
    t[0]=*(s+7);
    t[1]=*(s+6);
    t[2]=*(s+5);
    t[3]=*(s+4);
    t[4]=*(s+3);
    t[5]=*(s+2);
    t[6]=*(s+1);
    t[7]=*(s  );
    break;
  case 16:
    t[0]=*(s+15);
    t[1]=*(s+14);
    t[2]=*(s+13);
    t[3]=*(s+12);
    t[4]=*(s+11);
    t[5]=*(s+10);
    t[6]=*(s+9);
    t[7]=*(s+8);
    t[8]=*(s+7);
    t[9]=*(s+6);
    t[10]=*(s+5);
    t[11]=*(s+4);
    t[12]=*(s+3);
    t[13]=*(s+2);
    t[14]=*(s+1);
    t[15]=*(s  );
    break;
  }
  return t;
}

static inline int16_t swap_int16(int16_t x)
{
  return *(int16_t*)swap_endian((char*)&x, sizeof(int16_t));
}

static inline uint16_t swap_uint16(uint16_t x)
{
  return *(uint16_t*)swap_endian((char*)&x, sizeof(uint16_t));
}

static inline int32_t swap_int32(int32_t x)
{
  return *(int32_t*)swap_endian((char*)&x, sizeof(int32_t));
}

static inline uint32_t swap_uint32(uint32_t x)
{
  return *(uint32_t*)swap_endian((char*)&x, sizeof(uint32_t));
}

static inline int64_t swap_int64(int64_t x)
{
  return *(int64_t*)swap_endian((char*)&x, sizeof(int64_t));
}

static inline uint64_t swap_uint64(uint64_t x)
{
  return *(uint64_t*)swap_endian((char*)&x, sizeof(uint64_t));
}

// NOTE: We assume IEEE754 here.
static inline double swap_double(double x)
{
  return *(double*)swap_endian((char*)&x, sizeof(double));
}

/* Expression serialization. */

// Special tags for list and tuple values and back references.
#define __LIST  (-48)
#define __LIST2 (-47)
#define __TUPLE (-46)
#define __REF   (-45)

// Magic header.
#define MAGIC 0x87329d00

// Platform-agnostic data types (up to endianness).
typedef uint64_t mysize_t;
typedef uint8_t mybool;
typedef int32_t myprec_t;
typedef uint16_t myfix_t;

// Be careful to lay out the fields in such a manner that the structs
// hopefully work on all 32 and 64 bit systems. This probably requires gcc.

struct hdrdata {
  int32_t tag;
  uint32_t crc;
  mysize_t n1, n2;
};

struct data1 {
  int32_t tag;
  mysize_t n __attribute__ ((aligned (8)));
};

struct data2 {
  int32_t tag;
  mysize_t n1 __attribute__ ((aligned (8))), n2;
};

struct symdata {
  int32_t tag;
  mybool clos;
};

struct symentry {
  int32_t f, g;
  myprec_t prec;
  myfix_t fix;
  mybool priv;
  const char *s __attribute__ ((aligned (8)));
};

struct symentrydata {
  int32_t f, g;
  myprec_t prec;
  myfix_t fix;
  mybool priv;
  mysize_t offs __attribute__ ((aligned (8)));
};

struct Blob {
  void *buf, *mem;
  size_t pos, size;
  int src_endian, dest_endian;
  map<int32_t,symentry> symtab;
  size_t align(size_t pos)
  {
    // Align to 8 byte boundaries.
    unsigned a = pos%8;
    if (a > 0) a = 8-a;
    return pos+a;
  }
  // Create and write a blob.
  Blob()
    : buf(0), mem(0), pos(0), size(0), src_endian(0), dest_endian(0)
  {
    write_header();
  }
  void ensure(size_t n)
  {
    // Resize the buffer to make room for n more bytes.
    if (pos+n > size) {
      size_t needed = pos+n;
      // Allocate memory in 8K chunks.
      if (needed % 8192 > 0)
	needed = ((needed/8192)+1)*8192;
      assert(needed >= 8192);
      void *p = realloc(buf, needed);
      assert(p && "memory allocation error");
      buf = p; size = needed;
    }
  }
  void write(size_t n, const void *data, bool a = true)
  {
    if (a) {
      unsigned a = align(pos)-pos;
      ensure(a+n);
      pos += a;
    } else
      ensure(n);
    if (n > 0) {
      assert(data);
      memcpy((char*)buf+pos, data, n);
      pos += n;
    }
  }
  void write_header()
  {
    hdrdata h = { (int32_t)MAGIC, // magic header
		  // these will be patched up later
		  0,   // crc
		  0,   // size
		  0 }; // offset of symbol table
    write(sizeof(hdrdata), &h);
  }
  void write_symtab()
  {
    size_t n = symtab.size();
    // A tag of zero marks the beginning of the symbol table.
    data1 d = {0, n};
    write(sizeof(data1), &d);
    write(0, 0); // force alignment
    // Calculate offsets of the symbol and the string table.
    size_t t_pos = pos, s_pos = align(t_pos+n*sizeof(symentrydata));
    // Symbol table entries.
    for (map<int32_t,symentry>::iterator it = symtab.begin(),
	   end = symtab.end(); it != end; ++it) {
      symentry& e = it->second;
      symentrydata ed = {e.f, e.g, e.prec, e.fix, e.priv, s_pos};
      write(sizeof(symentrydata), &ed, false);
      s_pos += strlen(it->second.s)+1;
    }
    write(0, 0); // force alignment
    // Print names of symbols.
    for (map<int32_t,symentry>::iterator it = symtab.begin(),
	   end = symtab.end(); it != end; ++it) {
      symentry& e = it->second;
      size_t l = strlen(e.s);
      write(l+1, e.s, false);
    }
  }
  void fini(bool rid = false)
  {
    if (pos == 0) rid = true;
    if (rid && buf) {
      free(buf);
      buf = 0; pos = size = 0;
    } else if (buf && size > pos) {
      size_t t_pos = align(pos);
      write_symtab();
      // Write an extra end marker.
      int32_t marker = -4711;
      write(sizeof(int32_t), &marker, false);
      void *p = realloc(buf, pos);
      if (p) buf = p; size = pos;
      // Fix up header information.
      hdrdata *h = (hdrdata*)buf;
      size_t ofs = align(sizeof(hdrdata));
      h->crc = cksum(size-ofs, (const unsigned char*)buf+ofs);
      h->n1 = size;
      h->n2 = t_pos;
    }
  }
  void dump(int32_t tag)
  {
    assert(tag == EXPR::APP);
    write(sizeof(int32_t), &tag);
  }
  void dump(int32_t tag, int32_t x)
  {
    assert(tag == EXPR::INT);
    write(sizeof(int32_t), &tag);
    write(sizeof(int32_t), &x);
  }
  void dump(int32_t tag, mpz_t x)
  {
    assert(tag == EXPR::BIGINT);
    size_t count;
    void *p = mpz_export(NULL, &count, -1, sizeof(uint32_t), 0, 0, x);
    assert(count == 0 || p);
    data1 d = {tag, (mysize_t)(mpz_sgn(x)*(int64_t)count)};
    write(sizeof(data1), &d);
    write(count*sizeof(uint32_t), p);
    free(p);
  }
  void dump(int32_t tag, double x)
  {
    assert(tag == EXPR::DBL);
    write(sizeof(int32_t), &tag);
    write(sizeof(double), &x);
  }
  void dump(int32_t tag, const char *x)
  {
    assert(tag == EXPR::STR);
    size_t n = strlen(x)+1;
    data1 d = {tag, n};
    write(sizeof(data1), &d);
    write(n, x);
  }
  void dump(int32_t tag, const void *x)
  {
    assert(tag == EXPR::PTR);
    write(sizeof(int32_t), &tag);
    write(sizeof(void*), &x);
  }
  void dump(int32_t tag, size_t n)
  {
    assert(tag == __LIST || tag == __LIST2 || tag == __TUPLE || tag == __REF);
    data1 d = {tag, n};
    write(sizeof(data1), &d);
  }
  void dump(int32_t tag, size_t n1, size_t n2)
  {
    assert(tag == EXPR::MATRIX);
    data2 d = {tag, n1, n2};
    write(sizeof(data2), &d);
  }
  void dump(int32_t tag, size_t n1, size_t n2, size_t tda, void *p)
  {
    assert(tag == EXPR::DMATRIX || tag == EXPR::IMATRIX ||
	   tag == EXPR::CMATRIX);
    data2 d = {tag, n1, n2};
    write(sizeof(data2), &d);
    size_t k = tag == EXPR::DMATRIX?sizeof(double):
      tag == EXPR::IMATRIX?sizeof(int):2*sizeof(double);
    if (tda == n2)
      write(n1*n2*k, p);
    else {
      // Fragmented data, do one write per row.
      write(0, 0); // force alignment
      char *q = (char*)p;
      for (size_t i = 0; i < n1; i++) {
	write(n2*k, q, false);
	q += tda*k;
      }
    }
  }
  void dump(int32_t tag, bool clos, const symbol &sym)
  {
    assert(tag > 0 && tag == sym.f);
    symdata d = {tag, clos};
    write(sizeof(symdata), &d);
    symentry e =
      {sym.f, sym.g, sym.prec, (myfix_t)sym.fix, sym.priv, sym.s.c_str()};
    if (symtab.find(tag) == symtab.end())
      symtab[tag] = e;
  }
  void dump(const symbol &sym)
  {
    symentry e =
      {sym.f, sym.g, sym.prec, (myfix_t)sym.fix, sym.priv, sym.s.c_str()};
    if (symtab.find(sym.f) == symtab.end())
      symtab[sym.f] = e;
  }
  inline int16_t swap(int16_t x)
  {
    if (src_endian != dest_endian)
      return swap_int16(x);
    else
      return x;
  }
  inline int32_t swap(int32_t x)
  {
    if (src_endian != dest_endian)
      return swap_int32(x);
    else
      return x;
  }
  inline int64_t swap(int64_t x)
  {
    if (src_endian != dest_endian)
      return swap_int64(x);
    else
      return x;
  }
  inline uint16_t swap(uint16_t x)
  {
    if (src_endian != dest_endian)
      return swap_uint16(x);
    else
      return x;
  }
  inline uint32_t swap(uint32_t x)
  {
    if (src_endian != dest_endian)
      return swap_uint32(x);
    else
      return x;
  }
  inline uint64_t swap(uint64_t x)
  {
    if (src_endian != dest_endian)
      return swap_uint64(x);
    else
      return x;
  }
  inline double swap(double x)
  {
    if (src_endian != dest_endian)
      return swap_double(x);
    else
      return x;
  }
  // Verify and read a blob.
  Blob(void *data)
    : buf(0), mem(0), pos(0), size(0), src_endian(0), dest_endian(0)
  {
    hdrdata *h = (hdrdata*)data;
    if (!data || !(h->tag == (int32_t)MAGIC ||
		   swap_int32(h->tag) == (int32_t)MAGIC)) return;
    // Determine source and destination endianness.
    dest_endian = WORDS_BIGENDIAN?1:-1;
    src_endian = h->tag==(int32_t)MAGIC?dest_endian:-dest_endian;
    size_t n1 = swap(h->n1), n2 = swap(h->n2); uint32_t crc = swap(h->crc);
    if (n1 < n2) return;
    int32_t tag = swap(*(int32_t*)((char*)data+n2));
    int32_t marker = swap(*(int32_t*)((char*)data+n1-sizeof(int32_t)));
    if (tag != 0 || marker != -4711) return;
    size_t ofs = align(sizeof(hdrdata));
    uint32_t mycrc = cksum(n1-ofs, (const unsigned char*)data+ofs);
    if (mycrc != crc) return; // failed crc check
    data1 *d = (data1*)((char*)data+n2);
    if (swap(d->tag) != 0) return; // invalid symbol table format
    size_t t_pos = align(n2+sizeof(data1));
    symentrydata *t = (symentrydata*)((char*)data+t_pos);
    size_t n = swap(d->n);
    if (t_pos+n*sizeof(symentrydata) > n1) return;
    for (size_t i = 0; i < n; i++) {
      symentrydata& ed = t[i];
      if (ed.f <= 0) return;
      symentry e = {swap(ed.f), swap(ed.g), swap(ed.prec), swap(ed.fix),
		    ed.priv, (char*)data+swap(ed.offs)};
      symtab[swap(ed.f)] = e;
    }
    // All nice and well so far. Assume that it's a valid blob.
    buf = data;
    size = n1;
    pos = align(sizeof(hdrdata));
  }
  ~Blob() {
    if (mem) free(mem);
  }
  void *mkmem(size_t size)
  {
    if (mem) free(mem);
    mem = size>0?malloc(size):0;
    assert(size == 0 || mem != 0);
    return mem;
  }
  bool verify()
  {
    return buf!=0;
  }
  int32_t tag()
  {
    pos = align(pos);
    if (pos+sizeof(int32_t) > size) return 0;
    int32_t tag = swap(*(int32_t*)((char*)buf+pos));
    return tag;
  }
  void read(size_t n, void*& data, bool a = true)
  {
    if (a) pos = align(pos);
    assert(pos+n<=size);
    data = (char*)buf+pos;
    pos += n;
  }
  void load(int32_t& tag)
  {
    void *p;
    read(sizeof(int32_t), p);
    tag = swap(*(int32_t*)p);
    assert(tag == EXPR::APP || tag>0);
  }
  void load(int32_t& tag, int32_t& x)
  {
    void *p;
    read(sizeof(int32_t), p);
    tag = swap(*(int32_t*)p);
    assert(tag == EXPR::INT);
    read(sizeof(int32_t), p);
    x = swap(*(int32_t*)p);
  }
  void load(int32_t& tag, mpz_t& x)
  {
    void *p;
    data1 *d;
    read(sizeof(data1), p);
    d = (data1*)p;
    tag = swap(d->tag); mysize_t n = swap(d->n);
    assert(tag == EXPR::BIGINT);
    int64_t len = (int64_t)n;
    int8_t sgn = (len<0)?-1:1;
    n = (mysize_t)(sgn*len);
    read(n*sizeof(uint32_t), p);
    mpz_import(x, n, -1, sizeof(uint32_t), src_endian, 0, p);
    x->_mp_size = sgn*x->_mp_size;
  }
  void load(int32_t& tag, double& x)
  {
    void *p;
    read(sizeof(int32_t), p);
    tag = swap(*(int32_t*)p);
    assert(tag == EXPR::DBL);
    read(sizeof(double), p);
    x = swap(*(double*)p);
  }
  void load(int32_t& tag, char*& x)
  {
    void *p;
    data1 *d;
    read(sizeof(data1), p);
    d = (data1*)p;
    tag = swap(d->tag); mysize_t n = swap(d->n);
    assert(tag == EXPR::STR && n>0);
    read(n, p);
    x = (char*)p;
    assert(x[n-1] == 0);
  }
  void load(int32_t& tag, void*& x)
  {
    void *p;
    read(sizeof(int32_t), p);
    tag = swap(*(int32_t*)p);
    assert(tag == EXPR::PTR);
    read(sizeof(void*), p);
    // This must always be a NULL pointer.
    //x = *(void**)p;
    x = NULL;
  }
  void load(int32_t& tag, size_t& n)
  {
    void *p;
    data1 *d;
    read(sizeof(data1), p);
    d = (data1*)p;
    tag = swap(d->tag); n = swap(d->n);
    assert(tag == __LIST || tag == __LIST2 || tag == __TUPLE || tag == __REF);
  }
  void load(int32_t& tag, size_t& n1, size_t& n2)
  {
    void *p;
    data2 *d;
    read(sizeof(data2), p);
    d = (data2*)p;
    tag = swap(d->tag); n1 = swap(d->n1); n2 = swap(d->n2);
    assert(tag == EXPR::MATRIX);
  }
  void load(int32_t& tag, size_t& n1, size_t& n2, void*& _p)
  {
    void *p;
    read(sizeof(data2), p);
    data2 *d = (data2*)p;
    tag = swap(d->tag); n1 = swap(d->n1); n2 = swap(d->n2);
    assert(tag == EXPR::DMATRIX || tag == EXPR::IMATRIX ||
	   tag == EXPR::CMATRIX);
    switch (tag) {
    case EXPR::DMATRIX:
      read(n1*n2*sizeof(double), _p);
      if (src_endian != dest_endian) {
	double *p = (double*)mkmem(n1*n2*sizeof(double));
	double *__p = (double*)_p;
	for (size_t i = 0; i < n1*n2; i++)
	  p[i] = swap(__p[i]);
	_p = p;
      }
      break;
    case EXPR::IMATRIX:
      read(n1*n2*sizeof(int), _p);
      if (src_endian != dest_endian) {
	int *p = (int*)mkmem(n1*n2*sizeof(int));
	int *__p = (int*)_p;
	for (size_t i = 0; i < n1*n2; i++)
	  p[i] = swap(__p[i]);
	_p = p;
      }
      break;
    case EXPR::CMATRIX:
      read(2*n1*n2*sizeof(double), _p);
      if (src_endian != dest_endian) {
	double *p = (double*)mkmem(2*n1*n2*sizeof(double));
	double *__p = (double*)_p;
	for (size_t i = 0; i < 2*n1*n2; i++)
	  p[i] = swap(__p[i]);
	_p = p;
      }
      break;
    default:
      assert(0 && "this can't happen");
      _p = 0;
      break;
    }
  }
  void load(int32_t& tag, bool& clos)
  {
    void *p;
    symdata *d;
    read(sizeof(symdata), p);
    d = (symdata*)p;
    tag = swap(d->tag); clos = d->clos;
    assert(tag > 0);
  }
  void print_symtab()
  {
    // Print the embedded symbol table (useful for debugging purposes).
    for (map<int32_t,symentry>::iterator it = symtab.begin(),
	   end = symtab.end(); it != end; ++it) {
      symentry& e = it->second;
      if (e.fix != outfix || e.g) {
	if (e.priv)
	  cout << "private";
	else
	  cout << "public";
      }
      switch (e.fix) {
      case infix:
	if (e.prec < PREC_MAX) cout << " infix " << e.prec;
	break;
      case infixl:
	cout << " infixl " << e.prec;
	break;
      case infixr:
	cout << " infixr " << e.prec;
	break;
      case prefix:
	cout << " prefix " << e.prec;
	break;
      case postfix:
	cout << " postfix " << e.prec;
	break;
      case outfix:
	if (e.g) cout << " outfix";
	break;
      case nonfix:
	cout << " nonfix";
	break;
      default:
	break;
      }
      if (e.fix == outfix) {
	if (e.g) cout << " " << e.s << " " << symtab[e.g].s << '\n';
      } else
	cout << " " << e.s << '\n';
    }
  }
};

static bool dump(Blob &b, map<pure_expr*,size_t>& ref, size_t& key,
		 pure_expr *x)
{
  char test;
  {
    map<pure_expr*,size_t>::iterator kt = ref.find(x);
    if (kt != ref.end()) {
      b.dump(__REF, kt->second);
      return true;
    }
  }
  switch (x->tag) {
  case EXPR::APP: {
    checkstk(test);
    size_t size;
    pure_expr **elems, *tl;
    if (is_list2(x, size, elems, tl)) {
      bool null = is_nil(tl);
      if (null)
	b.dump(__LIST, size);
      else
	b.dump(__LIST2, size+1);
      for (size_t i = 0; i < size; i++)
	if (!dump(b, ref, key, elems[i])) {
	  free(elems);
	  return false;
	}
      free(elems);
      if (!null && !dump(b, ref, key, tl))
	return false;
    } else if (is_tuple(x, size, elems)) {
      b.dump(__TUPLE, size);
      for (size_t i = 0; i < size; i++)
	if (!dump(b, ref, key, elems[i])) {
	  free(elems);
	  return false;
	}
      free(elems);
    } else {
      b.dump(EXPR::APP);
      if (!dump(b, ref, key, x->data.x[0]) ||
	  !dump(b, ref, key, x->data.x[1]))
	return false;
    }
    ref[x] = key++;
    return true;
  }
  case EXPR::INT:
    b.dump(EXPR::INT, x->data.i);
    ref[x] = key++;
    return true;
  case EXPR::BIGINT:
    b.dump(EXPR::BIGINT, x->data.z);
    ref[x] = key++;
    return true;
  case EXPR::DBL:
    b.dump(EXPR::DBL, x->data.d);
    ref[x] = key++;
    return true;
  case EXPR::STR:
    b.dump(EXPR::STR, x->data.s);
    ref[x] = key++;
    return true;
  case EXPR::PTR:
    // Only NULL pointers allowed.
    if (x->data.p)
      return false;
    else {
      b.dump(EXPR::PTR, x->data.p);
      ref[x] = key++;
      return true;
    }
  case EXPR::DMATRIX:
    if (x->data.mat.p) {
      gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
      size_t n1 = m->size1, n2 = m->size2;
      b.dump(EXPR::DMATRIX, n1, n2, m->tda, m->data);
      ref[x] = key++;
      return true;
    } else
      return false;
  case EXPR::IMATRIX:
    if (x->data.mat.p) {
      gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
      size_t n1 = m->size1, n2 = m->size2;
      b.dump(EXPR::IMATRIX, n1, n2, m->tda, m->data);
      ref[x] = key++;
      return true;
    } else
      return false;
  case EXPR::CMATRIX:
    if (x->data.mat.p) {
      gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
      size_t n1 = m->size1, n2 = m->size2;
      b.dump(EXPR::CMATRIX, n1, n2, m->tda, m->data);
      ref[x] = key++;
      return true;
    } else
      return false;
  case EXPR::MATRIX:
    if (x->data.mat.p) {
      checkstk(test);
      gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
      size_t n1 = m->size1, n2 = m->size2;
      b.dump(EXPR::MATRIX, n1, n2);
      for (size_t i = 0; i < n1; i++)
	for (size_t j = 0; j < n2; j++)
	  if (!dump(b, ref, key, m->data[i*m->tda+j]))
	    return false;
      ref[x] = key++;
      return true;
    } else
      return false;
  default: {
    assert(x->tag >= 0);
    // Only plain symbols or global named closures are allowed here.
    if (x->tag > 0 && (!x->data.clos || !x->data.clos->local)) {
      interpreter& interp = *interpreter::g_interp;
      symbol &sym = interp.symtab.sym(x->tag);
      b.dump(x->tag, x->data.clos != 0, sym);
      if (sym.fix == outfix && sym.g)
	b.dump(interp.symtab.sym(sym.g));
      ref[x] = key++;
      return true;
    } else
      return false;
  }
  }
}

static int32_t make_symbol(symentry& e, symentry* e2)
{
  int32_t tag = e.f;
  if (tag == 0) return 0;
  // First check to see whether we already have an entry for this symbol.
  interpreter& interp = *interpreter::g_interp;
  if (tag <= interp.symtab.nsyms()) {
    symbol &sym = interp.symtab.sym(tag);
    if (strcmp(sym.s.c_str(), e.s) == 0) {
      /* Verify the symbols for consistency. We don't actually care about the
	 exact precedence levels and whether it's a private symbol or not, but
	 the fixities ought to match. */
      if (sym.fix != e.fix ||
	  (e.fix == outfix && (e.g==0)!=(sym.g==0))) {
	/* Mark this symbol as "bad" so that we can bail out immediately the
	   next time we see it. */
	e.f = 0;
	return 0;
      } else
	return tag;
    }
  }
  // Next check for a similar entry.
  tag = pure_getsym(e.s);
  if (tag > 0) {
    symbol &sym = interp.symtab.sym(tag);
    if (sym.fix != e.fix ||
	(e.fix == outfix && (e.g==0)!=(sym.g==0))) {
      e.f = 0;
      return 0;
    } else
      return tag;
  }
  // No existing symbol, create a new one.
  if (e.fix == outfix && e2 && pure_getsym(e2->s) > 0) {
    /* We have an outfix symbol, but its right bracket is already in use.
       There's really not much we can do here, so give up. */
    e.f = 0;
    return 0;
  }
  tag = pure_sym(e.s);
  assert(tag > 0);
  symbol& sym = interp.symtab.sym(tag);
  sym.prec = (prec_t)e.prec; sym.fix = (fix_t)e.fix; sym.priv = (bool)e.priv;
  if (e.fix == outfix && e2)
    sym.g = make_symbol(*e2, 0);
  return tag;
}

static inline pure_expr *add_ref(map<size_t,pure_expr*>& ref, size_t& key,
				 pure_expr *x)
{
  ref[key++] = x;
  return x;
}

static pure_expr *load(Blob &b, map<size_t,pure_expr*>& ref, size_t& key)
{
  char test;
  int32_t tag;
  size_t n, n1, n2;
  switch (b.tag()) {
  case __REF: {
    b.load(tag, n);
    map<size_t,pure_expr*>::iterator kt = ref.find(n);
    if (kt != ref.end())
      return kt->second;
    else
      return 0;
  }
  case __LIST: {
    checkstk(test);
    b.load(tag, n);
    pure_expr **elems = new pure_expr*[n];
    for (size_t i = 0; i < n; i++) {
      elems[i] = load(b, ref, key);
      if (!elems[i]) {
	pure_new_vect(i, elems);
	for (size_t j = 0; j < i; j++)
	  pure_free_internal(elems[j]);
	delete[] elems;
	return 0;
      }
    }
    pure_expr *x = pure_listv(n, elems);
    delete[] elems;
    return add_ref(ref, key, x);
  }
  case __LIST2: {
    checkstk(test);
    b.load(tag, n);
    assert(n>0); n--;
    pure_expr **elems = new pure_expr*[n];
    for (size_t i = 0; i < n; i++) {
      elems[i] = load(b, ref, key);
      if (!elems[i]) {
	pure_new_vect(i, elems);
	for (size_t j = 0; j < i; j++)
	  pure_free_internal(elems[j]);
	delete[] elems;
	return 0;
      }
    }
    pure_expr *tl = load(b, ref, key);
    if (!tl) {
      pure_new_vect(n, elems);
      for (size_t j = 0; j < n; j++)
	pure_free_internal(elems[j]);
      delete[] elems;
      return 0;
    }
    pure_expr *x = pure_listv2(n, elems, tl);
    delete[] elems;
    return add_ref(ref, key, x);
  }
  case __TUPLE: {
    checkstk(test);
    b.load(tag, n);
    pure_expr **elems = new pure_expr*[n];
    for (size_t i = 0; i < n; i++) {
      elems[i] = load(b, ref, key);
      if (!elems[i]) {
	pure_new_vect(i, elems);
	for (size_t j = 0; j < i; j++)
	  pure_free_internal(elems[j]);
	delete[] elems;
	return 0;
      }
    }
    pure_expr *x = pure_tuplev(n, elems);
    delete[] elems;
    return add_ref(ref, key, x);
  }
  case EXPR::APP: {
    checkstk(test);
    b.load(tag);
    pure_expr *x = load(b, ref, key);
    if (!x) return 0;
    pure_expr *y = load(b, ref, key);
    if (!y) {
      pure_freenew(x);
      return 0;
    }
    return add_ref(ref, key, pure_apply2(x, y));
  }
  case EXPR::INT: {
    int32_t x;
    b.load(tag, x);
    return add_ref(ref, key, pure_int(x));
  }
  case EXPR::BIGINT: {
    mpz_t z;
    mpz_init(z);
    b.load(tag, z);
    pure_expr *x = pure_mpz(z);
    mpz_clear(z);
    return add_ref(ref, key, x);
  }
  case EXPR::DBL: {
    double x;
    b.load(tag, x);
    return add_ref(ref, key, pure_double(x));
  }
  case EXPR::STR: {
    char *x;
    b.load(tag, x);
    return add_ref(ref, key, pure_string_dup(x));
  }
  case EXPR::PTR: {
    void *x;
    b.load(tag, x);
    assert(x == NULL);
    return add_ref(ref, key, pure_pointer(x));
  }
  case EXPR::DMATRIX: {
    void *p;
    b.load(tag, n1, n2, p);
    return add_ref(ref, key, matrix_from_double_array(n1, n2, p));
  }
  case EXPR::IMATRIX: {
    void *p;
    b.load(tag, n1, n2, p);
    return add_ref(ref, key, matrix_from_int_array(n1, n2, p));
  }
  case EXPR::CMATRIX: {
    void *p;
    b.load(tag, n1, n2, p);
    return add_ref(ref, key, matrix_from_complex_array(n1, n2, p));
  }
  case EXPR::MATRIX: {
    checkstk(test);
    b.load(tag, n1, n2);
    n = n1*n2;
    pure_expr **elems = new pure_expr*[n];
    for (size_t i = 0; i < n; i++) {
      elems[i] = load(b, ref, key);
      if (!elems[i]) {
	pure_new_vect(i, elems);
	for (size_t j = 0; j < i; j++)
	  pure_free_internal(elems[j]);
	delete[] elems;
	return 0;
      }
    }
    gsl_matrix_symbolic *m = create_symbolic_matrix(n1, n2);
    assert(m);
    pure_expr **data = m->data;
    size_t tda = m->tda;
    for (size_t i = 0; i < n1; i++)
      for (size_t j = 0; j < n2; j++)
	data[i*tda+j] = elems[i*n2+j];
    pure_expr *x = pure_symbolic_matrix(m);
    delete[] elems;
    return add_ref(ref, key, x);
  }
  default: {
    bool clos;
    b.load(tag, clos);
    assert(tag > 0 && b.symtab.find(tag) != b.symtab.end());
    symentry& e = b.symtab[tag];
    if (e.f == 0) return 0; // bad symbol
    symentry* e2 = (e.fix==outfix&&e.g)?&b.symtab[e.g]:0;
    tag = make_symbol(e, e2);
    if (tag == 0) return 0; // bad symbol
    return add_ref(ref, key, clos?pure_symbol(tag):pure_const(tag));
  }
  }
}

extern "C"
pure_expr *blob(pure_expr *x)
{
  Blob b;
  map<pure_expr*,size_t> ref;
  size_t key = 0;
  bool ret = dump(b, ref, key, x);
  b.fini(!ret);
  if (ret) {
    // This assumes that ::free is declared in the library.
    pure_expr *x = pure_pointer(b.buf),
      *y = pure_symbol(pure_sym("::free"));
    assert(x && y);
    return pure_sentry(y, x);
  } else
    return 0;
}

extern "C"
pure_expr *val(pure_expr *x)
{
  void *p;
  const char *s;
  if (pure_is_pointer(x, &p) && pure_check_tag(0, x)) {
    Blob b(p);
    if (b.verify()) {
      //b.print_symtab();
      map<size_t,pure_expr*> ref;
      size_t key = 0;
      return load(b, ref, key);
    } else
      return 0;
  } else if (pure_is_string(x, &s))
    return pure_val(s);
  else
    return 0;
}

extern "C"
bool blobp(pure_expr *x)
{
  void *p;
  if (pure_is_pointer(x, &p) && p && pure_check_tag(0, x)) {
    hdrdata *h = (hdrdata*)p;
    if (h->tag == (int32_t)MAGIC)
      return h->n1 >= h->n2;
    else if (swap_int32(h->tag) == (int32_t)MAGIC)
      return swap_uint64(h->n1) >= swap_uint64(h->n2);
    else
      return false;
  } else
    return false;
}

extern "C"
pure_expr *blob_size(pure_expr *x)
{
  void *p;
  if (pure_is_pointer(x, &p) && p && pure_check_tag(0, x)) {
    hdrdata *h = (hdrdata*)p;
    if (h->tag == (int32_t)MAGIC)
      return pure_uint64(h->n1);
    else if (swap_int32(h->tag) == (int32_t)MAGIC)
      return pure_uint64(swap_uint64(h->n1));
    else
      return 0;
  } else
    return 0;
}

extern "C"
pure_expr *blob_crc(pure_expr *x)
{
  void *p;
  if (pure_is_pointer(x, &p) && p && pure_check_tag(0, x)) {
    hdrdata *h = (hdrdata*)p;
    if (h->tag == (int32_t)MAGIC)
      return pure_int((int32_t)h->crc);
    else if (swap_int32(h->tag) == (int32_t)MAGIC)
      return pure_int((int32_t)swap_uint32(h->crc));
    else
      return 0;
  } else
    return 0;
}

extern "C"
uint32_t matrix_size(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    return m->size1*m->size2;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    return m->size1*m->size2;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    return m->size1*m->size2;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    return m->size1*m->size2;
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_dim(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    return pure_tuplel(2, pure_int(m->size1), pure_int(m->size2));
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    return pure_tuplel(2, pure_int(m->size1), pure_int(m->size2));
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    return pure_tuplel(2, pure_int(m->size1), pure_int(m->size2));
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    return pure_tuplel(2, pure_int(m->size1), pure_int(m->size2));
  }
  default:
    return 0;
  }
}

extern "C"
uint32_t matrix_stride(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    return m->tda;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    return m->tda;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    return m->tda;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    return m->tda;
  }
  default:
    return 0;
  }
}

extern "C"
int32_t matrix_type(pure_expr *x)
{
  uint32_t t = (uint32_t)x->tag;
  if ((t&0xfffffff0) == (uint32_t)EXPR::MATRIX)
    // the lowest nibble is the subtype tag
    return (int32_t)(t&0xf);
  else
    return -1;
}

extern "C"
bool matrix_check(pure_expr *x, uint32_t k, uint32_t l)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    return k == m->size1 && l == m->size2;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    return k == m->size1 && l == m->size2;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    return k == m->size1 && l == m->size2;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    return k == m->size1 && l == m->size2;
  }
  default:
    return false;
  }
}

extern "C"
pure_expr *matrix_elem_at(pure_expr *x, int32_t _i)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    if (m->tda > m->size2) {
      const size_t i = _i/m->size2, j = _i%m->size2, k = i*m->tda+j;
      return m->data[k];
    } else
      return m->data[_i];
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    if (m->tda > m->size2) {
      const size_t i = _i/m->size2, j = _i%m->size2, k = i*m->tda+j;
      return pure_double(m->data[k]);
    } else
      return pure_double(m->data[_i]);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    if (m->tda > m->size2) {
      const size_t i = _i/m->size2, j = _i%m->size2, k = 2*(i*m->tda+j);
      return make_complex(m->data[k], m->data[k+1]);
    } else {
      const size_t k = 2*_i;
      return make_complex(m->data[k], m->data[k+1]);
    }
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    if (m->tda > m->size2) {
      const size_t i = _i/m->size2, j = _i%m->size2, k = i*m->tda+j;
      return pure_int(m->data[k]);
    } else
      return pure_int(m->data[_i]);
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_elem_at2(pure_expr *x, int32_t i, int32_t j)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    const size_t k = i*m->tda+j;
    return m->data[k];
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    const size_t k = i*m->tda+j;
    return pure_double(m->data[k]);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    const size_t k = 2*(i*m->tda+j);
    return make_complex(m->data[k], m->data[k+1]);
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    const size_t k = i*m->tda+j;
    return pure_int(m->data[k]);
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_slice(pure_expr *x, int32_t i1, int32_t j1,
			int32_t i2, int32_t j2)
{
  void *p = 0;
  if (i1<0) i1 = 0; if (j1<0) j1 = 0;
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    if (i2 >= (int)m->size1) i2 = m->size1-1;
    if (j2 >= (int)m->size2) j2 = m->size2-1;
    size_t n1 = (i1<(int)m->size1 && i2>=i1)?(i2+1-i1):0,
      n2 = (j1<(int)m->size2 && j2>=j1)?(j2+1-j1):0;
    if (n1 == 0 || n2 == 0) // empty matrix
      return pure_symbolic_matrix(create_symbolic_matrix(n1, n2));
    gsl_matrix_symbolic_view v =
      gsl_matrix_symbolic_submatrix(m, i1, j1, n1, n2);
    // take a copy of the view matrix
    gsl_matrix_symbolic *m1 =
      (gsl_matrix_symbolic*)malloc(sizeof(gsl_matrix_symbolic));
    assert(m1 && v.matrix.data);
    *m1 = v.matrix;
    p = m1;
    break;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    if (i2 >= (int)m->size1) i2 = m->size1-1;
    if (j2 >= (int)m->size2) j2 = m->size2-1;
    size_t n1 = (i1<(int)m->size1 && i2>=i1)?(i2+1-i1):0,
      n2 = (j1<(int)m->size2 && j2>=j1)?(j2+1-j1):0;
    if (n1 == 0 || n2 == 0) // empty matrix
      return pure_double_matrix(create_double_matrix(n1, n2));
    gsl_matrix_view v = gsl_matrix_submatrix(m, i1, j1, n1, n2);
    // take a copy of the view matrix
    gsl_matrix *m1 = (gsl_matrix*)malloc(sizeof(gsl_matrix));
    assert(m1 && v.matrix.data);
    *m1 = v.matrix;
    p = m1;
    break;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    if (i2 >= (int)m->size1) i2 = m->size1-1;
    if (j2 >= (int)m->size2) j2 = m->size2-1;
    size_t n1 = (i1<(int)m->size1 && i2>=i1)?(i2+1-i1):0,
      n2 = (j1<(int)m->size2 && j2>=j1)?(j2+1-j1):0;
    if (n1 == 0 || n2 == 0) // empty matrix
      return pure_complex_matrix(create_complex_matrix(n1, n2));
    gsl_matrix_complex_view v =
      gsl_matrix_complex_submatrix(m, i1, j1, n1, n2);
    // take a copy of the view matrix
    gsl_matrix_complex *m1 =
      (gsl_matrix_complex*)malloc(sizeof(gsl_matrix_complex));
    assert(m1 && v.matrix.data);
    *m1 = v.matrix;
    p = m1;
    break;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    if (i2 >= (int)m->size1) i2 = m->size1-1;
    if (j2 >= (int)m->size2) j2 = m->size2-1;
    size_t n1 = (i1<(int)m->size1 && i2>=i1)?(i2+1-i1):0,
      n2 = (j1<(int)m->size2 && j2>=j1)?(j2+1-j1):0;
    if (n1 == 0 || n2 == 0) // empty matrix
      return pure_int_matrix(create_int_matrix(n1, n2));
    gsl_matrix_int_view v = gsl_matrix_int_submatrix(m, i1, j1, n1, n2);
    // take a copy of the view matrix
    gsl_matrix_int *m1 = (gsl_matrix_int*)malloc(sizeof(gsl_matrix_int));
    assert(m1 && v.matrix.data);
    *m1 = v.matrix;
    p = m1;
    break;
  }
  default:
    return 0;
  }
  // create a new expression for the slice, update the reference counter for
  // the underlying GSL matrix
  pure_expr *y = new_expr();
  y->tag = x->tag;
  y->data.mat.p = p;
  y->data.mat.refc = x->data.mat.refc;
  (*y->data.mat.refc)++;
  MEMDEBUG_NEW(y)
  return y;
}

extern "C"
pure_expr *matrix_diag(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n = (n1<n2)?n1:n2;
    gsl_matrix_symbolic *m1 = create_symbolic_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      m1->data[i] = m->data[i*(m->tda+1)];
    return pure_symbolic_matrix(m1);
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n = (n1<n2)?n1:n2;
    gsl_matrix *m1 = create_double_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      m1->data[i] = m->data[i*(m->tda+1)];
    return pure_double_matrix(m1);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n = (n1<n2)?n1:n2;
    gsl_matrix_complex *m1 = create_complex_matrix(1, n);
    for (size_t i = 0; i < n; i++) {
      const size_t k = 2*i*(m->tda+1);
      m1->data[2*i] = m->data[k];
      m1->data[2*i+1] = m->data[k+1];
    }
    return pure_complex_matrix(m1);
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n = (n1<n2)?n1:n2;
    gsl_matrix_int *m1 = create_int_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      m1->data[i] = m->data[i*(m->tda+1)];
    return pure_int_matrix(m1);
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_subdiag(pure_expr *x, int32_t k)
{
  if (k<0) return matrix_supdiag(x, -k);
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n0 = (n1<n2)?n1:n2;
    const size_t n = (n0>(size_t)k)?n0-k:0, k0 = k*m->tda;
    gsl_matrix_symbolic *m1 = create_symbolic_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      m1->data[i] = m->data[k0+i*(m->tda+1)];
    return pure_symbolic_matrix(m1);
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n0 = (n1<n2)?n1:n2;
    const size_t n = (n0>(size_t)k)?n0-k:0, k0 = k*m->tda;
    gsl_matrix *m1 = create_double_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      m1->data[i] = m->data[k0+i*(m->tda+1)];
    return pure_double_matrix(m1);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n0 = (n1<n2)?n1:n2;
    const size_t n = (n0>(size_t)k)?n0-k:0, k0 = k*m->tda;
    gsl_matrix_complex *m1 = create_complex_matrix(1, n);
    for (size_t i = 0; i < n; i++) {
      const size_t k = 2*k0+2*i*(m->tda+1);
      m1->data[2*i] = m->data[k];
      m1->data[2*i+1] = m->data[k+1];
    }
    return pure_complex_matrix(m1);
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n0 = (n1<n2)?n1:n2;
    const size_t n = (n0>(size_t)k)?n0-k:0, k0 = k*m->tda;
    gsl_matrix_int *m1 = create_int_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      m1->data[i] = m->data[k0+i*(m->tda+1)];
    return pure_int_matrix(m1);
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_supdiag(pure_expr *x, int32_t k)
{
  if (k<0) return matrix_subdiag(x, -k);
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n0 = (n1<n2)?n1:n2;
    const size_t n = (n0>(size_t)k)?n0-k:0, k0 = k;
    gsl_matrix_symbolic *m1 = create_symbolic_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      m1->data[i] = m->data[k0+i*(m->tda+1)];
    return pure_symbolic_matrix(m1);
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n0 = (n1<n2)?n1:n2;
    const size_t n = (n0>(size_t)k)?n0-k:0, k0 = k;
    gsl_matrix *m1 = create_double_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      m1->data[i] = m->data[k0+i*(m->tda+1)];
    return pure_double_matrix(m1);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n0 = (n1<n2)?n1:n2;
    const size_t n = (n0>(size_t)k)?n0-k:0, k0 = k;
    gsl_matrix_complex *m1 = create_complex_matrix(1, n);
    for (size_t i = 0; i < n; i++) {
      const size_t k = 2*k0+2*i*(m->tda+1);
      m1->data[2*i] = m->data[k];
      m1->data[2*i+1] = m->data[k+1];
    }
    return pure_complex_matrix(m1);
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2, n0 = (n1<n2)?n1:n2;
    const size_t n = (n0>(size_t)k)?n0-k:0, k0 = k;
    gsl_matrix_int *m1 = create_int_matrix(1, n);
    for (size_t i = 0; i < n; i++)
      m1->data[i] = m->data[k0+i*(m->tda+1)];
    return pure_int_matrix(m1);
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_diagm(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix_symbolic *m1 = create_symbolic_matrix(n, n);
    pure_expr *zero = pure_int(0);
    assert(zero);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
	m1->data[i*m1->tda+j] = zero;
    for (size_t i = 0; i < n; i++)
      m1->data[i*(m1->tda+1)] = m->data[i];
    return pure_symbolic_matrix(m1);
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix *m1 = create_double_matrix(n, n);
    memset(m1->data, 0, m1->block->size*sizeof(double));
    for (size_t i = 0; i < n; i++)
      m1->data[i*(m1->tda+1)] = m->data[i];
    return pure_double_matrix(m1);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix_complex *m1 = create_complex_matrix(n, n);
    memset(m1->data, 0, m1->block->size*2*sizeof(double));
    for (size_t i = 0; i < n; i++) {
      const size_t k = 2*i*(m1->tda+1);
      m1->data[k] = m->data[2*i];
      m1->data[k+1] = m->data[2*i+1];
    }
    return pure_complex_matrix(m1);
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix_int *m1 = create_int_matrix(n, n);
    memset(m1->data, 0, m1->block->size*sizeof(int));
    for (size_t i = 0; i < n; i++)
      m1->data[i*(m1->tda+1)] = m->data[i];
    return pure_int_matrix(m1);
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_subdiagm(pure_expr *x, int32_t k)
{
  if (k<0) return matrix_supdiagm(x, -k);
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix_symbolic *m1 = create_symbolic_matrix(n+k, n+k);
    const size_t k0 = k*m1->tda;
    pure_expr *zero = pure_int(0);
    assert(zero);
    for (size_t i = 0; i < n+k; i++)
      for (size_t j = 0; j < n+k; j++)
	m1->data[i*m1->tda+j] = zero;
    for (size_t i = 0; i < n; i++)
      m1->data[k0+i*(m1->tda+1)] = m->data[i];
    return pure_symbolic_matrix(m1);
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix *m1 = create_double_matrix(n+k, n+k);
    const size_t k0 = k*m1->tda;
    memset(m1->data, 0, m1->block->size*sizeof(double));
    for (size_t i = 0; i < n; i++)
      m1->data[k0+i*(m1->tda+1)] = m->data[i];
    return pure_double_matrix(m1);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix_complex *m1 = create_complex_matrix(n+k, n+k);
    const size_t k0 = k*m1->tda;
    memset(m1->data, 0, m1->block->size*2*sizeof(double));
    for (size_t i = 0; i < n; i++) {
      const size_t k = 2*k0+2*i*(m1->tda+1);
      m1->data[k] = m->data[2*i];
      m1->data[k+1] = m->data[2*i+1];
    }
    return pure_complex_matrix(m1);
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix_int *m1 = create_int_matrix(n+k, n+k);
    const size_t k0 = k*m1->tda;
    memset(m1->data, 0, m1->block->size*sizeof(int));
    for (size_t i = 0; i < n; i++)
      m1->data[k0+i*(m1->tda+1)] = m->data[i];
    return pure_int_matrix(m1);
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_supdiagm(pure_expr *x, int32_t k)
{
  if (k<0) return matrix_subdiagm(x, -k);
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix_symbolic *m1 = create_symbolic_matrix(n+k, n+k);
    const size_t k0 = k;
    pure_expr *zero = pure_int(0);
    assert(zero);
    for (size_t i = 0; i < n+k; i++)
      for (size_t j = 0; j < n+k; j++)
	m1->data[i*m1->tda+j] = zero;
    for (size_t i = 0; i < n; i++)
      m1->data[k0+i*(m1->tda+1)] = m->data[i];
    return pure_symbolic_matrix(m1);
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix *m1 = create_double_matrix(n+k, n+k);
    const size_t k0 = k;
    memset(m1->data, 0, m1->block->size*sizeof(double));
    for (size_t i = 0; i < n; i++)
      m1->data[k0+i*(m1->tda+1)] = m->data[i];
    return pure_double_matrix(m1);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix_complex *m1 = create_complex_matrix(n+k, n+k);
    const size_t k0 = k;
    memset(m1->data, 0, m1->block->size*2*sizeof(double));
    for (size_t i = 0; i < n; i++) {
      const size_t k = 2*k0+2*i*(m1->tda+1);
      m1->data[k] = m->data[2*i];
      m1->data[k+1] = m->data[2*i+1];
    }
    return pure_complex_matrix(m1);
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    const size_t n1 = m->size1, n = m->size2;
    if (n1 != 1) return 0;
    gsl_matrix_int *m1 = create_int_matrix(n+k, n+k);
    const size_t k0 = k;
    memset(m1->data, 0, m1->block->size*sizeof(int));
    for (size_t i = 0; i < n; i++)
      m1->data[k0+i*(m1->tda+1)] = m->data[i];
    return pure_int_matrix(m1);
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_redim(pure_expr *x, int32_t n, int32_t m)
{
  void *p = 0;
  if (n<0 || m<0) return 0;
  const size_t n1 = (size_t)n, n2 = (size_t)m;
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    if (n1*n2!=m->size1*m->size2) return 0;
    if (m->tda == (m->size2>0?m->size2:1)) {
      // No copying necessary, just create a new view of this matrix.
      gsl_matrix_symbolic *m1 =
	(gsl_matrix_symbolic*)malloc(sizeof(gsl_matrix_symbolic));
      assert(m1);
      *m1 = *m; m1->q = 0;
      m1->size1 = n1; m1->tda = m1->size2 = n2;
      if (m1->tda == 0) m1->tda = 1;
      p = m1;
    } else {
      // Create a new matrix containing the same elements.
      pure_expr *y = pure_symbolic_matrix_dup(m);
      if (y) {
	gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)y->data.mat.p;
	m->size1 = n1; m->tda = m->size2 = n2;
	if (m->tda == 0) m->tda = 1;
      }
      return y;
    }
    break;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    if (n1*n2!=m->size1*m->size2) return 0;
    if (m->tda == (m->size2>0?m->size2:1)) {
      // No copying necessary, just create a new view of this matrix.
      gsl_matrix *m1 = (gsl_matrix*)malloc(sizeof(gsl_matrix));
      assert(m1);
      *m1 = *m;
      m1->size1 = n1; m1->tda = m1->size2 = n2;
      if (m1->tda == 0) m1->tda = 1;
      p = m1;
    } else {
      // Create a new matrix containing the same elements.
      pure_expr *y = pure_double_matrix_dup(m);
      if (y) {
	gsl_matrix *m = (gsl_matrix*)y->data.mat.p;
	m->size1 = n1; m->tda = m->size2 = n2;
	if (m->tda == 0) m->tda = 1;
      }
      return y;
    }
    break;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    if (n1*n2!=m->size1*m->size2) return 0;
    if (m->tda == (m->size2>0?m->size2:1)) {
      // No copying necessary, just create a new view of this matrix.
      gsl_matrix_complex *m1 =
	(gsl_matrix_complex*)malloc(sizeof(gsl_matrix_complex));
      assert(m1);
      *m1 = *m;
      m1->size1 = n1; m1->tda = m1->size2 = n2;
      if (m1->tda == 0) m1->tda = 1;
      p = m1;
    } else {
      // Create a new matrix containing the same elements.
      pure_expr *y = pure_complex_matrix_dup(m);
      if (y) {
	gsl_matrix_complex *m = (gsl_matrix_complex*)y->data.mat.p;
	m->size1 = n1; m->tda = m->size2 = n2;
	if (m->tda == 0) m->tda = 1;
      }
      return y;
    }
    break;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    if (n1*n2!=m->size1*m->size2) return 0;
    if (m->tda == (m->size2>0?m->size2:1)) {
      // No copying necessary, just create a new view of this matrix.
      gsl_matrix_int *m1 =
	(gsl_matrix_int*)malloc(sizeof(gsl_matrix_int));
      assert(m1);
      *m1 = *m;
      m1->size1 = n1; m1->tda = m1->size2 = n2;
      if (m1->tda == 0) m1->tda = 1;
      p = m1;
    } else {
      // Create a new matrix containing the same elements.
      pure_expr *y = pure_int_matrix_dup(m);
      if (y) {
	gsl_matrix_int *m = (gsl_matrix_int*)y->data.mat.p;
	m->size1 = n1; m->tda = m->size2 = n2;
	if (m->tda == 0) m->tda = 1;
      }
      return y;
    }
    break;
  }
  default:
    return 0;
  }
  pure_expr *y = new_expr();
  y->tag = x->tag;
  y->data.mat.p = p;
  y->data.mat.refc = x->data.mat.refc;
  (*y->data.mat.refc)++;
  MEMDEBUG_NEW(y)
  return y;
}

static pure_expr *matrix_rowsv(uint32_t n, pure_expr **xs)
{
  int k = -1;
  size_t nrows = 0, ncols = 0;
  int32_t target = 0;
  bool have_matrix = false;
  pure_expr *x = 0;
  for (size_t i = 0; i < n; i++) {
    x = xs[i];
    switch (x->tag) {
    case EXPR::MATRIX: {
      gsl_matrix_symbolic *mp = (gsl_matrix_symbolic*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  goto err;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::MATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::DBL:
      set_target_type(target, EXPR::DMATRIX);
      if (k >= 0 && k != 1) goto err;
      nrows++; k = 1;
      break;
    case EXPR::INT:
      set_target_type(target, EXPR::IMATRIX);
      if (k >= 0 && k != 1) goto err;
      nrows++; k = 1;
      break;
    case EXPR::APP: {
      double a, b;
      if (k >= 0 && k != 1) goto err;
      nrows++; k = 1;
      if (get_complex(x, a, b))
	set_target_type(target, EXPR::CMATRIX);
      else
	set_target_type(target, EXPR::MATRIX);
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *mp = (gsl_matrix*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  goto err;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::DMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *mp = (gsl_matrix_complex*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  goto err;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::CMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mp = (gsl_matrix_int*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size2 != (size_t)k)
	  goto err;
	nrows += mp->size1; k = mp->size2;
	set_target_type(target, EXPR::IMATRIX);
	have_matrix = true;
      }
      break;
    }
    default:
      if (k >= 0 && k != 1) goto err;
      nrows++; k = 1;
      set_target_type(target, EXPR::MATRIX);
      break;
    }
  }
  if (n == 1 && have_matrix) return xs[0];
  if (k < 0) k = 0;
  ncols = k;
  if (target == 0) target = EXPR::MATRIX;
  switch (target) {
  case EXPR::MATRIX:
    return symbolic_matrix_rows(nrows, ncols, n, xs);
  case EXPR::DMATRIX:
    return double_matrix_rows(nrows, ncols, n, xs);
  case EXPR::CMATRIX:
    return complex_matrix_rows(nrows, ncols, n, xs);
  case EXPR::IMATRIX:
    return int_matrix_rows(nrows, ncols, n, xs);
  default:
    assert(0 && "this can't happen");
    return 0;
  }
 err:
  pure_throw(bad_matrix_exception(x));
  return 0;
}

static pure_expr *matrix_columnsv(uint32_t n, pure_expr **xs)
{
  int k = -1;
  size_t nrows = 0, ncols = 0;
  int32_t target = 0;
  bool have_matrix = false;
  pure_expr *x = 0;
  for (size_t i = 0; i < n; i++) {
    x = xs[i];
    switch (x->tag) {
    case EXPR::MATRIX: {
      gsl_matrix_symbolic *mp = (gsl_matrix_symbolic*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  goto err;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::MATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::DBL:
      set_target_type(target, EXPR::DMATRIX);
      if (k >= 0 && k != 1) goto err;
      ncols++; k = 1;
      break;
    case EXPR::INT:
      set_target_type(target, EXPR::IMATRIX);
      if (k >= 0 && k != 1) goto err;
      ncols++; k = 1;
      break;
    case EXPR::APP: {
      double a, b;
      if (k >= 0 && k != 1) goto err;
      ncols++; k = 1;
      if (get_complex(x, a, b))
	set_target_type(target, EXPR::CMATRIX);
      else
	set_target_type(target, EXPR::MATRIX);
      break;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *mp = (gsl_matrix*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  goto err;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::DMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *mp = (gsl_matrix_complex*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  goto err;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::CMATRIX);
	have_matrix = true;
      }
      break;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *mp = (gsl_matrix_int*)x->data.mat.p;
      if (mp->size1 > 0 && mp->size2 > 0) {
	if (k >= 0 && mp->size1 != (size_t)k)
	  goto err;
	ncols += mp->size2; k = mp->size1;
	set_target_type(target, EXPR::IMATRIX);
	have_matrix = true;
      }
      break;
    }
    default:
      if (k >= 0 && k != 1) goto err;
      ncols++; k = 1;
      set_target_type(target, EXPR::MATRIX);
      break;
    }
  }
  if (n == 1 && have_matrix) return xs[0];
  if (k < 0) k = 0;
  nrows = k;
  if (target == 0) target = EXPR::MATRIX;
  switch (target) {
  case EXPR::MATRIX:
    return symbolic_matrix_columns(nrows, ncols, n, xs);
  case EXPR::DMATRIX:
    return double_matrix_columns(nrows, ncols, n, xs);
  case EXPR::CMATRIX:
    return complex_matrix_columns(nrows, ncols, n, xs);
  case EXPR::IMATRIX:
    return int_matrix_columns(nrows, ncols, n, xs);
  default:
    assert(0 && "this can't happen");
    return 0;
  }
 err:
  pure_throw(bad_matrix_exception(x));
  return 0;
}

extern "C"
pure_expr *matrix_matcat(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *mp = (gsl_matrix_symbolic*)x->data.mat.p;
    if (mp->size1 == 0 || mp->size2 == 0) {
      // empty output matrix
      gsl_matrix_symbolic *sm = create_symbolic_matrix(0, 0);
      return pure_symbolic_matrix(sm);
    }
    pure_expr **ys = (pure_expr**)malloc(mp->size1*sizeof(pure_expr*));
    if (!ys)
      return 0; // FIXME: should maybe throw an exception here
    for (size_t i = 0; i < mp->size1; i++) {
      pure_expr **xs = mp->data+i*mp->tda;
      if (!(ys[i] = matrix_columnsv(mp->size2, xs))) {
	for (size_t j = 0; j < i; j++)
	  pure_freenew(ys[j]);
	free(ys);
	return 0;
      }
    }
    pure_expr *y = matrix_rowsv(mp->size1, ys);
    free(ys);
    return y;
  }
  case EXPR::DMATRIX:
  case EXPR::CMATRIX:
  case EXPR::IMATRIX:
    return x;
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_rows(pure_expr *xs)
{
  size_t n;
  pure_expr **elems;
  if (pure_is_listv(xs, &n, &elems)) {
    pure_expr *ret = matrix_rowsv(n, elems);
    if (elems) free(elems);
    return ret;
  } else
    return 0;
}

extern "C"
pure_expr *matrix_columns(pure_expr *xs)
{
  size_t n;
  pure_expr **elems;
  if (pure_is_listv(xs, &n, &elems)) {
    pure_expr *ret = matrix_columnsv(n, elems);
    if (elems) free(elems);
    return ret;
  } else
    return 0;
}

extern "C"
pure_expr *matrix_transpose(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m1 = (gsl_matrix_symbolic*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_symbolic *m2 = create_symbolic_matrix(m, n);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	m2->data[j*m2->tda+i] = m1->data[i*m1->tda+j];
    return pure_symbolic_matrix(m2);
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix *m2 = create_double_matrix(m, n);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	m2->data[j*m2->tda+i] = m1->data[i*m1->tda+j];
    return pure_double_matrix(m2);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_complex *m2 = create_complex_matrix(m, n);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t k = 2*(i*m1->tda+j), l = 2*(j*m2->tda+i);
	m2->data[l] = m1->data[k];
	m2->data[l+1] = m1->data[k+1];
      }
    return pure_complex_matrix(m2);
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_int *m2 = create_int_matrix(m, n);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	m2->data[j*m2->tda+i] = m1->data[i*m1->tda+j];
    return pure_int_matrix(m2);
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_double(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m1 = (gsl_matrix_symbolic*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	switch (m1->data[i*m1->tda+j]->tag) {
	case EXPR::DBL:
	case EXPR::INT:
	case EXPR::BIGINT:
	  break;
	default:
	  return 0;
	}
    gsl_matrix *m2 = create_double_matrix(n, m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	switch (m1->data[i*m1->tda+j]->tag) {
	case EXPR::DBL:
	  m2->data[i*m2->tda+j] = m1->data[i*m1->tda+j]->data.d;
	  break;
	case EXPR::INT:
	  m2->data[i*m2->tda+j] = (double)m1->data[i*m1->tda+j]->data.i;
	  break;
	case EXPR::BIGINT:
	  m2->data[i*m2->tda+j] = mpz_get_d(m1->data[i*m1->tda+j]->data.z);
	  break;
	default:
	  return 0; // can't happen
	}
    return pure_double_matrix(m2);
  }
  case EXPR::DMATRIX:
    return x;
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix *m2 = create_double_matrix(n, m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	m2->data[i*m2->tda+j] = (double)m1->data[i*m1->tda+j];
    return pure_double_matrix(m2);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix *m2 = create_double_matrix(n, 2*m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t k = i*m2->tda+2*j;
	size_t l = 2*(i*m1->tda+j);
	m2->data[k] = m1->data[l];
	m2->data[k+1] = m1->data[l+1];
      }
    return pure_double_matrix(m2);
  }
  default: {
    size_t n;
    pure_expr **xs;
    if (pure_is_listv(x, &n, &xs)) {
      for (size_t i = 0; i < n; i++)
	if (xs[i]->tag != EXPR::DBL) {
	  free(xs);
	  return 0;
	}
      gsl_matrix *m2 = create_double_matrix(1, n);
      for (size_t i = 0; i < n; i++)
	m2->data[i] = xs[i]->data.d;
      if (xs) free(xs);
      return pure_double_matrix(m2);
    } else
      return 0;
  }
  }
}

extern "C"
pure_expr *matrix_complex(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m1 = (gsl_matrix_symbolic*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	switch (m1->data[i*m1->tda+j]->tag) {
	case EXPR::DBL:
	case EXPR::INT:
	case EXPR::BIGINT:
	  break;
	default:
	  if (!is_complex(m1->data[i*m1->tda+j]))
	    return 0;
	  else
	    break;
	}
    gsl_matrix_complex *m2 = create_complex_matrix(n, m);
    double a = 0.0, b = 0.0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t k = 2*(i*m2->tda+j);
	switch (m1->data[i*m1->tda+j]->tag) {
	case EXPR::DBL:
	  m2->data[k] = m1->data[i*m1->tda+j]->data.d;
	  m2->data[k+1] = 0.0;
	  break;
	case EXPR::INT:
	  m2->data[k] = (double)m1->data[i*m1->tda+j]->data.i;
	  m2->data[k+1] = 0.0;
	  break;
	case EXPR::BIGINT:
	  m2->data[k] = mpz_get_d(m1->data[i*m1->tda+j]->data.z);
	  m2->data[k+1] = 0.0;
	  break;
	default:
	  get_complex(m1->data[i*m1->tda+j], a, b);
	  m2->data[k] = a;
	  m2->data[k+1] = b;
	}
      }
    return pure_complex_matrix(m2);
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_complex *m2 = create_complex_matrix(n, m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t k = 2*(i*m2->tda+j);
	m2->data[k] = m1->data[i*m1->tda+j];
	m2->data[k+1] = 0.0;
      }
    return pure_complex_matrix(m2);
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_complex *m2 = create_complex_matrix(n, m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t k = 2*(i*m2->tda+j);
	m2->data[k] = (double)m1->data[i*m1->tda+j];
	m2->data[k+1] = 0.0;
      }
    return pure_complex_matrix(m2);
  }
  case EXPR::CMATRIX:
    return x;
  default: {
    size_t n;
    pure_expr **xs;
    if (pure_is_listv(x, &n, &xs)) {
      for (size_t i = 0; i < n; i++)
	if (!is_complex(xs[i])) {
	  free(xs);
	  return 0;
	}
      gsl_matrix_complex *m2 = create_complex_matrix(1, n);
      double a = 0.0, b = 0.0;
      for (size_t i = 0; i < n; i++) {
	size_t k = 2*i;
	get_complex(xs[i], a, b);
	m2->data[k] = a;
	m2->data[k+1] = b;
      }
      if (xs) free(xs);
      return pure_complex_matrix(m2);
    } else
      return 0;
  }
  }
}

extern "C"
pure_expr *matrix_int(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m1 = (gsl_matrix_symbolic*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	switch (m1->data[i*m1->tda+j]->tag) {
	case EXPR::DBL:
	case EXPR::INT:
	case EXPR::BIGINT:
	  break;
	default:
	  return 0;
	}
    gsl_matrix_int *m2 = create_int_matrix(n, m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	switch (m1->data[i*m1->tda+j]->tag) {
	case EXPR::DBL:
	  m2->data[i*m2->tda+j] = (int32_t)m1->data[i*m1->tda+j]->data.d;
	  break;
	case EXPR::INT:
	  m2->data[i*m2->tda+j] = m1->data[i*m1->tda+j]->data.i;
	  break;
	case EXPR::BIGINT:
	  m2->data[i*m2->tda+j] = pure_get_int(m1->data[i*m1->tda+j]);
	  break;
	default:
	  return 0; // can't happen
	}
    return pure_int_matrix(m2);
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_int *m2 = create_int_matrix(n, m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	m2->data[i*m2->tda+j] = (int)m1->data[i*m1->tda+j];
    return pure_int_matrix(m2);
  }
  case EXPR::IMATRIX:
    return x;
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_int *m2 = create_int_matrix(n, 2*m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t k = i*m2->tda+2*j;
	size_t l = 2*(i*m1->tda+j);
	m2->data[k] = (int)m1->data[l];
	m2->data[k+1] = (int)m1->data[l+1];
      }
    return pure_int_matrix(m2);
  }
  default: {
    size_t n;
    pure_expr **xs;
    if (pure_is_listv(x, &n, &xs)) {
      for (size_t i = 0; i < n; i++)
	if (xs[i]->tag != EXPR::INT) {
	  free(xs);
	  return 0;
	}
      gsl_matrix_int *m2 = create_int_matrix(1, n);
      for (size_t i = 0; i < n; i++)
	m2->data[i] = xs[i]->data.i;
      if (xs) free(xs);
      return pure_int_matrix(m2);
    } else
      return 0;
  }
  }
}

extern "C"
pure_expr *matrix_symbolic(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::MATRIX:
    return x;
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_symbolic *m2 = create_symbolic_matrix(n, m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	m2->data[i*m2->tda+j] = pure_double(m1->data[i*m1->tda+j]);
    return pure_symbolic_matrix(m2);
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_symbolic *m2 = create_symbolic_matrix(n, m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	m2->data[i*m2->tda+j] = pure_int(m1->data[i*m1->tda+j]);
    return pure_symbolic_matrix(m2);
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_symbolic *m2 = create_symbolic_matrix(n, m);
    interpreter& interp = *interpreter::g_interp;
    symbol &rect = interp.symtab.complex_rect_sym();
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t l = 2*(i*m1->tda+j);
	m2->data[i*m2->tda+j] = make_complex2(rect, m1->data[l], m1->data[l+1]);
      }
    return pure_symbolic_matrix(m2);
  }
  default: {
    size_t n;
    pure_expr **xs;
    if (pure_is_listv(x, &n, &xs)) {
      gsl_matrix_symbolic *m2 = create_symbolic_matrix(1, n);
      if (xs) {
	memcpy(m2->data, xs, n*sizeof(pure_expr*));
	free(xs);
      }
      return pure_symbolic_matrix(m2);
    } else
      return 0;
  }
  }
}

extern "C"
pure_expr *matrix_re(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix *m2 = create_double_matrix(n, m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t k = 2*(i*m1->tda+j), l = i*m2->tda+j;
	m2->data[l] = m1->data[k];
      }
    return pure_double_matrix(m2);
  }
  case EXPR::DMATRIX:
  case EXPR::IMATRIX:
    return x;
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_im(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix *m2 = create_double_matrix(n, m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t k = 2*(i*m1->tda+j), l = i*m2->tda+j;
	m2->data[l] = m1->data[k+1];
      }
    return pure_double_matrix(m2);
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix *m2 = create_double_matrix(n, m);
    memset(m2->data, 0, n*m*sizeof(double));
    return pure_double_matrix(m2);
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_int *m2 = create_int_matrix(n, m);
    memset(m2->data, 0, n*m*sizeof(int));
    return pure_int_matrix(m2);
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_conj(pure_expr *x)
{
  switch (x->tag) {
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    gsl_matrix_complex *m2 = create_complex_matrix(n, m);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t k = 2*(i*m1->tda+j), l = 2*(i*m2->tda+j);
	m2->data[l] = m1->data[k];
	m2->data[l+1] = -m1->data[k+1];
      }
    return pure_complex_matrix(m2);
  }
  case EXPR::DMATRIX:
  case EXPR::IMATRIX:
    return x;
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_from_double_array_nodup(uint32_t n1, uint32_t n2, void *p)
{
  if (n1 == 0 || n2 == 0) // empty matrix
    return pure_double_matrix(create_double_matrix(n1, n2));
  if (!p) return 0;
  gsl_matrix_view v = gsl_matrix_view_array((double*)p, n1, n2);
  // take a copy of the view matrix
  gsl_matrix *m = (gsl_matrix*)malloc(sizeof(gsl_matrix));
  assert(m && v.matrix.data);
  *m = v.matrix;
  pure_expr *x = new_expr();
  x->tag = EXPR::DMATRIX;
  x->data.mat.p = m;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *matrix_from_complex_array_nodup(uint32_t n1, uint32_t n2, void *p)
{
  if (n1 == 0 || n2 == 0) // empty matrix
    return pure_complex_matrix(create_complex_matrix(n1, n2));
  if (!p) return 0;
  gsl_matrix_complex_view v =
    gsl_matrix_complex_view_array((double*)p, n1, n2);
  // take a copy of the view matrix
  gsl_matrix_complex *m =
    (gsl_matrix_complex*)malloc(sizeof(gsl_matrix_complex));
  assert(m && v.matrix.data);
  *m = v.matrix;
  pure_expr *x = new_expr();
  x->tag = EXPR::CMATRIX;
  x->data.mat.p = m;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *matrix_from_int_array_nodup(uint32_t n1, uint32_t n2, void *p)
{
  if (n1 == 0 || n2 == 0) // empty matrix
    return pure_int_matrix(create_int_matrix(n1, n2));
  if (!p) return 0;
  gsl_matrix_int_view v = gsl_matrix_int_view_array((int*)p, n1, n2);
  // take a copy of the view matrix
  gsl_matrix_int *m = (gsl_matrix_int*)malloc(sizeof(gsl_matrix_int));
  assert(m && v.matrix.data);
  *m = v.matrix;
  pure_expr *x = new_expr();
  x->tag = EXPR::IMATRIX;
  x->data.mat.p = m;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *matrix_from_double_array(uint32_t n1, uint32_t n2, void *p)
{
  if (n1 == 0 || n2 == 0) // empty matrix
    return pure_double_matrix(create_double_matrix(n1, n2));
  if (!p)
    p = calloc(n1*n2, sizeof(double));
  else {
    void *q = malloc(n1*n2*sizeof(double));
    memcpy(q, p, n1*n2*sizeof(double));
    p = q;
  }
  if (!p) return 0;
  gsl_matrix_view v = gsl_matrix_view_array((double*)p, n1, n2);
  // take a copy of the view matrix
  gsl_matrix *m = (gsl_matrix*)malloc(sizeof(gsl_matrix));
  gsl_block *b = (gsl_block*)malloc(sizeof(gsl_block));
  assert(m && b && v.matrix.data);
  *m = v.matrix;
  b->size = n1*n2;
  b->data = m->data;
  m->block = b;
  pure_expr *x = new_expr();
  x->tag = EXPR::DMATRIX;
  x->data.mat.p = m;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *matrix_from_complex_array(uint32_t n1, uint32_t n2, void *p)
{
  if (n1 == 0 || n2 == 0) // empty matrix
    return pure_complex_matrix(create_complex_matrix(n1, n2));
  if (!p)
    p = calloc(2*n1*n2, sizeof(double));
  else {
    void *q = malloc(2*n1*n2*sizeof(double));
    memcpy(q, p, 2*n1*n2*sizeof(double));
    p = q;
  }
  if (!p) return 0;
  gsl_matrix_complex_view v =
    gsl_matrix_complex_view_array((double*)p, n1, n2);
  // take a copy of the view matrix
  gsl_matrix_complex *m =
    (gsl_matrix_complex*)malloc(sizeof(gsl_matrix_complex));
  gsl_block_complex *b = (gsl_block_complex*)malloc(sizeof(gsl_block_complex));
  assert(m && b && v.matrix.data);
  *m = v.matrix;
  b->size = n1*n2;
  b->data = m->data;
  m->block = b;
  pure_expr *x = new_expr();
  x->tag = EXPR::CMATRIX;
  x->data.mat.p = m;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *matrix_from_int_array(uint32_t n1, uint32_t n2, void *p)
{
  if (n1 == 0 || n2 == 0) // empty matrix
    return pure_int_matrix(create_int_matrix(n1, n2));
  if (!p)
    p = calloc(n1*n2, sizeof(int));
  else {
    void *q = malloc(n1*n2*sizeof(int));
    memcpy(q, p, n1*n2*sizeof(int));
    p = q;
  }
  if (!p) return 0;
  gsl_matrix_int_view v = gsl_matrix_int_view_array((int*)p, n1, n2);
  // take a copy of the view matrix
  gsl_matrix_int *m = (gsl_matrix_int*)malloc(sizeof(gsl_matrix_int));
  gsl_block_int *b = (gsl_block_int*)malloc(sizeof(gsl_block_int));
  assert(m && b && v.matrix.data);
  *m = v.matrix;
  b->size = n1*n2;
  b->data = m->data;
  m->block = b;
  pure_expr *x = new_expr();
  x->tag = EXPR::IMATRIX;
  x->data.mat.p = m;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
void *matrix_to_double_array(void *p, pure_expr *x)
{
  switch (x->tag) {
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(double));
    if (!p) return 0;
    double *q = (double*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t l = 2*(i*m1->tda+j);
	q[k++] = m1->data[l];
	q[k++] = m1->data[l+1];
      }
    return p;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(double));
    if (!p) return 0;
    double *q = (double*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = m1->data[i*m1->tda+j];
    return p;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(double));
    if (!p) return 0;
    double *q = (double*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = (double)m1->data[i*m1->tda+j];
    return p;
  }
  default:
    return 0;
  }
}

extern "C"
void *matrix_to_complex_array(void *p, pure_expr *x)
{
  switch (x->tag) {
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(double));
    if (!p) return 0;
    double *q = (double*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t l = 2*(i*m1->tda+j);
	q[k++] = m1->data[l];
	q[k++] = m1->data[l+1];
      }
    return p;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(double));
    if (!p) return 0;
    double *q = (double*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	q[k++] = m1->data[i*m1->tda+j];
	q[k++] = 0.0;
      }
    return p;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(double));
    if (!p) return 0;
    double *q = (double*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	q[k++] = (double)m1->data[i*m1->tda+j];
	q[k++] = 0.0;
      }
    return p;
  }
  default:
    return 0;
  }
}

extern "C"
void *matrix_to_int_array(void *p, pure_expr *x)
{
  switch (x->tag) {
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(int));
    if (!p) return 0;
    int *q = (int*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t l = 2*(i*m1->tda+j);
	q[k++] = (int)m1->data[l];
	q[k++] = (int)m1->data[l+1];
      }
    return p;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(int));
    if (!p) return 0;
    int *q = (int*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = (int)m1->data[i*m1->tda+j];
    return p;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(int));
    if (!p) return 0;
    int *q = (int*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = m1->data[i*m1->tda+j];
    return p;
  }
  default:
    return 0;
  }
}

extern "C"
pure_expr *matrix_from_float_array(uint32_t n1, uint32_t n2, void *p)
{
  if (n1 == 0 || n2 == 0) // empty matrix
    return pure_double_matrix(create_double_matrix(n1, n2));
  if (!p)
    p = calloc(n1*n2, sizeof(double));
  else {
    void *q = malloc(n1*n2*sizeof(double));
    float *p1 = (float*)p; double *q1 = (double*)q;
    for (size_t i = 0; i < n1*n2; i++) q1[i] = (double)p1[i];
    p = q;
  }
  if (!p) return 0;
  gsl_matrix_view v = gsl_matrix_view_array((double*)p, n1, n2);
  // take a copy of the view matrix
  gsl_matrix *m = (gsl_matrix*)malloc(sizeof(gsl_matrix));
  gsl_block *b = (gsl_block*)malloc(sizeof(gsl_block));
  assert(m && b && v.matrix.data);
  *m = v.matrix;
  b->size = n1*n2;
  b->data = m->data;
  m->block = b;
  pure_expr *x = new_expr();
  x->tag = EXPR::DMATRIX;
  x->data.mat.p = m;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *matrix_from_complex_float_array(uint32_t n1, uint32_t n2, void *p)
{
  if (n1 == 0 || n2 == 0) // empty matrix
    return pure_complex_matrix(create_complex_matrix(n1, n2));
  if (!p)
    p = calloc(2*n1*n2, sizeof(double));
  else {
    void *q = malloc(2*n1*n2*sizeof(double));
    float *p1 = (float*)p; double *q1 = (double*)q;
    for (size_t i = 0; i < 2*n1*n2; i++) q1[i] = (double)p1[i];
    p = q;
  }
  if (!p) return 0;
  gsl_matrix_complex_view v =
    gsl_matrix_complex_view_array((double*)p, n1, n2);
  // take a copy of the view matrix
  gsl_matrix_complex *m =
    (gsl_matrix_complex*)malloc(sizeof(gsl_matrix_complex));
  gsl_block_complex *b = (gsl_block_complex*)malloc(sizeof(gsl_block_complex));
  assert(m && b && v.matrix.data);
  *m = v.matrix;
  b->size = n1*n2;
  b->data = m->data;
  m->block = b;
  pure_expr *x = new_expr();
  x->tag = EXPR::CMATRIX;
  x->data.mat.p = m;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *matrix_from_int64_array(uint32_t n1, uint32_t n2, void *p)
{
  gsl_matrix_symbolic *mat = create_symbolic_matrix(n1, n2);
  if (!mat) return 0;
  if (n1 == 0 || n2 == 0) // empty matrix
    return pure_symbolic_matrix(mat);
  pure_expr **data = mat->data;
  size_t tda = mat->tda, k = 0;
  const int64_t *q = (int64_t*)p;
  if (q) {
    // convert int64 values to bigints
    for (size_t i = 0; i < n1; i++)
      for (size_t j = 0; j < n2; j++)
	data[i*tda+j] = pure_int64(q[k++]);
  } else {
    // create a zero matrix
    pure_expr *x = pure_int64(0);
    for (size_t i = 0; i < n1; i++)
      for (size_t j = 0; j < n2; j++)
	data[i*tda+j] = x;
  }
  return pure_symbolic_matrix(mat);
}

extern "C"
pure_expr *matrix_from_short_array(uint32_t n1, uint32_t n2, void *p)
{
  if (n1 == 0 || n2 == 0) // empty matrix
    return pure_int_matrix(create_int_matrix(n1, n2));
  if (!p)
    p = calloc(n1*n2, sizeof(int));
  else {
    void *q = malloc(n1*n2*sizeof(int));
    short *p1 = (short*)p; int *q1 = (int*)q;
    for (size_t i = 0; i < n1*n2; i++) q1[i] = (int)p1[i];
    p = q;
  }
  if (!p) return 0;
  gsl_matrix_int_view v = gsl_matrix_int_view_array((int*)p, n1, n2);
  // take a copy of the view matrix
  gsl_matrix_int *m = (gsl_matrix_int*)malloc(sizeof(gsl_matrix_int));
  gsl_block_int *b = (gsl_block_int*)malloc(sizeof(gsl_block_int));
  assert(m && b && v.matrix.data);
  *m = v.matrix;
  b->size = n1*n2;
  b->data = m->data;
  m->block = b;
  pure_expr *x = new_expr();
  x->tag = EXPR::IMATRIX;
  x->data.mat.p = m;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
pure_expr *matrix_from_byte_array(uint32_t n1, uint32_t n2, void *p)
{
  if (n1 == 0 || n2 == 0) // empty matrix
    return pure_int_matrix(create_int_matrix(n1, n2));
  if (!p)
    p = calloc(n1*n2, sizeof(int));
  else {
    void *q = malloc(n1*n2*sizeof(int));
    int8_t *p1 = (int8_t*)p; int *q1 = (int*)q;
    for (size_t i = 0; i < n1*n2; i++) q1[i] = (int)p1[i];
    p = q;
  }
  if (!p) return 0;
  gsl_matrix_int_view v = gsl_matrix_int_view_array((int*)p, n1, n2);
  // take a copy of the view matrix
  gsl_matrix_int *m = (gsl_matrix_int*)malloc(sizeof(gsl_matrix_int));
  gsl_block_int *b = (gsl_block_int*)malloc(sizeof(gsl_block_int));
  assert(m && b && v.matrix.data);
  *m = v.matrix;
  b->size = n1*n2;
  b->data = m->data;
  m->block = b;
  pure_expr *x = new_expr();
  x->tag = EXPR::IMATRIX;
  x->data.mat.p = m;
  x->data.mat.refc = new uint32_t;
  *x->data.mat.refc = 1;
  MEMDEBUG_NEW(x)
  return x;
}

extern "C"
void *matrix_to_float_array(void *p, pure_expr *x)
{
  switch (x->tag) {
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(float));
    if (!p) return 0;
    float *q = (float*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t l = 2*(i*m1->tda+j);
	q[k++] = (float)m1->data[l];
	q[k++] = (float)m1->data[l+1];
      }
    return p;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(float));
    if (!p) return 0;
    float *q = (float*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = (float)m1->data[i*m1->tda+j];
    return p;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(float));
    if (!p) return 0;
    float *q = (float*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = (float)m1->data[i*m1->tda+j];
    return p;
  }
  default:
    return 0;
  }
}

extern "C"
void *matrix_to_complex_float_array(void *p, pure_expr *x)
{
  switch (x->tag) {
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(float));
    if (!p) return 0;
    float *q = (float*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t l = 2*(i*m1->tda+j);
	q[k++] = (float)m1->data[l];
	q[k++] = (float)m1->data[l+1];
      }
    return p;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(float));
    if (!p) return 0;
    float *q = (float*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	q[k++] = (float)m1->data[i*m1->tda+j];
	q[k++] = 0.0;
      }
    return p;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(float));
    if (!p) return 0;
    float *q = (float*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	q[k++] = (float)m1->data[i*m1->tda+j];
	q[k++] = 0.0;
      }
    return p;
  }
  default:
    return 0;
  }
}

extern "C"
void *matrix_to_int64_array(void *p, pure_expr *x)
{
  switch (x->tag) {
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(int64_t));
    if (!p) return 0;
    int64_t *q = (int64_t*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t l = 2*(i*m1->tda+j);
	q[k++] = (int64_t)m1->data[l];
	q[k++] = (int64_t)m1->data[l+1];
      }
    return p;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(int64_t));
    if (!p) return 0;
    int64_t *q = (int64_t*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = (int64_t)m1->data[i*m1->tda+j];
    return p;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(int64_t));
    if (!p) return 0;
    int64_t *q = (int64_t*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = (int64_t)m1->data[i*m1->tda+j];
    return p;
  }
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m1 = (gsl_matrix_symbolic*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(int64_t));
    if (!p) return 0;
    int64_t *q = (int64_t*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	pure_expr *x = m1->data[i*m1->tda+j];
	if (x->tag == EXPR::BIGINT)
	  q[k++] = pure_get_int64(x);
	else
	  q[k++] = 0;
      }
    return p;
  }
  default:
    return 0;
  }
}

extern "C"
void *matrix_to_short_array(void *p, pure_expr *x)
{
  switch (x->tag) {
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(short));
    if (!p) return 0;
    short *q = (short*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t l = 2*(i*m1->tda+j);
	q[k++] = (short)m1->data[l];
	q[k++] = (short)m1->data[l+1];
      }
    return p;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(short));
    if (!p) return 0;
    short *q = (short*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = (short)m1->data[i*m1->tda+j];
    return p;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(short));
    if (!p) return 0;
    short *q = (short*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = (short)m1->data[i*m1->tda+j];
    return p;
  }
  default:
    return 0;
  }
}

extern "C"
void *matrix_to_byte_array(void *p, pure_expr *x)
{
  switch (x->tag) {
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(2*n*m*sizeof(int8_t));
    if (!p) return 0;
    int8_t *q = (int8_t*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++) {
	size_t l = 2*(i*m1->tda+j);
	q[k++] = (int8_t)m1->data[l];
	q[k++] = (int8_t)m1->data[l+1];
      }
    return p;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(int8_t));
    if (!p) return 0;
    int8_t *q = (int8_t*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = (int8_t)m1->data[i*m1->tda+j];
    return p;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
    size_t n = m1->size1, m = m1->size2;
    if (n==0 || m==0) return p;
    if (!p) p = malloc(n*m*sizeof(int8_t));
    if (!p) return 0;
    int8_t *q = (int8_t*)p;
    size_t k = 0;
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < m; j++)
	q[k++] = (int8_t)m1->data[i*m1->tda+j];
    return p;
  }
  default:
    return 0;
  }
}

static uint32_t mpz_hash(const mpz_t z)
{
  uint32_t h = 0;
  int i, len = z->_mp_size;
  if (len < 0) len = -len;
  if (sizeof(mp_limb_t) == 8) {
    for (i=0; i<len; i++) {
      h ^= (uint32_t)(uint64_t)z->_mp_d[i];
      h ^= (uint32_t)(((uint64_t)z->_mp_d[i])>>32);
    }
  } else {
    for (i=0; i<len; i++)
      h ^= z->_mp_d[i];
  }
  if (z->_mp_size < 0)
    h = -h;
  return h;
}

static uint32_t double_hash(double d)
{
  uint32_t h;
  unsigned char *c;
  size_t i;
  c = (unsigned char*)&d;
#if WORDS_BIGENDIAN
  for (h=0, i=sizeof(double); i-->0; )
#else
  for (h=0, i=0; i<sizeof(double); i++)
#endif
  {
    h = (h * 971) ^ c[i];
  }
  return h;
}

static uint32_t string_hash(const char *s)
{
  uint32_t h = 0, g;
  while (*s) {
    h = (h<<4)+*(s++);
    if ((g = (h & 0xf0000000)))	{
      h = h^(g>>24);
      h = h^g;
    }
  }
  return h;
}

extern "C"
uint32_t hash(pure_expr *x)
{
  char test;
  if (is_thunk(x)) pure_force(x);
  switch (x->tag) {
  case EXPR::INT:
    return (uint32_t)x->data.i;
  case EXPR::BIGINT:
    return mpz_hash(x->data.z);
  case EXPR::DBL:
    return double_hash(x->data.d);
  case EXPR::STR:
    return string_hash(x->data.s);
  case EXPR::PTR: {
    if (x->data.p) {
      int tag = pure_get_tag(x);
      pure_equal_fun eq = pure_pointer_equal(tag);
      if (eq) {
	// Use a custom hash function if it is available.
	pure_hash_fun hash = pure_pointer_hash(tag);
	if (hash) return hash(x->data.p);
	// Or fall back to hashing the print representation if it is available.
	pure_printer_fun printer = pure_pointer_printer(tag);
	const char *s;
	if (printer && (s = printer(x->data.p))) return string_hash(s);
      }
    }
    // Standard generic pointer hash.
#if SIZEOF_VOID_P==8
    return ((uint32_t)(uint64_t)x->data.p) ^ ((uint32_t)(((uint64_t)x->data.p)>>32));
#else
    return (uint32_t)x->data.p;
#endif
  }
  case EXPR::APP: {
    checkstk(test);
    int h;
    h = ::hash(x->data.x[0]);
    h = (h<<1) | (h<0 ? 1 : 0);
    h ^= ::hash(x->data.x[1]);
    return (uint32_t)h;
  }
  case EXPR::MATRIX: {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    const size_t tda = m->tda;
    checkstk(test);
    int h = EXPR::MATRIX;
    for (size_t i = 0; i < m->size1; i++)
      for (size_t j = 0; j < m->size2; j++) {
	h = (h<<1) | (h<0 ? 1 : 0);
	h ^= ::hash(m->data[i*tda+j]);
      }
    return (uint32_t)h;
  }
  case EXPR::DMATRIX: {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    const size_t tda = m->tda;
    int h = EXPR::DMATRIX;
    for (size_t i = 0; i < m->size1; i++)
      for (size_t j = 0; j < m->size2; j++) {
	h = (h<<1) | (h<0 ? 1 : 0);
	h ^= double_hash(m->data[i*tda+j]);
      }
    return (uint32_t)h;
  }
  case EXPR::CMATRIX: {
    gsl_matrix_complex *m = (gsl_matrix_complex*)x->data.mat.p;
    const size_t tda = m->tda;
    int h = EXPR::CMATRIX;
    for (size_t i = 0; i < m->size1; i++)
      for (size_t j = 0; j < m->size2; j++) {
	const size_t k = 2*(i*tda+j);
	h = (h<<1) | (h<0 ? 1 : 0);
	h ^= double_hash(m->data[k]);
	h = (h<<1) | (h<0 ? 1 : 0);
	h ^= double_hash(m->data[k+1]);
      }
    return (uint32_t)h;
  }
  case EXPR::IMATRIX: {
    gsl_matrix_int *m = (gsl_matrix_int*)x->data.mat.p;
    const size_t tda = m->tda;
    int h = EXPR::IMATRIX;
    for (size_t i = 0; i < m->size1; i++)
      for (size_t j = 0; j < m->size2; j++) {
	h = (h<<1) | (h<0 ? 1 : 0);
	h ^= (int)m->data[i*tda+j];
      }
    return (uint32_t)h;
  }
  default:
    assert(x->tag>=0);
    if (x->data.clos && x->data.clos->local) {
      int h = x->tag ^ x->data.clos->key;
      for (uint32_t i = 0; i < x->data.clos->m; i++) {
	h = (h<<1) | (h<0 ? 1 : 0);
	h ^= ::hash(x->data.clos->env[i]);
      }
      return (uint32_t)h;
    } else
      return (uint32_t)x->tag;
  }
}

static inline bool same_env(pure_closure *clos1, pure_closure *clos2)
{
  if (clos1->m != clos2->m)
    return false;
  for (uint32_t i = 0; i < clos1->m; i++)
    if (!same(clos1->env[i], clos2->env[i]))
      return false;
  return true;
}

extern "C"
bool same(pure_expr *x, pure_expr *y)
{
  char test;
 tail:
  if (x == y)
    return 1;
  if (is_thunk(x)) pure_force(x);
  if (is_thunk(y)) pure_force(y);
  if (x->tag != y->tag)
    return 0;
  else if (x->tag >= 0 && y->tag >= 0)
    if (x->data.clos && y->data.clos)
      /* We're comparing two closures. Unfortunately, just comparing the
	 function pointers doesn't work reliably in the JIT-hosted
	 environment, since the JIT creates stubs which later get replaced
	 with the real code when a function is first executed. We solve this
	 by having a unique key assigned to at least all local closures. */
      if (!x->data.clos->local || !y->data.clos->local)
	/* If one of the closures is a global, they can only be equal if the
	   other is a global, too, and in this case we already know that they
	   must be identical since their function symbols match up. */
	return x->data.clos->local == y->data.clos->local;
      else
	/* Otherwise we have two local closures, in which case we just need to
	   compare their keys and environments. */
	return x->data.clos->key == y->data.clos->key &&
	  same_env(x->data.clos, y->data.clos);
    else
      /* This will assert two function symbols to be equal only if they both
	 have closures attached to them, and will thus fail if one of the
	 symbols is quoted. */
      return x->data.clos == y->data.clos;
  else {
    switch (x->tag) {
    case EXPR::APP: {
      checkstk(test);
      if (!same(x->data.x[0], y->data.x[0]))
	return 0;
      /* Fake a tail call here, so that we do not run out of stack space when
	 comparing large lists or similar right-recursive structures. */
      x = x->data.x[1]; y = y->data.x[1];
      goto tail;
    }
    case EXPR::INT:
      return x->data.i == y->data.i;
    case EXPR::BIGINT:
      return mpz_cmp(x->data.z, y->data.z) == 0;
    case EXPR::DBL:
      return x->data.d == y->data.d || (is_nan(x->data.d) && is_nan(y->data.d));
    case EXPR::STR:
      return strcmp(x->data.s, y->data.s) == 0;
    case EXPR::PTR: {
      int tagx = pure_get_tag(x), tagy = pure_get_tag(y);
      if (tagx == tagy && x->data.p && y->data.p) {
	pure_equal_fun eq = pure_pointer_equal(tagx);
	if (eq) return eq(x->data.p, y->data.p);
      }
      return x->data.p == y->data.p;
    }
    case EXPR::MATRIX: {
      gsl_matrix_symbolic *m1 = (gsl_matrix_symbolic*)x->data.mat.p;
      gsl_matrix_symbolic *m2 = (gsl_matrix_symbolic*)y->data.mat.p;
      const size_t tda1 = m1->tda, tda2 = m2->tda;
      if (m1->size1 != m2->size1 || m1->size2 != m2->size2)
	return 0;
      checkstk(test);
      for (size_t i = 0; i < m1->size1; i++)
	for (size_t j = 0; j < m1->size2; j++)
	  if (!same(m1->data[i*tda1+j], m2->data[i*tda2+j]))
	    return 0;
      return 1;
    }
    case EXPR::DMATRIX: {
      gsl_matrix *m1 = (gsl_matrix*)x->data.mat.p;
      gsl_matrix *m2 = (gsl_matrix*)y->data.mat.p;
      const size_t tda1 = m1->tda, tda2 = m2->tda;
      if (m1->size1 != m2->size1 || m1->size2 != m2->size2)
	return 0;
      for (size_t i = 0; i < m1->size1; i++)
	for (size_t j = 0; j < m1->size2; j++)
	  if (m1->data[i*tda1+j] != m2->data[i*tda2+j])
	    return 0;
      return 1;
    }
    case EXPR::CMATRIX: {
      gsl_matrix_complex *m1 = (gsl_matrix_complex*)x->data.mat.p;
      gsl_matrix_complex *m2 = (gsl_matrix_complex*)y->data.mat.p;
      const size_t tda1 = m1->tda, tda2 = m2->tda;
      if (m1->size1 != m2->size1 || m1->size2 != m2->size2)
	return 0;
      for (size_t i = 0; i < m1->size1; i++)
	for (size_t j = 0; j < m1->size2; j++) {
	  const size_t k1 = 2*(i*tda1+j), k2 = 2*(i*tda2+j);
	  if (m1->data[k1] != m2->data[k2] ||
	      m1->data[k1+1] != m2->data[k2+1])
	    return 0;
	}
      return 1;
    }
    case EXPR::IMATRIX: {
      gsl_matrix_int *m1 = (gsl_matrix_int*)x->data.mat.p;
      gsl_matrix_int *m2 = (gsl_matrix_int*)y->data.mat.p;
      const size_t tda1 = m1->tda, tda2 = m2->tda;
      if (m1->size1 != m2->size1 || m1->size2 != m2->size2)
	return 0;
      for (size_t i = 0; i < m1->size1; i++)
	for (size_t j = 0; j < m1->size2; j++)
	  if (m1->data[i*tda1+j] != m2->data[i*tda2+j])
	    return 0;
      return 1;
    }
    default:
      return 1;
    }
  }
}

extern "C"
bool funp(const pure_expr *x)
{
  return (x->tag > 0 && x->data.clos);
}

extern "C"
bool lambdap(const pure_expr *x)
{
  return (x->tag == 0 && x->data.clos && x->data.clos->n > 0);
}

extern "C"
bool thunkp(const pure_expr *x)
{
  return (x->tag == 0 && x->data.clos && x->data.clos->n == 0);
}

extern "C"
bool symbolp(const pure_expr *x)
{
  return x->tag > 0;
}

extern "C"
bool varp(const pure_expr *x)
{
  if (x->tag <= 0) return false;
  // Check that the symbol is a variable.
  interpreter& interp = *interpreter::g_interp;
  symbol& sym = interp.symtab.sym(x->tag);
  return sym.prec == PREC_MAX && sym.fix != nonfix && sym.fix != outfix;
}

extern "C"
int32_t nargs(const pure_expr *x)
{
  size_t n = 0;
  while (x->tag == EXPR::APP) {
    n++;
    x = x->data.x[0];
  }
  if (x->tag >= 0 && x->data.clos && x->data.clos->n >= n)
    return x->data.clos->n-n;
  else
    return -1;
}

extern "C"
int32_t arity(const pure_expr *x)
{
  if (x->tag > 0) {
    symbol& sym = interpreter::g_interp->symtab.sym(x->tag);
    if (sym.prec < PREC_MAX)
      if (sym.fix == prefix || sym.fix == postfix)
	return 1;
      else
	return 2;
    else if (sym.fix == outfix)
      return 1;
    else if (sym.fix == nonfix)
      return 0;
    else
      return -1;
  } else
    return -1;
}

extern "C"
int32_t fixity(const pure_expr *x)
{
  if (x->tag > 0) {
    symbol& sym = interpreter::g_interp->symtab.sym(x->tag);
    return nprec(sym.prec, sym.fix);
  } else
    return NPREC_MAX;
}

extern "C"
int32_t pointer_get_byte(void *ptr)
{
  int8_t *p = (int8_t*)ptr;
  return *p;
}

extern "C"
int32_t pointer_get_short(void *ptr)
{
  int16_t *p = (int16_t*)ptr;
  return *p;
}

extern "C"
int32_t pointer_get_int(void *ptr)
{
  int32_t *p = (int32_t*)ptr;
  return *p;
}

extern "C"
int64_t pointer_get_int64(void *ptr)
{
  int64_t *p = (int64_t*)ptr;
  return *p;
}

extern "C"
long pointer_get_long(void *ptr)
{
#if SIZEOF_LONG==4
  int32_t *p = (int32_t*)ptr;
  return *p;
#else
  int64_t *p = (int64_t*)ptr;
  return *p;
#endif
}

extern "C"
double pointer_get_float(void *ptr)
{
  float *p = (float*)ptr;
  return *p;
}

extern "C"
double pointer_get_double(void *ptr)
{
  double *p = (double*)ptr;
  return *p;
}

extern "C"
char *pointer_get_string(void *ptr)
{
  char **p = (char**)ptr;
  return *p;
}

extern "C"
void *pointer_get_pointer(void *ptr)
{
  void **p = (void**)ptr;
  return *p;
}

extern "C"
pure_expr *pointer_get_expr(void *ptr)
{
  pure_expr **p = (pure_expr**)ptr;
  return *p;
}

extern "C"
void pointer_put_byte(void *ptr, int32_t x)
{
  int8_t *p = (int8_t*)ptr;
  *p = x;
}

extern "C"
void pointer_put_short(void *ptr, int32_t x)
{
  int16_t *p = (int16_t*)ptr;
  *p = x;
}

extern "C"
void pointer_put_int(void *ptr, int32_t x)
{
  int32_t *p = (int32_t*)ptr;
  *p = x;
}

extern "C"
void pointer_put_int64(void *ptr, int64_t x)
{
  int64_t *p = (int64_t*)ptr;
  *p = x;
}

extern "C"
void pointer_put_long(void *ptr, long x)
{
#if SIZEOF_LONG==4
  int32_t *p = (int32_t*)ptr;
  *p = x;
#else
  int64_t *p = (int64_t*)ptr;
  *p = x;
#endif
}

extern "C"
void pointer_put_float(void *ptr, double x)
{
  float *p = (float*)ptr;
  *p = x;
}

extern "C"
void pointer_put_double(void *ptr, double x)
{
  double *p = (double*)ptr;
  *p = x;
}

extern "C"
void pointer_put_string(void *ptr, const char *x)
{
  char **p = (char**)ptr;
  *p = strdup(x);
}

extern "C"
void pointer_put_pointer(void *ptr, void *x)
{
  void **p = (void**)ptr;
  *p = x;
}

extern "C"
void pointer_put_expr(void *ptr, pure_expr *x)
{
  pure_expr **p = (pure_expr**)ptr;
  *p = x;
}

#include <errno.h>

extern "C"
int pure_errno(void)
{
  return errno;
}

extern "C"
void pure_set_errno(int value)
{
  errno = value;
}

#include <time.h>

extern "C"
int64_t pure_time(void)
{
  return (int64_t)time(NULL);
}

static inline void
df(interpreter& interp, const char* s, pure_expr *x);

extern "C"
void pure_tzset(void)
{
  interpreter& interp = *interpreter::g_interp;
#ifndef HAVE_DAYLIGHT_IN_TIME_H
  int32_t timezone = 0;
  int32_t daylight = 0;
# ifdef HAVE_TM_GMTOFF_IN_TM
  time_t t = time(NULL);
  tm* lt = localtime(&t);
  // SUSv3: The value of tm_isdst shall be
  // positive if Daylight Savings Time is in effect,
  // 0 if Daylight Savings Time is not in effect,
  // and negative if the information is not available.
  //
  // We treat "Not available" as "not in effect".
  timezone = (lt->tm_isdst > 0) * 3600 - lt->tm_gmtoff;
  daylight = lt->tm_isdst;
# endif
#endif
  tzset();
  df(interp, "timezone",	pure_int(timezone));
  df(interp, "daylight",	pure_int(daylight));
  df(interp, "tzname",		pure_listl(2, pure_cstring_dup(tzname[0]),
					   pure_cstring_dup(tzname[1])));
}

/* Note that the following are not thread-safe as they use statically
   allocated buffers. */

extern "C"
char *pure_ctime(int64_t t)
{
  time_t time = (time_t)t;
  return ctime(&time);
}

extern "C"
struct tm *pure_gmtime(int64_t t)
{
  time_t time = (time_t)t;
  return gmtime(&time);
}

extern "C"
struct tm *pure_localtime(int64_t t)
{
  time_t time = (time_t)t;
  return localtime(&time);
}

extern "C"
int64_t pure_mktime(struct tm *tm)
{
  return (int64_t)mktime(tm);
}

extern "C"
char *pure_strftime(const char *format, struct tm *tm)
{
  static char buf[1024];
  if (!strftime(buf, 1024, format, tm))
    /* The interface to strftime is rather brain-damaged since it returns zero
       both in case of a buffer overflow and when the resulting string is
       empty. We just pretend that there cannot be any errors and return an
       empty string in both cases. */
    buf[0] = 0;
  return buf;
}

extern "C"
pure_expr *pure_strptime(const char *s, const char *format, struct tm *tm)
{
  const char *res = strptime(s, format, tm);
  if (res)
    return pure_cstring_dup(res);
  else
    return 0;
}

#ifdef HAVE_GETTIMEOFDAY
#include <sys/time.h>
extern "C"
double pure_gettimeofday(void)
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return ((double)tv.tv_sec)+((double)tv.tv_usec)*1e-6;
}
#else
#ifdef HAVE_FTIME
#include <sys/timeb.h>
extern "C"
double pure_gettimeofday(void)
{
  struct timeb tb;
  ftime(&tb);
  return ((double)tb.time)+((double)tb.millitm)*1e-3;
}
#else
extern "C"
double pure_gettimeofday(void)
{
  return (double)time(NULL);
}
#endif
#endif

#ifdef __MINGW32__
#include <windows.h>
double pure_nanosleep(double t)
{
  if (t > 0.0) {
    unsigned long secs;
    unsigned short msecs;
    double ip, fp;
    if (t > LONG_MAX) t = LONG_MAX;
    fp = modf(t, &ip);
    secs = (unsigned long)ip;
    msecs = (unsigned short)(fp*1e3);
    Sleep(secs*1000U+msecs);
  }
  return 0.0;
}
#else
double pure_nanosleep(double t)
{
  if (t > 0.0) {
    double ip, fp;
    unsigned long secs;
#ifdef HAVE_NANOSLEEP
    unsigned long nsecs;
    struct timespec req, rem;
    fp = modf(t, &ip);
    if (ip > LONG_MAX) { ip = (double)LONG_MAX; fp = 0.0; }
    secs = (unsigned long)ip;
    nsecs = (unsigned long)(fp*1e9);
    req.tv_sec = secs; req.tv_nsec = nsecs;
    if (nanosleep(&req, &rem))
      return ((double)rem.tv_sec)+((double)rem.tv_nsec)*1e-9;
    else
      return 0.0;
#else
#ifdef HAVE_USLEEP
    unsigned long usecs;
    if (t > LONG_MAX) t = LONG_MAX;
    fp = modf(t, &ip);
    secs = (unsigned long)ip;
    usecs = (unsigned long)(fp*1e6);
    usleep(secs*1000000U+usecs);
    return 0.0;
#else
    fp = modf(t, &ip);
    if (ip > LONG_MAX) ip = (double)LONG_MAX;
    secs = (unsigned long)ip;
    return (double)sleep(secs);
#endif
#endif
  } else
    return 0.0;
}
#endif

#ifndef __MINGW32__
extern "C"
int spawnv(int mode, const char *prog, char * const argv[])
{
  int pid;
  if (mode == P_OVERLAY) {
    execv(prog, argv);
    return -1;
  }
  switch ((pid = fork())) {
  case 0:
    execv(prog, argv);
    exit(1);
  case -1:
    return -1;
  }
  if (mode == P_WAIT) {
    int status;
    waitpid(pid, &status, 0);
    return status;
  } else
    return pid;
}

extern "C"
int spawnvp(int mode, const char *prog, char * const argv[])
{
  int pid;
  if (mode == P_OVERLAY) {
    execvp(prog, argv);
    return -1;
  }
  switch ((pid = fork())) {
  case 0:
    execvp(prog, argv);
    exit(1);
  case -1:
    return -1;
  }
  if (mode == P_WAIT) {
    int status;
    waitpid(pid, &status, 0);
    return status;
  } else
    return pid;
}

extern "C"
int spawnve(int mode, const char *prog, char * const argv[],
		 char * const envp[])
{
  int pid;
  if (mode == P_OVERLAY) {
    execve(prog, argv, envp);
    return -1;
  }
  switch ((pid = fork())) {
  case 0:
    execve(prog, argv, envp);
    exit(1);
  case -1:
    return -1;
  }
  if (mode == P_WAIT) {
    int status;
    waitpid(pid, &status, 0);
    return status;
  } else
    return pid;
}
#endif

#ifdef __MINGW32__
/* Windows compatibility. */

extern "C"
int execv(const char* prog, char* const argv[])
{
  return _execv(prog, argv);
}

extern "C"
int execvp(const char* prog, char* const argv[])
{
  return _execvp(prog, argv);
}

extern "C"
int execve(const char* prog, char* const argv[], char* const envp[])
{
  return _execve(prog, argv, envp);
}

#ifdef __MINGW64__
// for reasons unknown to mankind, these return intptr_t instead of an int
extern "C"
intptr_t spawnv(int mode, const char* prog, char* const argv[])
{
  return _spawnv(mode, prog, argv);
}

extern "C"
intptr_t spawnvp(int mode, const char* prog, char* const argv[])
{
  return _spawnvp(mode, prog, argv);
}

extern "C"
intptr_t spawnve(int mode, const char* prog, char* const argv[],
	    char* const envp[])
{
  return _spawnve(mode, prog, argv, envp);
}
#else
extern "C"
int spawnv(int mode, const char* prog, char* const argv[])
{
  return _spawnv(mode, prog, argv);
}

extern "C"
int spawnvp(int mode, const char* prog, char* const argv[])
{
  return _spawnvp(mode, prog, argv);
}

extern "C"
int spawnve(int mode, const char* prog, char* const argv[],
	    char* const envp[])
{
  return _spawnve(mode, prog, argv, envp);
}
#endif

#undef fileno

extern "C"
int fileno(FILE *fp)
{
  return _fileno(fp);
}

extern "C"
FILE *fdopen(int fd, const char *mode)
{
  return _fdopen(fd, mode);
}

#undef popen
#undef pclose

extern "C"
FILE *popen(const char *command, const char *type)
{
  return _popen(command, type);
}

extern "C"
int pclose(FILE *stream)
{
  return _pclose(stream);
}

extern "C"
unsigned int sleep(unsigned int secs)
{
  Sleep(secs*1000);
  return 0;
}
#endif

/* Horrible kludge to get round, trunc and the inverse hyperbolic functions
   from libmingwex.a (these are in C99, but not in the Windows system
   libraries, and LLVM doesn't know how to get them either). */

extern "C"
double __round(double x)
{
  return round(x);
}

extern "C"
double __trunc(double x)
{
  return trunc(x);
}

extern "C"
double __asinh(double x)
{
  return asinh(x);
}

extern "C"
double __acosh(double x)
{
  return acosh(x);
}

extern "C"
double __atanh(double x)
{
  return atanh(x);
}

/* File type bits. */

static pure_expr *statbuf(struct stat *buf)
{
  pure_expr *st[11];
  /* On modern 64 bit systems, most of these fields may be 64 bit values, we
     encode these as bigints on all platforms. */
  st[0]  = pure_uint64(buf->st_dev);
  st[1]  = pure_uint64(buf->st_ino);
  st[2]  = pure_int(buf->st_mode);
  st[3]  = pure_int(buf->st_nlink);
  st[4]  = pure_int(buf->st_uid);
  st[5]  = pure_int(buf->st_gid);
#ifdef HAVE_STRUCT_STAT_ST_RDEV
  st[6]  = pure_uint64(buf->st_rdev);
#else
  st[6]  = pure_uint64(0);
#endif
  st[7]  = pure_uint64(buf->st_size);
  st[8]  = pure_int64(buf->st_atime);
  st[9]  = pure_int64(buf->st_mtime);
  st[10] = pure_int64(buf->st_ctime);
  return pure_tuplev(11, st);
}

extern "C"
pure_expr *pure_stat(const char *path)
{
  struct stat buf;
  if (stat(path, &buf))
    return 0;
  else
    return statbuf(&buf);
}

#ifdef _WIN32
#define lstat stat
#endif

extern "C"
pure_expr *pure_lstat(const char *path)
{
  struct stat buf;
  if (lstat(path, &buf))
    return 0;
  else
    return statbuf(&buf);
}

extern "C"
pure_expr *pure_fstat(FILE *fp)
{
#ifdef HAVE_FSTAT
  struct stat buf;
  int fd = fileno(fp);
  if (fd<0 || fstat(fd, &buf))
    return 0;
  else
    return statbuf(&buf);
#else
  return 0;
#endif
}

// Get rid of "format not a string literal" warnings.
#pragma GCC diagnostic ignored "-Wformat-security"

/* Helper functions to parse printf/scanf format strings. These also support
   the gmp and mpfr type modifiers Z and R. */

extern "C"
pure_expr *pure_printf_split(const char *format)
{
  size_t n = 0;
  const char *s = format, *t;
  char *buf;
  pure_expr *x, **xv;
  while (*s) {
    if (*s == '%') {
      t = s+1;
      if (*t == '%') {
	++t; goto literal;
      }
      while (strchr("+-#0 ", *t)) ++t;
      while (isdigit(*t)) ++t;
      if (*t == '.') {
	++t;
	while (isdigit(*t)) ++t;
      }
      if (*t == 'Z') {
        ++t;
      } else if (*t == 'R') {
        if (strchr("UDYZN", *++t)) ++t;
      }
      if (strchr("cdiouxXeEfgGaAsp", *t)) {
	++t; ++n;
	s = t;
      } else
	return NULL; /* bad format specifier */
    } else {
      t = s+1;
    literal:
      while (*t && *t != '%') t++;
      ++n;
      s = t;
    }
  }
  if (n == 0) return pure_listl(0);
  xv = (pure_expr**)malloc(n*sizeof(pure_expr*));
  n = 0; s = format;
  while (*s) {
    if (*s == '%') {
      t = s+1;
      if (*t == '%') {
	++t; goto literal2;
      }
      while (strchr("+-#0 ", *t)) ++t;
      while (isdigit(*t)) ++t;
      if (*t == '.') {
	++t;
	while (isdigit(*t)) ++t;
      }
      if (*t == 'Z') {
        ++t;
      } else if (*t == 'R') {
        if (strchr("UDYZN", *++t)) ++t;
      }
      if (strchr("cdiouxXeEfgGaAsp", *t)) {
	++t;
	buf = (char*)malloc(t-s+1);
	strncpy(buf, s, t-s);
	buf[t-s] = 0;
	xv[n++] = pure_cstring(buf);
	s = t;
      } else
	return NULL; /* bad format specifier */
    } else {
      t = s+1;
    literal2:
      while (*t && *t != '%') t++;
      buf = (char*)malloc(t-s+1);
      strncpy(buf, s, t-s);
      buf[t-s] = 0;
      xv[n++] = pure_cstring(buf);
      s = t;
    }
  }
  x = pure_listv(n, xv);
  free(xv);
  return x;
}

extern "C"
pure_expr *pure_scanf_split(const char *format)
{
  size_t n = 0;
  const char *s = format, *t;
  char *buf;
  pure_expr *x, **xv;
  while (*s) {
    if (*s == '%') {
      t = s+1;
      if (*t == '%') {
	++t; goto literal;
      }
      if (*t == '*') ++t;
      while (isdigit(*t)) ++t;
      if (*t == 'Z') ++t;
      if (strchr("cdiouxXneEfgasp", *t)) {
	++t; ++n;
	s = t;
      } else if (*t == '[') {
        ++t; ++n;
	if (*t == '^') ++t;
	if (*t == ']') ++t;
	while (*t && *t != ']') ++t;
	if (*t != ']') return NULL; /* bad format specifier */
	s = ++t;
      } else
	return NULL; /* bad format specifier */
    } else {
      t = s+1;
    literal:
      while (*t && *t != '%') t++;
      ++n;
      s = t;
    }
  }
  if (n == 0) return pure_listl(0);
  xv = (pure_expr**)malloc(n*sizeof(pure_expr*));
  n = 0; s = format;
  while (*s) {
    if (*s == '%') {
      t = s+1;
      if (*t == '%') {
	++t; goto literal2;
      }
      if (*t == '*') ++t;
      while (isdigit(*t)) ++t;
      if (*t == 'Z') ++t;
      if (strchr("cdiouxXneEfgasp", *t)) {
	++t;
	buf = (char*)malloc(t-s+1);
	strncpy(buf, s, t-s);
	buf[t-s] = 0;
	xv[n++] = pure_cstring(buf);
	s = t;
      } else if (*t == '[') {
        ++t;
	if (*t == '^') ++t;
	if (*t == ']') ++t;
	while (*t && *t != ']') ++t;
	if (*t != ']') return NULL; /* bad format specifier */
	++t;
	buf = (char*)malloc(t-s+1);
	strncpy(buf, s, t-s);
	buf[t-s] = 0;
	xv[n++] = pure_cstring(buf);
	s = t;
      } else
	return NULL; /* bad format specifier */
    } else {
      t = s+1;
    literal2:
      while (*t && *t != '%') t++;
      buf = (char*)malloc(t-s+1);
      strncpy(buf, s, t-s);
      buf[t-s] = 0;
      xv[n++] = pure_cstring(buf);
      s = t;
    }
  }
  x = pure_listv(n, xv);
  free(xv);
  return x;
}

extern "C"
int pure_scanf_prec(const char *format)
{
  if (*format == '%') ++format; else return 0;
  if (*format == '*') ++format;
  if (isdigit(*format)) return atoi(format);
  return 0;
}

extern "C"
void pure_mpz_clear(mpz_t z)
{
  mpz_clear(z);
}

extern "C"
size_t pure_mpz_sizeinbase(mpz_t z, int b)
{
  return mpz_sizeinbase(z, b);
}

extern "C"
int pure_mpfr_get_prec(mpfr_ptr x)
{
  return mpfr_get_prec(x);
}

extern "C"
int pure_fprintf(FILE *fp, const char *format)
{
  /* Note that 'format' *must* be passed as the format string here in order to
     expand embedded '%%' literals. Some recent gcc versions will complain
     about this ("format not a string literal and no format arguments"), the
     pragma above should get rid of this. */
  return fprintf(fp, format);
}

extern "C"
int pure_fprintf_int(FILE *fp, const char *format, int32_t x)
{
  return fprintf(fp, format, x);
}

extern "C"
int pure_fprintf_double(FILE *fp, const char *format, double x)
{
  return fprintf(fp, format, x);
}

extern "C"
int pure_fprintf_string(FILE *fp, const char *format, const char *x)
{
  return fprintf(fp, format, x);
}

extern "C"
int pure_fprintf_pointer(FILE *fp, const char *format, const void *x)
{
  return fprintf(fp, format, x);
}

extern "C"
int pure_fprintf_mpz(FILE *fp, const char *format, mpz_t x)
{
  return gmp_fprintf(fp, format, x);
}

extern "C"
int pure_fprintf_mpfr(FILE *fp, const char *format, mpfr_ptr x)
{
  return mpfr_fprintf(fp, format, x);
}

extern "C"
int pure_snprintf(char *buf, size_t size, const char *format)
{
  return snprintf(buf, size, format);
}

extern "C"
int pure_snprintf_int(char *buf, size_t size, const char *format, int32_t x)
{
  return snprintf(buf, size, format, x);
}

extern "C"
int pure_snprintf_double(char *buf, size_t size, const char *format, double x)
{
  return snprintf(buf, size, format, x);
}

extern "C"
int pure_snprintf_string(char *buf, size_t size, const char *format, const char *x)
{
  return snprintf(buf, size, format, x);
}

extern "C"
int pure_snprintf_pointer(char *buf, size_t size, const char *format, const void *x)
{
  return snprintf(buf, size, format, x);
}

extern "C"
int pure_snprintf_mpz(char *s, int n, const char *format, mpz_t x)
{
  return gmp_snprintf(s, n, format, x);
}

extern "C"
int pure_snprintf_mpfr(char *s, int n, const char *format, mpfr_ptr x)
{
  return mpfr_snprintf(s, n, format, x);
}

#define myformat(format) scanf_format((char*)alloca(strlen(format)+3), format)

static inline char *scanf_format(char *buf, const char *format)
{
  strcpy(buf, format); strcat(buf, "%n");
  return buf;
}

extern "C"
int pure_fscanf(FILE *fp, const char *format)
{
  int count = -1;
  if (fscanf(fp, myformat(format), &count) == EOF) count = -1;
  return count;
}

extern "C"
int pure_fscanf_int(FILE *fp, const char *format, int32_t *x)
{
  // wrap this up in case int on the target platform is not 32 bit
  int count = -1, y;
  if (fscanf(fp, myformat(format), &y, &count) == EOF) count = -1;
  if (count >= 0) *x = y;
  return count;
}

extern "C"
int pure_fscanf_double(FILE *fp, const char *format, double *x)
{
  int count = -1;
  if (fscanf(fp, myformat(format), x, &count) == EOF) count = -1;
  return count;
}

extern "C"
int pure_fscanf_string(FILE *fp, const char *format, const char *x)
{
  int count = -1;
  if (fscanf(fp, myformat(format), x, &count) == EOF) count = -1;
  return count;
}

extern "C"
int pure_fscanf_pointer(FILE *fp, const char *format, const void **x)
{
  int count = -1;
  if (fscanf(fp, myformat(format), x, &count) == EOF) count = -1;
  return count;
}

extern "C"
int pure_fscanf_mpz(FILE *fp, const char *format, mpz_t x)
{
  int count = -1;
  if (gmp_fscanf(fp, myformat(format), x, &count) == EOF) count = -1;
  return count;
}

extern "C"
int pure_sscanf(const char *buf, const char *format)
{
  int count = -1;
  sscanf(buf, myformat(format), &count);
  return count;
}

extern "C"
int pure_sscanf_int(const char *buf, const char *format, int32_t *x)
{
  // wrap this up in case int on the target platform is not 32 bit
  int count = -1, y; sscanf(buf, myformat(format), &y, &count);
  if (count >= 0) *x = y;
  return count;
}

extern "C"
int pure_sscanf_double(const char *buf, const char *format, double *x)
{
  int count = -1;
  sscanf(buf, myformat(format), x, &count);
  return count;
}

extern "C"
int pure_sscanf_string(const char *buf, const char *format, char *x)
{
  int count = -1;
  sscanf(buf, myformat(format), x, &count);
  return count;
}

extern "C"
int pure_sscanf_pointer(const char *buf, const char *format, void **x)
{
  int count = -1;
  sscanf(buf, myformat(format), x, &count);
  return count;
}

extern "C"
int pure_sscanf_mpz(const char *buf, const char *format, mpz_t x)
{
  int count = -1;
  if (gmp_sscanf(buf, myformat(format), x, &count) == EOF) count = -1;
  return count;
}

#if HAVE_DIRENT_H
#include <dirent.h>
#endif
#define N_ENT 1024 // buffer increment

pure_expr *pure_readdir(const char *name)
{
#ifdef HAVE_READDIR
  pure_expr *x;
  DIR *dir;
  struct dirent *d;
  int n = 0, m = N_ENT;
  /* open directory */
  if (!(dir = opendir(name))) {
    return 0;
  }
  /* allocate an initial buffer large enough for N_ENT entries */
  pure_expr **xv = (pure_expr **)malloc(m * sizeof(pure_expr *));
  if (!xv) goto alloc_error;
  /* read entries */
  for (d = readdir(dir); d; d = readdir(dir)) {
    if (n >= m) {
      /* we need to enlarge the buffer */
      m += N_ENT;
      pure_expr **xv1 = (pure_expr **)realloc(xv, m * sizeof(pure_expr *));
      if (!xv1) goto alloc_error;
      xv = xv1;
    }
    if ((xv[n] = pure_cstring_dup(d->d_name)))
      n++;
    else
      goto alloc_error;
  }
  /* close directory */
  closedir(dir);
  /* return the list */
  x = pure_listv(n, xv);
  free(xv);
  return x;
 alloc_error:
  closedir(dir);
  if (xv) {
    while (n > 0) pure_freenew(xv[--n]);
    free(xv);
  }
  pure_throw(pure_symbol(pure_sym("malloc_error")));
#endif
  return 0;
}

#include <fnmatch.h>
#include <glob.h>

extern "C"
pure_expr *globlist(const glob_t *pglob)
{
  interpreter& interp = *interpreter::g_interp;
  pure_expr *x = mk_nil();
  int i = pglob->gl_pathc;
  while (--i >= 0) {
    pure_expr *f = pure_symbol(interp.symtab.cons_sym().f);
    pure_expr *y = pure_cstring_dup(pglob->gl_pathv[i]);
    x = pure_apply2(pure_apply2(f, y), x);
  }
  return x;
}

#include <sys/types.h>
#if USE_PCRE
#include <pcre.h>
#include <pcreposix.h>
#else
#include <regex.h>
#endif

static int translate_pos(char *s, int p, int l)
{
  assert(p >= 0 && p <= l);
  char c = s[p]; s[p] = 0;
  /* XXXFIXME: This probably won't work for stateful encodings, because we're
     converting a substring here. */
  char *t = toutf8(s); assert(t);
  int res = strlen(t);
  free(t); s[p] = c;
  return res;
}

#if 0

/* This is the old regex interface helper functions for Pure <= 0.41. These
   are still listed here for documentation purposes, but aren't included in
   the runtime any more. */

extern "C"
pure_expr *regmatches(const regex_t *preg, int flags)
{
  interpreter& interp = *interpreter::g_interp;
  int n = (flags&REG_NOSUB)?0:preg->re_nsub+1;
  regmatch_t *matches = 0;
  if (n > 0) matches = (regmatch_t*)malloc(n*sizeof(regmatch_t));
  pure_expr *f = pure_symbol(interp.symtab.pair_sym().f);
  pure_expr *x = pure_apply2(pure_apply2(f, pure_int(n)),
			     pure_pointer(matches));
  return x;
}

extern "C"
pure_expr *reglist(const regex_t *preg, const char *s,
		   const regmatch_t *matches)
{
  interpreter& interp = *interpreter::g_interp;
  if (!matches) return mk_void();
  int n = preg->re_nsub+1;
  pure_expr *x = 0;
  int i = n;
  if (strcmp(default_encoding(), "UTF-8") == 0) {
    // Optimize for the case that the system encoding is utf-8. This should be
    // pretty much standard on Linux/Unix systems these days.
    while (--i >= 0) {
      pure_expr *f = pure_symbol(interp.symtab.pair_sym().f);
      pure_expr *y1 = pure_int(matches[i].rm_so);
      pure_expr *y2;
      if (matches[i].rm_so >= 0 && matches[i].rm_eo >= matches[i].rm_so) {
	size_t n = matches[i].rm_eo - matches[i].rm_so;
	char *buf = (char*)malloc(n+1); assert(buf);
	strncpy(buf, s+matches[i].rm_so, n); buf[n] = 0;
	y2 = pure_cstring(buf);
      } else
	y2 = pure_cstring_dup("");
      if (x)
	x = pure_apply2(pure_apply2(f, y2), x);
      else
	x = y2;
      x = pure_apply2(pure_apply2(f, y1), x);
    }
    return x;
  }
  // Translate the subject string to the system encoding. We also use this as
  // a work buffer to translate system encoding offsets to the utf-8 encoding
  // on the fly.
  char *t = fromutf8(s); assert(t);
  // Compute the position of the first match.
  assert(matches[0].rm_so >= 0);
  int l0 = strlen(t), p0 = matches[0].rm_so, q0 = translate_pos(t, p0, l0);
  char *u = t+p0;
  int l = l0-p0;
  while (--i >= 0) {
    int p = matches[i].rm_so, q;
    if (p < 0)
      q = -1;
    else {
      assert(p >= p0);
      q = q0 + translate_pos(u, p-p0, l);
    }
    pure_expr *f = pure_symbol(interp.symtab.pair_sym().f);
    pure_expr *y1 = pure_int(q);
    pure_expr *y2;
    if (q >= 0 && matches[i].rm_eo >= matches[i].rm_so) {
      size_t n = matches[i].rm_eo - matches[i].rm_so;
      char *buf = (char*)malloc(n+1); assert(buf);
      strncpy(buf, t+matches[i].rm_so, n); buf[n] = 0;
      y2 = pure_cstring(buf);
    } else
      y2 = pure_cstring_dup("");
    if (x)
      x = pure_apply2(pure_apply2(f, y2), x);
    else
      x = y2;
    x = pure_apply2(pure_apply2(f, y1), x);
  }
  free(t);
  return x;
}

#endif

/* New regex interface (Pure 0.42 and later). We now maintain our own regex
   pattern buffer which supports additional convenience operations for
   iterating over matches. (This data structure is *not* compatible with the
   POSIX regex functions, so don't try passing instances of pure_regex_t to
   those!) */

// TLD: POSIX-compatible regex API. We define these as function pointers here
// which get resolved after the necessary system libraries have been loaded,
// so that we always get the right collection of functions depending on
// whether we're using pcre or not.
static int (*myregcomp)(regex_t *, const char *, int);
static int (*myregexec)(const regex_t *, const char *, size_t, regmatch_t *, int);
static size_t (*myregerror)(int, const regex_t *, char *, size_t);
static void (*myregfree)(regex_t *);

#ifdef HAVE_LLVM_SUPPORT_DYNAMICLIBRARY_H
#include <llvm/Support/DynamicLibrary.h>
#else
#include <llvm/System/DynamicLibrary.h>
#endif

static bool init_regex()
{
  if (!myregcomp) {
#if USE_PCRE && defined(LIBPCRE)
    string msg;
    llvm::sys::DynamicLibrary lib =
      llvm::sys::DynamicLibrary::getPermanentLibrary(LIBPCRE, &msg);
    if (lib.isValid()) {
      // On some systems (e.g., Ubuntu), the libpcreposix functions are
      // actually #defined as the real function names which are stropped with
      // a prefix for disambiguation purposes. In this case we just fall
      // through to the default case below, which should pick up the right
      // functions the runtime gets linked with at load time. Otherwise we use
      // explicit dlopen/dlsym calls to make sure that we get the functions
      // from the right library.
#ifndef regcomp
      myregcomp = (int (*)(regex_t*, const char*, int))
	lib.getAddressOfSymbol("regcomp");
      myregexec = (int (*)(const regex_t*, const char*, size_t, regmatch_t*, int))
	lib.getAddressOfSymbol("regexec");
      myregerror = (size_t (*)(int, const regex_t*, char*, size_t))
	lib.getAddressOfSymbol("regerror");
      myregfree = (void (*)(regex_t*))
	lib.getAddressOfSymbol("regfree");
      if (myregcomp && myregexec && myregerror && myregfree) return true;
      msg = LIBPCRE ": missing regex functions";
#endif
    }
#if 0
    if (!msg.empty()) std::cerr << "warning: " << msg << '\n';
#endif
    // If we couldn't find the functions by these means, we fall through to
    // the default case below which just uses whatever regex functions the
    // runtime is linked with. This should still do the right thing in most
    // cases, but we're at the mercy of the linker here. On some systems, we
    // might get incompatible regex functions from system libraries, which may
    // cause anything from faulty behavior to downright crashes.
#endif
    myregcomp = (int (*)(regex_t*, const char*, int))regcomp;
    myregexec = (int (*)(const regex_t*, const char*, size_t, regmatch_t*, int))regexec;
    myregerror = (size_t (*)(int, const regex_t*, char*, size_t))regerror;
    myregfree = (void (*)(regex_t*))regfree;
    if (!myregcomp || !myregexec || !myregerror || !myregfree) {
      myregcomp = 0; myregexec = 0; myregerror = 0; myregfree = 0;
      std::cerr << "error: couldn't find any regex functions" << '\n';
      return false;
    }
  }
  return true;
}

struct pure_regex_t {
  regex_t reg; // compiled pattern
  int res; // last result
  int n; // number of submatches
  regmatch_t *matches; // submatches, if any
  char *s, *p, *q; // current string to be matched, match pointers
  int eflags; // current execution flags
  mbstate_t st; // shift state for multibyte processing
  pure_regex_t(const char *pat, int cflags)
    : n(0), matches(0), s(0), p(0), q(0), eflags(0)
  {
    if (!init_regex()) exit(1);
    res = myregcomp(&reg, pat, cflags);
    if (res == 0) {
      memset(&st, 0, sizeof(mbstate_t));
      n = (cflags&REG_NOSUB)?0:reg.re_nsub+1;
      if (n > 0) matches = new regmatch_t[n];
    }
  }
  ~pure_regex_t()
  {
    if (!init_regex()) exit(1);
    myregfree(&reg);
    if (matches) delete[] matches;
    if (s) free(s);
  }
  // start a new match
  int start_match(const char *_s, int _eflags)
  {
    if (!init_regex()) exit(1);
    if (res == 0 || res == REG_NOMATCH) {
      if (s) free(s);
      s = strdup(_s); p = q = s; assert(s);
      eflags = _eflags;
      memset(&st, 0, sizeof(mbstate_t));
      res = myregexec(&reg, s, n, matches, eflags);
    }
    return res;
  }
  // next match
  int next_match(bool overlap = false)
  {
    if (!init_regex()) exit(1);
    if (res == 0 && s) {
      if (n > 0) {
	regoff_t k = overlap?matches[0].rm_so:matches[0].rm_eo;
	if (k > 0) {
	  skipchars(k);
	  if (overlap) nextchar();
	  res = myregexec(&reg, p, n, matches, eflags);
	} else if (*p) {
	  nextchar();
	  res = myregexec(&reg, p, n, matches, eflags);
	} else
	  res = REG_NOMATCH;
      } else
	res = REG_NOMATCH;
    } else if (res == 0)
      res = REG_NOMATCH;
    return res;
  }
  // finish match
  void end_match()
  {
    if (s) free(s);
    s = p = q = 0;
  }
  // advance the pointer by one (multibyte) char
  void nextchar()
  {
    int n = mbrtowc(0, p, MB_LEN_MAX, &st);
    q = p;
    if (n > 0)
      p += n;
    else if (*p)
      // failed conversion, assume single byte char
      p++;
  }
  // advance the pointer by the given byte count
  void skipchars(int n)
  {
    char *buf = new char[n+1];
    strncpy(buf, p, n); buf[n] = 0;
    const char *s = buf;
    mbsrtowcs(NULL, &s, 0, &st);
    delete[] buf;
    p += n; q = p;
  }
  // retrieve error information
  char *error_info()
  {
    if (!init_regex()) exit(1);
    size_t size = myregerror(res, &reg, 0, 0);
    char *buf = (char*)malloc(size);
    myregerror(res, &reg, buf, size);
    return buf;
  }
  // retrieve match information
  pure_expr *match_info();
  pure_expr *skip_info();
};

static inline regoff_t shift_offs(regoff_t x, size_t y)
{
  return (x < 0)?x:x+y;
}

pure_expr *pure_regex_t::match_info()
{
  interpreter& interp = *interpreter::g_interp;
  if (!s || res) return 0;
  if (!matches || !s) return mk_void();
  pure_expr *x = 0;
  int i = n;
  size_t offs = p-s;
  if (strcmp(default_encoding(), "UTF-8") == 0) {
    // Optimize for the case that the system encoding is utf-8. This should be
    // pretty much standard on Linux/Unix systems these days.
    while (--i >= 0) {
      pure_expr *f = pure_symbol(interp.symtab.pair_sym().f);
      pure_expr *y1 = pure_int(shift_offs(matches[i].rm_so, offs));
      pure_expr *y2;
      if (matches[i].rm_so >= 0 && matches[i].rm_eo >= matches[i].rm_so) {
	size_t n = matches[i].rm_eo - matches[i].rm_so;
	char *buf = (char*)malloc(n+1); assert(buf);
	strncpy(buf, p+matches[i].rm_so, n); buf[n] = 0;
	y2 = pure_cstring(buf);
      } else
	y2 = pure_cstring_dup("");
      if (x)
	x = pure_apply2(pure_apply2(f, y2), x);
      else
	x = y2;
      x = pure_apply2(pure_apply2(f, y1), x);
    }
    return x;
  }
  // Compute the position of the first match.
  char *t = p;
  assert(matches[0].rm_so >= 0);
  int l0 = strlen(t), p0 = matches[0].rm_so, q0 = translate_pos(t, p0, l0);
  char *u = t+p0;
  int l = l0-p0;
  while (--i >= 0) {
    int p = matches[i].rm_so, q;
    if (p < 0)
      q = -1;
    else {
      assert(p >= p0);
      q = q0 + translate_pos(u, p-p0, l);
    }
    pure_expr *f = pure_symbol(interp.symtab.pair_sym().f);
    pure_expr *y1 = pure_int(shift_offs(q, offs));
    pure_expr *y2;
    if (q >= 0 && matches[i].rm_eo >= matches[i].rm_so) {
      size_t n = matches[i].rm_eo - matches[i].rm_so;
      char *buf = (char*)malloc(n+1); assert(buf);
      strncpy(buf, t+matches[i].rm_so, n); buf[n] = 0;
      y2 = pure_cstring(buf);
    } else
      y2 = pure_cstring_dup("");
    if (x)
      x = pure_apply2(pure_apply2(f, y2), x);
    else
      x = y2;
    x = pure_apply2(pure_apply2(f, y1), x);
  }
  return x;
}

pure_expr *pure_regex_t::skip_info()
{
  if (!matches || !s) return 0;
  if (res == REG_NOMATCH) return pure_cstring_dup(p);
  assert(q <= p && matches[0].rm_so >= 0);
  size_t n = p-q+matches[0].rm_so;
  char *buf = (char*)malloc(n+1); assert(buf);
  strncpy(buf, q, n); buf[n] = 0;
  return pure_cstring(buf);
}

// C wrappers.

extern "C"
pure_regex_t *pure_regcomp(const char *pat, int cflags)
{
  return new pure_regex_t(pat, cflags);
}

extern "C"
void pure_regfree(pure_regex_t *reg)
{
  if (reg) delete reg;
}

extern "C"
int pure_regexec(pure_regex_t *reg, const char *s, int eflags)
{
  return reg?reg->start_match(s, eflags):-1;
}

extern "C"
int pure_regnext(pure_regex_t *reg, int overlap)
{
  return reg?reg->next_match(overlap != 0):-1;
}

extern "C"
void pure_regdone(pure_regex_t *reg)
{
  if (reg) reg->end_match();
}

extern "C"
int pure_regstatus(pure_regex_t *reg)
{
  return reg?reg->res:-1;
}

extern "C"
pure_expr *pure_regerror(pure_regex_t *reg)
{
  return reg?pure_cstring(reg->error_info()):0;
}

extern "C"
pure_expr *pure_regmatch(pure_regex_t *reg)
{
  return reg?reg->match_info():0;
}

extern "C"
pure_expr *pure_regskip(pure_regex_t *reg)
{
  return reg?reg->skip_info():0;
}

extern "C"
pure_expr *pure_addr(const char *s)
{
  void *p = llvm::sys::DynamicLibrary::SearchForAddressOfSymbol(s);
  if (p)
    return pure_pointer(p);
  else
    return 0;
}

static inline void
df(interpreter& interp, const char* s, pure_expr *x)
{
  try {
    interp.defn(s, x);
  } catch (err &e) {
    cerr << "warning: " << e.what() << endl;
  }
  MEMDEBUG_FREE(x)
}

static inline void
cdf(interpreter& interp, const char* s, pure_expr *x)
{
  try {
    interp.const_defn(s, x);
  } catch (err &e) {
    cerr << "warning: " << e.what() << endl;
  }
  pure_freenew(x);
}

extern "C"
void pure_sys_vars(void)
{
  interpreter& interp = *interpreter::g_interp;
  // standard I/O streams
  int ty = pure_pointer_tag("FILE*");
  df(interp, "stdin",	pure_tag(ty, pure_pointer(stdin)));
  df(interp, "stdout",	pure_tag(ty, pure_pointer(stdout)));
  df(interp, "stderr",	pure_tag(ty, pure_pointer(stderr)));
  // time functions
  cdf(interp, "CLOCKS_PER_SEC",	pure_int(CLOCKS_PER_SEC));
  cdf(interp, "SIZEOF_TM",	pure_int(sizeof(struct tm)));
  pure_tzset();
  // fnmatch, glob
  cdf(interp, "FNM_NOESCAPE",	pure_int(FNM_NOESCAPE));
  cdf(interp, "FNM_PATHNAME",	pure_int(FNM_PATHNAME));
  cdf(interp, "FNM_PERIOD",	pure_int(FNM_PERIOD));
  cdf(interp, "FNM_CASEFOLD",	pure_int(FNM_CASEFOLD));
  // We should probably get rid of this one, but system::glob still needs it.
  cdf(interp, "SIZEOF_GLOB",	pure_int(sizeof(glob_t)));
  cdf(interp, "GLOB_ERR",	pure_int(GLOB_ERR));
  cdf(interp, "GLOB_MARK",	pure_int(GLOB_MARK));
  cdf(interp, "GLOB_NOSORT",	pure_int(GLOB_NOSORT));
  cdf(interp, "GLOB_NOCHECK",	pure_int(GLOB_NOCHECK));
  cdf(interp, "GLOB_NOESCAPE",	pure_int(GLOB_NOESCAPE));
  /* These seem to be GNU-specific. */
#ifdef GLOB_PERIOD
  cdf(interp, "GLOB_PERIOD",	pure_int(GLOB_PERIOD));
#endif
#ifdef GLOB_ONLYDIR
  cdf(interp, "GLOB_ONLYDIR",	pure_int(GLOB_ONLYDIR));
#endif
  cdf(interp, "GLOB_BRACE",	pure_int(GLOB_BRACE));
  cdf(interp, "GLOB_NOMAGIC",	pure_int(GLOB_NOMAGIC));
  cdf(interp, "GLOB_TILDE",	pure_int(GLOB_TILDE));
  // signal actions
  cdf(interp, "SIG_TRAP",	pure_int(1));
  cdf(interp, "SIG_IGN",	pure_int(-1));
  cdf(interp, "SIG_DFL",	pure_int(0));
  // signals
#ifdef SIGHUP
  cdf(interp, "SIGHUP",		pure_int(SIGHUP));
#endif
#ifdef SIGINT
  cdf(interp, "SIGINT",		pure_int(SIGINT));
#endif
#ifdef SIGQUIT
  cdf(interp, "SIGQUIT",	pure_int(SIGQUIT));
#endif
#ifdef SIGILL
  cdf(interp, "SIGILL",		pure_int(SIGILL));
#endif
#ifdef SIGABRT
  cdf(interp, "SIGABRT",	pure_int(SIGABRT));
#endif
#ifdef SIGFPE
  cdf(interp, "SIGFPE",		pure_int(SIGFPE));
#endif
#ifdef SIGKILL
  cdf(interp, "SIGKILL",	pure_int(SIGKILL));
#endif
#ifdef SIGSEGV
  cdf(interp, "SIGSEGV",	pure_int(SIGSEGV));
#endif
#ifdef SIGPIPE
  cdf(interp, "SIGPIPE",	pure_int(SIGPIPE));
#endif
#ifdef SIGALRM
  cdf(interp, "SIGALRM",	pure_int(SIGALRM));
#endif
#ifdef SIGTERM
  cdf(interp, "SIGTERM",	pure_int(SIGTERM));
#endif
#ifdef SIGUSR1
  cdf(interp, "SIGUSR1",	pure_int(SIGUSR1));
#endif
#ifdef SIGUSR2
  cdf(interp, "SIGUSR2",	pure_int(SIGUSR2));
#endif
#ifdef SIGCHLD
  cdf(interp, "SIGCHLD",	pure_int(SIGCHLD));
#endif
#ifdef SIGCONT
  cdf(interp, "SIGCONT",	pure_int(SIGCONT));
#endif
#ifdef SIGSTOP
  cdf(interp, "SIGSTOP",	pure_int(SIGSTOP));
#endif
#ifdef SIGTSTP
  cdf(interp, "SIGTSTP",	pure_int(SIGTSTP));
#endif
#ifdef SIGTTIN
  cdf(interp, "SIGTTIN",	pure_int(SIGTTIN));
#endif
#ifdef SIGTTOU
  cdf(interp, "SIGTTOU",	pure_int(SIGTTOU));
#endif
  // setlocale
#ifdef LC_ALL
  cdf(interp, "LC_ALL",		pure_int(LC_ALL));
#endif
#ifdef LC_COLLATE
  cdf(interp, "LC_COLLATE",	pure_int(LC_COLLATE));
#endif
#ifdef LC_CTYPE
  cdf(interp, "LC_CTYPE",	pure_int(LC_CTYPE));
#endif
#ifdef LC_MESSAGES
  cdf(interp, "LC_MESSAGES",	pure_int(LC_MESSAGES));
#endif
#ifdef LC_MONETARY
  cdf(interp, "LC_MONETARY",	pure_int(LC_MONETARY));
#endif
#ifdef LC_NUMERIC
  cdf(interp, "LC_NUMERIC",	pure_int(LC_NUMERIC));
#endif
#ifdef LC_TIME
  cdf(interp, "LC_TIME",	pure_int(LC_TIME));
#endif
#ifdef SEEK_SET
  cdf(interp, "SEEK_SET",	pure_int(SEEK_SET));
#endif
#ifdef SEEK_CUR
  cdf(interp, "SEEK_CUR",	pure_int(SEEK_CUR));
#endif
#ifdef SEEK_END
  cdf(interp, "SEEK_END",	pure_int(SEEK_END));
#endif
#ifdef BUFSIZ
  cdf(interp, "BUFSIZ",		pure_int(BUFSIZ));
#endif
#ifdef _IONBF
  cdf(interp, "_IONBF",		pure_int(_IONBF));
#endif
#ifdef _IOLBF
  cdf(interp, "_IOLBF",		pure_int(_IOLBF));
#endif
#ifdef _IOFBF
  cdf(interp, "_IOFBF",		pure_int(_IOFBF));
#endif
  // file type bits
#ifdef S_IFMT
  cdf(interp, "S_IFMT",		pure_int(S_IFMT));
#endif
#ifdef S_IFBLK
  cdf(interp, "S_IFBLK",	pure_int(S_IFBLK));
#endif
#ifdef S_IFCHR
  cdf(interp, "S_IFCHR",	pure_int(S_IFCHR));
#endif
#ifdef S_IFIFO
  cdf(interp, "S_IFIFO",	pure_int(S_IFIFO));
#endif
#ifdef S_IFREG
  cdf(interp, "S_IFREG",	pure_int(S_IFREG));
#endif
#ifdef S_IFDIR
  cdf(interp, "S_IFDIR",	pure_int(S_IFDIR));
#endif
#ifdef S_IFLNK
  cdf(interp, "S_IFLNK",	pure_int(S_IFLNK));
#endif
#ifdef S_IFSOCK
  cdf(interp, "S_IFSOCK",	pure_int(S_IFSOCK));
#endif
#ifdef S_ISUID
  cdf(interp, "S_ISUID",	pure_int(S_ISUID));
#endif
  // file permission bits
#ifdef S_ISGID
  cdf(interp, "S_ISGID",	pure_int(S_ISGID));
#endif
#ifdef S_ISVTX
  cdf(interp, "S_ISVTX",	pure_int(S_ISVTX));
#endif
#ifdef S_IRWXU
  cdf(interp, "S_IRWXU",	pure_int(S_IRWXU));
#endif
#ifdef S_IRUSR
  cdf(interp, "S_IRUSR",	pure_int(S_IRUSR));
#endif
#ifdef S_IWUSR
  cdf(interp, "S_IWUSR",	pure_int(S_IWUSR));
#endif
#ifdef S_IXUSR
  cdf(interp, "S_IXUSR",	pure_int(S_IXUSR));
#endif
#ifdef S_IRWXG
  cdf(interp, "S_IRWXG",	pure_int(S_IRWXG));
#endif
#ifdef S_IRGRP
  cdf(interp, "S_IRGRP",	pure_int(S_IRGRP));
#endif
#ifdef S_IWGRP
  cdf(interp, "S_IWGRP",	pure_int(S_IWGRP));
#endif
#ifdef S_IXGRP
  cdf(interp, "S_IXGRP",	pure_int(S_IXGRP));
#endif
#ifdef S_IRWXO
  cdf(interp, "S_IRWXO",	pure_int(S_IRWXO));
#endif
#ifdef S_IROTH
  cdf(interp, "S_IROTH",	pure_int(S_IROTH));
#endif
#ifdef S_IWOTH
  cdf(interp, "S_IWOTH",	pure_int(S_IWOTH));
#endif
#ifdef S_IXOTH
  cdf(interp, "S_IXOTH",	pure_int(S_IXOTH));
#endif
  // open/fcntl flags
#ifdef O_ACCMODE
  cdf(interp, "O_ACCMODE",	pure_int(O_ACCMODE));
#endif
#ifdef O_RDONLY
  cdf(interp, "O_RDONLY",	pure_int(O_RDONLY));
#endif
#ifdef O_WRONLY
  cdf(interp, "O_WRONLY",	pure_int(O_WRONLY));
#endif
#ifdef O_RDWR
  cdf(interp, "O_RDWR",	pure_int(O_RDWR));
#endif
#ifdef O_CREAT
  cdf(interp, "O_CREAT",	pure_int(O_CREAT));
#endif
#ifdef O_EXCL
  cdf(interp, "O_EXCL",	pure_int(O_EXCL));
#endif
#ifdef O_NOCTTY
  cdf(interp, "O_NOCTTY",	pure_int(O_NOCTTY));
#endif
#ifdef O_TRUNC
  cdf(interp, "O_TRUNC",	pure_int(O_TRUNC));
#endif
#ifdef O_APPEND
  cdf(interp, "O_APPEND",	pure_int(O_APPEND));
#endif
#ifdef O_NONBLOCK
  cdf(interp, "O_NONBLOCK",	pure_int(O_NONBLOCK));
#endif
#ifdef O_NDELAY
  cdf(interp, "O_NDELAY",	pure_int(O_NDELAY));
#endif
#ifdef O_SYNC
  cdf(interp, "O_SYNC",		pure_int(O_SYNC));
#endif
#ifdef O_FSYNC
  cdf(interp, "O_FSYNC",	pure_int(O_FSYNC));
#endif
#ifdef O_ASYNC
  cdf(interp, "O_ASYNC",	pure_int(O_ASYNC));
#endif
#ifdef FD_CLOEXEC
  cdf(interp, "FD_CLOEXEC",	pure_int(FD_CLOEXEC));
#endif
#ifdef F_DUPFD
  cdf(interp, "F_DUPFD",	pure_int(F_DUPFD));
#endif
#ifdef F_GETFD
  cdf(interp, "F_GETFD",	pure_int(F_GETFD));
#endif
#ifdef F_SETFD
  cdf(interp, "F_SETFD",	pure_int(F_SETFD));
#endif
#ifdef F_GETFL
  cdf(interp, "F_GETFL",	pure_int(F_GETFL));
#endif
#ifdef F_SETFL
  cdf(interp, "F_SETFL",	pure_int(F_SETFL));
#endif
#ifdef F_GETLK
  cdf(interp, "F_GETLK",	pure_int(F_GETLK));
#endif
#ifdef F_SETLK
  cdf(interp, "F_SETLK",	pure_int(F_SETLK));
#endif
#ifdef F_SETLKW
  cdf(interp, "F_SETLKW",	pure_int(F_SETLKW));
#endif
#ifdef F_RDLCK
  cdf(interp, "F_RDLCK",	pure_int(F_RDLCK));
#endif
#ifdef F_WRLCK
  cdf(interp, "F_WRLCK",	pure_int(F_WRLCK));
#endif
#ifdef F_UNLCK
  cdf(interp, "F_UNLCK",	pure_int(F_UNLCK));
#endif
  // wait flags
#ifdef WNOHANG
  cdf(interp, "WNOHANG",	pure_int(WNOHANG));
#endif
#ifdef WUNTRACED
  cdf(interp, "WUNTRACED",	pure_int(WUNTRACED));
#endif
#ifdef WCONTINUED
  cdf(interp, "WCONTINUED",	pure_int(WCONTINUED));
#endif
  // spawnv flags
#ifdef P_WAIT
  cdf(interp, "P_WAIT",		pure_int(P_WAIT));
#endif
#ifdef P_NOWAIT
  cdf(interp, "P_NOWAIT",	pure_int(P_NOWAIT));
#endif
#ifdef P_OVERLAY
  cdf(interp, "P_OVERLAY",	pure_int(P_OVERLAY));
#endif
#ifdef P_DETACH
  cdf(interp, "P_DETACH",	pure_int(P_DETACH));
#endif
}

extern "C"
void pure_regex_vars(void)
{
  interpreter& interp = *interpreter::g_interp;
  // regex stuff
#if USE_PCRE
  // This sets the pcre_version variable if the runtime is built with PCRE
  // support.
  char buf[100];
  sprintf(buf, "%d.%d", PCRE_MAJOR, PCRE_MINOR);
  df(interp, "pcre_version",	pure_cstring_dup(buf));
#endif
  // regcomp flags
  df(interp, "REG_EXTENDED",	pure_int(REG_EXTENDED));
  df(interp, "REG_ICASE",	pure_int(REG_ICASE));
  df(interp, "REG_NOSUB",	pure_int(REG_NOSUB));
  df(interp, "REG_NEWLINE",	pure_int(REG_NEWLINE));
  // regexec flags
  df(interp, "REG_NOTBOL",	pure_int(REG_NOTBOL));
  df(interp, "REG_NOTEOL",	pure_int(REG_NOTEOL));
  // regcomp error codes
  df(interp, "REG_BADBR",	pure_int(REG_BADBR));
  df(interp, "REG_BADPAT",	pure_int(REG_BADPAT));
  df(interp, "REG_BADRPT",	pure_int(REG_BADRPT));
  df(interp, "REG_ECOLLATE",	pure_int(REG_ECOLLATE));
  df(interp, "REG_ECTYPE",	pure_int(REG_ECTYPE));
  df(interp, "REG_EESCAPE",	pure_int(REG_EESCAPE));
  df(interp, "REG_ESUBREG",	pure_int(REG_ESUBREG));
  df(interp, "REG_EBRACK",	pure_int(REG_EBRACK));
  df(interp, "REG_EPAREN",	pure_int(REG_EPAREN));
  df(interp, "REG_EBRACE",	pure_int(REG_EBRACE));
  df(interp, "REG_ERANGE",	pure_int(REG_ERANGE));
  df(interp, "REG_ESPACE",	pure_int(REG_ESPACE));
  // regexec error codes
  df(interp, "REG_NOMATCH",	pure_int(REG_NOMATCH));
}

/* Optimized matrix functions contributed by Scott E. Dillard. */

namespace matrix {
//some templates to ease specialization of matrix functions

//the element type of a matrix
template <typename t> struct element_of {};
template <> struct element_of<gsl_matrix> { typedef double type; };
template <> struct element_of<gsl_matrix_int> { typedef int type; };
template <> struct element_of<gsl_matrix_complex> { typedef Complex type; };
template <> struct element_of<gsl_matrix_symbolic>
{ typedef pure_expr* type; };

//create a matrix
template <typename t> t* create_matrix(size_t n,size_t m);
template <> gsl_matrix* create_matrix<gsl_matrix>(size_t n, size_t m)
{ return create_double_matrix(n,m); }
template <> gsl_matrix_int* create_matrix<gsl_matrix_int>(size_t n, size_t m)
{ return create_int_matrix(n,m); }
template <> gsl_matrix_complex* create_matrix<gsl_matrix_complex>
(size_t n, size_t m) { return create_complex_matrix(n,m); }
template <> gsl_matrix_symbolic* create_matrix<gsl_matrix_symbolic>
(size_t n, size_t m) { return create_symbolic_matrix(n,m); }

//free a matrix
void matrix_free(gsl_matrix *m) { gsl_matrix_free(m); }
void matrix_free(gsl_matrix_int *m) { gsl_matrix_int_free(m); }
void matrix_free(gsl_matrix_complex *m) { gsl_matrix_complex_free(m); }
void matrix_free(gsl_matrix_symbolic *m) { gsl_matrix_symbolic_free(m); }

//convert a matrix element into an expression
static inline pure_expr* to_expr(double & d) { return pure_double(d); }
static inline pure_expr* to_expr(int32_t i) { return pure_int(i); }
static inline pure_expr* to_expr(Complex & c)
{
  return make_complex(c.real(), c.imag());
}
static inline pure_expr* to_expr(pure_expr* e) { return e; }


//convert an expression into a matrix element
inline bool from_expr( pure_expr* e, double & d )
{ return pure_is_double(e,&d); }
inline bool from_expr( pure_expr* e, int32_t & i )
{ return pure_is_int(e,&i); }
inline bool from_expr( pure_expr* e, Complex & c )
{
  double a, b;
  if (get_complex(e, a, b)) {
    c = Complex(a, b);
    return true;
  } else
    return false;
}


// generix matrix do
template <typename matrix_type>
void matrix_do( pure_expr *f, pure_expr *x ) {
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  typedef typename element_of<matrix_type>::type elem_type;

  for (size_t i=0; i<xm->size1; ++i) {
    //pi is input read head
    elem_type *pi = reinterpret_cast<elem_type*>(xm->data)+i*xm->tda;
    for (size_t j=0; j<xm->size2; ++j,++pi) {
      pure_expr *y = pure_app( f, to_expr(*pi) );
      pure_freenew(y);
    }
  }
}


//numeric map loop : optmize common case that the output of the mapped
//function is all of the same numerical type. If not, fall back to symbolic
//map loop

template < typename in_mat_type,
           typename out_mat_type >
pure_expr*
numeric_map_loop
( pure_expr *f,       //the function being mapped
  in_mat_type *in,    //the operand matrix
  out_mat_type *out,  //the result matrix
  size_t *lasti,      //last row and column written to
  size_t *lastj )     //  before bailing out in the event that
                      //  the output of f is not the right type
{
  typedef typename element_of< in_mat_type>::type  in_elem_type;
  typedef typename element_of<out_mat_type>::type out_elem_type;

  //need casts for complex case, when data pointer is double*, but
  //we really want complex*
  in_elem_type  *inp  = (reinterpret_cast<in_elem_type* >(in->data ))+1;
  out_elem_type *outp = (reinterpret_cast<out_elem_type*>(out->data))+1;
  pure_expr *r; //result expression
  out_elem_type v; //result value

  //we already performed the first map, so now we apply f to all but the first
  //column in the first row..
  *lasti=0;
  for (size_t j=1; j<in->size2; ++j,++inp,++outp) {
    *lastj=j;
    r = pure_app( f, to_expr(*inp) );
    if (!from_expr(r,v)) return r;
    *outp = v;
    pure_freenew(r);
  }

  //..then the rest of the rows
  for (size_t i=1; i<in->size1; ++i) {
    *lasti=i;
    inp  = (reinterpret_cast< in_elem_type*>( in->data))+i*in->tda;
    outp = (reinterpret_cast<out_elem_type*>(out->data))+i*out->tda;
    for (size_t j=0; j<in->size2; ++j,++inp,++outp) {
      *lastj=j;
      r = pure_app( f, to_expr(*inp) );
      if (!from_expr(r,v)) return r;
      *outp = v;
      pure_freenew(r);
    }
  }
  return 0;
}


//symbolic map loop : mapped function results in heterogenous / non-numerical
//types. This function may pick up where numerical_map_loop left off, in that
//case copying out the already-evaluated numbers from the matrix 'num'

template < typename in_mat_type,  //the operand matrix type
           typename num_mat_type >
void
symbolic_map_loop
( pure_expr *f,            //the function being mapped
  in_mat_type *in,         //the operand matrix

  num_mat_type *num,       //the numerical matrix that _would_ have been
                           //  the output if the results of f had all been of
                           //  the same numerical type

  gsl_matrix_symbolic *out,//the symbolic matrix that will be the end result

  size_t lasti ,           //some of the map may already be evaluated.
  size_t lastj ,           //  lasti,lastj was the last element mapped
  pure_expr *last )        //  and last is the result
{
  typedef typename element_of< in_mat_type>::type  in_elem_type;
  typedef typename element_of<num_mat_type>::type num_elem_type;

  in_elem_type *inp = 0;
  pure_expr **outp = 0;
  num_elem_type *nump = 0;

  //copy the already-evaluated stuff out of num
  if (lasti>0 || lastj>0) {
    if (lasti>0) {
      for (size_t i=0; i<lasti; ++i) {
        outp = out->data + i*out->tda;
        nump = (reinterpret_cast<num_elem_type*>(num->data)) + i*num->tda;
        for (size_t j=0; j<in->size2; ++j,++outp,++nump) {
          *outp = to_expr(*nump);
        }
      }
    }
    if (lastj>0) {
      outp = out->data + lasti*out->tda;
      nump = (reinterpret_cast<num_elem_type*>(num->data)) + lasti*num->tda;
      for (size_t j=0; j<lastj; ++j,++outp,++nump) {
        *outp = to_expr(*nump);
      }
    }
  }

  out->data[lasti*out->tda+lastj] = last;
  lastj++;
  if (lastj>=out->size2) {
    lasti++;
    lastj=0;
    if (lasti>=out->size1)
      return;
  }

  inp  = (reinterpret_cast<in_elem_type*>(in->data))+lasti*in->tda+lastj;
  outp = out->data+lasti*out->tda+lastj;
  //finish the map
  for (size_t j=lastj; j<in->size2; ++j,++inp,++outp) {
    *outp = pure_app(f,to_expr(*inp));
  }

  for (size_t i=lasti+1; i<in->size1; ++i) {
    inp =  (reinterpret_cast<in_elem_type*>(in->data))+i*in->tda;
    outp = out->data+i*out->tda;
    for (size_t j=0; j<in->size2; ++j,++inp,++outp) {
      *outp = pure_app(f,to_expr(*inp));
    }
  }
}


//generic matrix map, dispatches on input matrix type (the template parameter)
//and output matrix type, which is guessed from the result type of f (x!0).
//The function speculates that all f(x!i) will be of the same type for all i.
//If not, numeric_map_loop bails out and symbolic_map_loop takes over.

template <typename matrix_type>
pure_expr* matrix_map( pure_expr *f, pure_expr *x )
{
  pure_ref(f);
  pure_ref(x);
  typedef typename element_of<matrix_type>::type elem_type;
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);

  if (xm->size1 == 0 || xm->size2 == 0) {
    // empty output matrix
    gsl_matrix_symbolic *sm = create_symbolic_matrix(xm->size1,xm->size2);
    pure_expr *out = pure_symbolic_matrix(sm);
    pure_unref(f);
    pure_unref(x);
    return out;
  }

  //need casts for complex case, when data pointer is double*, but
  //we really want complex*
  elem_type *inp = reinterpret_cast<elem_type* >(xm->data);
  //make a guess at what the result type is by the first element
  pure_expr *first = pure_app(f,to_expr(*inp));
  int32_t firsti;
  double firstd;
  Complex firstc;

  size_t lasti=0,lastj=0;
  pure_expr *out=0; //final result

  if (from_expr(first,firstd)) {
    //DOUBLE
    gsl_matrix *dm = create_double_matrix(xm->size1,xm->size2);
    dm->data[0] = firstd;
    pure_expr *last = numeric_map_loop(f,xm,dm,&lasti,&lastj);
    if (!last) {
      out = pure_double_matrix(dm);
    } else {
      gsl_matrix_symbolic *sm = create_symbolic_matrix(xm->size1,xm->size2);
      symbolic_map_loop(f,xm,dm,sm,lasti,lastj,last);
      gsl_matrix_free(dm);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(first,firsti)) {
    //INT
    gsl_matrix_int *im = create_int_matrix(xm->size1,xm->size2);
    im->data[0] = firsti;
    pure_expr *last = numeric_map_loop(f,xm,im,&lasti,&lastj);
    if (!last) {
      out = pure_int_matrix(im);
    } else {
      gsl_matrix_symbolic *sm = create_symbolic_matrix(xm->size1,xm->size2);
      symbolic_map_loop(f,xm,im,sm,lasti,lastj,last);
      gsl_matrix_int_free(im);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(first,firstc)) {
    //COMPLEX
    gsl_matrix_complex *cm = create_complex_matrix(xm->size1,xm->size2);
    reinterpret_cast<Complex*>(cm->data)[0] = firstc;
    pure_expr *last = numeric_map_loop(f,xm,cm,&lasti,&lastj);
    if (!last) {
      out = pure_complex_matrix(cm);
    } else {
      gsl_matrix_symbolic *sm = create_symbolic_matrix(xm->size1,xm->size2);
      symbolic_map_loop(f,xm,cm,sm,lasti,lastj,last);
      gsl_matrix_complex_free(cm);
      out = pure_symbolic_matrix(sm);
    }
  } else
  { //for anything else, default to symbolic.
    gsl_matrix_symbolic *sm = create_symbolic_matrix(xm->size1,xm->size2);
    symbolic_map_loop(f,xm,static_cast<matrix_type*>(0),sm,0,0,first);
    out = pure_symbolic_matrix(sm);
  }

  if (first->refc==0) pure_freenew(first);
  pure_unref(f);
  pure_unref(x);
  return out;
}


//numeric zipwith loop : optmize common case that the output of the zipped
//function is all of the same numerical type. If not, fall back to symbolic
//zipwith loop

template < typename in1_mat_type,
           typename in2_mat_type,
           typename out_mat_type >
pure_expr*
numeric_zipwith_loop
( pure_expr *f,       //the function being zipped
  in1_mat_type *in1,   //the 1st operand matrix
  in2_mat_type *in2,   //the 2nd operand matrix
  out_mat_type *out,  //the result matrix
  size_t *lasti,      //last row and column written to
  size_t *lastj )     //  before bailing out in the event that
                      //  the output of f is not the right type
{
  typedef typename element_of<in1_mat_type>::type in1_elem_type;
  typedef typename element_of<in2_mat_type>::type in2_elem_type;
  typedef typename element_of<out_mat_type>::type out_elem_type;

  //need casts for complex case, when data pointer is double*, but
  //we really want complex*
  in1_elem_type *inp1 = (reinterpret_cast<in1_elem_type*>(in1->data ))+1;
  in2_elem_type *inp2 = (reinterpret_cast<in2_elem_type*>(in2->data ))+1;
  out_elem_type *outp = (reinterpret_cast<out_elem_type*>(out->data))+1;
  pure_expr *r; //result expression
  out_elem_type v; //result value

  //we already performed the first zipwith, so now we apply f to all but the
  //first column in the first row..
  *lasti=0;
  for (size_t j=1; j<in1->size2 && j<in2->size2; ++j,++inp1,++inp2,++outp) {
    *lastj=j;
    r = pure_appl( f, 2, to_expr(*inp1), to_expr(*inp2) );
    if (!from_expr(r,v)) return r;
    *outp = v;
    pure_freenew(r);
  }

  //..then the rest of the rows
  for (size_t i=1; i<in1->size1 && i<in2->size1; ++i) {
    *lasti=i;
    inp1 = (reinterpret_cast<in1_elem_type*>(in1->data))+i*in1->tda;
    inp2 = (reinterpret_cast<in2_elem_type*>(in2->data))+i*in2->tda;
    outp = (reinterpret_cast<out_elem_type*>(out->data))+i*out->tda;
    for (size_t j=0; j<in1->size2 && j<in2->size2; ++j,++inp1,++inp2,++outp) {
      *lastj=j;
      r = pure_appl( f, 2, to_expr(*inp1), to_expr(*inp2) );
      if (!from_expr(r,v)) return r;
      *outp = v;
      pure_freenew(r);
    }
  }
  return 0;
}


//symbolic zipwith loop : zipped function results in heterogenous /
//non-numerical types. This function may pick up where numerical_zipwith_loop
//left off, in that case copying out the already-evaluated numbers from the
//matrix 'num'

template < typename in1_mat_type,  //the operand matrix type
           typename in2_mat_type,
           typename num_mat_type >
void
symbolic_zipwith_loop
( pure_expr *f,            //the function being zipped
  in1_mat_type *in1,        //the 1st operand matrix
  in2_mat_type *in2,        //the 2nd operand matrix

  num_mat_type *num,       //the numerical matrix that _would_ have been
                           //  the output if the results of f had all been of
                           //  the same numerical type

  gsl_matrix_symbolic *out,//the symbolic matrix that will be the end result

  size_t lasti ,           //some of the zipwith may already be evaluated.
  size_t lastj ,           //  lasti,lastj was the last element zipped
  pure_expr *last )        //  and last is the result
{
  typedef typename element_of<in1_mat_type>::type in1_elem_type;
  typedef typename element_of<in2_mat_type>::type in2_elem_type;
  typedef typename element_of<num_mat_type>::type num_elem_type;

  in1_elem_type *inp1 = 0;
  in2_elem_type *inp2 = 0;
  pure_expr **outp = 0;
  num_elem_type *nump = 0;

  //copy the already-evaluated stuff out of num
  if (lasti>0 || lastj>0) {
    if (lasti>0) {
      for (size_t i=0; i<lasti; ++i) {
        outp = out->data + i*out->tda;
        nump = (reinterpret_cast<num_elem_type*>(num->data)) + i*num->tda;
        for (size_t j=0; j<in1->size2 && j<in2->size2; ++j,++outp,++nump) {
          *outp = to_expr(*nump);
        }
      }
    }
    if (lastj>0) {
      outp = out->data + lasti*out->tda;
      nump = (reinterpret_cast<num_elem_type*>(num->data)) + lasti*num->tda;
      for (size_t j=0; j<lastj; ++j,++outp,++nump) {
        *outp = to_expr(*nump);
      }
    }
  }

  out->data[lasti*out->tda+lastj] = last;
  lastj++;
  if (lastj>=out->size2) {
    lasti++;
    lastj=0;
    if (lasti>=out->size1)
      return;
  }

  inp1 = (reinterpret_cast<in1_elem_type*>(in1->data))+lasti*in1->tda+lastj;
  inp2 = (reinterpret_cast<in2_elem_type*>(in2->data))+lasti*in2->tda+lastj;
  outp = out->data+lasti*out->tda+lastj;
  //finish the zipwith
  for (size_t j=lastj; j<in1->size2 && j<in2->size2; ++j,++inp1,++inp2,++outp)
  {
    *outp = pure_appl(f,2,to_expr(*inp1),to_expr(*inp2));
  }

  for (size_t i=lasti+1; i<in1->size1 && i<in2->size1; ++i) {
    inp1 =  (reinterpret_cast<in1_elem_type*>(in1->data))+i*in1->tda;
    inp2 =  (reinterpret_cast<in2_elem_type*>(in2->data))+i*in2->tda;
    outp = out->data+i*out->tda;
    for (size_t j=0; j<in1->size2 && j<in2->size2; ++j,++inp1,++inp2,++outp) {
      *outp = pure_appl(f,2,to_expr(*inp1),to_expr(*inp2));
    }
  }
}


//generic matrix zipwith, dispatches on input matrix type (the template
//parameters) and output matrix type, which is guessed from the result type of
//f (x!0) (y!0). The function speculates that all f (x!i) (y!i) will be of the
//same type for all i. If not, numeric_zipwith_loop bails out and
//symbolic_zipwith_loop takes over.

template <typename matrix1_type, typename matrix2_type>
pure_expr* matrix_zipwith( pure_expr *f, pure_expr *x, pure_expr *y )
{
  pure_ref(f);
  pure_ref(x);
  pure_ref(y);
  typedef typename element_of<matrix1_type>::type elem1_type;
  typedef typename element_of<matrix2_type>::type elem2_type;
  matrix1_type *xm = static_cast<matrix1_type*>(x->data.mat.p);
  matrix2_type *ym = static_cast<matrix2_type*>(y->data.mat.p);

  size_t size1 = xm->size1, size2 = xm->size2;
  if (size1 > ym->size1) size1 = ym->size1;
  if (size2 > ym->size2) size2 = ym->size2;

  if (size1 == 0 || size2 == 0) {
    // empty output matrix
    gsl_matrix_symbolic *sm = create_symbolic_matrix(size1,size2);
    pure_expr *out = pure_symbolic_matrix(sm);
    pure_unref(f);
    pure_unref(x);
    pure_unref(y);
    return out;
  }

  //need casts for complex case, when data pointer is double*, but
  //we really want complex*
  elem1_type *inp1 = reinterpret_cast<elem1_type* >(xm->data);
  elem2_type *inp2 = reinterpret_cast<elem2_type* >(ym->data);
  //make a guess at what the result type is by the first element
  pure_expr *first = pure_appl(f,2,to_expr(*inp1),to_expr(*inp2));
  int32_t firsti;
  double firstd;
  Complex firstc;

  size_t lasti=0,lastj=0;
  pure_expr *out=0; //final result

  if (from_expr(first,firstd)) {
    //DOUBLE
    gsl_matrix *dm = create_double_matrix(size1,size2);
    dm->data[0] = firstd;
    pure_expr *last = numeric_zipwith_loop(f,xm,ym,dm,&lasti,&lastj);
    if (!last) {
      out = pure_double_matrix(dm);
    } else {
      gsl_matrix_symbolic *sm = create_symbolic_matrix(size1,size2);
      symbolic_zipwith_loop(f,xm,ym,dm,sm,lasti,lastj,last);
      gsl_matrix_free(dm);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(first,firsti)) {
    //INT
    gsl_matrix_int *im = create_int_matrix(size1,size2);
    im->data[0] = firsti;
    pure_expr *last = numeric_zipwith_loop(f,xm,ym,im,&lasti,&lastj);
    if (!last) {
      out = pure_int_matrix(im);
    } else {
      gsl_matrix_symbolic *sm = create_symbolic_matrix(size1,size2);
      symbolic_zipwith_loop(f,xm,ym,im,sm,lasti,lastj,last);
      gsl_matrix_int_free(im);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(first,firstc)) {
    //COMPLEX
    gsl_matrix_complex *cm = create_complex_matrix(size1,size2);
    reinterpret_cast<Complex*>(cm->data)[0] = firstc;
    pure_expr *last = numeric_zipwith_loop(f,xm,ym,cm,&lasti,&lastj);
    if (!last) {
      out = pure_complex_matrix(cm);
    } else {
      gsl_matrix_symbolic *sm = create_symbolic_matrix(size1,size2);
      symbolic_zipwith_loop(f,xm,ym,cm,sm,lasti,lastj,last);
      gsl_matrix_complex_free(cm);
      out = pure_symbolic_matrix(sm);
    }
  } else
  { //for anything else, default to symbolic.
    gsl_matrix_symbolic *sm = create_symbolic_matrix(size1,size2);
    symbolic_zipwith_loop(f,xm,ym,static_cast<matrix1_type*>(0),sm,0,0,first);
    out = pure_symbolic_matrix(sm);
  }

  if (first->refc==0) pure_freenew(first);
  pure_unref(f);
  pure_unref(x);
  pure_unref(y);
  return out;
}


//numeric zipwith3 loop : optmize common case that the output of the zipped
//function is all of the same numerical type. If not, fall back to symbolic
//zipwith3 loop

template < typename in1_mat_type,
           typename in2_mat_type,
           typename in3_mat_type,
           typename out_mat_type >
pure_expr*
numeric_zipwith3_loop
( pure_expr *f,       //the function being zipped
  in1_mat_type *in1,   //the 1st operand matrix
  in2_mat_type *in2,   //the 2nd operand matrix
  in3_mat_type *in3,   //the 3rd operand matrix
  out_mat_type *out,  //the result matrix
  size_t *lasti,      //last row and column written to
  size_t *lastj )     //  before bailing out in the event that
                      //  the output of f is not the right type
{
  typedef typename element_of<in1_mat_type>::type in1_elem_type;
  typedef typename element_of<in2_mat_type>::type in2_elem_type;
  typedef typename element_of<in3_mat_type>::type in3_elem_type;
  typedef typename element_of<out_mat_type>::type out_elem_type;

  //need casts for complex case, when data pointer is double*, but
  //we really want complex*
  in1_elem_type *inp1 = (reinterpret_cast<in1_elem_type*>(in1->data ))+1;
  in2_elem_type *inp2 = (reinterpret_cast<in2_elem_type*>(in2->data ))+1;
  in3_elem_type *inp3 = (reinterpret_cast<in3_elem_type*>(in3->data ))+1;
  out_elem_type *outp = (reinterpret_cast<out_elem_type*>(out->data))+1;
  pure_expr *r; //result expression
  out_elem_type v; //result value

  //we already performed the first zipwith3, so now we apply f to all but the
  //first column in the first row..
  *lasti=0;
  for (size_t j=1; j<in1->size2 && j<in2->size2 && j<in3->size2;
       ++j,++inp1,++inp2,++inp3,++outp) {
    *lastj=j;
    r = pure_appl( f, 3, to_expr(*inp1), to_expr(*inp2), to_expr(*inp3) );
    if (!from_expr(r,v)) return r;
    *outp = v;
    pure_freenew(r);
  }

  //..then the rest of the rows
  for (size_t i=1; i<in1->size1 && i<in2->size1 && i<in3->size1; ++i) {
    *lasti=i;
    inp1 = (reinterpret_cast<in1_elem_type*>(in1->data))+i*in1->tda;
    inp2 = (reinterpret_cast<in2_elem_type*>(in2->data))+i*in2->tda;
    inp3 = (reinterpret_cast<in3_elem_type*>(in3->data))+i*in3->tda;
    outp = (reinterpret_cast<out_elem_type*>(out->data))+i*out->tda;
    for (size_t j=0; j<in1->size2 && j<in2->size2 && j<in3->size2;
	 ++j,++inp1,++inp2,++inp3,++outp) {
      *lastj=j;
      r = pure_appl( f, 3, to_expr(*inp1), to_expr(*inp2), to_expr(*inp3) );
      if (!from_expr(r,v)) return r;
      *outp = v;
      pure_freenew(r);
    }
  }
  return 0;
}


//symbolic zipwith3 loop : zipped function results in heterogenous /
//non-numerical types. This function may pick up where numerical_zipwith3_loop
//left off, in that case copying out the already-evaluated numbers from the
//matrix 'num'

template < typename in1_mat_type,  //the operand matrix type
           typename in2_mat_type,
           typename in3_mat_type,
           typename num_mat_type >
void
symbolic_zipwith3_loop
( pure_expr *f,            //the function being zipped
  in1_mat_type *in1,        //the 1st operand matrix
  in2_mat_type *in2,        //the 2nd operand matrix
  in3_mat_type *in3,        //the 3rd operand matrix

  num_mat_type *num,       //the numerical matrix that _would_ have been
                           //  the output if the results of f had all been of
                           //  the same numerical type

  gsl_matrix_symbolic *out,//the symbolic matrix that will be the end result

  size_t lasti ,           //some of the zipwith3 may already be evaluated.
  size_t lastj ,           //  lasti,lastj was the last element zipped
  pure_expr *last )        //  and last is the result
{
  typedef typename element_of<in1_mat_type>::type in1_elem_type;
  typedef typename element_of<in2_mat_type>::type in2_elem_type;
  typedef typename element_of<in3_mat_type>::type in3_elem_type;
  typedef typename element_of<num_mat_type>::type num_elem_type;

  in1_elem_type *inp1 = 0;
  in2_elem_type *inp2 = 0;
  in3_elem_type *inp3 = 0;
  pure_expr **outp = 0;
  num_elem_type *nump = 0;

  //copy the already-evaluated stuff out of num
  if (lasti>0 || lastj>0) {
    if (lasti>0) {
      for (size_t i=0; i<lasti; ++i) {
        outp = out->data + i*out->tda;
        nump = (reinterpret_cast<num_elem_type*>(num->data)) + i*num->tda;
        for (size_t j=0; j<in1->size2 && j<in2->size2 && j<in3->size2;
	     ++j,++outp,++nump) {
          *outp = to_expr(*nump);
        }
      }
    }
    if (lastj>0) {
      outp = out->data + lasti*out->tda;
      nump = (reinterpret_cast<num_elem_type*>(num->data)) + lasti*num->tda;
      for (size_t j=0; j<lastj; ++j,++outp,++nump) {
        *outp = to_expr(*nump);
      }
    }
  }

  out->data[lasti*out->tda+lastj] = last;
  lastj++;
  if (lastj>=out->size2) {
    lasti++;
    lastj=0;
    if (lasti>=out->size1)
      return;
  }

  inp1 = (reinterpret_cast<in1_elem_type*>(in1->data))+lasti*in1->tda+lastj;
  inp2 = (reinterpret_cast<in2_elem_type*>(in2->data))+lasti*in2->tda+lastj;
  inp3 = (reinterpret_cast<in3_elem_type*>(in3->data))+lasti*in3->tda+lastj;
  outp = out->data+lasti*out->tda+lastj;
  //finish the zipwith3
  for (size_t j=lastj; j<in1->size2 && j<in2->size2 && j<in3->size2;
       ++j,++inp1,++inp2,++inp3,++outp)
  {
    *outp = pure_appl(f,3,to_expr(*inp1),to_expr(*inp2),to_expr(*inp3));
  }

  for (size_t i=lasti+1; i<in1->size1 && i<in2->size1 && i<in3->size1; ++i) {
    inp1 =  (reinterpret_cast<in1_elem_type*>(in1->data))+i*in1->tda;
    inp2 =  (reinterpret_cast<in2_elem_type*>(in2->data))+i*in2->tda;
    inp3 =  (reinterpret_cast<in3_elem_type*>(in3->data))+i*in3->tda;
    outp = out->data+i*out->tda;
    for (size_t j=0; j<in1->size2 && j<in2->size2 && j<in3->size2;
	 ++j,++inp1,++inp2,++inp3,++outp) {
      *outp = pure_appl(f,3,to_expr(*inp1),to_expr(*inp2),to_expr(*inp3));
    }
  }
}


//generic matrix zipwith3, dispatches on input matrix type (the template
//parameters) and output matrix type, which is guessed from the result type of
//f (x!0) (y!0). The function speculates that all f (x!i) (y!i) will be of the
//same type for all i. If not, numeric_zipwith3_loop bails out and
//symbolic_zipwith3_loop takes over.

template <typename matrix1_type, typename matrix2_type, typename matrix3_type>
pure_expr* matrix_zipwith3( pure_expr *f, pure_expr *x, pure_expr *y,
			    pure_expr *z )
{
  pure_ref(f);
  pure_ref(x);
  pure_ref(y);
  pure_ref(z);
  typedef typename element_of<matrix1_type>::type elem1_type;
  typedef typename element_of<matrix2_type>::type elem2_type;
  typedef typename element_of<matrix3_type>::type elem3_type;
  matrix1_type *xm = static_cast<matrix1_type*>(x->data.mat.p);
  matrix2_type *ym = static_cast<matrix2_type*>(y->data.mat.p);
  matrix3_type *zm = static_cast<matrix3_type*>(z->data.mat.p);

  size_t size1 = xm->size1, size2 = xm->size2;
  if (size1 > ym->size1) size1 = ym->size1;
  if (size2 > ym->size2) size2 = ym->size2;
  if (size1 > zm->size1) size1 = zm->size1;
  if (size2 > zm->size2) size2 = zm->size2;

  if (size1 == 0 || size2 == 0) {
    // empty output matrix
    gsl_matrix_symbolic *sm = create_symbolic_matrix(size1,size2);
    pure_expr *out = pure_symbolic_matrix(sm);
    pure_unref(f);
    pure_unref(x);
    pure_unref(y);
    pure_unref(z);
    return out;
  }

  //need casts for complex case, when data pointer is double*, but
  //we really want complex*
  elem1_type *inp1 = reinterpret_cast<elem1_type* >(xm->data);
  elem2_type *inp2 = reinterpret_cast<elem2_type* >(ym->data);
  elem3_type *inp3 = reinterpret_cast<elem3_type* >(zm->data);
  //make a guess at what the result type is by the first element
  pure_expr *first = pure_appl(f,3,to_expr(*inp1),to_expr(*inp2),
			       to_expr(*inp3));
  int32_t firsti;
  double firstd;
  Complex firstc;

  size_t lasti=0,lastj=0;
  pure_expr *out=0; //final result

  if (from_expr(first,firstd)) {
    //DOUBLE
    gsl_matrix *dm = create_double_matrix(size1,size2);
    dm->data[0] = firstd;
    pure_expr *last = numeric_zipwith3_loop(f,xm,ym,zm,dm,&lasti,&lastj);
    if (!last) {
      out = pure_double_matrix(dm);
    } else {
      gsl_matrix_symbolic *sm = create_symbolic_matrix(size1,size2);
      symbolic_zipwith3_loop(f,xm,ym,zm,dm,sm,lasti,lastj,last);
      gsl_matrix_free(dm);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(first,firsti)) {
    //INT
    gsl_matrix_int *im = create_int_matrix(size1,size2);
    im->data[0] = firsti;
    pure_expr *last = numeric_zipwith3_loop(f,xm,ym,zm,im,&lasti,&lastj);
    if (!last) {
      out = pure_int_matrix(im);
    } else {
      gsl_matrix_symbolic *sm = create_symbolic_matrix(size1,size2);
      symbolic_zipwith3_loop(f,xm,ym,zm,im,sm,lasti,lastj,last);
      gsl_matrix_int_free(im);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(first,firstc)) {
    //COMPLEX
    gsl_matrix_complex *cm = create_complex_matrix(size1,size2);
    reinterpret_cast<Complex*>(cm->data)[0] = firstc;
    pure_expr *last = numeric_zipwith3_loop(f,xm,ym,zm,cm,&lasti,&lastj);
    if (!last) {
      out = pure_complex_matrix(cm);
    } else {
      gsl_matrix_symbolic *sm = create_symbolic_matrix(size1,size2);
      symbolic_zipwith3_loop(f,xm,ym,zm,cm,sm,lasti,lastj,last);
      gsl_matrix_complex_free(cm);
      out = pure_symbolic_matrix(sm);
    }
  } else
  { //for anything else, default to symbolic.
    gsl_matrix_symbolic *sm = create_symbolic_matrix(size1,size2);
    symbolic_zipwith3_loop(f,xm,ym,zm,static_cast<matrix1_type*>(0),sm,0,0,
			   first);
    out = pure_symbolic_matrix(sm);
  }

  if (first->refc==0) pure_freenew(first);
  pure_unref(f);
  pure_unref(x);
  pure_unref(y);
  pure_unref(z);
  return out;
}


//numeric scanl loop : optmize common case that the output of the function
//is all of the same numerical type. If not, fall back to symbolic scanl loop

template < typename in_mat_type,
           typename out_mat_type >
pure_expr*
numeric_scanl_loop
( pure_expr *f,       //the function being mapped
  pure_expr *z,       //seed value
  bool init,          //1 if the seed value was taken from the matrix
  in_mat_type *in,    //the operand matrix
  out_mat_type *out,  //the result matrix
  size_t *lasti,      //last row and column written to
  size_t *lastj )     //  before bailing out in the event that
                      //  the output of f is not the right type
{
  typedef typename element_of< in_mat_type>::type  in_elem_type;
  typedef typename element_of<out_mat_type>::type out_elem_type;

  out_elem_type v;

  if (in->size1 == 0 || in->size2 == 0) return 0;

  out_elem_type *outp = (reinterpret_cast<out_elem_type*>(out->data))+1;
  pure_new(z);
  for (size_t i=0; i<in->size1; ++i) {
    *lasti=i;
    in_elem_type *inp =
      (reinterpret_cast<in_elem_type*>(in->data))+i*in->tda+init;
    for (size_t j=init; j<in->size2; ++j,++inp,++outp) {
      *lastj=j;
      pure_expr *zz = z;
      z = pure_new( pure_appl( f, 2, z, to_expr(*inp) ) );
      if (!from_expr(z,v)) {
	pure_unref(z);
	return z;
      }
      *outp = v;
      pure_free(zz);
    }
    init = 0;
  }
  pure_free(z);
  return 0;
}


//symbolic scanl loop : function results in heterogenous / non-numerical
//types. This function may pick up where numerical_scanl_loop left off, in
//that case copying out the already-evaluated numbers from the matrix 'num'

template < typename in_mat_type,  //the operand matrix type
           typename num_mat_type >
void
symbolic_scanl_loop
( pure_expr *f,            //The function being mapped
  pure_expr *z,            //The seed value OR last value produced in
                           //  numerical loop
  bool init,               //1 if the seed value was taken from the matrix
  in_mat_type *in,         //The operand matrix
  num_mat_type *num,       //The numerical matrix that _would_ have been
                           //  the output if the results of f had all been of
                           //  the same numerical type. Possibly null.
  gsl_matrix_symbolic *out,//The symbolic matrix that will be the end result
  ptrdiff_t lasti ,        //Some of the scan may already be evaluated.
  ptrdiff_t lastj )        //lasti,lastj was the last argument given to f
                              //and z was the result.
{
  typedef typename element_of< in_mat_type>::type  in_elem_type;
  typedef typename element_of<num_mat_type>::type num_elem_type;

  in_elem_type *inp = 0;
  pure_expr **outp = out->data + (!num||init?0:1);
    // num==0 indicates that z is the initial seed value

  //copy the already-evaluated stuff out of num
  if (num) {
    assert(num->size1 == 1);
    assert(num->size1 == out->size1);
    num_elem_type *nump = reinterpret_cast<num_elem_type*>(num->data);
    for (size_t j=0; j<lasti*in->size2+lastj; ++j) {
        *(outp++) = to_expr(*(nump++)) ;
    }
  }

  *(outp++) = z;
  lastj++;
  if ((size_t)lastj>=in->size2) {
    lasti++;
    lastj=0;
    if ((size_t)lasti>=in->size1)
      return;
  }

  inp = reinterpret_cast<in_elem_type*>(in->data)+lasti*in->tda+lastj;
  //finish the scan
  for (size_t j=lastj; j<in->size2; ++j) {
    pure_expr *zz = pure_new(z);
    *(outp++) = z = pure_appl(f,2,z,to_expr(*(inp++)));
    pure_unref(zz);
  }

  for (size_t i=lasti+1; i<in->size1; ++i) {
    inp = reinterpret_cast<in_elem_type*>(in->data)+i*in->tda;
    for (size_t j=0; j<in->size2; ++j) {
      pure_expr *zz = pure_new(z);
      *(outp++) = z = pure_appl(f,2,z,to_expr(*(inp++)));
      pure_unref(zz);
    }
  }

  assert(out->size1==1);
  for (size_t i=0; i<out->size2; ++i) assert(out->data[i]);
}


//Generic matrix scanl, dispatches on input matrix type (the template
//parameter) and output matrix type, which is guessed from the seed value (z).
//The function speculates that all results will be of the same type.  If not,
//numeric_scanl_loop bails out and symbolic_scanl_loop takes over.

template <typename matrix_type>
pure_expr* matrix_scanl( pure_expr *f, pure_expr *z, pure_expr *x )
{
  pure_ref(f);
  pure_ref(x);

  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  size_t lasti,lastj;
  pure_expr *out; //result matrix

  double zd;
  int32_t zi;
  Complex zc;

  if (from_expr(z,zd)) {
    gsl_matrix *dm = create_double_matrix(1,1+xm->size1*xm->size2);
    dm->data[0] = zd;
    pure_expr *last = numeric_scanl_loop(f,z,0,xm,dm,&lasti,&lastj);
    if (!last) {
      out = pure_double_matrix(dm);
    } else {
      gsl_matrix_symbolic *sm =
	create_symbolic_matrix(1,1+xm->size1*xm->size2);
      sm->data[0] = z;
      symbolic_scanl_loop(f,last,0,xm,dm,sm,lasti,lastj);
      gsl_matrix_free(dm);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(z,zi)) {
    gsl_matrix_int *im = create_int_matrix(1,1+xm->size1*xm->size2);
    im->data[0] = zi;
    pure_expr *last = numeric_scanl_loop(f,z,0,xm,im,&lasti,&lastj);
    if (!last) {
      out = pure_int_matrix(im);
    } else {
      gsl_matrix_symbolic *sm =
	create_symbolic_matrix(1,1+xm->size1*xm->size2);
      sm->data[0] = z;
      symbolic_scanl_loop(f,last,0,xm,im,sm,lasti,lastj);
      gsl_matrix_int_free(im);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(z,zc)) {
    gsl_matrix_complex *cm = create_complex_matrix(1,1+xm->size1*xm->size2);
    reinterpret_cast<Complex*>(cm->data)[0] = zc;
    pure_expr *last = numeric_scanl_loop(f,z,0,xm,cm,&lasti,&lastj);
    if (!last) {
      out = pure_complex_matrix(cm);
    } else {
      gsl_matrix_symbolic *sm =
	create_symbolic_matrix(1,1+xm->size1*xm->size2);
      sm->data[0] = z;
      symbolic_scanl_loop(f,last,0,xm,cm,sm,lasti,lastj);
      gsl_matrix_complex_free(cm);
      out = pure_symbolic_matrix(sm);
    }
  } else
  { //for anything else, default to symbolic.
      gsl_matrix_symbolic *sm =
	create_symbolic_matrix(1,1+xm->size1*xm->size2);
      symbolic_scanl_loop(f,z,0,xm,static_cast<matrix_type*>(0),sm,0,-1);
      out = pure_symbolic_matrix(sm);
  }
  pure_unref(f);
  pure_unref(x);
  return out;
}


// same for scanl1
template <typename matrix_type>
pure_expr* matrix_scanl1( pure_expr *f, pure_expr *x )
{
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  if (xm->size1 == 0 || xm->size2 == 0) {
    gsl_matrix_symbolic *sm = create_symbolic_matrix(1, 0);
    return pure_symbolic_matrix(sm);
  }
  pure_ref(f);
  pure_ref(x);

  typedef typename element_of<matrix_type>::type elem_type;
  size_t lasti,lastj;
  pure_expr *out; //result matrix

  double zd;
  int32_t zi;
  Complex zc;

  elem_type *p = reinterpret_cast<elem_type*>(xm->data);
  pure_expr *z = to_expr(*p);

  if (from_expr(z,zd)) {
    gsl_matrix *dm = create_double_matrix(1,xm->size1*xm->size2);
    dm->data[0] = zd;
    pure_expr *last = numeric_scanl_loop(f,z,1,xm,dm,&lasti,&lastj);
    if (!last) {
      out = pure_double_matrix(dm);
    } else {
      gsl_matrix_symbolic *sm =
	create_symbolic_matrix(1,xm->size1*xm->size2);
      symbolic_scanl_loop(f,last,1,xm,dm,sm,lasti,lastj);
      gsl_matrix_free(dm);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(z,zi)) {
    gsl_matrix_int *im = create_int_matrix(1,xm->size1*xm->size2);
    im->data[0] = zi;
    pure_expr *last = numeric_scanl_loop(f,z,1,xm,im,&lasti,&lastj);
    if (!last) {
      out = pure_int_matrix(im);
    } else {
      gsl_matrix_symbolic *sm =
	create_symbolic_matrix(1,xm->size1*xm->size2);
      symbolic_scanl_loop(f,last,1,xm,im,sm,lasti,lastj);
      gsl_matrix_int_free(im);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(z,zc)) {
    gsl_matrix_complex *cm = create_complex_matrix(1,xm->size1*xm->size2);
    reinterpret_cast<Complex*>(cm->data)[0] = zc;
    pure_expr *last = numeric_scanl_loop(f,z,1,xm,cm,&lasti,&lastj);
    if (!last) {
      out = pure_complex_matrix(cm);
    } else {
      gsl_matrix_symbolic *sm =
	create_symbolic_matrix(1,xm->size1*xm->size2);
      symbolic_scanl_loop(f,last,1,xm,cm,sm,lasti,lastj);
      gsl_matrix_complex_free(cm);
      out = pure_symbolic_matrix(sm);
    }
  } else
  { //for anything else, default to symbolic.
      gsl_matrix_symbolic *sm =
	create_symbolic_matrix(1,xm->size1*xm->size2);
      symbolic_scanl_loop(f,z,1,xm,static_cast<matrix_type*>(0),sm,0,0);
      out = pure_symbolic_matrix(sm);
  }
  pure_unref(f);
  pure_unref(x);
  return out;
}


//Numeric scanr loop : optmize common case that the output of the function
//is all of the same numerical type. If not, fall back to symbolic scanr loop

template < typename in_mat_type,
           typename out_mat_type >
pure_expr*
numeric_scanr_loop
( pure_expr *f,       //the function being mapped
  pure_expr *z,       //seed value
  bool init,          //1 if the seed value was taken from the matrix
  in_mat_type *in,    //the operand matrix
  out_mat_type *out,  //the result matrix
  ptrdiff_t *lasti,   //last row and column written to
  ptrdiff_t *lastj )  //  before bailing out in the event that
                      //  the output of f is not the right type
{
  typedef typename element_of< in_mat_type>::type  in_elem_type;
  typedef typename element_of<out_mat_type>::type out_elem_type;

  if (in->size1 == 0 || in->size2 == 0) return 0;

  out_elem_type v;
  out_elem_type *outp = reinterpret_cast<out_elem_type*>(out->data)+
                         in->size1*in->size2-1-init;
  pure_new(z);
  for (ptrdiff_t i=in->size1-1; i>=0; --i) {
    *lasti=i;
    in_elem_type *inp = reinterpret_cast<in_elem_type*>(in->data)+
                         i*in->tda+in->size2-1-init;
    for (ptrdiff_t j=in->size2-1-init; j>=0; --j,--inp,--outp) {
      *lastj=j;
      pure_expr *zz = z;
      z = pure_new( pure_appl( f, 2, to_expr(*inp), z ) );
      if (!from_expr(z,v)) {
	pure_unref(z);
	return z;
      }
      *outp = v;
      pure_free(zz);
    }
    init = 0;
  }
  pure_free(z);
  return 0;
}


//Symbolic scanr loop : function results in heterogenous / non-numerical
//types. This function may pick up where numerical_scanl_loop left off, in
//that case copying out the already-evaluated numbers from the matrix 'num'

template < typename in_mat_type,  //the operand matrix type
           typename num_mat_type >
void
symbolic_scanr_loop
( pure_expr *f,            //The function being mapped
  pure_expr *z,            //The seed value OR the last result of the
                           //  numerical loop.
  bool init,               //1 if the seed value was taken from the matrix
  in_mat_type *in,         //The operand matrix
  num_mat_type *num,       //The numerical matrix that would have been
                           //  the output if the results of f had all been of
                           //  the same numerical type. Possibly null.
  gsl_matrix_symbolic *out,//The symbolic matrix that will be the end result
  ptrdiff_t lasti ,        //Some of the scan may already be evaluated.
  ptrdiff_t lastj )        //lasti,lastj was the last argument given to f
                              //and z was the result.
{
  typedef typename element_of< in_mat_type>::type  in_elem_type;
  typedef typename element_of<num_mat_type>::type num_elem_type;

  in_elem_type *inp = 0;
  pure_expr **outp = out->data+in->size1*in->size2 - (!num||init?0:1) - init;
    //num==0 indicates that z is the initial seed value

  //copy the already-evaluated stuff out of num
  if (num) {
    assert(num->size1 == 1);
    assert(num->size1 == out->size1);
    num_elem_type *nump =
      reinterpret_cast<num_elem_type*>(num->data)+num->size2-1;

                         //        here we start at -2 because -1 was
                         //vvvvvvv already set before calling
    for ( size_t j=num->size2-2+init; j>lasti*in->size2+lastj; --j )
        *(outp--) = to_expr(*(nump--));
  }

  *(outp--) = z;
  lastj--;
  if (lastj == -1) {
    lasti--;
    lastj=in->size2-1;
    if (lasti == -1)
      return;
  }

  inp = reinterpret_cast<in_elem_type*>(in->data)+lasti*in->tda+lastj;
  //finish the scan
  for (ptrdiff_t j=lastj; j>=0; --j) {
    pure_expr *zz = pure_new(z);
    *(outp--) = z = pure_appl(f,2,to_expr(*(inp--)),z);
    pure_unref(zz);
  }

  for (ptrdiff_t i=lasti-1; i>=0; --i) {
    inp = reinterpret_cast<in_elem_type*>(in->data)+i*in->tda+in->size2-1;
    for (ptrdiff_t j=in->size2-1; j>=0; --j) {
      pure_expr *zz = pure_new(z);
      *(outp--) = z = pure_appl(f,2,to_expr(*(inp--)),z);
      pure_unref(zz);
    }
  }

  assert(out->size1==1);
  for (size_t i=0; i<out->size2; ++i) assert(out->data[i]);
}


//Generic matrix scanr, dispatches on input matrix type (the template
//parameter) and output matrix type, which is guessed from the seed value (z).
//The function speculates that all values will be of the same type for all i.
//If not, numeric_scanr_loop bails out and symbolic_scanr_loop takes over.

template <typename matrix_type>
pure_expr* matrix_scanr( pure_expr *f, pure_expr *z, pure_expr *x )
{
  pure_ref(f);
  pure_ref(x);

  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  ptrdiff_t lasti,lastj;
  pure_expr *out; //result matrix
  double zd;
  int32_t zi;
  Complex zc;

  if (from_expr(z,zd)) {
    //DOUBLE
    gsl_matrix *dm = create_double_matrix(1,1+xm->size1*xm->size2);
    dm->data[xm->size1*xm->size2] = zd;
    pure_expr *last = numeric_scanr_loop(f,z,0,xm,dm,&lasti,&lastj);
    if (!last) {
      out = pure_double_matrix(dm);
    } else {
      gsl_matrix_symbolic *sm =
          create_symbolic_matrix(1,1+xm->size1*xm->size2);
      sm->data[xm->size1*xm->size2] = z;
      symbolic_scanr_loop(f,last,0,xm,dm,sm,lasti,lastj);
      gsl_matrix_free(dm);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(z,zi)) {
    //INT
    gsl_matrix_int *im = create_int_matrix(1,1+xm->size1*xm->size2);
    im->data[xm->size1*xm->size2] = zi;
    pure_expr *last = numeric_scanr_loop(f,z,0,xm,im,&lasti,&lastj);
    if (!last) {
      out = pure_int_matrix(im);
    } else {
      gsl_matrix_symbolic *sm =
          create_symbolic_matrix(1,1+xm->size1*xm->size2);
      sm->data[xm->size1*xm->size2] = z;
      symbolic_scanr_loop(f,last,0,xm,im,sm,lasti,lastj);
      gsl_matrix_int_free(im);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(z,zc)) {
    //COMPLEX
    gsl_matrix_complex *cm = create_complex_matrix(1,1+xm->size1*xm->size2);
    reinterpret_cast<Complex*>(cm->data)[xm->size1*xm->size2] = zc;
    pure_expr *last = numeric_scanr_loop(f,z,0,xm,cm,&lasti,&lastj);
    if (!last) {
      out = pure_complex_matrix(cm);
    } else {
      gsl_matrix_symbolic *sm =
          create_symbolic_matrix(1,1+xm->size1*xm->size2);
      sm->data[xm->size1*xm->size2] = z;
      symbolic_scanr_loop(f,last,0,xm,cm,sm,lasti,lastj);
      gsl_matrix_complex_free(cm);
      out = pure_symbolic_matrix(sm);
    }
  } else
  {
    //for anything else, default to symbolic.
    gsl_matrix_symbolic *sm =
      create_symbolic_matrix(1,1+xm->size1*xm->size2);
    symbolic_scanr_loop(f,z,0,xm,static_cast<matrix_type*>(0),sm,
                        xm->size1-1,xm->size2);
    out = pure_symbolic_matrix(sm);
  }
  pure_unref(f);
  pure_unref(x);
  return out;
}

// same for scanr1
template <typename matrix_type>
pure_expr* matrix_scanr1( pure_expr *f, pure_expr *x )
{
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  if (xm->size1 == 0 || xm->size2 == 0) {
    gsl_matrix_symbolic *sm = create_symbolic_matrix(1, 0);
    return pure_symbolic_matrix(sm);
  }
  pure_ref(f);
  pure_ref(x);

  typedef typename element_of<matrix_type>::type elem_type;
  ptrdiff_t lasti,lastj;
  pure_expr *out; //result matrix
  double zd;
  int32_t zi;
  Complex zc;

  elem_type *p = reinterpret_cast<elem_type*>(xm->data+xm->size1*xm->size2-1);
  pure_expr *z = to_expr(*p);

  if (from_expr(z,zd)) {
    //DOUBLE
    gsl_matrix *dm = create_double_matrix(1,xm->size1*xm->size2);
    dm->data[xm->size1*xm->size2-1] = zd;
    pure_expr *last = numeric_scanr_loop(f,z,1,xm,dm,&lasti,&lastj);
    if (!last) {
      out = pure_double_matrix(dm);
    } else {
      gsl_matrix_symbolic *sm =
          create_symbolic_matrix(1,xm->size1*xm->size2);
      symbolic_scanr_loop(f,last,1,xm,dm,sm,lasti,lastj);
      gsl_matrix_free(dm);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(z,zi)) {
    //INT
    gsl_matrix_int *im = create_int_matrix(1,xm->size1*xm->size2);
    im->data[xm->size1*xm->size2-1] = zi;
    pure_expr *last = numeric_scanr_loop(f,z,1,xm,im,&lasti,&lastj);
    if (!last) {
      out = pure_int_matrix(im);
    } else {
      gsl_matrix_symbolic *sm =
          create_symbolic_matrix(1,xm->size1*xm->size2);
      symbolic_scanr_loop(f,last,1,xm,im,sm,lasti,lastj);
      gsl_matrix_int_free(im);
      out = pure_symbolic_matrix(sm);
    }
  } else if (from_expr(z,zc)) {
    //COMPLEX
    gsl_matrix_complex *cm = create_complex_matrix(1,xm->size1*xm->size2);
    reinterpret_cast<Complex*>(cm->data)[xm->size1*xm->size2-1] = zc;
    pure_expr *last = numeric_scanr_loop(f,z,1,xm,cm,&lasti,&lastj);
    if (!last) {
      out = pure_complex_matrix(cm);
    } else {
      gsl_matrix_symbolic *sm =
          create_symbolic_matrix(1,xm->size1*xm->size2);
      symbolic_scanr_loop(f,last,1,xm,cm,sm,lasti,lastj);
      gsl_matrix_complex_free(cm);
      out = pure_symbolic_matrix(sm);
    }
  } else
  {
    //for anything else, default to symbolic.
    gsl_matrix_symbolic *sm =
      create_symbolic_matrix(1,xm->size1*xm->size2);
    symbolic_scanr_loop(f,z,1,xm,static_cast<matrix_type*>(0),sm,
                        xm->size1-1,xm->size2-1);
    out = pure_symbolic_matrix(sm);
  }
  pure_unref(f);
  pure_unref(x);
  return out;
}


//generic matrix foldl
template <typename matrix_type>
pure_expr* matrix_foldl( pure_expr *f, pure_expr *z, pure_expr *x )
{
  pure_ref(f);
  pure_ref(x);
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  typedef typename element_of<matrix_type>::type elem_type;

  pure_new(z);
  for (size_t i=0; i<xm->size1; ++i) {
    elem_type *p = reinterpret_cast<elem_type*>(xm->data)+i*xm->tda;
    for (size_t j=0; j<xm->size2; ++j,++p) {
      pure_expr *zz = z;
      z = pure_new( pure_appl( f, 2, z, to_expr(*p)  ) );
      pure_free(zz);
    }
  }
  pure_unref(f);
  pure_unref(x);
  pure_unref(z);
  return z;
}


//generic matrix foldl1
template <typename matrix_type>
pure_expr* matrix_foldl1( pure_expr *f, pure_expr *x )
{
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  if (xm->size1 == 0 || xm->size2 == 0) return 0;
  pure_ref(f);
  pure_ref(x);
  typedef typename element_of<matrix_type>::type elem_type;

  pure_expr *z = 0;
  for (size_t i=0; i<xm->size1; ++i) {
    elem_type *p = reinterpret_cast<elem_type*>(xm->data)+i*xm->tda;
    for (size_t j=0; j<xm->size2; ++j,++p)
      if (z) {
	pure_expr *zz = z;
	z = pure_new( pure_appl( f, 2, z, to_expr(*p)  ) );
	pure_free(zz);
      } else
	z = pure_new(to_expr(*p));
  }
  pure_unref(f);
  pure_unref(x);
  pure_unref(z);
  return z;
}


//generic matrix foldr
template <typename matrix_type>
pure_expr* matrix_foldr( pure_expr *f, pure_expr *z, pure_expr *x )
{
  pure_ref(f);
  pure_ref(x);
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  typedef typename element_of<matrix_type>::type elem_type;

  pure_new(z);
  for (ptrdiff_t i=xm->size1-1; i>=0; --i) {
    elem_type *p =
      reinterpret_cast<elem_type*>(xm->data)+i*xm->tda+xm->size2-1;
    for (ptrdiff_t j=xm->size2-1; j>=0; --j,--p) {
      pure_expr *zz = z;
      z = pure_new( pure_appl( f, 2, to_expr(*p), z  ) );
      pure_free(zz);
    }
  }
  pure_unref(f);
  pure_unref(x);
  pure_unref(z);
  return z;
}


//generic matrix foldr1
template <typename matrix_type>
pure_expr* matrix_foldr1( pure_expr *f, pure_expr *x )
{
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  if (xm->size1 == 0 || xm->size2 == 0) return 0;
  pure_ref(f);
  pure_ref(x);
  typedef typename element_of<matrix_type>::type elem_type;

  pure_expr *z = 0;
  for (ptrdiff_t i=xm->size1-1; i>=0; --i) {
    elem_type *p =
      reinterpret_cast<elem_type*>(xm->data)+i*xm->tda+xm->size2-1;
    for (ptrdiff_t j=xm->size2-1; j>=0; --j,--p)
      if (z) {
	pure_expr *zz = z;
	z = pure_new( pure_appl( f, 2, to_expr(*p), z  ) );
	pure_free(zz);
      } else
	z = pure_new(to_expr(*p));
  }
  pure_unref(f);
  pure_unref(x);
  pure_unref(z);
  return z;
}


//generic matrix filter. Throws failed_cond if p(x!i) is not int for all i.
template <typename matrix_type>
matrix_type* matrix_filter( pure_expr *p, pure_expr *x ) {
  pure_ref(p);
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  typedef typename element_of<matrix_type>::type elem_type;

  //the output matrix
  matrix_type *o = create_matrix<matrix_type>(1,xm->size1*xm->size2);
  size_t n; //the size of the output matrix
  elem_type *po = reinterpret_cast<elem_type*>(o->data);
                  //po is output write head
  for (size_t i=0; i<xm->size1; ++i) {
    //pi is input read head
    elem_type *pi = reinterpret_cast<elem_type*>(xm->data)+i*xm->tda;
    for (size_t j=0; j<xm->size2; ++j,++pi) {
      pure_expr *b = pure_app( p, to_expr(*pi) );
      int32_t bi = 0;
      bool res = pure_is_int(b,&bi);
      pure_freenew(b);
      if (!res) goto exception;
      if (bi) *(po++) = *pi;
    }
  }
  n = po - reinterpret_cast<elem_type*>(o->data);

  //compact the output matrix
  if (n != xm->size1*xm->size2) {
    matrix_type *o2 = create_matrix<matrix_type>(1,n);
    memcpy(o2->data,o->data,n*sizeof(elem_type));
    matrix_free(o);
    o = o2;
  }
  pure_unref(p);
  return o;

  exception:
    pure_unref(p);
    matrix_free(o);
    pure_throw( pure_symbol(
      interpreter::g_interp->symtab.failed_cond_sym().f ) );
    return 0;
}


//generic matrix_takewhile/dropwhile. Throws failed_cond.

template <typename matrix_type>
matrix_type* matrix_takewhile( pure_expr *p, pure_expr *x ) {
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  typedef typename element_of<matrix_type>::type elem_type;

  size_t count = 0;
  for (size_t i=0; i<xm->size1; ++i) {
    //pi is input read head
    elem_type *pi = reinterpret_cast<elem_type*>(xm->data)+i*xm->tda;
    for (size_t j=0; j<xm->size2; ++j,++pi) {
      pure_expr *b = pure_app( p, to_expr(*pi) );
      int32_t bi = 0;
      bool res = pure_is_int(b,&bi);
      pure_freenew(b);
      if (!res) goto exception;
      if (!bi) goto done;
      count++;
    }
  }
  goto done;

 exception:
    pure_unref(p);
    pure_throw( pure_symbol(
      interpreter::g_interp->symtab.failed_cond_sym().f ) );
    return 0;

 done:
  //the output matrix
  matrix_type *o = create_matrix<matrix_type>(1,count);
  size_t n = count; //the size of the output matrix
  elem_type *po = reinterpret_cast<elem_type*>(o->data);
                  //po is output write head
  count = 0;
  for (size_t i=0; i<xm->size1; ++i) {
    //pi is input read head
    elem_type *pi = reinterpret_cast<elem_type*>(xm->data)+i*xm->tda;
    for (size_t j=0; j<xm->size2; ++j,++pi,++po) {
      if (count >= n) goto really_done;
      *po = *pi;
      count++;
    }
  }
 really_done:
  return o;
}

template <typename matrix_type>
matrix_type* matrix_dropwhile( pure_expr *p, pure_expr *x ) {
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  typedef typename element_of<matrix_type>::type elem_type;

  size_t count = 0, lasti = xm->size1, lastj = xm->size2;
  for (size_t i=0; i<xm->size1; ++i) {
    //pi is input read head
    elem_type *pi = reinterpret_cast<elem_type*>(xm->data)+i*xm->tda;
    for (size_t j=0; j<xm->size2; ++j,++pi) {
      pure_expr *b = pure_app( p, to_expr(*pi) );
      int32_t bi = 0;
      bool res = pure_is_int(b,&bi);
      pure_freenew(b);
      if (!res) goto exception;
      if (!bi) {
	lasti = i; lastj = j;
	goto done;
      }
      count++;
    }
  }
  goto done;

 exception:
    pure_unref(p);
    pure_throw( pure_symbol(
      interpreter::g_interp->symtab.failed_cond_sym().f ) );
    return 0;

 done:
  //the output matrix
  size_t n = xm->size1*xm->size2-count; //the size of the output matrix
  matrix_type *o = create_matrix<matrix_type>(1,n);
  elem_type *po = reinterpret_cast<elem_type*>(o->data);
                  //po is output write head
  for (size_t i=lasti; i<xm->size1; ++i) {
    //pi is input read head
    elem_type *pi = reinterpret_cast<elem_type*>(xm->data)+i*xm->tda+lastj;
    for (size_t j=lastj; j<xm->size2; ++j,++pi,++po) {
      *po = *pi;
    }
    lastj = 0;
  }
  return o;
}


//generic matrix checkers. Throw failed_cond if p(x!i) is not int for all i.

template <typename matrix_type>
bool matrix_all( pure_expr *p, pure_expr *x ) {
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  typedef typename element_of<matrix_type>::type elem_type;

  for (size_t i=0; i<xm->size1; ++i) {
    //pi is input read head
    elem_type *pi = reinterpret_cast<elem_type*>(xm->data)+i*xm->tda;
    for (size_t j=0; j<xm->size2; ++j,++pi) {
      pure_expr *b = pure_app( p, to_expr(*pi) );
      int32_t bi = 0;
      bool res = pure_is_int(b,&bi);
      pure_freenew(b);
      if (!res) goto exception;
      if (!bi) return false;
    }
  }
  return true;

  exception:
    pure_unref(p);
    pure_throw( pure_symbol(
      interpreter::g_interp->symtab.failed_cond_sym().f ) );
    return false;
}

template <typename matrix_type>
bool matrix_any( pure_expr *p, pure_expr *x ) {
  matrix_type *xm = static_cast<matrix_type*>(x->data.mat.p);
  typedef typename element_of<matrix_type>::type elem_type;

  for (size_t i=0; i<xm->size1; ++i) {
    //pi is input read head
    elem_type *pi = reinterpret_cast<elem_type*>(xm->data)+i*xm->tda;
    for (size_t j=0; j<xm->size2; ++j,++pi) {
      pure_expr *b = pure_app( p, to_expr(*pi) );
      int32_t bi = 0;
      bool res = pure_is_int(b,&bi);
      pure_freenew(b);
      if (!res) goto exception;
      if (bi) return true;
    }
  }
  return false;

  exception:
    pure_unref(p);
    pure_throw( pure_symbol(
      interpreter::g_interp->symtab.failed_cond_sym().f ) );
    return false;
}


} // namespace matrix


extern "C"
void matrix_do ( pure_expr *f, pure_expr *x )
{
  switch (x->tag) {
  case EXPR::DMATRIX : return matrix::matrix_do<gsl_matrix>(f,x);
  case EXPR::IMATRIX : return matrix::matrix_do<gsl_matrix_int>(f,x);
  case EXPR::CMATRIX : return matrix::matrix_do<gsl_matrix_complex>(f,x);
  case EXPR::MATRIX  : return matrix::matrix_do<gsl_matrix_symbolic>(f,x);
  }
}

extern "C"
pure_expr* matrix_map ( pure_expr *f, pure_expr *x )
{
  switch (x->tag) {
  case EXPR::DMATRIX : return matrix::matrix_map<gsl_matrix>(f,x);
  case EXPR::IMATRIX : return matrix::matrix_map<gsl_matrix_int>(f,x);
  case EXPR::CMATRIX : return matrix::matrix_map<gsl_matrix_complex>(f,x);
  case EXPR::MATRIX  : return matrix::matrix_map<gsl_matrix_symbolic>(f,x);
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_zipwith ( pure_expr *f, pure_expr *x, pure_expr *y )
{
  switch (x->tag) {
  case EXPR::DMATRIX :
    switch (y->tag) {
    case EXPR::DMATRIX :
      return matrix::matrix_zipwith<gsl_matrix,gsl_matrix>(f,x,y);
    case EXPR::IMATRIX :
      return matrix::matrix_zipwith<gsl_matrix,gsl_matrix_int>(f,x,y);
    case EXPR::CMATRIX :
      return matrix::matrix_zipwith<gsl_matrix,gsl_matrix_complex>(f,x,y);
    case EXPR::MATRIX :
      return matrix::matrix_zipwith<gsl_matrix,gsl_matrix_symbolic>(f,x,y);
    default : return 0;
    }
  case EXPR::IMATRIX :
    switch (y->tag) {
    case EXPR::DMATRIX :
      return matrix::matrix_zipwith<gsl_matrix_int,gsl_matrix>(f,x,y);
    case EXPR::IMATRIX :
      return matrix::matrix_zipwith<gsl_matrix_int,gsl_matrix_int>(f,x,y);
    case EXPR::CMATRIX :
      return matrix::matrix_zipwith<gsl_matrix_int,gsl_matrix_complex>(f,x,y);
    case EXPR::MATRIX :
      return matrix::matrix_zipwith<gsl_matrix_int,gsl_matrix_symbolic>(f,x,y);
    default : return 0;
    }
  case EXPR::CMATRIX :
    switch (y->tag) {
    case EXPR::DMATRIX :
      return matrix::matrix_zipwith<gsl_matrix_complex,gsl_matrix>(f,x,y);
    case EXPR::IMATRIX :
      return matrix::matrix_zipwith<gsl_matrix_complex,gsl_matrix_int>(f,x,y);
    case EXPR::CMATRIX :
      return matrix::matrix_zipwith<gsl_matrix_complex,gsl_matrix_complex>(f,x,y);
    case EXPR::MATRIX :
      return matrix::matrix_zipwith<gsl_matrix_complex,gsl_matrix_symbolic>(f,x,y);
    default : return 0;
    }
  case EXPR::MATRIX  :
    switch (y->tag) {
    case EXPR::DMATRIX :
      return matrix::matrix_zipwith<gsl_matrix_symbolic,gsl_matrix>(f,x,y);
    case EXPR::IMATRIX :
      return matrix::matrix_zipwith<gsl_matrix_symbolic,gsl_matrix_int>(f,x,y);
    case EXPR::CMATRIX :
      return matrix::matrix_zipwith<gsl_matrix_symbolic,gsl_matrix_complex>(f,x,y);
    case EXPR::MATRIX :
      return matrix::matrix_zipwith<gsl_matrix_symbolic,gsl_matrix_symbolic>(f,x,y);
    default : return 0;
    }
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_zipwith3 ( pure_expr *f, pure_expr *x, pure_expr *y,
			     pure_expr *z )
{
  switch (x->tag) {
  case EXPR::DMATRIX :
    switch (y->tag) {
    case EXPR::DMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::IMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_int,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_int,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_int,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_int,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::CMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_complex,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_complex,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_complex,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_complex,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::MATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_symbolic,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_symbolic,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_symbolic,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix,gsl_matrix_symbolic,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    default : return 0;
    }
  case EXPR::IMATRIX :
    switch (y->tag) {
    case EXPR::DMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::IMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_int,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_int,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_int,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_int,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::CMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_complex,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_complex,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_complex,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_complex,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::MATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_symbolic,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_symbolic,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_symbolic,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_int,gsl_matrix_symbolic,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    default : return 0;
    }
  case EXPR::CMATRIX :
    switch (y->tag) {
    case EXPR::DMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::IMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_int,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_int,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_int,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_int,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::CMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_complex,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_complex,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_complex,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_complex,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::MATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_symbolic,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_symbolic,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_symbolic,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_complex,gsl_matrix_symbolic,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    default : return 0;
    }
  case EXPR::MATRIX  :
    switch (y->tag) {
    case EXPR::DMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::IMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_int,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_int,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_int,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_int,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::CMATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_complex,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_complex,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_complex,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_complex,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    case EXPR::MATRIX :
      switch (z->tag) {
      case EXPR::DMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_symbolic,gsl_matrix>(f,x,y,z);
      case EXPR::IMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_symbolic,gsl_matrix_int>(f,x,y,z);
      case EXPR::CMATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_symbolic,gsl_matrix_complex>(f,x,y,z);
      case EXPR::MATRIX :
	return matrix::matrix_zipwith3<gsl_matrix_symbolic,gsl_matrix_symbolic,gsl_matrix_symbolic>(f,x,y,z);
      default : return 0;
      }
    default : return 0;
    }
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_scanl ( pure_expr *f, pure_expr *z, pure_expr *x )
{
  switch (x->tag) {
  case EXPR::DMATRIX :
    return matrix::matrix_scanl<gsl_matrix>(f,z,x);
  case EXPR::IMATRIX :
    return matrix::matrix_scanl<gsl_matrix_int>(f,z,x);
  case EXPR::CMATRIX :
    return matrix::matrix_scanl<gsl_matrix_complex>(f,z,x);
  case EXPR::MATRIX  :
    return matrix::matrix_scanl<gsl_matrix_symbolic>(f,z,x);
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_scanl1 ( pure_expr *f, pure_expr *x )
{
  switch (x->tag) {
  case EXPR::DMATRIX :
    return matrix::matrix_scanl1<gsl_matrix>(f,x);
  case EXPR::IMATRIX :
    return matrix::matrix_scanl1<gsl_matrix_int>(f,x);
  case EXPR::CMATRIX :
    return matrix::matrix_scanl1<gsl_matrix_complex>(f,x);
  case EXPR::MATRIX  :
    return matrix::matrix_scanl1<gsl_matrix_symbolic>(f,x);
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_scanr ( pure_expr *f, pure_expr *z, pure_expr *x )
{
  switch (x->tag) {
  case EXPR::DMATRIX :
    return matrix::matrix_scanr<gsl_matrix>(f,z,x);
  case EXPR::IMATRIX :
    return matrix::matrix_scanr<gsl_matrix_int>(f,z,x);
  case EXPR::CMATRIX :
    return matrix::matrix_scanr<gsl_matrix_complex>(f,z,x);
  case EXPR::MATRIX  :
    return matrix::matrix_scanr<gsl_matrix_symbolic>(f,z,x);
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_scanr1 ( pure_expr *f, pure_expr *x )
{
  switch (x->tag) {
  case EXPR::DMATRIX :
    return matrix::matrix_scanr1<gsl_matrix>(f,x);
  case EXPR::IMATRIX :
    return matrix::matrix_scanr1<gsl_matrix_int>(f,x);
  case EXPR::CMATRIX :
    return matrix::matrix_scanr1<gsl_matrix_complex>(f,x);
  case EXPR::MATRIX  :
    return matrix::matrix_scanr1<gsl_matrix_symbolic>(f,x);
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_foldl ( pure_expr *f, pure_expr *z, pure_expr *x )
{
  switch (x->tag) {
  case EXPR::DMATRIX :
    return matrix::matrix_foldl<gsl_matrix>(f,z,x);
  case EXPR::IMATRIX :
    return matrix::matrix_foldl<gsl_matrix_int>(f,z,x);
  case EXPR::CMATRIX :
    return matrix::matrix_foldl<gsl_matrix_complex>(f,z,x);
  case EXPR::MATRIX  :
    return matrix::matrix_foldl<gsl_matrix_symbolic>(f,z,x);
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_foldl1 ( pure_expr *f, pure_expr *x )
{
  switch (x->tag) {
  case EXPR::DMATRIX :
    return matrix::matrix_foldl1<gsl_matrix>(f,x);
  case EXPR::IMATRIX :
    return matrix::matrix_foldl1<gsl_matrix_int>(f,x);
  case EXPR::CMATRIX :
    return matrix::matrix_foldl1<gsl_matrix_complex>(f,x);
  case EXPR::MATRIX  :
    return matrix::matrix_foldl1<gsl_matrix_symbolic>(f,x);
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_foldr ( pure_expr *f, pure_expr *z, pure_expr *x )
{
  switch (x->tag) {
  case EXPR::DMATRIX :
    return matrix::matrix_foldr<gsl_matrix>(f,z,x);
  case EXPR::IMATRIX :
    return matrix::matrix_foldr<gsl_matrix_int>(f,z,x);
  case EXPR::CMATRIX :
    return matrix::matrix_foldr<gsl_matrix_complex>(f,z,x);
  case EXPR::MATRIX  :
    return matrix::matrix_foldr<gsl_matrix_symbolic>(f,z,x);
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_foldr1 ( pure_expr *f, pure_expr *x )
{
  switch (x->tag) {
  case EXPR::DMATRIX :
    return matrix::matrix_foldr1<gsl_matrix>(f,x);
  case EXPR::IMATRIX :
    return matrix::matrix_foldr1<gsl_matrix_int>(f,x);
  case EXPR::CMATRIX :
    return matrix::matrix_foldr1<gsl_matrix_complex>(f,x);
  case EXPR::MATRIX  :
    return matrix::matrix_foldr1<gsl_matrix_symbolic>(f,x);
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_filter ( pure_expr *p, pure_expr *x )
{
  switch ( x->tag ) {
  case EXPR::DMATRIX :
    return pure_double_matrix( matrix::matrix_filter<gsl_matrix>(p,x) );
  case EXPR::IMATRIX :
    return pure_int_matrix( matrix::matrix_filter<gsl_matrix_int>(p,x) );
  case EXPR::CMATRIX :
    return pure_complex_matrix
      ( matrix::matrix_filter<gsl_matrix_complex>(p,x) );
  case EXPR::MATRIX :
    return pure_symbolic_matrix
      ( matrix::matrix_filter<gsl_matrix_symbolic>(p,x) );
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_dropwhile ( pure_expr *p, pure_expr *x )
{
  switch ( x->tag ) {
  case EXPR::DMATRIX :
    return pure_double_matrix( matrix::matrix_dropwhile<gsl_matrix>(p,x) );
  case EXPR::IMATRIX :
    return pure_int_matrix( matrix::matrix_dropwhile<gsl_matrix_int>(p,x) );
  case EXPR::CMATRIX :
    return pure_complex_matrix
      ( matrix::matrix_dropwhile<gsl_matrix_complex>(p,x) );
  case EXPR::MATRIX :
    return pure_symbolic_matrix
      ( matrix::matrix_dropwhile<gsl_matrix_symbolic>(p,x) );
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_takewhile ( pure_expr *p, pure_expr *x )
{
  switch ( x->tag ) {
  case EXPR::DMATRIX :
    return pure_double_matrix( matrix::matrix_takewhile<gsl_matrix>(p,x) );
  case EXPR::IMATRIX :
    return pure_int_matrix( matrix::matrix_takewhile<gsl_matrix_int>(p,x) );
  case EXPR::CMATRIX :
    return pure_complex_matrix
      ( matrix::matrix_takewhile<gsl_matrix_complex>(p,x) );
  case EXPR::MATRIX :
    return pure_symbolic_matrix
      ( matrix::matrix_takewhile<gsl_matrix_symbolic>(p,x) );
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_all ( pure_expr *p, pure_expr *x )
{
  switch ( x->tag ) {
  case EXPR::DMATRIX :
    return pure_int( matrix::matrix_all<gsl_matrix>(p,x) );
  case EXPR::IMATRIX :
    return pure_int( matrix::matrix_all<gsl_matrix_int>(p,x) );
  case EXPR::CMATRIX :
    return pure_int( matrix::matrix_all<gsl_matrix_complex>(p,x) );
  case EXPR::MATRIX :
    return pure_int( matrix::matrix_all<gsl_matrix_symbolic>(p,x) );
  default : return 0;
  }
}

extern "C"
pure_expr* matrix_any ( pure_expr *p, pure_expr *x )
{
  switch ( x->tag ) {
  case EXPR::DMATRIX :
    return pure_int( matrix::matrix_any<gsl_matrix>(p,x) );
  case EXPR::IMATRIX :
    return pure_int( matrix::matrix_any<gsl_matrix_int>(p,x) );
  case EXPR::CMATRIX :
    return pure_int( matrix::matrix_any<gsl_matrix_complex>(p,x) );
  case EXPR::MATRIX :
    return pure_int( matrix::matrix_any<gsl_matrix_symbolic>(p,x) );
  default : return 0;
  }
}

/* Additional record functions. These deal with symbolic vectors of hash pairs
   with symbols or strings as keys. */

#if 0
static inline bool is_hash_pair(interpreter& interp, pure_expr *x)
{
  return x->tag == EXPR::APP && x->data.x[0]->tag == EXPR::APP &&
    x->data.x[0]->data.x[0]->tag == interp.symtab.mapsto_sym().f;
}
#endif

static inline bool is_hash_pair(interpreter& interp, pure_expr *x,
				pure_expr*& y, pure_expr*& z)
{
  if (x->tag == EXPR::APP && x->data.x[0]->tag == EXPR::APP &&
      x->data.x[0]->data.x[0]->tag == interp.symtab.mapsto_sym().f) {
    y = x->data.x[0]->data.x[1];
    z = x->data.x[1];
    return true;
  } else
    return false;
}

/* To speed up field accesses in records, we create an index mapping keys to
   real member indices, on which we can do binary search. This index is
   constructed once on first access to the structure and is then stored in a
   private field of the Pure matrix data structure. */

struct index_entry_t {
  int32_t tag; // symbol (key)
  const char *s; // key if tag is EXPR::STR
  size_t i; // field index in vector
};

struct index_t {
  size_t n, k; // total record size and number of index entries
  index_entry_t *ix; // pointer to index data
};

static int indexcmp(const void *m1, const void *m2)
{
  index_entry_t *p = (index_entry_t*)m1, *q = (index_entry_t*)m2;
  if (p->tag != q->tag)
    return p->tag - q->tag;
  else if (p->tag == EXPR::STR)
    return strcmp(p->s, q->s);
  else
    return 0;
}

static int indexcmps(const void *m1, const void *m2)
{
  // Same as above, but makes sure that duplicate entries are kept in the
  // right order (by index).
  index_entry_t *p = (index_entry_t*)m1, *q = (index_entry_t*)m2;
  if (p->tag != q->tag)
    return p->tag - q->tag;
  else if (p->tag == EXPR::STR) {
    int ret = strcmp(p->s, q->s);
    if (ret == 0)
      return (p->i<q->i)?1:(p->i>q->i)?-1:0;
    else
      return ret;
  } else
    return (p->i<q->i)?1:(p->i>q->i)?-1:0;
}

static void free_record(void *q)
{
  index_t *idx = (index_t*)q;
  if (idx->ix) free(idx->ix);
  free(q);
}

static bool is_record(pure_expr *x, pure_expr** &data, index_t* &idx)
{
  if (x->tag == EXPR::MATRIX) {
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2;
    if (m->q) {
      data = m->data;
      idx = (index_t*)m->q;
      return true;
    } else if (n1 <= 1 || n2 <= 1) {
      size_t n = n1*n2;
      data = m->data;
      if (n > 0) {
	// Allocate the index.
	idx = (index_t*)malloc(sizeof(index_t));
	if (!idx) return false;
	idx->ix = (index_entry_t*)malloc(n*sizeof(index_entry_t));
	if (!idx->ix) {
	  free(idx);
	  return false;
	}
	idx->n = idx->k = n;
	/* Build the index. */
	interpreter& interp = *interpreter::g_interp;
	for (size_t i = 0; i < n; i++) {
	  pure_expr *u, *v;
	  if (!is_hash_pair(interp, data[i], u, v) ||
	      (u->tag <= 0 && u->tag != EXPR::STR)) {
	    free(idx); free(idx->ix);
	    return false;
	  }
	  idx->ix[i].tag = u->tag;
	  idx->ix[i].s = (u->tag == EXPR::STR)?u->data.s:0;
	  idx->ix[i].i = i;
	}
	qsort(idx->ix, n, sizeof(index_entry_t), indexcmps);
	// remove duplicates
	size_t k = 0;
	for (size_t i = 0; i < n; k++) {
	  size_t j = i+1;
	  while (j < n && indexcmp(idx->ix+i, idx->ix+j) == 0) j++;
	  if (k < i) idx->ix[k] = idx->ix[i];
	  i = j;
	}
	if (k < n) {
	  index_entry_t *ix1 =
	    (index_entry_t*)realloc(idx->ix, k*sizeof(index_entry_t));
	  if (ix1) idx->ix = ix1;
	  idx->k = k;
	}
      } else {
	idx = (index_t*)malloc(sizeof(index_t));
	if (!idx) return false;
	idx->n = idx->k = 0;
	idx->ix = 0;
      }
      m->q = idx;
      return true;
    } else
      return false;
  } else
    return false;
}

static bool record_lookup(pure_expr **data, index_t *idx,
			  pure_expr *x, size_t &i)
{
  if (x->tag <= 0 && x->tag != EXPR::STR) return false;
  index_entry_t key = { x->tag, (x->tag == EXPR::STR)?x->data.s:0, 0 };
  index_entry_t *it = (index_entry_t*)bsearch(&key, idx->ix, idx->k,
					      sizeof(index_entry_t), indexcmp);
  if (it) {
    i = it->i;
    return true;
  } else
    return false;
}

static bool record_lookup(pure_expr **data, index_t *idx,
			  pure_expr *x, pure_expr* &y)
{
  if (x->tag <= 0 && x->tag != EXPR::STR) return false;
  index_entry_t key = { x->tag, (x->tag == EXPR::STR)?x->data.s:0, 0 };
  index_entry_t *it = (index_entry_t*)bsearch(&key, idx->ix, idx->k,
					      sizeof(index_entry_t), indexcmp);
  if (it) {
    pure_expr *u, *v;
    interpreter& interp = *interpreter::g_interp;
    if (is_hash_pair(interp, data[it->i], u, v)) {
      y = v;
      return true;
    } else
      return false;
  } else
    return false;
}

bool record_check(pure_expr *x)
{
  pure_expr **data;
  index_t *idx;
  return is_record(x, data, idx);
}

extern "C"
bool record_member(pure_expr *x, pure_expr *y)
{
  pure_expr **data;
  index_t *idx;
  if (is_record(x, data, idx)) {
    size_t i;
    return record_lookup(data, idx, y, i);
  } else
    return false;
}

extern "C"
pure_expr* record_elem_at(pure_expr *x, pure_expr *y)
{
  pure_expr **data;
  index_t *idx;
  if (is_record(x, data, idx)) {
    pure_expr *z;
    if (record_lookup(data, idx, y, z))
      return z;
    else
      return 0;
  } else
    return 0;
}

extern "C"
pure_expr* record_update(pure_expr *x, pure_expr *u, pure_expr *v)
{
  pure_expr **data;
  index_t *idx;
  if (is_record(x, data, idx)) {
    interpreter& interp = *interpreter::g_interp;
    gsl_matrix_symbolic *m;
    size_t i;
    if (idx->n == 0) {
      m = create_symbolic_matrix(1, 1);
      m->data[0] = pure_appl(pure_symbol(interp.symtab.mapsto_sym().f),
			     2, u, v);
      return pure_symbolic_matrix(m);
    }
    m = (gsl_matrix_symbolic*)x->data.mat.p;
    if (record_lookup(data, idx, u, i)) {
      pure_expr *u1, *v1;
      if (is_hash_pair(interp, m->data[i], u1, v1) && v1 != v) {
	pure_expr *y = pure_symbolic_matrix_dup(m);
	if (y) {
	  m = (gsl_matrix_symbolic*)y->data.mat.p;
	  pure_expr *w = pure_appl(pure_symbol(interp.symtab.mapsto_sym().f),
				   2, u, v);
	  pure_free_internal(m->data[i]);
	  m->data[i] = pure_new_internal(w);
	}
	return y;
      } else
	return x;
    }
    const size_t n1 = m->size1, n2 = m->size2;
    gsl_matrix_symbolic *m2;
    if (n1 > 1)
      m2 = create_symbolic_matrix(n1+1, n2);
    else
      m2 = create_symbolic_matrix(n1, n2+1);
    if (!m2) return 0;
    const size_t n = idx->n;
    memcpy(m2->data, m->data, n*sizeof(pure_expr*));
    m2->data[n] = pure_appl(pure_symbol(interp.symtab.mapsto_sym().f),
			    2, u, v);
    return pure_symbolic_matrix(m2);
  } else
    return 0;
}

extern "C"
pure_expr* record_delete(pure_expr *x, pure_expr *y)
{
  pure_expr **data;
  index_t *idx;
  if (is_record(x, data, idx)) {
    size_t i;
    if (!record_lookup(data, idx, y, i)) return x;
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2;
    gsl_matrix_symbolic *m2;
    const size_t n = idx->n;
    if (n == 1)
      m2 = create_symbolic_matrix(0, 0);
    else if (n1 > 1)
      m2 = create_symbolic_matrix(n1-1, n2);
    else
      m2 = create_symbolic_matrix(n1, n2-1);
    if (!m2) return 0;
    if (i>0)
      memcpy(m2->data, m->data, i*sizeof(pure_expr*));
    if (i < n-1)
      memcpy(m2->data+i, m->data+i+1, (n-i-1)*sizeof(pure_expr*));
    return pure_symbolic_matrix(m2);
  } else
    return 0;
}

extern "C"
pure_expr* record_pack(pure_expr *x)
{
  pure_expr **data;
  index_t *idx;
  if (is_record(x, data, idx)) {
    const size_t n = idx->n, k = idx->k;
    if (n == k) {
      bool same = true;
      for (size_t i = 0; i < k; i++)
	if (idx->ix[i].i != i) {
	  same = false;
	  break;
	}
      if (same) return x;
    }
    gsl_matrix_symbolic *m = (gsl_matrix_symbolic*)x->data.mat.p;
    const size_t n1 = m->size1, n2 = m->size2;
    gsl_matrix_symbolic *m2;
    if (n1 == n)
      m2 = create_symbolic_matrix(k, n2);
    else
      m2 = create_symbolic_matrix(n1, k);
    if (!m2) return 0;
    for (size_t i = 0; i < k; i++)
      m2->data[i] = data[idx->ix[i].i];
    return pure_symbolic_matrix(m2);
  } else
    return 0;
}

/* Direct Faust interface. */

#ifndef DSPEXT
// These are dsp modules which reside in bitcode files created with the Faust
// compiler.
#define DSPEXT ".bc"
#endif

extern "C"
pure_expr *faust_load(const char *fname)
{
  string name = fname, msg;
  interpreter& interp = *interpreter::g_interp;
  interp.errmsg.clear(); interp.errpos.clear();
  // See whether we need to add the DSPEXT suffix.
  if (name.size() <= strlen(DSPEXT) ||
      name.substr(name.size()-strlen(DSPEXT)) != DSPEXT)
    name += DSPEXT;
  if (!interp.LoadFaustDSP(false, name.c_str(), &msg)) {
    bool logging = interp.logging;
    interp.logging = true;
    if (msg.empty()) msg = name+": Error loading Faust bitcode file";
    interp.error(msg);
    interp.logging = logging;
    return 0;
  } else
    return pure_tuplel(0);
}

/* These data structures and routines are used to handle the metadata and
   control parameters in a Faust DSP. (This is basically the same as in the
   pure-faust module, but tailored for faust -lang llvm.) */

typedef pair<const char*,const char*> strpair;

struct FaustMeta
{
  list<strpair> data;
  void declare (const char* key, const char* value)
  { data.push_back(strpair(key, value)); }
};

class FaustUI
{
  bool	fStopped;
public:

  FaustUI() : fStopped(false) {}
  virtual ~FaustUI() {}

  virtual void addButton(const char* label, double* zone) = 0;
  virtual void addCheckButton(const char* label, double* zone) = 0;
  virtual void addVerticalSlider(const char* label, double* zone, double init, double min, double max, double step) = 0;
  virtual void addHorizontalSlider(const char* label, double* zone, double init, double min, double max, double step) = 0;
  virtual void addNumEntry(const char* label, double* zone, double init, double min, double max, double step) = 0;

  virtual void addHorizontalBargraph(const char* label, double* zone, double min, double max) = 0;
  virtual void addVerticalBargraph(const char* label, double* zone, double min, double max) = 0;

  virtual void openTabBox(const char* label) = 0;
  virtual void openHorizontalBox(const char* label) = 0;
  virtual void openVerticalBox(const char* label) = 0;
  virtual void closeBox() = 0;

  virtual void run() = 0;

  void stop()	{ fStopped = true; }
  bool stopped() 	{ return fStopped; }

  virtual void declare(double* zone, const char* key, const char* value) {}
};

enum ui_elem_type_t {
  UI_BUTTON, UI_CHECK_BUTTON,
  UI_V_SLIDER, UI_H_SLIDER, UI_NUM_ENTRY,
  UI_V_BARGRAPH, UI_H_BARGRAPH,
  UI_END_GROUP, UI_V_GROUP, UI_H_GROUP, UI_T_GROUP
};

struct ui_elem_t {
  ui_elem_type_t type;
  const char *label;
  double *zone;
  double init, min, max, step;
};

class PureFaustUI : public FaustUI
{
public:
  int nelems;
  ui_elem_t *elems;
  map< int, list<strpair> > metadata;

  PureFaustUI();
  virtual ~PureFaustUI();

protected:
  void add_elem(ui_elem_type_t type, const char *label = NULL);
  void add_elem(ui_elem_type_t type, const char *label, double *zone);
  void add_elem(ui_elem_type_t type, const char *label, double *zone,
		double init, double min, double max, double step);
  void add_elem(ui_elem_type_t type, const char *label, double *zone,
		double min, double max);

public:
  virtual void addButton(const char* label, double* zone);
  virtual void addCheckButton(const char* label, double* zone);
  virtual void addVerticalSlider(const char* label, double* zone, double init, double min, double max, double step);
  virtual void addHorizontalSlider(const char* label, double* zone, double init, double min, double max, double step);
  virtual void addNumEntry(const char* label, double* zone, double init, double min, double max, double step);

  virtual void addHorizontalBargraph(const char* label, double* zone, double min, double max);
  virtual void addVerticalBargraph(const char* label, double* zone, double min, double max);

  virtual void openTabBox(const char* label);
  virtual void openHorizontalBox(const char* label);
  virtual void openVerticalBox(const char* label);
  virtual void closeBox();

  virtual void run();

  virtual void declare(double* zone, const char* key, const char* value);
};

PureFaustUI::PureFaustUI()
{
  nelems = 0;
  elems = NULL;
}

PureFaustUI::~PureFaustUI()
{
  if (elems) free(elems);
}

void PureFaustUI::declare(double* zone, const char* key, const char* value)
{
  map< int, list<strpair> >::iterator it = metadata.find(nelems);
  if (it != metadata.end())
    it->second.push_back(strpair(key, value));
  else
    metadata[nelems] = list<strpair>(1, strpair(key, value));
}

inline void PureFaustUI::add_elem(ui_elem_type_t type, const char *label)
{
  ui_elem_t *elems1 = (ui_elem_t*)realloc(elems, (nelems+1)*sizeof(ui_elem_t));
  if (elems1)
    elems = elems1;
  else
    return;
  elems[nelems].type = type;
  elems[nelems].label = label;
  elems[nelems].zone = NULL;
  elems[nelems].init = 0.0;
  elems[nelems].min = 0.0;
  elems[nelems].max = 0.0;
  elems[nelems].step = 0.0;
  nelems++;
}

inline void PureFaustUI::add_elem(ui_elem_type_t type, const char *label, double *zone)
{
  ui_elem_t *elems1 = (ui_elem_t*)realloc(elems, (nelems+1)*sizeof(ui_elem_t));
  if (elems1)
    elems = elems1;
  else
    return;
  elems[nelems].type = type;
  elems[nelems].label = label;
  elems[nelems].zone = zone;
  elems[nelems].init = 0.0;
  elems[nelems].min = 0.0;
  elems[nelems].max = 0.0;
  elems[nelems].step = 0.0;
  nelems++;
}

inline void PureFaustUI::add_elem(ui_elem_type_t type, const char *label, double *zone,
			     double init, double min, double max, double step)
{
  ui_elem_t *elems1 = (ui_elem_t*)realloc(elems, (nelems+1)*sizeof(ui_elem_t));
  if (elems1)
    elems = elems1;
  else
    return;
  elems[nelems].type = type;
  elems[nelems].label = label;
  elems[nelems].zone = zone;
  elems[nelems].init = init;
  elems[nelems].min = min;
  elems[nelems].max = max;
  elems[nelems].step = step;
  nelems++;
}

inline void PureFaustUI::add_elem(ui_elem_type_t type, const char *label, double *zone,
			     double min, double max)
{
  ui_elem_t *elems1 = (ui_elem_t*)realloc(elems, (nelems+1)*sizeof(ui_elem_t));
  if (elems1)
    elems = elems1;
  else
    return;
  elems[nelems].type = type;
  elems[nelems].label = label;
  elems[nelems].zone = zone;
  elems[nelems].init = 0.0;
  elems[nelems].min = min;
  elems[nelems].max = max;
  elems[nelems].step = 0.0;
  nelems++;
}

void PureFaustUI::addButton(const char* label, double* zone)
{ add_elem(UI_BUTTON, label, zone); }
void PureFaustUI::addCheckButton(const char* label, double* zone)
{ add_elem(UI_CHECK_BUTTON, label, zone); }
void PureFaustUI::addVerticalSlider(const char* label, double* zone, double init, double min, double max, double step)
{ add_elem(UI_V_SLIDER, label, zone, init, min, max, step); }
void PureFaustUI::addHorizontalSlider(const char* label, double* zone, double init, double min, double max, double step)
{ add_elem(UI_H_SLIDER, label, zone, init, min, max, step); }
void PureFaustUI::addNumEntry(const char* label, double* zone, double init, double min, double max, double step)
{ add_elem(UI_NUM_ENTRY, label, zone, init, min, max, step); }

void PureFaustUI::addHorizontalBargraph(const char* label, double* zone, double min, double max)
{ add_elem(UI_H_BARGRAPH, label, zone, min, max); }
void PureFaustUI::addVerticalBargraph(const char* label, double* zone, double min, double max)
{ add_elem(UI_V_BARGRAPH, label, zone, min, max); }

void PureFaustUI::openTabBox(const char* label)
{ add_elem(UI_T_GROUP, label); }
void PureFaustUI::openHorizontalBox(const char* label)
{ add_elem(UI_H_GROUP, label); }
void PureFaustUI::openVerticalBox(const char* label)
{ add_elem(UI_V_GROUP, label); }
void PureFaustUI::closeBox()
{ add_elem(UI_END_GROUP); }

void PureFaustUI::run() {}

/* C callbacks for the UI and metadata construction methods. These are called
   in the Faust-generated LLVM code (buildUserInterface and metadata
   routines). Note that for some of these there are two variations, depending
   on whether floating point parameters are passed as double (faust -double)
   or not. */

// You might have to disable this if you're still running an old Faust2 version.
#define FAUST_NEW_META 1

extern "C" {

struct MetaGlue {
#if FAUST_NEW_META
  void *data;
#endif
  void *declare;
};

// XXXFIXME: Faust didn't use to pass the metadata here, so we use a static
// FaustMeta object to process the metadata right now. NOTE: This has been
// fixed in the latest Faust2, but we still keep it this way for now, in order
// not to break compatibility with older Faust2 versions. Eventually, the
// global data should be replaced with the first member of the glue data
// structure.
static FaustMeta *__faust_metadata; // TLD
#if FAUST_NEW_META
static void declareMetaGlue(void *data, const char* key, const char* value)
#else
static void declareMetaGlue(const char* key, const char* value)
#endif
{
  if (__faust_metadata) __faust_metadata->declare(key, value);
}

void *faust_new_metadata()
{
  MetaGlue *glue = (MetaGlue*)malloc(sizeof(MetaGlue));
  glue->declare = (void*)declareMetaGlue;
  __faust_metadata = new FaustMeta;
#if FAUST_NEW_META
  glue->data = __faust_metadata;
#endif
  return glue;
}

void faust_free_metadata(void *p)
{
  MetaGlue *glue = (MetaGlue*)p;
  if (__faust_metadata) delete __faust_metadata;
  free(glue);
}

static pure_expr *faust_make_meta(list<strpair>& m)
{
  size_t n = m.size();
  pure_expr **xv = (pure_expr**)malloc(n*sizeof(pure_expr*));
  pure_expr *f = pure_symbol(pure_sym("=>"));
  assert(f && xv);
  list<strpair>::iterator it = m.begin();
  for (size_t i = 0; i < n; i++, it++) {
    assert(it != m.end());
    xv[i] = pure_appl(f, 2,
		      pure_cstring_dup(it->first),
		      pure_cstring_dup(it->second));
  }
  pure_expr *x = pure_listv(n, xv); free(xv);
  return x;
}

pure_expr *faust_make_metadata(void *p)
{
  FaustMeta *m = __faust_metadata;
  if (!m) return 0;
  return faust_make_meta(m->data);
}

struct UIGlue {

  void* uiInterface;

  void *openTabBox;
  void *openHorizontalBox;
  void *openVerticalBox;
  void *closeBox;
  void *addButton;
  void *addCheckButton;
  void *addVerticalSlider;
  void *addHorizontalSlider;
  void *addNumEntry;
  void *addHorizontalBargraph;
  void *addVerticalBargraph;
  void *declare;

};

static void openTabBoxGlue(void* cpp_interface, const char* label)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->openTabBox(label);
}

static void openHorizontalBoxGlue(void* cpp_interface, const char* label)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->openHorizontalBox(label);
}

static void openVerticalBoxGlue(void* cpp_interface, const char* label)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->openVerticalBox(label);
}

static void closeBoxGlue(void* cpp_interface)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->closeBox();
}

static void addButtonFloatGlue(void* cpp_interface, const char* label, float* zone)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addButton(label, (double*)zone);
}

static void addCheckButtonFloatGlue(void* cpp_interface, const char* label, float* zone)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addCheckButton(label, (double*)zone);
}

static void addVerticalSliderFloatGlue(void* cpp_interface, const char* label, float* zone, float init, float min, float max, float step)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addVerticalSlider(label, (double*)zone, init, min, max, step);
}

static void addHorizontalSliderFloatGlue(void* cpp_interface, const char* label, float* zone, float init, float min, float max, float step)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addHorizontalSlider(label, (double*)zone, init, min, max, step);
}

static void addNumEntryFloatGlue(void* cpp_interface, const char* label, float* zone, float init, float min, float max, float step)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addNumEntry(label, (double*)zone, init, min, max, step);
}

static void addHorizontalBargraphFloatGlue(void* cpp_interface, const char* label, float* zone, float min, float max)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addHorizontalBargraph(label, (double*)zone, min, max);
}

static void addVerticalBargraphFloatGlue(void* cpp_interface, const char* label, float* zone, float min, float max)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addVerticalBargraph(label, (double*)zone, min, max);
}

static void declareFloatGlue(void* cpp_interface, float* zone, const char* key, const char* value)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->declare((double*)zone, key, value);
}

static void addButtonDoubleGlue(void* cpp_interface, const char* label, double* zone)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addButton(label, zone);
}

static void addCheckButtonDoubleGlue(void* cpp_interface, const char* label, double* zone)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addCheckButton(label, zone);
}

static void addVerticalSliderDoubleGlue(void* cpp_interface, const char* label, double* zone, double init, double min, double max, double step)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addVerticalSlider(label, zone, init, min, max, step);
}

static void addHorizontalSliderDoubleGlue(void* cpp_interface, const char* label, double* zone, double init, double min, double max, double step)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addHorizontalSlider(label, zone, init, min, max, step);
}

static void addNumEntryDoubleGlue(void* cpp_interface, const char* label, double* zone, double init, double min, double max, double step)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addNumEntry(label, zone, init, min, max, step);
}

static void addHorizontalBargraphDoubleGlue(void* cpp_interface, const char* label, double* zone, double min, double max)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addHorizontalBargraph(label, zone, min, max);
}

static void addVerticalBargraphDoubleGlue(void* cpp_interface, const char* label, double* zone, double min, double max)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->addVerticalBargraph(label, zone, min, max);
}

static void declareDoubleGlue(void* cpp_interface, double* zone, const char* key, const char* value)
{
    FaustUI* fstinterface = static_cast<FaustUI*>(cpp_interface);
    fstinterface->declare(zone, key, value);
}

/* Set up a Faust UI data structure, ready to be used as the second argument
   of the Faust-generated buildUserInterface routine. The returned data
   objects are allocated dynamically and must be freed by the caller when no
   longer needed, using the faust_free_ui routine provided below. */

void *faust_float_ui()
{
  UIGlue *glue = (UIGlue*)malloc(sizeof(UIGlue));
  PureFaustUI *ui = new PureFaustUI;
  assert(glue && ui);
  glue->uiInterface = ui;
  glue->openTabBox = (void*)openTabBoxGlue;
  glue->openHorizontalBox = (void*)openHorizontalBoxGlue;
  glue->openVerticalBox = (void*)openVerticalBoxGlue;
  glue->closeBox = (void*)closeBoxGlue;
  glue->addButton = (void*)addButtonFloatGlue;
  glue->addCheckButton = (void*)addCheckButtonFloatGlue;
  glue->addVerticalSlider = (void*)addVerticalSliderFloatGlue;
  glue->addHorizontalSlider = (void*)addHorizontalSliderFloatGlue;
  glue->addNumEntry = (void*)addNumEntryFloatGlue;
  glue->addHorizontalBargraph = (void*)addHorizontalBargraphFloatGlue;
  glue->addVerticalBargraph = (void*)addVerticalBargraphFloatGlue;
  glue->declare = (void*)declareFloatGlue;
  return glue;
}

void *faust_double_ui()
{
  UIGlue *glue = (UIGlue*)malloc(sizeof(UIGlue));
  PureFaustUI *ui = new PureFaustUI;
  assert(glue && ui);
  glue->uiInterface = ui;
  glue->openTabBox = (void*)openTabBoxGlue;
  glue->openHorizontalBox = (void*)openHorizontalBoxGlue;
  glue->openVerticalBox = (void*)openVerticalBoxGlue;
  glue->closeBox = (void*)closeBoxGlue;
  glue->addButton = (void*)addButtonDoubleGlue;
  glue->addCheckButton = (void*)addCheckButtonDoubleGlue;
  glue->addVerticalSlider = (void*)addVerticalSliderDoubleGlue;
  glue->addHorizontalSlider = (void*)addHorizontalSliderDoubleGlue;
  glue->addNumEntry = (void*)addNumEntryDoubleGlue;
  glue->addHorizontalBargraph = (void*)addHorizontalBargraphDoubleGlue;
  glue->addVerticalBargraph = (void*)addVerticalBargraphDoubleGlue;
  glue->declare = (void*)declareDoubleGlue;
  return glue;
}

void faust_free_ui(void *p)
{
  UIGlue *glue = (UIGlue*)p;
  FaustUI* ui = static_cast<FaustUI*>(glue->uiInterface);
  delete ui;
  free(glue);
}

/* Grab the control descriptions from a UI data structure, as returned by
   faust_float_ui or faust_double_ui, and return it as a Pure expression. This
   is analogous to the faust_info routine in the pure-faust module. */

static struct stack_elem_t {
  int i, n;
  pure_expr **xv;
} *stack = NULL; // TLD

static int astacksz = 0, stacksz = 0; // TLD

static void clear()
{
  for (int i = 0; i < stacksz; i++)
    if (stack[i].xv) free(stack[i].xv);
  free(stack); stack = NULL; astacksz = stacksz = 0;
}

static int push(int i, int n, pure_expr **xv)
{
  if (stacksz+1 >= astacksz) {
    stack_elem_t *stack1 =
      (stack_elem_t*)realloc(stack, (astacksz+100)*sizeof(stack_elem_t));
    if (!stack1) return 0;
    stack = stack1;
    astacksz += 100;
  }
  stack[stacksz].i = i;
  stack[stacksz].n = n;
  stack[stacksz].xv = xv;
  stacksz++;
  return 1;
}

static int pop(int &i, int &n, pure_expr **&xv)
{
  if (stacksz <= 0) return 0;
  stacksz--;
  i = stack[stacksz].i;
  n = stack[stacksz].n;
  xv = stack[stacksz].xv;
  return 1;
}

static pure_expr *faust_make_ui(void *p, const char *name)
{
  UIGlue *glue = (UIGlue*)p;
  PureFaustUI *ui = static_cast<PureFaustUI*>(glue->uiInterface);
  bool is_double = glue->addButton == (void*)addButtonDoubleGlue;
  int ty = pure_pointer_tag(is_double?"double*":"float*");
  if (ui->nelems <= 0) return pure_tuplel(0);
  pure_expr **xv = NULL;
  int n = 0;
  // Get rid of bogus "0x00" labels in recent Faust revisions. Also, for
  // backward compatibility with old Faust versions, make sure that default
  // toplevel groups and explicit toplevel groups with an empty label are
  // treated alike (these both return "0x00" labels in the latest Faust, but
  // would be treated inconsistently in earlier versions).
  for (int i = 0; i < ui->nelems; i++) {
    if (!ui->elems[i].label) continue;
    if (!*ui->elems[i].label ||
	strcmp(ui->elems[i].label, "0x00") == 0) {
      if (i == 0)
	// toplevel group with empty label, map to dsp name
	ui->elems[i].label = name;
      else
	// empty label
	ui->elems[i].label = "";
    }
  }
  for (int i = 0; i < ui->nelems; i++) {
    pure_expr *x;
    pure_expr **xv1 = (pure_expr**)realloc(xv, (n+1)*sizeof(pure_expr*));
    if (xv1)
      xv = xv1;
    else {
      free(xv);
      clear();
      return NULL;
    }
    switch (ui->elems[i].type) {
    case UI_BUTTON:
      xv[n++] = pure_appl(pure_symbol(pure_sym("button")), 3,
			  pure_tag(ty, pure_pointer(ui->elems[i].zone)),
			  faust_make_meta(ui->metadata[i]),
			  pure_cstring_dup(ui->elems[i].label));
      break;
    case UI_CHECK_BUTTON:
      xv[n++] = pure_appl(pure_symbol(pure_sym("checkbox")), 3,
			  pure_tag(ty, pure_pointer(ui->elems[i].zone)),
			  faust_make_meta(ui->metadata[i]),
			  pure_cstring_dup(ui->elems[i].label));
      break;
    case UI_V_SLIDER:
      xv[n++] = pure_appl(pure_symbol(pure_sym("vslider")), 3,
			  pure_tag(ty, pure_pointer(ui->elems[i].zone)),
			  faust_make_meta(ui->metadata[i]),
			  pure_tuplel(5, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].init),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max),
				      pure_double(ui->elems[i].step)));
      break;
    case UI_H_SLIDER:
      xv[n++] = pure_appl(pure_symbol(pure_sym("hslider")), 3,
			  pure_tag(ty, pure_pointer(ui->elems[i].zone)),
			  faust_make_meta(ui->metadata[i]),
			  pure_tuplel(5, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].init),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max),
				      pure_double(ui->elems[i].step)));
      break;
    case UI_NUM_ENTRY:
      xv[n++] = pure_appl(pure_symbol(pure_sym("nentry")), 3,
			  pure_tag(ty, pure_pointer(ui->elems[i].zone)),
			  faust_make_meta(ui->metadata[i]),
			  pure_tuplel(5, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].init),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max),
				      pure_double(ui->elems[i].step)));
      break;
    case UI_V_BARGRAPH:
      xv[n++] = pure_appl(pure_symbol(pure_sym("vbargraph")), 3,
			  pure_tag(ty, pure_pointer(ui->elems[i].zone)),
			  faust_make_meta(ui->metadata[i]),
			  pure_tuplel(3, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max)));
      break;
    case UI_H_BARGRAPH:
      xv[n++] = pure_appl(pure_symbol(pure_sym("hbargraph")), 3,
			  pure_tag(ty, pure_pointer(ui->elems[i].zone)),
			  faust_make_meta(ui->metadata[i]),
			  pure_tuplel(3, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max)));
      break;
    case UI_T_GROUP:
    case UI_V_GROUP:
    case UI_H_GROUP:
      push(i, n, xv);
      n = 0; xv = NULL;
      break;
    case UI_END_GROUP: {
      int i = 0;
      x = pure_listv(n, xv); free(xv);
      pop(i, n, xv);
      switch (ui->elems[i].type) {
      case UI_T_GROUP:
	xv[n++] = pure_appl
	  (pure_symbol(pure_sym("tgroup")), 2,
	   faust_make_meta(ui->metadata[i]),
	   pure_tuplel(2, pure_cstring_dup(ui->elems[i].label), x));
	break;
      case UI_V_GROUP:
	xv[n++] = pure_appl
	  (pure_symbol(pure_sym("vgroup")), 2,
	   faust_make_meta(ui->metadata[i]),
	   pure_tuplel(2, pure_cstring_dup(ui->elems[i].label), x));
	break;
      case UI_H_GROUP:
	xv[n++] = pure_appl
	  (pure_symbol(pure_sym("hgroup")), 2,
	   faust_make_meta(ui->metadata[i]),
	   pure_tuplel(2, pure_cstring_dup(ui->elems[i].label), x));
	break;
      default:
	/* can't happen */
	xv[n++] = NULL;
	break;
      }
      break;
    }
    default:
      /* can't happen */
      xv[n++] = NULL;
      break;
    }
  }
  clear();
  assert(n==1);
  pure_expr *x = xv[0];
  free(xv);
  return x;
}

/* Create dsp-related information, including the UI data. */

pure_expr *faust_make_info(int n_in, int n_out, void *ui,
			   const char *modname)
{
  return pure_tuplel(3, pure_int(n_in), pure_int(n_out),
		     faust_make_ui(ui, modname));
}

/* Initialize runtime type information about Faust dsps in batch-compiled
   scripts. */

void faust_add_rtti(const char *name, int tag, bool dbl)
{
  interpreter& interp = *interpreter::g_interp;
  bcmap::iterator m = interp.loaded_dsps.find(name);
  if (m == interp.loaded_dsps.end()) {
    interp.loaded_dsps[name].tag = tag;
    interp.loaded_dsps[name].dbl = dbl;
  }
  map<int,bcmap::iterator>::const_iterator t = interp.dsp_mods.find(tag);
  if (t == interp.dsp_mods.end()) {
    m = interp.loaded_dsps.find(name);
    if (m != interp.loaded_dsps.end())
      interp.dsp_mods[tag] = m;
  }
}

/* Runtime type information about Faust dsps. */

pure_expr *faust_name(pure_expr *dsp)
{
  int tag = pure_get_tag(dsp);
  if (tag == 0) return 0;
  interpreter& interp = *interpreter::g_interp;
  map<int,bcmap::iterator>::const_iterator it = interp.dsp_mods.find(tag);
  if (it == interp.dsp_mods.end()) return 0;
  const string& name = it->second->first;
  return pure_cstring_dup(name.c_str());
}

pure_expr *faust_dbl(pure_expr *dsp)
{
  int tag = pure_get_tag(dsp);
  if (tag == 0) return 0;
  interpreter& interp = *interpreter::g_interp;
  map<int,bcmap::iterator>::const_iterator it = interp.dsp_mods.find(tag);
  if (it == interp.dsp_mods.end()) return 0;
  const bcdata_t& info = it->second->second;
  return pure_int(info.dbl);
}

pure_expr *faust_method(pure_expr *dsp, const char *method)
{
  if (!method || !*method) return 0;
  string name;
  const char *s;
  interpreter& interp = *interpreter::g_interp;
  if (pure_is_string(dsp, &s)) {
    name = s;
  } else {
    int tag = pure_get_tag(dsp);
    if (!tag) return 0;
    map<int,bcmap::iterator>::const_iterator it = interp.dsp_mods.find(tag);
    if (it == interp.dsp_mods.end()) return 0;
    name = it->second->first;
  }
  string asname = name+"::"+method;
  const symbol* sym = interp.symtab.lookup(asname);
  if (!sym || sym->f <= 0) return 0;
  return pure_symbol(sym->f);
}

/* Build a list of all Faust modules currently loaded. */

pure_expr *faust_mods()
{
  interpreter& interp = *interpreter::g_interp;
  size_t i = 0, n = interp.loaded_dsps.size();
  pure_expr **xs = new pure_expr*[n];
  for (bcmap::const_iterator m = interp.loaded_dsps.begin(),
	 end = interp.loaded_dsps.end(); m != end; ++m) {
    const string& modname = m->first;
    const bcdata_t& info = m->second;
    pure_expr *u = pure_cstring_dup(modname.c_str()), *v = pure_int(info.dbl);
    xs[i++] = pure_appl(pure_symbol(interp.symtab.mapsto_sym().f), 2, u, v);
  }
  pure_expr *x = pure_listv(n, xs);
  delete[] xs;
  return x;
}

}
