#ifndef RUNTIME_H
#define RUNTIME_H

/* The Pure runtime interface. */

/* Copyright (c) 2008-2012 by Albert Graef <Dr.Graef@t-online.de>.
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

/* Define this to use the MPIR header instead of GMP. NOTE: If you have both
   gmp.h and mpir.h installed on your system, it should be ok to just include
   gmp.h here, as MPIR is supposed to be binary-compatible with GMP and we
   only use the mpz_t type here. */
/* #undef USE_MPIR */

#include <stdint.h>
#include <stdbool.h>
#include <setjmp.h>
#ifdef USE_MPIR
#include <mpir.h>
#else
#include <gmp.h>
#endif
#include <mpfr.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Our "limb" type. Used to pass bigint constants to the runtime. */
typedef mp_limb_t limb_t;

/* The following data structures should be considered opaque by
   applications. */

/* Closure data. This is a bit on the heavy side, so expressions which need
   it (i.e., functions) refer to this extra data via an allocated pointer. */

typedef struct {
  void *fp;			// pointer to executable code
  void *ep;			// pointer to compile time environment (Env*)
  void *refp;			// pointer to reference counter (uint32_t*)
  uint32_t n, m;		// number of arguments and environment size
  struct _pure_expr **env;	// captured environment (if m>0, 0 otherwise)
  bool local;			// local function?
  uint32_t key;			// key identifying a local closure (0 = none)
} pure_closure;

/* Matrix data. The GSL-compatible matrix data is represented as a void* whose
   actual type depends on the expression tag. Different expressions may share
   the same underlying memory block, so we do our own reference counting to
   manage these. */

typedef struct {
  uint32_t *refc;		// reference counter
  void *p;			// pointer to GSL matrix struct
} pure_matrix;

/* The runtime expression data structure. Keep this lean and mean. */

typedef struct _pure_expr {
  /* Public fields, these *must* be layed out exactly as indicated.
     The JIT depends on it! */
  int32_t tag; // type tag or symbol, see expr.hh for possible values
  uint32_t refc;		// reference counter
  union {
    struct _pure_expr *x[2];	// application arguments (EXPR::APP)
    int32_t i;			// integer (EXPR::INT)
    mpz_t z;			// GMP bigint (EXPR::BIGINT)
    double d;			// double (EXPR::DBL)
    char *s;			// C string (EXPR::STR)
    void *p;			// generic pointer (EXPR::PTR)
    pure_matrix mat;		// matrix data (EXPR::MATRIX et al)
    pure_closure *clos;		// closure (0 if none)
  } data;
  /* Internal fields (DO NOT TOUCH). The JIT doesn't care about these. */
  struct _pure_expr *sy;	// sentry
  struct _pure_expr *xp;	// freelist pointer
} pure_expr;

/* Blocks of expression memory allocated in one chunk. */

#define MEMSIZE 0x20000 // 128K

typedef struct _pure_mem {
  struct _pure_mem *next;	// link to next block
  pure_expr *p;			// pointer to last used expression
  pure_expr x[MEMSIZE];		// expression data
} pure_mem;

/* PUBLIC API. **************************************************************/

/* The following routines are meant to be used by external C modules and other
   applications which need direct access to Pure expression data. */

/* Symbol table access. pure_sym returns the integer code of a (function or
   variable) symbol given by its print name; if the symbol doesn't exist yet,
   it is created (as an ordinary function or variable symbol). pure_getsym is
   like pure_sym, but returns 0 if the symbol doesn't exist.

   Given the (positive) symbol number, pure_sym_pname returns its print name.
   pure_sym_other checks whether the given symbol is an outfix symbol; if so,
   it returns the number of the corresponding terminating bracket symbol,
   otherwise it returns zero. pure_sym_nprec returns the "normalized"
   precedence of a symbol. The latter is a small integer value defined as
   nprec = 10*prec+fix, where prec is the precedence level of the symbol and
   fix its fixity. */

/* Fixity values. */
#define OP_INFIX   0
#define OP_INFIXL  1
#define OP_INFIXR  2
#define OP_PREFIX  3
#define OP_POSTFIX 4
#define OP_NULLARY 5

/* Maximum precedence. */
#define PREC_MAX 16777216
#define NPREC_MAX 167772160

int32_t pure_sym(const char *s);
int32_t pure_getsym(const char *s);
const char *pure_sym_pname(int32_t sym);
int32_t pure_sym_other(int32_t sym);
int32_t pure_sym_nprec(int32_t sym);

/* Expression constructors. pure_symbol takes the integer code of a symbol and
   returns that symbol as a Pure value. If the symbol is a global variable
   bound to a value then that value is returned, if it's a parameterless
   function then it is evaluated, giving the return value of the function as
   the result. pure_symbolx works like pure_symbol, but also catches
   exceptions and returns them in the parameter e. pure_quoted_symbol
   constructs a symbol expression, too, but doesn't evaluate it. pure_int,
   pure_mpz, pure_double and pure_pointer construct a Pure machine int,
   bigint, floating point value and pointer from a 32 bit integer, (copy of a)
   GMP mpz_t, double and C pointer, respectively. */

pure_expr *pure_symbol(int32_t sym);
pure_expr *pure_symbolx(int32_t sym, pure_expr **e);
pure_expr *pure_quoted_symbol(int32_t tag);
pure_expr *pure_int(int32_t i);
pure_expr *pure_mpz(const mpz_t z);
pure_expr *pure_double(double d);
pure_expr *pure_pointer(void *p);

/* Expression pointers. The following routine returns a Pure pointer object
   suitably allocated to hold a Pure expression (pure_expr*). The pointer is
   initialized to hold a null pointer value. */

pure_expr *pure_expr_pointer(void);

/* String constructors. There are four variations of these, depending on
   whether the original string is already in utf-8 (_string routines) or in
   the system encoding (_cstring), and whether the string should be copied
   (_dup suffix) or whether Pure takes ownership of the string. All four
   routines handle the case that the given string is a NULL pointer and will
   then return the appropriate Pure pointer expression instead. */

pure_expr *pure_string_dup(const char *s);
pure_expr *pure_cstring_dup(const char *s);
pure_expr *pure_string(char *s);
pure_expr *pure_cstring(char *s);

/* Matrix constructors. The given pointer must point to a valid GSL matrix
   struct of the corresponding GSL matrix type or the special symbolic matrix
   struct provided by the runtime (see gsl_structs.h in the sources). These
   are just given as void* here to avoid depending on the GSL headers. In the
   case of the _matrix routines, the matrix must be allocated dynamically and
   Pure takes ownership of the matrix. The matrix_dup routines first take a
   copy of the matrix, so the ownership of the original matrix remains with
   the caller. The result is a Pure expression representing the matrix object,
   or NULL if some error occurs. */

pure_expr *pure_symbolic_matrix(void *p);
pure_expr *pure_double_matrix(void *p);
pure_expr *pure_complex_matrix(void *p);
pure_expr *pure_int_matrix(void *p);
pure_expr *pure_symbolic_matrix_dup(const void *p);
pure_expr *pure_double_matrix_dup(const void *p);
pure_expr *pure_complex_matrix_dup(const void *p);
pure_expr *pure_int_matrix_dup(const void *p);

/* Convenience functions to construct a Pure matrix from a vector or a varargs
   list of element expressions, which can be component matrices or scalars.
   These work like the built-in matrix construction operations. The
   pure_matrix_rows functions arrange the elements vertically, while the
   pure_matrix_columns functions arrange them horizontally, given that the
   other dimensions match.

   The result is always a matrix of the smallest type necessary to accomodate
   all component values. Thus if all components are integers or integer
   matrices, the result will again be an integer matrix; likewise for double
   and complex values. If any component is a symbolic matrix or some other
   symbolic value (i.e., a scalar value which cannot be represented as a
   machine int, double or complex value; this includes bigints), or if the
   (numeric) values in the matrix have different types, the result is always a
   symbolic matrix.

   A NULL value is returned in case of an error (dimension mismatch,
   insufficient memory), leaving the input expressions untouched. Otherwise a
   new matrix expression is returned and references are counted on component
   expressions as appropriate (temporary components may also be
   garbage-collected if they are no longer needed). In any case, the elems
   vectors are owned by the caller and won't be freed. */

pure_expr *pure_matrix_rowsl(uint32_t n, ...);
pure_expr *pure_matrix_rowsv(uint32_t n, pure_expr **elems);
pure_expr *pure_matrix_columnsl(uint32_t n, ...);
pure_expr *pure_matrix_columnsv(uint32_t n, pure_expr **elems);

/* Direct Pure function calls. This is provided to call a function in
   batch-compiled Pure code (which cannot be done directly, because the
   function needs a stack frame; pure_funcall supplies this). The first
   argument must be a pointer to a global Pure function, which is followed by
   the number of arguments and the arguments (pure_expr*) themselves. The
   number of arguments *must* match the number of parameters of the function
   (no partial applications!), otherwise the results are undefined.
   pure_funcallx works like pure_funcall but also catches and reports
   exceptions. */

pure_expr *pure_funcall(void *f, uint32_t n, ...);
pure_expr *pure_funcallx(void *f, pure_expr **e, uint32_t n, ...);

/* Function applications. pure_app applies the given function to the given
   argument. The result is evaluated if possible (i.e., if it is a saturated
   function call). Otherwise, the result is a literal application and
   references on function and argument are counted automatically. */

pure_expr *pure_app(pure_expr *fun, pure_expr *arg);

/* Convenience functions to construct an application of the given function to
   a vector or varargs list of argument expressions. The vectors are owned by
   the caller and won't be freed. References on the argument expressions are
   counted automatically. */

pure_expr *pure_appl(pure_expr *fun, size_t argc, ...);
pure_expr *pure_appv(pure_expr *fun, size_t argc, pure_expr **args);

/* These work like pure_app, pure_appl and pure_appv above, but also catch and
   report exceptions. */

pure_expr *pure_appx(pure_expr *fun, pure_expr *arg, pure_expr **e);
pure_expr *pure_appxl(pure_expr *fun, pure_expr **e, size_t argc, ...);
pure_expr *pure_appxv(pure_expr *fun, size_t argc, pure_expr **args,
		      pure_expr **e);

/* Convenience functions to construct Pure list and tuple values from a vector
   or a varargs list of element expressions. (Internally these are actually
   represented as function applications.) Note that pure_listv2 works like
   pure_listv but takes a third argument denoting the tail of the list; this
   allows you to create list values with an arbitrary tail instead of []. In
   any case, the vectors are owned by the caller and won't be freed.
   References on the element expressions are counted automatically. */

pure_expr *pure_listl(size_t size, ...);
pure_expr *pure_listv(size_t size, pure_expr **elems);
pure_expr *pure_listv2(size_t size, pure_expr **elems, pure_expr *tail);
pure_expr *pure_tuplel(size_t size, ...);
pure_expr *pure_tuplev(size_t size, pure_expr **elems);

/* These variations create quoted literals, without evaluating the list/tuple
   constructors. */

pure_expr *pure_listlq(size_t size, ...);
pure_expr *pure_listvq(size_t size, pure_expr **elems);
pure_expr *pure_listv2q(size_t size, pure_expr **elems, pure_expr *tail);
pure_expr *pure_tuplelq(size_t size, ...);
pure_expr *pure_tuplevq(size_t size, pure_expr **elems);

/* Convenience functions to create Pure lists and tuples of int or double
   values. These work like the corresponding generic functions above but take
   their input from a C vector of 32 bit integers or double values,
   respectively. */

pure_expr *pure_intlistv(size_t size, int32_t *elems);
pure_expr *pure_intlistv2(size_t size, int32_t *elems, pure_expr *tail);
pure_expr *pure_inttuplev(size_t size, int32_t *elems);

pure_expr *pure_doublelistv(size_t size, double *elems);
pure_expr *pure_doublelistv2(size_t size, double *elems, pure_expr *tail);
pure_expr *pure_doubletuplev(size_t size, double *elems);

pure_expr *pure_intlistvq(size_t size, int32_t *elems);
pure_expr *pure_intlistv2q(size_t size, int32_t *elems, pure_expr *tail);
pure_expr *pure_inttuplevq(size_t size, int32_t *elems);

pure_expr *pure_doublelistvq(size_t size, double *elems);
pure_expr *pure_doublelistv2q(size_t size, double *elems, pure_expr *tail);
pure_expr *pure_doubletuplevq(size_t size, double *elems);

/* Expression deconstructors for all of the expression types above. These
   return a bool value indicating whether the given expression is of the
   corresponding type and, if so, set the remaining pointers to the
   corresponding values. Parameter pointers may be NULL in which case they are
   not set.

   NOTES: pure_is_mpz takes a pointer to an uninitialized mpz_t and
   initializes it with a copy of the Pure bigint. pure_is_symbol will return
   true not only for (constant and unbound variable) symbols, but also for
   arbitrary closures including local and anonymous functions. In the case of
   an anonymous closure, the returned symbol will be 0. You can check whether
   an expression actually represents a named or anonymous closure using the
   funp and lambdap predicates from the library API (see below). */

bool pure_is_symbol(const pure_expr *x, int32_t *sym);
bool pure_is_int(const pure_expr *x, int32_t *i);
bool pure_is_mpz(const pure_expr *x, mpz_t *z);
bool pure_is_double(const pure_expr *x, double *d);
bool pure_is_pointer(const pure_expr *x, void **p);

/* String deconstructors. Here the string results are copied if using the _dup
   routines (it is then the caller's responsibility to free them when
   appropriate). pure_is_cstring_dup also converts the string to the system
   encoding. The string value returned by pure_is_string points directly to
   the string data in the Pure expression and must not be changed by the
   caller. */

bool pure_is_string(const pure_expr *x, const char **s);
bool pure_is_string_dup(const pure_expr *x, char **s);
bool pure_is_cstring_dup(const pure_expr *x, char **s);

/* Matrix deconstructors. The returned GSL matrix pointer (represented as a
   void*) points to memory owned by Pure which should be considered read-only
   and must not be freed. */

bool pure_is_symbolic_matrix(const pure_expr *x, void **p);
bool pure_is_double_matrix(const pure_expr *x, void **p);
bool pure_is_complex_matrix(const pure_expr *x, void **p);
bool pure_is_int_matrix(const pure_expr *x, void **p);

/* Deconstruct literal applications. */

bool pure_is_app(const pure_expr *x, pure_expr **fun, pure_expr **arg);

/* Convenience function to decompose a function application into a function
   and a vector of argument expressions. The returned element vectors are
   malloc'ed and must be freed by the caller (unless the number of arguments
   is zero in which case the returned vector will be NULL). Note that this
   function always yields true, since a singleton expression which is not an
   application is considered to be a function applied to zero arguments. In
   such a case you can check the returned function object with pure_is_symbol
   to see whether it actually is a symbol or closure. */

bool pure_is_appv(pure_expr *x, pure_expr **fun,
		  size_t *argc, pure_expr ***args);

/* Convenience functions to deconstruct lists and tuples. The returned element
   vectors are malloc'ed and must be freed by the caller (unless the number of
   elements is zero in which case the returned vector will be NULL). Note that
   pure_is_tuplev will always return true, since a singleton expression, which
   is not either a pair or (), is considered to be a tuple of size 1. */

bool pure_is_listv(pure_expr *x, size_t *size, pure_expr ***elems);
bool pure_is_tuplev(pure_expr *x, size_t *size, pure_expr ***elems);

/* Complex number support. These are actually defined as an algebraic type in
   math.pure, but some applications may require access to these values in the
   C interface. Note that we don't want to rely on ISOC99 complex number
   support or any kind of third-party library here, so this API represents
   complex values simply as a double[2]. */

pure_expr *pure_complex(double c[2]);
bool pure_is_complex(pure_expr *x, double *c);

/* Same for rational values (cf. math.pure). */

pure_expr *pure_rationalz(const mpz_t z[2]);
bool pure_is_rationalz(const pure_expr *x, mpz_t *z);

/* Memory management. */

/* Count a new reference to an expression. This should be called whenever you
   want to store an expression somewhere, in order to prevent it from being
   garbage-collected. */

pure_expr *pure_new(pure_expr *x);

/* Drop a reference to an expression. This will cause the expression to be
   garbage-collected when its reference count drops to zero. The current
   reference count must be nonzero when calling this function. */

void pure_free(pure_expr *x);

/* Count a reference and then immediately drop it. This is useful to collect
   temporaries which are not referenced yet. */

void pure_freenew(pure_expr *x);

/* Increment and decrement the reference counter of an expression. This can be
   used to temporarily protect an expression from being garbage-collected. It
   doesn't normally change the status (referenced, temporary, to be freed
   immediately) of the expression and does not collect it.

   This is only for special uses where you need to temporarily modify the
   reference counter of an expression without actually changing its status. To
   these ends, the two operations are typically used in concert. For instance,
   the runtime itself uses pure_ref/pure_unref to temporarily count an extra
   reference on a return value while collecting other terms (e.g., arguments
   which might contain the return value as a subterm), to prevent the return
   value from being garbage-collected.

   In addition, pure_unref can also be used in conjuction with a prior
   invocation of pure_new. To these ends, pure_unref puts its argument back on
   the list of temporaries (expressions with a reference count of zero) if it
   isn't already there and the reference count drops to zero. This provides an
   alternative to pure_free in cases where you still want to keep the
   expression around but turn it into a temporary. Such temporaries may be
   garbage-collected at the toplevel or when an exception is thrown. Don't
   expect them to survive the lifetime of the current call unless they are
   used as the return value. */

void pure_ref(pure_expr *x);
void pure_unref(pure_expr *x);

/* Sentries. These are expression "guards" which are applied to the target
   expression when it is garbage-collected. pure_sentry places a sentry at an
   expression (or removes it if sentry is NULL) and returns the modified
   expression, pure_get_sentry returns the current sentry of an expression, if
   any (NULL otherwise), and pure_has_sentry returns a flag indicating whether
   the expression currently has a sentry. pure_clear_sentry(x) is the same as
   pure_sentry(NULL, x). NOTE: In the current implementation sentries can only
   be placed at applications and pointer objects, pure_sentry will return NULL
   if you apply it to other kinds of expressions. The sentry itself can be any
   type of object (but usually it's a function). */

pure_expr *pure_sentry(pure_expr *sentry, pure_expr *x);
pure_expr *pure_get_sentry(pure_expr *x);
bool pure_has_sentry(pure_expr *x);
pure_expr *pure_clear_sentry(pure_expr *x);

/* Pointer tags. As of Pure 0.45 this provides a way to keep track of C
   pointer types. The basic operations below allow you to set a tag on a Pure
   pointer value, to get the current tag, and to check the tag against a given
   value. In addition, a new tag can be created with the pure_make_tag()
   function. These operations give you direct access to the lowlevel pointer
   tagging machinery, in order to implement your own pointer typing schemes if
   necessary. (Normally this shouldn't be necessary, since the code generator
   already keeps track of pointer types in extern declarations now.) */

/* Define this symbol here so that modules can check for this feature. */
#define PURE_POINTER_TAG 1

pure_expr *pure_tag(int tag, pure_expr *x);
int pure_get_tag(const pure_expr *x);
bool pure_check_tag(int tag, const pure_expr *x);
int pure_make_tag();

/* Highlevel pointer tag operations. These associate pointer tags with actual
   pointer types in extern declarations. pure_pointer_tag() returns the tag
   for a given pointer type (this will automatically create a new tag if
   necessary). pure_pointer_type() retrieves the pointer type name associated
   with a pointer tag. This will be NULL in the case of an "anonymous" tag,
   which may have been created with pure_make_tag() above, or if the tag is
   simply unknown because it hasn't been created yet. Finally,
   pure_pointer_cast() allows you to create a copy of a given pointer value
   with the given tag on it, thereby "casting" the pointer to the new
   type. (This simply returns the original pointer value if the new tag is the
   same as x's current tag.) */

int pure_pointer_tag(const char *name);
const char *pure_pointer_type(int tag);
pure_expr *pure_pointer_cast(int tag, pure_expr *x);

/* Custom syntactic equality checking of tagged pointers. If registered, this
   function will be invoked by the same() function on two (non-NULL) pointers
   with the given type tag, instead of just comparing the pointer values. Note
   that if this callback is set, then the pointer type is also assumed to
   implement its own custom 'null' predicate in Pure land. */

typedef bool (*pure_equal_fun)(void*, void*);
void pure_pointer_add_equal(int tag, pure_equal_fun equal);
pure_equal_fun pure_pointer_equal(int tag);

/* Custom hashing of tagged pointers. If registered, this function will be
   invoked by the hash() function on (non-NULL) pointers with the given type
   tag, instead of just hashing the pointer value itself. Note that if you
   have registered a custom syntactic equality predicate, then you should also
   register a corresponding hash function, which should guarantee that two
   syntactically equal pointers have the same hash code. (If no such function
   is defined, then hash() may also fall back to hashing a custom print
   representation if it is defined; see below.) */

typedef uint32_t (*pure_hash_fun)(void*);
void pure_pointer_add_hash(int tag, pure_hash_fun hash);
pure_hash_fun pure_pointer_hash(int tag);

/* Custom pretty-printing of tagged pointers. This is supposed to be used from
   C/C++ code to implement custom pretty-printing of opaque C/C++ data
   structures, as a light-weight alternative to __show__ which only works with
   tagged pointers.

   pure_pointer_add_printer() registers a pretty-printer for a given tag. The
   second argument is the pretty-printing function which must be declared as
   const char *printer(void*), and is supposed to return a static buffer
   filled with the printable representation of the given pointer. The third
   argument may be used to specify a second callback of the form int
   printer(void*) which is used to calculate the (normalized) precedence of
   the print representation, so that the expression is printed in a
   syntactically correct way (you can leave this as NULL if this is not
   needed).

   For instance, here's a sample pretty printer which reproduces Pure's
   default unparsing of pointer values:

   const char *printer(void *p) {
     static char buf[100];
     sprintf(buf, "#<pointer %p>", p);
     return buf;
   }

   pure_pointer_printer() returns the pretty-printing function registered for
   the given pointer tag, or NULL if none has been registered. Likewise,
   pure_pointer_printer() returns the callback doing the precedence
   calculation (or NULL). This information is stored with the interpreter
   instance, and used by the interpreter's pretty-printing functions (unless
   overridden by custom __show__ rules). */

typedef const char *(*pure_printer_fun)(void*);
typedef int (*pure_printer_prec_fun)(void*);
void pure_pointer_add_printer(int tag, pure_printer_fun printer,
			      pure_printer_prec_fun prec);
pure_printer_fun pure_pointer_printer(int tag);
pure_printer_prec_fun pure_pointer_printer_prec(int tag);

/* Variable and constant definitions. These allow you to directly bind
   variable and constant symbols to pure_expr* values, as the 'let' and
   'const' constructs do in the Pure language. The functions return true if
   successful, false otherwise. */

bool pure_let(int32_t sym, pure_expr *x);
bool pure_def(int32_t sym, pure_expr *x);

/* Purge the definition of a global (constant, variable or function) symbol. */

bool pure_clear(int32_t sym);

/* Manage temporary definition levels (see the Pure manual for details).
   pure_save starts a new level, pure_restore returns to the previous level,
   removing all definitions of the current level. In either case the new level
   is returned. A zero return value of pure_save indicates an error condition,
   most likely because the maximum number of levels was exceeded.

   Note that the command line version of the interpreter starts at temporary
   level 1, while the standalone interpreters created with the public API (see
   below) start at level 0. Hence in the latter case you first need to invoke
   pure_save before you can define temporaries. */

uint32_t pure_save();
uint32_t pure_restore();

/* pure_savelevel() returns the current save level. */

uint32_t pure_savelevel();

/* pure_val() parses the given string, which must conform to the simple
   expression syntax, and returns the corresponding expression as is, i.e.,
   without evaluating it. NULL is returned in case of an error, in which case
   you can use lasterr() in the library API to check for error messages from
   the interpreter. */

pure_expr *pure_val(const char *s);

/* Like eval() and evalcmd() in the library API, the following routines
   evaluate Pure expressions and other Pure code, but, for convenience, the
   input is specified as a string. pure_eval() only executes ordinary Pure
   code, while pure_evalcmd() also executes interactive commands and captures
   their output; see the description of the library API routines for details.
   pure_eval() returns the (last) evaluated expression (if any), while
   pure_evalcmd only returns the output from interactive commands like 'show'
   as a string (NULL if none). Both routines also return NULL in case of an
   error; in that case you can use lasterr() in the library API to check for
   error messages from the interpreter. */

pure_expr *pure_eval(const char *s);
char *pure_evalcmd(const char *s);

/* pure_evalx() evaluates a (quoted) Pure expression, like eval() in the
   library API, but returns exceptions (if any) to the caller. In the latter
   case, NULL is returned, and the exception value is stored in *e. */

pure_expr *pure_evalx(pure_expr *x, pure_expr** e);

/* Logging of error messages and warnings from the compiler. This works
   independently from pure_eval() et al, so it can also be used to record
   messages while compiling Pure source loaded from script files. The messages
   can then be retrieved with lasterr() from the library API as usual. */

void pure_start_logging();
void pure_stop_logging();

/* The following routines provide standalone C/C++ applications with fully
   initialized interpreter instances which can be used together with the
   operations listed above. This is only needed for modules which are not to
   be loaded by the command line version of the interpreter. */

/* The pure_interp type serves as a C proxy for Pure interpreters. Pointers
   to these are used as C handles for the real Pure interpreter objects (which
   are actually implemented by a C++ class). If your application needs more
   elaborate control over interpreters as provided by this API, pure_interp*
   can be cast to interpreter* (cf. interpreter.hh in the Pure sources). */

typedef struct _pure_interp pure_interp;

/* Manage interpreter instances. The argc, argv parameters passed to
   pure_create_interp specify the command line arguments of the interpreter
   instance. This includes any scripts that are to be loaded on startup as
   well as any other options understood by the command line version of the
   interpreter. (Options like -i and -q won't have any effect, though, and the
   interpreter will always be in non-interactive mode.) The argv vector must
   be NULL-terminated, and argv[0] should be set to the name of the hosting
   application (usually the main program of the application). For convenience,
   you can also just pass in a NULL vector (and argc=0) to denote an empty
   parameter list.

   An application may use multiple interpreter instances, but only a single
   instance can be active at any one time. By default, the first created
   instance will be active, but you can switch between different instances
   with the pure_switch_interp function. The pure_delete_interp routine
   destroys an interpreter instance; if the destroyed instance is currently
   active, the active instance will be undefined afterwards, so you'll have to
   either create or switch to another instance before calling any other
   operations. The pure_current_interp function returns the currently active
   instance. If the application is hosted by the command line interpreter,
   this will return a handle to the command line interpreter if it is invoked
   before switching to any other interpreter instance.

   Note that when using different interpreter instances in concert, it is
   *not* possible to pass pure_expr* values created with one interpreter
   instance to another. Instead, you can use str (from the library API, see
   below) and pure_eval (see above) to first unparse the expression in the
   source interpreter and then reparse it in the target interpreter. */

pure_interp *pure_create_interp(int argc, char *argv[]);
void pure_delete_interp(pure_interp *interp);
void pure_switch_interp(pure_interp *interp);
pure_interp *pure_current_interp();

/* POSIX multithreading support. As the Pure runtime isn't thread-safe right
   now, it is *not* safe to concurrently run Pure code in a multithreaded
   program. This means that if you run multiple interpreter instances (or even
   a single instance) in a multithreaded C/C++ application then you'll have to
   serialize accesses to the runtime. The following routines implement a
   global interpreter lock (GIL) which lets you switch between different
   interpreter instances in a thread-safe way. Note that in a multithreaded
   application you'll have to explicitly call these whenever running an
   interpreter in a critical code section which may be run concurrently, even
   if there is only a single global interpreter instance. (Depending on your
   application, this may become a major bottleneck, so beware!)

   To use these functions, call pure_lock_interp() on the interpreter instance
   to be used when entering a critical section. This obtains the GIL, switches
   to the given interpreter (as with pure_switch_interp()) and returns the
   previously active interpreter instance (the previous pure_current_interp()).
   Save the returned instance in a local variable and do whatever processing
   is needed. When exiting the critical section, call pure_unlock_interp() on
   the saved interpreter instance which releases the GIL and restores the
   saved instance. (Note that you *must* call pure_unlock_interp() some time
   after pure_lock_interp() in order to prevent deadlocks.) */

pure_interp *pure_lock_interp(pure_interp *interp);
pure_interp *pure_unlock_interp(pure_interp *interp);

/* Eager compilation. This runs the JIT, as necessary, on the given global
   function and transitively all other functions it might use. This often
   increases startup times, since the given function and its callees are
   compiled beforehand, but might be useful for programs with a defined entry
   point where lazy compilation is not appropriate, such as realtime
   applications. NOTE: If the program doesn't have a defined entry point, the
   function symbol fno can also be zero or negative. In this case the JIT is
   run on each and every function of the program. This is *very* slow on
   startup, because it compiles all definitions no matter whether they are
   actually used by the running program, and thus should only be used as a
   last resort. */

void pure_interp_compile(pure_interp *interp, int32_t fno);

/* Interpreter-local storage (ILS). This works similar to pthread_create_key
   and friends. pure_interp_key() obtains a new key (!=0) to interpreter-local
   data (a void*). The key will initially be associated with NULL in all
   interpreters. You can get the pointer value for the current interpreter
   with pure_interp_get() and set its value with pure_interp_set(). Each key
   may be associated with a destroy function which is executed automatically
   on a corresponding interpreter-local pointer value if the interpreter goes
   out of existence. (This is usually free() if the interpreter-local pointer
   was obtained with malloc(); use NULL if this is not needed.) */

typedef uint32_t pure_interp_key_t;
pure_interp_key_t pure_interp_key(void (*destroy)(void*));
void pure_interp_set(pure_interp_key_t key, void *ptr);
void *pure_interp_get(pure_interp_key_t key);

/* Application-defined finalizers. This is for the benefit of addon modules
   which need to do their cleanup *before* the usual system atexit() handlers
   are called. The Pure interpreter main program and standalone compiled
   programs do this automatically. If you're running Pure in some other way
   (e.g., calling it yourself via C/C++) then you should call pure_finalize()
   when your main program is about to exit. */

int pure_atexit(void (*function)(void));
void pure_finalize(void);

/* END OF PUBLIC API. *******************************************************/

/* Stuff below this line is for internal use by the Pure interpreter. Don't
   call these directly, unless you know what you are doing. */

/* Create an interpreter instance from a dump of the internal tables. This is
   used in code generated by the batch compiler. */

pure_interp *pure_interp_main(int argc, char *argv[],
			      int32_t nsyms, char *syms,
			      pure_expr ***vars, void **vals,
			      int32_t *arities, void **externs,
			      pure_expr ***sstk, void **fptr);

/* Construct constant symbols and closures. */

pure_expr *pure_const(int32_t tag);
pure_expr *pure_clos(bool local, int32_t tag, uint32_t key, uint32_t n,
		     void *f, void *e, uint32_t m, /* m x pure_expr* */ ...);

/* Construct a literal application. */

pure_expr *pure_applc(pure_expr *x, pure_expr *y);

/* Additional bigint constructors. */

pure_expr *pure_int64(int64_t l);
pure_expr *pure_uint64(uint64_t l);
pure_expr *pure_bigint(int32_t size, const limb_t *limbs);

/* Additional bigint list, tuple and matrix constructors. These take the limbs
   of all elements in a single array. The offs array gives the offset of each
   bigint in the limbs array, and the sz array gives the size (and sign) of
   each bigint. */

pure_expr *pure_bigintlistv(size_t size, limb_t *limbs,
			    uint32_t *offs, int32_t *sz);
pure_expr *pure_bigintlistv2(size_t size, limb_t *limbs,
			     uint32_t *offs, int32_t *sz, pure_expr *tail);
pure_expr *pure_biginttuplev(size_t size, limb_t *limbs,
			     uint32_t *offs, int32_t *sz);
pure_expr *pure_bigintmatrixv(size_t nrows, size_t ncols, limb_t *limbs,
			      uint32_t *offs, int32_t *sz);

pure_expr *pure_bigintlistvq(size_t size, limb_t *limbs,
			     uint32_t *offs, int32_t *sz);
pure_expr *pure_bigintlistv2q(size_t size, limb_t *limbs,
			      uint32_t *offs, int32_t *sz, pure_expr *tail);
pure_expr *pure_biginttuplevq(size_t size, limb_t *limbs,
			      uint32_t *offs, int32_t *sz);

/* Additional string list, tuple and matrix constructors. These take a char
   array containing all (0-terminated) strings as an argument. The offs array
   gives the offset of each string in the chars array. The strings are assumed
   to be in utf-8 encoding. */

pure_expr *pure_strlistv(size_t size, char *chars, uint32_t *offs);
pure_expr *pure_strlistv2(size_t size, char *chars, uint32_t *offs,
			  pure_expr *tail);
pure_expr *pure_strtuplev(size_t size, char *chars, uint32_t *offs);
pure_expr *pure_strmatrixv(size_t nrows, size_t ncols, char *chars,
			   uint32_t *offs);

pure_expr *pure_strlistvq(size_t size, char *chars, uint32_t *offs);
pure_expr *pure_strlistv2q(size_t size, char *chars, uint32_t *offs,
			   pure_expr *tail);
pure_expr *pure_strtuplevq(size_t size, char *chars, uint32_t *offs);

/* Compare a bigint or string expression against a constant value. This is
   used by the pattern matching code. */

int32_t pure_cmp_bigint(pure_expr *x, int32_t size, const limb_t *limbs);
int32_t pure_cmp_string(pure_expr *x, const char *s);

/* Check against a user-defined type tag. This is used by the pattern matching
   code. pure_safe_typecheck is the same as pure_typecheck, but never collects
   its argument; this is used in global pattern bindings (let, const). */

bool pure_typecheck(int32_t tag, pure_expr *x);
bool pure_safe_typecheck(int32_t tag, pure_expr *x);

/* Get the string value of a string expression in the system encoding. Each
   call returns a new string, pure_free_cstrings() frees the temporary
   storage. This is only to be used internally, to unbox string arguments in
   the C interface. */

char *pure_get_cstring(pure_expr *x);
void pure_free_cstrings();

/* Convert a bigint expression to a pointer (mpz_t) or a 64 or 32 bit
   integer. This is used to marshall bigint arguments in the C interface. */

void *pure_get_bigint(pure_expr *x);
int64_t pure_get_int64(pure_expr *x);
int32_t pure_get_int(pure_expr *x);

/* Convert a matrix expression to a pointer to the corresponding GSL matrix
   struct or a pointer to the data itself. This allows to pass matrix data per
   reference to generic pointer arguments. It is used to marshall matrix
   arguments in the C interface. */

void *pure_get_matrix(pure_expr *x);
void *pure_get_matrix_data(pure_expr *x);

/* These work like pure_get_matrix_data above, but do the necessary
   conversions to and from conformant target data types (char <-> int, short
   <-> int, int64_t <-> int, float <-> double) on the fly, copying the data to
   temporary storage if necessary. These operations maintain the illusion that
   the matrices are in contiguous storage (copying the data to temporary
   storage if the original matrix has a stride which doesn't match its row
   length) and that (in spite of the conversions) the matrices are passed per
   reference so that their data can be modified in-place; results are
   converted back to the matrix as necessary. pure_free_cvectors is to be
   called afterwards to free the temporary storage and convert back results. */

void pure_free_cvectors();

void *pure_get_matrix_data_byte(pure_expr *x);
void *pure_get_matrix_data_short(pure_expr *x);
void *pure_get_matrix_data_int(pure_expr *x);
void *pure_get_matrix_data_int64(pure_expr *x);
void *pure_get_matrix_data_float(pure_expr *x);
void *pure_get_matrix_data_double(pure_expr *x);

/* Variations of the above which allow matrices of the appropriate type to be
   passed for a vector of pointers (pointing to the rows of the matrix) in
   temporary storage. Any necessary data type conversions are done on the
   fly. pure_free_cvectors is to be called afterwards to free the temporary
   storage and convert back results. */

void *pure_get_matrix_vector_byte(pure_expr *x);
void *pure_get_matrix_vector_short(pure_expr *x);
void *pure_get_matrix_vector_int(pure_expr *x);
void *pure_get_matrix_vector_int64(pure_expr *x);
void *pure_get_matrix_vector_float(pure_expr *x);
void *pure_get_matrix_vector_double(pure_expr *x);

/* In addition, the following routines allow symbolic matrices of pointers
   and/or strings to be passed as void** and char** to a C function. The
   vector is always terminated with a NULL pointer. The data pointed to can be
   modified by the callee (with care), but the input matrix itself is never
   modified. In the char** case, the Pure strings are converted to the system
   encoding, so the callee only sees a copy of the string data.
   pure_free_cvectors is to be called afterwards to free the temporary storage
   for the C vector. */

void *pure_get_matrix_vector_void(pure_expr *x);
void *pure_get_matrix_vector_char(pure_expr *x);

/* Additional matrix constructors. These work like pure_matrix_rowsl and
   pure_matrix_columnsl in the public API, but are intended to be called
   directly from generated code and raise the appropriate Pure exceptions in
   case of an error condition. */

pure_expr *pure_matrix_rows(uint32_t n, ...);
pure_expr *pure_matrix_columns(uint32_t n, ...);

/* These are used internally to construct quoted matrices. */

pure_expr *pure_matrix_rowsq(uint32_t n, ...);
pure_expr *pure_matrix_columnsq(uint32_t n, ...);
pure_expr *pure_matrix_rowsvq(uint32_t n, pure_expr **xs);
pure_expr *pure_matrix_columnsvq(uint32_t n, pure_expr **xs);

/* Execute a closure. If the given expression x (or x y in the case of
   pure_apply) is a parameterless closure (or a saturated application of a
   closure), call it with the appropriate parameters and environment, if
   any. Otherwise just return x (or the literal application x y). */

pure_expr *pure_call(pure_expr *x);
pure_expr *pure_apply(pure_expr *x, pure_expr *y);

/* This is like pure_call above, but only executes anonymous parameterless
   closures (thunks), and returns the result in that case (which is then
   memoized). */

pure_expr *pure_force(pure_expr *x);

/* Activation records (exceptions and indirect tail calls). */

typedef struct _pure_aframe {
  struct _pure_aframe *prev;	// previous frame
  jmp_buf jmp;			// landing pad
  pure_expr* e;			// exception value (if any)
  size_t sz;			// size of shadow stack
  // information to keep track of indirect tail calls
  void *fp;			// function pointer
  uint32_t n, m;		// arg and environment counts
  pure_expr **argv;		// argument vector
  uint32_t count;		// trampoline counter
} pure_aframe;

/* NOTE: pure_throw() and pure_trap() are in the library API now. */

/* Throw an 'unresolved_external' exception. */

void pure_unresolved();

/* Throw a 'signal SIGFPE' exception. This is used to signal division by
   zero. */

void pure_sigfpe(void);

/* Execute a parameterless fbox x and return its result. If an exception
   occurs while x is executed, apply h to the value of the exception
   instead. */

pure_expr *pure_catch(pure_expr *h, pure_expr *x);

/* Run a Pure function and catch exceptions. If everything goes normal,
   pure_invoke returns the return value of the executed function. Otherwise it
   returns 0 and sets e to the exception value, as given by pure_throw().
   NOTE: This is provided for internal purposes and for backward
   compatibility. Applications should use pure_funcallx instead. */

pure_expr *pure_invoke(void *f, pure_expr** e);

/* Manage arguments of a function call. pure_new_args counts references on a
   given collection of arguments in preparation for a function call, while
   pure_free_args collects the arguments of a function call. In both cases the
   arguments follow the given parameter count n. For pure_free_args, the first
   expression argument is the return value; if not NULL, an extra reference is
   temporarily counted on this expression so that it doesn't get freed if the
   return value happens to be a (subterm of an) argument or environment
   expression. These functions are only to be used for internal calls (apply,
   catch, etc.); for calls which are to be visible on the shadow stack see
   pure_push_args and pure_pop_args below. */

void pure_new_args(uint32_t n, ...);
void pure_free_args(pure_expr *x, uint32_t n, ...);

/* The following are similar to pure_new_args and pure_free_args above, but
   also maintain an internal shadow stack, for the purpose of keeping track of
   dynamically allocated environment vectors, for cleaning up function
   arguments and environments, and for providing data needed by the symbolic
   debugger. The arguments n and m denote the number of function parameters
   and environment variables, respectively. pure_push_args takes the function
   parameters followed by the environment values as additional arguments and
   returns the index of the first environment variable on the shadow stack (0
   if none). Parameters and environment are reclaimed with a corresponding
   pure_pop_args/pure_pop_tail_args call; pure_pop_tail_args is to be called
   instead of pure_pop_args in case of a (potential) tail call, since in this
   case the new stack frame of the tail-called function is already on the
   stack and thus the previous stack frame is to be popped instead of the
   current one. */

uint32_t pure_push_args(uint32_t n, uint32_t m, ...);
void pure_pop_args(pure_expr *x, uint32_t n, uint32_t m);
void pure_pop_tail_args(pure_expr *x, uint32_t n, uint32_t m);

/* Optimize the special case of a single argument without environment to be
   pushed/popped. */

void pure_push_arg(pure_expr *x);
void pure_pop_arg(pure_expr *x);
void pure_pop_tail_arg(pure_expr *x);

/* Do extra checking for signals and stack overflow. */

void pure_checks(void);

/* Debugging support. Experimental. */

void pure_debug(int32_t tag, const char *format, ...);
void pure_debug_rule(void *e, void *r);
void pure_debug_redn(void *e, void *r, pure_expr *x);

/* Pure runtime type information (RTTI). This maintains the necessary
   information about type tags and pointer type names and tags and makes these
   available to Pure programs. */

void pure_add_rtty(int32_t tag, int argc, void *fp);
void pure_add_rtti(const char *name, int tag);

/* Internal Faust interface (Pure 0.45 and later). These provide the necessary
   helper functions to create useful information about a Faust dsp. NOTE: You
   should never have to call any of these directly; instead, applications
   should use the dspname::info and dspname::meta functions generated by the
   Pure compiler when the Faust dsp is loaded. */

void *faust_float_ui();
void *faust_double_ui();
void faust_free_ui(void *ui);
pure_expr *faust_make_info(int n_in, int n_out, void *ui, const char *modname);

void *faust_new_metadata();
void faust_free_metadata(void *m);
pure_expr *faust_make_metadata(void *m);

/* This is to be called in initialization functions to make RTTI (see below)
   available in batch-compiled scripts. */
void faust_add_rtti(const char *name, int tag, bool dbl);

/* Faust RTTI. Currently this comprises the module name of a dsp (faust_name,
   a string) and its sample format (faust_dbl, a flag which is true iff double
   samples and control values are used; false indicates single precision).
   Moreover, faust_method gives access to the interface functions of a dsp,
   specified either by its name (a string) or an existing dsp object. (This
   will return a closure in all cases except for the 'new' and 'meta' methods
   which take no arguments and thus will be evaluated immediately.) Finally,
   faust_mods builds a list of all Faust modules currently loaded; each
   element is a hash pair name=>dbl of the module name and the sample
   format. (These actually belong to the library API, but we put them here to
   keep the Faust-related stuff together.) */

pure_expr *faust_name(pure_expr *dsp);
pure_expr *faust_dbl(pure_expr *dsp);
pure_expr *faust_method(pure_expr *dsp, const char *method);
pure_expr *faust_mods();

/* Load a Faust dsp in bitcode format. This works like 'using "dsp:..."' but
   doesn't perform any library search, so the given filename is taken as is,
   except that the .bc filename extension is added if necessary. The function
   fails if there was an error loading the file, leaving an error message in
   lasterr(), otherwise it returns (). */

pure_expr *faust_load(const char *name);

/* LIBRARY API. *************************************************************/

/* Add any stuff that is needed in the standard library here. Applications and
   external C modules may call these, but be warned that these APIs are
   subject to change without further notice. */

/* Throw the given expression (which may also be NULL) as an exception. */

void pure_throw(pure_expr* e);

/* Configure signal handlers. The second argument is the signal number, the
   first the action to take (-1 = ignore, 1 = handle, 0 = default). */

void pure_trap(int32_t action, int32_t sig);

/* Trigger debugging and tracing at a given point in the program. These
   require that the interpreter is running in debugging mode. The debugger
   will be invoked at the next opportunity (usually when a function is called
   or a reduction is completed). */

void pure_break(void);
void pure_trace(void);

/* Construct arithmetic sequences. */

pure_expr *pure_int_seq(int32_t from, int32_t to, int32_t step);
pure_expr *pure_double_seq(double from, double to, double step);

/* Same for row and column vectors. */

pure_expr *pure_int_rowvect(int32_t from, int32_t to, int32_t step);
pure_expr *pure_double_rowvect(double from, double to, double step);
pure_expr *pure_int_colvect(int32_t from, int32_t to, int32_t step);
pure_expr *pure_double_colvect(double from, double to, double step);

/* Interface to the C qsort() routine. The first argument is the "less-than"
   predicate to be used, the second argument the list to be sorted. A
   'failed_cond' exception is thrown if a comparison doesn't evaluate to a
   machine int. */

pure_expr *pure_sort(pure_expr *p, pure_expr *xs);

/* Conversions between numeric and pointer types. The input argument must be
   an expression denoting an int, double, bigint or pointer value. The numeric
   value of a pointer is its address, cast to a suitably large integer type,
   which can be converted to/from an integer, but not a double value. Strings
   and matrices can be converted to pointers as well, but not the other way
   round. */

pure_expr *pure_intval(pure_expr *x);
pure_expr *pure_dblval(pure_expr *x);
pure_expr *pure_bigintval(pure_expr *x);
pure_expr *pure_pointerval(pure_expr *x);

/* Convert a double to a rational number, without rounding. Returns a pair n,d
   of two bigint values, where n is the numerator and d the denominator. */

pure_expr *pure_rational(double d);

/* Random number generator (Mersenne twister). See this URL for details:
   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html */

void init_genrand(unsigned s);
void init_genrand_array(unsigned *init_key, int key_length);
unsigned genrand_int32(void);
int genrand_int31(void);
double genrand_real1(void);
double genrand_real2(void);
double genrand_real3(void);
double genrand_res53(void);

/* Construct a "byte string" from a string. The result is a raw pointer object
   pointing to the converted string. The original string is copied (and, in
   the case of pure_byte_cstring, converted to the system encoding). The
   resulting byte string is a malloc'ed pointer which can be used like a C
   char* (using pointer arithmetic etc.; the usual caveats apply), and has to
   be freed by the caller when no longer needed. */

pure_expr *pure_byte_string(const char *s);
pure_expr *pure_byte_cstring(const char *s);

/* Bigint arithmetic. */

pure_expr *bigint_neg(mpz_t x);

pure_expr *bigint_add(mpz_t x, mpz_t y);
pure_expr *bigint_sub(mpz_t x, mpz_t y);
pure_expr *bigint_mul(mpz_t x, mpz_t y);
pure_expr *bigint_div(mpz_t x, mpz_t y);
pure_expr *bigint_mod(mpz_t x, mpz_t y);
pure_expr *bigint_pow(mpz_t x, uint32_t y);
pure_expr *bigint_shl(mpz_t x, int32_t y);
pure_expr *bigint_shr(mpz_t x, int32_t y);

pure_expr *bigint_not(mpz_t x);

pure_expr *bigint_and(mpz_t x, mpz_t y);
pure_expr *bigint_or(mpz_t x, mpz_t y);

pure_expr *bigint_gcd(mpz_t x, mpz_t y);
pure_expr *bigint_lcm(mpz_t x, mpz_t y);

int32_t bigint_cmp(mpz_t x, mpz_t y);

/* String operations. In difference to the string operations from the C
   library, these assume a utf-8 encoded string and will count and index
   individual characters accordingly. */

bool string_null(const char *s);
uint32_t string_size(const char *s);
pure_expr *string_char_at(const char *s, uint32_t n);

/* Note that indexing an utf-8 string takes linear time and is thus slow. As a
   remedy, we offer the following linear-time operation which converts an
   entire string to a list of its utf-8 characters in one go. */

pure_expr *string_chars(const char *s);

/* Concatenation, splitting, substrings and finding a substring in a string. */

pure_expr *string_concat(const char* s, const char *t);
pure_expr *string_concat_list(pure_expr *xs);
pure_expr *string_join(const char *delim, pure_expr *xs);
pure_expr *string_split(const char *delim, const char *s);
pure_expr *string_substr(const char* s, uint32_t pos, uint32_t size);
int32_t string_index(const char* s, const char *t);

/* Conversions between utf-8 characters and numbers. These convert a Unicode
   character code (code point) to the corresponding utf-8 character, and vice
   versa. */

pure_expr *string_chr(uint32_t n);
pure_expr *string_ord(const char *c);

/* Reflection operations. Note that most of these are restricted or completely
   dysfunctional in batch-compiled scripts, please check the Pure manual for
   details. */

/* Convert a Pure expression to a string and vice versa. The result of str()
   is a malloc'ed string in the system encoding which must be freed by the
   caller. Note that eval() can be invoked on either a string or any other
   Pure expression (subject to the same constraints as constant values). In
   the former case, it parses and executes any Pure source, which can be
   employed, e.g., to add new rules to the executing program at runtime. In
   the latter case the given expression is compiled and reevaluated. The
   result of eval() is the (last) computed expression if any, NULL otherwise;
   in the latter case you can inspect the result of lasterr() below to
   determine whether there were any compilation errors. */

char *str(const pure_expr *x);
pure_expr *eval(pure_expr *x);

/* Variation of str() which returns a Pure string as the result, or fails
   (returning 0). */

pure_expr *pure_str(const pure_expr *x);

/* A special version of str() which pretty-prints quoted specials like
   '(x->x+1) only. This can be used with __show__. */

pure_expr *__str__(pure_expr *x);

/* evalcmd() is like eval(), but executes interactive commands and returns
   their output as a string. Only the regular output of a few commands can be
   captured right now, most notably 'show' and 'completion_matches'; otherwise
   the result string will be empty. No other results are returned. */

pure_expr *evalcmd(pure_expr *x);

/* reduce evaluates a given expression in a given dynamic environment of local
   function bindings. */

pure_expr *reduce(pure_expr *locals, pure_expr *x);

/* After an invokation of eval() or evalcmd(), this returns error messages
   from the interpreter (an empty string if none). Use clear_lasterr() to
   clear the messages. */

const char *lasterr();
void clear_lasterr();

/* More detailed error information. This returns a list of individual error
   messages, along with position information (filename, from_line, from_col,
   to_line, to_col), if available. */

pure_expr *lasterrpos();

/* In interactive mode, this returns the most recent toplevel expression
   printed by the interpreter (NULL if none). */

pure_expr *lastres();

/* Get the list of rewriting rules defining a function, type or macro. This
   uses the quoted runtime representation of rules and specials described in
   the Pure manual. The empty list is returned if no corresponding definition
   is found. */

pure_expr *get_fundef(pure_expr *f);
pure_expr *get_typedef(pure_expr *f);
pure_expr *get_macdef(pure_expr *f);

/* Get the definition of an interface type. get_interface() returns the list
   of interface patterns, while get_interface_typedef() returns the list of
   actual patterns for the type (which might be used with add_typedef to
   recreate the type as a concrete type). The empty list is returned if no
   corresponding definition is found, or (in the latter case) if the type
   hasn't been instantiated yet (i.e., the interface functions haven't been
   implemented yet, or they define an empty pattern set). Note that Pure
   allows you to have *both* an interface and a regular (concrete) definition
   of a type, in which case both get_typedef() and get_interface() will be
   defined, and get_interface_typedef() may well return a result different
   from get_typedef(). */

pure_expr *get_interface(pure_expr *f);
pure_expr *get_interface_typedef(pure_expr *f);

/* Get definitions of variables and constants. The result takes the form
   [var-->val] if the symbol is defined, otherwise the empty list is
   returned. */

pure_expr *get_vardef(pure_expr *f);
pure_expr *get_constdef(pure_expr *f);

/* Add rewriting rules for a function, type or macro, using the same runtime
   representation as returned by get_fundef(), get_typedef() and get_macdef().
   Like eval(), these routines may return NULL and leave an error message in
   lasterr(). */

pure_expr *add_fundef(pure_expr *x);
pure_expr *add_typedef(pure_expr *x);
pure_expr *add_macdef(pure_expr *x);

/* Add patterns for an interface type. Note that f must be the symbol denoting
   the interface. The list x should be the list of interface patterns, in the
   same format as returned by get_interface(). */

pure_expr *add_interface(pure_expr *f, pure_expr *x);

/* Same as above, but add the given rules x before a given rule y. In this
   case, all rules must be for the same head symbol which matches the head
   symbol of the rule y. */

pure_expr *add_fundef_at(pure_expr *y, pure_expr *x);
pure_expr *add_typedef_at(pure_expr *y, pure_expr *x);
pure_expr *add_macdef_at(pure_expr *y, pure_expr *x);

/* Add the given interface patterns x before a given pattern y.  */

pure_expr *add_interface_at(pure_expr *f, pure_expr *y, pure_expr *x);

/* Set global variables or constants. The argument is a list of rules in the
   format var-->val; the left-hand side of each rule *must* be a symbol (no
   pattern matching is performed by these routines). These work pretty much
   like pure_let() and pure_def() in the public API, but take their input in
   rule form and report errors in lasterr() like add_fundef() et al above. */

pure_expr *add_vardef(pure_expr *x);
pure_expr *add_constdef(pure_expr *x);

/* Delete rewriting rules for a function, type or macro. The argument is the
   rule to be deleted, which is specified using the same runtime
   representation as returned by get_fundef(), get_typedef() and
   get_macdef(). The result is () if a matching rule was found (and deleted),
   NULL otherwise. */

pure_expr *del_fundef(pure_expr *x);
pure_expr *del_typedef(pure_expr *x);
pure_expr *del_macdef(pure_expr *x);

/* Delete patterns for an interface type. */

pure_expr *del_interface(pure_expr *f, pure_expr *x);

/* Delete global variables or constants. Here the argument is just the
   variable or constant symbol to be deleted. */

pure_expr *del_vardef(pure_expr *x);
pure_expr *del_constdef(pure_expr *x);

/* Expression serialization. These operations can be used to safely transfer
   expression data to/from persistent storage and between different processes
   in a compact format. blob() stores the expression contents as a binary
   object (returned as a cooked pointer object which frees itself when
   garbage-collected), val() retrieves the serialized expression. blobp() does
   a quick check for a valid blob object, blob_size() and blob_crc() determine
   the size (in bytes) and crc checksum of a blob, respectively. Note that
   val() may fail even if blobp() returns true, because for performance
   reasons blobp() only does a quick plausibility check on the header
   information of the blob, whereas val() also performs a crc check and
   verifies data integrity. Also note that val() is overloaded; on strings, it
   invokes pure_val() (see above). */

/* The current implementation has some limitations. Specifically, runtime data
   (local closures and pointers) can't be serialized right now, causing blob()
   to fail (however, it *is* possible to transfer a NULL pointer, or a global
   function, provided that the function exists in both the sending and the
   receiving process). Sharing of subexpressions will in general be preserved,
   but shared tails in lists and tuples can't be reconstructed unless the
   entire list/tuple is shared. Finally, val() may fail even on a valid blob
   if there is a conflict in symbol fixities between the symbol tables of the
   sending and the receiving process; to avoid this, make sure that symbol
   declarations in the sending and the receiving script match up. */

pure_expr *blob(pure_expr *x);
pure_expr *val(pure_expr *x);
bool blobp(pure_expr *x);
pure_expr *blob_size(pure_expr *x);
pure_expr *blob_crc(pure_expr *x);

/* Basic matrix operations. These work with all supported GSL matrix types.
   matrix_size determines the number of elements in a matrix, matrix_dim the
   number of rows and columns, which are returned as a pair (n,m).
   matrix_stride returns the real row length of a matrix, which may be larger
   than its column count if the matrix is actually a slice of a larger matrix.
   (This value won't be of much use in Pure programs since there is no way to
   access the "extra" elements in each row, but may be useful if the data
   pointer is passed to an external C routine.) matrix_type determines the
   exact type of a matrix, returning an integer denoting the subtype tag (0 =
   symbolic, 1 = double, 2 = complex, 3 = integer matrix; -1 is returned if
   the given object is not a matrix). */

uint32_t matrix_size(pure_expr *x);
pure_expr *matrix_dim(pure_expr *x);
uint32_t matrix_stride(pure_expr *x);
int32_t matrix_type(pure_expr *x);

/* Matrix elements can be retrieved either by a single index (using row-major
   order), or by row and column index. All indices are zero-based. Indices
   aren't range-checked, if this is needed you have to do it beforehand,
   checking against matrix_size or matrix_dim above. In addition, matrix_check
   can be used to verify the dimensions of a matrix. */

bool matrix_check(pure_expr *x, uint32_t n, uint32_t m);
pure_expr *matrix_elem_at(pure_expr *x, int32_t i);
pure_expr *matrix_elem_at2(pure_expr *x, int32_t i, int32_t j);

/* The following operation retrieves a slice a.k.a. submatrix of a matrix and
   returns it as a new matrix object. The result matrix shares the underlying
   storage with the input matrix (i.e., matrix elements are *not* copied) and
   so this is a comparatively cheap operation. Indices are zero-based and are
   clamped to the available index range automatically. */

pure_expr *matrix_slice(pure_expr *x, int32_t i1, int32_t j1,
			int32_t i2, int32_t j2);

/* Convert a matrix to a new matrix with same size but different dimensions.
   Reuse the storage of the original matrix if its data is contiguous. */

pure_expr *matrix_redim(pure_expr *x, int32_t n, int32_t m);

/* Retrieve (sub-,super-)diagonals of a matrix, as a 1xn matrix. */

pure_expr *matrix_diag(pure_expr *x);
pure_expr *matrix_subdiag(pure_expr *x, int32_t k);
pure_expr *matrix_supdiag(pure_expr *x, int32_t k);

/* Create a (sub-,super-)diagonal matrix from a vector. The result is of the
   same type (double, complex, int, symbolic) as the vector. */

pure_expr *matrix_diagm(pure_expr *x);
pure_expr *matrix_subdiagm(pure_expr *x, int32_t k);
pure_expr *matrix_supdiagm(pure_expr *x, int32_t k);

/* Matrix construction. These work like the corresponding functions in the
   public API, but take their input from a Pure list and raise the appropriate
   exception in case of dimension mismatch. */

pure_expr *matrix_rows(pure_expr *xs);
pure_expr *matrix_columns(pure_expr *xs);

/* Construct a matrix from a (symbolic) matrix of other matrices and/or
   scalars. This works like a combination of matrix_rows and matrix_columns,
   but draws its input from a matrix instead of a list of matrices, and
   preserves the overall layout of the "host" matrix. The net effect is that
   the host matrix is flattened out. If all elements of the input matrix are
   scalars already, the input matrix is returned unchanged. */

pure_expr *matrix_matcat(pure_expr *x);

/* Transpose a matrix. The resulting matrix has the rows of the original
   matrix as its columns, and vice versa. */

pure_expr *matrix_transpose(pure_expr *x);

/* Convert between different types of numeric and symbolic matrices. Also
   convert lists to rowvectors, if the elements are already of the
   corresponding type. */

pure_expr *matrix_double(pure_expr *x);
pure_expr *matrix_complex(pure_expr *x);
pure_expr *matrix_int(pure_expr *x);
pure_expr *matrix_symbolic(pure_expr *x);

/* Extract the real and imaginary parts of a numeric matrix. If the input is a
   complex matrix, the result is a new double matrix. Otherwise the type of
   the result is the same as that of the input matrix (in this case matrix_re
   just returns the same matrix, and matrix_im returns a new zero matrix of
   the same dimensions). */

pure_expr *matrix_re(pure_expr *x);
pure_expr *matrix_im(pure_expr *x);

/* Complex conjugate of a numeric matrix. In the case of int and double
   matrices, this returns just the matrix itself. */

pure_expr *matrix_conj(pure_expr *x);

/* Create a matrix object of the given dimensions which uses the given pointer
   p as its underlying storage. There are no checks whatsoever and the data is
   *not* copied, so the caller is responsible for making sure that the memory
   has the right size, is properly initialized and is not freed while the
   matrix is still in use. The memory is *not* freed when the matrix is
   garbage-collected but remains in the ownership of the caller. */

pure_expr *matrix_from_double_array_nodup(uint32_t n, uint32_t m, void *p);
pure_expr *matrix_from_complex_array_nodup(uint32_t n, uint32_t m, void *p);
pure_expr *matrix_from_int_array_nodup(uint32_t n, uint32_t m, void *p);

/* The following routines work like the above, but copy the data to newly
   allocated memory, so the original data can be freed after the call.
   Moreover, the source pointer p may also be NULL in which case the new
   matrix is filled with zeros instead. */

pure_expr *matrix_from_double_array(uint32_t n, uint32_t m, void *p);
pure_expr *matrix_from_complex_array(uint32_t n, uint32_t m, void *p);
pure_expr *matrix_from_int_array(uint32_t n, uint32_t m, void *p);

/* Copy data from the given matrix to the given data pointer, which is then
   returned. If p is NULL then memory of sufficient size is malloc'ed;
   otherwise p must point to a memory area of sufficient size. */

void *matrix_to_double_array(void *p, pure_expr *x);
void *matrix_to_complex_array(void *p, pure_expr *x);
void *matrix_to_int_array(void *p, pure_expr *x);

/* Additional routines for alternative base types. These work like the
   routines above but take data consisting of base types which are not
   directly supported by Pure GSL matrices: float, complex float, int64_t,
   short, byte. */

pure_expr *matrix_from_float_array(uint32_t n, uint32_t m, void *p);
pure_expr *matrix_from_complex_float_array(uint32_t n, uint32_t m, void *p);
pure_expr *matrix_from_int64_array(uint32_t n, uint32_t m, void *p);
pure_expr *matrix_from_short_array(uint32_t n, uint32_t m, void *p);
pure_expr *matrix_from_byte_array(uint32_t n, uint32_t m, void *p);

void *matrix_to_float_array(void *p, pure_expr *x);
void *matrix_to_complex_float_array(void *p, pure_expr *x);
void *matrix_to_int64_array(void *p, pure_expr *x);
void *matrix_to_short_array(void *p, pure_expr *x);
void *matrix_to_byte_array(void *p, pure_expr *x);

/* Optimized list-like matrix functions, by Scott E. Dillard. */

pure_expr* matrix_all(pure_expr *p, pure_expr *x);
pure_expr* matrix_any(pure_expr *p, pure_expr *x);
void matrix_do(pure_expr *f, pure_expr *x);
pure_expr* matrix_map(pure_expr *f, pure_expr *x);
pure_expr* matrix_zipwith(pure_expr *f, pure_expr *x, pure_expr *y);
pure_expr* matrix_zipwith3(pure_expr *f, pure_expr *x, pure_expr *y,
			   pure_expr *z);
pure_expr* matrix_filter(pure_expr *p, pure_expr *x);
pure_expr* matrix_dropwhile(pure_expr *p, pure_expr *x);
pure_expr* matrix_takewhile(pure_expr *p, pure_expr *x);
pure_expr* matrix_foldl(pure_expr *f, pure_expr *z, pure_expr *x);
pure_expr* matrix_foldl1(pure_expr *f, pure_expr *x);
pure_expr* matrix_foldr(pure_expr *f, pure_expr *z, pure_expr *x);
pure_expr* matrix_foldr1(pure_expr *f, pure_expr *x);
pure_expr* matrix_scanl(pure_expr *f, pure_expr *z, pure_expr *x);
pure_expr* matrix_scanl1(pure_expr *f, pure_expr *x);
pure_expr* matrix_scanr(pure_expr *f, pure_expr *z, pure_expr *x);
pure_expr* matrix_scanr1(pure_expr *f, pure_expr *x);

/* Additional record functions. Records are represented as symbolic vectors of
   hash pairs (key=>value) with symbols or strings as keys. Some specialized
   functions are provided here to implement element lookup and non-destructive
   updates of records in a reasonably efficient manner. */

bool record_check(pure_expr *x);
bool record_member(pure_expr *x, pure_expr *y);
pure_expr* record_elem_at(pure_expr *x, pure_expr *y);
pure_expr* record_update(pure_expr *x, pure_expr *y, pure_expr *z);
pure_expr* record_delete(pure_expr *x, pure_expr *y);
pure_expr* record_pack(pure_expr *x);

/* Compute a 32 bit hash code of a Pure expression. This makes it possible to
   use arbitary Pure values as keys in a hash table. */

uint32_t hash(pure_expr *x);

/* Check whether two objects are the "same" (syntactically). */

bool same(pure_expr *x, pure_expr *y);

/* Generic type-checking predicate. This works with built-in as well as with
   user-defined types. */

pure_expr* typep(pure_expr *ty, pure_expr *x);

/* Check for different kinds of closures: named functions, anonymous
   functions (lambdas) and thunks. */

bool funp(const pure_expr *x);
bool lambdap(const pure_expr *x);
bool thunkp(const pure_expr *x);

/* Check for any symbol (this also includes operator and nonfix symbols) and
   free variable symbols. Note that varp always returns false for operator and
   nonfix symbols, but returns true for *any* symbol that could in principle
   be bound to a value, either globally or locally, which holds even if the
   symbol is currently bound to a function, macro or constant. */

bool symbolp(const pure_expr *x);
bool varp(const pure_expr *x);

/* Get the argument count of a closure (named, anonymous or thunk), i.e., the
   number of arguments it needs to be saturated. Returns -1 if not a closure
   or the closure is over-saturated. */

int nargs(const pure_expr *x);

/* Determine the arity and fixity of an operator symbol. arity is 0, 1 or 2
   for nullary, unary and binary symbols, respectively, -1 for symbols without
   a fixity declaration or other kinds of objects. fixity is encoded as a
   2-digit number 10n+m where n is the precedence level (ranging from 0 to
   PREC_MAX; PREC_MAX denotes the precedence of primary expressions) and m
   indicates the actual fixity (0 = infix, 1 = infixl, 2 = infixr, 3 = prefix,
   4 = postfix). For non-symbol objects, fixity is always NPREC_MAX =
   10*PREC_MAX. */

int arity(const pure_expr *x);
int fixity(const pure_expr *x);

/* Direct memory accesses. Use these with care. In particular, note that the
   pointer_put_expr() routine doesn't do any reference counting by itself, so
   you'll have to use the memory management routines above to do that. */

int32_t pointer_get_byte(void *ptr);
int32_t pointer_get_short(void *ptr);
int32_t pointer_get_int(void *ptr);
int64_t pointer_get_int64(void *ptr);
long pointer_get_long(void *ptr);
double pointer_get_float(void *ptr);
double pointer_get_double(void *ptr);
char *pointer_get_string(void *ptr);
void *pointer_get_pointer(void *ptr);
pure_expr *pointer_get_expr(void *ptr);

void pointer_put_byte(void *ptr, int32_t x);
void pointer_put_short(void *ptr, int32_t x);
void pointer_put_int(void *ptr, int32_t x);
void pointer_put_int64(void *ptr, int64_t x);
void pointer_put_long(void *ptr, long x);
void pointer_put_float(void *ptr, double x);
void pointer_put_double(void *ptr, double x);
void pointer_put_string(void *ptr, const char *x);
void pointer_put_pointer(void *ptr, void *x);
void pointer_put_expr(void *ptr, pure_expr *x);

/* Get the address of a C symbol at runtime. The library containing the symbol
   must already be loaded. Note that this can in fact be any kind of
   externally visible C symbol, so it's also possible to get the addresses of
   global variables. The result is returned as a pointer. The function fails
   if the symbol was not found. */

pure_expr *pure_addr(const char *s);

/* Initialize a bunch of variables with useful system constants. */

void pure_sys_vars(void);
void pure_regex_vars(void);

/* errno access. */

int pure_errno(void);
void pure_set_errno(int value);

/* time() function. We provide an interface to this routine to account for
   platform incompatibilities. The result is always int64_t, as time_t
   nowadays is a 64 bit type on many OSes. We also provide wrappers for
   ctime() and gmtime() which convert a time value to a string, using either
   the local timezone or UTC. */

int64_t pure_time(void);

/* Call the C tzset() routine and initialize the (Pure) variables timezone,
   daylight and tzname accordingly. */

void pure_tzset(void);

/* The following routines allow you to convert a time value to broken-down
   time or a string, using different formats. See ctime(3), gmtime(3),
   localtime(3), strftime(3) and strptime(3) for details. */

char *pure_ctime(int64_t t);
struct tm *pure_gmtime(int64_t t);
struct tm *pure_localtime(int64_t t);
int64_t pure_mktime(struct tm *tm);
char *pure_strftime(const char *format, struct tm *tm);
pure_expr *pure_strptime(const char *s, const char *format, struct tm *tm);

/* gettimeofday() interface. This may actually be implemented using different
   system functions, depending on what's available on the host OS. */

double pure_gettimeofday(void);

/* nanosleep() interface. This may actually be implemented using different
   system functions, depending on what's available on the host OS. */

double pure_nanosleep(double t);

/* stat() and friends. These wrappers return a tuple with the most important
   fields from the stat structure (all bigint values). Note that in difference
   to the C library, our fstat() wrapper takes a file pointer as argument. On
   systems, where lstat() and fstat() aren't supported (e.g., Windows), the
   pure_lstat() works the same as pure_stat() and pure_fstat() always fails. */

#include <stdio.h>

pure_expr *pure_stat(const char *path);
pure_expr *pure_lstat(const char *path);
pure_expr *pure_fstat(FILE *fp);

/* printf/scanf support. As it's not possible to construct C vararg lists in a
   portable manner, we provide wrappers to process at most one value at a
   time. This also includes support for mpz_t and mpfr_t conversions. It is
   the responsibility of the caller that the provided parameters match up with
   the format specifiers. */

/* Helper functions to parse printf/scanf format strings. */

pure_expr *pure_printf_split(const char *format);
pure_expr *pure_scanf_split(const char *format);
int pure_scanf_prec(const char *format);

/* Wrappers needed by printf/scanf implementation (system.pure). These are
   provided here to work around Windows incompatibilities. */

void pure_mpz_clear(mpz_t z);
size_t pure_mpz_sizeinbase(mpz_t z, int b);
int pure_mpfr_get_prec(mpfr_ptr x);

/* printf et al */

int pure_fprintf(FILE *fp, const char *format);
int pure_fprintf_int(FILE *fp, const char *format, int32_t x);
int pure_fprintf_double(FILE *fp, const char *format, double x);
int pure_fprintf_string(FILE *fp, const char *format, const char *x);
int pure_fprintf_pointer(FILE *fp, const char *format, const void *x);
int pure_fprintf_mpz(FILE *fp, const char *format, mpz_t x);
int pure_fprintf_mpfr(FILE *fp, const char *format, mpfr_ptr x);

int pure_snprintf(char *buf, size_t size, const char *format);
int pure_snprintf_int(char *buf, size_t size, const char *format, int x);
int pure_snprintf_double(char *buf, size_t size, const char *format, double x);
int pure_snprintf_string(char *buf, size_t size, const char *format, const char *x);
int pure_snprintf_pointer(char *buf, size_t size, const char *format, const void *x);
int pure_snprintf_mpz(char *buf, int n, const char *format, mpz_t x);
int pure_snprintf_mpfr(char *buf, int n, const char *format, mpfr_ptr x);

/* scanf et al; note that mpfr doesn't support these */

int pure_fscanf(FILE *fp, const char *format);
int pure_fscanf_int(FILE *fp, const char *format, int32_t *x);
int pure_fscanf_double(FILE *fp, const char *format, double *x);
int pure_fscanf_string(FILE *fp, const char *format, const char *x);
int pure_fscanf_pointer(FILE *fp, const char *format, const void **x);
int pure_fscanf_mpz(FILE *fp, const char *format, mpz_t x);

int pure_sscanf(const char *buf, const char *format);
int pure_sscanf_int(const char *buf, const char *format, int32_t *x);
int pure_sscanf_double(const char *buf, const char *format, double *x);
int pure_sscanf_string(const char *buf, const char *format, char *x);
int pure_sscanf_pointer(const char *buf, const char *format, void **x);
int pure_sscanf_mpz(const char *buf, const char *format, mpz_t x);

/* readdir(3) support. */

pure_expr *pure_readdir(const char *name);

/* glob(3) support. */

#include <glob.h>

/* Decode the result of glob into a Pure list. */
pure_expr *globlist(const glob_t *pglob);

/* New regex interface (Pure 0.42 and later). */

#ifdef __cplusplus
struct pure_regex_t;
#else
typedef struct pure_regex_t pure_regex_t;
#endif

pure_regex_t *pure_regcomp(const char *pat, int cflags);
void pure_regfree(pure_regex_t *reg);
int pure_regexec(pure_regex_t *reg, const char *s, int eflags);
int pure_regnext(pure_regex_t *reg, int overlap);
void pure_regdone(pure_regex_t *reg);
int pure_regstatus(pure_regex_t *reg);
pure_expr *pure_regerror(pure_regex_t *reg);
pure_expr *pure_regmatch(pure_regex_t *reg);
pure_expr *pure_regskip(pure_regex_t *reg);

#ifdef __cplusplus
}
#endif

#endif // ! RUNTIME_H
