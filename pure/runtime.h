#ifndef RUNTIME_H
#define RUNTIME_H

/* The Pure runtime interface. */

#include <stdint.h>
#include <stdbool.h>
#include <setjmp.h>
#include <gmp.h>

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
  uint32_t n, m;		// number of arguments and environment size
  struct _pure_expr **env;	// captured environment (if m>0, 0 otherwise)
  bool local;			// local function?
  bool thunked;			// thunked closure? (kept unevaluated)
} pure_closure;

/* Matrix data. The GSL matrix data is represented as a void* whose actual
   type depends on the expression tag. Different expressions may share the
   same underlying memory block, so we do our own reference counting to manage
   these. */

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
    struct _pure_expr *x[3];	// application arguments (EXPR::APP), sentry
    int32_t i;			// integer (EXPR::INT)
    mpz_t z;			// GMP bigint (EXPR::BIGINT)
    double d;			// double (EXPR::DBL)
    char *s;			// C string (EXPR::STR)
    void *p;			// generic pointer (EXPR::PTR)
    pure_matrix mat;		// matrix data (EXPR::MATRIX et al)
    pure_closure *clos;		// closure (0 if none)
  } data;
  /* Internal fields (DO NOT TOUCH). The JIT doesn't care about these. */
  struct _pure_expr *xp;	// freelist pointer
} pure_expr;

/* Fake GSL matrix struct used to represent symbolic matrix expressions. These
   mimic GSL's interface to numeric matrices. */

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

typedef struct _gsl_matrix_symbolic_view
{
  gsl_matrix_symbolic matrix;
} gsl_matrix_symbolic_view;

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

   Given the (positive) symbol number, pure_sym_pname returns its print name
   and pure_sym_nprec its "normalized" precedence. The latter is a small
   integer value defined as nprec = 10*prec+fix, where prec is the precedence
   level of the symbol and fix its fixity. For operators, the combined value
   ranges from 0 (weakest infix operator on level 0) to 94 (strongest postfix
   operator on level 9). Applications have nprec=95, ordinary function and
   variable symbols nprec=100. */

int32_t pure_sym(const char *s);
int32_t pure_getsym(const char *s);
const char *pure_sym_pname(int32_t sym);
int8_t pure_sym_nprec(int32_t sym);

/* Expression constructors. pure_symbol takes the integer code of a symbol and
   returns that symbol as a Pure value. If the symbol is a global variable
   bound to a value then that value is returned, if it's a parameterless
   function then it is evaluated, giving the return value of the function as
   the result. pure_int, pure_mpz, pure_double and pure_pointer construct a
   Pure machine int, bigint, floating point value and pointer from a 32 bit
   integer, (copy of a) GMP mpz_t, double and C pointer, respectively. */

pure_expr *pure_symbol(int32_t sym);
pure_expr *pure_int(int32_t i);
pure_expr *pure_mpz(const mpz_t z);
pure_expr *pure_double(double d);
pure_expr *pure_pointer(void *p);

/* Expression pointers. The following routine returns a Pure pointer object
   suitably allocated to hold a Pure expression (pure_expr*). The pointer is
   initialized to hold a null expression. */

pure_expr *pure_expr_pointer(void);

/* String constructors. There are four variations of these, depending on
   whether the original string is already in utf-8 (_string routines) or in
   the system encoding (_cstring), and whether the string should be copied
   (_dup suffix) or whether Pure takes ownership of the string. All four
   routines handle the case that the given string is a null pointer and will
   then return the appropriate Pure pointer expression instead. */

pure_expr *pure_string_dup(const char *s);
pure_expr *pure_cstring_dup(const char *s);
pure_expr *pure_string(char *s);
pure_expr *pure_cstring(char *s);

/* Matrix constructors. The given pointer must point to a valid GSL matrix
   struct of the corresponding GSL matrix type (gsl_matrix,
   gsl_matrix_complex, gsl_matrix_int), or a pointer to the special
   gsl_matrix_symbolic struct provide by the runtime. (These are just given as
   void* here to avoid depending on the GSL headers which might not be
   available for some implementations.) In the case of the _matrix routines,
   the matrix must be allocated dynamically and Pure takes ownership of the
   matrix. The matrix_dup routines first take a copy of the matrix, so the
   ownership of the original matrix remains with the caller. The result is a
   Pure expression representing the matrix object, or null if GSL matrix
   support is not available or some other error occurs. */

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

   A null expression is returned in case of an error (dimension mismatch,
   insufficient memory), leaving the input expressions untouched. Otherwise a
   new matrix expression is returned and references are counted on component
   expressions as appropriate (temporary components may also be
   garbage-collected if they are no longer needed). In any case, the elems
   vectors are owned by the caller and won't be freed. */

pure_expr *pure_matrix_rowsl(uint32_t n, ...);
pure_expr *pure_matrix_rowsv(uint32_t n, pure_expr **elems);
pure_expr *pure_matrix_columnsl(uint32_t n, ...);
pure_expr *pure_matrix_columnsv(uint32_t n, pure_expr **elems);

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

/* Convenience functions to construct Pure list and tuple values from a vector
   or a varargs list of element expressions. (Internally these are actually
   represented as function applications.) The vectors are owned by the caller
   and won't be freed. References on the element expressions are counted
   automatically. */

pure_expr *pure_listl(size_t size, ...);
pure_expr *pure_listv(size_t size, pure_expr **elems);
pure_expr *pure_tuplel(size_t size, ...);
pure_expr *pure_tuplev(size_t size, pure_expr **elems);

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

/* Memory management. */

/* Count a new reference to an expression. This should be called whenever you
   want to store an expression somewhere, in order to prevent it from being
   garbage-collected. */

pure_expr *pure_new(pure_expr *x);

/* Drop a reference to an expression. This will cause the expression to be
   garbage-collected when it is no longer needed. */

void pure_free(pure_expr *x);

/* Count a reference and then immediately drop it. This is useful to collect
   temporaries which are not referenced yet. */

void pure_freenew(pure_expr *x);

/* Increment and decrement the reference counter of an expression. This can be
   used to temporarily protect an expression from being garbage-collected. It
   doesn't actually change the status of the expression and does not collect
   it. */

void pure_ref(pure_expr *x);
void pure_unref(pure_expr *x);

/* Sentries. These are expression "guards" which are applied to the target
   expression when it is garbage-collected. pure_sentry places a sentry at an
   expression (or removes it if sentry is NULL) and returns the modified
   expression, pure_get_sentry returns the current sentry of an expression, if
   any (NULL otherwise). pure_clear_sentry(x) is a convenience function for
   pure_sentry(NULL, x). NOTE: In the current implementation sentries can only
   be placed at applications and pointer objects, pure_sentry will return NULL
   if you apply it to other kinds of expressions. The sentry itself can be any
   type of object (but usually it's a function). */

pure_expr *pure_sentry(pure_expr *sentry, pure_expr *x);
pure_expr *pure_get_sentry(pure_expr *x);
pure_expr *pure_clear_sentry(pure_expr *x);

/* Variable and constant definitions. These allow you to directly bind
   variable and constant symbols to pure_expr* values, as the 'let' and 'def'
   constructs do in the Pure language. The functions return true if
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

uint8_t pure_save();
uint8_t pure_restore();

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
   application (usually the main program of the application).

   An application may use multiple interpreter instances, but only a single
   instance can be active at any one time. By default, the first created
   instance will be active, but you can switch between different instances
   with the pure_switch_interp function. The pure_delete_interp routine
   destroys an interpreter instance; if the destroyed instance is currently
   active, the active instance will be undefined afterwards, so you'll have to
   either create or switch to another instance before calling any other
   operations. The pure_current_interp returns the currently active
   instance. If the application is hosted by the command line interpreter,
   this will return a handle to the command line interpreter if it is invoked
   before switching to any other interpreter instance.

   Note that when using different interpreter instances in concert, it is
   *not* possible to pass pure_expr* values created with one interpreter
   instance to another. Instead, you can use the str and eval functions from
   the library API (see below) to first unparse the expression in the source
   interpreter and then reparse it in the target interpreter. */

pure_interp *pure_create_interp(int argc, char *argv[]);
void pure_delete_interp(pure_interp *interp);
void pure_switch_interp(pure_interp *interp);
pure_interp *pure_current_interp();

/* END OF PUBLIC API. *******************************************************/

/* Stuff below this line is for internal use by the Pure interpreter. Don't
   call these directly, unless you know what you are doing. */

/* Construct constant symbols and closures. */

pure_expr *pure_const(int32_t tag);
pure_expr *pure_clos(bool local, bool thunked, int32_t tag, uint32_t n,
		     void *f, void *e, uint32_t m, /* m x pure_expr* */ ...);

/* Additional bigint constructors. */

pure_expr *pure_long(int64_t l);
pure_expr *pure_bigint(int32_t size, const limb_t *limbs);

/* Compare a bigint or string expression against a constant value. This is
   used by the pattern matching code. */

int32_t pure_cmp_bigint(pure_expr *x, int32_t size, const limb_t *limbs);
int32_t pure_cmp_string(pure_expr *x, const char *s);

/* Get the string value of a string expression in the system encoding. Each
   call returns a new string, pure_free_cstrings() frees the temporary
   storage. This is only to be used internally, to unbox string arguments in
   the C interface. */

char *pure_get_cstring(pure_expr *x);
void pure_free_cstrings();

/* Convert a bigint expression to a pointer (mpz_t) or a 64 or 32 bit
   integer. This is used to marshall bigint arguments in the C interface. */

void *pure_get_bigint(pure_expr *x);
int64_t pure_get_long(pure_expr *x);
int32_t pure_get_int(pure_expr *x);

/* Convert a matrix expression to a pointer to the corresponding GSL matrix
   struct. This is used to marshall matrix arguments in the C interface. */

void *pure_get_matrix(pure_expr *x);

/* Additional matrix constructors. These work like pure_matrix_rowsl and
   pure_matrix_columnsl in the public API, but are intended to be called
   directly from generated code and raise the appropriate Pure exceptions in
   case of an error condition. */

pure_expr *pure_matrix_rows(uint32_t n, ...);
pure_expr *pure_matrix_columns(uint32_t n, ...);

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

/* Exception handling stuff. */

typedef struct { jmp_buf jmp; pure_expr* e; size_t sz; } pure_exception;

/* Throw the given expression (which may also be null) as an exception. */

void pure_throw(pure_expr* e);

/* Throw a 'signal SIGFPE' exception. This is used to signal division by
   zero. */

void pure_sigfpe(void);

/* Configure signal handlers. The second argument is the signal number, the
   first the action to take (-1 = ignore, 1 = handle, 0 = default). */

void pure_trap(int32_t action, int32_t sig);

/* Execute a parameterless fbox x and return its result. If an exception
   occurs while x is executed, apply h to the value of the exception
   instead. */

pure_expr *pure_catch(pure_expr *h, pure_expr *x);

/* Run a Pure function and catch exceptions. If everything goes normal,
   pure_invoke returns the return value of the executed function. Otherwise it
   returns 0 and sets e to the exception value, as given by pure_throw().
   FIXME: This only supports parameterless functions right now. */

pure_expr *pure_invoke(void *f, pure_expr** e);

/* Manage arguments of a function call. pure_new_args counts references on a
   given collection of arguments in preparation for a function call, while
   pure_free_args collects the arguments of a function call. In both cases the
   arguments follow the given parameter count n. For pure_free_args, the first
   expression argument is the return value; if not null, an extra reference is
   temporarily counted on this expression so that it doesn't get freed if the
   return value happens to be a (subterm of an) argument or environment
   expression. (It's the caller's duty to call pure_unref later.) These
   functions are only to be used for internal calls (apply, catch, etc.); for
   calls which are to be visible on the shadow stack see pure_push_args and
   pure_pop_args below. */

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
void pure_pop_arg();
void pure_pop_tail_arg();

/* Debugging support. Preliminary. */

void pure_debug(int32_t tag, const char *format, ...);

/* LIBRARY API. *************************************************************/

/* Add any stuff that is needed in the standard library here. Applications and
   external C modules may call these, but be warned that these APIs are
   subject to change without further notice. */

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

/* Random number generator. This uses the Mersenne twister, in order to avoid
   bad generators present in some C libraries. pure_random returns a
   pseudorandom 32 bit integer, pure_srandom sets the seed of the
   generator. */

uint32_t pure_random(void);
void pure_srandom(uint32_t seed);

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

/* Concatenation, substrings and finding a substring in a string. */

pure_expr *string_concat(const char* s, const char *t);
pure_expr *string_concat_list(pure_expr *xs);
pure_expr *string_substr(const char* s, uint32_t pos, uint32_t size);
int32_t string_index(const char* s, const char *t);

/* Conversions between utf-8 characters and numbers. These convert a Unicode
   character code (code point) to the corresponding utf-8 character, and vice
   versa. */

pure_expr *string_chr(uint32_t n);
pure_expr *string_ord(const char *c);

/* Convert a Pure expression to a string and vice versa. Note that eval() will
   actually parse and execute any Pure source, so it can be used, e.g., to add
   new rules to the executing program at runtime. The result of eval() is the
   last computed expression, NULL if none; in the latter case you can inspect
   the result of lasterr() below to determine whether there were any errors.
   The result of str() is a malloc'ed string in the system encoding which must
   be freed by the caller. */

char *str(const pure_expr *x);
pure_expr *eval(const char *s);

/* After an invokation of eval(), this returns error messages from the
   interpreter (an empty string if none). */

const char *lasterr();

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
   checking against matrix_size or matrix_dim above. */

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

/* Matrix construction. These work like the corresponding functions in the
   public API, but take their input from a Pure list and raise the appropriate
   exception in case of dimension mismatch. */

pure_expr *matrix_rows(pure_expr *xs);
pure_expr *matrix_columns(pure_expr *xs);

/* Transpose a matrix. The resulting matrix has the rows of the original
   matrix as its columns, and vice versa. */

pure_expr *matrix_transpose(pure_expr *x);

/* Convert between different types of numeric matrices. */

pure_expr *matrix_double(pure_expr *x);
pure_expr *matrix_complex(pure_expr *x);
pure_expr *matrix_int(pure_expr *x);

/* Extract the real and imaginary parts of a numeric matrix. If the input is a
   complex matrix, the result is a new double matrix. Otherwise the type of
   the result is the same as that of the input matrix (in this case matrix_re
   just returns the same matrix, and matrix_im returns a new zero matrix of
   the same dimensions). */

pure_expr *matrix_re(pure_expr *x);
pure_expr *matrix_im(pure_expr *x);

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
   directly supported by Pure GSL matrices: float, complex float, short,
   byte. */

pure_expr *matrix_from_float_array(uint32_t n, uint32_t m, void *p);
pure_expr *matrix_from_complex_float_array(uint32_t n, uint32_t m, void *p);
pure_expr *matrix_from_short_array(uint32_t n, uint32_t m, void *p);
pure_expr *matrix_from_byte_array(uint32_t n, uint32_t m, void *p);

void *matrix_to_float_array(void *p, pure_expr *x);
void *matrix_to_complex_float_array(void *p, pure_expr *x);
void *matrix_to_short_array(void *p, pure_expr *x);
void *matrix_to_byte_array(void *p, pure_expr *x);

/* Compute a 32 bit hash code of a Pure expression. This makes it possible to
   use arbitary Pure values as keys in a hash table. */

uint32_t hash(pure_expr *x);

/* Check whether two objects are the "same" (syntactically). */

bool same(pure_expr *x, pure_expr *y);

/* Check whether an object is a named function (closure), an anonymous
   function (lambda or thunk), or a global variable, respectively. */

bool funp(const pure_expr *x);
bool lambdap(const pure_expr *x);
bool thunkp(const pure_expr *x);
bool varp(const pure_expr *x);

/* Direct memory accesses. Use these with care. In particular, note that the
   pointer_put_expr() routine doesn't do any reference counting by itself, so
   you'll have to use the memory management routines above to do that. */

int32_t pointer_get_byte(void *ptr);
int32_t pointer_get_short(void *ptr);
int32_t pointer_get_int(void *ptr);
double pointer_get_float(void *ptr);
double pointer_get_double(void *ptr);
char *pointer_get_string(void *ptr);
void *pointer_get_pointer(void *ptr);
pure_expr *pointer_get_expr(void *ptr);

void pointer_put_byte(void *ptr, int32_t x);
void pointer_put_short(void *ptr, int32_t x);
void pointer_put_int(void *ptr, int32_t x);
void pointer_put_float(void *ptr, double x);
void pointer_put_double(void *ptr, double x);
void pointer_put_string(void *ptr, const char *x);
void pointer_put_pointer(void *ptr, void *x);
void pointer_put_expr(void *ptr, pure_expr *x);

/* Initialize a bunch of variables with useful system constants. */

void pure_sys_vars(void);

/* errno access. */

int pure_errno(void);
void pure_set_errno(int value);

/* time() function. We provide an interface to this routine to account for
   platform incompatibilities. The result is always int64_t, as time_t
   nowadays is a 64 bit type on many OSes. We also provide wrappers for
   ctime() and gmtime() which convert a time value to a string, using either
   the local timezone or UTC. */

int64_t pure_time(void);

/* The following routines allow you to convert a time value to a string, using
   different formats. See ctime(3), gmtime(3) and strftime(3) for details. */

char *pure_ctime(int64_t t);
char *pure_gmtime(int64_t t);
char *pure_strftime(const char *format, int64_t t);

/* gettimeofday() interface. This may actually be implemented using different
   system functions, depending on what's available on the host OS. */

double pure_gettimeofday(void);

/* nanosleep() interface. This may actually be implemented using different
   system functions, depending on what's available on the host OS. */

double pure_nanosleep(double t);

#ifdef __MINGW32__
/* Windows compatibility. */
FILE *popen(const char *command, const char *type);
int pclose(FILE *stream);
unsigned int sleep(unsigned int secs);
#endif

/* printf/scanf support. Since we don't support calling C vararg functions
   from Pure right now, these little wrappers are provided to process at most
   one value at a time. It is the responsibility of the caller that the
   provided parameters match up with the format specifiers. */

#include <stdio.h>

int pure_fprintf(FILE *fp, const char *format);
int pure_fprintf_int(FILE *fp, const char *format, int32_t x);
int pure_fprintf_double(FILE *fp, const char *format, double x);
int pure_fprintf_string(FILE *fp, const char *format, const char *x);
int pure_fprintf_pointer(FILE *fp, const char *format, const void *x);

int pure_snprintf(char *buf, size_t size, const char *format);
int pure_snprintf_int(char *buf, size_t size, const char *format, int x);
int pure_snprintf_double(char *buf, size_t size, const char *format, double x);
int pure_snprintf_string(char *buf, size_t size, const char *format, const char *x);
int pure_snprintf_pointer(char *buf, size_t size, const char *format, const void *x);

int pure_fscanf(FILE *fp, const char *format);
int pure_fscanf_int(FILE *fp, const char *format, int32_t *x);
int pure_fscanf_double(FILE *fp, const char *format, double *x);
int pure_fscanf_string(FILE *fp, const char *format, const char *x);
int pure_fscanf_pointer(FILE *fp, const char *format, const void **x);

int pure_sscanf(const char *buf, const char *format);
int pure_sscanf_int(const char *buf, const char *format, int32_t *x);
int pure_sscanf_double(const char *buf, const char *format, double *x);
int pure_sscanf_string(const char *buf, const char *format, char *x);
int pure_sscanf_pointer(const char *buf, const char *format, void **x);

/* glob(3) support. */

#include <glob.h>

/* Decode the result of glob into a Pure list. */
pure_expr *globlist(const glob_t *pglob);

/* regexec/regcomp(3) support. */

#include <sys/types.h>
#include <regex.h>

/* Return the number of subre's and allocate storage for the matches. */
pure_expr *regmatches(const regex_t *preg, int flags);

/* Decode the result of regexec into a list of matches. */
pure_expr *reglist(const regex_t *preg, const char *s,
		   const regmatch_t *matches);

#ifdef __cplusplus
}
#endif

#endif // ! RUNTIME_H
