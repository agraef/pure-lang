
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ffi.h>
#include <pure/runtime.h>

#if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
#define HAVE_LONG_DOUBLE 1
#endif

#if PURE_POINTER_TAG
/* Convenience macros to handle tagged pointers (new in Pure 0.45). */
#define __pointer(ty, p) pure_tag(ty, pure_pointer(p))
#define __check_tag(ty, x) pure_check_tag(ty, x)
#define __tag(ty) pure_pointer_tag(#ty)
#else
/* For compatibility with older Pure versions. */
#define __pointer(ty, p) pure_pointer(p)
#define __check_tag(ty, x) 1
#define __tag(ty) 0
#endif

void ffi_defs(void)
{
  /* Platform-specific ABI constants. This is probably incomplete; the
     values listed below were gleaned from the ffitarget.h file on a Linux
     system. Add other values as required. */
  pure_def(pure_sym("FFI_DEFAULT_ABI"), pure_int(FFI_DEFAULT_ABI));
#ifdef X86_WIN32
  pure_def(pure_sym("FFI_SYSV"), pure_int(FFI_SYSV));
  pure_def(pure_sym("FFI_STDCALL"), pure_int(FFI_STDCALL));
#else
#if (defined(__i386__) || defined(__x86_64__)) && !defined(__MINGW64__)
  pure_def(pure_sym("FFI_SYSV"), pure_int(FFI_SYSV));
  pure_def(pure_sym("FFI_UNIX64"), pure_int(FFI_UNIX64));
#endif
#endif
  /* Type identifiers. */
  pure_def(pure_sym("FFI_TYPE_VOID"), pure_int(FFI_TYPE_VOID));
  pure_def(pure_sym("FFI_TYPE_INT"), pure_int(FFI_TYPE_INT));
  pure_def(pure_sym("FFI_TYPE_FLOAT"), pure_int(FFI_TYPE_FLOAT));
  pure_def(pure_sym("FFI_TYPE_DOUBLE"), pure_int(FFI_TYPE_DOUBLE));
  pure_def(pure_sym("FFI_TYPE_LONGDOUBLE"), pure_int(FFI_TYPE_LONGDOUBLE));
  pure_def(pure_sym("FFI_TYPE_UINT8"), pure_int(FFI_TYPE_UINT8));
  pure_def(pure_sym("FFI_TYPE_SINT8"), pure_int(FFI_TYPE_SINT8));
  pure_def(pure_sym("FFI_TYPE_UINT16"), pure_int(FFI_TYPE_UINT16));
  pure_def(pure_sym("FFI_TYPE_SINT16"), pure_int(FFI_TYPE_SINT16));
  pure_def(pure_sym("FFI_TYPE_UINT32"), pure_int(FFI_TYPE_UINT32));
  pure_def(pure_sym("FFI_TYPE_SINT32"), pure_int(FFI_TYPE_SINT32));
  pure_def(pure_sym("FFI_TYPE_UINT64"), pure_int(FFI_TYPE_UINT64));
  pure_def(pure_sym("FFI_TYPE_SINT64"), pure_int(FFI_TYPE_SINT64));
  pure_def(pure_sym("FFI_TYPE_STRUCT"), pure_int(FFI_TYPE_STRUCT));
  pure_def(pure_sym("FFI_TYPE_POINTER"), pure_int(FFI_TYPE_POINTER));
}

/* Atomic types. We add our own string (char*) type to ease marshalling of
   Pure string values. Note that this is just a copy of libffi's pointer type
   under an alias. */

ffi_type ffi_type_string;

ffi_type *ffi_type_void_ptr(void)
{
  return &ffi_type_void;
}

ffi_type *ffi_type_uint8_ptr(void)
{
  return &ffi_type_uint8;
}

ffi_type *ffi_type_sint8_ptr(void)
{
  return &ffi_type_sint8;
}

ffi_type *ffi_type_uint16_ptr(void)
{
  return &ffi_type_uint16;
}

ffi_type *ffi_type_sint16_ptr(void)
{
  return &ffi_type_sint16;
}

ffi_type *ffi_type_uint32_ptr(void)
{
  return &ffi_type_uint32;
}

ffi_type *ffi_type_sint32_ptr(void)
{
  return &ffi_type_sint32;
}

ffi_type *ffi_type_uint64_ptr(void)
{
  return &ffi_type_uint64;
}

ffi_type *ffi_type_sint64_ptr(void)
{
  return &ffi_type_sint64;
}

ffi_type *ffi_type_float_ptr(void)
{
  return &ffi_type_float;
}

ffi_type *ffi_type_double_ptr(void)
{
  return &ffi_type_double;
}

ffi_type *ffi_type_longdouble_ptr(void)
{
  return &ffi_type_longdouble;
}

ffi_type *ffi_type_pointer_ptr(void)
{
  return &ffi_type_pointer;
}

ffi_type *ffi_type_uchar_ptr(void)
{
  return &ffi_type_uchar;
}

ffi_type *ffi_type_schar_ptr(void)
{
  return &ffi_type_schar;
}

ffi_type *ffi_type_ushort_ptr(void)
{
  return &ffi_type_ushort;
}

ffi_type *ffi_type_sshort_ptr(void)
{
  return &ffi_type_sshort;
}

ffi_type *ffi_type_uint_ptr(void)
{
  return &ffi_type_uint;
}

ffi_type *ffi_type_sint_ptr(void)
{
  return &ffi_type_sint;
}

ffi_type *ffi_type_ulong_ptr(void)
{
  return &ffi_type_ulong;
}

ffi_type *ffi_type_slong_ptr(void)
{
  return &ffi_type_slong;
}

ffi_type *ffi_type_string_ptr(void)
{
  if (ffi_type_string.type != FFI_TYPE_POINTER)
    ffi_type_string = ffi_type_pointer;
  return &ffi_type_string;
}

/* Manage type descriptions. Struct descriptions have to be refcounted, as we
   create them dynamically. */

typedef struct _ffi_type1
{
  size_t size;
  unsigned short alignment;
  unsigned short type;
  struct _ffi_type **elements;
  size_t refc;
} ffi_type1;

static ffi_type *ffi_ref_type(ffi_type *type)
{
  if (type && type->type == FFI_TYPE_STRUCT)
    ((ffi_type1*)type)->refc++;
  return type;
}

static void ffi_unref_type(ffi_type *type)
{
  ffi_type **t;
  if (!type || type->type != FFI_TYPE_STRUCT) return;
  if (type->elements)
    for (t = type->elements; *t; t++)
      ffi_unref_type(*t);
  if (--((ffi_type1*)type)->refc == 0)
    free(type);
}

ffi_type *ffi_new_struct_t(ffi_type **elements)
{
  ffi_type1 *type;
  ffi_type **t;
  if (!elements) return 0;
  type = malloc(sizeof(ffi_type1));
  assert(type != 0);
  type->size = type->alignment = 0;
  type->type = FFI_TYPE_STRUCT;
  type->elements = elements;
  for (t = type->elements; *t; t++)
    ffi_ref_type(*t);
  type->refc = 1;
  return (ffi_type*)type;
}

void ffi_free_struct_t(ffi_type *type)
{
  ffi_unref_type(type);
}

/* Retrieve information about a type in a Pure-friendly format. */

pure_expr *ffi_type_info(ffi_type *type)
{
  unsigned nelems = 0, i;
  ffi_type **t;
  pure_expr **xs, *x;
  if (type->type == FFI_TYPE_STRUCT) {
    if (type->alignment == 0) {
      /* Type information hasn't been filled in yet; do a dummy call to
	 ffi_prep_cif to do that now. */
      ffi_cif cif;
      ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 0, type, 0);
    }
    /* Count the element types. */
    for (t = type->elements; *t; t++)
      nelems++;
  }
  xs = malloc((3+nelems)*sizeof(pure_expr*));
  assert(xs);
  /* type->size is actually a size_t field, so we should actually use a bigint
     here. But that seems overkill. */
  xs[0] = pure_int(type->size);
  xs[1] = pure_int(type->alignment);
  xs[2] = pure_int(type->type);
  if (type->type == FFI_TYPE_STRUCT) {
    int ty = __tag(ffi_type*);
    for (i = 0; i < nelems; i++)
      xs[3+i] = __pointer(ty, ffi_ref_type(type->elements[i]));
  }
  x = pure_tuplev(3+nelems, xs);
  free(xs);
  return x;
}

/* Construction of struct values. In Pure land, these are implemented as
   cooked pointers which carry typing information in the sentry, which also
   takes care of freeing the struct when it is garbage-collected. NOTE: To
   make this work, the ffi_free_struct routine must be visible in ffi.pure
   under the name "__C::ffi_free_struct". */

static inline pure_expr *pure_struct(ffi_type *type, void *v)
{
  pure_expr *x = pure_pointer(v),
    *y = pure_app(pure_symbol(pure_sym("__C::ffi_free_struct")),
		  __pointer(__tag(ffi_type*), type));
  assert(x && y);
  return pure_sentry(y, x);
}

static inline bool pure_is_struct(pure_expr *x, ffi_type **type, void **v)
{
  pure_expr *y, *z, *f;
  return pure_is_pointer(x, v) && (y = pure_get_sentry(x)) &&
    pure_is_app(y, &f, &z) && f->tag > 0 &&
    strcmp(pure_sym_pname(f->tag), "__C::ffi_free_struct") == 0 &&
    pure_is_pointer(z, (void**)type) &&
    __check_tag(__tag(ffi_type*), z) && *type &&
    (*type)->type == FFI_TYPE_STRUCT;
}

/* Compute the member offsets in a struct relative to the given base
   pointer. */

static void offsets(void *data, unsigned n, ffi_type **types, void **v)
{
  size_t ofs = 0;
  unsigned i;
  for (i = 0; i < n; i++) {
    unsigned short a = ofs % types[i]->alignment;
    if (a != 0) ofs += types[i]->alignment-a;
    v[i] = data+ofs;
    ofs += types[i]->size;
  }
}

/* Compute a single offset on the fly. */

static void *offset(void *data, unsigned n, ffi_type **types)
{
  size_t ofs = 0;
  unsigned i;
  unsigned short a;
  for (i = 0; i < n && types[i]; i++) {
    a = ofs % types[i]->alignment;
    if (a != 0) ofs += types[i]->alignment-a;
    ofs += types[i]->size;
  }
  if (i < n || !types[i])
    return 0;
  a = ofs % types[i]->alignment;
  if (a != 0) ofs += types[i]->alignment-a;
  return data+ofs;
}

/* Function to compute a single offset on the fly -
   equivalent of the C macro offsetof. */

pure_expr *ffi_struct_offsetof(ffi_type *type, int i)
{
  if (i < 0) return 0;
  pure_expr *x;
  void *v;
  void *data = NULL + 2; // make a non-null pointer
  if (type->type != FFI_TYPE_STRUCT) return 0;
  v = offset(data, i, type->elements);
  if (v) {
    x = pure_int(v - data);
    return x;
  } else
    return 0;
}

static void *ffi_to_c(void *v, ffi_type *type, pure_expr *x);
static pure_expr *ffi_from_c(ffi_type *type, void *v);
static pure_expr *ffi_from_cvect(ffi_cif *cif, void **v);

void *ffi_new_struct(ffi_type *type, pure_expr *x)
{
  ffi_type **t;
  unsigned nelems = 0;
  void **v = 0, *data = 0;
  pure_expr **xs = 0;
  size_t i, n;
  if (!type || type->type != FFI_TYPE_STRUCT) return 0;
  for (t = type->elements; *t; t++)
    nelems++;
  if (type->alignment == 0) {
    /* Type information hasn't been filled in yet; do a dummy call to
       ffi_prep_cif to do that now. */
    ffi_cif cif;
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 0, type, 0) != FFI_OK)
      return 0;
  }
  if (!pure_is_tuplev(x, &n, &xs) || n != nelems) goto err;
  data = malloc(type->size);
  assert(type->size == 0 || data);
  v = malloc(nelems*sizeof(void*));
  assert(nelems == 0 || v);
  offsets(data, nelems, type->elements, v);
  for (i = 0; i < n; i++) {
    if (type->elements[i]->type == FFI_TYPE_VOID)
      continue;
    assert(type->elements[i]->size == 0 || v[i] != 0);
    if (!ffi_to_c(v[i], type->elements[i], xs[i])) {
      free(data);
      data = 0;
      goto err;
    }
  }
 err:
  if (v) free(v);
  if (xs) free(xs);
  return data;
}

void *ffi_copy_struct(ffi_type *type, void *data)
{
  void *data2;
  if (!data || !type || type->type != FFI_TYPE_STRUCT) return 0;
  if (type->alignment == 0) {
    /* Type information hasn't been filled in yet; do a dummy call to
       ffi_prep_cif to do that now. */
    ffi_cif cif;
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 0, type, 0) != FFI_OK)
      return 0;
  }
  data2 = malloc(type->size);
  assert(type->size == 0 || data);
  memcpy(data2, data, type->size);
  return data2;
}

void ffi_free_struct(ffi_type *type, void *data)
{
  if (type && data) free(data);
}

/* Retrieve type information, member data and pointers of a struct. */

pure_expr *ffi_struct_type(pure_expr *x)
{
  ffi_type *type;
  void *data;
  if (pure_is_struct(x, &type, &data))
    return __pointer(__tag(ffi_type*), type);
  else
    return 0;
}

pure_expr *ffi_struct_members(pure_expr *x)
{
  ffi_type *type;
  void *data;
  if (pure_is_struct(x, &type, &data)) {
    ffi_cif cif;
    ffi_type **t;
    unsigned nelems = 0;
    void **v;
    pure_expr *x;
    if (type->type != FFI_TYPE_STRUCT) return 0;
    for (t = type->elements; *t; t++)
      nelems++;
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nelems,
		     &ffi_type_void, type->elements) != FFI_OK)
      return 0;
    v = malloc(nelems*sizeof(void*));
    assert(nelems == 0 || v);
    offsets(data, nelems, type->elements, v);
    x = ffi_from_cvect(&cif, v);
    if (v) free(v);
    return x;
  } else
    return 0;
}

pure_expr *ffi_struct_pointers(pure_expr *x)
{
  ffi_type *type;
  void *data;
  if (pure_is_struct(x, &type, &data)) {
    ffi_cif cif;
    ffi_type **t;
    unsigned i, nelems = 0;
    void **v;
    pure_expr *x, **xs;
    if (type->type != FFI_TYPE_STRUCT) return 0;
    for (t = type->elements; *t; t++)
      nelems++;
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nelems,
		     &ffi_type_void, type->elements) != FFI_OK)
      return 0;
    v = malloc(nelems*sizeof(void*));
    assert(nelems == 0 || v);
    xs = malloc(nelems*sizeof(pure_expr*));
    assert(nelems == 0 || xs);
    offsets(data, nelems, type->elements, v);
    for (i = 0; i < nelems; i++)
      xs[i] = pure_pointer(v[i]);
    x = pure_tuplev(nelems, xs);
    if (v) free(v);
    if (xs) free(xs);
    return x;
  } else
    return 0;
}

/* Access a single member or pointer. */

pure_expr *ffi_struct_member(pure_expr *x, int i)
{
  ffi_type *type;
  void *data;
  if (i < 0) return 0;
  if (pure_is_struct(x, &type, &data)) {
    void *v;
    pure_expr *x;
    if (type->type != FFI_TYPE_STRUCT) return 0;
    v = offset(data, i, type->elements);
    if (v) {
      x = ffi_from_c(type->elements[i], v);
      return x;
    } else
      return 0;
  } else
    return 0;
}

pure_expr *ffi_put_struct_member(pure_expr *x, int i, pure_expr *y)
{
  ffi_type *type;
  void *data;
  if (i < 0) return 0;
  if (pure_is_struct(x, &type, &data)) {
    void *v;
    pure_expr *x;
    if (type->type != FFI_TYPE_STRUCT) return 0;
    v = offset(data, i, type->elements);
    if (!v)
      return 0;
    else if (ffi_to_c(v, type->elements[i], y))
      return pure_int(1);
    else
      return pure_int(0);
  } else
    return 0;
}

pure_expr *ffi_struct_pointer(pure_expr *x, int i)
{
  ffi_type *type;
  void *data;
  if (i < 0) return 0;
  if (pure_is_struct(x, &type, &data)) {
    void *v;
    pure_expr *x;
    if (type->type != FFI_TYPE_STRUCT) return 0;
    v = offset(data, i, type->elements);
    if (v)
      return pure_pointer(v);
    else
      return 0;
  } else
    return 0;
}

/* Check types for structural equivalence. */

static bool same_type(ffi_type *type1, ffi_type *type2)
{
  if (type1 == type2)
    return true;
  else if (type1->type != type2->type)
    return false;
  else if (type1->type == FFI_TYPE_STRUCT) {
    /* Different struct types are considered equivalent iff their member types
       are. */
    ffi_type **t1 = type1->elements, **t2 = type2->elements;
    if (t1 == t2) return true;
    while (*t1 && *t2 && same_type(*t1, *t2)) t1++, t2++;
    return !*t1 && !*t2;
  } else
    return false;
}

/* Construct a 0-terminated type vector to be passed as the atypes argument of
   ffi_new_cif and ffi_new_struct_t. */

ffi_type **ffi_typevect(pure_expr *types)
{
  void *p;
  ffi_type **v = 0;
  size_t i, j, n;
  pure_expr **xs;
  if (pure_is_tuplev(types, &n, &xs)) {
    int ty = __tag(ffi_type*);
    if (n == 0) {
      v = malloc(sizeof(ffi_type*));
      assert(v != 0);
      v[0] = 0;
      return v;
    }
    for (i = 0; i < n; i++)
      if (!pure_is_pointer(xs[i], 0) || !__check_tag(ty, xs[i]))
	goto err;
  } else
    return 0;
  v = malloc((n+1)*sizeof(ffi_type*));
  assert(v != 0);
  v[n] = 0;
  v[0] = (ffi_type*)p;
  for (i = 0; i < n; i++) {
    pure_is_pointer(xs[i], &p);
    v[i] = (ffi_type*)p;
  }
 err:
  free(xs);
  return v;
}

/* Construct a call interface. */

void ffi_free_cif(ffi_cif *cif)
{
  if (!cif) return;
  ffi_unref_type(cif->rtype);
  if (cif->arg_types) {
    unsigned i;
    for (i = 0; i < cif->nargs; i++)
      ffi_unref_type(cif->arg_types[i]);
    free(cif->arg_types);
  }
  free(cif);
}

ffi_cif *ffi_new_cif(ffi_abi abi, ffi_type *rtype, ffi_type **atypes)
{
  ffi_status rc;
  ffi_cif *cif;
  ffi_type **a;
  unsigned nargs = 0;
  if (!rtype || !atypes)
    return 0; /* FIXME: possible memleak on rtype or atypes */
  cif = calloc(1, sizeof(ffi_cif));
  assert(cif != 0);
  ffi_ref_type(rtype);
  for (a = atypes; *a; a++) {
    ffi_ref_type(*a);
    nargs++;
  }
  rc = ffi_prep_cif(cif, abi, nargs, rtype, atypes);
  if (rc == FFI_OK)
    return cif;
  else {
    ffi_free_cif(cif);
    return 0;
  }
}

/* Marshalling between Pure and C data. */

static void *ffi_to_c(void *v, ffi_type *type, pure_expr *x)
{
  void *p;
  char *s;
  double d;
  int32_t i;
  mpz_t z;
  if (!v) return 0;
  switch (type->type) {
  case FFI_TYPE_VOID:
    break;
  case FFI_TYPE_FLOAT:
    if (pure_is_double(x, &d))
      *(float*)v = (float)d;
    else if (pure_is_int(x, &i))
      *(float*)v = (float)i;
    else if (pure_is_mpz(x, &z)) {
      *(float*)v = (float)mpz_get_d(z);
      mpz_clear(z);
    } else
      return 0;
    break;
  case FFI_TYPE_DOUBLE:
    if (pure_is_double(x, &d))
      *(double*)v = d;
    else if (pure_is_int(x, &i))
      *(double*)v = (double)i;
    else if (pure_is_mpz(x, &z)) {
      *(double*)v = mpz_get_d(z);
      mpz_clear(z);
    } else
      return 0;
    break;
#if HAVE_LONG_DOUBLE
  case FFI_TYPE_LONGDOUBLE:
    if (pure_is_double(x, &d))
      *(long double*)v = (long double)d;
    else if (pure_is_int(x, &i))
      *(long double*)v = (long double)i;
    else if (pure_is_mpz(x, &z)) {
      *(long double*)v = (long double)mpz_get_d(z);
      mpz_clear(z);
    } else
      return 0;
    break;
#endif
  case FFI_TYPE_UINT8:
  case FFI_TYPE_SINT8:
    if (pure_is_int(x, &i))
      *(int8_t*)v = (int8_t)i;
    else if (pure_is_mpz(x, NULL)) {
      *(int8_t*)v = (int8_t)pure_get_int(x);
    } else
      return 0;
    break;
  case FFI_TYPE_UINT16:
  case FFI_TYPE_SINT16:
    if (pure_is_int(x, &i))
      *(int16_t*)v = (int16_t)i;
    else if (pure_is_mpz(x, NULL)) {
      *(int16_t*)v = (int16_t)pure_get_int(x);
    } else
      return 0;
    break;
  /* We assume 32 bit ints here. */
  case FFI_TYPE_INT:
  case FFI_TYPE_UINT32:
  case FFI_TYPE_SINT32:
    if (pure_is_int(x, &i))
      *(int32_t*)v = i;
    else if (pure_is_mpz(x, NULL)) {
      *(int32_t*)v = pure_get_int(x);
    } else
      return 0;
    break;
  case FFI_TYPE_UINT64:
  case FFI_TYPE_SINT64:
    if (pure_is_int(x, &i))
      *(int64_t*)v = (int64_t)i;
    else if (pure_is_mpz(x, NULL)) {
      *(int64_t*)v = pure_get_int64(x);
    } else
      return 0;
    break;
  case FFI_TYPE_POINTER:
    if (type == &ffi_type_string) {
      if (pure_is_cstring_dup(x, &s))
	/* This is a malloc'd string, remember to free it later! */
	*(void**)v = s;
      else
	return 0;
    } else {
      if (pure_is_pointer(x, &p))
	*(void**)v = p;
      else if (pure_is_string(x, (const char**)&s))
	/* Passes string as is, without encoding conversion. */
	*(void**)v = s;
      else
	return 0;
    }
    break;
  case FFI_TYPE_STRUCT: {
    /* This is supposed to be a pointer created with ffi_new_struct, which
       gets passed by value. */
    ffi_type *t;
    if (pure_is_struct(x, &t, &p) && same_type(type, t))
      memcpy(v, p, type->size);
    else
      return 0;
    break;
  }
  default:
    return 0;
  }
  return v;
}

static pure_expr *ffi_from_c(ffi_type *type, void *v)
{
  if (type == 0 || (type->type != FFI_TYPE_VOID && v == 0))
    return 0;
  switch (type->type) {
  case FFI_TYPE_VOID:
    return pure_tuplel(0);
  case FFI_TYPE_FLOAT:
    return pure_double((double)*(float*)v);
  case FFI_TYPE_DOUBLE:
    return pure_double(*(double*)v);
#if HAVE_LONG_DOUBLE
  case FFI_TYPE_LONGDOUBLE:
    return pure_double(*(long double*)v);
#endif
  case FFI_TYPE_UINT8:
  case FFI_TYPE_SINT8:
    return pure_int((int32_t)*(int8_t*)v);
  case FFI_TYPE_UINT16:
  case FFI_TYPE_SINT16:
    return pure_int((int32_t)*(int16_t*)v);
  /* We assume 32 bit ints here. */
  case FFI_TYPE_INT:
  case FFI_TYPE_UINT32:
  case FFI_TYPE_SINT32:
    return pure_int(*(int32_t*)v);
  case FFI_TYPE_UINT64:
  case FFI_TYPE_SINT64:
    return pure_int64(*(int64_t*)v);
  case FFI_TYPE_POINTER:
    if (type == &ffi_type_string)
      return pure_cstring_dup(*(char**)v);
    else
      return pure_pointer(*(void**)v);
  case FFI_TYPE_STRUCT:
    return pure_struct(type, v);
  default:
    return 0;
  }
}

static void **ffi_to_cvect(ffi_cif *cif, pure_expr *x)
{
  size_t i, j, n;
  pure_expr **xs;
  if (pure_is_tuplev(x, &n, &xs)) {
    void **v = 0;
    if (n != cif->nargs) goto err;
    if (n > 0) {
      v = malloc(n*sizeof(void*));
      assert(v!=0);
    }
    for (i = 0; i < n; i++) {
      v[i] = malloc(cif->arg_types[i]->size);
      if (cif->arg_types[i]->type == FFI_TYPE_VOID)
	continue;
      assert(cif->arg_types[i]->size == 0 || v[i] != 0);
      if (!ffi_to_c(v[i], cif->arg_types[i], xs[i])) {
	if (v[i]) free(v[i]);
	for (j = 0; j < i; j++) {
	  if (cif->arg_types[i] == &ffi_type_string) free(*(void**)v[j]);
	  free(v[j]);
	}
	free(v);
	v = 0;
	goto err;
      }
    }
  err:
    if (xs) free(xs);
    return v;
  } else
    return 0;
}

static pure_expr *ffi_from_cvect(ffi_cif *cif, void **v)
{
  size_t i, j, n = cif->nargs;
  pure_expr *x = 0, **xs = 0;
  if (n > 0) {
    xs = malloc(n*sizeof(pure_expr*));
    assert(xs!=0);
  }
  for (i = 0; i < n; i++) {
    xs[i] = ffi_from_c(cif->arg_types[i], v[i]);
    if (xs[i] == 0) {
      for (j = 0; j < i; j++)
	pure_freenew(xs[j]);
      goto err;
    }
  }
  x = pure_tuplev(n, xs);
 err:
  if (xs) free(xs);
  return x;
}

/* Call Pure -> C. */

pure_expr *ffi_fcall(ffi_cif *cif, void (*fn)(), pure_expr *x)
{
  pure_expr *y = 0;
  void *r = 0, **v = 0;
  if (!cif) return 0;
  if (cif->rtype->type != FFI_TYPE_VOID) {
    r = malloc(cif->rtype->size);
    assert(cif->rtype->size == 0 || r != 0);
  }
  v = ffi_to_cvect(cif, x);
  if (cif->nargs > 0 && !v) goto err;
  ffi_call(cif, fn, r, v);
  y = ffi_from_c(cif->rtype, r);
 err:
  if (r && cif->rtype->type != FFI_TYPE_STRUCT) free(r);
  if (v) {
    unsigned i;
    for (i = 0; i < cif->nargs; i++) {
      if (v[i]) {
	if (cif->arg_types[i] == &ffi_type_string) free(*(void**)v[i]);
	free(v[i]);
      }
    }
    free(v);
  }
  return y;
}

/* Call C -> Pure. This may be unsupported on some platforms. */

#if FFI_CLOSURES
typedef struct {
  pure_expr *fn;
  void *code;
} ffi_closure_data;
#else
#define ffi_closure_data void
#endif

#if FFI_CLOSURES
static void ffi_closure_fun(ffi_cif *cif, void *ret, void **args, void *v)
{
  ffi_closure_data *data = (ffi_closure_data*)v;
  pure_expr *x, *y;
  if (!cif) return;
  if (ret) memset(ret, 0, cif->rtype->size);
  x = ffi_from_cvect(cif, args);
  if (!x) return;
  y = pure_app(data->fn, x);
  if (!y) return;
  ffi_to_c(ret, cif->rtype, y);
  pure_freenew(y);
}
#endif

ffi_closure *ffi_new_closure(ffi_cif *cif, pure_expr *fn)
{
#if FFI_CLOSURES
  ffi_status rc;
  ffi_closure *clos;
  ffi_closure_data *data;
  void *code;
  if (!cif) return 0;
  data = malloc(sizeof(ffi_closure_data));
  assert(data!=0);
  clos = ffi_closure_alloc(sizeof(ffi_closure), &code);
  if (!clos) {
    free(data);
    ffi_free_cif(cif);
    return 0;
  }
  rc = ffi_prep_closure_loc(clos, cif, ffi_closure_fun, data, code);
  if (rc != FFI_OK) {
    free(data);
    ffi_free_cif(cif);
    ffi_closure_free(clos);
    return 0;
  }
  data->fn = pure_new(fn);
  data->code = code;
  return clos;
#else
  ffi_free_cif(cif);
  return 0;
#endif
}

void ffi_free_closure(ffi_closure *clos, void *code)
{
#if FFI_CLOSURES
  ffi_closure_data *data;
  if (!clos) return;
  data = (ffi_closure_data*)clos->user_data;
  pure_free(data->fn);
  ffi_free_cif(clos->cif);
  ffi_closure_free(clos);
#endif
}

void *ffi_closure_addr(ffi_closure *clos)
{
#if FFI_CLOSURES
  if (clos) {
    ffi_closure_data *data = (ffi_closure_data*)clos->user_data;
    return data->code;
  } else
#endif
    return 0;
}
