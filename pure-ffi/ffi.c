
#include <stdlib.h>
#include <string.h>
#include <ffi.h>
#include <pure/runtime.h>

/* Platform-specific ABI constants. This is probably incomplete; the values
   listed below were gleaned from the ffitarget.h file on a Linux system. Add
   other values as needed. */

void ffi_defs(void)
{
  pure_def(pure_sym("FFI_DEFAULT_ABI"), pure_int(FFI_DEFAULT_ABI));
#ifdef X86_WIN32
  pure_def(pure_sym("FFI_SYSV"), pure_int(FFI_SYSV));
  pure_def(pure_sym("FFI_STDCALL"), pure_int(FFI_STDCALL));
#else
#if defined(__i386__) || defined(__x86_64__)
  pure_def(pure_sym("FFI_SYSV"), pure_int(FFI_SYSV));
  pure_def(pure_sym("FFI_UNIX64"), pure_int(FFI_UNIX64));
#endif
#endif
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

/* Construct a call interface. */

void ffi_free_cif(ffi_cif *cif)
{
  if (!cif) return;
  if (cif->arg_types) free(cif->arg_types);
  free(cif);
}

ffi_cif *ffi_new_cif(ffi_abi abi, ffi_type *rtype, ffi_type **atypes)
{
  ffi_status rc;
  ffi_cif *cif;
  ffi_type **a;
  unsigned int nargs = 0;
  if (!rtype || !atypes) return 0;
  cif = calloc(1, sizeof(ffi_cif));
  if (!cif) return 0;
  for (a = atypes; *a; a++) nargs++;
  rc = ffi_prep_cif(cif, abi, nargs, rtype, atypes);
  if (rc == FFI_OK)
    return cif;
  else {
    ffi_free_cif(cif);
    return 0;
  }
}

/* Construct a 0-terminated type vector to be passed as the atypes argument of
   ffi_new_cif. */

ffi_type **ffi_typevect(pure_expr *types)
{
  void *p;
  ffi_type **v = 0;
  size_t i, n;
  pure_expr **xs;
  if (pure_is_tuplev(types, &n, &xs)) {
    int32_t tag;
    if (n == 0) {
      v = malloc(sizeof(ffi_type*));
      if (!v) goto err;
      v[0] = 0;
      return v;
    }
    tag = xs[0]->tag;
    for (i = 1; i < n; i++)
      if (xs[i]->tag != tag) goto err;
  } else
    return 0;
  if (pure_is_pointer(xs[0], &p)) {
    v = malloc((n+1)*sizeof(ffi_type*));
    if (!v) goto err;
    v[n] = 0;
    v[0] = (ffi_type*)p;
    for (i = 1; i < n; i++) {
      pure_is_pointer(xs[i], &p);
      v[i] = (ffi_type*)p;
    }
  }
 err:
  free(xs);
  return v;
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
    else
      return 0;
    break;
  case FFI_TYPE_DOUBLE:
    if (pure_is_double(x, &d))
      *(double*)v = d;
    else
      return 0;
    break;
  case FFI_TYPE_UINT8:
  case FFI_TYPE_SINT8:
    if (pure_is_int(x, &i))
      *(int8_t*)v = (int8_t)i;
    else if (pure_is_mpz(x, &z)) {
      mpz_clear(z);
      *(int8_t*)v = (int8_t)pure_get_int(x);
    } else
      return 0;
    break;
  case FFI_TYPE_UINT16:
  case FFI_TYPE_SINT16:
    if (pure_is_int(x, &i))
      *(int16_t*)v = (int16_t)i;
    else if (pure_is_mpz(x, &z)) {
      mpz_clear(z);
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
    else if (pure_is_mpz(x, &z)) {
      mpz_clear(z);
      *(int32_t*)v = pure_get_int(x);
    } else
      return 0;
    break;
  case FFI_TYPE_UINT64:
  case FFI_TYPE_SINT64:
    if (pure_is_int(x, &i))
      *(int64_t*)v = (int64_t)i;
    else if (pure_is_mpz(x, &z)) {
      mpz_clear(z);
      *(int64_t*)v = pure_get_long(x);
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
  case FFI_TYPE_STRUCT:
    /* TODO */
  default:
    return 0;
  }
  return v;
}

static pure_expr *ffi_from_c(ffi_type *type, void *v)
{
  if (type == 0 || type->size > 0 && v == 0)
    return 0;
  switch (type->type) {
  case FFI_TYPE_VOID:
    return pure_tuplel(0);
  case FFI_TYPE_FLOAT:
    return pure_double((double)*(float*)v);
  case FFI_TYPE_DOUBLE:
    return pure_double(*(double*)v);
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
    return pure_long(*(int64_t*)v);
  case FFI_TYPE_POINTER:
    if (type == &ffi_type_string)
      return pure_cstring_dup(*(char**)v);
    else
      return pure_pointer(*(void**)v);
  case FFI_TYPE_STRUCT:
    /* TODO */
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
    if (n > 0 && (v = malloc(n*sizeof(void*))) == 0)
      goto err;
    for (i = 0; i < n; i++) {
      v[i] = malloc(cif->arg_types[i]->size);
      if (cif->arg_types[i]->type == FFI_TYPE_VOID)
	continue;
      if (!v[i] || !ffi_to_c(v[i], cif->arg_types[i], xs[i])) {
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
  if (n > 0 && (xs = malloc(n*sizeof(pure_expr*))) == 0)
    goto err;
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

pure_expr *ffi_c_call(ffi_cif *cif, void (*fn)(), pure_expr *x)
{
  pure_expr *y = 0;
  void *r = 0, **v = 0;
  if (!cif) return 0;
  if (cif->rtype->type != FFI_TYPE_VOID) {
    r = malloc(cif->rtype->size);
    if (!r) goto err;
  }
  v = ffi_to_cvect(cif, x);
  if (cif->nargs > 0 && !v) goto err;
  ffi_call(cif, fn, r, v);
  y = ffi_from_c(cif->rtype, r);
 err:
  if (r) free(r);
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

typedef struct {
  pure_expr *fn;
  void *code;
} ffi_closure_data;

void ffi_closure_fun(ffi_cif *cif, void *ret, void **args, void *v)
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

ffi_closure *ffi_new_closure(ffi_cif *cif, pure_expr *fn)
{
  ffi_status rc;
  ffi_closure *clos;
  ffi_closure_data *data;
  void *code;
  if (!cif) return 0;
  data = malloc(sizeof(ffi_closure_data));
  if (!data) {
    ffi_free_cif(cif);
    return 0;
  }
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
}

void ffi_free_closure(ffi_closure *clos, void *code)
{
  ffi_closure_data *data;
  if (!clos) return;
  data = (ffi_closure_data*)clos->user_data;
  pure_free(data->fn);
  ffi_free_cif(clos->cif);
  ffi_closure_free(clos);
}

void *ffi_closure_addr(ffi_closure *clos)
{
  if (clos) {
    ffi_closure_data *data = (ffi_closure_data*)clos->user_data;
    return data->code;
  } else
    return 0;
}
