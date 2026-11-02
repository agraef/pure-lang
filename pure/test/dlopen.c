/* Load libpure the way a plugin host does (PostgreSQL extensions, Pd
   externals, LV2 plugins): with dlopen, into a process that is not linked
   against it, and use it from the main thread and a second thread.

   libpure uses the initial-exec TLS model for its hot per-thread state (see
   PURE_TLS_HOT in util.hh), so its whole TLS segment must fit in the
   static TLS space the dynamic loader keeps for dlopen'ed libraries. That
   is only 128 bytes by default on FreeBSD; glibc keeps more, so on Linux
   the dlopen alone would not notice the segment growing. The test also
   reads the size of the segment and fails if it is over 128 bytes.

   Usage: test/dlopen path/to/libpure.so */

#define _GNU_SOURCE /* dl_iterate_phdr on glibc */
#include <dlfcn.h>
#include <pthread.h>
#include <stdio.h>
#include <string.h>

#if defined(__ELF__)
#include <link.h>
#endif

#define TLS_BUDGET 128

typedef void *(*create_fn)(int, char **);
typedef void (*switch_fn)(void *);
typedef void *(*eval_fn)(const char *);
typedef int (*is_int_fn)(void *, int *);
typedef void (*delete_fn)(void *);

static create_fn create;
static switch_fn sw;
static eval_fn eval;
static is_int_fn is_int;
static delete_fn del;

static int run(const char *expr, int want)
{
	char *argv[] = {"dlopen", "--noprelude", NULL};
	void *interp = create(2, argv);
	void *x;
	int k = 0;

	if (!interp)
		return 0;
	sw(interp);
	x = eval(expr);
	if (!x || !is_int(x, &k))
		k = want + 1;
	del(interp);
	return k == want;
}

static void *thread_main(void *arg)
{
	(void) arg;
	return (void *) (long) run("6*7", 42);
}

#if defined(__ELF__)
/* Find the size of libpure's TLS segment from its program headers, and
   count the loaded objects named libpure* (there must be one). */
static long tls_size = -1;
static int ncopies = 0;

static int phdr_cb(struct dl_phdr_info *info, size_t size, void *data)
{
	const char *base;

	(void) size;
	(void) data;
	if (!info->dlpi_name)
		return 0;
	base = strrchr(info->dlpi_name, '/');
	base = base ? base + 1 : info->dlpi_name;
	if (strncmp(base, "libpure", 7) != 0)
		return 0;
	ncopies++;
	if (tls_size < 0) {
		tls_size = 0;
		for (int i = 0; i < info->dlpi_phnum; i++)
			if (info->dlpi_phdr[i].p_type == PT_TLS)
				tls_size = (long) info->dlpi_phdr[i].p_memsz;
	}
	return 0;
}
#endif

int main(int argc, char **argv)
{
	void *h, *res;
	pthread_t t;
	int ok = 1;

	if (argc != 2) {
		fprintf(stderr, "usage: %s libpure.so\n", argv[0]);
		return 2;
	}
	h = dlopen(argv[1], RTLD_NOW | RTLD_GLOBAL);
	if (!h) {
		printf("FAIL: dlopen: %s\n", dlerror());
		return 1;
	}
	printf("ok:   dlopen\n");
	create = (create_fn) dlsym(h, "pure_create_interp");
	sw = (switch_fn) dlsym(h, "pure_switch_interp");
	eval = (eval_fn) dlsym(h, "pure_eval");
	is_int = (is_int_fn) dlsym(h, "pure_is_int");
	del = (delete_fn) dlsym(h, "pure_delete_interp");
	if (!create || !sw || !eval || !is_int || !del) {
		printf("FAIL: dlsym\n");
		return 1;
	}
	if (run("1+2", 3))
		printf("ok:   evaluate on the main thread\n");
	else {
		printf("FAIL: evaluate on the main thread\n");
		ok = 0;
	}
	if (pthread_create(&t, NULL, thread_main, NULL) == 0 &&
	    pthread_join(t, &res) == 0 && res)
		printf("ok:   evaluate on a second thread\n");
	else {
		printf("FAIL: evaluate on a second thread\n");
		ok = 0;
	}
#if defined(__ELF__)
	dl_iterate_phdr(phdr_cb, NULL);
	if (ncopies == 1)
		printf("ok:   one copy of libpure is loaded\n");
	else {
		printf("FAIL: %d copies of libpure are loaded\n", ncopies);
		ok = 0;
	}
	if (tls_size < 0)
		printf("skip: TLS segment size (libpure not found)\n");
	else if (tls_size <= TLS_BUDGET)
		printf("ok:   TLS segment is %ld bytes (limit %d)\n", tls_size,
		       TLS_BUDGET);
	else {
		printf("FAIL: TLS segment is %ld bytes, over the %d bytes FreeBSD "
		       "reserves for dlopen\n", tls_size, TLS_BUDGET);
		ok = 0;
	}
#endif
	printf(ok ? "dlopen tests passed\n" : "dlopen tests FAILED\n");
	return !ok;
}
