/* POSIX names for the MSVC build of libpure.

   Pure programs call C functions by name through the JIT (system.pure
   declares extern fileno, popen, execv, sleep, ...), which looks the names
   up in the loaded DLLs. The Microsoft C runtime exports most of these only
   under their ISO underscore names (_fileno, _popen, _execv); the plain
   names are link-time aliases in oldnames.lib that the JIT cannot see. So
   libpure exports them itself, as the MinGW build does in runtime.cc. They
   are defined here under a pure_win32_ prefix and exported under the POSIX
   name, so that they do not collide with the CRT's own declarations.

   realpath(3), which interpreter.cc uses to canonicalize script paths, is
   also missing; it is defined here with _fullpath, failing like realpath
   when the file does not exist. */

#include <errno.h>
#include <io.h>
#include <process.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <windows.h>

#define EXPORT_AS(posix, ours) \
  __pragma(comment(linker, "/EXPORT:" #posix "=" #ours))

int pure_win32_fileno(FILE *fp) { return _fileno(fp); }
EXPORT_AS(fileno, pure_win32_fileno)

FILE *pure_win32_fdopen(int fd, const char *mode) { return _fdopen(fd, mode); }
EXPORT_AS(fdopen, pure_win32_fdopen)

FILE *pure_win32_popen(const char *cmd, const char *mode) { return _popen(cmd, mode); }
EXPORT_AS(popen, pure_win32_popen)

int pure_win32_pclose(FILE *fp) { return _pclose(fp); }
EXPORT_AS(pclose, pure_win32_pclose)

unsigned int pure_win32_sleep(unsigned int secs)
{
  Sleep(secs * 1000U);
  return 0;
}
EXPORT_AS(sleep, pure_win32_sleep)

/* The CRT's exec and spawn functions take const char *const * argument
   vectors and return intptr_t, as the MinGW-w64 ones do (system.pure
   declares the spawn functions int64 on 64 bit Windows). */

typedef const char *const *argv_t;

intptr_t pure_win32_execv(const char *prog, char *const argv[])
{
  return _execv(prog, (argv_t)argv);
}
EXPORT_AS(execv, pure_win32_execv)

intptr_t pure_win32_execvp(const char *prog, char *const argv[])
{
  return _execvp(prog, (argv_t)argv);
}
EXPORT_AS(execvp, pure_win32_execvp)

intptr_t pure_win32_execve(const char *prog, char *const argv[],
			   char *const envp[])
{
  return _execve(prog, (argv_t)argv, (argv_t)envp);
}
EXPORT_AS(execve, pure_win32_execve)

intptr_t pure_win32_spawnv(int mode, const char *prog, char *const argv[])
{
  return _spawnv(mode, prog, (argv_t)argv);
}
EXPORT_AS(spawnv, pure_win32_spawnv)

intptr_t pure_win32_spawnvp(int mode, const char *prog, char *const argv[])
{
  return _spawnvp(mode, prog, (argv_t)argv);
}
EXPORT_AS(spawnvp, pure_win32_spawnvp)

intptr_t pure_win32_spawnve(int mode, const char *prog, char *const argv[],
			    char *const envp[])
{
  return _spawnve(mode, prog, (argv_t)argv, (argv_t)envp);
}
EXPORT_AS(spawnve, pure_win32_spawnve)

char *realpath(const char *path, char *resolved)
{
  struct _stat64 st;
  char *res;
  if (!path) {
    errno = EINVAL;
    return NULL;
  }
  if (_stat64(path, &st)) return NULL; /* errno set by _stat64 */
  res = _fullpath(resolved, path, resolved ? _MAX_PATH : 0);
  if (!res) errno = ENAMETOOLONG;
  return res;
}

/* lib/primitives.pure declares round and trunc as __round and __trunc (a
   MinGW workaround in runtime.cc). MSVC treats those names as intrinsics,
   so runtime.cc cannot define them; export them from here instead. */

#include <math.h>

double pure_win32_round(double x) { return round(x); }
EXPORT_AS(__round, pure_win32_round)

double pure_win32_trunc(double x) { return trunc(x); }
EXPORT_AS(__trunc, pure_win32_trunc)
