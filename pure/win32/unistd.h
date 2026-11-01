/* Minimal <unistd.h> for MSVC builds of Pure. The functions are declared
   by <io.h>, <direct.h> and <process.h> (see msvc-compat.h); this adds
   ssize_t, the S_IS* macros of <sys/stat.h>, and realpath (posix.c). */
#ifndef PURE_WIN32_UNISTD_H
#define PURE_WIN32_UNISTD_H

#include <io.h>
#include <direct.h>
#include <process.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <BaseTsd.h>

typedef SSIZE_T ssize_t;

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2
#endif

#ifndef S_ISDIR
#define S_ISDIR(m) (((m) & _S_IFMT) == _S_IFDIR)
#endif
#ifndef S_ISREG
#define S_ISREG(m) (((m) & _S_IFMT) == _S_IFREG)
#endif

#ifdef __cplusplus
extern "C"
#endif
char *realpath(const char *path, char *resolved);

#endif
