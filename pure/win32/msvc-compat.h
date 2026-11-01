/* Included ahead of every source file in the MSVC build (CMakeLists.txt,
   /FI). The Microsoft C runtime has the POSIX functions Pure uses, but
   declares some only under their ISO underscore names, and declares the
   others in <io.h>, <direct.h> and <process.h> rather than <unistd.h> and
   <stdio.h>. */
#ifndef PURE_MSVC_COMPAT_H
#define PURE_MSVC_COMPAT_H

#include <io.h>
#include <direct.h>
#include <process.h>
#include <stdio.h>
#include <string.h>

#define popen _popen
#define pclose _pclose
#define strcasecmp _stricmp
#define strncasecmp _strnicmp

#endif
