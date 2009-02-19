#include <stdio.h>
#include <stdlib.h>

#include <pure/runtime.h>

/* Some of this is cribbed from Sven Panne's excellent Haskell OpenGL
   bindings. */

/*
Copyright (c) 2009, Scott E Dillard
Copyright (c) 2002-2005, Sven Panne
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of its contributors may be
   used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef _WIN32
#define APIENTRY 
#endif

#if defined(_WIN32) /* Windows */
#include <windows.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

static void*
get_proc_addr(const char *name)
{
  static int firstTime = 1;
  static HINSTANCE gl32 = 0, glu32 = 0, glut = 0;
  void *p;
  if (firstTime) {
    gl32 = LoadLibrary("opengl32.dll");
    glu32 = LoadLibrary("glu32.dll");
    glut = LoadLibrary("freeglut.dll");
    firstTime = 0;
  }
  (void)
    ((p = wglGetProcAddress(name)) ||
     (p = GetProcAddress(gl32, name)) ||
     (p = GetProcAddress(glu32, name)) ||
     (p = GetProcAddress(glut, name)));
  return p;
}

#elif defined(__APPLE__) /* Mac */
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <OpenGL/glut.h>
#include <mach-o/dyld.h>

static void*
get_proc_addr(const char *name)
{
  NSSymbol symbol;

  /* Prepend a '_' for the Unix C symbol mangling convention */
  char* symbolName = (char*)malloc(strlen(name) + 2);
  if (!symbolName) {
    fprintf(stderr, "Failed to allocate memory for NSGLGetProcAddress\n");
    return NULL;
  }
  symbolName[0] = '_';
  strcpy(symbolName + 1, name);

  if (!NSIsSymbolNameDefined(symbolName)) {
    free(symbolName);
    return NULL;
  }

  symbol = NSLookupAndBindSymbol(symbolName);
  free(symbolName);
  if (!symbol) {
    return NULL;
  }

  return NSAddressOfSymbol(symbol);
}

/* ToDo: This should really be based on a feature test. */
#elif defined(__sgi) || defined (__sun)
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <dlfcn.h>

static const char* gpaNames[] = {
  "glXGetProcAddress", "glXGetProcAddressARB", "glXGetProcAddressEXT",
  "_glXGetProcAddress", "_glXGetProcAddressARB", "_glXGetProcAddressEXT"
};

static void*
get_proc_addr(const char *name)
{
  static int firstTime = 1;
  static void *handle = NULL;
  static void *gpa = NULL;

  if (firstTime) {
    firstTime = 0;

    /* Get a handle for our executable. */
    handle = dlopen(NULL, RTLD_LAZY);
    /* If fail this early, there's not much we can do about it. */
    if (!handle) {
      return NULL;
    }

    {
      /* Let's see if our platform supports a glXGetProcAddress() variant. */
      int numNames = (int)(sizeof(gpaNames) / sizeof(gpaNames[0]));
      int i;
      for (i = 0;   (!gpa) && (i < numNames);   ++i) {
        gpa = dlsym(handle, gpaNames[i]);
      }
    }
  }

  if (gpa) {
    /* Fine, we seem to have some kind of glXGetProcAddress(), so use it. */
    return ((void *(*)(const GLubyte *))gpa)(name);
  } else if (handle) {
    /* Fallback to dlsym() if we have no glXGetProcAddress(), although we then
       ignore the fact that OpenGL entry points could be context dependent. */
    return dlsym(handle, name);
  } else {
    return NULL;
  }
}

#else /* Linux */
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <GL/glx.h>
#include <dlfcn.h>

static void*
get_proc_addr(const char *name)
{
  static int firstTime = 1;
  static void *glu = NULL, *glut = NULL;
  void *p;
  if (firstTime) {
    firstTime = 0;
    glu = dlopen("libGLU.so", RTLD_LAZY);
    glut = dlopen("libglut.so", RTLD_LAZY);
  }
  /* Make sure to call these in this order. For whatever reason,
     glXGetProcAddress appears to yield bogus addresses for GLUT routines. */
  (void)
    ((glu && (p = dlsym(glu, name))) ||
     (glut && (p = dlsym(glut, name))) ||
     (p = glXGetProcAddress((const GLubyte*)name)));
  return p;
}

#endif

static void throw_unsupported(const char* name)
{
  pure_throw(pure_app(pure_symbol(pure_sym("gl_unsupported")), 
		      pure_cstring_dup(name) ));
}

void Pure_glAccum(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glAccum";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glAlphaFunc(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glAlphaFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glAreTexturesResident(int arg0, unsigned int const* arg1, unsigned char* arg2)
{
  static unsigned char(APIENTRY*ptr)(int arg0, unsigned int const* arg1, unsigned char* arg2) = NULL;
  static const char name[] = "glAreTexturesResident";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(int arg0, unsigned int const* arg1, unsigned char* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glArrayElement(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glArrayElement";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBegin(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glBegin";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBindTexture(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBindTexture";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glBitmap(int arg0, int arg1, float arg2, float arg3, float arg4, float arg5, unsigned char const* arg6)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, float arg2, float arg3, float arg4, float arg5, unsigned char const* arg6) = NULL;
  static const char name[] = "glBitmap";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, float arg2, float arg3, float arg4, float arg5, unsigned char const* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glBlendFunc(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBlendFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glCallList(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glCallList";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glCallLists(int arg0, unsigned int arg1, void const* arg2)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, void const* arg2) = NULL;
  static const char name[] = "glCallLists";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, void const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glClear(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glClear";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glClearAccum(float arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glClearAccum";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glClearColor(float arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glClearColor";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glClearDepth(double arg0)
{
  static void(APIENTRY*ptr)(double arg0) = NULL;
  static const char name[] = "glClearDepth";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glClearIndex(float arg0)
{
  static void(APIENTRY*ptr)(float arg0) = NULL;
  static const char name[] = "glClearIndex";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glClearStencil(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glClearStencil";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glClipPlane(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glClipPlane";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glColor3b(char arg0, char arg1, char arg2)
{
  static void(APIENTRY*ptr)(char arg0, char arg1, char arg2) = NULL;
  static const char name[] = "glColor3b";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char arg0, char arg1, char arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColor3bv(char const* arg0)
{
  static void(APIENTRY*ptr)(char const* arg0) = NULL;
  static const char name[] = "glColor3bv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor3d(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glColor3d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColor3dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glColor3dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor3f(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glColor3f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColor3fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glColor3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor3i(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glColor3i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColor3iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glColor3iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor3s(short arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glColor3s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColor3sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glColor3sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor3ub(unsigned char arg0, unsigned char arg1, unsigned char arg2)
{
  static void(APIENTRY*ptr)(unsigned char arg0, unsigned char arg1, unsigned char arg2) = NULL;
  static const char name[] = "glColor3ub";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char arg0, unsigned char arg1, unsigned char arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColor3ubv(unsigned char const* arg0)
{
  static void(APIENTRY*ptr)(unsigned char const* arg0) = NULL;
  static const char name[] = "glColor3ubv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor3ui(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glColor3ui";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColor3uiv(unsigned int const* arg0)
{
  static void(APIENTRY*ptr)(unsigned int const* arg0) = NULL;
  static const char name[] = "glColor3uiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor3us(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2) = NULL;
  static const char name[] = "glColor3us";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColor3usv(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glColor3usv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor4b(char arg0, char arg1, char arg2, char arg3)
{
  static void(APIENTRY*ptr)(char arg0, char arg1, char arg2, char arg3) = NULL;
  static const char name[] = "glColor4b";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char arg0, char arg1, char arg2, char arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glColor4bv(char const* arg0)
{
  static void(APIENTRY*ptr)(char const* arg0) = NULL;
  static const char name[] = "glColor4bv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor4d(double arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glColor4d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glColor4dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glColor4dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor4f(float arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glColor4f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glColor4fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glColor4fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor4i(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glColor4i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glColor4iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glColor4iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor4s(short arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glColor4s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glColor4sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glColor4sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor4ub(unsigned char arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3)
{
  static void(APIENTRY*ptr)(unsigned char arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3) = NULL;
  static const char name[] = "glColor4ub";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glColor4ubv(unsigned char const* arg0)
{
  static void(APIENTRY*ptr)(unsigned char const* arg0) = NULL;
  static const char name[] = "glColor4ubv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor4ui(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glColor4ui";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glColor4uiv(unsigned int const* arg0)
{
  static void(APIENTRY*ptr)(unsigned int const* arg0) = NULL;
  static const char name[] = "glColor4uiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor4us(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3) = NULL;
  static const char name[] = "glColor4us";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glColor4usv(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glColor4usv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColorMask(unsigned char arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3)
{
  static void(APIENTRY*ptr)(unsigned char arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3) = NULL;
  static const char name[] = "glColorMask";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glColorMaterial(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glColorMaterial";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glColorPointer(int arg0, unsigned int arg1, int arg2, void const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, int arg2, void const* arg3) = NULL;
  static const char name[] = "glColorPointer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, int arg2, void const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glCopyPixels(int arg0, int arg1, int arg2, int arg3, unsigned int arg4)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3, unsigned int arg4) = NULL;
  static const char name[] = "glCopyPixels";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3, unsigned int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glCopyTexImage1D(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6) = NULL;
  static const char name[] = "glCopyTexImage1D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glCopyTexImage2D(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7) = NULL;
  static const char name[] = "glCopyTexImage2D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_glCopyTexSubImage1D(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) = NULL;
  static const char name[] = "glCopyTexSubImage1D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glCopyTexSubImage2D(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7) = NULL;
  static const char name[] = "glCopyTexSubImage2D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_glCullFace(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glCullFace";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDeleteLists(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glDeleteLists";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDeleteTextures(int arg0, unsigned int const* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int const* arg1) = NULL;
  static const char name[] = "glDeleteTextures";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDepthFunc(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDepthFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDepthMask(unsigned char arg0)
{
  static void(APIENTRY*ptr)(unsigned char arg0) = NULL;
  static const char name[] = "glDepthMask";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDepthRange(double arg0, double arg1)
{
  static void(APIENTRY*ptr)(double arg0, double arg1) = NULL;
  static const char name[] = "glDepthRange";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDisable(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDisable";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDisableClientState(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDisableClientState";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDrawArrays(unsigned int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glDrawArrays";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glDrawBuffer(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDrawBuffer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDrawElements(unsigned int arg0, int arg1, unsigned int arg2, void const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, void const* arg3) = NULL;
  static const char name[] = "glDrawElements";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, void const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glDrawPixels(int arg0, int arg1, unsigned int arg2, unsigned int arg3, void const* arg4)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned int arg2, unsigned int arg3, void const* arg4) = NULL;
  static const char name[] = "glDrawPixels";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned int arg2, unsigned int arg3, void const* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glEdgeFlag(unsigned char arg0)
{
  static void(APIENTRY*ptr)(unsigned char arg0) = NULL;
  static const char name[] = "glEdgeFlag";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEdgeFlagPointer(int arg0, void const* arg1)
{
  static void(APIENTRY*ptr)(int arg0, void const* arg1) = NULL;
  static const char name[] = "glEdgeFlagPointer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, void const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glEdgeFlagv(unsigned char const* arg0)
{
  static void(APIENTRY*ptr)(unsigned char const* arg0) = NULL;
  static const char name[] = "glEdgeFlagv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEnable(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glEnable";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEnableClientState(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glEnableClientState";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEnd()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glEnd";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glEndList()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glEndList";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glEvalCoord1d(double arg0)
{
  static void(APIENTRY*ptr)(double arg0) = NULL;
  static const char name[] = "glEvalCoord1d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEvalCoord1dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glEvalCoord1dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEvalCoord1f(float arg0)
{
  static void(APIENTRY*ptr)(float arg0) = NULL;
  static const char name[] = "glEvalCoord1f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEvalCoord1fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glEvalCoord1fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEvalCoord2d(double arg0, double arg1)
{
  static void(APIENTRY*ptr)(double arg0, double arg1) = NULL;
  static const char name[] = "glEvalCoord2d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glEvalCoord2dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glEvalCoord2dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEvalCoord2f(float arg0, float arg1)
{
  static void(APIENTRY*ptr)(float arg0, float arg1) = NULL;
  static const char name[] = "glEvalCoord2f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glEvalCoord2fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glEvalCoord2fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEvalMesh1(unsigned int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glEvalMesh1";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glEvalMesh2(unsigned int arg0, int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glEvalMesh2";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glEvalPoint1(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glEvalPoint1";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEvalPoint2(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glEvalPoint2";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glFeedbackBuffer(int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glFeedbackBuffer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glFinish()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glFinish";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glFlush()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glFlush";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glFogf(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glFogf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glFogfv(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glFogfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glFogi(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glFogi";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glFogiv(unsigned int arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1) = NULL;
  static const char name[] = "glFogiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glFrontFace(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glFrontFace";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glFrustum(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5) = NULL;
  static const char name[] = "glFrustum";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

unsigned int Pure_glGenLists(int arg0)
{
  static unsigned int(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glGenLists";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glGenTextures(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenTextures";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetBooleanv(unsigned int arg0, unsigned char* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char* arg1) = NULL;
  static const char name[] = "glGetBooleanv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetClipPlane(unsigned int arg0, double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double* arg1) = NULL;
  static const char name[] = "glGetClipPlane";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetDoublev(unsigned int arg0, double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double* arg1) = NULL;
  static const char name[] = "glGetDoublev";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned int Pure_glGetError()
{
  static unsigned int(APIENTRY*ptr)() = NULL;
  static const char name[] = "glGetError";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glGetFloatv(unsigned int arg0, float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float* arg1) = NULL;
  static const char name[] = "glGetFloatv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetIntegerv(unsigned int arg0, int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int* arg1) = NULL;
  static const char name[] = "glGetIntegerv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetLightfv(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetLightfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetLightiv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetLightiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMapdv(unsigned int arg0, unsigned int arg1, double* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double* arg2) = NULL;
  static const char name[] = "glGetMapdv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMapfv(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetMapfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMapiv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetMapiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMaterialfv(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetMaterialfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMaterialiv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetMaterialiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetPixelMapfv(unsigned int arg0, float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float* arg1) = NULL;
  static const char name[] = "glGetPixelMapfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetPixelMapuiv(unsigned int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGetPixelMapuiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetPixelMapusv(unsigned int arg0, unsigned short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short* arg1) = NULL;
  static const char name[] = "glGetPixelMapusv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetPointerv(unsigned int arg0, void** arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, void** arg1) = NULL;
  static const char name[] = "glGetPointerv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, void** arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetPolygonStipple(unsigned char* arg0)
{
  static void(APIENTRY*ptr)(unsigned char* arg0) = NULL;
  static const char name[] = "glGetPolygonStipple";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

unsigned char const* Pure_glGetString(unsigned int arg0)
{
  static unsigned char const*(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glGetString";
  if (!ptr) {
    ptr = (unsigned char const*(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glGetTexEnvfv(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetTexEnvfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetTexEnviv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetTexEnviv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetTexGendv(unsigned int arg0, unsigned int arg1, double* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double* arg2) = NULL;
  static const char name[] = "glGetTexGendv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetTexGenfv(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetTexGenfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetTexGeniv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetTexGeniv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetTexImage(unsigned int arg0, int arg1, unsigned int arg2, unsigned int arg3, void* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, unsigned int arg3, void* arg4) = NULL;
  static const char name[] = "glGetTexImage";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, unsigned int arg3, void* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetTexLevelParameterfv(unsigned int arg0, int arg1, unsigned int arg2, float* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, float* arg3) = NULL;
  static const char name[] = "glGetTexLevelParameterfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, float* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetTexLevelParameteriv(unsigned int arg0, int arg1, unsigned int arg2, int* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int* arg3) = NULL;
  static const char name[] = "glGetTexLevelParameteriv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetTexParameterfv(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetTexParameterfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetTexParameteriv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetTexParameteriv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glHint(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glHint";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glIndexMask(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIndexMask";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glIndexPointer(unsigned int arg0, int arg1, void const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, void const* arg2) = NULL;
  static const char name[] = "glIndexPointer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, void const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glIndexd(double arg0)
{
  static void(APIENTRY*ptr)(double arg0) = NULL;
  static const char name[] = "glIndexd";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glIndexdv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glIndexdv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glIndexf(float arg0)
{
  static void(APIENTRY*ptr)(float arg0) = NULL;
  static const char name[] = "glIndexf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glIndexfv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glIndexfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glIndexi(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glIndexi";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glIndexiv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glIndexiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glIndexs(short arg0)
{
  static void(APIENTRY*ptr)(short arg0) = NULL;
  static const char name[] = "glIndexs";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glIndexsv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glIndexsv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glIndexub(unsigned char arg0)
{
  static void(APIENTRY*ptr)(unsigned char arg0) = NULL;
  static const char name[] = "glIndexub";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glIndexubv(unsigned char const* arg0)
{
  static void(APIENTRY*ptr)(unsigned char const* arg0) = NULL;
  static const char name[] = "glIndexubv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glInitNames()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glInitNames";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glInterleavedArrays(unsigned int arg0, int arg1, void const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, void const* arg2) = NULL;
  static const char name[] = "glInterleavedArrays";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, void const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

unsigned char Pure_glIsEnabled(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsEnabled";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

unsigned char Pure_glIsList(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsList";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

unsigned char Pure_glIsTexture(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsTexture";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glLightModelf(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glLightModelf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glLightModelfv(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glLightModelfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glLightModeli(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glLightModeli";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glLightModeliv(unsigned int arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1) = NULL;
  static const char name[] = "glLightModeliv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glLightf(unsigned int arg0, unsigned int arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float arg2) = NULL;
  static const char name[] = "glLightf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glLightfv(unsigned int arg0, unsigned int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float const* arg2) = NULL;
  static const char name[] = "glLightfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glLighti(unsigned int arg0, unsigned int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2) = NULL;
  static const char name[] = "glLighti";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glLightiv(unsigned int arg0, unsigned int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int const* arg2) = NULL;
  static const char name[] = "glLightiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glLineStipple(int arg0, unsigned short arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned short arg1) = NULL;
  static const char name[] = "glLineStipple";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glLineWidth(float arg0)
{
  static void(APIENTRY*ptr)(float arg0) = NULL;
  static const char name[] = "glLineWidth";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glListBase(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glListBase";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glLoadIdentity()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glLoadIdentity";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glLoadMatrixd(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glLoadMatrixd";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glLoadMatrixf(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glLoadMatrixf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glLoadName(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glLoadName";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glLogicOp(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glLogicOp";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMap1d(unsigned int arg0, double arg1, double arg2, int arg3, int arg4, double const* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, int arg3, int arg4, double const* arg5) = NULL;
  static const char name[] = "glMap1d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, int arg3, int arg4, double const* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glMap1f(unsigned int arg0, float arg1, float arg2, int arg3, int arg4, float const* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, int arg3, int arg4, float const* arg5) = NULL;
  static const char name[] = "glMap1f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, int arg3, int arg4, float const* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glMap2d(unsigned int arg0, double arg1, double arg2, int arg3, int arg4, double arg5, double arg6, int arg7, int arg8, double const* arg9)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, int arg3, int arg4, double arg5, double arg6, int arg7, int arg8, double const* arg9) = NULL;
  static const char name[] = "glMap2d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, int arg3, int arg4, double arg5, double arg6, int arg7, int arg8, double const* arg9))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_glMap2f(unsigned int arg0, float arg1, float arg2, int arg3, int arg4, float arg5, float arg6, int arg7, int arg8, float const* arg9)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, int arg3, int arg4, float arg5, float arg6, int arg7, int arg8, float const* arg9) = NULL;
  static const char name[] = "glMap2f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, int arg3, int arg4, float arg5, float arg6, int arg7, int arg8, float const* arg9))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_glMapGrid1d(int arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(int arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glMapGrid1d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMapGrid1f(int arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(int arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glMapGrid1f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMapGrid2d(int arg0, double arg1, double arg2, int arg3, double arg4, double arg5)
{
  static void(APIENTRY*ptr)(int arg0, double arg1, double arg2, int arg3, double arg4, double arg5) = NULL;
  static const char name[] = "glMapGrid2d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, double arg1, double arg2, int arg3, double arg4, double arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glMapGrid2f(int arg0, float arg1, float arg2, int arg3, float arg4, float arg5)
{
  static void(APIENTRY*ptr)(int arg0, float arg1, float arg2, int arg3, float arg4, float arg5) = NULL;
  static const char name[] = "glMapGrid2f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float arg1, float arg2, int arg3, float arg4, float arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glMaterialf(unsigned int arg0, unsigned int arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float arg2) = NULL;
  static const char name[] = "glMaterialf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMaterialfv(unsigned int arg0, unsigned int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float const* arg2) = NULL;
  static const char name[] = "glMaterialfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMateriali(unsigned int arg0, unsigned int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2) = NULL;
  static const char name[] = "glMateriali";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMaterialiv(unsigned int arg0, unsigned int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int const* arg2) = NULL;
  static const char name[] = "glMaterialiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMatrixMode(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glMatrixMode";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMultMatrixd(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glMultMatrixd";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMultMatrixf(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glMultMatrixf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glNewList(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glNewList";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glNormal3b(char arg0, char arg1, char arg2)
{
  static void(APIENTRY*ptr)(char arg0, char arg1, char arg2) = NULL;
  static const char name[] = "glNormal3b";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char arg0, char arg1, char arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glNormal3bv(char const* arg0)
{
  static void(APIENTRY*ptr)(char const* arg0) = NULL;
  static const char name[] = "glNormal3bv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glNormal3d(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glNormal3d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glNormal3dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glNormal3dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glNormal3f(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glNormal3f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glNormal3fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glNormal3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glNormal3i(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glNormal3i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glNormal3iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glNormal3iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glNormal3s(short arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glNormal3s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glNormal3sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glNormal3sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glNormalPointer(unsigned int arg0, int arg1, void const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, void const* arg2) = NULL;
  static const char name[] = "glNormalPointer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, void const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glOrtho(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5) = NULL;
  static const char name[] = "glOrtho";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glPassThrough(float arg0)
{
  static void(APIENTRY*ptr)(float arg0) = NULL;
  static const char name[] = "glPassThrough";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glPixelMapfv(unsigned int arg0, int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, float const* arg2) = NULL;
  static const char name[] = "glPixelMapfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glPixelMapuiv(unsigned int arg0, int arg1, unsigned int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int const* arg2) = NULL;
  static const char name[] = "glPixelMapuiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glPixelMapusv(unsigned int arg0, int arg1, unsigned short const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned short const* arg2) = NULL;
  static const char name[] = "glPixelMapusv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned short const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glPixelStoref(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glPixelStoref";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPixelStorei(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glPixelStorei";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPixelTransferf(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glPixelTransferf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPixelTransferi(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glPixelTransferi";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPixelZoom(float arg0, float arg1)
{
  static void(APIENTRY*ptr)(float arg0, float arg1) = NULL;
  static const char name[] = "glPixelZoom";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPointSize(float arg0)
{
  static void(APIENTRY*ptr)(float arg0) = NULL;
  static const char name[] = "glPointSize";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glPolygonMode(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glPolygonMode";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPolygonOffset(float arg0, float arg1)
{
  static void(APIENTRY*ptr)(float arg0, float arg1) = NULL;
  static const char name[] = "glPolygonOffset";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPolygonStipple(unsigned char const* arg0)
{
  static void(APIENTRY*ptr)(unsigned char const* arg0) = NULL;
  static const char name[] = "glPolygonStipple";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glPopAttrib()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glPopAttrib";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glPopClientAttrib()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glPopClientAttrib";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glPopMatrix()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glPopMatrix";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glPopName()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glPopName";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glPrioritizeTextures(int arg0, unsigned int const* arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int const* arg1, float const* arg2) = NULL;
  static const char name[] = "glPrioritizeTextures";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int const* arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glPushAttrib(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glPushAttrib";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glPushClientAttrib(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glPushClientAttrib";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glPushMatrix()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glPushMatrix";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glPushName(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glPushName";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos2d(double arg0, double arg1)
{
  static void(APIENTRY*ptr)(double arg0, double arg1) = NULL;
  static const char name[] = "glRasterPos2d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glRasterPos2dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glRasterPos2dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos2f(float arg0, float arg1)
{
  static void(APIENTRY*ptr)(float arg0, float arg1) = NULL;
  static const char name[] = "glRasterPos2f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glRasterPos2fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glRasterPos2fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos2i(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glRasterPos2i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glRasterPos2iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glRasterPos2iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos2s(short arg0, short arg1)
{
  static void(APIENTRY*ptr)(short arg0, short arg1) = NULL;
  static const char name[] = "glRasterPos2s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glRasterPos2sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glRasterPos2sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos3d(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glRasterPos3d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glRasterPos3dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glRasterPos3dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos3f(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glRasterPos3f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glRasterPos3fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glRasterPos3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos3i(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glRasterPos3i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glRasterPos3iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glRasterPos3iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos3s(short arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glRasterPos3s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glRasterPos3sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glRasterPos3sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos4d(double arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glRasterPos4d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glRasterPos4dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glRasterPos4dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos4f(float arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glRasterPos4f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glRasterPos4fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glRasterPos4fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos4i(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glRasterPos4i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glRasterPos4iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glRasterPos4iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRasterPos4s(short arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glRasterPos4s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glRasterPos4sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glRasterPos4sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glReadBuffer(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glReadBuffer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glReadPixels(int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void* arg6)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void* arg6) = NULL;
  static const char name[] = "glReadPixels";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glRectd(double arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glRectd";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glRectdv(double const* arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(double const* arg0, double const* arg1) = NULL;
  static const char name[] = "glRectdv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glRectf(float arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glRectf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glRectfv(float const* arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(float const* arg0, float const* arg1) = NULL;
  static const char name[] = "glRectfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glRecti(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glRecti";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glRectiv(int const* arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(int const* arg0, int const* arg1) = NULL;
  static const char name[] = "glRectiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glRects(short arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glRects";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glRectsv(short const* arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(short const* arg0, short const* arg1) = NULL;
  static const char name[] = "glRectsv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

int Pure_glRenderMode(unsigned int arg0)
{
  static int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glRenderMode";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glRotated(double arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glRotated";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glRotatef(float arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glRotatef";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glScaled(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glScaled";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glScalef(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glScalef";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glScissor(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glScissor";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glSelectBuffer(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glSelectBuffer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glShadeModel(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glShadeModel";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glStencilFunc(unsigned int arg0, int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glStencilFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glStencilMask(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glStencilMask";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glStencilOp(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glStencilOp";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexCoord1d(double arg0)
{
  static void(APIENTRY*ptr)(double arg0) = NULL;
  static const char name[] = "glTexCoord1d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord1dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glTexCoord1dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord1f(float arg0)
{
  static void(APIENTRY*ptr)(float arg0) = NULL;
  static const char name[] = "glTexCoord1f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord1fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glTexCoord1fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord1i(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glTexCoord1i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord1iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glTexCoord1iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord1s(short arg0)
{
  static void(APIENTRY*ptr)(short arg0) = NULL;
  static const char name[] = "glTexCoord1s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord1sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glTexCoord1sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord2d(double arg0, double arg1)
{
  static void(APIENTRY*ptr)(double arg0, double arg1) = NULL;
  static const char name[] = "glTexCoord2d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glTexCoord2dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glTexCoord2dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord2f(float arg0, float arg1)
{
  static void(APIENTRY*ptr)(float arg0, float arg1) = NULL;
  static const char name[] = "glTexCoord2f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glTexCoord2fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glTexCoord2fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord2i(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glTexCoord2i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glTexCoord2iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glTexCoord2iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord2s(short arg0, short arg1)
{
  static void(APIENTRY*ptr)(short arg0, short arg1) = NULL;
  static const char name[] = "glTexCoord2s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glTexCoord2sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glTexCoord2sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord3d(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glTexCoord3d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexCoord3dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glTexCoord3dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord3f(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glTexCoord3f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexCoord3fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glTexCoord3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord3i(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glTexCoord3i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexCoord3iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glTexCoord3iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord3s(short arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glTexCoord3s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexCoord3sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glTexCoord3sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord4d(double arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glTexCoord4d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glTexCoord4dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glTexCoord4dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord4f(float arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glTexCoord4f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glTexCoord4fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glTexCoord4fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord4i(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glTexCoord4i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glTexCoord4iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glTexCoord4iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord4s(short arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glTexCoord4s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glTexCoord4sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glTexCoord4sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoordPointer(int arg0, unsigned int arg1, int arg2, void const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, int arg2, void const* arg3) = NULL;
  static const char name[] = "glTexCoordPointer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, int arg2, void const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glTexEnvf(unsigned int arg0, unsigned int arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float arg2) = NULL;
  static const char name[] = "glTexEnvf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexEnvfv(unsigned int arg0, unsigned int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float const* arg2) = NULL;
  static const char name[] = "glTexEnvfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexEnvi(unsigned int arg0, unsigned int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2) = NULL;
  static const char name[] = "glTexEnvi";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexEnviv(unsigned int arg0, unsigned int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int const* arg2) = NULL;
  static const char name[] = "glTexEnviv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexGend(unsigned int arg0, unsigned int arg1, double arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double arg2) = NULL;
  static const char name[] = "glTexGend";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexGendv(unsigned int arg0, unsigned int arg1, double const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double const* arg2) = NULL;
  static const char name[] = "glTexGendv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexGenf(unsigned int arg0, unsigned int arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float arg2) = NULL;
  static const char name[] = "glTexGenf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexGenfv(unsigned int arg0, unsigned int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float const* arg2) = NULL;
  static const char name[] = "glTexGenfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexGeni(unsigned int arg0, unsigned int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2) = NULL;
  static const char name[] = "glTexGeni";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexGeniv(unsigned int arg0, unsigned int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int const* arg2) = NULL;
  static const char name[] = "glTexGeniv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexImage1D(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, unsigned int arg5, unsigned int arg6, void const* arg7)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, unsigned int arg5, unsigned int arg6, void const* arg7) = NULL;
  static const char name[] = "glTexImage1D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, unsigned int arg5, unsigned int arg6, void const* arg7))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_glTexImage2D(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned int arg7, void const* arg8)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned int arg7, void const* arg8) = NULL;
  static const char name[] = "glTexImage2D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned int arg7, void const* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_glTexParameterf(unsigned int arg0, unsigned int arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float arg2) = NULL;
  static const char name[] = "glTexParameterf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexParameterfv(unsigned int arg0, unsigned int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float const* arg2) = NULL;
  static const char name[] = "glTexParameterfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexParameteri(unsigned int arg0, unsigned int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2) = NULL;
  static const char name[] = "glTexParameteri";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexParameteriv(unsigned int arg0, unsigned int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int const* arg2) = NULL;
  static const char name[] = "glTexParameteriv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexSubImage1D(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void const* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void const* arg6) = NULL;
  static const char name[] = "glTexSubImage1D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void const* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glTexSubImage2D(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned int arg7, void const* arg8)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned int arg7, void const* arg8) = NULL;
  static const char name[] = "glTexSubImage2D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned int arg7, void const* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_glTranslated(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glTranslated";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTranslatef(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glTranslatef";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertex2d(double arg0, double arg1)
{
  static void(APIENTRY*ptr)(double arg0, double arg1) = NULL;
  static const char name[] = "glVertex2d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertex2dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glVertex2dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex2f(float arg0, float arg1)
{
  static void(APIENTRY*ptr)(float arg0, float arg1) = NULL;
  static const char name[] = "glVertex2f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertex2fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glVertex2fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex2i(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glVertex2i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertex2iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glVertex2iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex2s(short arg0, short arg1)
{
  static void(APIENTRY*ptr)(short arg0, short arg1) = NULL;
  static const char name[] = "glVertex2s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertex2sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glVertex2sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex3d(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glVertex3d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertex3dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glVertex3dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex3f(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glVertex3f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertex3fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glVertex3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex3i(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glVertex3i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertex3iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glVertex3iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex3s(short arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glVertex3s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertex3sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glVertex3sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex4d(double arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glVertex4d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertex4dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glVertex4dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex4f(float arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glVertex4f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertex4fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glVertex4fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex4i(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glVertex4i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertex4iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glVertex4iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex4s(short arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glVertex4s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertex4sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glVertex4sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertexPointer(int arg0, unsigned int arg1, int arg2, void const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, int arg2, void const* arg3) = NULL;
  static const char name[] = "glVertexPointer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, int arg2, void const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glViewport(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glViewport";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glBlendColor(float arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glBlendColor";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glBlendEquation(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glBlendEquation";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDrawRangeElements(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int arg4, void const* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int arg4, void const* arg5) = NULL;
  static const char name[] = "glDrawRangeElements";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int arg4, void const* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glColorTable(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, void const* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, void const* arg5) = NULL;
  static const char name[] = "glColorTable";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, void const* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glColorTableParameterfv(unsigned int arg0, unsigned int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float const* arg2) = NULL;
  static const char name[] = "glColorTableParameterfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColorTableParameteriv(unsigned int arg0, unsigned int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int const* arg2) = NULL;
  static const char name[] = "glColorTableParameteriv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glCopyColorTable(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glCopyColorTable";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetColorTable(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3) = NULL;
  static const char name[] = "glGetColorTable";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetColorTableParameterfv(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetColorTableParameterfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetColorTableParameteriv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetColorTableParameteriv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColorSubTable(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, void const* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, void const* arg5) = NULL;
  static const char name[] = "glColorSubTable";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, void const* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glCopyColorSubTable(unsigned int arg0, int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glCopyColorSubTable";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glConvolutionFilter1D(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, void const* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, void const* arg5) = NULL;
  static const char name[] = "glConvolutionFilter1D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, void const* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glConvolutionFilter2D(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void const* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void const* arg6) = NULL;
  static const char name[] = "glConvolutionFilter2D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void const* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glConvolutionParameterf(unsigned int arg0, unsigned int arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float arg2) = NULL;
  static const char name[] = "glConvolutionParameterf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glConvolutionParameterfv(unsigned int arg0, unsigned int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float const* arg2) = NULL;
  static const char name[] = "glConvolutionParameterfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glConvolutionParameteri(unsigned int arg0, unsigned int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2) = NULL;
  static const char name[] = "glConvolutionParameteri";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glConvolutionParameteriv(unsigned int arg0, unsigned int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int const* arg2) = NULL;
  static const char name[] = "glConvolutionParameteriv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glCopyConvolutionFilter1D(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glCopyConvolutionFilter1D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glCopyConvolutionFilter2D(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5) = NULL;
  static const char name[] = "glCopyConvolutionFilter2D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glGetConvolutionFilter(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3) = NULL;
  static const char name[] = "glGetConvolutionFilter";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetConvolutionParameterfv(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetConvolutionParameterfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetConvolutionParameteriv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetConvolutionParameteriv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetSeparableFilter(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3, void* arg4, void* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3, void* arg4, void* arg5) = NULL;
  static const char name[] = "glGetSeparableFilter";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3, void* arg4, void* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glSeparableFilter2D(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void const* arg6, void const* arg7)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void const* arg6, void const* arg7) = NULL;
  static const char name[] = "glSeparableFilter2D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, void const* arg6, void const* arg7))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_glGetHistogram(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4) = NULL;
  static const char name[] = "glGetHistogram";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetHistogramParameterfv(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetHistogramParameterfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetHistogramParameteriv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetHistogramParameteriv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMinmax(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4) = NULL;
  static const char name[] = "glGetMinmax";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetMinmaxParameterfv(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetMinmaxParameterfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMinmaxParameteriv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetMinmaxParameteriv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glHistogram(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3) = NULL;
  static const char name[] = "glHistogram";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMinmax(unsigned int arg0, unsigned int arg1, unsigned char arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned char arg2) = NULL;
  static const char name[] = "glMinmax";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned char arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glResetHistogram(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glResetHistogram";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glResetMinmax(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glResetMinmax";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexImage3D(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, unsigned int arg7, unsigned int arg8, void const* arg9)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, unsigned int arg7, unsigned int arg8, void const* arg9) = NULL;
  static const char name[] = "glTexImage3D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, unsigned int arg7, unsigned int arg8, void const* arg9))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_glTexSubImage3D(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, unsigned int arg9, void const* arg10)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, unsigned int arg9, void const* arg10) = NULL;
  static const char name[] = "glTexSubImage3D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, unsigned int arg9, void const* arg10))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_glCopyTexSubImage3D(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8) = NULL;
  static const char name[] = "glCopyTexSubImage3D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_glActiveTexture(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glActiveTexture";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glClientActiveTexture(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glClientActiveTexture";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMultiTexCoord1d(unsigned int arg0, double arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1) = NULL;
  static const char name[] = "glMultiTexCoord1d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1dv(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord1dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1f(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glMultiTexCoord1f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1fv(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord1fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1i(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glMultiTexCoord1i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1iv(unsigned int arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord1iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1s(unsigned int arg0, short arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1) = NULL;
  static const char name[] = "glMultiTexCoord1s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1sv(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord1sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord2d(unsigned int arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glMultiTexCoord2d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMultiTexCoord2dv(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord2dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord2f(unsigned int arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glMultiTexCoord2f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMultiTexCoord2fv(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord2fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord2i(unsigned int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glMultiTexCoord2i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMultiTexCoord2iv(unsigned int arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord2iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord2s(unsigned int arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glMultiTexCoord2s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMultiTexCoord2sv(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord2sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord3d(unsigned int arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glMultiTexCoord3d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMultiTexCoord3dv(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord3dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord3f(unsigned int arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glMultiTexCoord3f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMultiTexCoord3fv(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord3i(unsigned int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glMultiTexCoord3i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMultiTexCoord3iv(unsigned int arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord3iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord3s(unsigned int arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glMultiTexCoord3s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMultiTexCoord3sv(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord3sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord4d(unsigned int arg0, double arg1, double arg2, double arg3, double arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4) = NULL;
  static const char name[] = "glMultiTexCoord4d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glMultiTexCoord4dv(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord4dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord4f(unsigned int arg0, float arg1, float arg2, float arg3, float arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4) = NULL;
  static const char name[] = "glMultiTexCoord4f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glMultiTexCoord4fv(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord4fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord4i(unsigned int arg0, int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glMultiTexCoord4i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glMultiTexCoord4iv(unsigned int arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord4iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord4s(unsigned int arg0, short arg1, short arg2, short arg3, short arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4) = NULL;
  static const char name[] = "glMultiTexCoord4s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glMultiTexCoord4sv(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord4sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glLoadTransposeMatrixf(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glLoadTransposeMatrixf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glLoadTransposeMatrixd(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glLoadTransposeMatrixd";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMultTransposeMatrixf(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glMultTransposeMatrixf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMultTransposeMatrixd(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glMultTransposeMatrixd";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSampleCoverage(float arg0, unsigned char arg1)
{
  static void(APIENTRY*ptr)(float arg0, unsigned char arg1) = NULL;
  static const char name[] = "glSampleCoverage";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, unsigned char arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glCompressedTexImage3D(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, void const* arg8)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, void const* arg8) = NULL;
  static const char name[] = "glCompressedTexImage3D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, void const* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_glCompressedTexImage2D(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, void const* arg7)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, void const* arg7) = NULL;
  static const char name[] = "glCompressedTexImage2D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, void const* arg7))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_glCompressedTexImage1D(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, void const* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, void const* arg6) = NULL;
  static const char name[] = "glCompressedTexImage1D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, void const* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glCompressedTexSubImage3D(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, int arg9, void const* arg10)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, int arg9, void const* arg10) = NULL;
  static const char name[] = "glCompressedTexSubImage3D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, int arg9, void const* arg10))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_glCompressedTexSubImage2D(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, int arg7, void const* arg8)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, int arg7, void const* arg8) = NULL;
  static const char name[] = "glCompressedTexSubImage2D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, int arg7, void const* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_glCompressedTexSubImage1D(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, int arg5, void const* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, int arg5, void const* arg6) = NULL;
  static const char name[] = "glCompressedTexSubImage1D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, int arg5, void const* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glGetCompressedTexImage(unsigned int arg0, int arg1, void* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, void* arg2) = NULL;
  static const char name[] = "glGetCompressedTexImage";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBlendFuncSeparate(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glBlendFuncSeparate";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glFogCoordf(float arg0)
{
  static void(APIENTRY*ptr)(float arg0) = NULL;
  static const char name[] = "glFogCoordf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glFogCoordfv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glFogCoordfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glFogCoordd(double arg0)
{
  static void(APIENTRY*ptr)(double arg0) = NULL;
  static const char name[] = "glFogCoordd";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glFogCoorddv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glFogCoorddv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glFogCoordPointer(unsigned int arg0, int arg1, void const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, void const* arg2) = NULL;
  static const char name[] = "glFogCoordPointer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, void const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMultiDrawArrays(unsigned int arg0, int* arg1, int* arg2, int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int* arg1, int* arg2, int arg3) = NULL;
  static const char name[] = "glMultiDrawArrays";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int* arg1, int* arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMultiDrawElements(unsigned int arg0, int const* arg1, unsigned int arg2, void const** arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1, unsigned int arg2, void const** arg3, int arg4) = NULL;
  static const char name[] = "glMultiDrawElements";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1, unsigned int arg2, void const** arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glPointParameterf(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glPointParameterf";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPointParameterfv(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glPointParameterfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPointParameteri(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glPointParameteri";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPointParameteriv(unsigned int arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1) = NULL;
  static const char name[] = "glPointParameteriv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glSecondaryColor3b(char arg0, char arg1, char arg2)
{
  static void(APIENTRY*ptr)(char arg0, char arg1, char arg2) = NULL;
  static const char name[] = "glSecondaryColor3b";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char arg0, char arg1, char arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3bv(char const* arg0)
{
  static void(APIENTRY*ptr)(char const* arg0) = NULL;
  static const char name[] = "glSecondaryColor3bv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3d(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glSecondaryColor3d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glSecondaryColor3dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3f(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glSecondaryColor3f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glSecondaryColor3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3i(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glSecondaryColor3i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glSecondaryColor3iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3s(short arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glSecondaryColor3s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glSecondaryColor3sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3ub(unsigned char arg0, unsigned char arg1, unsigned char arg2)
{
  static void(APIENTRY*ptr)(unsigned char arg0, unsigned char arg1, unsigned char arg2) = NULL;
  static const char name[] = "glSecondaryColor3ub";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char arg0, unsigned char arg1, unsigned char arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3ubv(unsigned char const* arg0)
{
  static void(APIENTRY*ptr)(unsigned char const* arg0) = NULL;
  static const char name[] = "glSecondaryColor3ubv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3ui(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glSecondaryColor3ui";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3uiv(unsigned int const* arg0)
{
  static void(APIENTRY*ptr)(unsigned int const* arg0) = NULL;
  static const char name[] = "glSecondaryColor3uiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3us(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2) = NULL;
  static const char name[] = "glSecondaryColor3us";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3usv(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glSecondaryColor3usv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColorPointer(int arg0, unsigned int arg1, int arg2, void const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, int arg2, void const* arg3) = NULL;
  static const char name[] = "glSecondaryColorPointer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, int arg2, void const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glWindowPos2d(double arg0, double arg1)
{
  static void(APIENTRY*ptr)(double arg0, double arg1) = NULL;
  static const char name[] = "glWindowPos2d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWindowPos2dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glWindowPos2dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos2f(float arg0, float arg1)
{
  static void(APIENTRY*ptr)(float arg0, float arg1) = NULL;
  static const char name[] = "glWindowPos2f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWindowPos2fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glWindowPos2fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos2i(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glWindowPos2i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWindowPos2iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glWindowPos2iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos2s(short arg0, short arg1)
{
  static void(APIENTRY*ptr)(short arg0, short arg1) = NULL;
  static const char name[] = "glWindowPos2s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWindowPos2sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glWindowPos2sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos3d(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glWindowPos3d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glWindowPos3dv(double const* arg0)
{
  static void(APIENTRY*ptr)(double const* arg0) = NULL;
  static const char name[] = "glWindowPos3dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos3f(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glWindowPos3f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glWindowPos3fv(float const* arg0)
{
  static void(APIENTRY*ptr)(float const* arg0) = NULL;
  static const char name[] = "glWindowPos3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos3i(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glWindowPos3i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glWindowPos3iv(int const* arg0)
{
  static void(APIENTRY*ptr)(int const* arg0) = NULL;
  static const char name[] = "glWindowPos3iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos3s(short arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glWindowPos3s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glWindowPos3sv(short const* arg0)
{
  static void(APIENTRY*ptr)(short const* arg0) = NULL;
  static const char name[] = "glWindowPos3sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glGenQueries(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenQueries";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDeleteQueries(int arg0, unsigned int const* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int const* arg1) = NULL;
  static const char name[] = "glDeleteQueries";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glIsQuery(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsQuery";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBeginQuery(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBeginQuery";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glEndQuery(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glEndQuery";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glGetQueryiv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetQueryiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetQueryObjectiv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetQueryObjectiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetQueryObjectuiv(unsigned int arg0, unsigned int arg1, unsigned int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int* arg2) = NULL;
  static const char name[] = "glGetQueryObjectuiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBindBuffer(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBindBuffer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDeleteBuffers(int arg0, unsigned int const* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int const* arg1) = NULL;
  static const char name[] = "glDeleteBuffers";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGenBuffers(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenBuffers";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glIsBuffer(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsBuffer";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBufferData(unsigned int arg0, long arg1, void const* arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, long arg1, void const* arg2, unsigned int arg3) = NULL;
  static const char name[] = "glBufferData";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, long arg1, void const* arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glBufferSubData(unsigned int arg0, long arg1, long arg2, void const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, long arg1, long arg2, void const* arg3) = NULL;
  static const char name[] = "glBufferSubData";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, long arg1, long arg2, void const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetBufferSubData(unsigned int arg0, long arg1, long arg2, void* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, long arg1, long arg2, void* arg3) = NULL;
  static const char name[] = "glGetBufferSubData";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, long arg1, long arg2, void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void* Pure_glMapBuffer(unsigned int arg0, unsigned int arg1)
{
  static void*(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glMapBuffer";
  if (!ptr) {
    ptr = (void*(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glUnmapBuffer(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glUnmapBuffer";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glGetBufferParameteriv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetBufferParameteriv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetBufferPointerv(unsigned int arg0, unsigned int arg1, void** arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, void** arg2) = NULL;
  static const char name[] = "glGetBufferPointerv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, void** arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBlendEquationSeparate(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBlendEquationSeparate";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDrawBuffers(int arg0, unsigned int const* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int const* arg1) = NULL;
  static const char name[] = "glDrawBuffers";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glStencilOpSeparate(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glStencilOpSeparate";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glStencilFuncSeparate(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glStencilFuncSeparate";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glStencilMaskSeparate(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glStencilMaskSeparate";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glAttachShader(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glAttachShader";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glBindAttribLocation(unsigned int arg0, unsigned int arg1, char const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, char const* arg2) = NULL;
  static const char name[] = "glBindAttribLocation";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, char const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glCompileShader(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glCompileShader";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

unsigned int Pure_glCreateProgram()
{
  static unsigned int(APIENTRY*ptr)() = NULL;
  static const char name[] = "glCreateProgram";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

unsigned int Pure_glCreateShader(unsigned int arg0)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glCreateShader";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDeleteProgram(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDeleteProgram";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDeleteShader(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDeleteShader";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDetachShader(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glDetachShader";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDisableVertexAttribArray(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDisableVertexAttribArray";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEnableVertexAttribArray(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glEnableVertexAttribArray";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glGetActiveAttrib(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6) = NULL;
  static const char name[] = "glGetActiveAttrib";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glGetActiveUniform(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6) = NULL;
  static const char name[] = "glGetActiveUniform";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glGetAttachedShaders(unsigned int arg0, int arg1, int* arg2, unsigned int* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int* arg2, unsigned int* arg3) = NULL;
  static const char name[] = "glGetAttachedShaders";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int* arg2, unsigned int* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

int Pure_glGetAttribLocation(unsigned int arg0, char const* arg1)
{
  static int(APIENTRY*ptr)(unsigned int arg0, char const* arg1) = NULL;
  static const char name[] = "glGetAttribLocation";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetProgramiv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetProgramiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetProgramInfoLog(unsigned int arg0, int arg1, int* arg2, char* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int* arg2, char* arg3) = NULL;
  static const char name[] = "glGetProgramInfoLog";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int* arg2, char* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetShaderiv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetShaderiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetShaderInfoLog(unsigned int arg0, int arg1, int* arg2, char* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int* arg2, char* arg3) = NULL;
  static const char name[] = "glGetShaderInfoLog";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int* arg2, char* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetShaderSource(unsigned int arg0, int arg1, int* arg2, char* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int* arg2, char* arg3) = NULL;
  static const char name[] = "glGetShaderSource";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int* arg2, char* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

int Pure_glGetUniformLocation(unsigned int arg0, char const* arg1)
{
  static int(APIENTRY*ptr)(unsigned int arg0, char const* arg1) = NULL;
  static const char name[] = "glGetUniformLocation";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetUniformfv(unsigned int arg0, int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, float* arg2) = NULL;
  static const char name[] = "glGetUniformfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetUniformiv(unsigned int arg0, int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int* arg2) = NULL;
  static const char name[] = "glGetUniformiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribdv(unsigned int arg0, unsigned int arg1, double* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double* arg2) = NULL;
  static const char name[] = "glGetVertexAttribdv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribfv(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetVertexAttribfv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribiv(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetVertexAttribiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribPointerv(unsigned int arg0, unsigned int arg1, void** arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, void** arg2) = NULL;
  static const char name[] = "glGetVertexAttribPointerv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, void** arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

unsigned char Pure_glIsProgram(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsProgram";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

unsigned char Pure_glIsShader(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsShader";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glLinkProgram(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glLinkProgram";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glShaderSource(unsigned int arg0, int arg1, char const** arg2, int const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, char const** arg2, int const* arg3) = NULL;
  static const char name[] = "glShaderSource";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, char const** arg2, int const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUseProgram(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glUseProgram";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glUniform1f(int arg0, float arg1)
{
  static void(APIENTRY*ptr)(int arg0, float arg1) = NULL;
  static const char name[] = "glUniform1f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glUniform2f(int arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(int arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glUniform2f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform3f(int arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(int arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glUniform3f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniform4f(int arg0, float arg1, float arg2, float arg3, float arg4)
{
  static void(APIENTRY*ptr)(int arg0, float arg1, float arg2, float arg3, float arg4) = NULL;
  static const char name[] = "glUniform4f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float arg1, float arg2, float arg3, float arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glUniform1i(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glUniform1i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glUniform2i(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glUniform2i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform3i(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glUniform3i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniform4i(int arg0, int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glUniform4i";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glUniform1fv(int arg0, int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, float const* arg2) = NULL;
  static const char name[] = "glUniform1fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform2fv(int arg0, int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, float const* arg2) = NULL;
  static const char name[] = "glUniform2fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform3fv(int arg0, int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, float const* arg2) = NULL;
  static const char name[] = "glUniform3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform4fv(int arg0, int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, float const* arg2) = NULL;
  static const char name[] = "glUniform4fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform1iv(int arg0, int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int const* arg2) = NULL;
  static const char name[] = "glUniform1iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform2iv(int arg0, int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int const* arg2) = NULL;
  static const char name[] = "glUniform2iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform3iv(int arg0, int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int const* arg2) = NULL;
  static const char name[] = "glUniform3iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform4iv(int arg0, int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int const* arg2) = NULL;
  static const char name[] = "glUniform4iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniformMatrix2fv(int arg0, int arg1, unsigned char arg2, float const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, float const* arg3) = NULL;
  static const char name[] = "glUniformMatrix2fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, float const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniformMatrix3fv(int arg0, int arg1, unsigned char arg2, float const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, float const* arg3) = NULL;
  static const char name[] = "glUniformMatrix3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, float const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniformMatrix4fv(int arg0, int arg1, unsigned char arg2, float const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, float const* arg3) = NULL;
  static const char name[] = "glUniformMatrix4fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, float const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glValidateProgram(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glValidateProgram";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertexAttrib1d(unsigned int arg0, double arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1) = NULL;
  static const char name[] = "glVertexAttrib1d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1dv(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glVertexAttrib1dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1f(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glVertexAttrib1f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1fv(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glVertexAttrib1fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1s(unsigned int arg0, short arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1) = NULL;
  static const char name[] = "glVertexAttrib1s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1sv(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib1sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib2d(unsigned int arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glVertexAttrib2d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttrib2dv(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glVertexAttrib2dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib2f(unsigned int arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glVertexAttrib2f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttrib2fv(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glVertexAttrib2fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib2s(unsigned int arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glVertexAttrib2s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttrib2sv(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib2sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib3d(unsigned int arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glVertexAttrib3d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttrib3dv(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glVertexAttrib3dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib3f(unsigned int arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glVertexAttrib3f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttrib3fv(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glVertexAttrib3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib3s(unsigned int arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glVertexAttrib3s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttrib3sv(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib3sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4Nbv(unsigned int arg0, char const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, char const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4Nbv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4Niv(unsigned int arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4Niv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4Nsv(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4Nsv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4Nub(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4) = NULL;
  static const char name[] = "glVertexAttrib4Nub";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4Nubv(unsigned int arg0, unsigned char const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4Nubv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4Nuiv(unsigned int arg0, unsigned int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4Nuiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4Nusv(unsigned int arg0, unsigned short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4Nusv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4bv(unsigned int arg0, char const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, char const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4bv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4d(unsigned int arg0, double arg1, double arg2, double arg3, double arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4) = NULL;
  static const char name[] = "glVertexAttrib4d";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4dv(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4dv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4f(unsigned int arg0, float arg1, float arg2, float arg3, float arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4) = NULL;
  static const char name[] = "glVertexAttrib4f";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4fv(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4iv(unsigned int arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4iv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4s(unsigned int arg0, short arg1, short arg2, short arg3, short arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4) = NULL;
  static const char name[] = "glVertexAttrib4s";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4sv(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4sv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4ubv(unsigned int arg0, unsigned char const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4ubv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4uiv(unsigned int arg0, unsigned int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4uiv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4usv(unsigned int arg0, unsigned short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4usv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribPointer(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3, int arg4, void const* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3, int arg4, void const* arg5) = NULL;
  static const char name[] = "glVertexAttribPointer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3, int arg4, void const* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glUniformMatrix2x3fv(int arg0, int arg1, unsigned char arg2, float const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, float const* arg3) = NULL;
  static const char name[] = "glUniformMatrix2x3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, float const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniformMatrix3x2fv(int arg0, int arg1, unsigned char arg2, float const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, float const* arg3) = NULL;
  static const char name[] = "glUniformMatrix3x2fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, float const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniformMatrix2x4fv(int arg0, int arg1, unsigned char arg2, float const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, float const* arg3) = NULL;
  static const char name[] = "glUniformMatrix2x4fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, float const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniformMatrix4x2fv(int arg0, int arg1, unsigned char arg2, float const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, float const* arg3) = NULL;
  static const char name[] = "glUniformMatrix4x2fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, float const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniformMatrix3x4fv(int arg0, int arg1, unsigned char arg2, float const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, float const* arg3) = NULL;
  static const char name[] = "glUniformMatrix3x4fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, float const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniformMatrix4x3fv(int arg0, int arg1, unsigned char arg2, float const* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, float const* arg3) = NULL;
  static const char name[] = "glUniformMatrix4x3fv";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, float const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}
