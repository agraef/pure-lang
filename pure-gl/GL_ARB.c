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

void Pure_glActiveTextureARB(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glActiveTextureARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glClientActiveTextureARB(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glClientActiveTextureARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMultiTexCoord1dARB(unsigned int arg0, double arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1) = NULL;
  static const char name[] = "glMultiTexCoord1dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1dvARB(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glMultiTexCoord1dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1fARB(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glMultiTexCoord1fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1fvARB(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glMultiTexCoord1fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1iARB(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glMultiTexCoord1iARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1ivARB(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glMultiTexCoord1ivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1sARB(unsigned int arg0, short arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1) = NULL;
  static const char name[] = "glMultiTexCoord1sARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1svARB(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glMultiTexCoord1svARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord2dARB(unsigned int arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glMultiTexCoord2dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMultiTexCoord2dvARB(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glMultiTexCoord2dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord2fARB(unsigned int arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glMultiTexCoord2fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMultiTexCoord2fvARB(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glMultiTexCoord2fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord2iARB(unsigned int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glMultiTexCoord2iARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMultiTexCoord2ivARB(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glMultiTexCoord2ivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord2sARB(unsigned int arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glMultiTexCoord2sARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMultiTexCoord2svARB(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glMultiTexCoord2svARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord3dARB(unsigned int arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glMultiTexCoord3dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMultiTexCoord3dvARB(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glMultiTexCoord3dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord3fARB(unsigned int arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glMultiTexCoord3fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMultiTexCoord3fvARB(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glMultiTexCoord3fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord3iARB(unsigned int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glMultiTexCoord3iARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMultiTexCoord3ivARB(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glMultiTexCoord3ivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord3sARB(unsigned int arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glMultiTexCoord3sARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMultiTexCoord3svARB(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glMultiTexCoord3svARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord4dARB(unsigned int arg0, double arg1, double arg2, double arg3, double arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4) = NULL;
  static const char name[] = "glMultiTexCoord4dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glMultiTexCoord4dvARB(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glMultiTexCoord4dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord4fARB(unsigned int arg0, float arg1, float arg2, float arg3, float arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4) = NULL;
  static const char name[] = "glMultiTexCoord4fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glMultiTexCoord4fvARB(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glMultiTexCoord4fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord4iARB(unsigned int arg0, int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glMultiTexCoord4iARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glMultiTexCoord4ivARB(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glMultiTexCoord4ivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord4sARB(unsigned int arg0, short arg1, short arg2, short arg3, short arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4) = NULL;
  static const char name[] = "glMultiTexCoord4sARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glMultiTexCoord4svARB(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glMultiTexCoord4svARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glLoadTransposeMatrixfARB(const float* arg0)
{
  static void(APIENTRY*ptr)(const float* arg0) = NULL;
  static const char name[] = "glLoadTransposeMatrixfARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const float* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glLoadTransposeMatrixdARB(const double* arg0)
{
  static void(APIENTRY*ptr)(const double* arg0) = NULL;
  static const char name[] = "glLoadTransposeMatrixdARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const double* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMultTransposeMatrixfARB(const float* arg0)
{
  static void(APIENTRY*ptr)(const float* arg0) = NULL;
  static const char name[] = "glMultTransposeMatrixfARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const float* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMultTransposeMatrixdARB(const double* arg0)
{
  static void(APIENTRY*ptr)(const double* arg0) = NULL;
  static const char name[] = "glMultTransposeMatrixdARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const double* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSampleCoverageARB(float arg0, unsigned char arg1)
{
  static void(APIENTRY*ptr)(float arg0, unsigned char arg1) = NULL;
  static const char name[] = "glSampleCoverageARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, unsigned char arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glCompressedTexImage3DARB(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, const void* arg8)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, const void* arg8) = NULL;
  static const char name[] = "glCompressedTexImage3DARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, const void* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_glCompressedTexImage2DARB(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, const void* arg7)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, const void* arg7) = NULL;
  static const char name[] = "glCompressedTexImage2DARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, const void* arg7))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_glCompressedTexImage1DARB(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, const void* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, const void* arg6) = NULL;
  static const char name[] = "glCompressedTexImage1DARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, const void* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glCompressedTexSubImage3DARB(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, int arg9, const void* arg10)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, int arg9, const void* arg10) = NULL;
  static const char name[] = "glCompressedTexSubImage3DARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, int arg9, const void* arg10))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_glCompressedTexSubImage2DARB(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, int arg7, const void* arg8)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, int arg7, const void* arg8) = NULL;
  static const char name[] = "glCompressedTexSubImage2DARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, int arg7, const void* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_glCompressedTexSubImage1DARB(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, int arg5, const void* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, int arg5, const void* arg6) = NULL;
  static const char name[] = "glCompressedTexSubImage1DARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, int arg5, const void* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glGetCompressedTexImageARB(unsigned int arg0, int arg1, void* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, void* arg2) = NULL;
  static const char name[] = "glGetCompressedTexImageARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glPointParameterfARB(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glPointParameterfARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPointParameterfvARB(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glPointParameterfvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWeightbvARB(int arg0, const char* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const char* arg1) = NULL;
  static const char name[] = "glWeightbvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWeightsvARB(int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const short* arg1) = NULL;
  static const char name[] = "glWeightsvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWeightivARB(int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const int* arg1) = NULL;
  static const char name[] = "glWeightivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWeightfvARB(int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const float* arg1) = NULL;
  static const char name[] = "glWeightfvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWeightdvARB(int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const double* arg1) = NULL;
  static const char name[] = "glWeightdvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWeightubvARB(int arg0, const unsigned char* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned char* arg1) = NULL;
  static const char name[] = "glWeightubvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWeightusvARB(int arg0, const unsigned short* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned short* arg1) = NULL;
  static const char name[] = "glWeightusvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWeightuivARB(int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glWeightuivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWeightPointerARB(int arg0, unsigned int arg1, int arg2, const void* arg3)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, int arg2, const void* arg3) = NULL;
  static const char name[] = "glWeightPointerARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, int arg2, const void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexBlendARB(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glVertexBlendARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glCurrentPaletteMatrixARB(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glCurrentPaletteMatrixARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMatrixIndexubvARB(int arg0, const unsigned char* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned char* arg1) = NULL;
  static const char name[] = "glMatrixIndexubvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMatrixIndexusvARB(int arg0, const unsigned short* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned short* arg1) = NULL;
  static const char name[] = "glMatrixIndexusvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMatrixIndexuivARB(int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glMatrixIndexuivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMatrixIndexPointerARB(int arg0, unsigned int arg1, int arg2, const void* arg3)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, int arg2, const void* arg3) = NULL;
  static const char name[] = "glMatrixIndexPointerARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, int arg2, const void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glWindowPos2dARB(double arg0, double arg1)
{
  static void(APIENTRY*ptr)(double arg0, double arg1) = NULL;
  static const char name[] = "glWindowPos2dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWindowPos2dvARB(const double* arg0)
{
  static void(APIENTRY*ptr)(const double* arg0) = NULL;
  static const char name[] = "glWindowPos2dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const double* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos2fARB(float arg0, float arg1)
{
  static void(APIENTRY*ptr)(float arg0, float arg1) = NULL;
  static const char name[] = "glWindowPos2fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWindowPos2fvARB(const float* arg0)
{
  static void(APIENTRY*ptr)(const float* arg0) = NULL;
  static const char name[] = "glWindowPos2fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const float* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos2iARB(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glWindowPos2iARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWindowPos2ivARB(const int* arg0)
{
  static void(APIENTRY*ptr)(const int* arg0) = NULL;
  static const char name[] = "glWindowPos2ivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const int* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos2sARB(short arg0, short arg1)
{
  static void(APIENTRY*ptr)(short arg0, short arg1) = NULL;
  static const char name[] = "glWindowPos2sARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glWindowPos2svARB(const short* arg0)
{
  static void(APIENTRY*ptr)(const short* arg0) = NULL;
  static const char name[] = "glWindowPos2svARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const short* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos3dARB(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glWindowPos3dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glWindowPos3dvARB(const double* arg0)
{
  static void(APIENTRY*ptr)(const double* arg0) = NULL;
  static const char name[] = "glWindowPos3dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const double* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos3fARB(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glWindowPos3fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glWindowPos3fvARB(const float* arg0)
{
  static void(APIENTRY*ptr)(const float* arg0) = NULL;
  static const char name[] = "glWindowPos3fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const float* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos3iARB(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glWindowPos3iARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glWindowPos3ivARB(const int* arg0)
{
  static void(APIENTRY*ptr)(const int* arg0) = NULL;
  static const char name[] = "glWindowPos3ivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const int* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glWindowPos3sARB(short arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glWindowPos3sARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glWindowPos3svARB(const short* arg0)
{
  static void(APIENTRY*ptr)(const short* arg0) = NULL;
  static const char name[] = "glWindowPos3svARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const short* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertexAttrib1dARB(unsigned int arg0, double arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1) = NULL;
  static const char name[] = "glVertexAttrib1dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1dvARB(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glVertexAttrib1dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1fARB(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glVertexAttrib1fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1fvARB(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glVertexAttrib1fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1sARB(unsigned int arg0, short arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1) = NULL;
  static const char name[] = "glVertexAttrib1sARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1svARB(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glVertexAttrib1svARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib2dARB(unsigned int arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glVertexAttrib2dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttrib2dvARB(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glVertexAttrib2dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib2fARB(unsigned int arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glVertexAttrib2fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttrib2fvARB(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glVertexAttrib2fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib2sARB(unsigned int arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glVertexAttrib2sARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttrib2svARB(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glVertexAttrib2svARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib3dARB(unsigned int arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glVertexAttrib3dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttrib3dvARB(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glVertexAttrib3dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib3fARB(unsigned int arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glVertexAttrib3fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttrib3fvARB(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glVertexAttrib3fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib3sARB(unsigned int arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glVertexAttrib3sARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttrib3svARB(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glVertexAttrib3svARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4NbvARB(unsigned int arg0, const char* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const char* arg1) = NULL;
  static const char name[] = "glVertexAttrib4NbvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4NivARB(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glVertexAttrib4NivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4NsvARB(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glVertexAttrib4NsvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4NubARB(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4) = NULL;
  static const char name[] = "glVertexAttrib4NubARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4NubvARB(unsigned int arg0, const unsigned char* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned char* arg1) = NULL;
  static const char name[] = "glVertexAttrib4NubvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4NuivARB(unsigned int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glVertexAttrib4NuivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4NusvARB(unsigned int arg0, const unsigned short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned short* arg1) = NULL;
  static const char name[] = "glVertexAttrib4NusvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4bvARB(unsigned int arg0, const char* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const char* arg1) = NULL;
  static const char name[] = "glVertexAttrib4bvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4dARB(unsigned int arg0, double arg1, double arg2, double arg3, double arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4) = NULL;
  static const char name[] = "glVertexAttrib4dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4dvARB(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glVertexAttrib4dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4fARB(unsigned int arg0, float arg1, float arg2, float arg3, float arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4) = NULL;
  static const char name[] = "glVertexAttrib4fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4fvARB(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glVertexAttrib4fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4ivARB(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glVertexAttrib4ivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4sARB(unsigned int arg0, short arg1, short arg2, short arg3, short arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4) = NULL;
  static const char name[] = "glVertexAttrib4sARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4svARB(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glVertexAttrib4svARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4ubvARB(unsigned int arg0, const unsigned char* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned char* arg1) = NULL;
  static const char name[] = "glVertexAttrib4ubvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4uivARB(unsigned int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glVertexAttrib4uivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4usvARB(unsigned int arg0, const unsigned short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned short* arg1) = NULL;
  static const char name[] = "glVertexAttrib4usvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribPointerARB(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3, int arg4, const void* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3, int arg4, const void* arg5) = NULL;
  static const char name[] = "glVertexAttribPointerARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3, int arg4, const void* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glEnableVertexAttribArrayARB(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glEnableVertexAttribArrayARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDisableVertexAttribArrayARB(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDisableVertexAttribArrayARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glProgramStringARB(unsigned int arg0, unsigned int arg1, int arg2, const void* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, const void* arg3) = NULL;
  static const char name[] = "glProgramStringARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, const void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glBindProgramARB(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBindProgramARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDeleteProgramsARB(int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glDeleteProgramsARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGenProgramsARB(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenProgramsARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glProgramEnvParameter4dARB(unsigned int arg0, unsigned int arg1, double arg2, double arg3, double arg4, double arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double arg2, double arg3, double arg4, double arg5) = NULL;
  static const char name[] = "glProgramEnvParameter4dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double arg2, double arg3, double arg4, double arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glProgramEnvParameter4dvARB(unsigned int arg0, unsigned int arg1, const double* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const double* arg2) = NULL;
  static const char name[] = "glProgramEnvParameter4dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const double* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glProgramEnvParameter4fARB(unsigned int arg0, unsigned int arg1, float arg2, float arg3, float arg4, float arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float arg2, float arg3, float arg4, float arg5) = NULL;
  static const char name[] = "glProgramEnvParameter4fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float arg2, float arg3, float arg4, float arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glProgramEnvParameter4fvARB(unsigned int arg0, unsigned int arg1, const float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const float* arg2) = NULL;
  static const char name[] = "glProgramEnvParameter4fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glProgramLocalParameter4dARB(unsigned int arg0, unsigned int arg1, double arg2, double arg3, double arg4, double arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double arg2, double arg3, double arg4, double arg5) = NULL;
  static const char name[] = "glProgramLocalParameter4dARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double arg2, double arg3, double arg4, double arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glProgramLocalParameter4dvARB(unsigned int arg0, unsigned int arg1, const double* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const double* arg2) = NULL;
  static const char name[] = "glProgramLocalParameter4dvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const double* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glProgramLocalParameter4fARB(unsigned int arg0, unsigned int arg1, float arg2, float arg3, float arg4, float arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float arg2, float arg3, float arg4, float arg5) = NULL;
  static const char name[] = "glProgramLocalParameter4fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float arg2, float arg3, float arg4, float arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glProgramLocalParameter4fvARB(unsigned int arg0, unsigned int arg1, const float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const float* arg2) = NULL;
  static const char name[] = "glProgramLocalParameter4fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetProgramEnvParameterdvARB(unsigned int arg0, unsigned int arg1, double* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double* arg2) = NULL;
  static const char name[] = "glGetProgramEnvParameterdvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetProgramEnvParameterfvARB(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetProgramEnvParameterfvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetProgramLocalParameterdvARB(unsigned int arg0, unsigned int arg1, double* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double* arg2) = NULL;
  static const char name[] = "glGetProgramLocalParameterdvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetProgramLocalParameterfvARB(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetProgramLocalParameterfvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetProgramivARB(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetProgramivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetProgramStringARB(unsigned int arg0, unsigned int arg1, void* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, void* arg2) = NULL;
  static const char name[] = "glGetProgramStringARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribdvARB(unsigned int arg0, unsigned int arg1, double* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double* arg2) = NULL;
  static const char name[] = "glGetVertexAttribdvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribfvARB(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetVertexAttribfvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribivARB(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetVertexAttribivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribPointervARB(unsigned int arg0, unsigned int arg1, void** arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, void** arg2) = NULL;
  static const char name[] = "glGetVertexAttribPointervARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, void** arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

unsigned char Pure_glIsProgramARB(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsProgramARB";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBindBufferARB(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBindBufferARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDeleteBuffersARB(int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glDeleteBuffersARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGenBuffersARB(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenBuffersARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glIsBufferARB(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsBufferARB";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBufferDataARB(unsigned int arg0, long arg1, const void* arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, long arg1, const void* arg2, unsigned int arg3) = NULL;
  static const char name[] = "glBufferDataARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, long arg1, const void* arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glBufferSubDataARB(unsigned int arg0, long arg1, long arg2, const void* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, long arg1, long arg2, const void* arg3) = NULL;
  static const char name[] = "glBufferSubDataARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, long arg1, long arg2, const void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetBufferSubDataARB(unsigned int arg0, long arg1, long arg2, void* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, long arg1, long arg2, void* arg3) = NULL;
  static const char name[] = "glGetBufferSubDataARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, long arg1, long arg2, void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void* Pure_glMapBufferARB(unsigned int arg0, unsigned int arg1)
{
  static void*(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glMapBufferARB";
  if (!ptr) {
    ptr = (void*(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glUnmapBufferARB(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glUnmapBufferARB";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glGetBufferParameterivARB(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetBufferParameterivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetBufferPointervARB(unsigned int arg0, unsigned int arg1, void** arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, void** arg2) = NULL;
  static const char name[] = "glGetBufferPointervARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, void** arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGenQueriesARB(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenQueriesARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDeleteQueriesARB(int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glDeleteQueriesARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glIsQueryARB(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsQueryARB";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBeginQueryARB(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBeginQueryARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glEndQueryARB(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glEndQueryARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glGetQueryivARB(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetQueryivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetQueryObjectivARB(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetQueryObjectivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetQueryObjectuivARB(unsigned int arg0, unsigned int arg1, unsigned int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int* arg2) = NULL;
  static const char name[] = "glGetQueryObjectuivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glDeleteObjectARB(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDeleteObjectARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

unsigned int Pure_glGetHandleARB(unsigned int arg0)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glGetHandleARB";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDetachObjectARB(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glDetachObjectARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned int Pure_glCreateShaderObjectARB(unsigned int arg0)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glCreateShaderObjectARB";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glShaderSourceARB(unsigned int arg0, int arg1, const char** arg2, const int* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, const char** arg2, const int* arg3) = NULL;
  static const char name[] = "glShaderSourceARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, const char** arg2, const int* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glCompileShaderARB(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glCompileShaderARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

unsigned int Pure_glCreateProgramObjectARB()
{
  static unsigned int(APIENTRY*ptr)() = NULL;
  static const char name[] = "glCreateProgramObjectARB";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glAttachObjectARB(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glAttachObjectARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glLinkProgramARB(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glLinkProgramARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glUseProgramObjectARB(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glUseProgramObjectARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glValidateProgramARB(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glValidateProgramARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glUniform1fARB(int arg0, float arg1)
{
  static void(APIENTRY*ptr)(int arg0, float arg1) = NULL;
  static const char name[] = "glUniform1fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glUniform2fARB(int arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(int arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glUniform2fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform3fARB(int arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(int arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glUniform3fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniform4fARB(int arg0, float arg1, float arg2, float arg3, float arg4)
{
  static void(APIENTRY*ptr)(int arg0, float arg1, float arg2, float arg3, float arg4) = NULL;
  static const char name[] = "glUniform4fARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float arg1, float arg2, float arg3, float arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glUniform1iARB(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glUniform1iARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glUniform2iARB(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glUniform2iARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform3iARB(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glUniform3iARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniform4iARB(int arg0, int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glUniform4iARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glUniform1fvARB(int arg0, int arg1, const float* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const float* arg2) = NULL;
  static const char name[] = "glUniform1fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform2fvARB(int arg0, int arg1, const float* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const float* arg2) = NULL;
  static const char name[] = "glUniform2fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform3fvARB(int arg0, int arg1, const float* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const float* arg2) = NULL;
  static const char name[] = "glUniform3fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform4fvARB(int arg0, int arg1, const float* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const float* arg2) = NULL;
  static const char name[] = "glUniform4fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform1ivARB(int arg0, int arg1, const int* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const int* arg2) = NULL;
  static const char name[] = "glUniform1ivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform2ivARB(int arg0, int arg1, const int* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const int* arg2) = NULL;
  static const char name[] = "glUniform2ivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform3ivARB(int arg0, int arg1, const int* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const int* arg2) = NULL;
  static const char name[] = "glUniform3ivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform4ivARB(int arg0, int arg1, const int* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const int* arg2) = NULL;
  static const char name[] = "glUniform4ivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniformMatrix2fvARB(int arg0, int arg1, unsigned char arg2, const float* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, const float* arg3) = NULL;
  static const char name[] = "glUniformMatrix2fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, const float* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniformMatrix3fvARB(int arg0, int arg1, unsigned char arg2, const float* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, const float* arg3) = NULL;
  static const char name[] = "glUniformMatrix3fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, const float* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniformMatrix4fvARB(int arg0, int arg1, unsigned char arg2, const float* arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, unsigned char arg2, const float* arg3) = NULL;
  static const char name[] = "glUniformMatrix4fvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, unsigned char arg2, const float* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetObjectParameterfvARB(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetObjectParameterfvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetObjectParameterivARB(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetObjectParameterivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetInfoLogARB(unsigned int arg0, int arg1, int* arg2, char* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int* arg2, char* arg3) = NULL;
  static const char name[] = "glGetInfoLogARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int* arg2, char* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetAttachedObjectsARB(unsigned int arg0, int arg1, int* arg2, unsigned int* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int* arg2, unsigned int* arg3) = NULL;
  static const char name[] = "glGetAttachedObjectsARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int* arg2, unsigned int* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

int Pure_glGetUniformLocationARB(unsigned int arg0, const char* arg1)
{
  static int(APIENTRY*ptr)(unsigned int arg0, const char* arg1) = NULL;
  static const char name[] = "glGetUniformLocationARB";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, const char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetActiveUniformARB(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6) = NULL;
  static const char name[] = "glGetActiveUniformARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glGetUniformfvARB(unsigned int arg0, int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, float* arg2) = NULL;
  static const char name[] = "glGetUniformfvARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetUniformivARB(unsigned int arg0, int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int* arg2) = NULL;
  static const char name[] = "glGetUniformivARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetShaderSourceARB(unsigned int arg0, int arg1, int* arg2, char* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int* arg2, char* arg3) = NULL;
  static const char name[] = "glGetShaderSourceARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int* arg2, char* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glBindAttribLocationARB(unsigned int arg0, unsigned int arg1, const char* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const char* arg2) = NULL;
  static const char name[] = "glBindAttribLocationARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const char* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetActiveAttribARB(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6) = NULL;
  static const char name[] = "glGetActiveAttribARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_glGetAttribLocationARB(unsigned int arg0, const char* arg1)
{
  static int(APIENTRY*ptr)(unsigned int arg0, const char* arg1) = NULL;
  static const char name[] = "glGetAttribLocationARB";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, const char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDrawBuffersARB(int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glDrawBuffersARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glClampColorARB(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glClampColorARB";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}
