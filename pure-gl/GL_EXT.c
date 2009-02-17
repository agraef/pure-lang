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

void Pure_glBlendColorEXT(float arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glBlendColorEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glPolygonOffsetEXT(float arg0, float arg1)
{
  static void(APIENTRY*ptr)(float arg0, float arg1) = NULL;
  static const char name[] = "glPolygonOffsetEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glTexImage3DEXT(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, unsigned int arg7, unsigned int arg8, const void* arg9)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, unsigned int arg7, unsigned int arg8, const void* arg9) = NULL;
  static const char name[] = "glTexImage3DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, unsigned int arg7, unsigned int arg8, const void* arg9))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_glTexSubImage3DEXT(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, unsigned int arg9, const void* arg10)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, unsigned int arg9, const void* arg10) = NULL;
  static const char name[] = "glTexSubImage3DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, unsigned int arg9, const void* arg10))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_glTexSubImage1DEXT(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6) = NULL;
  static const char name[] = "glTexSubImage1DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glTexSubImage2DEXT(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned int arg7, const void* arg8)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned int arg7, const void* arg8) = NULL;
  static const char name[] = "glTexSubImage2DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned int arg7, const void* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_glCopyTexImage1DEXT(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6) = NULL;
  static const char name[] = "glCopyTexImage1DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glCopyTexImage2DEXT(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7) = NULL;
  static const char name[] = "glCopyTexImage2DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, int arg7))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_glCopyTexSubImage1DEXT(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) = NULL;
  static const char name[] = "glCopyTexSubImage1DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glCopyTexSubImage2DEXT(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7) = NULL;
  static const char name[] = "glCopyTexSubImage2DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_glCopyTexSubImage3DEXT(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8) = NULL;
  static const char name[] = "glCopyTexSubImage3DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_glGetHistogramEXT(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4) = NULL;
  static const char name[] = "glGetHistogramEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetHistogramParameterfvEXT(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetHistogramParameterfvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetHistogramParameterivEXT(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetHistogramParameterivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMinmaxEXT(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4) = NULL;
  static const char name[] = "glGetMinmaxEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char arg1, unsigned int arg2, unsigned int arg3, void* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetMinmaxParameterfvEXT(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetMinmaxParameterfvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMinmaxParameterivEXT(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetMinmaxParameterivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glHistogramEXT(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3) = NULL;
  static const char name[] = "glHistogramEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMinmaxEXT(unsigned int arg0, unsigned int arg1, unsigned char arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned char arg2) = NULL;
  static const char name[] = "glMinmaxEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned char arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glResetHistogramEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glResetHistogramEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glResetMinmaxEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glResetMinmaxEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glConvolutionFilter1DEXT(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5) = NULL;
  static const char name[] = "glConvolutionFilter1DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glConvolutionFilter2DEXT(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6) = NULL;
  static const char name[] = "glConvolutionFilter2DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glConvolutionParameterfEXT(unsigned int arg0, unsigned int arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float arg2) = NULL;
  static const char name[] = "glConvolutionParameterfEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glConvolutionParameterfvEXT(unsigned int arg0, unsigned int arg1, const float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const float* arg2) = NULL;
  static const char name[] = "glConvolutionParameterfvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glConvolutionParameteriEXT(unsigned int arg0, unsigned int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2) = NULL;
  static const char name[] = "glConvolutionParameteriEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glConvolutionParameterivEXT(unsigned int arg0, unsigned int arg1, const int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const int* arg2) = NULL;
  static const char name[] = "glConvolutionParameterivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glCopyConvolutionFilter1DEXT(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glCopyConvolutionFilter1DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glCopyConvolutionFilter2DEXT(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5) = NULL;
  static const char name[] = "glCopyConvolutionFilter2DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glGetConvolutionFilterEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3) = NULL;
  static const char name[] = "glGetConvolutionFilterEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetConvolutionParameterfvEXT(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetConvolutionParameterfvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetConvolutionParameterivEXT(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetConvolutionParameterivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetSeparableFilterEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3, void* arg4, void* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3, void* arg4, void* arg5) = NULL;
  static const char name[] = "glGetSeparableFilterEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3, void* arg4, void* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glSeparableFilter2DEXT(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6, const void* arg7)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6, const void* arg7) = NULL;
  static const char name[] = "glSeparableFilter2DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6, const void* arg7))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

unsigned char Pure_glAreTexturesResidentEXT(int arg0, const unsigned int* arg1, unsigned char* arg2)
{
  static unsigned char(APIENTRY*ptr)(int arg0, const unsigned int* arg1, unsigned char* arg2) = NULL;
  static const char name[] = "glAreTexturesResidentEXT";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(int arg0, const unsigned int* arg1, unsigned char* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBindTextureEXT(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBindTextureEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDeleteTexturesEXT(int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glDeleteTexturesEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGenTexturesEXT(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenTexturesEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glIsTextureEXT(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsTextureEXT";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glPrioritizeTexturesEXT(int arg0, const unsigned int* arg1, const float* arg2)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned int* arg1, const float* arg2) = NULL;
  static const char name[] = "glPrioritizeTexturesEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned int* arg1, const float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glArrayElementEXT(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glArrayElementEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColorPointerEXT(int arg0, unsigned int arg1, int arg2, int arg3, const void* arg4)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, int arg2, int arg3, const void* arg4) = NULL;
  static const char name[] = "glColorPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, int arg2, int arg3, const void* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glDrawArraysEXT(unsigned int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glDrawArraysEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glEdgeFlagPointerEXT(int arg0, int arg1, const unsigned char* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const unsigned char* arg2) = NULL;
  static const char name[] = "glEdgeFlagPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const unsigned char* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetPointervEXT(unsigned int arg0, void** arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, void** arg1) = NULL;
  static const char name[] = "glGetPointervEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, void** arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glIndexPointerEXT(unsigned int arg0, int arg1, int arg2, const void* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, const void* arg3) = NULL;
  static const char name[] = "glIndexPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, const void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glNormalPointerEXT(unsigned int arg0, int arg1, int arg2, const void* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, const void* arg3) = NULL;
  static const char name[] = "glNormalPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, const void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glTexCoordPointerEXT(int arg0, unsigned int arg1, int arg2, int arg3, const void* arg4)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, int arg2, int arg3, const void* arg4) = NULL;
  static const char name[] = "glTexCoordPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, int arg2, int arg3, const void* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexPointerEXT(int arg0, unsigned int arg1, int arg2, int arg3, const void* arg4)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, int arg2, int arg3, const void* arg4) = NULL;
  static const char name[] = "glVertexPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, int arg2, int arg3, const void* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glBlendEquationEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glBlendEquationEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glPointParameterfEXT(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glPointParameterfEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPointParameterfvEXT(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glPointParameterfvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glColorSubTableEXT(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5) = NULL;
  static const char name[] = "glColorSubTableEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glCopyColorSubTableEXT(unsigned int arg0, int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glCopyColorSubTableEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glColorTableEXT(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5) = NULL;
  static const char name[] = "glColorTableEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glGetColorTableEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3) = NULL;
  static const char name[] = "glGetColorTableEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetColorTableParameterivEXT(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetColorTableParameterivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetColorTableParameterfvEXT(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetColorTableParameterfvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glIndexMaterialEXT(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glIndexMaterialEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glIndexFuncEXT(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glIndexFuncEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glLockArraysEXT(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glLockArraysEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glUnlockArraysEXT()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glUnlockArraysEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glCullParameterdvEXT(unsigned int arg0, double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double* arg1) = NULL;
  static const char name[] = "glCullParameterdvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glCullParameterfvEXT(unsigned int arg0, float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float* arg1) = NULL;
  static const char name[] = "glCullParameterfvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDrawRangeElementsEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int arg4, const void* arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int arg4, const void* arg5) = NULL;
  static const char name[] = "glDrawRangeElementsEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int arg4, const void* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glApplyTextureEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glApplyTextureEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTextureLightEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glTextureLightEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTextureMaterialEXT(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glTextureMaterialEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPixelTransformParameteriEXT(unsigned int arg0, unsigned int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2) = NULL;
  static const char name[] = "glPixelTransformParameteriEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glPixelTransformParameterfEXT(unsigned int arg0, unsigned int arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float arg2) = NULL;
  static const char name[] = "glPixelTransformParameterfEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glPixelTransformParameterivEXT(unsigned int arg0, unsigned int arg1, const int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const int* arg2) = NULL;
  static const char name[] = "glPixelTransformParameterivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glPixelTransformParameterfvEXT(unsigned int arg0, unsigned int arg1, const float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const float* arg2) = NULL;
  static const char name[] = "glPixelTransformParameterfvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3bEXT(char arg0, char arg1, char arg2)
{
  static void(APIENTRY*ptr)(char arg0, char arg1, char arg2) = NULL;
  static const char name[] = "glSecondaryColor3bEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char arg0, char arg1, char arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3bvEXT(const char* arg0)
{
  static void(APIENTRY*ptr)(const char* arg0) = NULL;
  static const char name[] = "glSecondaryColor3bvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const char* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3dEXT(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glSecondaryColor3dEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3dvEXT(const double* arg0)
{
  static void(APIENTRY*ptr)(const double* arg0) = NULL;
  static const char name[] = "glSecondaryColor3dvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const double* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3fEXT(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glSecondaryColor3fEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3fvEXT(const float* arg0)
{
  static void(APIENTRY*ptr)(const float* arg0) = NULL;
  static const char name[] = "glSecondaryColor3fvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const float* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3iEXT(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glSecondaryColor3iEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3ivEXT(const int* arg0)
{
  static void(APIENTRY*ptr)(const int* arg0) = NULL;
  static const char name[] = "glSecondaryColor3ivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const int* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3sEXT(short arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glSecondaryColor3sEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3svEXT(const short* arg0)
{
  static void(APIENTRY*ptr)(const short* arg0) = NULL;
  static const char name[] = "glSecondaryColor3svEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const short* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3ubEXT(unsigned char arg0, unsigned char arg1, unsigned char arg2)
{
  static void(APIENTRY*ptr)(unsigned char arg0, unsigned char arg1, unsigned char arg2) = NULL;
  static const char name[] = "glSecondaryColor3ubEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned char arg0, unsigned char arg1, unsigned char arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3ubvEXT(const unsigned char* arg0)
{
  static void(APIENTRY*ptr)(const unsigned char* arg0) = NULL;
  static const char name[] = "glSecondaryColor3ubvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const unsigned char* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3uiEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glSecondaryColor3uiEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3uivEXT(const unsigned int* arg0)
{
  static void(APIENTRY*ptr)(const unsigned int* arg0) = NULL;
  static const char name[] = "glSecondaryColor3uivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const unsigned int* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3usEXT(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2) = NULL;
  static const char name[] = "glSecondaryColor3usEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3usvEXT(const unsigned short* arg0)
{
  static void(APIENTRY*ptr)(const unsigned short* arg0) = NULL;
  static const char name[] = "glSecondaryColor3usvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const unsigned short* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColorPointerEXT(int arg0, unsigned int arg1, int arg2, const void* arg3)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, int arg2, const void* arg3) = NULL;
  static const char name[] = "glSecondaryColorPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, int arg2, const void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glTextureNormalEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glTextureNormalEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMultiDrawArraysEXT(unsigned int arg0, int* arg1, int* arg2, int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int* arg1, int* arg2, int arg3) = NULL;
  static const char name[] = "glMultiDrawArraysEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int* arg1, int* arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMultiDrawElementsEXT(unsigned int arg0, const int* arg1, unsigned int arg2, const void** arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1, unsigned int arg2, const void** arg3, int arg4) = NULL;
  static const char name[] = "glMultiDrawElementsEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1, unsigned int arg2, const void** arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glFogCoordfEXT(float arg0)
{
  static void(APIENTRY*ptr)(float arg0) = NULL;
  static const char name[] = "glFogCoordfEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glFogCoordfvEXT(const float* arg0)
{
  static void(APIENTRY*ptr)(const float* arg0) = NULL;
  static const char name[] = "glFogCoordfvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const float* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glFogCoorddEXT(double arg0)
{
  static void(APIENTRY*ptr)(double arg0) = NULL;
  static const char name[] = "glFogCoorddEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glFogCoorddvEXT(const double* arg0)
{
  static void(APIENTRY*ptr)(const double* arg0) = NULL;
  static const char name[] = "glFogCoorddvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const double* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glFogCoordPointerEXT(unsigned int arg0, int arg1, const void* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, const void* arg2) = NULL;
  static const char name[] = "glFogCoordPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, const void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTangent3bEXT(char arg0, char arg1, char arg2)
{
  static void(APIENTRY*ptr)(char arg0, char arg1, char arg2) = NULL;
  static const char name[] = "glTangent3bEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char arg0, char arg1, char arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTangent3bvEXT(const char* arg0)
{
  static void(APIENTRY*ptr)(const char* arg0) = NULL;
  static const char name[] = "glTangent3bvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const char* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTangent3dEXT(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glTangent3dEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTangent3dvEXT(const double* arg0)
{
  static void(APIENTRY*ptr)(const double* arg0) = NULL;
  static const char name[] = "glTangent3dvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const double* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTangent3fEXT(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glTangent3fEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTangent3fvEXT(const float* arg0)
{
  static void(APIENTRY*ptr)(const float* arg0) = NULL;
  static const char name[] = "glTangent3fvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const float* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTangent3iEXT(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glTangent3iEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTangent3ivEXT(const int* arg0)
{
  static void(APIENTRY*ptr)(const int* arg0) = NULL;
  static const char name[] = "glTangent3ivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const int* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTangent3sEXT(short arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glTangent3sEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTangent3svEXT(const short* arg0)
{
  static void(APIENTRY*ptr)(const short* arg0) = NULL;
  static const char name[] = "glTangent3svEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const short* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBinormal3bEXT(char arg0, char arg1, char arg2)
{
  static void(APIENTRY*ptr)(char arg0, char arg1, char arg2) = NULL;
  static const char name[] = "glBinormal3bEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char arg0, char arg1, char arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBinormal3bvEXT(const char* arg0)
{
  static void(APIENTRY*ptr)(const char* arg0) = NULL;
  static const char name[] = "glBinormal3bvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const char* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBinormal3dEXT(double arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glBinormal3dEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBinormal3dvEXT(const double* arg0)
{
  static void(APIENTRY*ptr)(const double* arg0) = NULL;
  static const char name[] = "glBinormal3dvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const double* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBinormal3fEXT(float arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(float arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glBinormal3fEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBinormal3fvEXT(const float* arg0)
{
  static void(APIENTRY*ptr)(const float* arg0) = NULL;
  static const char name[] = "glBinormal3fvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const float* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBinormal3iEXT(int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glBinormal3iEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBinormal3ivEXT(const int* arg0)
{
  static void(APIENTRY*ptr)(const int* arg0) = NULL;
  static const char name[] = "glBinormal3ivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const int* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBinormal3sEXT(short arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(short arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glBinormal3sEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(short arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBinormal3svEXT(const short* arg0)
{
  static void(APIENTRY*ptr)(const short* arg0) = NULL;
  static const char name[] = "glBinormal3svEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const short* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTangentPointerEXT(unsigned int arg0, int arg1, const void* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, const void* arg2) = NULL;
  static const char name[] = "glTangentPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, const void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBinormalPointerEXT(unsigned int arg0, int arg1, const void* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, const void* arg2) = NULL;
  static const char name[] = "glBinormalPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, const void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBlendFuncSeparateEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glBlendFuncSeparateEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexWeightfEXT(float arg0)
{
  static void(APIENTRY*ptr)(float arg0) = NULL;
  static const char name[] = "glVertexWeightfEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertexWeightfvEXT(const float* arg0)
{
  static void(APIENTRY*ptr)(const float* arg0) = NULL;
  static const char name[] = "glVertexWeightfvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(const float* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertexWeightPointerEXT(int arg0, unsigned int arg1, int arg2, const void* arg3)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, int arg2, const void* arg3) = NULL;
  static const char name[] = "glVertexWeightPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, int arg2, const void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glSampleMaskEXT(float arg0, unsigned char arg1)
{
  static void(APIENTRY*ptr)(float arg0, unsigned char arg1) = NULL;
  static const char name[] = "glSampleMaskEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(float arg0, unsigned char arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glSamplePatternEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glSamplePatternEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBeginVertexShaderEXT()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glBeginVertexShaderEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glEndVertexShaderEXT()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glEndVertexShaderEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glBindVertexShaderEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glBindVertexShaderEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

unsigned int Pure_glGenVertexShadersEXT(unsigned int arg0)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glGenVertexShadersEXT";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDeleteVertexShaderEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDeleteVertexShaderEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glShaderOp1EXT(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glShaderOp1EXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glShaderOp2EXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glShaderOp2EXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glShaderOp3EXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4) = NULL;
  static const char name[] = "glShaderOp3EXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glSwizzleEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5) = NULL;
  static const char name[] = "glSwizzleEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glWriteMaskEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5) = NULL;
  static const char name[] = "glWriteMaskEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glInsertComponentEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glInsertComponentEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glExtractComponentEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glExtractComponentEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

unsigned int Pure_glGenSymbolsEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glGenSymbolsEXT";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glSetInvariantEXT(unsigned int arg0, unsigned int arg1, const void* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const void* arg2) = NULL;
  static const char name[] = "glSetInvariantEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSetLocalConstantEXT(unsigned int arg0, unsigned int arg1, const void* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const void* arg2) = NULL;
  static const char name[] = "glSetLocalConstantEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVariantbvEXT(unsigned int arg0, const char* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const char* arg1) = NULL;
  static const char name[] = "glVariantbvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVariantsvEXT(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glVariantsvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVariantivEXT(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glVariantivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVariantfvEXT(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glVariantfvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVariantdvEXT(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glVariantdvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVariantubvEXT(unsigned int arg0, const unsigned char* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned char* arg1) = NULL;
  static const char name[] = "glVariantubvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVariantusvEXT(unsigned int arg0, const unsigned short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned short* arg1) = NULL;
  static const char name[] = "glVariantusvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVariantuivEXT(unsigned int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glVariantuivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVariantPointerEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, const void* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, const void* arg3) = NULL;
  static const char name[] = "glVariantPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, const void* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glEnableVariantClientStateEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glEnableVariantClientStateEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDisableVariantClientStateEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDisableVariantClientStateEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

unsigned int Pure_glBindLightParameterEXT(unsigned int arg0, unsigned int arg1)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBindLightParameterEXT";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned int Pure_glBindMaterialParameterEXT(unsigned int arg0, unsigned int arg1)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBindMaterialParameterEXT";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned int Pure_glBindTexGenParameterEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glBindTexGenParameterEXT";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

unsigned int Pure_glBindTextureUnitParameterEXT(unsigned int arg0, unsigned int arg1)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBindTextureUnitParameterEXT";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned int Pure_glBindParameterEXT(unsigned int arg0)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glBindParameterEXT";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

unsigned char Pure_glIsVariantEnabledEXT(unsigned int arg0, unsigned int arg1)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glIsVariantEnabledEXT";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetVariantBooleanvEXT(unsigned int arg0, unsigned int arg1, unsigned char* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned char* arg2) = NULL;
  static const char name[] = "glGetVariantBooleanvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned char* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVariantIntegervEXT(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetVariantIntegervEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVariantFloatvEXT(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetVariantFloatvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVariantPointervEXT(unsigned int arg0, unsigned int arg1, void** arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, void** arg2) = NULL;
  static const char name[] = "glGetVariantPointervEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, void** arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetInvariantBooleanvEXT(unsigned int arg0, unsigned int arg1, unsigned char* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned char* arg2) = NULL;
  static const char name[] = "glGetInvariantBooleanvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned char* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetInvariantIntegervEXT(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetInvariantIntegervEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetInvariantFloatvEXT(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetInvariantFloatvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetLocalConstantBooleanvEXT(unsigned int arg0, unsigned int arg1, unsigned char* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned char* arg2) = NULL;
  static const char name[] = "glGetLocalConstantBooleanvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned char* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetLocalConstantIntegervEXT(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetLocalConstantIntegervEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetLocalConstantFloatvEXT(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetLocalConstantFloatvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glActiveStencilFaceEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glActiveStencilFaceEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDepthBoundsEXT(double arg0, double arg1)
{
  static void(APIENTRY*ptr)(double arg0, double arg1) = NULL;
  static const char name[] = "glDepthBoundsEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glBlendEquationSeparateEXT(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBlendEquationSeparateEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glIsRenderbufferEXT(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsRenderbufferEXT";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBindRenderbufferEXT(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBindRenderbufferEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDeleteRenderbuffersEXT(int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glDeleteRenderbuffersEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGenRenderbuffersEXT(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenRenderbuffersEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glRenderbufferStorageEXT(unsigned int arg0, unsigned int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glRenderbufferStorageEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetRenderbufferParameterivEXT(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetRenderbufferParameterivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

unsigned char Pure_glIsFramebufferEXT(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsFramebufferEXT";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBindFramebufferEXT(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBindFramebufferEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDeleteFramebuffersEXT(int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glDeleteFramebuffersEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGenFramebuffersEXT(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenFramebuffersEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned int Pure_glCheckFramebufferStatusEXT(unsigned int arg0)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glCheckFramebufferStatusEXT";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glFramebufferTexture1DEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int arg4) = NULL;
  static const char name[] = "glFramebufferTexture1DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glFramebufferTexture2DEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int arg4) = NULL;
  static const char name[] = "glFramebufferTexture2DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glFramebufferTexture3DEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5) = NULL;
  static const char name[] = "glFramebufferTexture3DEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glFramebufferRenderbufferEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glFramebufferRenderbufferEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetFramebufferAttachmentParameterivEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3) = NULL;
  static const char name[] = "glGetFramebufferAttachmentParameterivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGenerateMipmapEXT(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glGenerateMipmapEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glGetQueryObjecti64vEXT(unsigned int arg0, unsigned int arg1, long long* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, long long* arg2) = NULL;
  static const char name[] = "glGetQueryObjecti64vEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, long long* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetQueryObjectui64vEXT(unsigned int arg0, unsigned int arg1, unsigned long long* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned long long* arg2) = NULL;
  static const char name[] = "glGetQueryObjectui64vEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned long long* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexBufferEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glTexBufferEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColorMaskIndexedEXT(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4) = NULL;
  static const char name[] = "glColorMaskIndexedEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetBooleanIndexedvEXT(unsigned int arg0, unsigned int arg1, unsigned char* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned char* arg2) = NULL;
  static const char name[] = "glGetBooleanIndexedvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned char* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetIntegerIndexedvEXT(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetIntegerIndexedvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glEnableIndexedEXT(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glEnableIndexedEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDisableIndexedEXT(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glDisableIndexedEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glIsEnabledIndexedEXT(unsigned int arg0, unsigned int arg1)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glIsEnabledIndexedEXT";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glTexParameterIivEXT(unsigned int arg0, unsigned int arg1, const int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const int* arg2) = NULL;
  static const char name[] = "glTexParameterIivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexParameterIuivEXT(unsigned int arg0, unsigned int arg1, const unsigned int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const unsigned int* arg2) = NULL;
  static const char name[] = "glTexParameterIuivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetTexParameterIivEXT(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetTexParameterIivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetTexParameterIuivEXT(unsigned int arg0, unsigned int arg1, unsigned int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int* arg2) = NULL;
  static const char name[] = "glGetTexParameterIuivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glClearColorIiEXT(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glClearColorIiEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glClearColorIuiEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glClearColorIuiEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniformBufferEXT(unsigned int arg0, int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glUniformBufferEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

int Pure_glGetUniformBufferSizeEXT(unsigned int arg0, int arg1)
{
  static int(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glGetUniformBufferSizeEXT";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

long Pure_glGetUniformOffsetEXT(unsigned int arg0, int arg1)
{
  static long(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glGetUniformOffsetEXT";
  if (!ptr) {
    ptr = (long(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetUniformuivEXT(unsigned int arg0, int arg1, unsigned int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int* arg2) = NULL;
  static const char name[] = "glGetUniformuivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBindFragDataLocationEXT(unsigned int arg0, unsigned int arg1, const char* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, const char* arg2) = NULL;
  static const char name[] = "glBindFragDataLocationEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, const char* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

int Pure_glGetFragDataLocationEXT(unsigned int arg0, const char* arg1)
{
  static int(APIENTRY*ptr)(unsigned int arg0, const char* arg1) = NULL;
  static const char name[] = "glGetFragDataLocationEXT";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, const char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glUniform1uiEXT(int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glUniform1uiEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glUniform2uiEXT(int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glUniform2uiEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform3uiEXT(int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glUniform3uiEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glUniform4uiEXT(int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4) = NULL;
  static const char name[] = "glUniform4uiEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glUniform1uivEXT(int arg0, int arg1, const unsigned int* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const unsigned int* arg2) = NULL;
  static const char name[] = "glUniform1uivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform2uivEXT(int arg0, int arg1, const unsigned int* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const unsigned int* arg2) = NULL;
  static const char name[] = "glUniform2uivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform3uivEXT(int arg0, int arg1, const unsigned int* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const unsigned int* arg2) = NULL;
  static const char name[] = "glUniform3uivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glUniform4uivEXT(int arg0, int arg1, const unsigned int* arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, const unsigned int* arg2) = NULL;
  static const char name[] = "glUniform4uivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, const unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribI1iEXT(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glVertexAttribI1iEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI2iEXT(unsigned int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glVertexAttribI2iEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribI3iEXT(unsigned int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glVertexAttribI3iEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttribI4iEXT(unsigned int arg0, int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glVertexAttribI4iEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttribI1uiEXT(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glVertexAttribI1uiEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI2uiEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glVertexAttribI2uiEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribI3uiEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glVertexAttribI3uiEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttribI4uiEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4) = NULL;
  static const char name[] = "glVertexAttribI4uiEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttribI1ivEXT(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glVertexAttribI1ivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI2ivEXT(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glVertexAttribI2ivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI3ivEXT(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glVertexAttribI3ivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI4ivEXT(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glVertexAttribI4ivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI1uivEXT(unsigned int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glVertexAttribI1uivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI2uivEXT(unsigned int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glVertexAttribI2uivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI3uivEXT(unsigned int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glVertexAttribI3uivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI4uivEXT(unsigned int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glVertexAttribI4uivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI4bvEXT(unsigned int arg0, const char* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const char* arg1) = NULL;
  static const char name[] = "glVertexAttribI4bvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI4svEXT(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glVertexAttribI4svEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI4ubvEXT(unsigned int arg0, const unsigned char* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned char* arg1) = NULL;
  static const char name[] = "glVertexAttribI4ubvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribI4usvEXT(unsigned int arg0, const unsigned short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const unsigned short* arg1) = NULL;
  static const char name[] = "glVertexAttribI4usvEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const unsigned short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribIPointerEXT(unsigned int arg0, int arg1, unsigned int arg2, int arg3, const void* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, const void* arg4) = NULL;
  static const char name[] = "glVertexAttribIPointerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, const void* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetVertexAttribIivEXT(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetVertexAttribIivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribIuivEXT(unsigned int arg0, unsigned int arg1, unsigned int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int* arg2) = NULL;
  static const char name[] = "glGetVertexAttribIuivEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glProgramParameteriEXT(unsigned int arg0, unsigned int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2) = NULL;
  static const char name[] = "glProgramParameteriEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glFramebufferTextureEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3) = NULL;
  static const char name[] = "glFramebufferTextureEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glFramebufferTextureLayerEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glFramebufferTextureLayerEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glFramebufferTextureFaceEXT(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int arg4) = NULL;
  static const char name[] = "glFramebufferTextureFaceEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glRenderbufferStorageMultisampleEXT(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glRenderbufferStorageMultisampleEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glBlitFramebufferEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, unsigned int arg9)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, unsigned int arg9) = NULL;
  static const char name[] = "glBlitFramebufferEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, unsigned int arg9))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_glDrawArraysInstancedEXT(unsigned int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glDrawArraysInstancedEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glDrawElementsInstancedEXT(unsigned int arg0, int arg1, unsigned int arg2, const void* arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, const void* arg3, int arg4) = NULL;
  static const char name[] = "glDrawElementsInstancedEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, const void* arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}
