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
#include <GLUT/glut.h>
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

void Pure_glFlushVertexArrayRangeNV()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glFlushVertexArrayRangeNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glVertexArrayRangeNV(int arg0, void const* arg1)
{
  static void(APIENTRY*ptr)(int arg0, void const* arg1) = NULL;
  static const char name[] = "glVertexArrayRangeNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, void const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glCombinerParameterfvNV(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glCombinerParameterfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glCombinerParameterfNV(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glCombinerParameterfNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glCombinerParameterivNV(unsigned int arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1) = NULL;
  static const char name[] = "glCombinerParameterivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glCombinerParameteriNV(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glCombinerParameteriNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glCombinerInputNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5) = NULL;
  static const char name[] = "glCombinerInputNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glCombinerOutputNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned char arg7, unsigned char arg8, unsigned char arg9)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned char arg7, unsigned char arg8, unsigned char arg9) = NULL;
  static const char name[] = "glCombinerOutputNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned char arg7, unsigned char arg8, unsigned char arg9))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_glFinalCombinerInputNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glFinalCombinerInputNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetCombinerInputParameterfvNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, float* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, float* arg4) = NULL;
  static const char name[] = "glGetCombinerInputParameterfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, float* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetCombinerInputParameterivNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int* arg4) = NULL;
  static const char name[] = "glGetCombinerInputParameterivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, int* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetCombinerOutputParameterfvNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, float* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, float* arg3) = NULL;
  static const char name[] = "glGetCombinerOutputParameterfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, float* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetCombinerOutputParameterivNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3) = NULL;
  static const char name[] = "glGetCombinerOutputParameterivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetFinalCombinerInputParameterfvNV(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetFinalCombinerInputParameterfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetFinalCombinerInputParameterivNV(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetFinalCombinerInputParameterivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glDeleteFencesNV(int arg0, unsigned int const* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int const* arg1) = NULL;
  static const char name[] = "glDeleteFencesNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGenFencesNV(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenFencesNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glIsFenceNV(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsFenceNV";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

unsigned char Pure_glTestFenceNV(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glTestFenceNV";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glGetFenceivNV(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetFenceivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glFinishFenceNV(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glFinishFenceNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSetFenceNV(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glSetFenceNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMapControlPointsNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, unsigned char arg7, void const* arg8)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, unsigned char arg7, void const* arg8) = NULL;
  static const char name[] = "glMapControlPointsNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int arg4, int arg5, int arg6, unsigned char arg7, void const* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_glMapParameterivNV(unsigned int arg0, unsigned int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int const* arg2) = NULL;
  static const char name[] = "glMapParameterivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMapParameterfvNV(unsigned int arg0, unsigned int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float const* arg2) = NULL;
  static const char name[] = "glMapParameterfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMapControlPointsNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int arg4, unsigned char arg5, void* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int arg4, unsigned char arg5, void* arg6) = NULL;
  static const char name[] = "glGetMapControlPointsNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int arg4, unsigned char arg5, void* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glGetMapParameterivNV(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetMapParameterivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMapParameterfvNV(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetMapParameterfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetMapAttribParameterivNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3) = NULL;
  static const char name[] = "glGetMapAttribParameterivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetMapAttribParameterfvNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, float* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, float* arg3) = NULL;
  static const char name[] = "glGetMapAttribParameterfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, float* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glEvalMapsNV(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glEvalMapsNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glCombinerStageParameterfvNV(unsigned int arg0, unsigned int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float const* arg2) = NULL;
  static const char name[] = "glCombinerStageParameterfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetCombinerStageParameterfvNV(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetCombinerStageParameterfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

unsigned char Pure_glAreProgramsResidentNV(int arg0, unsigned int const* arg1, unsigned char* arg2)
{
  static unsigned char(APIENTRY*ptr)(int arg0, unsigned int const* arg1, unsigned char* arg2) = NULL;
  static const char name[] = "glAreProgramsResidentNV";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(int arg0, unsigned int const* arg1, unsigned char* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBindProgramNV(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBindProgramNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDeleteProgramsNV(int arg0, unsigned int const* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int const* arg1) = NULL;
  static const char name[] = "glDeleteProgramsNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glExecuteProgramNV(unsigned int arg0, unsigned int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float const* arg2) = NULL;
  static const char name[] = "glExecuteProgramNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGenProgramsNV(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenProgramsNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetProgramParameterdvNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, double* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, double* arg3) = NULL;
  static const char name[] = "glGetProgramParameterdvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, double* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetProgramParameterfvNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, float* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, float* arg3) = NULL;
  static const char name[] = "glGetProgramParameterfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, float* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetProgramivNV(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetProgramivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetProgramStringNV(unsigned int arg0, unsigned int arg1, unsigned char* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned char* arg2) = NULL;
  static const char name[] = "glGetProgramStringNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned char* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetTrackMatrixivNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3) = NULL;
  static const char name[] = "glGetTrackMatrixivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetVertexAttribdvNV(unsigned int arg0, unsigned int arg1, double* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double* arg2) = NULL;
  static const char name[] = "glGetVertexAttribdvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribfvNV(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetVertexAttribfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribivNV(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetVertexAttribivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribPointervNV(unsigned int arg0, unsigned int arg1, void** arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, void** arg2) = NULL;
  static const char name[] = "glGetVertexAttribPointervNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, void** arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

unsigned char Pure_glIsProgramNV(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsProgramNV";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glLoadProgramNV(unsigned int arg0, unsigned int arg1, int arg2, unsigned char const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, unsigned char const* arg3) = NULL;
  static const char name[] = "glLoadProgramNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, unsigned char const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glProgramParameter4dNV(unsigned int arg0, unsigned int arg1, double arg2, double arg3, double arg4, double arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double arg2, double arg3, double arg4, double arg5) = NULL;
  static const char name[] = "glProgramParameter4dNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double arg2, double arg3, double arg4, double arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glProgramParameter4dvNV(unsigned int arg0, unsigned int arg1, double const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, double const* arg2) = NULL;
  static const char name[] = "glProgramParameter4dvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, double const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glProgramParameter4fNV(unsigned int arg0, unsigned int arg1, float arg2, float arg3, float arg4, float arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float arg2, float arg3, float arg4, float arg5) = NULL;
  static const char name[] = "glProgramParameter4fNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float arg2, float arg3, float arg4, float arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glProgramParameter4fvNV(unsigned int arg0, unsigned int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float const* arg2) = NULL;
  static const char name[] = "glProgramParameter4fvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glProgramParameters4dvNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, double const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, double const* arg3) = NULL;
  static const char name[] = "glProgramParameters4dvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, double const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glProgramParameters4fvNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, float const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, float const* arg3) = NULL;
  static const char name[] = "glProgramParameters4fvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, float const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glRequestResidentProgramsNV(int arg0, unsigned int const* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int const* arg1) = NULL;
  static const char name[] = "glRequestResidentProgramsNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glTrackMatrixNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glTrackMatrixNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttribPointerNV(unsigned int arg0, int arg1, unsigned int arg2, int arg3, void const* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, void const* arg4) = NULL;
  static const char name[] = "glVertexAttribPointerNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, void const* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib1dNV(unsigned int arg0, double arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1) = NULL;
  static const char name[] = "glVertexAttrib1dNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1dvNV(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glVertexAttrib1dvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1fNV(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glVertexAttrib1fNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1fvNV(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glVertexAttrib1fvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1sNV(unsigned int arg0, short arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1) = NULL;
  static const char name[] = "glVertexAttrib1sNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1svNV(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib1svNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib2dNV(unsigned int arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glVertexAttrib2dNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttrib2dvNV(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glVertexAttrib2dvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib2fNV(unsigned int arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glVertexAttrib2fNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttrib2fvNV(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glVertexAttrib2fvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib2sNV(unsigned int arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glVertexAttrib2sNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttrib2svNV(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib2svNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib3dNV(unsigned int arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glVertexAttrib3dNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttrib3dvNV(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glVertexAttrib3dvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib3fNV(unsigned int arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glVertexAttrib3fNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttrib3fvNV(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glVertexAttrib3fvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib3sNV(unsigned int arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glVertexAttrib3sNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttrib3svNV(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib3svNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4dNV(unsigned int arg0, double arg1, double arg2, double arg3, double arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4) = NULL;
  static const char name[] = "glVertexAttrib4dNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4dvNV(unsigned int arg0, double const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4dvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4fNV(unsigned int arg0, float arg1, float arg2, float arg3, float arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4) = NULL;
  static const char name[] = "glVertexAttrib4fNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4fvNV(unsigned int arg0, float const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4fvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4sNV(unsigned int arg0, short arg1, short arg2, short arg3, short arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4) = NULL;
  static const char name[] = "glVertexAttrib4sNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4svNV(unsigned int arg0, short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4svNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4ubNV(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4) = NULL;
  static const char name[] = "glVertexAttrib4ubNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4ubvNV(unsigned int arg0, unsigned char const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned char const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4ubvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribs1dvNV(unsigned int arg0, int arg1, double const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, double const* arg2) = NULL;
  static const char name[] = "glVertexAttribs1dvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, double const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs1fvNV(unsigned int arg0, int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, float const* arg2) = NULL;
  static const char name[] = "glVertexAttribs1fvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs1svNV(unsigned int arg0, int arg1, short const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, short const* arg2) = NULL;
  static const char name[] = "glVertexAttribs1svNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, short const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs2dvNV(unsigned int arg0, int arg1, double const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, double const* arg2) = NULL;
  static const char name[] = "glVertexAttribs2dvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, double const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs2fvNV(unsigned int arg0, int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, float const* arg2) = NULL;
  static const char name[] = "glVertexAttribs2fvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs2svNV(unsigned int arg0, int arg1, short const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, short const* arg2) = NULL;
  static const char name[] = "glVertexAttribs2svNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, short const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs3dvNV(unsigned int arg0, int arg1, double const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, double const* arg2) = NULL;
  static const char name[] = "glVertexAttribs3dvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, double const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs3fvNV(unsigned int arg0, int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, float const* arg2) = NULL;
  static const char name[] = "glVertexAttribs3fvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs3svNV(unsigned int arg0, int arg1, short const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, short const* arg2) = NULL;
  static const char name[] = "glVertexAttribs3svNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, short const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs4dvNV(unsigned int arg0, int arg1, double const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, double const* arg2) = NULL;
  static const char name[] = "glVertexAttribs4dvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, double const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs4fvNV(unsigned int arg0, int arg1, float const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, float const* arg2) = NULL;
  static const char name[] = "glVertexAttribs4fvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, float const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs4svNV(unsigned int arg0, int arg1, short const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, short const* arg2) = NULL;
  static const char name[] = "glVertexAttribs4svNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, short const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs4ubvNV(unsigned int arg0, int arg1, unsigned char const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned char const* arg2) = NULL;
  static const char name[] = "glVertexAttribs4ubvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned char const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGenOcclusionQueriesNV(int arg0, unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int* arg1) = NULL;
  static const char name[] = "glGenOcclusionQueriesNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDeleteOcclusionQueriesNV(int arg0, unsigned int const* arg1)
{
  static void(APIENTRY*ptr)(int arg0, unsigned int const* arg1) = NULL;
  static const char name[] = "glDeleteOcclusionQueriesNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, unsigned int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned char Pure_glIsOcclusionQueryNV(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsOcclusionQueryNV";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBeginOcclusionQueryNV(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glBeginOcclusionQueryNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEndOcclusionQueryNV()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glEndOcclusionQueryNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glGetOcclusionQueryivNV(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetOcclusionQueryivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetOcclusionQueryuivNV(unsigned int arg0, unsigned int arg1, unsigned int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int* arg2) = NULL;
  static const char name[] = "glGetOcclusionQueryuivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glPointParameteriNV(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glPointParameteriNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPointParameterivNV(unsigned int arg0, int const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1) = NULL;
  static const char name[] = "glPointParameterivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glProgramNamedParameter4fNV(unsigned int arg0, int arg1, unsigned char const* arg2, float arg3, float arg4, float arg5, float arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned char const* arg2, float arg3, float arg4, float arg5, float arg6) = NULL;
  static const char name[] = "glProgramNamedParameter4fNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned char const* arg2, float arg3, float arg4, float arg5, float arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glProgramNamedParameter4dNV(unsigned int arg0, int arg1, unsigned char const* arg2, double arg3, double arg4, double arg5, double arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned char const* arg2, double arg3, double arg4, double arg5, double arg6) = NULL;
  static const char name[] = "glProgramNamedParameter4dNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned char const* arg2, double arg3, double arg4, double arg5, double arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glProgramNamedParameter4fvNV(unsigned int arg0, int arg1, unsigned char const* arg2, float const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned char const* arg2, float const* arg3) = NULL;
  static const char name[] = "glProgramNamedParameter4fvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned char const* arg2, float const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glProgramNamedParameter4dvNV(unsigned int arg0, int arg1, unsigned char const* arg2, double const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned char const* arg2, double const* arg3) = NULL;
  static const char name[] = "glProgramNamedParameter4dvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned char const* arg2, double const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetProgramNamedParameterfvNV(unsigned int arg0, int arg1, unsigned char const* arg2, float* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned char const* arg2, float* arg3) = NULL;
  static const char name[] = "glGetProgramNamedParameterfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned char const* arg2, float* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetProgramNamedParameterdvNV(unsigned int arg0, int arg1, unsigned char const* arg2, double* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned char const* arg2, double* arg3) = NULL;
  static const char name[] = "glGetProgramNamedParameterdvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned char const* arg2, double* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertex2hNV(unsigned short arg0, unsigned short arg1)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1) = NULL;
  static const char name[] = "glVertex2hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertex2hvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glVertex2hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex3hNV(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2) = NULL;
  static const char name[] = "glVertex3hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertex3hvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glVertex3hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertex4hNV(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3) = NULL;
  static const char name[] = "glVertex4hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertex4hvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glVertex4hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glNormal3hNV(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2) = NULL;
  static const char name[] = "glNormal3hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glNormal3hvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glNormal3hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor3hNV(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2) = NULL;
  static const char name[] = "glColor3hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColor3hvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glColor3hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glColor4hNV(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3) = NULL;
  static const char name[] = "glColor4hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glColor4hvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glColor4hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord1hNV(unsigned short arg0)
{
  static void(APIENTRY*ptr)(unsigned short arg0) = NULL;
  static const char name[] = "glTexCoord1hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord1hvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glTexCoord1hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord2hNV(unsigned short arg0, unsigned short arg1)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1) = NULL;
  static const char name[] = "glTexCoord2hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glTexCoord2hvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glTexCoord2hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord3hNV(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2) = NULL;
  static const char name[] = "glTexCoord3hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTexCoord3hvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glTexCoord3hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glTexCoord4hNV(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3) = NULL;
  static const char name[] = "glTexCoord4hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glTexCoord4hvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glTexCoord4hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glMultiTexCoord1hNV(unsigned int arg0, unsigned short arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short arg1) = NULL;
  static const char name[] = "glMultiTexCoord1hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord1hvNV(unsigned int arg0, unsigned short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord1hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord2hNV(unsigned int arg0, unsigned short arg1, unsigned short arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short arg1, unsigned short arg2) = NULL;
  static const char name[] = "glMultiTexCoord2hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short arg1, unsigned short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glMultiTexCoord2hvNV(unsigned int arg0, unsigned short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord2hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord3hNV(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3) = NULL;
  static const char name[] = "glMultiTexCoord3hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glMultiTexCoord3hvNV(unsigned int arg0, unsigned short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord3hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glMultiTexCoord4hNV(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3, unsigned short arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3, unsigned short arg4) = NULL;
  static const char name[] = "glMultiTexCoord4hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3, unsigned short arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glMultiTexCoord4hvNV(unsigned int arg0, unsigned short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short const* arg1) = NULL;
  static const char name[] = "glMultiTexCoord4hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glFogCoordhNV(unsigned short arg0)
{
  static void(APIENTRY*ptr)(unsigned short arg0) = NULL;
  static const char name[] = "glFogCoordhNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glFogCoordhvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glFogCoordhvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glSecondaryColor3hNV(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  static void(APIENTRY*ptr)(unsigned short arg0, unsigned short arg1, unsigned short arg2) = NULL;
  static const char name[] = "glSecondaryColor3hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0, unsigned short arg1, unsigned short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSecondaryColor3hvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glSecondaryColor3hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertexWeighthNV(unsigned short arg0)
{
  static void(APIENTRY*ptr)(unsigned short arg0) = NULL;
  static const char name[] = "glVertexWeighthNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertexWeighthvNV(unsigned short const* arg0)
{
  static void(APIENTRY*ptr)(unsigned short const* arg0) = NULL;
  static const char name[] = "glVertexWeighthvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned short const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertexAttrib1hNV(unsigned int arg0, unsigned short arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short arg1) = NULL;
  static const char name[] = "glVertexAttrib1hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib1hvNV(unsigned int arg0, unsigned short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib1hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib2hNV(unsigned int arg0, unsigned short arg1, unsigned short arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short arg1, unsigned short arg2) = NULL;
  static const char name[] = "glVertexAttrib2hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short arg1, unsigned short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttrib2hvNV(unsigned int arg0, unsigned short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib2hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib3hNV(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3) = NULL;
  static const char name[] = "glVertexAttrib3hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttrib3hvNV(unsigned int arg0, unsigned short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib3hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttrib4hNV(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3, unsigned short arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3, unsigned short arg4) = NULL;
  static const char name[] = "glVertexAttrib4hNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short arg1, unsigned short arg2, unsigned short arg3, unsigned short arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexAttrib4hvNV(unsigned int arg0, unsigned short const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned short const* arg1) = NULL;
  static const char name[] = "glVertexAttrib4hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned short const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexAttribs1hvNV(unsigned int arg0, int arg1, unsigned short const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned short const* arg2) = NULL;
  static const char name[] = "glVertexAttribs1hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned short const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs2hvNV(unsigned int arg0, int arg1, unsigned short const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned short const* arg2) = NULL;
  static const char name[] = "glVertexAttribs2hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned short const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs3hvNV(unsigned int arg0, int arg1, unsigned short const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned short const* arg2) = NULL;
  static const char name[] = "glVertexAttribs3hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned short const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexAttribs4hvNV(unsigned int arg0, int arg1, unsigned short const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned short const* arg2) = NULL;
  static const char name[] = "glVertexAttribs4hvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned short const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glPixelDataRangeNV(unsigned int arg0, int arg1, void* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, void* arg2) = NULL;
  static const char name[] = "glPixelDataRangeNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glFlushPixelDataRangeNV(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glFlushPixelDataRangeNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glPrimitiveRestartNV()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glPrimitiveRestartNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glPrimitiveRestartIndexNV(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glPrimitiveRestartIndexNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBeginTransformFeedbackNV(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glBeginTransformFeedbackNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glEndTransformFeedbackNV()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glEndTransformFeedbackNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glTransformFeedbackAttribsNV(unsigned int arg0, int const* arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int const* arg1, unsigned int arg2) = NULL;
  static const char name[] = "glTransformFeedbackAttribsNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int const* arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glBindBufferRangeNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, long arg3, long arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, long arg3, long arg4) = NULL;
  static const char name[] = "glBindBufferRangeNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, long arg3, long arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glBindBufferOffsetNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, long arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, long arg3) = NULL;
  static const char name[] = "glBindBufferOffsetNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, long arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glBindBufferBaseNV(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glBindBufferBaseNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glTransformFeedbackVaryingsNV(unsigned int arg0, int arg1, int const* arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int const* arg2, unsigned int arg3) = NULL;
  static const char name[] = "glTransformFeedbackVaryingsNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int const* arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glActiveVaryingNV(unsigned int arg0, char const* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, char const* arg1) = NULL;
  static const char name[] = "glActiveVaryingNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

int Pure_glGetVaryingLocationNV(unsigned int arg0, char const* arg1)
{
  static int(APIENTRY*ptr)(unsigned int arg0, char const* arg1) = NULL;
  static const char name[] = "glGetVaryingLocationNV";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetActiveVaryingNV(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6) = NULL;
  static const char name[] = "glGetActiveVaryingNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int* arg3, int* arg4, unsigned int* arg5, char* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glGetTransformFeedbackVaryingNV(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetTransformFeedbackVaryingNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glDepthRangedNV(double arg0, double arg1)
{
  static void(APIENTRY*ptr)(double arg0, double arg1) = NULL;
  static const char name[] = "glDepthRangedNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glClearDepthdNV(double arg0)
{
  static void(APIENTRY*ptr)(double arg0) = NULL;
  static const char name[] = "glClearDepthdNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDepthBoundsdNV(double arg0, double arg1)
{
  static void(APIENTRY*ptr)(double arg0, double arg1) = NULL;
  static const char name[] = "glDepthBoundsdNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glProgramVertexLimitNV(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glProgramVertexLimitNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glProgramLocalParameterI4iNV(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5) = NULL;
  static const char name[] = "glProgramLocalParameterI4iNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glProgramLocalParameterI4ivNV(unsigned int arg0, unsigned int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int const* arg2) = NULL;
  static const char name[] = "glProgramLocalParameterI4ivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glProgramLocalParametersI4ivNV(unsigned int arg0, unsigned int arg1, int arg2, int const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int const* arg3) = NULL;
  static const char name[] = "glProgramLocalParametersI4ivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glProgramLocalParameterI4uiNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5) = NULL;
  static const char name[] = "glProgramLocalParameterI4uiNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glProgramLocalParameterI4uivNV(unsigned int arg0, unsigned int arg1, unsigned int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int const* arg2) = NULL;
  static const char name[] = "glProgramLocalParameterI4uivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glProgramLocalParametersI4uivNV(unsigned int arg0, unsigned int arg1, int arg2, unsigned int const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int const* arg3) = NULL;
  static const char name[] = "glProgramLocalParametersI4uivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glProgramEnvParameterI4iNV(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5) = NULL;
  static const char name[] = "glProgramEnvParameterI4iNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glProgramEnvParameterI4ivNV(unsigned int arg0, unsigned int arg1, int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int const* arg2) = NULL;
  static const char name[] = "glProgramEnvParameterI4ivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glProgramEnvParametersI4ivNV(unsigned int arg0, unsigned int arg1, int arg2, int const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, int const* arg3) = NULL;
  static const char name[] = "glProgramEnvParametersI4ivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, int const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glProgramEnvParameterI4uiNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5) = NULL;
  static const char name[] = "glProgramEnvParameterI4uiNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glProgramEnvParameterI4uivNV(unsigned int arg0, unsigned int arg1, unsigned int const* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int const* arg2) = NULL;
  static const char name[] = "glProgramEnvParameterI4uivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int const* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glProgramEnvParametersI4uivNV(unsigned int arg0, unsigned int arg1, int arg2, unsigned int const* arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int const* arg3) = NULL;
  static const char name[] = "glProgramEnvParametersI4uivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int const* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glGetProgramLocalParameterIivNV(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetProgramLocalParameterIivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetProgramLocalParameterIuivNV(unsigned int arg0, unsigned int arg1, unsigned int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int* arg2) = NULL;
  static const char name[] = "glGetProgramLocalParameterIuivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetProgramEnvParameterIivNV(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetProgramEnvParameterIivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetProgramEnvParameterIuivNV(unsigned int arg0, unsigned int arg1, unsigned int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int* arg2) = NULL;
  static const char name[] = "glGetProgramEnvParameterIuivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glProgramBufferParametersfvNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, float const* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, float const* arg4) = NULL;
  static const char name[] = "glProgramBufferParametersfvNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, float const* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glProgramBufferParametersIivNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int const* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int const* arg4) = NULL;
  static const char name[] = "glProgramBufferParametersIivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, int const* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glProgramBufferParametersIuivNV(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int const* arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int const* arg4) = NULL;
  static const char name[] = "glProgramBufferParametersIuivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int const* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glRenderbufferStorageMultisampleCoverageNV(unsigned int arg0, int arg1, int arg2, unsigned int arg3, int arg4, int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, int arg4, int arg5) = NULL;
  static const char name[] = "glRenderbufferStorageMultisampleCoverageNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, int arg4, int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glPresentFrameKeyedNV(unsigned int arg0, unsigned long long arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned long long arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10) = NULL;
  static const char name[] = "glPresentFrameKeyedNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned long long arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_glPresentFrameDualFillNV(unsigned int arg0, unsigned long long arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10, unsigned int arg11, unsigned int arg12)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned long long arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10, unsigned int arg11, unsigned int arg12) = NULL;
  static const char name[] = "glPresentFrameDualFillNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned long long arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10, unsigned int arg11, unsigned int arg12))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

void Pure_glGetVideoivNV(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetVideoivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVideouivNV(unsigned int arg0, unsigned int arg1, unsigned int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int* arg2) = NULL;
  static const char name[] = "glGetVideouivNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVideoi64vNV(unsigned int arg0, unsigned int arg1, long long* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, long long* arg2) = NULL;
  static const char name[] = "glGetVideoi64vNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, long long* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVideoui64vNV(unsigned int arg0, unsigned int arg1, unsigned long long* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned long long* arg2) = NULL;
  static const char name[] = "glGetVideoui64vNV";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned long long* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}
