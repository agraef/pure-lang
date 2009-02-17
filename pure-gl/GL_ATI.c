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

void Pure_glBlendEquationSeparateATI(unsigned int arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1) = NULL;
  static const char name[] = "glBlendEquationSeparateATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glTexBumpParameterivATI(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glTexBumpParameterivATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glTexBumpParameterfvATI(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glTexBumpParameterfvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetTexBumpParameterivATI(unsigned int arg0, int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int* arg1) = NULL;
  static const char name[] = "glGetTexBumpParameterivATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glGetTexBumpParameterfvATI(unsigned int arg0, float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float* arg1) = NULL;
  static const char name[] = "glGetTexBumpParameterfvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned int Pure_glGenFragmentShadersATI(unsigned int arg0)
{
  static unsigned int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glGenFragmentShadersATI";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBindFragmentShaderATI(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glBindFragmentShaderATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glDeleteFragmentShaderATI(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glDeleteFragmentShaderATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glBeginFragmentShaderATI()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glBeginFragmentShaderATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glEndFragmentShaderATI()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glEndFragmentShaderATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glPassTexCoordATI(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glPassTexCoordATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glSampleMapATI(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2) = NULL;
  static const char name[] = "glSampleMapATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glColorFragmentOp1ATI(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6) = NULL;
  static const char name[] = "glColorFragmentOp1ATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glColorFragmentOp2ATI(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9) = NULL;
  static const char name[] = "glColorFragmentOp2ATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_glColorFragmentOp3ATI(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10, unsigned int arg11, unsigned int arg12)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10, unsigned int arg11, unsigned int arg12) = NULL;
  static const char name[] = "glColorFragmentOp3ATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10, unsigned int arg11, unsigned int arg12))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

void Pure_glAlphaFragmentOp1ATI(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5) = NULL;
  static const char name[] = "glAlphaFragmentOp1ATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glAlphaFragmentOp2ATI(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8) = NULL;
  static const char name[] = "glAlphaFragmentOp2ATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_glAlphaFragmentOp3ATI(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10, unsigned int arg11)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10, unsigned int arg11) = NULL;
  static const char name[] = "glAlphaFragmentOp3ATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9, unsigned int arg10, unsigned int arg11))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

void Pure_glSetFragmentShaderConstantATI(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glSetFragmentShaderConstantATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPNTrianglesiATI(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glPNTrianglesiATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glPNTrianglesfATI(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glPNTrianglesfATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

unsigned int Pure_glNewObjectBufferATI(int arg0, const void* arg1, unsigned int arg2)
{
  static unsigned int(APIENTRY*ptr)(int arg0, const void* arg1, unsigned int arg2) = NULL;
  static const char name[] = "glNewObjectBufferATI";
  if (!ptr) {
    ptr = (unsigned int(APIENTRY*)(int arg0, const void* arg1, unsigned int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

unsigned char Pure_glIsObjectBufferATI(unsigned int arg0)
{
  static unsigned char(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glIsObjectBufferATI";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glUpdateObjectBufferATI(unsigned int arg0, unsigned int arg1, int arg2, const void* arg3, unsigned int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, const void* arg3, unsigned int arg4) = NULL;
  static const char name[] = "glUpdateObjectBufferATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, const void* arg3, unsigned int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetObjectBufferfvATI(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetObjectBufferfvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetObjectBufferivATI(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetObjectBufferivATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glFreeObjectBufferATI(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glFreeObjectBufferATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glArrayObjectATI(unsigned int arg0, int arg1, unsigned int arg2, int arg3, unsigned int arg4, unsigned int arg5)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, unsigned int arg4, unsigned int arg5) = NULL;
  static const char name[] = "glArrayObjectATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, int arg3, unsigned int arg4, unsigned int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_glGetArrayObjectfvATI(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetArrayObjectfvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetArrayObjectivATI(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetArrayObjectivATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVariantArrayObjectATI(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4) = NULL;
  static const char name[] = "glVariantArrayObjectATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3, unsigned int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glGetVariantArrayObjectfvATI(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetVariantArrayObjectfvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVariantArrayObjectivATI(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetVariantArrayObjectivATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexStream1sATI(unsigned int arg0, short arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1) = NULL;
  static const char name[] = "glVertexStream1sATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream1svATI(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glVertexStream1svATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream1iATI(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glVertexStream1iATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream1ivATI(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glVertexStream1ivATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream1fATI(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glVertexStream1fATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream1fvATI(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glVertexStream1fvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream1dATI(unsigned int arg0, double arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1) = NULL;
  static const char name[] = "glVertexStream1dATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream1dvATI(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glVertexStream1dvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream2sATI(unsigned int arg0, short arg1, short arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2) = NULL;
  static const char name[] = "glVertexStream2sATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexStream2svATI(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glVertexStream2svATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream2iATI(unsigned int arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glVertexStream2iATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexStream2ivATI(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glVertexStream2ivATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream2fATI(unsigned int arg0, float arg1, float arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2) = NULL;
  static const char name[] = "glVertexStream2fATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexStream2fvATI(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glVertexStream2fvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream2dATI(unsigned int arg0, double arg1, double arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2) = NULL;
  static const char name[] = "glVertexStream2dATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glVertexStream2dvATI(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glVertexStream2dvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream3sATI(unsigned int arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glVertexStream3sATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexStream3svATI(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glVertexStream3svATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream3iATI(unsigned int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glVertexStream3iATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexStream3ivATI(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glVertexStream3ivATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream3fATI(unsigned int arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glVertexStream3fATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexStream3fvATI(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glVertexStream3fvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream3dATI(unsigned int arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glVertexStream3dATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexStream3dvATI(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glVertexStream3dvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream4sATI(unsigned int arg0, short arg1, short arg2, short arg3, short arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4) = NULL;
  static const char name[] = "glVertexStream4sATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3, short arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexStream4svATI(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glVertexStream4svATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream4iATI(unsigned int arg0, int arg1, int arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glVertexStream4iATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexStream4ivATI(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glVertexStream4ivATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream4fATI(unsigned int arg0, float arg1, float arg2, float arg3, float arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4) = NULL;
  static const char name[] = "glVertexStream4fATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3, float arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexStream4fvATI(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glVertexStream4fvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexStream4dATI(unsigned int arg0, double arg1, double arg2, double arg3, double arg4)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4) = NULL;
  static const char name[] = "glVertexStream4dATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3, double arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glVertexStream4dvATI(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glVertexStream4dvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glNormalStream3bATI(unsigned int arg0, char arg1, char arg2, char arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, char arg1, char arg2, char arg3) = NULL;
  static const char name[] = "glNormalStream3bATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, char arg1, char arg2, char arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glNormalStream3bvATI(unsigned int arg0, const char* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const char* arg1) = NULL;
  static const char name[] = "glNormalStream3bvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glNormalStream3sATI(unsigned int arg0, short arg1, short arg2, short arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, short arg1, short arg2, short arg3) = NULL;
  static const char name[] = "glNormalStream3sATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, short arg1, short arg2, short arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glNormalStream3svATI(unsigned int arg0, const short* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const short* arg1) = NULL;
  static const char name[] = "glNormalStream3svATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const short* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glNormalStream3iATI(unsigned int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glNormalStream3iATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glNormalStream3ivATI(unsigned int arg0, const int* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const int* arg1) = NULL;
  static const char name[] = "glNormalStream3ivATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glNormalStream3fATI(unsigned int arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glNormalStream3fATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glNormalStream3fvATI(unsigned int arg0, const float* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const float* arg1) = NULL;
  static const char name[] = "glNormalStream3fvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glNormalStream3dATI(unsigned int arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "glNormalStream3dATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glNormalStream3dvATI(unsigned int arg0, const double* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const double* arg1) = NULL;
  static const char name[] = "glNormalStream3dvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const double* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glClientActiveVertexStreamATI(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glClientActiveVertexStreamATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glVertexBlendEnviATI(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glVertexBlendEnviATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glVertexBlendEnvfATI(unsigned int arg0, float arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, float arg1) = NULL;
  static const char name[] = "glVertexBlendEnvfATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, float arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glElementPointerATI(unsigned int arg0, const void* arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, const void* arg1) = NULL;
  static const char name[] = "glElementPointerATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, const void* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDrawElementArrayATI(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glDrawElementArrayATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glDrawRangeElementArrayATI(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3) = NULL;
  static const char name[] = "glDrawRangeElementArrayATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glDrawBuffersATI(int arg0, const unsigned int* arg1)
{
  static void(APIENTRY*ptr)(int arg0, const unsigned int* arg1) = NULL;
  static const char name[] = "glDrawBuffersATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, const unsigned int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void* Pure_glMapObjectBufferATI(unsigned int arg0)
{
  static void*(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glMapObjectBufferATI";
  if (!ptr) {
    ptr = (void*(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glUnmapObjectBufferATI(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glUnmapObjectBufferATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glStencilOpSeparateATI(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glStencilOpSeparateATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glStencilFuncSeparateATI(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3) = NULL;
  static const char name[] = "glStencilFuncSeparateATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int arg2, unsigned int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glVertexAttribArrayObjectATI(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3, int arg4, unsigned int arg5, unsigned int arg6)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3, int arg4, unsigned int arg5, unsigned int arg6) = NULL;
  static const char name[] = "glVertexAttribArrayObjectATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1, unsigned int arg2, unsigned char arg3, int arg4, unsigned int arg5, unsigned int arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_glGetVertexAttribArrayObjectfvATI(unsigned int arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "glGetVertexAttribArrayObjectfvATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glGetVertexAttribArrayObjectivATI(unsigned int arg0, unsigned int arg1, int* arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, unsigned int arg1, int* arg2) = NULL;
  static const char name[] = "glGetVertexAttribArrayObjectivATI";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, unsigned int arg1, int* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}
