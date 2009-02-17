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

void Pure_gluBeginCurve(GLUnurbs* arg0)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0) = NULL;
  static const char name[] = "gluBeginCurve";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluBeginPolygon(GLUtesselator* arg0)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0) = NULL;
  static const char name[] = "gluBeginPolygon";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluBeginSurface(GLUnurbs* arg0)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0) = NULL;
  static const char name[] = "gluBeginSurface";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluBeginTrim(GLUnurbs* arg0)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0) = NULL;
  static const char name[] = "gluBeginTrim";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_gluBuild1DMipmapLevels(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, int arg5, int arg6, int arg7, const void* arg8)
{
  static int(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, int arg5, int arg6, int arg7, const void* arg8) = NULL;
  static const char name[] = "gluBuild1DMipmapLevels";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, int arg5, int arg6, int arg7, const void* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

int Pure_gluBuild1DMipmaps(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5)
{
  static int(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5) = NULL;
  static const char name[] = "gluBuild1DMipmaps";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, const void* arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gluBuild2DMipmapLevels(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, int arg6, int arg7, int arg8, const void* arg9)
{
  static int(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, int arg6, int arg7, int arg8, const void* arg9) = NULL;
  static const char name[] = "gluBuild2DMipmapLevels";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, int arg6, int arg7, int arg8, const void* arg9))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

int Pure_gluBuild2DMipmaps(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6)
{
  static int(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6) = NULL;
  static const char name[] = "gluBuild2DMipmaps";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5, const void* arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_gluBuild3DMipmapLevels(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, unsigned int arg5, unsigned int arg6, int arg7, int arg8, int arg9, const void* arg10)
{
  static int(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, unsigned int arg5, unsigned int arg6, int arg7, int arg8, int arg9, const void* arg10) = NULL;
  static const char name[] = "gluBuild3DMipmapLevels";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, unsigned int arg5, unsigned int arg6, int arg7, int arg8, int arg9, const void* arg10))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

int Pure_gluBuild3DMipmaps(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, unsigned int arg5, unsigned int arg6, const void* arg7)
{
  static int(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, unsigned int arg5, unsigned int arg6, const void* arg7) = NULL;
  static const char name[] = "gluBuild3DMipmaps";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, int arg1, int arg2, int arg3, int arg4, unsigned int arg5, unsigned int arg6, const void* arg7))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

unsigned char Pure_gluCheckExtension(const unsigned char* arg0, const unsigned char* arg1)
{
  static unsigned char(APIENTRY*ptr)(const unsigned char* arg0, const unsigned char* arg1) = NULL;
  static const char name[] = "gluCheckExtension";
  if (!ptr) {
    ptr = (unsigned char(APIENTRY*)(const unsigned char* arg0, const unsigned char* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_gluCylinder(GLUquadric* arg0, double arg1, double arg2, double arg3, int arg4, int arg5)
{
  static void(APIENTRY*ptr)(GLUquadric* arg0, double arg1, double arg2, double arg3, int arg4, int arg5) = NULL;
  static const char name[] = "gluCylinder";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUquadric* arg0, double arg1, double arg2, double arg3, int arg4, int arg5))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gluDeleteNurbsRenderer(GLUnurbs* arg0)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0) = NULL;
  static const char name[] = "gluDeleteNurbsRenderer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluDeleteQuadric(GLUquadric* arg0)
{
  static void(APIENTRY*ptr)(GLUquadric* arg0) = NULL;
  static const char name[] = "gluDeleteQuadric";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUquadric* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluDeleteTess(GLUtesselator* arg0)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0) = NULL;
  static const char name[] = "gluDeleteTess";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluDisk(GLUquadric* arg0, double arg1, double arg2, int arg3, int arg4)
{
  static void(APIENTRY*ptr)(GLUquadric* arg0, double arg1, double arg2, int arg3, int arg4) = NULL;
  static const char name[] = "gluDisk";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUquadric* arg0, double arg1, double arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gluEndCurve(GLUnurbs* arg0)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0) = NULL;
  static const char name[] = "gluEndCurve";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluEndPolygon(GLUtesselator* arg0)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0) = NULL;
  static const char name[] = "gluEndPolygon";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluEndSurface(GLUnurbs* arg0)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0) = NULL;
  static const char name[] = "gluEndSurface";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluEndTrim(GLUnurbs* arg0)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0) = NULL;
  static const char name[] = "gluEndTrim";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

const unsigned char* Pure_gluErrorString(unsigned int arg0)
{
  static const unsigned char*(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "gluErrorString";
  if (!ptr) {
    ptr = (const unsigned char*(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluGetNurbsProperty(GLUnurbs* arg0, unsigned int arg1, float* arg2)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0, unsigned int arg1, float* arg2) = NULL;
  static const char name[] = "gluGetNurbsProperty";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0, unsigned int arg1, float* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

const unsigned char* Pure_gluGetString(unsigned int arg0)
{
  static const unsigned char*(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "gluGetString";
  if (!ptr) {
    ptr = (const unsigned char*(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluGetTessProperty(GLUtesselator* arg0, unsigned int arg1, double* arg2)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0, unsigned int arg1, double* arg2) = NULL;
  static const char name[] = "gluGetTessProperty";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0, unsigned int arg1, double* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_gluLoadSamplingMatrices(GLUnurbs* arg0, const float* arg1, const float* arg2, const int* arg3)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0, const float* arg1, const float* arg2, const int* arg3) = NULL;
  static const char name[] = "gluLoadSamplingMatrices";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0, const float* arg1, const float* arg2, const int* arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_gluLookAt(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5, double arg6, double arg7, double arg8)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5, double arg6, double arg7, double arg8) = NULL;
  static const char name[] = "gluLookAt";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5, double arg6, double arg7, double arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

GLUnurbs* Pure_gluNewNurbsRenderer()
{
  static GLUnurbs*(APIENTRY*ptr)() = NULL;
  static const char name[] = "gluNewNurbsRenderer";
  if (!ptr) {
    ptr = (GLUnurbs*(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

GLUquadric* Pure_gluNewQuadric()
{
  static GLUquadric*(APIENTRY*ptr)() = NULL;
  static const char name[] = "gluNewQuadric";
  if (!ptr) {
    ptr = (GLUquadric*(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

GLUtesselator* Pure_gluNewTess()
{
  static GLUtesselator*(APIENTRY*ptr)() = NULL;
  static const char name[] = "gluNewTess";
  if (!ptr) {
    ptr = (GLUtesselator*(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_gluNextContour(GLUtesselator* arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0, unsigned int arg1) = NULL;
  static const char name[] = "gluNextContour";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_gluNurbsCallback(GLUnurbs* arg0, unsigned int arg1, void* arg2)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0, unsigned int arg1, void* arg2) = NULL;
  static const char name[] = "gluNurbsCallback";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0, unsigned int arg1, void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_gluNurbsCallbackData(GLUnurbs* arg0, void* arg1)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0, void* arg1) = NULL;
  static const char name[] = "gluNurbsCallbackData";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0, void* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_gluNurbsCallbackDataEXT(GLUnurbs* arg0, void* arg1)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0, void* arg1) = NULL;
  static const char name[] = "gluNurbsCallbackDataEXT";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0, void* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_gluNurbsCurve(GLUnurbs* arg0, int arg1, float* arg2, int arg3, float* arg4, int arg5, unsigned int arg6)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0, int arg1, float* arg2, int arg3, float* arg4, int arg5, unsigned int arg6) = NULL;
  static const char name[] = "gluNurbsCurve";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0, int arg1, float* arg2, int arg3, float* arg4, int arg5, unsigned int arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gluNurbsProperty(GLUnurbs* arg0, unsigned int arg1, float arg2)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0, unsigned int arg1, float arg2) = NULL;
  static const char name[] = "gluNurbsProperty";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0, unsigned int arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_gluNurbsSurface(GLUnurbs* arg0, int arg1, float* arg2, int arg3, float* arg4, int arg5, int arg6, float* arg7, int arg8, int arg9, unsigned int arg10)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0, int arg1, float* arg2, int arg3, float* arg4, int arg5, int arg6, float* arg7, int arg8, int arg9, unsigned int arg10) = NULL;
  static const char name[] = "gluNurbsSurface";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0, int arg1, float* arg2, int arg3, float* arg4, int arg5, int arg6, float* arg7, int arg8, int arg9, unsigned int arg10))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gluOrtho2D(double arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "gluOrtho2D";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_gluPartialDisk(GLUquadric* arg0, double arg1, double arg2, int arg3, int arg4, double arg5, double arg6)
{
  static void(APIENTRY*ptr)(GLUquadric* arg0, double arg1, double arg2, int arg3, int arg4, double arg5, double arg6) = NULL;
  static const char name[] = "gluPartialDisk";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUquadric* arg0, double arg1, double arg2, int arg3, int arg4, double arg5, double arg6))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gluPerspective(double arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "gluPerspective";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_gluPickMatrix(double arg0, double arg1, double arg2, double arg3, int* arg4)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3, int* arg4) = NULL;
  static const char name[] = "gluPickMatrix";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, double arg2, double arg3, int* arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gluProject(double arg0, double arg1, double arg2, const double* arg3, const double* arg4, const int* arg5, double* arg6, double* arg7, double* arg8)
{
  static int(APIENTRY*ptr)(double arg0, double arg1, double arg2, const double* arg3, const double* arg4, const int* arg5, double* arg6, double* arg7, double* arg8) = NULL;
  static const char name[] = "gluProject";
  if (!ptr) {
    ptr = (int(APIENTRY*)(double arg0, double arg1, double arg2, const double* arg3, const double* arg4, const int* arg5, double* arg6, double* arg7, double* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gluPwlCurve(GLUnurbs* arg0, int arg1, float* arg2, int arg3, unsigned int arg4)
{
  static void(APIENTRY*ptr)(GLUnurbs* arg0, int arg1, float* arg2, int arg3, unsigned int arg4) = NULL;
  static const char name[] = "gluPwlCurve";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUnurbs* arg0, int arg1, float* arg2, int arg3, unsigned int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gluQuadricCallback(GLUquadric* arg0, unsigned int arg1, void* arg2)
{
  static void(APIENTRY*ptr)(GLUquadric* arg0, unsigned int arg1, void* arg2) = NULL;
  static const char name[] = "gluQuadricCallback";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUquadric* arg0, unsigned int arg1, void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_gluQuadricDrawStyle(GLUquadric* arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(GLUquadric* arg0, unsigned int arg1) = NULL;
  static const char name[] = "gluQuadricDrawStyle";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUquadric* arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_gluQuadricNormals(GLUquadric* arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(GLUquadric* arg0, unsigned int arg1) = NULL;
  static const char name[] = "gluQuadricNormals";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUquadric* arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_gluQuadricOrientation(GLUquadric* arg0, unsigned int arg1)
{
  static void(APIENTRY*ptr)(GLUquadric* arg0, unsigned int arg1) = NULL;
  static const char name[] = "gluQuadricOrientation";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUquadric* arg0, unsigned int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_gluQuadricTexture(GLUquadric* arg0, unsigned char arg1)
{
  static void(APIENTRY*ptr)(GLUquadric* arg0, unsigned char arg1) = NULL;
  static const char name[] = "gluQuadricTexture";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUquadric* arg0, unsigned char arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

int Pure_gluScaleImage(unsigned int arg0, int arg1, int arg2, unsigned int arg3, const void* arg4, int arg5, int arg6, unsigned int arg7, void* arg8)
{
  static int(APIENTRY*ptr)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, const void* arg4, int arg5, int arg6, unsigned int arg7, void* arg8) = NULL;
  static const char name[] = "gluScaleImage";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0, int arg1, int arg2, unsigned int arg3, const void* arg4, int arg5, int arg6, unsigned int arg7, void* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gluSphere(GLUquadric* arg0, double arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(GLUquadric* arg0, double arg1, int arg2, int arg3) = NULL;
  static const char name[] = "gluSphere";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUquadric* arg0, double arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_gluTessBeginContour(GLUtesselator* arg0)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0) = NULL;
  static const char name[] = "gluTessBeginContour";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluTessBeginPolygon(GLUtesselator* arg0, void* arg1)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0, void* arg1) = NULL;
  static const char name[] = "gluTessBeginPolygon";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0, void* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_gluTessCallback(GLUtesselator* arg0, unsigned int arg1, void* arg2)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0, unsigned int arg1, void* arg2) = NULL;
  static const char name[] = "gluTessCallback";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0, unsigned int arg1, void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_gluTessEndContour(GLUtesselator* arg0)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0) = NULL;
  static const char name[] = "gluTessEndContour";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluTessEndPolygon(GLUtesselator* arg0)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0) = NULL;
  static const char name[] = "gluTessEndPolygon";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_gluTessNormal(GLUtesselator* arg0, double arg1, double arg2, double arg3)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0, double arg1, double arg2, double arg3) = NULL;
  static const char name[] = "gluTessNormal";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0, double arg1, double arg2, double arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_gluTessProperty(GLUtesselator* arg0, unsigned int arg1, double arg2)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0, unsigned int arg1, double arg2) = NULL;
  static const char name[] = "gluTessProperty";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0, unsigned int arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_gluTessVertex(GLUtesselator* arg0, double* arg1, void* arg2)
{
  static void(APIENTRY*ptr)(GLUtesselator* arg0, double* arg1, void* arg2) = NULL;
  static const char name[] = "gluTessVertex";
  if (!ptr) {
    ptr = (void(APIENTRY*)(GLUtesselator* arg0, double* arg1, void* arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

int Pure_gluUnProject(double arg0, double arg1, double arg2, const double* arg3, const double* arg4, const int* arg5, double* arg6, double* arg7, double* arg8)
{
  static int(APIENTRY*ptr)(double arg0, double arg1, double arg2, const double* arg3, const double* arg4, const int* arg5, double* arg6, double* arg7, double* arg8) = NULL;
  static const char name[] = "gluUnProject";
  if (!ptr) {
    ptr = (int(APIENTRY*)(double arg0, double arg1, double arg2, const double* arg3, const double* arg4, const int* arg5, double* arg6, double* arg7, double* arg8))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

int Pure_gluUnProject4(double arg0, double arg1, double arg2, double arg3, const double* arg4, const double* arg5, const int* arg6, double arg7, double arg8, double* arg9, double* arg10, double* arg11, double* arg12)
{
  static int(APIENTRY*ptr)(double arg0, double arg1, double arg2, double arg3, const double* arg4, const double* arg5, const int* arg6, double arg7, double arg8, double* arg9, double* arg10, double* arg11, double* arg12) = NULL;
  static const char name[] = "gluUnProject4";
  if (!ptr) {
    ptr = (int(APIENTRY*)(double arg0, double arg1, double arg2, double arg3, const double* arg4, const double* arg5, const int* arg6, double arg7, double arg8, double* arg9, double* arg10, double* arg11, double* arg12))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}
