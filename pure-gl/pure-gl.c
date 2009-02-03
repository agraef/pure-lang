#include <stdio.h>
#include <stdlib.h>

#include <pure/runtime.h>

/* Some of this is cribbed from Sven Panne's excellent Haskel OpenGL bindings */

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


#if defined(_WIN32) /* Windows */
#include <GL/gl.h>
#include <GL/wgl.h>
#define get_proc_addr(x) wglGetProcAddress((LPCSTR)(x))
#elif defined(USE_QUARTZ_OPENGL) /* Mac */
#include <OpenGL/gl.h>
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
#include <GL/glx.h>
#define get_proc_addr(x) glXGetProcAddress((const GLubyte*)(x))
#endif


static void throw_unsupported( const char* name )
{
    pure_throw( pure_app( pure_symbol(pure_sym("gl_unsupported")), 
                          pure_cstring_dup(name) ));
}


void PureGL_glAccum(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glAccum";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glAlphaFunc(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glAlphaFunc";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glAreTexturesResident(int arg0, int* arg1, char* arg2) {
  static char(*ptr)(int,int*,char*) = NULL;
  static const char name[] = "glAreTexturesResident";
  if (!ptr) {
    ptr = (char(*)(int,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glArrayElement(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glArrayElement";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBegin(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glBegin";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBindTexture(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBindTexture";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glBitmap(int arg0, int arg1, float arg2, float arg3, float arg4, float arg5, char* arg6) {
  static void(*ptr)(int,int,float,float,float,float,char*) = NULL;
  static const char name[] = "glBitmap";
  if (!ptr) {
    ptr = (void(*)(int,int,float,float,float,float,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glBlendFunc(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBlendFunc";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glCallList(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glCallList";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glCallLists(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glCallLists";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glClear(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glClear";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glClearAccum(float arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(float,float,float,float) = NULL;
  static const char name[] = "glClearAccum";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glClearColor(float arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(float,float,float,float) = NULL;
  static const char name[] = "glClearColor";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glClearDepth(double arg0) {
  static void(*ptr)(double) = NULL;
  static const char name[] = "glClearDepth";
  if (!ptr) {
    ptr = (void(*)(double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glClearIndex(float arg0) {
  static void(*ptr)(float) = NULL;
  static const char name[] = "glClearIndex";
  if (!ptr) {
    ptr = (void(*)(float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glClearStencil(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glClearStencil";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glClipPlane(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glClipPlane";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glColor3b(char arg0, char arg1, char arg2) {
  static void(*ptr)(char,char,char) = NULL;
  static const char name[] = "glColor3b";
  if (!ptr) {
    ptr = (void(*)(char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColor3bv(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glColor3bv";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor3d(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glColor3d";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColor3dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glColor3dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor3f(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glColor3f";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColor3fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glColor3fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor3i(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glColor3i";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColor3iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glColor3iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor3s(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glColor3s";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColor3sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glColor3sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor3ub(char arg0, char arg1, char arg2) {
  static void(*ptr)(char,char,char) = NULL;
  static const char name[] = "glColor3ub";
  if (!ptr) {
    ptr = (void(*)(char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColor3ubv(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glColor3ubv";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor3ui(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glColor3ui";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColor3uiv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glColor3uiv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor3us(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glColor3us";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColor3usv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glColor3usv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor4b(char arg0, char arg1, char arg2, char arg3) {
  static void(*ptr)(char,char,char,char) = NULL;
  static const char name[] = "glColor4b";
  if (!ptr) {
    ptr = (void(*)(char,char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glColor4bv(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glColor4bv";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor4d(double arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(double,double,double,double) = NULL;
  static const char name[] = "glColor4d";
  if (!ptr) {
    ptr = (void(*)(double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glColor4dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glColor4dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor4f(float arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(float,float,float,float) = NULL;
  static const char name[] = "glColor4f";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glColor4fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glColor4fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor4i(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glColor4i";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glColor4iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glColor4iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor4s(short arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(short,short,short,short) = NULL;
  static const char name[] = "glColor4s";
  if (!ptr) {
    ptr = (void(*)(short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glColor4sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glColor4sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor4ub(char arg0, char arg1, char arg2, char arg3) {
  static void(*ptr)(char,char,char,char) = NULL;
  static const char name[] = "glColor4ub";
  if (!ptr) {
    ptr = (void(*)(char,char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glColor4ubv(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glColor4ubv";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor4ui(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glColor4ui";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glColor4uiv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glColor4uiv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor4us(short arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(short,short,short,short) = NULL;
  static const char name[] = "glColor4us";
  if (!ptr) {
    ptr = (void(*)(short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glColor4usv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glColor4usv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColorMask(char arg0, char arg1, char arg2, char arg3) {
  static void(*ptr)(char,char,char,char) = NULL;
  static const char name[] = "glColorMask";
  if (!ptr) {
    ptr = (void(*)(char,char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glColorMaterial(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glColorMaterial";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glColorPointer(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glColorPointer";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glCopyPixels(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glCopyPixels";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glCopyTexImage1D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6) {
  static void(*ptr)(int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyTexImage1D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glCopyTexImage2D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7) {
  static void(*ptr)(int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyTexImage2D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glCopyTexSubImage1D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyTexSubImage1D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glCopyTexSubImage2D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7) {
  static void(*ptr)(int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyTexSubImage2D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glCullFace(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glCullFace";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDeleteLists(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glDeleteLists";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteTextures(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteTextures";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDepthFunc(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDepthFunc";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDepthMask(char arg0) {
  static void(*ptr)(char) = NULL;
  static const char name[] = "glDepthMask";
  if (!ptr) {
    ptr = (void(*)(char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDepthRange(double arg0, double arg1) {
  static void(*ptr)(double,double) = NULL;
  static const char name[] = "glDepthRange";
  if (!ptr) {
    ptr = (void(*)(double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDisable(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDisable";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDisableClientState(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDisableClientState";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDrawArrays(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glDrawArrays";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glDrawBuffer(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDrawBuffer";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDrawElements(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glDrawElements";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glDrawPixels(int arg0, int arg1, int arg2, int arg3, void* arg4) {
  static void(*ptr)(int,int,int,int,void*) = NULL;
  static const char name[] = "glDrawPixels";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glEdgeFlag(char arg0) {
  static void(*ptr)(char) = NULL;
  static const char name[] = "glEdgeFlag";
  if (!ptr) {
    ptr = (void(*)(char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEdgeFlagPointer(int arg0, void* arg1) {
  static void(*ptr)(int,void*) = NULL;
  static const char name[] = "glEdgeFlagPointer";
  if (!ptr) {
    ptr = (void(*)(int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glEdgeFlagv(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glEdgeFlagv";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEnable(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glEnable";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEnableClientState(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glEnableClientState";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEnd() {
  static void(*ptr)() = NULL;
  static const char name[] = "glEnd";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glEndList() {
  static void(*ptr)() = NULL;
  static const char name[] = "glEndList";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glEvalCoord1d(double arg0) {
  static void(*ptr)(double) = NULL;
  static const char name[] = "glEvalCoord1d";
  if (!ptr) {
    ptr = (void(*)(double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEvalCoord1dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glEvalCoord1dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEvalCoord1f(float arg0) {
  static void(*ptr)(float) = NULL;
  static const char name[] = "glEvalCoord1f";
  if (!ptr) {
    ptr = (void(*)(float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEvalCoord1fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glEvalCoord1fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEvalCoord2d(double arg0, double arg1) {
  static void(*ptr)(double,double) = NULL;
  static const char name[] = "glEvalCoord2d";
  if (!ptr) {
    ptr = (void(*)(double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glEvalCoord2dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glEvalCoord2dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEvalCoord2f(float arg0, float arg1) {
  static void(*ptr)(float,float) = NULL;
  static const char name[] = "glEvalCoord2f";
  if (!ptr) {
    ptr = (void(*)(float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glEvalCoord2fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glEvalCoord2fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEvalMesh1(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glEvalMesh1";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glEvalMesh2(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glEvalMesh2";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glEvalPoint1(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glEvalPoint1";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEvalPoint2(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glEvalPoint2";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFeedbackBuffer(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glFeedbackBuffer";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFinish() {
  static void(*ptr)() = NULL;
  static const char name[] = "glFinish";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glFlush() {
  static void(*ptr)() = NULL;
  static const char name[] = "glFlush";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glFogf(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glFogf";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFogfv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glFogfv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFogi(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glFogi";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFogiv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glFogiv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFrontFace(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glFrontFace";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFrustum(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5) {
  static void(*ptr)(double,double,double,double,double,double) = NULL;
  static const char name[] = "glFrustum";
  if (!ptr) {
    ptr = (void(*)(double,double,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

int PureGL_glGenLists(int arg0) {
  static int(*ptr)(int) = NULL;
  static const char name[] = "glGenLists";
  if (!ptr) {
    ptr = (int(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGenTextures(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenTextures";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetBooleanv(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glGetBooleanv";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetClipPlane(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glGetClipPlane";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetDoublev(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glGetDoublev";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glGetError() {
  static int(*ptr)() = NULL;
  static const char name[] = "glGetError";
  if (!ptr) {
    ptr = (int(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glGetFloatv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glGetFloatv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetIntegerv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGetIntegerv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetLightfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetLightfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetLightiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetLightiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMapdv(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glGetMapdv";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMapfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetMapfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMapiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetMapiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMaterialfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetMaterialfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMaterialiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetMaterialiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetPixelMapfv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glGetPixelMapfv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetPixelMapuiv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGetPixelMapuiv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetPixelMapusv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glGetPixelMapusv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetPointerv(int arg0, void** arg1) {
  static void(*ptr)(int,void**) = NULL;
  static const char name[] = "glGetPointerv";
  if (!ptr) {
    ptr = (void(*)(int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetPolygonStipple(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glGetPolygonStipple";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

char* PureGL_glGetString(int arg0) {
  static char*(*ptr)(int) = NULL;
  static const char name[] = "glGetString";
  if (!ptr) {
    ptr = (char*(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGetTexEnvfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetTexEnvfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetTexEnviv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetTexEnviv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetTexGendv(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glGetTexGendv";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetTexGenfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetTexGenfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetTexGeniv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetTexGeniv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetTexImage(int arg0, int arg1, int arg2, int arg3, void* arg4) {
  static void(*ptr)(int,int,int,int,void*) = NULL;
  static const char name[] = "glGetTexImage";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetTexLevelParameterfv(int arg0, int arg1, int arg2, float* arg3) {
  static void(*ptr)(int,int,int,float*) = NULL;
  static const char name[] = "glGetTexLevelParameterfv";
  if (!ptr) {
    ptr = (void(*)(int,int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetTexLevelParameteriv(int arg0, int arg1, int arg2, int* arg3) {
  static void(*ptr)(int,int,int,int*) = NULL;
  static const char name[] = "glGetTexLevelParameteriv";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetTexParameterfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetTexParameterfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetTexParameteriv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetTexParameteriv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glHint(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glHint";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glIndexMask(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glIndexMask";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glIndexPointer(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glIndexPointer";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glIndexd(double arg0) {
  static void(*ptr)(double) = NULL;
  static const char name[] = "glIndexd";
  if (!ptr) {
    ptr = (void(*)(double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glIndexdv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glIndexdv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glIndexf(float arg0) {
  static void(*ptr)(float) = NULL;
  static const char name[] = "glIndexf";
  if (!ptr) {
    ptr = (void(*)(float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glIndexfv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glIndexfv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glIndexi(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glIndexi";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glIndexiv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glIndexiv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glIndexs(short arg0) {
  static void(*ptr)(short) = NULL;
  static const char name[] = "glIndexs";
  if (!ptr) {
    ptr = (void(*)(short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glIndexsv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glIndexsv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glIndexub(char arg0) {
  static void(*ptr)(char) = NULL;
  static const char name[] = "glIndexub";
  if (!ptr) {
    ptr = (void(*)(char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glIndexubv(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glIndexubv";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glInitNames() {
  static void(*ptr)() = NULL;
  static const char name[] = "glInitNames";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glInterleavedArrays(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glInterleavedArrays";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

char PureGL_glIsEnabled(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsEnabled";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

char PureGL_glIsList(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsList";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

char PureGL_glIsTexture(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsTexture";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glLightModelf(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glLightModelf";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glLightModelfv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glLightModelfv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glLightModeli(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glLightModeli";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glLightModeliv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glLightModeliv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glLightf(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glLightf";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glLightfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glLightfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glLighti(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glLighti";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glLightiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glLightiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glLineStipple(int arg0, short arg1) {
  static void(*ptr)(int,short) = NULL;
  static const char name[] = "glLineStipple";
  if (!ptr) {
    ptr = (void(*)(int,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glLineWidth(float arg0) {
  static void(*ptr)(float) = NULL;
  static const char name[] = "glLineWidth";
  if (!ptr) {
    ptr = (void(*)(float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glListBase(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glListBase";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glLoadIdentity() {
  static void(*ptr)() = NULL;
  static const char name[] = "glLoadIdentity";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glLoadMatrixd(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glLoadMatrixd";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glLoadMatrixf(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glLoadMatrixf";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glLoadName(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glLoadName";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glLogicOp(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glLogicOp";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMap1d(int arg0, double arg1, double arg2, int arg3, int arg4, double* arg5) {
  static void(*ptr)(int,double,double,int,int,double*) = NULL;
  static const char name[] = "glMap1d";
  if (!ptr) {
    ptr = (void(*)(int,double,double,int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glMap1f(int arg0, float arg1, float arg2, int arg3, int arg4, float* arg5) {
  static void(*ptr)(int,float,float,int,int,float*) = NULL;
  static const char name[] = "glMap1f";
  if (!ptr) {
    ptr = (void(*)(int,float,float,int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glMap2d(int arg0, double arg1, double arg2, int arg3, int arg4, double arg5, double arg6, int arg7, int arg8, double* arg9) {
  static void(*ptr)(int,double,double,int,int,double,double,int,int,double*) = NULL;
  static const char name[] = "glMap2d";
  if (!ptr) {
    ptr = (void(*)(int,double,double,int,int,double,double,int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
}

void PureGL_glMap2f(int arg0, float arg1, float arg2, int arg3, int arg4, float arg5, float arg6, int arg7, int arg8, float* arg9) {
  static void(*ptr)(int,float,float,int,int,float,float,int,int,float*) = NULL;
  static const char name[] = "glMap2f";
  if (!ptr) {
    ptr = (void(*)(int,float,float,int,int,float,float,int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
}

void PureGL_glMapGrid1d(int arg0, double arg1, double arg2) {
  static void(*ptr)(int,double,double) = NULL;
  static const char name[] = "glMapGrid1d";
  if (!ptr) {
    ptr = (void(*)(int,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMapGrid1f(int arg0, float arg1, float arg2) {
  static void(*ptr)(int,float,float) = NULL;
  static const char name[] = "glMapGrid1f";
  if (!ptr) {
    ptr = (void(*)(int,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMapGrid2d(int arg0, double arg1, double arg2, int arg3, double arg4, double arg5) {
  static void(*ptr)(int,double,double,int,double,double) = NULL;
  static const char name[] = "glMapGrid2d";
  if (!ptr) {
    ptr = (void(*)(int,double,double,int,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glMapGrid2f(int arg0, float arg1, float arg2, int arg3, float arg4, float arg5) {
  static void(*ptr)(int,float,float,int,float,float) = NULL;
  static const char name[] = "glMapGrid2f";
  if (!ptr) {
    ptr = (void(*)(int,float,float,int,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glMaterialf(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glMaterialf";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMaterialfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glMaterialfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMateriali(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glMateriali";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMaterialiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glMaterialiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMatrixMode(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glMatrixMode";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMultMatrixd(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glMultMatrixd";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMultMatrixf(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glMultMatrixf";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glNewList(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glNewList";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glNormal3b(char arg0, char arg1, char arg2) {
  static void(*ptr)(char,char,char) = NULL;
  static const char name[] = "glNormal3b";
  if (!ptr) {
    ptr = (void(*)(char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glNormal3bv(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glNormal3bv";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glNormal3d(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glNormal3d";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glNormal3dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glNormal3dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glNormal3f(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glNormal3f";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glNormal3fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glNormal3fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glNormal3i(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glNormal3i";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glNormal3iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glNormal3iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glNormal3s(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glNormal3s";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glNormal3sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glNormal3sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glNormalPointer(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glNormalPointer";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glOrtho(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5) {
  static void(*ptr)(double,double,double,double,double,double) = NULL;
  static const char name[] = "glOrtho";
  if (!ptr) {
    ptr = (void(*)(double,double,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glPassThrough(float arg0) {
  static void(*ptr)(float) = NULL;
  static const char name[] = "glPassThrough";
  if (!ptr) {
    ptr = (void(*)(float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glPixelMapfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glPixelMapfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPixelMapuiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glPixelMapuiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPixelMapusv(int arg0, int arg1, short* arg2) {
  static void(*ptr)(int,int,short*) = NULL;
  static const char name[] = "glPixelMapusv";
  if (!ptr) {
    ptr = (void(*)(int,int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPixelStoref(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glPixelStoref";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPixelStorei(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glPixelStorei";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPixelTransferf(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glPixelTransferf";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPixelTransferi(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glPixelTransferi";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPixelZoom(float arg0, float arg1) {
  static void(*ptr)(float,float) = NULL;
  static const char name[] = "glPixelZoom";
  if (!ptr) {
    ptr = (void(*)(float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPointSize(float arg0) {
  static void(*ptr)(float) = NULL;
  static const char name[] = "glPointSize";
  if (!ptr) {
    ptr = (void(*)(float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glPolygonMode(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glPolygonMode";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPolygonOffset(float arg0, float arg1) {
  static void(*ptr)(float,float) = NULL;
  static const char name[] = "glPolygonOffset";
  if (!ptr) {
    ptr = (void(*)(float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPolygonStipple(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glPolygonStipple";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glPopAttrib() {
  static void(*ptr)() = NULL;
  static const char name[] = "glPopAttrib";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glPopClientAttrib() {
  static void(*ptr)() = NULL;
  static const char name[] = "glPopClientAttrib";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glPopMatrix() {
  static void(*ptr)() = NULL;
  static const char name[] = "glPopMatrix";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glPopName() {
  static void(*ptr)() = NULL;
  static const char name[] = "glPopName";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glPrioritizeTextures(int arg0, int* arg1, float* arg2) {
  static void(*ptr)(int,int*,float*) = NULL;
  static const char name[] = "glPrioritizeTextures";
  if (!ptr) {
    ptr = (void(*)(int,int*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPushAttrib(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glPushAttrib";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glPushClientAttrib(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glPushClientAttrib";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glPushMatrix() {
  static void(*ptr)() = NULL;
  static const char name[] = "glPushMatrix";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glPushName(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glPushName";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos2d(double arg0, double arg1) {
  static void(*ptr)(double,double) = NULL;
  static const char name[] = "glRasterPos2d";
  if (!ptr) {
    ptr = (void(*)(double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glRasterPos2dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glRasterPos2dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos2f(float arg0, float arg1) {
  static void(*ptr)(float,float) = NULL;
  static const char name[] = "glRasterPos2f";
  if (!ptr) {
    ptr = (void(*)(float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glRasterPos2fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glRasterPos2fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos2i(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glRasterPos2i";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glRasterPos2iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glRasterPos2iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos2s(short arg0, short arg1) {
  static void(*ptr)(short,short) = NULL;
  static const char name[] = "glRasterPos2s";
  if (!ptr) {
    ptr = (void(*)(short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glRasterPos2sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glRasterPos2sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos3d(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glRasterPos3d";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glRasterPos3dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glRasterPos3dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos3f(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glRasterPos3f";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glRasterPos3fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glRasterPos3fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos3i(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glRasterPos3i";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glRasterPos3iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glRasterPos3iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos3s(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glRasterPos3s";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glRasterPos3sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glRasterPos3sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos4d(double arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(double,double,double,double) = NULL;
  static const char name[] = "glRasterPos4d";
  if (!ptr) {
    ptr = (void(*)(double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glRasterPos4dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glRasterPos4dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos4f(float arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(float,float,float,float) = NULL;
  static const char name[] = "glRasterPos4f";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glRasterPos4fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glRasterPos4fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos4i(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glRasterPos4i";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glRasterPos4iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glRasterPos4iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRasterPos4s(short arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(short,short,short,short) = NULL;
  static const char name[] = "glRasterPos4s";
  if (!ptr) {
    ptr = (void(*)(short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glRasterPos4sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glRasterPos4sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glReadBuffer(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glReadBuffer";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glReadPixels(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, void* arg6) {
  static void(*ptr)(int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glReadPixels";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glRectd(double arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(double,double,double,double) = NULL;
  static const char name[] = "glRectd";
  if (!ptr) {
    ptr = (void(*)(double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glRectdv(double* arg0, double* arg1) {
  static void(*ptr)(double*,double*) = NULL;
  static const char name[] = "glRectdv";
  if (!ptr) {
    ptr = (void(*)(double*,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glRectf(float arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(float,float,float,float) = NULL;
  static const char name[] = "glRectf";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glRectfv(float* arg0, float* arg1) {
  static void(*ptr)(float*,float*) = NULL;
  static const char name[] = "glRectfv";
  if (!ptr) {
    ptr = (void(*)(float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glRecti(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glRecti";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glRectiv(int* arg0, int* arg1) {
  static void(*ptr)(int*,int*) = NULL;
  static const char name[] = "glRectiv";
  if (!ptr) {
    ptr = (void(*)(int*,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glRects(short arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(short,short,short,short) = NULL;
  static const char name[] = "glRects";
  if (!ptr) {
    ptr = (void(*)(short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glRectsv(short* arg0, short* arg1) {
  static void(*ptr)(short*,short*) = NULL;
  static const char name[] = "glRectsv";
  if (!ptr) {
    ptr = (void(*)(short*,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glRenderMode(int arg0) {
  static int(*ptr)(int) = NULL;
  static const char name[] = "glRenderMode";
  if (!ptr) {
    ptr = (int(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glRotated(double arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(double,double,double,double) = NULL;
  static const char name[] = "glRotated";
  if (!ptr) {
    ptr = (void(*)(double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glRotatef(float arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(float,float,float,float) = NULL;
  static const char name[] = "glRotatef";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glScaled(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glScaled";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glScalef(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glScalef";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glScissor(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glScissor";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glSelectBuffer(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glSelectBuffer";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glShadeModel(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glShadeModel";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glStencilFunc(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glStencilFunc";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glStencilMask(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glStencilMask";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glStencilOp(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glStencilOp";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexCoord1d(double arg0) {
  static void(*ptr)(double) = NULL;
  static const char name[] = "glTexCoord1d";
  if (!ptr) {
    ptr = (void(*)(double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord1dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glTexCoord1dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord1f(float arg0) {
  static void(*ptr)(float) = NULL;
  static const char name[] = "glTexCoord1f";
  if (!ptr) {
    ptr = (void(*)(float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord1fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glTexCoord1fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord1i(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glTexCoord1i";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord1iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glTexCoord1iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord1s(short arg0) {
  static void(*ptr)(short) = NULL;
  static const char name[] = "glTexCoord1s";
  if (!ptr) {
    ptr = (void(*)(short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord1sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glTexCoord1sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord2d(double arg0, double arg1) {
  static void(*ptr)(double,double) = NULL;
  static const char name[] = "glTexCoord2d";
  if (!ptr) {
    ptr = (void(*)(double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTexCoord2dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glTexCoord2dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord2f(float arg0, float arg1) {
  static void(*ptr)(float,float) = NULL;
  static const char name[] = "glTexCoord2f";
  if (!ptr) {
    ptr = (void(*)(float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTexCoord2fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glTexCoord2fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord2i(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glTexCoord2i";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTexCoord2iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glTexCoord2iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord2s(short arg0, short arg1) {
  static void(*ptr)(short,short) = NULL;
  static const char name[] = "glTexCoord2s";
  if (!ptr) {
    ptr = (void(*)(short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTexCoord2sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glTexCoord2sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord3d(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glTexCoord3d";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexCoord3dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glTexCoord3dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord3f(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glTexCoord3f";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexCoord3fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glTexCoord3fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord3i(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glTexCoord3i";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexCoord3iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glTexCoord3iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord3s(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glTexCoord3s";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexCoord3sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glTexCoord3sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord4d(double arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(double,double,double,double) = NULL;
  static const char name[] = "glTexCoord4d";
  if (!ptr) {
    ptr = (void(*)(double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glTexCoord4dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glTexCoord4dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord4f(float arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(float,float,float,float) = NULL;
  static const char name[] = "glTexCoord4f";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glTexCoord4fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glTexCoord4fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord4i(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glTexCoord4i";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glTexCoord4iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glTexCoord4iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord4s(short arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(short,short,short,short) = NULL;
  static const char name[] = "glTexCoord4s";
  if (!ptr) {
    ptr = (void(*)(short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glTexCoord4sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glTexCoord4sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoordPointer(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glTexCoordPointer";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glTexEnvf(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glTexEnvf";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexEnvfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glTexEnvfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexEnvi(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glTexEnvi";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexEnviv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glTexEnviv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexGend(int arg0, int arg1, double arg2) {
  static void(*ptr)(int,int,double) = NULL;
  static const char name[] = "glTexGend";
  if (!ptr) {
    ptr = (void(*)(int,int,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexGendv(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glTexGendv";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexGenf(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glTexGenf";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexGenfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glTexGenfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexGeni(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glTexGeni";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexGeniv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glTexGeniv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexImage1D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, void* arg7) {
  static void(*ptr)(int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexImage1D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glTexImage2D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, void* arg8) {
  static void(*ptr)(int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexImage2D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glTexParameterf(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glTexParameterf";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexParameterfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glTexParameterfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexParameteri(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glTexParameteri";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexParameteriv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glTexParameteriv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexSubImage1D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, void* arg6) {
  static void(*ptr)(int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexSubImage1D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glTexSubImage2D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, void* arg8) {
  static void(*ptr)(int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexSubImage2D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glTranslated(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glTranslated";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTranslatef(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glTranslatef";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertex2d(double arg0, double arg1) {
  static void(*ptr)(double,double) = NULL;
  static const char name[] = "glVertex2d";
  if (!ptr) {
    ptr = (void(*)(double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertex2dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glVertex2dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex2f(float arg0, float arg1) {
  static void(*ptr)(float,float) = NULL;
  static const char name[] = "glVertex2f";
  if (!ptr) {
    ptr = (void(*)(float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertex2fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glVertex2fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex2i(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glVertex2i";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertex2iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glVertex2iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex2s(short arg0, short arg1) {
  static void(*ptr)(short,short) = NULL;
  static const char name[] = "glVertex2s";
  if (!ptr) {
    ptr = (void(*)(short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertex2sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glVertex2sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex3d(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glVertex3d";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertex3dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glVertex3dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex3f(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glVertex3f";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertex3fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glVertex3fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex3i(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glVertex3i";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertex3iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glVertex3iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex3s(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glVertex3s";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertex3sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glVertex3sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex4d(double arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(double,double,double,double) = NULL;
  static const char name[] = "glVertex4d";
  if (!ptr) {
    ptr = (void(*)(double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertex4dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glVertex4dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex4f(float arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(float,float,float,float) = NULL;
  static const char name[] = "glVertex4f";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertex4fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glVertex4fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex4i(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glVertex4i";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertex4iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glVertex4iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex4s(short arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(short,short,short,short) = NULL;
  static const char name[] = "glVertex4s";
  if (!ptr) {
    ptr = (void(*)(short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertex4sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glVertex4sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertexPointer(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glVertexPointer";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glViewport(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glViewport";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glBlendColor(float arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(float,float,float,float) = NULL;
  static const char name[] = "glBlendColor";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glBlendEquation(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glBlendEquation";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDrawRangeElements(int arg0, int arg1, int arg2, int arg3, int arg4, void* arg5) {
  static void(*ptr)(int,int,int,int,int,void*) = NULL;
  static const char name[] = "glDrawRangeElements";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glColorTable(int arg0, int arg1, int arg2, int arg3, int arg4, void* arg5) {
  static void(*ptr)(int,int,int,int,int,void*) = NULL;
  static const char name[] = "glColorTable";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glColorTableParameterfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glColorTableParameterfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColorTableParameteriv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glColorTableParameteriv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glCopyColorTable(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glCopyColorTable";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetColorTable(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glGetColorTable";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetColorTableParameterfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetColorTableParameterfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetColorTableParameteriv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetColorTableParameteriv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColorSubTable(int arg0, int arg1, int arg2, int arg3, int arg4, void* arg5) {
  static void(*ptr)(int,int,int,int,int,void*) = NULL;
  static const char name[] = "glColorSubTable";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glCopyColorSubTable(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glCopyColorSubTable";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glConvolutionFilter1D(int arg0, int arg1, int arg2, int arg3, int arg4, void* arg5) {
  static void(*ptr)(int,int,int,int,int,void*) = NULL;
  static const char name[] = "glConvolutionFilter1D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glConvolutionFilter2D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, void* arg6) {
  static void(*ptr)(int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glConvolutionFilter2D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glConvolutionParameterf(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glConvolutionParameterf";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glConvolutionParameterfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glConvolutionParameterfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glConvolutionParameteri(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glConvolutionParameteri";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glConvolutionParameteriv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glConvolutionParameteriv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glCopyConvolutionFilter1D(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glCopyConvolutionFilter1D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glCopyConvolutionFilter2D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyConvolutionFilter2D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glGetConvolutionFilter(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glGetConvolutionFilter";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetConvolutionParameterfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetConvolutionParameterfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetConvolutionParameteriv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetConvolutionParameteriv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetSeparableFilter(int arg0, int arg1, int arg2, void* arg3, void* arg4, void* arg5) {
  static void(*ptr)(int,int,int,void*,void*,void*) = NULL;
  static const char name[] = "glGetSeparableFilter";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*,void*,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glSeparableFilter2D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, void* arg6, void* arg7) {
  static void(*ptr)(int,int,int,int,int,int,void*,void*) = NULL;
  static const char name[] = "glSeparableFilter2D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,void*,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glGetHistogram(int arg0, char arg1, int arg2, int arg3, void* arg4) {
  static void(*ptr)(int,char,int,int,void*) = NULL;
  static const char name[] = "glGetHistogram";
  if (!ptr) {
    ptr = (void(*)(int,char,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetHistogramParameterfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetHistogramParameterfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetHistogramParameteriv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetHistogramParameteriv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMinmax(int arg0, char arg1, int arg2, int arg3, void* arg4) {
  static void(*ptr)(int,char,int,int,void*) = NULL;
  static const char name[] = "glGetMinmax";
  if (!ptr) {
    ptr = (void(*)(int,char,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetMinmaxParameterfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetMinmaxParameterfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMinmaxParameteriv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetMinmaxParameteriv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glHistogram(int arg0, int arg1, int arg2, char arg3) {
  static void(*ptr)(int,int,int,char) = NULL;
  static const char name[] = "glHistogram";
  if (!ptr) {
    ptr = (void(*)(int,int,int,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMinmax(int arg0, int arg1, char arg2) {
  static void(*ptr)(int,int,char) = NULL;
  static const char name[] = "glMinmax";
  if (!ptr) {
    ptr = (void(*)(int,int,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glResetHistogram(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glResetHistogram";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glResetMinmax(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glResetMinmax";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexImage3D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, void* arg9) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexImage3D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
}

void PureGL_glTexSubImage3D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9, void* arg10) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexSubImage3D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10);
}

void PureGL_glCopyTexSubImage3D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyTexSubImage3D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glActiveTexture(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glActiveTexture";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glClientActiveTexture(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glClientActiveTexture";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMultiTexCoord1d(int arg0, double arg1) {
  static void(*ptr)(int,double) = NULL;
  static const char name[] = "glMultiTexCoord1d";
  if (!ptr) {
    ptr = (void(*)(int,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1dv(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glMultiTexCoord1dv";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1f(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glMultiTexCoord1f";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1fv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glMultiTexCoord1fv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1i(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glMultiTexCoord1i";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1iv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glMultiTexCoord1iv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1s(int arg0, short arg1) {
  static void(*ptr)(int,short) = NULL;
  static const char name[] = "glMultiTexCoord1s";
  if (!ptr) {
    ptr = (void(*)(int,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1sv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord1sv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord2d(int arg0, double arg1, double arg2) {
  static void(*ptr)(int,double,double) = NULL;
  static const char name[] = "glMultiTexCoord2d";
  if (!ptr) {
    ptr = (void(*)(int,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMultiTexCoord2dv(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glMultiTexCoord2dv";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord2f(int arg0, float arg1, float arg2) {
  static void(*ptr)(int,float,float) = NULL;
  static const char name[] = "glMultiTexCoord2f";
  if (!ptr) {
    ptr = (void(*)(int,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMultiTexCoord2fv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glMultiTexCoord2fv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord2i(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glMultiTexCoord2i";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMultiTexCoord2iv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glMultiTexCoord2iv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord2s(int arg0, short arg1, short arg2) {
  static void(*ptr)(int,short,short) = NULL;
  static const char name[] = "glMultiTexCoord2s";
  if (!ptr) {
    ptr = (void(*)(int,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMultiTexCoord2sv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord2sv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord3d(int arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(int,double,double,double) = NULL;
  static const char name[] = "glMultiTexCoord3d";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiTexCoord3dv(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glMultiTexCoord3dv";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord3f(int arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(int,float,float,float) = NULL;
  static const char name[] = "glMultiTexCoord3f";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiTexCoord3fv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glMultiTexCoord3fv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord3i(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glMultiTexCoord3i";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiTexCoord3iv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glMultiTexCoord3iv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord3s(int arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(int,short,short,short) = NULL;
  static const char name[] = "glMultiTexCoord3s";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiTexCoord3sv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord3sv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord4d(int arg0, double arg1, double arg2, double arg3, double arg4) {
  static void(*ptr)(int,double,double,double,double) = NULL;
  static const char name[] = "glMultiTexCoord4d";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glMultiTexCoord4dv(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glMultiTexCoord4dv";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord4f(int arg0, float arg1, float arg2, float arg3, float arg4) {
  static void(*ptr)(int,float,float,float,float) = NULL;
  static const char name[] = "glMultiTexCoord4f";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glMultiTexCoord4fv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glMultiTexCoord4fv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord4i(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glMultiTexCoord4i";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glMultiTexCoord4iv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glMultiTexCoord4iv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord4s(int arg0, short arg1, short arg2, short arg3, short arg4) {
  static void(*ptr)(int,short,short,short,short) = NULL;
  static const char name[] = "glMultiTexCoord4s";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glMultiTexCoord4sv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord4sv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glLoadTransposeMatrixf(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glLoadTransposeMatrixf";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glLoadTransposeMatrixd(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glLoadTransposeMatrixd";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMultTransposeMatrixf(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glMultTransposeMatrixf";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMultTransposeMatrixd(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glMultTransposeMatrixd";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSampleCoverage(float arg0, char arg1) {
  static void(*ptr)(float,char) = NULL;
  static const char name[] = "glSampleCoverage";
  if (!ptr) {
    ptr = (void(*)(float,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glCompressedTexImage3D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, void* arg8) {
  static void(*ptr)(int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexImage3D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glCompressedTexImage2D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, void* arg7) {
  static void(*ptr)(int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexImage2D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glCompressedTexImage1D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, void* arg6) {
  static void(*ptr)(int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexImage1D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glCompressedTexSubImage3D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9, void* arg10) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexSubImage3D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10);
}

void PureGL_glCompressedTexSubImage2D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, void* arg8) {
  static void(*ptr)(int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexSubImage2D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glCompressedTexSubImage1D(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, void* arg6) {
  static void(*ptr)(int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexSubImage1D";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glGetCompressedTexImage(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glGetCompressedTexImage";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBlendFuncSeparate(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glBlendFuncSeparate";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glFogCoordf(float arg0) {
  static void(*ptr)(float) = NULL;
  static const char name[] = "glFogCoordf";
  if (!ptr) {
    ptr = (void(*)(float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFogCoordfv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glFogCoordfv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFogCoordd(double arg0) {
  static void(*ptr)(double) = NULL;
  static const char name[] = "glFogCoordd";
  if (!ptr) {
    ptr = (void(*)(double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFogCoorddv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glFogCoorddv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFogCoordPointer(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glFogCoordPointer";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMultiDrawArrays(int arg0, int* arg1, int* arg2, int arg3) {
  static void(*ptr)(int,int*,int*,int) = NULL;
  static const char name[] = "glMultiDrawArrays";
  if (!ptr) {
    ptr = (void(*)(int,int*,int*,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiDrawElements(int arg0, int* arg1, int arg2, void** arg3, int arg4) {
  static void(*ptr)(int,int*,int,void**,int) = NULL;
  static const char name[] = "glMultiDrawElements";
  if (!ptr) {
    ptr = (void(*)(int,int*,int,void**,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glPointParameterf(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glPointParameterf";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPointParameterfv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glPointParameterfv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPointParameteri(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glPointParameteri";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPointParameteriv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glPointParameteriv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glSecondaryColor3b(char arg0, char arg1, char arg2) {
  static void(*ptr)(char,char,char) = NULL;
  static const char name[] = "glSecondaryColor3b";
  if (!ptr) {
    ptr = (void(*)(char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3bv(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glSecondaryColor3bv";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3d(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glSecondaryColor3d";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glSecondaryColor3dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3f(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glSecondaryColor3f";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glSecondaryColor3fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3i(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glSecondaryColor3i";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glSecondaryColor3iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3s(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glSecondaryColor3s";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glSecondaryColor3sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3ub(char arg0, char arg1, char arg2) {
  static void(*ptr)(char,char,char) = NULL;
  static const char name[] = "glSecondaryColor3ub";
  if (!ptr) {
    ptr = (void(*)(char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3ubv(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glSecondaryColor3ubv";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3ui(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glSecondaryColor3ui";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3uiv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glSecondaryColor3uiv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3us(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glSecondaryColor3us";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3usv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glSecondaryColor3usv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColorPointer(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glSecondaryColorPointer";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glWindowPos2d(double arg0, double arg1) {
  static void(*ptr)(double,double) = NULL;
  static const char name[] = "glWindowPos2d";
  if (!ptr) {
    ptr = (void(*)(double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glWindowPos2dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos2f(float arg0, float arg1) {
  static void(*ptr)(float,float) = NULL;
  static const char name[] = "glWindowPos2f";
  if (!ptr) {
    ptr = (void(*)(float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glWindowPos2fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos2i(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glWindowPos2i";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glWindowPos2iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos2s(short arg0, short arg1) {
  static void(*ptr)(short,short) = NULL;
  static const char name[] = "glWindowPos2s";
  if (!ptr) {
    ptr = (void(*)(short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glWindowPos2sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3d(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glWindowPos3d";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3dv(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glWindowPos3dv";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3f(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glWindowPos3f";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3fv(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glWindowPos3fv";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3i(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glWindowPos3i";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3iv(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glWindowPos3iv";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3s(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glWindowPos3s";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3sv(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glWindowPos3sv";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGenQueries(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenQueries";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteQueries(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteQueries";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glIsQuery(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsQuery";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBeginQuery(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBeginQuery";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glEndQuery(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glEndQuery";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGetQueryiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetQueryiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetQueryObjectiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetQueryObjectiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetQueryObjectuiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetQueryObjectuiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBindBuffer(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBindBuffer";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteBuffers(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteBuffers";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGenBuffers(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenBuffers";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glIsBuffer(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsBuffer";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBufferData(int arg0, int arg1, void* arg2, int arg3) {
  static void(*ptr)(int,int,void*,int) = NULL;
  static const char name[] = "glBufferData";
  if (!ptr) {
    ptr = (void(*)(int,int,void*,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glBufferSubData(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glBufferSubData";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetBufferSubData(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glGetBufferSubData";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void* PureGL_glMapBuffer(int arg0, int arg1) {
  static void*(*ptr)(int,int) = NULL;
  static const char name[] = "glMapBuffer";
  if (!ptr) {
    ptr = (void*(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glUnmapBuffer(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glUnmapBuffer";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGetBufferParameteriv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetBufferParameteriv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetBufferPointerv(int arg0, int arg1, void** arg2) {
  static void(*ptr)(int,int,void**) = NULL;
  static const char name[] = "glGetBufferPointerv";
  if (!ptr) {
    ptr = (void(*)(int,int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBlendEquationSeparate(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBlendEquationSeparate";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDrawBuffers(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDrawBuffers";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glStencilOpSeparate(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glStencilOpSeparate";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glStencilFuncSeparate(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glStencilFuncSeparate";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glStencilMaskSeparate(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glStencilMaskSeparate";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glAttachShader(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glAttachShader";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glBindAttribLocation(int arg0, int arg1, char* arg2) {
  static void(*ptr)(int,int,char*) = NULL;
  static const char name[] = "glBindAttribLocation";
  if (!ptr) {
    ptr = (void(*)(int,int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glCompileShader(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glCompileShader";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int PureGL_glCreateProgram() {
  static int(*ptr)() = NULL;
  static const char name[] = "glCreateProgram";
  if (!ptr) {
    ptr = (int(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

int PureGL_glCreateShader(int arg0) {
  static int(*ptr)(int) = NULL;
  static const char name[] = "glCreateShader";
  if (!ptr) {
    ptr = (int(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDeleteProgram(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDeleteProgram";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDeleteShader(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDeleteShader";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDetachShader(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glDetachShader";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDisableVertexAttribArray(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDisableVertexAttribArray";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEnableVertexAttribArray(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glEnableVertexAttribArray";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGetActiveAttrib(int arg0, int arg1, int arg2, int* arg3, int* arg4, int* arg5, char* arg6) {
  static void(*ptr)(int,int,int,int*,int*,int*,char*) = NULL;
  static const char name[] = "glGetActiveAttrib";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*,int*,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glGetActiveUniform(int arg0, int arg1, int arg2, int* arg3, int* arg4, int* arg5, char* arg6) {
  static void(*ptr)(int,int,int,int*,int*,int*,char*) = NULL;
  static const char name[] = "glGetActiveUniform";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*,int*,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glGetAttachedShaders(int arg0, int arg1, int* arg2, int* arg3) {
  static void(*ptr)(int,int,int*,int*) = NULL;
  static const char name[] = "glGetAttachedShaders";
  if (!ptr) {
    ptr = (void(*)(int,int,int*,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

int PureGL_glGetAttribLocation(int arg0, char* arg1) {
  static int(*ptr)(int,char*) = NULL;
  static const char name[] = "glGetAttribLocation";
  if (!ptr) {
    ptr = (int(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetProgramiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetProgramiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetProgramInfoLog(int arg0, int arg1, int* arg2, char* arg3) {
  static void(*ptr)(int,int,int*,char*) = NULL;
  static const char name[] = "glGetProgramInfoLog";
  if (!ptr) {
    ptr = (void(*)(int,int,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetShaderiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetShaderiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetShaderInfoLog(int arg0, int arg1, int* arg2, char* arg3) {
  static void(*ptr)(int,int,int*,char*) = NULL;
  static const char name[] = "glGetShaderInfoLog";
  if (!ptr) {
    ptr = (void(*)(int,int,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetShaderSource(int arg0, int arg1, int* arg2, char* arg3) {
  static void(*ptr)(int,int,int*,char*) = NULL;
  static const char name[] = "glGetShaderSource";
  if (!ptr) {
    ptr = (void(*)(int,int,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

int PureGL_glGetUniformLocation(int arg0, char* arg1) {
  static int(*ptr)(int,char*) = NULL;
  static const char name[] = "glGetUniformLocation";
  if (!ptr) {
    ptr = (int(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetUniformfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetUniformfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetUniformiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetUniformiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribdv(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glGetVertexAttribdv";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribfv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetVertexAttribfv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribiv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetVertexAttribiv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribPointerv(int arg0, int arg1, void** arg2) {
  static void(*ptr)(int,int,void**) = NULL;
  static const char name[] = "glGetVertexAttribPointerv";
  if (!ptr) {
    ptr = (void(*)(int,int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

char PureGL_glIsProgram(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsProgram";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

char PureGL_glIsShader(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsShader";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glLinkProgram(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glLinkProgram";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glShaderSource(int arg0, int arg1, char** arg2, int* arg3) {
  static void(*ptr)(int,int,char**,int*) = NULL;
  static const char name[] = "glShaderSource";
  if (!ptr) {
    ptr = (void(*)(int,int,char**,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUseProgram(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glUseProgram";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glUniform1f(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glUniform1f";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glUniform2f(int arg0, float arg1, float arg2) {
  static void(*ptr)(int,float,float) = NULL;
  static const char name[] = "glUniform2f";
  if (!ptr) {
    ptr = (void(*)(int,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform3f(int arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(int,float,float,float) = NULL;
  static const char name[] = "glUniform3f";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniform4f(int arg0, float arg1, float arg2, float arg3, float arg4) {
  static void(*ptr)(int,float,float,float,float) = NULL;
  static const char name[] = "glUniform4f";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glUniform1i(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glUniform1i";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glUniform2i(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glUniform2i";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform3i(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glUniform3i";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniform4i(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glUniform4i";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glUniform1fv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glUniform1fv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform2fv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glUniform2fv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform3fv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glUniform3fv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform4fv(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glUniform4fv";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform1iv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform1iv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform2iv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform2iv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform3iv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform3iv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform4iv(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform4iv";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniformMatrix2fv(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix2fv";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniformMatrix3fv(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix3fv";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniformMatrix4fv(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix4fv";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glValidateProgram(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glValidateProgram";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertexAttrib1d(int arg0, double arg1) {
  static void(*ptr)(int,double) = NULL;
  static const char name[] = "glVertexAttrib1d";
  if (!ptr) {
    ptr = (void(*)(int,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1dv(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib1dv";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1f(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glVertexAttrib1f";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1fv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib1fv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1s(int arg0, short arg1) {
  static void(*ptr)(int,short) = NULL;
  static const char name[] = "glVertexAttrib1s";
  if (!ptr) {
    ptr = (void(*)(int,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1sv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib1sv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib2d(int arg0, double arg1, double arg2) {
  static void(*ptr)(int,double,double) = NULL;
  static const char name[] = "glVertexAttrib2d";
  if (!ptr) {
    ptr = (void(*)(int,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttrib2dv(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib2dv";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib2f(int arg0, float arg1, float arg2) {
  static void(*ptr)(int,float,float) = NULL;
  static const char name[] = "glVertexAttrib2f";
  if (!ptr) {
    ptr = (void(*)(int,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttrib2fv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib2fv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib2s(int arg0, short arg1, short arg2) {
  static void(*ptr)(int,short,short) = NULL;
  static const char name[] = "glVertexAttrib2s";
  if (!ptr) {
    ptr = (void(*)(int,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttrib2sv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib2sv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib3d(int arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(int,double,double,double) = NULL;
  static const char name[] = "glVertexAttrib3d";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttrib3dv(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib3dv";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib3f(int arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(int,float,float,float) = NULL;
  static const char name[] = "glVertexAttrib3f";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttrib3fv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib3fv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib3s(int arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(int,short,short,short) = NULL;
  static const char name[] = "glVertexAttrib3s";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttrib3sv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib3sv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4Nbv(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVertexAttrib4Nbv";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4Niv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttrib4Niv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4Nsv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib4Nsv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4Nub(int arg0, char arg1, char arg2, char arg3, char arg4) {
  static void(*ptr)(int,char,char,char,char) = NULL;
  static const char name[] = "glVertexAttrib4Nub";
  if (!ptr) {
    ptr = (void(*)(int,char,char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4Nubv(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVertexAttrib4Nubv";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4Nuiv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttrib4Nuiv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4Nusv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib4Nusv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4bv(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVertexAttrib4bv";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4d(int arg0, double arg1, double arg2, double arg3, double arg4) {
  static void(*ptr)(int,double,double,double,double) = NULL;
  static const char name[] = "glVertexAttrib4d";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4dv(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib4dv";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4f(int arg0, float arg1, float arg2, float arg3, float arg4) {
  static void(*ptr)(int,float,float,float,float) = NULL;
  static const char name[] = "glVertexAttrib4f";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4fv(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib4fv";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4iv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttrib4iv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4s(int arg0, short arg1, short arg2, short arg3, short arg4) {
  static void(*ptr)(int,short,short,short,short) = NULL;
  static const char name[] = "glVertexAttrib4s";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4sv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib4sv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4ubv(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVertexAttrib4ubv";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4uiv(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttrib4uiv";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4usv(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib4usv";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribPointer(int arg0, int arg1, int arg2, char arg3, int arg4, void* arg5) {
  static void(*ptr)(int,int,int,char,int,void*) = NULL;
  static const char name[] = "glVertexAttribPointer";
  if (!ptr) {
    ptr = (void(*)(int,int,int,char,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glUniformMatrix2x3fv(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix2x3fv";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniformMatrix3x2fv(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix3x2fv";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniformMatrix2x4fv(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix2x4fv";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniformMatrix4x2fv(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix4x2fv";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniformMatrix3x4fv(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix3x4fv";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniformMatrix4x3fv(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix4x3fv";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glActiveTextureARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glActiveTextureARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glClientActiveTextureARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glClientActiveTextureARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMultiTexCoord1dARB(int arg0, double arg1) {
  static void(*ptr)(int,double) = NULL;
  static const char name[] = "glMultiTexCoord1dARB";
  if (!ptr) {
    ptr = (void(*)(int,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1dvARB(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glMultiTexCoord1dvARB";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1fARB(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glMultiTexCoord1fARB";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1fvARB(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glMultiTexCoord1fvARB";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1iARB(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glMultiTexCoord1iARB";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1ivARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glMultiTexCoord1ivARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1sARB(int arg0, short arg1) {
  static void(*ptr)(int,short) = NULL;
  static const char name[] = "glMultiTexCoord1sARB";
  if (!ptr) {
    ptr = (void(*)(int,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1svARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord1svARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord2dARB(int arg0, double arg1, double arg2) {
  static void(*ptr)(int,double,double) = NULL;
  static const char name[] = "glMultiTexCoord2dARB";
  if (!ptr) {
    ptr = (void(*)(int,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMultiTexCoord2dvARB(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glMultiTexCoord2dvARB";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord2fARB(int arg0, float arg1, float arg2) {
  static void(*ptr)(int,float,float) = NULL;
  static const char name[] = "glMultiTexCoord2fARB";
  if (!ptr) {
    ptr = (void(*)(int,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMultiTexCoord2fvARB(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glMultiTexCoord2fvARB";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord2iARB(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glMultiTexCoord2iARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMultiTexCoord2ivARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glMultiTexCoord2ivARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord2sARB(int arg0, short arg1, short arg2) {
  static void(*ptr)(int,short,short) = NULL;
  static const char name[] = "glMultiTexCoord2sARB";
  if (!ptr) {
    ptr = (void(*)(int,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMultiTexCoord2svARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord2svARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord3dARB(int arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(int,double,double,double) = NULL;
  static const char name[] = "glMultiTexCoord3dARB";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiTexCoord3dvARB(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glMultiTexCoord3dvARB";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord3fARB(int arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(int,float,float,float) = NULL;
  static const char name[] = "glMultiTexCoord3fARB";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiTexCoord3fvARB(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glMultiTexCoord3fvARB";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord3iARB(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glMultiTexCoord3iARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiTexCoord3ivARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glMultiTexCoord3ivARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord3sARB(int arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(int,short,short,short) = NULL;
  static const char name[] = "glMultiTexCoord3sARB";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiTexCoord3svARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord3svARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord4dARB(int arg0, double arg1, double arg2, double arg3, double arg4) {
  static void(*ptr)(int,double,double,double,double) = NULL;
  static const char name[] = "glMultiTexCoord4dARB";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glMultiTexCoord4dvARB(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glMultiTexCoord4dvARB";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord4fARB(int arg0, float arg1, float arg2, float arg3, float arg4) {
  static void(*ptr)(int,float,float,float,float) = NULL;
  static const char name[] = "glMultiTexCoord4fARB";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glMultiTexCoord4fvARB(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glMultiTexCoord4fvARB";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord4iARB(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glMultiTexCoord4iARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glMultiTexCoord4ivARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glMultiTexCoord4ivARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord4sARB(int arg0, short arg1, short arg2, short arg3, short arg4) {
  static void(*ptr)(int,short,short,short,short) = NULL;
  static const char name[] = "glMultiTexCoord4sARB";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glMultiTexCoord4svARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord4svARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glLoadTransposeMatrixfARB(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glLoadTransposeMatrixfARB";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glLoadTransposeMatrixdARB(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glLoadTransposeMatrixdARB";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMultTransposeMatrixfARB(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glMultTransposeMatrixfARB";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMultTransposeMatrixdARB(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glMultTransposeMatrixdARB";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSampleCoverageARB(float arg0, char arg1) {
  static void(*ptr)(float,char) = NULL;
  static const char name[] = "glSampleCoverageARB";
  if (!ptr) {
    ptr = (void(*)(float,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glCompressedTexImage3DARB(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, void* arg8) {
  static void(*ptr)(int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexImage3DARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glCompressedTexImage2DARB(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, void* arg7) {
  static void(*ptr)(int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexImage2DARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glCompressedTexImage1DARB(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, void* arg6) {
  static void(*ptr)(int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexImage1DARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glCompressedTexSubImage3DARB(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9, void* arg10) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexSubImage3DARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10);
}

void PureGL_glCompressedTexSubImage2DARB(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, void* arg8) {
  static void(*ptr)(int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexSubImage2DARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glCompressedTexSubImage1DARB(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, void* arg6) {
  static void(*ptr)(int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glCompressedTexSubImage1DARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glGetCompressedTexImageARB(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glGetCompressedTexImageARB";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPointParameterfARB(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glPointParameterfARB";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPointParameterfvARB(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glPointParameterfvARB";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWeightbvARB(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glWeightbvARB";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWeightsvARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glWeightsvARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWeightivARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glWeightivARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWeightfvARB(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glWeightfvARB";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWeightdvARB(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glWeightdvARB";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWeightubvARB(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glWeightubvARB";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWeightusvARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glWeightusvARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWeightuivARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glWeightuivARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWeightPointerARB(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glWeightPointerARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexBlendARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glVertexBlendARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glCurrentPaletteMatrixARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glCurrentPaletteMatrixARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMatrixIndexubvARB(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glMatrixIndexubvARB";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMatrixIndexusvARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMatrixIndexusvARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMatrixIndexuivARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glMatrixIndexuivARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMatrixIndexPointerARB(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glMatrixIndexPointerARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glWindowPos2dARB(double arg0, double arg1) {
  static void(*ptr)(double,double) = NULL;
  static const char name[] = "glWindowPos2dARB";
  if (!ptr) {
    ptr = (void(*)(double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2dvARB(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glWindowPos2dvARB";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos2fARB(float arg0, float arg1) {
  static void(*ptr)(float,float) = NULL;
  static const char name[] = "glWindowPos2fARB";
  if (!ptr) {
    ptr = (void(*)(float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2fvARB(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glWindowPos2fvARB";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos2iARB(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glWindowPos2iARB";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2ivARB(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glWindowPos2ivARB";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos2sARB(short arg0, short arg1) {
  static void(*ptr)(short,short) = NULL;
  static const char name[] = "glWindowPos2sARB";
  if (!ptr) {
    ptr = (void(*)(short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2svARB(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glWindowPos2svARB";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3dARB(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glWindowPos3dARB";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3dvARB(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glWindowPos3dvARB";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3fARB(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glWindowPos3fARB";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3fvARB(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glWindowPos3fvARB";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3iARB(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glWindowPos3iARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3ivARB(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glWindowPos3ivARB";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3sARB(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glWindowPos3sARB";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3svARB(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glWindowPos3svARB";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertexAttrib1dARB(int arg0, double arg1) {
  static void(*ptr)(int,double) = NULL;
  static const char name[] = "glVertexAttrib1dARB";
  if (!ptr) {
    ptr = (void(*)(int,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1dvARB(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib1dvARB";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1fARB(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glVertexAttrib1fARB";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1fvARB(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib1fvARB";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1sARB(int arg0, short arg1) {
  static void(*ptr)(int,short) = NULL;
  static const char name[] = "glVertexAttrib1sARB";
  if (!ptr) {
    ptr = (void(*)(int,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1svARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib1svARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib2dARB(int arg0, double arg1, double arg2) {
  static void(*ptr)(int,double,double) = NULL;
  static const char name[] = "glVertexAttrib2dARB";
  if (!ptr) {
    ptr = (void(*)(int,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttrib2dvARB(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib2dvARB";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib2fARB(int arg0, float arg1, float arg2) {
  static void(*ptr)(int,float,float) = NULL;
  static const char name[] = "glVertexAttrib2fARB";
  if (!ptr) {
    ptr = (void(*)(int,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttrib2fvARB(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib2fvARB";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib2sARB(int arg0, short arg1, short arg2) {
  static void(*ptr)(int,short,short) = NULL;
  static const char name[] = "glVertexAttrib2sARB";
  if (!ptr) {
    ptr = (void(*)(int,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttrib2svARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib2svARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib3dARB(int arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(int,double,double,double) = NULL;
  static const char name[] = "glVertexAttrib3dARB";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttrib3dvARB(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib3dvARB";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib3fARB(int arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(int,float,float,float) = NULL;
  static const char name[] = "glVertexAttrib3fARB";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttrib3fvARB(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib3fvARB";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib3sARB(int arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(int,short,short,short) = NULL;
  static const char name[] = "glVertexAttrib3sARB";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttrib3svARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib3svARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4NbvARB(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVertexAttrib4NbvARB";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4NivARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttrib4NivARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4NsvARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib4NsvARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4NubARB(int arg0, char arg1, char arg2, char arg3, char arg4) {
  static void(*ptr)(int,char,char,char,char) = NULL;
  static const char name[] = "glVertexAttrib4NubARB";
  if (!ptr) {
    ptr = (void(*)(int,char,char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4NubvARB(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVertexAttrib4NubvARB";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4NuivARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttrib4NuivARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4NusvARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib4NusvARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4bvARB(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVertexAttrib4bvARB";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4dARB(int arg0, double arg1, double arg2, double arg3, double arg4) {
  static void(*ptr)(int,double,double,double,double) = NULL;
  static const char name[] = "glVertexAttrib4dARB";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4dvARB(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib4dvARB";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4fARB(int arg0, float arg1, float arg2, float arg3, float arg4) {
  static void(*ptr)(int,float,float,float,float) = NULL;
  static const char name[] = "glVertexAttrib4fARB";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4fvARB(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib4fvARB";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4ivARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttrib4ivARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4sARB(int arg0, short arg1, short arg2, short arg3, short arg4) {
  static void(*ptr)(int,short,short,short,short) = NULL;
  static const char name[] = "glVertexAttrib4sARB";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4svARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib4svARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4ubvARB(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVertexAttrib4ubvARB";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4uivARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttrib4uivARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4usvARB(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib4usvARB";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribPointerARB(int arg0, int arg1, int arg2, char arg3, int arg4, void* arg5) {
  static void(*ptr)(int,int,int,char,int,void*) = NULL;
  static const char name[] = "glVertexAttribPointerARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,char,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glEnableVertexAttribArrayARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glEnableVertexAttribArrayARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDisableVertexAttribArrayARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDisableVertexAttribArrayARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glProgramStringARB(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glProgramStringARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glBindProgramARB(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBindProgramARB";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteProgramsARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteProgramsARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGenProgramsARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenProgramsARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glProgramEnvParameter4dARB(int arg0, int arg1, double arg2, double arg3, double arg4, double arg5) {
  static void(*ptr)(int,int,double,double,double,double) = NULL;
  static const char name[] = "glProgramEnvParameter4dARB";
  if (!ptr) {
    ptr = (void(*)(int,int,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glProgramEnvParameter4dvARB(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glProgramEnvParameter4dvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glProgramEnvParameter4fARB(int arg0, int arg1, float arg2, float arg3, float arg4, float arg5) {
  static void(*ptr)(int,int,float,float,float,float) = NULL;
  static const char name[] = "glProgramEnvParameter4fARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glProgramEnvParameter4fvARB(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glProgramEnvParameter4fvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glProgramLocalParameter4dARB(int arg0, int arg1, double arg2, double arg3, double arg4, double arg5) {
  static void(*ptr)(int,int,double,double,double,double) = NULL;
  static const char name[] = "glProgramLocalParameter4dARB";
  if (!ptr) {
    ptr = (void(*)(int,int,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glProgramLocalParameter4dvARB(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glProgramLocalParameter4dvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glProgramLocalParameter4fARB(int arg0, int arg1, float arg2, float arg3, float arg4, float arg5) {
  static void(*ptr)(int,int,float,float,float,float) = NULL;
  static const char name[] = "glProgramLocalParameter4fARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glProgramLocalParameter4fvARB(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glProgramLocalParameter4fvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetProgramEnvParameterdvARB(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glGetProgramEnvParameterdvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetProgramEnvParameterfvARB(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetProgramEnvParameterfvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetProgramLocalParameterdvARB(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glGetProgramLocalParameterdvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetProgramLocalParameterfvARB(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetProgramLocalParameterfvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetProgramivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetProgramivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetProgramStringARB(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glGetProgramStringARB";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribdvARB(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glGetVertexAttribdvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribfvARB(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetVertexAttribfvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetVertexAttribivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribPointervARB(int arg0, int arg1, void** arg2) {
  static void(*ptr)(int,int,void**) = NULL;
  static const char name[] = "glGetVertexAttribPointervARB";
  if (!ptr) {
    ptr = (void(*)(int,int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

char PureGL_glIsProgramARB(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsProgramARB";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBindBufferARB(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBindBufferARB";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteBuffersARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteBuffersARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGenBuffersARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenBuffersARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glIsBufferARB(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsBufferARB";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBufferDataARB(int arg0, int arg1, void* arg2, int arg3) {
  static void(*ptr)(int,int,void*,int) = NULL;
  static const char name[] = "glBufferDataARB";
  if (!ptr) {
    ptr = (void(*)(int,int,void*,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glBufferSubDataARB(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glBufferSubDataARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetBufferSubDataARB(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glGetBufferSubDataARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void* PureGL_glMapBufferARB(int arg0, int arg1) {
  static void*(*ptr)(int,int) = NULL;
  static const char name[] = "glMapBufferARB";
  if (!ptr) {
    ptr = (void*(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glUnmapBufferARB(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glUnmapBufferARB";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGetBufferParameterivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetBufferParameterivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetBufferPointervARB(int arg0, int arg1, void** arg2) {
  static void(*ptr)(int,int,void**) = NULL;
  static const char name[] = "glGetBufferPointervARB";
  if (!ptr) {
    ptr = (void(*)(int,int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGenQueriesARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenQueriesARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteQueriesARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteQueriesARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glIsQueryARB(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsQueryARB";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBeginQueryARB(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBeginQueryARB";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glEndQueryARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glEndQueryARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGetQueryivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetQueryivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetQueryObjectivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetQueryObjectivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetQueryObjectuivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetQueryObjectuivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glDeleteObjectARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDeleteObjectARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int PureGL_glGetHandleARB(int arg0) {
  static int(*ptr)(int) = NULL;
  static const char name[] = "glGetHandleARB";
  if (!ptr) {
    ptr = (int(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDetachObjectARB(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glDetachObjectARB";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glCreateShaderObjectARB(int arg0) {
  static int(*ptr)(int) = NULL;
  static const char name[] = "glCreateShaderObjectARB";
  if (!ptr) {
    ptr = (int(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glShaderSourceARB(int arg0, int arg1, char** arg2, int* arg3) {
  static void(*ptr)(int,int,char**,int*) = NULL;
  static const char name[] = "glShaderSourceARB";
  if (!ptr) {
    ptr = (void(*)(int,int,char**,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glCompileShaderARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glCompileShaderARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int PureGL_glCreateProgramObjectARB() {
  static int(*ptr)() = NULL;
  static const char name[] = "glCreateProgramObjectARB";
  if (!ptr) {
    ptr = (int(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glAttachObjectARB(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glAttachObjectARB";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glLinkProgramARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glLinkProgramARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glUseProgramObjectARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glUseProgramObjectARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glValidateProgramARB(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glValidateProgramARB";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glUniform1fARB(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glUniform1fARB";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glUniform2fARB(int arg0, float arg1, float arg2) {
  static void(*ptr)(int,float,float) = NULL;
  static const char name[] = "glUniform2fARB";
  if (!ptr) {
    ptr = (void(*)(int,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform3fARB(int arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(int,float,float,float) = NULL;
  static const char name[] = "glUniform3fARB";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniform4fARB(int arg0, float arg1, float arg2, float arg3, float arg4) {
  static void(*ptr)(int,float,float,float,float) = NULL;
  static const char name[] = "glUniform4fARB";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glUniform1iARB(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glUniform1iARB";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glUniform2iARB(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glUniform2iARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform3iARB(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glUniform3iARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniform4iARB(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glUniform4iARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glUniform1fvARB(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glUniform1fvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform2fvARB(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glUniform2fvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform3fvARB(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glUniform3fvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform4fvARB(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glUniform4fvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform1ivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform1ivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform2ivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform2ivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform3ivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform3ivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform4ivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform4ivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniformMatrix2fvARB(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix2fvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniformMatrix3fvARB(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix3fvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniformMatrix4fvARB(int arg0, int arg1, char arg2, float* arg3) {
  static void(*ptr)(int,int,char,float*) = NULL;
  static const char name[] = "glUniformMatrix4fvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,char,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetObjectParameterfvARB(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetObjectParameterfvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetObjectParameterivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetObjectParameterivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetInfoLogARB(int arg0, int arg1, int* arg2, char* arg3) {
  static void(*ptr)(int,int,int*,char*) = NULL;
  static const char name[] = "glGetInfoLogARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetAttachedObjectsARB(int arg0, int arg1, int* arg2, int* arg3) {
  static void(*ptr)(int,int,int*,int*) = NULL;
  static const char name[] = "glGetAttachedObjectsARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

int PureGL_glGetUniformLocationARB(int arg0, char* arg1) {
  static int(*ptr)(int,char*) = NULL;
  static const char name[] = "glGetUniformLocationARB";
  if (!ptr) {
    ptr = (int(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetActiveUniformARB(int arg0, int arg1, int arg2, int* arg3, int* arg4, int* arg5, char* arg6) {
  static void(*ptr)(int,int,int,int*,int*,int*,char*) = NULL;
  static const char name[] = "glGetActiveUniformARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*,int*,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glGetUniformfvARB(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetUniformfvARB";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetUniformivARB(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetUniformivARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetShaderSourceARB(int arg0, int arg1, int* arg2, char* arg3) {
  static void(*ptr)(int,int,int*,char*) = NULL;
  static const char name[] = "glGetShaderSourceARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glBindAttribLocationARB(int arg0, int arg1, char* arg2) {
  static void(*ptr)(int,int,char*) = NULL;
  static const char name[] = "glBindAttribLocationARB";
  if (!ptr) {
    ptr = (void(*)(int,int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetActiveAttribARB(int arg0, int arg1, int arg2, int* arg3, int* arg4, int* arg5, char* arg6) {
  static void(*ptr)(int,int,int,int*,int*,int*,char*) = NULL;
  static const char name[] = "glGetActiveAttribARB";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*,int*,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

int PureGL_glGetAttribLocationARB(int arg0, char* arg1) {
  static int(*ptr)(int,char*) = NULL;
  static const char name[] = "glGetAttribLocationARB";
  if (!ptr) {
    ptr = (int(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDrawBuffersARB(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDrawBuffersARB";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glClampColorARB(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glClampColorARB";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glBlendColorEXT(float arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(float,float,float,float) = NULL;
  static const char name[] = "glBlendColorEXT";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glPolygonOffsetEXT(float arg0, float arg1) {
  static void(*ptr)(float,float) = NULL;
  static const char name[] = "glPolygonOffsetEXT";
  if (!ptr) {
    ptr = (void(*)(float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTexImage3DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, void* arg9) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexImage3DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
}

void PureGL_glTexSubImage3DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9, void* arg10) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexSubImage3DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10);
}

void PureGL_glGetTexFilterFuncSGIS(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetTexFilterFuncSGIS";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexFilterFuncSGIS(int arg0, int arg1, int arg2, float* arg3) {
  static void(*ptr)(int,int,int,float*) = NULL;
  static const char name[] = "glTexFilterFuncSGIS";
  if (!ptr) {
    ptr = (void(*)(int,int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glTexSubImage1DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, void* arg6) {
  static void(*ptr)(int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexSubImage1DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glTexSubImage2DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, void* arg8) {
  static void(*ptr)(int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexSubImage2DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glCopyTexImage1DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6) {
  static void(*ptr)(int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyTexImage1DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glCopyTexImage2DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7) {
  static void(*ptr)(int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyTexImage2DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glCopyTexSubImage1DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyTexSubImage1DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glCopyTexSubImage2DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7) {
  static void(*ptr)(int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyTexSubImage2DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glCopyTexSubImage3DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyTexSubImage3DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glGetHistogramEXT(int arg0, char arg1, int arg2, int arg3, void* arg4) {
  static void(*ptr)(int,char,int,int,void*) = NULL;
  static const char name[] = "glGetHistogramEXT";
  if (!ptr) {
    ptr = (void(*)(int,char,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetHistogramParameterfvEXT(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetHistogramParameterfvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetHistogramParameterivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetHistogramParameterivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMinmaxEXT(int arg0, char arg1, int arg2, int arg3, void* arg4) {
  static void(*ptr)(int,char,int,int,void*) = NULL;
  static const char name[] = "glGetMinmaxEXT";
  if (!ptr) {
    ptr = (void(*)(int,char,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetMinmaxParameterfvEXT(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetMinmaxParameterfvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMinmaxParameterivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetMinmaxParameterivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glHistogramEXT(int arg0, int arg1, int arg2, char arg3) {
  static void(*ptr)(int,int,int,char) = NULL;
  static const char name[] = "glHistogramEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMinmaxEXT(int arg0, int arg1, char arg2) {
  static void(*ptr)(int,int,char) = NULL;
  static const char name[] = "glMinmaxEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glResetHistogramEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glResetHistogramEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glResetMinmaxEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glResetMinmaxEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glConvolutionFilter1DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, void* arg5) {
  static void(*ptr)(int,int,int,int,int,void*) = NULL;
  static const char name[] = "glConvolutionFilter1DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glConvolutionFilter2DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, void* arg6) {
  static void(*ptr)(int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glConvolutionFilter2DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glConvolutionParameterfEXT(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glConvolutionParameterfEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glConvolutionParameterfvEXT(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glConvolutionParameterfvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glConvolutionParameteriEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glConvolutionParameteriEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glConvolutionParameterivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glConvolutionParameterivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glCopyConvolutionFilter1DEXT(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glCopyConvolutionFilter1DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glCopyConvolutionFilter2DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glCopyConvolutionFilter2DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glGetConvolutionFilterEXT(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glGetConvolutionFilterEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetConvolutionParameterfvEXT(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetConvolutionParameterfvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetConvolutionParameterivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetConvolutionParameterivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetSeparableFilterEXT(int arg0, int arg1, int arg2, void* arg3, void* arg4, void* arg5) {
  static void(*ptr)(int,int,int,void*,void*,void*) = NULL;
  static const char name[] = "glGetSeparableFilterEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*,void*,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glSeparableFilter2DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, void* arg6, void* arg7) {
  static void(*ptr)(int,int,int,int,int,int,void*,void*) = NULL;
  static const char name[] = "glSeparableFilter2DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,void*,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glColorTableSGI(int arg0, int arg1, int arg2, int arg3, int arg4, void* arg5) {
  static void(*ptr)(int,int,int,int,int,void*) = NULL;
  static const char name[] = "glColorTableSGI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glColorTableParameterfvSGI(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glColorTableParameterfvSGI";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColorTableParameterivSGI(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glColorTableParameterivSGI";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glCopyColorTableSGI(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glCopyColorTableSGI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetColorTableSGI(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glGetColorTableSGI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetColorTableParameterfvSGI(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetColorTableParameterfvSGI";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetColorTableParameterivSGI(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetColorTableParameterivSGI";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPixelTexGenSGIX(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glPixelTexGenSGIX";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glPixelTexGenParameteriSGIS(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glPixelTexGenParameteriSGIS";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPixelTexGenParameterivSGIS(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glPixelTexGenParameterivSGIS";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPixelTexGenParameterfSGIS(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glPixelTexGenParameterfSGIS";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPixelTexGenParameterfvSGIS(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glPixelTexGenParameterfvSGIS";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetPixelTexGenParameterivSGIS(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGetPixelTexGenParameterivSGIS";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetPixelTexGenParameterfvSGIS(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glGetPixelTexGenParameterfvSGIS";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTexImage4DSGIS(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9, void* arg10) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexImage4DSGIS";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10);
}

void PureGL_glTexSubImage4DSGIS(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9, int arg10, int arg11, void* arg12) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,int,int,int,void*) = NULL;
  static const char name[] = "glTexSubImage4DSGIS";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12);
}

char PureGL_glAreTexturesResidentEXT(int arg0, int* arg1, char* arg2) {
  static char(*ptr)(int,int*,char*) = NULL;
  static const char name[] = "glAreTexturesResidentEXT";
  if (!ptr) {
    ptr = (char(*)(int,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBindTextureEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBindTextureEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteTexturesEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteTexturesEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGenTexturesEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenTexturesEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glIsTextureEXT(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsTextureEXT";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glPrioritizeTexturesEXT(int arg0, int* arg1, float* arg2) {
  static void(*ptr)(int,int*,float*) = NULL;
  static const char name[] = "glPrioritizeTexturesEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glDetailTexFuncSGIS(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glDetailTexFuncSGIS";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetDetailTexFuncSGIS(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glGetDetailTexFuncSGIS";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glSharpenTexFuncSGIS(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glSharpenTexFuncSGIS";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetSharpenTexFuncSGIS(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glGetSharpenTexFuncSGIS";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glSampleMaskSGIS(float arg0, char arg1) {
  static void(*ptr)(float,char) = NULL;
  static const char name[] = "glSampleMaskSGIS";
  if (!ptr) {
    ptr = (void(*)(float,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glSamplePatternSGIS(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glSamplePatternSGIS";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glArrayElementEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glArrayElementEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColorPointerEXT(int arg0, int arg1, int arg2, int arg3, void* arg4) {
  static void(*ptr)(int,int,int,int,void*) = NULL;
  static const char name[] = "glColorPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glDrawArraysEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glDrawArraysEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glEdgeFlagPointerEXT(int arg0, int arg1, char* arg2) {
  static void(*ptr)(int,int,char*) = NULL;
  static const char name[] = "glEdgeFlagPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetPointervEXT(int arg0, void** arg1) {
  static void(*ptr)(int,void**) = NULL;
  static const char name[] = "glGetPointervEXT";
  if (!ptr) {
    ptr = (void(*)(int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glIndexPointerEXT(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glIndexPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glNormalPointerEXT(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glNormalPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glTexCoordPointerEXT(int arg0, int arg1, int arg2, int arg3, void* arg4) {
  static void(*ptr)(int,int,int,int,void*) = NULL;
  static const char name[] = "glTexCoordPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexPointerEXT(int arg0, int arg1, int arg2, int arg3, void* arg4) {
  static void(*ptr)(int,int,int,int,void*) = NULL;
  static const char name[] = "glVertexPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glBlendEquationEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glBlendEquationEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSpriteParameterfSGIX(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glSpriteParameterfSGIX";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glSpriteParameterfvSGIX(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glSpriteParameterfvSGIX";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glSpriteParameteriSGIX(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glSpriteParameteriSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glSpriteParameterivSGIX(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glSpriteParameterivSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPointParameterfEXT(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glPointParameterfEXT";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPointParameterfvEXT(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glPointParameterfvEXT";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPointParameterfSGIS(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glPointParameterfSGIS";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPointParameterfvSGIS(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glPointParameterfvSGIS";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glGetInstrumentsSGIX() {
  static int(*ptr)() = NULL;
  static const char name[] = "glGetInstrumentsSGIX";
  if (!ptr) {
    ptr = (int(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glInstrumentsBufferSGIX(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glInstrumentsBufferSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glPollInstrumentsSGIX(int* arg0) {
  static int(*ptr)(int*) = NULL;
  static const char name[] = "glPollInstrumentsSGIX";
  if (!ptr) {
    ptr = (int(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glReadInstrumentsSGIX(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glReadInstrumentsSGIX";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glStartInstrumentsSGIX() {
  static void(*ptr)() = NULL;
  static const char name[] = "glStartInstrumentsSGIX";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glStopInstrumentsSGIX(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glStopInstrumentsSGIX";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFrameZoomSGIX(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glFrameZoomSGIX";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTagSampleBufferSGIX() {
  static void(*ptr)() = NULL;
  static const char name[] = "glTagSampleBufferSGIX";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glDeformationMap3dSGIX(int arg0, double arg1, double arg2, int arg3, int arg4, double arg5, double arg6, int arg7, int arg8, double arg9, double arg10, int arg11, int arg12, double* arg13) {
  static void(*ptr)(int,double,double,int,int,double,double,int,int,double,double,int,int,double*) = NULL;
  static const char name[] = "glDeformationMap3dSGIX";
  if (!ptr) {
    ptr = (void(*)(int,double,double,int,int,double,double,int,int,double,double,int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13);
}

void PureGL_glDeformationMap3fSGIX(int arg0, float arg1, float arg2, int arg3, int arg4, float arg5, float arg6, int arg7, int arg8, float arg9, float arg10, int arg11, int arg12, float* arg13) {
  static void(*ptr)(int,float,float,int,int,float,float,int,int,float,float,int,int,float*) = NULL;
  static const char name[] = "glDeformationMap3fSGIX";
  if (!ptr) {
    ptr = (void(*)(int,float,float,int,int,float,float,int,int,float,float,int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13);
}

void PureGL_glDeformSGIX(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDeformSGIX";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glLoadIdentityDeformationMapSGIX(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glLoadIdentityDeformationMapSGIX";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glReferencePlaneSGIX(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glReferencePlaneSGIX";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFlushRasterSGIX() {
  static void(*ptr)() = NULL;
  static const char name[] = "glFlushRasterSGIX";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glFogFuncSGIS(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glFogFuncSGIS";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetFogFuncSGIS(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glGetFogFuncSGIS";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glImageTransformParameteriHP(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glImageTransformParameteriHP";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glImageTransformParameterfHP(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glImageTransformParameterfHP";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glImageTransformParameterivHP(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glImageTransformParameterivHP";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glImageTransformParameterfvHP(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glImageTransformParameterfvHP";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetImageTransformParameterivHP(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetImageTransformParameterivHP";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetImageTransformParameterfvHP(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetImageTransformParameterfvHP";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColorSubTableEXT(int arg0, int arg1, int arg2, int arg3, int arg4, void* arg5) {
  static void(*ptr)(int,int,int,int,int,void*) = NULL;
  static const char name[] = "glColorSubTableEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glCopyColorSubTableEXT(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glCopyColorSubTableEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glHintPGI(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glHintPGI";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glColorTableEXT(int arg0, int arg1, int arg2, int arg3, int arg4, void* arg5) {
  static void(*ptr)(int,int,int,int,int,void*) = NULL;
  static const char name[] = "glColorTableEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glGetColorTableEXT(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glGetColorTableEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetColorTableParameterivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetColorTableParameterivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetColorTableParameterfvEXT(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetColorTableParameterfvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetListParameterfvSGIX(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetListParameterfvSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetListParameterivSGIX(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetListParameterivSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glListParameterfSGIX(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glListParameterfSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glListParameterfvSGIX(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glListParameterfvSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glListParameteriSGIX(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glListParameteriSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glListParameterivSGIX(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glListParameterivSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glIndexMaterialEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glIndexMaterialEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glIndexFuncEXT(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glIndexFuncEXT";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glLockArraysEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glLockArraysEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glUnlockArraysEXT() {
  static void(*ptr)() = NULL;
  static const char name[] = "glUnlockArraysEXT";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glCullParameterdvEXT(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glCullParameterdvEXT";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glCullParameterfvEXT(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glCullParameterfvEXT";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFragmentColorMaterialSGIX(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glFragmentColorMaterialSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFragmentLightfSGIX(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glFragmentLightfSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFragmentLightfvSGIX(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glFragmentLightfvSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFragmentLightiSGIX(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glFragmentLightiSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFragmentLightivSGIX(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glFragmentLightivSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFragmentLightModelfSGIX(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glFragmentLightModelfSGIX";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFragmentLightModelfvSGIX(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glFragmentLightModelfvSGIX";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFragmentLightModeliSGIX(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glFragmentLightModeliSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFragmentLightModelivSGIX(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glFragmentLightModelivSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFragmentMaterialfSGIX(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glFragmentMaterialfSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFragmentMaterialfvSGIX(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glFragmentMaterialfvSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFragmentMaterialiSGIX(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glFragmentMaterialiSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFragmentMaterialivSGIX(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glFragmentMaterialivSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetFragmentLightfvSGIX(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetFragmentLightfvSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetFragmentLightivSGIX(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetFragmentLightivSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetFragmentMaterialfvSGIX(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetFragmentMaterialfvSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetFragmentMaterialivSGIX(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetFragmentMaterialivSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glLightEnviSGIX(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glLightEnviSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDrawRangeElementsEXT(int arg0, int arg1, int arg2, int arg3, int arg4, void* arg5) {
  static void(*ptr)(int,int,int,int,int,void*) = NULL;
  static const char name[] = "glDrawRangeElementsEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glApplyTextureEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glApplyTextureEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTextureLightEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glTextureLightEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTextureMaterialEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glTextureMaterialEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glAsyncMarkerSGIX(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glAsyncMarkerSGIX";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int PureGL_glFinishAsyncSGIX(int* arg0) {
  static int(*ptr)(int*) = NULL;
  static const char name[] = "glFinishAsyncSGIX";
  if (!ptr) {
    ptr = (int(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int PureGL_glPollAsyncSGIX(int* arg0) {
  static int(*ptr)(int*) = NULL;
  static const char name[] = "glPollAsyncSGIX";
  if (!ptr) {
    ptr = (int(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int PureGL_glGenAsyncMarkersSGIX(int arg0) {
  static int(*ptr)(int) = NULL;
  static const char name[] = "glGenAsyncMarkersSGIX";
  if (!ptr) {
    ptr = (int(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDeleteAsyncMarkersSGIX(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glDeleteAsyncMarkersSGIX";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glIsAsyncMarkerSGIX(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsAsyncMarkerSGIX";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertexPointervINTEL(int arg0, int arg1, void** arg2) {
  static void(*ptr)(int,int,void**) = NULL;
  static const char name[] = "glVertexPointervINTEL";
  if (!ptr) {
    ptr = (void(*)(int,int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glNormalPointervINTEL(int arg0, void** arg1) {
  static void(*ptr)(int,void**) = NULL;
  static const char name[] = "glNormalPointervINTEL";
  if (!ptr) {
    ptr = (void(*)(int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glColorPointervINTEL(int arg0, int arg1, void** arg2) {
  static void(*ptr)(int,int,void**) = NULL;
  static const char name[] = "glColorPointervINTEL";
  if (!ptr) {
    ptr = (void(*)(int,int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexCoordPointervINTEL(int arg0, int arg1, void** arg2) {
  static void(*ptr)(int,int,void**) = NULL;
  static const char name[] = "glTexCoordPointervINTEL";
  if (!ptr) {
    ptr = (void(*)(int,int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPixelTransformParameteriEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glPixelTransformParameteriEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPixelTransformParameterfEXT(int arg0, int arg1, float arg2) {
  static void(*ptr)(int,int,float) = NULL;
  static const char name[] = "glPixelTransformParameterfEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPixelTransformParameterivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glPixelTransformParameterivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPixelTransformParameterfvEXT(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glPixelTransformParameterfvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3bEXT(char arg0, char arg1, char arg2) {
  static void(*ptr)(char,char,char) = NULL;
  static const char name[] = "glSecondaryColor3bEXT";
  if (!ptr) {
    ptr = (void(*)(char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3bvEXT(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glSecondaryColor3bvEXT";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3dEXT(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glSecondaryColor3dEXT";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3dvEXT(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glSecondaryColor3dvEXT";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3fEXT(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glSecondaryColor3fEXT";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3fvEXT(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glSecondaryColor3fvEXT";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3iEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glSecondaryColor3iEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3ivEXT(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glSecondaryColor3ivEXT";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3sEXT(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glSecondaryColor3sEXT";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3svEXT(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glSecondaryColor3svEXT";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3ubEXT(char arg0, char arg1, char arg2) {
  static void(*ptr)(char,char,char) = NULL;
  static const char name[] = "glSecondaryColor3ubEXT";
  if (!ptr) {
    ptr = (void(*)(char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3ubvEXT(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glSecondaryColor3ubvEXT";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3uiEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glSecondaryColor3uiEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3uivEXT(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glSecondaryColor3uivEXT";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3usEXT(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glSecondaryColor3usEXT";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3usvEXT(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glSecondaryColor3usvEXT";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColorPointerEXT(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glSecondaryColorPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glTextureNormalEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glTextureNormalEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMultiDrawArraysEXT(int arg0, int* arg1, int* arg2, int arg3) {
  static void(*ptr)(int,int*,int*,int) = NULL;
  static const char name[] = "glMultiDrawArraysEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*,int*,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiDrawElementsEXT(int arg0, int* arg1, int arg2, void** arg3, int arg4) {
  static void(*ptr)(int,int*,int,void**,int) = NULL;
  static const char name[] = "glMultiDrawElementsEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*,int,void**,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glFogCoordfEXT(float arg0) {
  static void(*ptr)(float) = NULL;
  static const char name[] = "glFogCoordfEXT";
  if (!ptr) {
    ptr = (void(*)(float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFogCoordfvEXT(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glFogCoordfvEXT";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFogCoorddEXT(double arg0) {
  static void(*ptr)(double) = NULL;
  static const char name[] = "glFogCoorddEXT";
  if (!ptr) {
    ptr = (void(*)(double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFogCoorddvEXT(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glFogCoorddvEXT";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFogCoordPointerEXT(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glFogCoordPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTangent3bEXT(char arg0, char arg1, char arg2) {
  static void(*ptr)(char,char,char) = NULL;
  static const char name[] = "glTangent3bEXT";
  if (!ptr) {
    ptr = (void(*)(char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTangent3bvEXT(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glTangent3bvEXT";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTangent3dEXT(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glTangent3dEXT";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTangent3dvEXT(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glTangent3dvEXT";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTangent3fEXT(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glTangent3fEXT";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTangent3fvEXT(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glTangent3fvEXT";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTangent3iEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glTangent3iEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTangent3ivEXT(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glTangent3ivEXT";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTangent3sEXT(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glTangent3sEXT";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTangent3svEXT(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glTangent3svEXT";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBinormal3bEXT(char arg0, char arg1, char arg2) {
  static void(*ptr)(char,char,char) = NULL;
  static const char name[] = "glBinormal3bEXT";
  if (!ptr) {
    ptr = (void(*)(char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBinormal3bvEXT(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glBinormal3bvEXT";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBinormal3dEXT(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glBinormal3dEXT";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBinormal3dvEXT(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glBinormal3dvEXT";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBinormal3fEXT(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glBinormal3fEXT";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBinormal3fvEXT(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glBinormal3fvEXT";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBinormal3iEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glBinormal3iEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBinormal3ivEXT(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glBinormal3ivEXT";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBinormal3sEXT(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glBinormal3sEXT";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBinormal3svEXT(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glBinormal3svEXT";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTangentPointerEXT(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glTangentPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBinormalPointerEXT(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glBinormalPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFinishTextureSUNX() {
  static void(*ptr)() = NULL;
  static const char name[] = "glFinishTextureSUNX";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glGlobalAlphaFactorbSUN(char arg0) {
  static void(*ptr)(char) = NULL;
  static const char name[] = "glGlobalAlphaFactorbSUN";
  if (!ptr) {
    ptr = (void(*)(char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGlobalAlphaFactorsSUN(short arg0) {
  static void(*ptr)(short) = NULL;
  static const char name[] = "glGlobalAlphaFactorsSUN";
  if (!ptr) {
    ptr = (void(*)(short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGlobalAlphaFactoriSUN(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glGlobalAlphaFactoriSUN";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGlobalAlphaFactorfSUN(float arg0) {
  static void(*ptr)(float) = NULL;
  static const char name[] = "glGlobalAlphaFactorfSUN";
  if (!ptr) {
    ptr = (void(*)(float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGlobalAlphaFactordSUN(double arg0) {
  static void(*ptr)(double) = NULL;
  static const char name[] = "glGlobalAlphaFactordSUN";
  if (!ptr) {
    ptr = (void(*)(double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGlobalAlphaFactorubSUN(char arg0) {
  static void(*ptr)(char) = NULL;
  static const char name[] = "glGlobalAlphaFactorubSUN";
  if (!ptr) {
    ptr = (void(*)(char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGlobalAlphaFactorusSUN(short arg0) {
  static void(*ptr)(short) = NULL;
  static const char name[] = "glGlobalAlphaFactorusSUN";
  if (!ptr) {
    ptr = (void(*)(short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGlobalAlphaFactoruiSUN(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glGlobalAlphaFactoruiSUN";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glReplacementCodeuiSUN(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glReplacementCodeuiSUN";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glReplacementCodeusSUN(short arg0) {
  static void(*ptr)(short) = NULL;
  static const char name[] = "glReplacementCodeusSUN";
  if (!ptr) {
    ptr = (void(*)(short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glReplacementCodeubSUN(char arg0) {
  static void(*ptr)(char) = NULL;
  static const char name[] = "glReplacementCodeubSUN";
  if (!ptr) {
    ptr = (void(*)(char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glReplacementCodeuivSUN(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glReplacementCodeuivSUN";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glReplacementCodeusvSUN(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glReplacementCodeusvSUN";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glReplacementCodeubvSUN(char* arg0) {
  static void(*ptr)(char*) = NULL;
  static const char name[] = "glReplacementCodeubvSUN";
  if (!ptr) {
    ptr = (void(*)(char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glReplacementCodePointerSUN(int arg0, int arg1, void** arg2) {
  static void(*ptr)(int,int,void**) = NULL;
  static const char name[] = "glReplacementCodePointerSUN";
  if (!ptr) {
    ptr = (void(*)(int,int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColor4ubVertex2fSUN(char arg0, char arg1, char arg2, char arg3, float arg4, float arg5) {
  static void(*ptr)(char,char,char,char,float,float) = NULL;
  static const char name[] = "glColor4ubVertex2fSUN";
  if (!ptr) {
    ptr = (void(*)(char,char,char,char,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glColor4ubVertex2fvSUN(char* arg0, float* arg1) {
  static void(*ptr)(char*,float*) = NULL;
  static const char name[] = "glColor4ubVertex2fvSUN";
  if (!ptr) {
    ptr = (void(*)(char*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glColor4ubVertex3fSUN(char arg0, char arg1, char arg2, char arg3, float arg4, float arg5, float arg6) {
  static void(*ptr)(char,char,char,char,float,float,float) = NULL;
  static const char name[] = "glColor4ubVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(char,char,char,char,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glColor4ubVertex3fvSUN(char* arg0, float* arg1) {
  static void(*ptr)(char*,float*) = NULL;
  static const char name[] = "glColor4ubVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(char*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glColor3fVertex3fSUN(float arg0, float arg1, float arg2, float arg3, float arg4, float arg5) {
  static void(*ptr)(float,float,float,float,float,float) = NULL;
  static const char name[] = "glColor3fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glColor3fVertex3fvSUN(float* arg0, float* arg1) {
  static void(*ptr)(float*,float*) = NULL;
  static const char name[] = "glColor3fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glNormal3fVertex3fSUN(float arg0, float arg1, float arg2, float arg3, float arg4, float arg5) {
  static void(*ptr)(float,float,float,float,float,float) = NULL;
  static const char name[] = "glNormal3fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glNormal3fVertex3fvSUN(float* arg0, float* arg1) {
  static void(*ptr)(float*,float*) = NULL;
  static const char name[] = "glNormal3fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glColor4fNormal3fVertex3fSUN(float arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6, float arg7, float arg8, float arg9) {
  static void(*ptr)(float,float,float,float,float,float,float,float,float,float) = NULL;
  static const char name[] = "glColor4fNormal3fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float,float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
}

void PureGL_glColor4fNormal3fVertex3fvSUN(float* arg0, float* arg1, float* arg2) {
  static void(*ptr)(float*,float*,float*) = NULL;
  static const char name[] = "glColor4fNormal3fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(float*,float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexCoord2fVertex3fSUN(float arg0, float arg1, float arg2, float arg3, float arg4) {
  static void(*ptr)(float,float,float,float,float) = NULL;
  static const char name[] = "glTexCoord2fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glTexCoord2fVertex3fvSUN(float* arg0, float* arg1) {
  static void(*ptr)(float*,float*) = NULL;
  static const char name[] = "glTexCoord2fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTexCoord4fVertex4fSUN(float arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6, float arg7) {
  static void(*ptr)(float,float,float,float,float,float,float,float) = NULL;
  static const char name[] = "glTexCoord4fVertex4fSUN";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glTexCoord4fVertex4fvSUN(float* arg0, float* arg1) {
  static void(*ptr)(float*,float*) = NULL;
  static const char name[] = "glTexCoord4fVertex4fvSUN";
  if (!ptr) {
    ptr = (void(*)(float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTexCoord2fColor4ubVertex3fSUN(float arg0, float arg1, char arg2, char arg3, char arg4, char arg5, float arg6, float arg7, float arg8) {
  static void(*ptr)(float,float,char,char,char,char,float,float,float) = NULL;
  static const char name[] = "glTexCoord2fColor4ubVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(float,float,char,char,char,char,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glTexCoord2fColor4ubVertex3fvSUN(float* arg0, char* arg1, float* arg2) {
  static void(*ptr)(float*,char*,float*) = NULL;
  static const char name[] = "glTexCoord2fColor4ubVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(float*,char*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexCoord2fColor3fVertex3fSUN(float arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6, float arg7) {
  static void(*ptr)(float,float,float,float,float,float,float,float) = NULL;
  static const char name[] = "glTexCoord2fColor3fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glTexCoord2fColor3fVertex3fvSUN(float* arg0, float* arg1, float* arg2) {
  static void(*ptr)(float*,float*,float*) = NULL;
  static const char name[] = "glTexCoord2fColor3fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(float*,float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexCoord2fNormal3fVertex3fSUN(float arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6, float arg7) {
  static void(*ptr)(float,float,float,float,float,float,float,float) = NULL;
  static const char name[] = "glTexCoord2fNormal3fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glTexCoord2fNormal3fVertex3fvSUN(float* arg0, float* arg1, float* arg2) {
  static void(*ptr)(float*,float*,float*) = NULL;
  static const char name[] = "glTexCoord2fNormal3fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(float*,float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexCoord2fColor4fNormal3fVertex3fSUN(float arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6, float arg7, float arg8, float arg9, float arg10, float arg11) {
  static void(*ptr)(float,float,float,float,float,float,float,float,float,float,float,float) = NULL;
  static const char name[] = "glTexCoord2fColor4fNormal3fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float,float,float,float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11);
}

void PureGL_glTexCoord2fColor4fNormal3fVertex3fvSUN(float* arg0, float* arg1, float* arg2, float* arg3) {
  static void(*ptr)(float*,float*,float*,float*) = NULL;
  static const char name[] = "glTexCoord2fColor4fNormal3fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(float*,float*,float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glTexCoord4fColor4fNormal3fVertex4fSUN(float arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6, float arg7, float arg8, float arg9, float arg10, float arg11, float arg12, float arg13, float arg14) {
  static void(*ptr)(float,float,float,float,float,float,float,float,float,float,float,float,float,float,float) = NULL;
  static const char name[] = "glTexCoord4fColor4fNormal3fVertex4fSUN";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float,float,float,float,float,float,float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14);
}

void PureGL_glTexCoord4fColor4fNormal3fVertex4fvSUN(float* arg0, float* arg1, float* arg2, float* arg3) {
  static void(*ptr)(float*,float*,float*,float*) = NULL;
  static const char name[] = "glTexCoord4fColor4fNormal3fVertex4fvSUN";
  if (!ptr) {
    ptr = (void(*)(float*,float*,float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glReplacementCodeuiVertex3fSUN(int arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(int,float,float,float) = NULL;
  static const char name[] = "glReplacementCodeuiVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glReplacementCodeuiVertex3fvSUN(int* arg0, float* arg1) {
  static void(*ptr)(int*,float*) = NULL;
  static const char name[] = "glReplacementCodeuiVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(int*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glReplacementCodeuiColor4ubVertex3fSUN(int arg0, char arg1, char arg2, char arg3, char arg4, float arg5, float arg6, float arg7) {
  static void(*ptr)(int,char,char,char,char,float,float,float) = NULL;
  static const char name[] = "glReplacementCodeuiColor4ubVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(int,char,char,char,char,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
}

void PureGL_glReplacementCodeuiColor4ubVertex3fvSUN(int* arg0, char* arg1, float* arg2) {
  static void(*ptr)(int*,char*,float*) = NULL;
  static const char name[] = "glReplacementCodeuiColor4ubVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(int*,char*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glReplacementCodeuiColor3fVertex3fSUN(int arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6) {
  static void(*ptr)(int,float,float,float,float,float,float) = NULL;
  static const char name[] = "glReplacementCodeuiColor3fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glReplacementCodeuiColor3fVertex3fvSUN(int* arg0, float* arg1, float* arg2) {
  static void(*ptr)(int*,float*,float*) = NULL;
  static const char name[] = "glReplacementCodeuiColor3fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(int*,float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glReplacementCodeuiNormal3fVertex3fSUN(int arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6) {
  static void(*ptr)(int,float,float,float,float,float,float) = NULL;
  static const char name[] = "glReplacementCodeuiNormal3fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glReplacementCodeuiNormal3fVertex3fvSUN(int* arg0, float* arg1, float* arg2) {
  static void(*ptr)(int*,float*,float*) = NULL;
  static const char name[] = "glReplacementCodeuiNormal3fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(int*,float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glReplacementCodeuiColor4fNormal3fVertex3fSUN(int arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6, float arg7, float arg8, float arg9, float arg10) {
  static void(*ptr)(int,float,float,float,float,float,float,float,float,float,float) = NULL;
  static const char name[] = "glReplacementCodeuiColor4fNormal3fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float,float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10);
}

void PureGL_glReplacementCodeuiColor4fNormal3fVertex3fvSUN(int* arg0, float* arg1, float* arg2, float* arg3) {
  static void(*ptr)(int*,float*,float*,float*) = NULL;
  static const char name[] = "glReplacementCodeuiColor4fNormal3fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(int*,float*,float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glReplacementCodeuiTexCoord2fVertex3fSUN(int arg0, float arg1, float arg2, float arg3, float arg4, float arg5) {
  static void(*ptr)(int,float,float,float,float,float) = NULL;
  static const char name[] = "glReplacementCodeuiTexCoord2fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glReplacementCodeuiTexCoord2fVertex3fvSUN(int* arg0, float* arg1, float* arg2) {
  static void(*ptr)(int*,float*,float*) = NULL;
  static const char name[] = "glReplacementCodeuiTexCoord2fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(int*,float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN(int arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6, float arg7, float arg8) {
  static void(*ptr)(int,float,float,float,float,float,float,float,float) = NULL;
  static const char name[] = "glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN(int* arg0, float* arg1, float* arg2, float* arg3) {
  static void(*ptr)(int*,float*,float*,float*) = NULL;
  static const char name[] = "glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(int*,float*,float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN(int arg0, float arg1, float arg2, float arg3, float arg4, float arg5, float arg6, float arg7, float arg8, float arg9, float arg10, float arg11, float arg12) {
  static void(*ptr)(int,float,float,float,float,float,float,float,float,float,float,float,float) = NULL;
  static const char name[] = "glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float,float,float,float,float,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12);
}

void PureGL_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN(int* arg0, float* arg1, float* arg2, float* arg3, float* arg4) {
  static void(*ptr)(int*,float*,float*,float*,float*) = NULL;
  static const char name[] = "glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN";
  if (!ptr) {
    ptr = (void(*)(int*,float*,float*,float*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glBlendFuncSeparateEXT(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glBlendFuncSeparateEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glBlendFuncSeparateINGR(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glBlendFuncSeparateINGR";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexWeightfEXT(float arg0) {
  static void(*ptr)(float) = NULL;
  static const char name[] = "glVertexWeightfEXT";
  if (!ptr) {
    ptr = (void(*)(float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertexWeightfvEXT(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glVertexWeightfvEXT";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertexWeightPointerEXT(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glVertexWeightPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glFlushVertexArrayRangeNV() {
  static void(*ptr)() = NULL;
  static const char name[] = "glFlushVertexArrayRangeNV";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glVertexArrayRangeNV(int arg0, void* arg1) {
  static void(*ptr)(int,void*) = NULL;
  static const char name[] = "glVertexArrayRangeNV";
  if (!ptr) {
    ptr = (void(*)(int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glCombinerParameterfvNV(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glCombinerParameterfvNV";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glCombinerParameterfNV(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glCombinerParameterfNV";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glCombinerParameterivNV(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glCombinerParameterivNV";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glCombinerParameteriNV(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glCombinerParameteriNV";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glCombinerInputNV(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glCombinerInputNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glCombinerOutputNV(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, char arg7, char arg8, char arg9) {
  static void(*ptr)(int,int,int,int,int,int,int,char,char,char) = NULL;
  static const char name[] = "glCombinerOutputNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
}

void PureGL_glFinalCombinerInputNV(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glFinalCombinerInputNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetCombinerInputParameterfvNV(int arg0, int arg1, int arg2, int arg3, float* arg4) {
  static void(*ptr)(int,int,int,int,float*) = NULL;
  static const char name[] = "glGetCombinerInputParameterfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetCombinerInputParameterivNV(int arg0, int arg1, int arg2, int arg3, int* arg4) {
  static void(*ptr)(int,int,int,int,int*) = NULL;
  static const char name[] = "glGetCombinerInputParameterivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetCombinerOutputParameterfvNV(int arg0, int arg1, int arg2, float* arg3) {
  static void(*ptr)(int,int,int,float*) = NULL;
  static const char name[] = "glGetCombinerOutputParameterfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetCombinerOutputParameterivNV(int arg0, int arg1, int arg2, int* arg3) {
  static void(*ptr)(int,int,int,int*) = NULL;
  static const char name[] = "glGetCombinerOutputParameterivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetFinalCombinerInputParameterfvNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetFinalCombinerInputParameterfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetFinalCombinerInputParameterivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetFinalCombinerInputParameterivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glResizeBuffersMESA() {
  static void(*ptr)() = NULL;
  static const char name[] = "glResizeBuffersMESA";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glWindowPos2dMESA(double arg0, double arg1) {
  static void(*ptr)(double,double) = NULL;
  static const char name[] = "glWindowPos2dMESA";
  if (!ptr) {
    ptr = (void(*)(double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2dvMESA(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glWindowPos2dvMESA";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos2fMESA(float arg0, float arg1) {
  static void(*ptr)(float,float) = NULL;
  static const char name[] = "glWindowPos2fMESA";
  if (!ptr) {
    ptr = (void(*)(float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2fvMESA(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glWindowPos2fvMESA";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos2iMESA(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glWindowPos2iMESA";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2ivMESA(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glWindowPos2ivMESA";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos2sMESA(short arg0, short arg1) {
  static void(*ptr)(short,short) = NULL;
  static const char name[] = "glWindowPos2sMESA";
  if (!ptr) {
    ptr = (void(*)(short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glWindowPos2svMESA(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glWindowPos2svMESA";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3dMESA(double arg0, double arg1, double arg2) {
  static void(*ptr)(double,double,double) = NULL;
  static const char name[] = "glWindowPos3dMESA";
  if (!ptr) {
    ptr = (void(*)(double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3dvMESA(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glWindowPos3dvMESA";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3fMESA(float arg0, float arg1, float arg2) {
  static void(*ptr)(float,float,float) = NULL;
  static const char name[] = "glWindowPos3fMESA";
  if (!ptr) {
    ptr = (void(*)(float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3fvMESA(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glWindowPos3fvMESA";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3iMESA(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glWindowPos3iMESA";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3ivMESA(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glWindowPos3ivMESA";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos3sMESA(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glWindowPos3sMESA";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glWindowPos3svMESA(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glWindowPos3svMESA";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos4dMESA(double arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(double,double,double,double) = NULL;
  static const char name[] = "glWindowPos4dMESA";
  if (!ptr) {
    ptr = (void(*)(double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glWindowPos4dvMESA(double* arg0) {
  static void(*ptr)(double*) = NULL;
  static const char name[] = "glWindowPos4dvMESA";
  if (!ptr) {
    ptr = (void(*)(double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos4fMESA(float arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(float,float,float,float) = NULL;
  static const char name[] = "glWindowPos4fMESA";
  if (!ptr) {
    ptr = (void(*)(float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glWindowPos4fvMESA(float* arg0) {
  static void(*ptr)(float*) = NULL;
  static const char name[] = "glWindowPos4fvMESA";
  if (!ptr) {
    ptr = (void(*)(float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos4iMESA(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glWindowPos4iMESA";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glWindowPos4ivMESA(int* arg0) {
  static void(*ptr)(int*) = NULL;
  static const char name[] = "glWindowPos4ivMESA";
  if (!ptr) {
    ptr = (void(*)(int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glWindowPos4sMESA(short arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(short,short,short,short) = NULL;
  static const char name[] = "glWindowPos4sMESA";
  if (!ptr) {
    ptr = (void(*)(short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glWindowPos4svMESA(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glWindowPos4svMESA";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMultiModeDrawArraysIBM(int* arg0, int* arg1, int* arg2, int arg3, int arg4) {
  static void(*ptr)(int*,int*,int*,int,int) = NULL;
  static const char name[] = "glMultiModeDrawArraysIBM";
  if (!ptr) {
    ptr = (void(*)(int*,int*,int*,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glMultiModeDrawElementsIBM(int* arg0, int* arg1, int arg2, void** arg3, int arg4, int arg5) {
  static void(*ptr)(int*,int*,int,void**,int,int) = NULL;
  static const char name[] = "glMultiModeDrawElementsIBM";
  if (!ptr) {
    ptr = (void(*)(int*,int*,int,void**,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glColorPointerListIBM(int arg0, int arg1, int arg2, void** arg3, int arg4) {
  static void(*ptr)(int,int,int,void**,int) = NULL;
  static const char name[] = "glColorPointerListIBM";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void**,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glSecondaryColorPointerListIBM(int arg0, int arg1, int arg2, void** arg3, int arg4) {
  static void(*ptr)(int,int,int,void**,int) = NULL;
  static const char name[] = "glSecondaryColorPointerListIBM";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void**,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glEdgeFlagPointerListIBM(int arg0, char** arg1, int arg2) {
  static void(*ptr)(int,char**,int) = NULL;
  static const char name[] = "glEdgeFlagPointerListIBM";
  if (!ptr) {
    ptr = (void(*)(int,char**,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFogCoordPointerListIBM(int arg0, int arg1, void** arg2, int arg3) {
  static void(*ptr)(int,int,void**,int) = NULL;
  static const char name[] = "glFogCoordPointerListIBM";
  if (!ptr) {
    ptr = (void(*)(int,int,void**,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glIndexPointerListIBM(int arg0, int arg1, void** arg2, int arg3) {
  static void(*ptr)(int,int,void**,int) = NULL;
  static const char name[] = "glIndexPointerListIBM";
  if (!ptr) {
    ptr = (void(*)(int,int,void**,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glNormalPointerListIBM(int arg0, int arg1, void** arg2, int arg3) {
  static void(*ptr)(int,int,void**,int) = NULL;
  static const char name[] = "glNormalPointerListIBM";
  if (!ptr) {
    ptr = (void(*)(int,int,void**,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glTexCoordPointerListIBM(int arg0, int arg1, int arg2, void** arg3, int arg4) {
  static void(*ptr)(int,int,int,void**,int) = NULL;
  static const char name[] = "glTexCoordPointerListIBM";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void**,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexPointerListIBM(int arg0, int arg1, int arg2, void** arg3, int arg4) {
  static void(*ptr)(int,int,int,void**,int) = NULL;
  static const char name[] = "glVertexPointerListIBM";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void**,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glTbufferMask3DFX(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glTbufferMask3DFX";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSampleMaskEXT(float arg0, char arg1) {
  static void(*ptr)(float,char) = NULL;
  static const char name[] = "glSampleMaskEXT";
  if (!ptr) {
    ptr = (void(*)(float,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glSamplePatternEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glSamplePatternEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTextureColorMaskSGIS(char arg0, char arg1, char arg2, char arg3) {
  static void(*ptr)(char,char,char,char) = NULL;
  static const char name[] = "glTextureColorMaskSGIS";
  if (!ptr) {
    ptr = (void(*)(char,char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glIglooInterfaceSGIX(int arg0, void* arg1) {
  static void(*ptr)(int,void*) = NULL;
  static const char name[] = "glIglooInterfaceSGIX";
  if (!ptr) {
    ptr = (void(*)(int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteFencesNV(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteFencesNV";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGenFencesNV(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenFencesNV";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glIsFenceNV(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsFenceNV";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

char PureGL_glTestFenceNV(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glTestFenceNV";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glGetFenceivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetFenceivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFinishFenceNV(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glFinishFenceNV";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSetFenceNV(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glSetFenceNV";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMapControlPointsNV(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, char arg7, void* arg8) {
  static void(*ptr)(int,int,int,int,int,int,int,char,void*) = NULL;
  static const char name[] = "glMapControlPointsNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,char,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glMapParameterivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glMapParameterivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMapParameterfvNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glMapParameterfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMapControlPointsNV(int arg0, int arg1, int arg2, int arg3, int arg4, char arg5, void* arg6) {
  static void(*ptr)(int,int,int,int,int,char,void*) = NULL;
  static const char name[] = "glGetMapControlPointsNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,char,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glGetMapParameterivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetMapParameterivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMapParameterfvNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetMapParameterfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetMapAttribParameterivNV(int arg0, int arg1, int arg2, int* arg3) {
  static void(*ptr)(int,int,int,int*) = NULL;
  static const char name[] = "glGetMapAttribParameterivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetMapAttribParameterfvNV(int arg0, int arg1, int arg2, float* arg3) {
  static void(*ptr)(int,int,int,float*) = NULL;
  static const char name[] = "glGetMapAttribParameterfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glEvalMapsNV(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glEvalMapsNV";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glCombinerStageParameterfvNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glCombinerStageParameterfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetCombinerStageParameterfvNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetCombinerStageParameterfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

char PureGL_glAreProgramsResidentNV(int arg0, int* arg1, char* arg2) {
  static char(*ptr)(int,int*,char*) = NULL;
  static const char name[] = "glAreProgramsResidentNV";
  if (!ptr) {
    ptr = (char(*)(int,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBindProgramNV(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBindProgramNV";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteProgramsNV(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteProgramsNV";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glExecuteProgramNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glExecuteProgramNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGenProgramsNV(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenProgramsNV";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetProgramParameterdvNV(int arg0, int arg1, int arg2, double* arg3) {
  static void(*ptr)(int,int,int,double*) = NULL;
  static const char name[] = "glGetProgramParameterdvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetProgramParameterfvNV(int arg0, int arg1, int arg2, float* arg3) {
  static void(*ptr)(int,int,int,float*) = NULL;
  static const char name[] = "glGetProgramParameterfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetProgramivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetProgramivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetProgramStringNV(int arg0, int arg1, char* arg2) {
  static void(*ptr)(int,int,char*) = NULL;
  static const char name[] = "glGetProgramStringNV";
  if (!ptr) {
    ptr = (void(*)(int,int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetTrackMatrixivNV(int arg0, int arg1, int arg2, int* arg3) {
  static void(*ptr)(int,int,int,int*) = NULL;
  static const char name[] = "glGetTrackMatrixivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetVertexAttribdvNV(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glGetVertexAttribdvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribfvNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetVertexAttribfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetVertexAttribivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribPointervNV(int arg0, int arg1, void** arg2) {
  static void(*ptr)(int,int,void**) = NULL;
  static const char name[] = "glGetVertexAttribPointervNV";
  if (!ptr) {
    ptr = (void(*)(int,int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

char PureGL_glIsProgramNV(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsProgramNV";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glLoadProgramNV(int arg0, int arg1, int arg2, char* arg3) {
  static void(*ptr)(int,int,int,char*) = NULL;
  static const char name[] = "glLoadProgramNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glProgramParameter4dNV(int arg0, int arg1, double arg2, double arg3, double arg4, double arg5) {
  static void(*ptr)(int,int,double,double,double,double) = NULL;
  static const char name[] = "glProgramParameter4dNV";
  if (!ptr) {
    ptr = (void(*)(int,int,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glProgramParameter4dvNV(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glProgramParameter4dvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glProgramParameter4fNV(int arg0, int arg1, float arg2, float arg3, float arg4, float arg5) {
  static void(*ptr)(int,int,float,float,float,float) = NULL;
  static const char name[] = "glProgramParameter4fNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glProgramParameter4fvNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glProgramParameter4fvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glProgramParameters4dvNV(int arg0, int arg1, int arg2, double* arg3) {
  static void(*ptr)(int,int,int,double*) = NULL;
  static const char name[] = "glProgramParameters4dvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glProgramParameters4fvNV(int arg0, int arg1, int arg2, float* arg3) {
  static void(*ptr)(int,int,int,float*) = NULL;
  static const char name[] = "glProgramParameters4fvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glRequestResidentProgramsNV(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glRequestResidentProgramsNV";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTrackMatrixNV(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glTrackMatrixNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttribPointerNV(int arg0, int arg1, int arg2, int arg3, void* arg4) {
  static void(*ptr)(int,int,int,int,void*) = NULL;
  static const char name[] = "glVertexAttribPointerNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib1dNV(int arg0, double arg1) {
  static void(*ptr)(int,double) = NULL;
  static const char name[] = "glVertexAttrib1dNV";
  if (!ptr) {
    ptr = (void(*)(int,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1dvNV(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib1dvNV";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1fNV(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glVertexAttrib1fNV";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1fvNV(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib1fvNV";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1sNV(int arg0, short arg1) {
  static void(*ptr)(int,short) = NULL;
  static const char name[] = "glVertexAttrib1sNV";
  if (!ptr) {
    ptr = (void(*)(int,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1svNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib1svNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib2dNV(int arg0, double arg1, double arg2) {
  static void(*ptr)(int,double,double) = NULL;
  static const char name[] = "glVertexAttrib2dNV";
  if (!ptr) {
    ptr = (void(*)(int,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttrib2dvNV(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib2dvNV";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib2fNV(int arg0, float arg1, float arg2) {
  static void(*ptr)(int,float,float) = NULL;
  static const char name[] = "glVertexAttrib2fNV";
  if (!ptr) {
    ptr = (void(*)(int,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttrib2fvNV(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib2fvNV";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib2sNV(int arg0, short arg1, short arg2) {
  static void(*ptr)(int,short,short) = NULL;
  static const char name[] = "glVertexAttrib2sNV";
  if (!ptr) {
    ptr = (void(*)(int,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttrib2svNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib2svNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib3dNV(int arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(int,double,double,double) = NULL;
  static const char name[] = "glVertexAttrib3dNV";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttrib3dvNV(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib3dvNV";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib3fNV(int arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(int,float,float,float) = NULL;
  static const char name[] = "glVertexAttrib3fNV";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttrib3fvNV(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib3fvNV";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib3sNV(int arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(int,short,short,short) = NULL;
  static const char name[] = "glVertexAttrib3sNV";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttrib3svNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib3svNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4dNV(int arg0, double arg1, double arg2, double arg3, double arg4) {
  static void(*ptr)(int,double,double,double,double) = NULL;
  static const char name[] = "glVertexAttrib4dNV";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4dvNV(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexAttrib4dvNV";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4fNV(int arg0, float arg1, float arg2, float arg3, float arg4) {
  static void(*ptr)(int,float,float,float,float) = NULL;
  static const char name[] = "glVertexAttrib4fNV";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4fvNV(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexAttrib4fvNV";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4sNV(int arg0, short arg1, short arg2, short arg3, short arg4) {
  static void(*ptr)(int,short,short,short,short) = NULL;
  static const char name[] = "glVertexAttrib4sNV";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4svNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib4svNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4ubNV(int arg0, char arg1, char arg2, char arg3, char arg4) {
  static void(*ptr)(int,char,char,char,char) = NULL;
  static const char name[] = "glVertexAttrib4ubNV";
  if (!ptr) {
    ptr = (void(*)(int,char,char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4ubvNV(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVertexAttrib4ubvNV";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribs1dvNV(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glVertexAttribs1dvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs1fvNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glVertexAttribs1fvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs1svNV(int arg0, int arg1, short* arg2) {
  static void(*ptr)(int,int,short*) = NULL;
  static const char name[] = "glVertexAttribs1svNV";
  if (!ptr) {
    ptr = (void(*)(int,int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs2dvNV(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glVertexAttribs2dvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs2fvNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glVertexAttribs2fvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs2svNV(int arg0, int arg1, short* arg2) {
  static void(*ptr)(int,int,short*) = NULL;
  static const char name[] = "glVertexAttribs2svNV";
  if (!ptr) {
    ptr = (void(*)(int,int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs3dvNV(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glVertexAttribs3dvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs3fvNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glVertexAttribs3fvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs3svNV(int arg0, int arg1, short* arg2) {
  static void(*ptr)(int,int,short*) = NULL;
  static const char name[] = "glVertexAttribs3svNV";
  if (!ptr) {
    ptr = (void(*)(int,int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs4dvNV(int arg0, int arg1, double* arg2) {
  static void(*ptr)(int,int,double*) = NULL;
  static const char name[] = "glVertexAttribs4dvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs4fvNV(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glVertexAttribs4fvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs4svNV(int arg0, int arg1, short* arg2) {
  static void(*ptr)(int,int,short*) = NULL;
  static const char name[] = "glVertexAttribs4svNV";
  if (!ptr) {
    ptr = (void(*)(int,int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs4ubvNV(int arg0, int arg1, char* arg2) {
  static void(*ptr)(int,int,char*) = NULL;
  static const char name[] = "glVertexAttribs4ubvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexBumpParameterivATI(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glTexBumpParameterivATI";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTexBumpParameterfvATI(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glTexBumpParameterfvATI";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetTexBumpParameterivATI(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGetTexBumpParameterivATI";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetTexBumpParameterfvATI(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glGetTexBumpParameterfvATI";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glGenFragmentShadersATI(int arg0) {
  static int(*ptr)(int) = NULL;
  static const char name[] = "glGenFragmentShadersATI";
  if (!ptr) {
    ptr = (int(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBindFragmentShaderATI(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glBindFragmentShaderATI";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDeleteFragmentShaderATI(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDeleteFragmentShaderATI";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBeginFragmentShaderATI() {
  static void(*ptr)() = NULL;
  static const char name[] = "glBeginFragmentShaderATI";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glEndFragmentShaderATI() {
  static void(*ptr)() = NULL;
  static const char name[] = "glEndFragmentShaderATI";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glPassTexCoordATI(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glPassTexCoordATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSampleMapATI(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glSampleMapATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColorFragmentOp1ATI(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6) {
  static void(*ptr)(int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glColorFragmentOp1ATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glColorFragmentOp2ATI(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glColorFragmentOp2ATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
}

void PureGL_glColorFragmentOp3ATI(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9, int arg10, int arg11, int arg12) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glColorFragmentOp3ATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12);
}

void PureGL_glAlphaFragmentOp1ATI(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glAlphaFragmentOp1ATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glAlphaFragmentOp2ATI(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glAlphaFragmentOp2ATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
}

void PureGL_glAlphaFragmentOp3ATI(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9, int arg10, int arg11) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glAlphaFragmentOp3ATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11);
}

void PureGL_glSetFragmentShaderConstantATI(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glSetFragmentShaderConstantATI";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPNTrianglesiATI(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glPNTrianglesiATI";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPNTrianglesfATI(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glPNTrianglesfATI";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glNewObjectBufferATI(int arg0, void* arg1, int arg2) {
  static int(*ptr)(int,void*,int) = NULL;
  static const char name[] = "glNewObjectBufferATI";
  if (!ptr) {
    ptr = (int(*)(int,void*,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

char PureGL_glIsObjectBufferATI(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsObjectBufferATI";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glUpdateObjectBufferATI(int arg0, int arg1, int arg2, void* arg3, int arg4) {
  static void(*ptr)(int,int,int,void*,int) = NULL;
  static const char name[] = "glUpdateObjectBufferATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetObjectBufferfvATI(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetObjectBufferfvATI";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetObjectBufferivATI(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetObjectBufferivATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFreeObjectBufferATI(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glFreeObjectBufferATI";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glArrayObjectATI(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glArrayObjectATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glGetArrayObjectfvATI(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetArrayObjectfvATI";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetArrayObjectivATI(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetArrayObjectivATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVariantArrayObjectATI(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glVariantArrayObjectATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetVariantArrayObjectfvATI(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetVariantArrayObjectfvATI";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVariantArrayObjectivATI(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetVariantArrayObjectivATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBeginVertexShaderEXT() {
  static void(*ptr)() = NULL;
  static const char name[] = "glBeginVertexShaderEXT";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glEndVertexShaderEXT() {
  static void(*ptr)() = NULL;
  static const char name[] = "glEndVertexShaderEXT";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glBindVertexShaderEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glBindVertexShaderEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int PureGL_glGenVertexShadersEXT(int arg0) {
  static int(*ptr)(int) = NULL;
  static const char name[] = "glGenVertexShadersEXT";
  if (!ptr) {
    ptr = (int(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDeleteVertexShaderEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDeleteVertexShaderEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glShaderOp1EXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glShaderOp1EXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glShaderOp2EXT(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glShaderOp2EXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glShaderOp3EXT(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glShaderOp3EXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glSwizzleEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glSwizzleEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glWriteMaskEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glWriteMaskEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glInsertComponentEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glInsertComponentEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glExtractComponentEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glExtractComponentEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

int PureGL_glGenSymbolsEXT(int arg0, int arg1, int arg2, int arg3) {
  static int(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glGenSymbolsEXT";
  if (!ptr) {
    ptr = (int(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glSetInvariantEXT(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glSetInvariantEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSetLocalConstantEXT(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glSetLocalConstantEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVariantbvEXT(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVariantbvEXT";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVariantsvEXT(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVariantsvEXT";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVariantivEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVariantivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVariantfvEXT(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVariantfvEXT";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVariantdvEXT(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVariantdvEXT";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVariantubvEXT(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVariantubvEXT";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVariantusvEXT(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVariantusvEXT";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVariantuivEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVariantuivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVariantPointerEXT(int arg0, int arg1, int arg2, void* arg3) {
  static void(*ptr)(int,int,int,void*) = NULL;
  static const char name[] = "glVariantPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glEnableVariantClientStateEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glEnableVariantClientStateEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDisableVariantClientStateEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glDisableVariantClientStateEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int PureGL_glBindLightParameterEXT(int arg0, int arg1) {
  static int(*ptr)(int,int) = NULL;
  static const char name[] = "glBindLightParameterEXT";
  if (!ptr) {
    ptr = (int(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glBindMaterialParameterEXT(int arg0, int arg1) {
  static int(*ptr)(int,int) = NULL;
  static const char name[] = "glBindMaterialParameterEXT";
  if (!ptr) {
    ptr = (int(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glBindTexGenParameterEXT(int arg0, int arg1, int arg2) {
  static int(*ptr)(int,int,int) = NULL;
  static const char name[] = "glBindTexGenParameterEXT";
  if (!ptr) {
    ptr = (int(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

int PureGL_glBindTextureUnitParameterEXT(int arg0, int arg1) {
  static int(*ptr)(int,int) = NULL;
  static const char name[] = "glBindTextureUnitParameterEXT";
  if (!ptr) {
    ptr = (int(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glBindParameterEXT(int arg0) {
  static int(*ptr)(int) = NULL;
  static const char name[] = "glBindParameterEXT";
  if (!ptr) {
    ptr = (int(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

char PureGL_glIsVariantEnabledEXT(int arg0, int arg1) {
  static char(*ptr)(int,int) = NULL;
  static const char name[] = "glIsVariantEnabledEXT";
  if (!ptr) {
    ptr = (char(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetVariantBooleanvEXT(int arg0, int arg1, char* arg2) {
  static void(*ptr)(int,int,char*) = NULL;
  static const char name[] = "glGetVariantBooleanvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVariantIntegervEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetVariantIntegervEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVariantFloatvEXT(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetVariantFloatvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVariantPointervEXT(int arg0, int arg1, void** arg2) {
  static void(*ptr)(int,int,void**) = NULL;
  static const char name[] = "glGetVariantPointervEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,void**))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetInvariantBooleanvEXT(int arg0, int arg1, char* arg2) {
  static void(*ptr)(int,int,char*) = NULL;
  static const char name[] = "glGetInvariantBooleanvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetInvariantIntegervEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetInvariantIntegervEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetInvariantFloatvEXT(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetInvariantFloatvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetLocalConstantBooleanvEXT(int arg0, int arg1, char* arg2) {
  static void(*ptr)(int,int,char*) = NULL;
  static const char name[] = "glGetLocalConstantBooleanvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetLocalConstantIntegervEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetLocalConstantIntegervEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetLocalConstantFloatvEXT(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetLocalConstantFloatvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexStream1sATI(int arg0, short arg1) {
  static void(*ptr)(int,short) = NULL;
  static const char name[] = "glVertexStream1sATI";
  if (!ptr) {
    ptr = (void(*)(int,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream1svATI(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexStream1svATI";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream1iATI(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glVertexStream1iATI";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream1ivATI(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexStream1ivATI";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream1fATI(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glVertexStream1fATI";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream1fvATI(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexStream1fvATI";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream1dATI(int arg0, double arg1) {
  static void(*ptr)(int,double) = NULL;
  static const char name[] = "glVertexStream1dATI";
  if (!ptr) {
    ptr = (void(*)(int,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream1dvATI(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexStream1dvATI";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream2sATI(int arg0, short arg1, short arg2) {
  static void(*ptr)(int,short,short) = NULL;
  static const char name[] = "glVertexStream2sATI";
  if (!ptr) {
    ptr = (void(*)(int,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexStream2svATI(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexStream2svATI";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream2iATI(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glVertexStream2iATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexStream2ivATI(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexStream2ivATI";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream2fATI(int arg0, float arg1, float arg2) {
  static void(*ptr)(int,float,float) = NULL;
  static const char name[] = "glVertexStream2fATI";
  if (!ptr) {
    ptr = (void(*)(int,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexStream2fvATI(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexStream2fvATI";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream2dATI(int arg0, double arg1, double arg2) {
  static void(*ptr)(int,double,double) = NULL;
  static const char name[] = "glVertexStream2dATI";
  if (!ptr) {
    ptr = (void(*)(int,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexStream2dvATI(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexStream2dvATI";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream3sATI(int arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(int,short,short,short) = NULL;
  static const char name[] = "glVertexStream3sATI";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexStream3svATI(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexStream3svATI";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream3iATI(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glVertexStream3iATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexStream3ivATI(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexStream3ivATI";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream3fATI(int arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(int,float,float,float) = NULL;
  static const char name[] = "glVertexStream3fATI";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexStream3fvATI(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexStream3fvATI";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream3dATI(int arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(int,double,double,double) = NULL;
  static const char name[] = "glVertexStream3dATI";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexStream3dvATI(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexStream3dvATI";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream4sATI(int arg0, short arg1, short arg2, short arg3, short arg4) {
  static void(*ptr)(int,short,short,short,short) = NULL;
  static const char name[] = "glVertexStream4sATI";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexStream4svATI(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexStream4svATI";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream4iATI(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glVertexStream4iATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexStream4ivATI(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexStream4ivATI";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream4fATI(int arg0, float arg1, float arg2, float arg3, float arg4) {
  static void(*ptr)(int,float,float,float,float) = NULL;
  static const char name[] = "glVertexStream4fATI";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexStream4fvATI(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glVertexStream4fvATI";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexStream4dATI(int arg0, double arg1, double arg2, double arg3, double arg4) {
  static void(*ptr)(int,double,double,double,double) = NULL;
  static const char name[] = "glVertexStream4dATI";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexStream4dvATI(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glVertexStream4dvATI";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glNormalStream3bATI(int arg0, char arg1, char arg2, char arg3) {
  static void(*ptr)(int,char,char,char) = NULL;
  static const char name[] = "glNormalStream3bATI";
  if (!ptr) {
    ptr = (void(*)(int,char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glNormalStream3bvATI(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glNormalStream3bvATI";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glNormalStream3sATI(int arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(int,short,short,short) = NULL;
  static const char name[] = "glNormalStream3sATI";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glNormalStream3svATI(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glNormalStream3svATI";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glNormalStream3iATI(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glNormalStream3iATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glNormalStream3ivATI(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glNormalStream3ivATI";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glNormalStream3fATI(int arg0, float arg1, float arg2, float arg3) {
  static void(*ptr)(int,float,float,float) = NULL;
  static const char name[] = "glNormalStream3fATI";
  if (!ptr) {
    ptr = (void(*)(int,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glNormalStream3fvATI(int arg0, float* arg1) {
  static void(*ptr)(int,float*) = NULL;
  static const char name[] = "glNormalStream3fvATI";
  if (!ptr) {
    ptr = (void(*)(int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glNormalStream3dATI(int arg0, double arg1, double arg2, double arg3) {
  static void(*ptr)(int,double,double,double) = NULL;
  static const char name[] = "glNormalStream3dATI";
  if (!ptr) {
    ptr = (void(*)(int,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glNormalStream3dvATI(int arg0, double* arg1) {
  static void(*ptr)(int,double*) = NULL;
  static const char name[] = "glNormalStream3dvATI";
  if (!ptr) {
    ptr = (void(*)(int,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glClientActiveVertexStreamATI(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glClientActiveVertexStreamATI";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertexBlendEnviATI(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glVertexBlendEnviATI";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexBlendEnvfATI(int arg0, float arg1) {
  static void(*ptr)(int,float) = NULL;
  static const char name[] = "glVertexBlendEnvfATI";
  if (!ptr) {
    ptr = (void(*)(int,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glElementPointerATI(int arg0, void* arg1) {
  static void(*ptr)(int,void*) = NULL;
  static const char name[] = "glElementPointerATI";
  if (!ptr) {
    ptr = (void(*)(int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDrawElementArrayATI(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glDrawElementArrayATI";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDrawRangeElementArrayATI(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glDrawRangeElementArrayATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glDrawMeshArraysSUN(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glDrawMeshArraysSUN";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGenOcclusionQueriesNV(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenOcclusionQueriesNV";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteOcclusionQueriesNV(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteOcclusionQueriesNV";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glIsOcclusionQueryNV(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsOcclusionQueryNV";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBeginOcclusionQueryNV(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glBeginOcclusionQueryNV";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEndOcclusionQueryNV() {
  static void(*ptr)() = NULL;
  static const char name[] = "glEndOcclusionQueryNV";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glGetOcclusionQueryivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetOcclusionQueryivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetOcclusionQueryuivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetOcclusionQueryuivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPointParameteriNV(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glPointParameteriNV";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glPointParameterivNV(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glPointParameterivNV";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glActiveStencilFaceEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glActiveStencilFaceEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glElementPointerAPPLE(int arg0, void* arg1) {
  static void(*ptr)(int,void*) = NULL;
  static const char name[] = "glElementPointerAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDrawElementArrayAPPLE(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glDrawElementArrayAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glDrawRangeElementArrayAPPLE(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glDrawRangeElementArrayAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glMultiDrawElementArrayAPPLE(int arg0, int* arg1, int* arg2, int arg3) {
  static void(*ptr)(int,int*,int*,int) = NULL;
  static const char name[] = "glMultiDrawElementArrayAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,int*,int*,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiDrawRangeElementArrayAPPLE(int arg0, int arg1, int arg2, int* arg3, int* arg4, int arg5) {
  static void(*ptr)(int,int,int,int*,int*,int) = NULL;
  static const char name[] = "glMultiDrawRangeElementArrayAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*,int*,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glGenFencesAPPLE(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenFencesAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteFencesAPPLE(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteFencesAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glSetFenceAPPLE(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glSetFenceAPPLE";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

char PureGL_glIsFenceAPPLE(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsFenceAPPLE";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

char PureGL_glTestFenceAPPLE(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glTestFenceAPPLE";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFinishFenceAPPLE(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glFinishFenceAPPLE";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

char PureGL_glTestObjectAPPLE(int arg0, int arg1) {
  static char(*ptr)(int,int) = NULL;
  static const char name[] = "glTestObjectAPPLE";
  if (!ptr) {
    ptr = (char(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFinishObjectAPPLE(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glFinishObjectAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glBindVertexArrayAPPLE(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glBindVertexArrayAPPLE";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDeleteVertexArraysAPPLE(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteVertexArraysAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGenVertexArraysAPPLE(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenVertexArraysAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glIsVertexArrayAPPLE(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsVertexArrayAPPLE";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertexArrayRangeAPPLE(int arg0, void* arg1) {
  static void(*ptr)(int,void*) = NULL;
  static const char name[] = "glVertexArrayRangeAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFlushVertexArrayRangeAPPLE(int arg0, void* arg1) {
  static void(*ptr)(int,void*) = NULL;
  static const char name[] = "glFlushVertexArrayRangeAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexArrayParameteriAPPLE(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glVertexArrayParameteriAPPLE";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDrawBuffersATI(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDrawBuffersATI";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glProgramNamedParameter4fNV(int arg0, int arg1, char* arg2, float arg3, float arg4, float arg5, float arg6) {
  static void(*ptr)(int,int,char*,float,float,float,float) = NULL;
  static const char name[] = "glProgramNamedParameter4fNV";
  if (!ptr) {
    ptr = (void(*)(int,int,char*,float,float,float,float))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glProgramNamedParameter4dNV(int arg0, int arg1, char* arg2, double arg3, double arg4, double arg5, double arg6) {
  static void(*ptr)(int,int,char*,double,double,double,double) = NULL;
  static const char name[] = "glProgramNamedParameter4dNV";
  if (!ptr) {
    ptr = (void(*)(int,int,char*,double,double,double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glProgramNamedParameter4fvNV(int arg0, int arg1, char* arg2, float* arg3) {
  static void(*ptr)(int,int,char*,float*) = NULL;
  static const char name[] = "glProgramNamedParameter4fvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,char*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glProgramNamedParameter4dvNV(int arg0, int arg1, char* arg2, double* arg3) {
  static void(*ptr)(int,int,char*,double*) = NULL;
  static const char name[] = "glProgramNamedParameter4dvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,char*,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetProgramNamedParameterfvNV(int arg0, int arg1, char* arg2, float* arg3) {
  static void(*ptr)(int,int,char*,float*) = NULL;
  static const char name[] = "glGetProgramNamedParameterfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,char*,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetProgramNamedParameterdvNV(int arg0, int arg1, char* arg2, double* arg3) {
  static void(*ptr)(int,int,char*,double*) = NULL;
  static const char name[] = "glGetProgramNamedParameterdvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,char*,double*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertex2hNV(short arg0, short arg1) {
  static void(*ptr)(short,short) = NULL;
  static const char name[] = "glVertex2hNV";
  if (!ptr) {
    ptr = (void(*)(short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertex2hvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glVertex2hvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex3hNV(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glVertex3hNV";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertex3hvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glVertex3hvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertex4hNV(short arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(short,short,short,short) = NULL;
  static const char name[] = "glVertex4hNV";
  if (!ptr) {
    ptr = (void(*)(short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertex4hvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glVertex4hvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glNormal3hNV(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glNormal3hNV";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glNormal3hvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glNormal3hvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor3hNV(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glColor3hNV";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glColor3hvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glColor3hvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glColor4hNV(short arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(short,short,short,short) = NULL;
  static const char name[] = "glColor4hNV";
  if (!ptr) {
    ptr = (void(*)(short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glColor4hvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glColor4hvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord1hNV(short arg0) {
  static void(*ptr)(short) = NULL;
  static const char name[] = "glTexCoord1hNV";
  if (!ptr) {
    ptr = (void(*)(short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord1hvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glTexCoord1hvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord2hNV(short arg0, short arg1) {
  static void(*ptr)(short,short) = NULL;
  static const char name[] = "glTexCoord2hNV";
  if (!ptr) {
    ptr = (void(*)(short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTexCoord2hvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glTexCoord2hvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord3hNV(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glTexCoord3hNV";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexCoord3hvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glTexCoord3hvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glTexCoord4hNV(short arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(short,short,short,short) = NULL;
  static const char name[] = "glTexCoord4hNV";
  if (!ptr) {
    ptr = (void(*)(short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glTexCoord4hvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glTexCoord4hvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glMultiTexCoord1hNV(int arg0, short arg1) {
  static void(*ptr)(int,short) = NULL;
  static const char name[] = "glMultiTexCoord1hNV";
  if (!ptr) {
    ptr = (void(*)(int,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord1hvNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord1hvNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord2hNV(int arg0, short arg1, short arg2) {
  static void(*ptr)(int,short,short) = NULL;
  static const char name[] = "glMultiTexCoord2hNV";
  if (!ptr) {
    ptr = (void(*)(int,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glMultiTexCoord2hvNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord2hvNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord3hNV(int arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(int,short,short,short) = NULL;
  static const char name[] = "glMultiTexCoord3hNV";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glMultiTexCoord3hvNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord3hvNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glMultiTexCoord4hNV(int arg0, short arg1, short arg2, short arg3, short arg4) {
  static void(*ptr)(int,short,short,short,short) = NULL;
  static const char name[] = "glMultiTexCoord4hNV";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glMultiTexCoord4hvNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glMultiTexCoord4hvNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glFogCoordhNV(short arg0) {
  static void(*ptr)(short) = NULL;
  static const char name[] = "glFogCoordhNV";
  if (!ptr) {
    ptr = (void(*)(short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFogCoordhvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glFogCoordhvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glSecondaryColor3hNV(short arg0, short arg1, short arg2) {
  static void(*ptr)(short,short,short) = NULL;
  static const char name[] = "glSecondaryColor3hNV";
  if (!ptr) {
    ptr = (void(*)(short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glSecondaryColor3hvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glSecondaryColor3hvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertexWeighthNV(short arg0) {
  static void(*ptr)(short) = NULL;
  static const char name[] = "glVertexWeighthNV";
  if (!ptr) {
    ptr = (void(*)(short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertexWeighthvNV(short* arg0) {
  static void(*ptr)(short*) = NULL;
  static const char name[] = "glVertexWeighthvNV";
  if (!ptr) {
    ptr = (void(*)(short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glVertexAttrib1hNV(int arg0, short arg1) {
  static void(*ptr)(int,short) = NULL;
  static const char name[] = "glVertexAttrib1hNV";
  if (!ptr) {
    ptr = (void(*)(int,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib1hvNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib1hvNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib2hNV(int arg0, short arg1, short arg2) {
  static void(*ptr)(int,short,short) = NULL;
  static const char name[] = "glVertexAttrib2hNV";
  if (!ptr) {
    ptr = (void(*)(int,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttrib2hvNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib2hvNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib3hNV(int arg0, short arg1, short arg2, short arg3) {
  static void(*ptr)(int,short,short,short) = NULL;
  static const char name[] = "glVertexAttrib3hNV";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttrib3hvNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib3hvNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttrib4hNV(int arg0, short arg1, short arg2, short arg3, short arg4) {
  static void(*ptr)(int,short,short,short,short) = NULL;
  static const char name[] = "glVertexAttrib4hNV";
  if (!ptr) {
    ptr = (void(*)(int,short,short,short,short))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttrib4hvNV(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttrib4hvNV";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribs1hvNV(int arg0, int arg1, short* arg2) {
  static void(*ptr)(int,int,short*) = NULL;
  static const char name[] = "glVertexAttribs1hvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs2hvNV(int arg0, int arg1, short* arg2) {
  static void(*ptr)(int,int,short*) = NULL;
  static const char name[] = "glVertexAttribs2hvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs3hvNV(int arg0, int arg1, short* arg2) {
  static void(*ptr)(int,int,short*) = NULL;
  static const char name[] = "glVertexAttribs3hvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribs4hvNV(int arg0, int arg1, short* arg2) {
  static void(*ptr)(int,int,short*) = NULL;
  static const char name[] = "glVertexAttribs4hvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glPixelDataRangeNV(int arg0, int arg1, void* arg2) {
  static void(*ptr)(int,int,void*) = NULL;
  static const char name[] = "glPixelDataRangeNV";
  if (!ptr) {
    ptr = (void(*)(int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFlushPixelDataRangeNV(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glFlushPixelDataRangeNV";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glPrimitiveRestartNV() {
  static void(*ptr)() = NULL;
  static const char name[] = "glPrimitiveRestartNV";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glPrimitiveRestartIndexNV(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glPrimitiveRestartIndexNV";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void* PureGL_glMapObjectBufferATI(int arg0) {
  static void*(*ptr)(int) = NULL;
  static const char name[] = "glMapObjectBufferATI";
  if (!ptr) {
    ptr = (void*(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glUnmapObjectBufferATI(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glUnmapObjectBufferATI";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glStencilOpSeparateATI(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glStencilOpSeparateATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glStencilFuncSeparateATI(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glStencilFuncSeparateATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttribArrayObjectATI(int arg0, int arg1, int arg2, char arg3, int arg4, int arg5, int arg6) {
  static void(*ptr)(int,int,int,char,int,int,int) = NULL;
  static const char name[] = "glVertexAttribArrayObjectATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int,char,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glGetVertexAttribArrayObjectfvATI(int arg0, int arg1, float* arg2) {
  static void(*ptr)(int,int,float*) = NULL;
  static const char name[] = "glGetVertexAttribArrayObjectfvATI";
  if (!ptr) {
    ptr = (void(*)(int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribArrayObjectivATI(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetVertexAttribArrayObjectivATI";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glDepthBoundsEXT(double arg0, double arg1) {
  static void(*ptr)(double,double) = NULL;
  static const char name[] = "glDepthBoundsEXT";
  if (!ptr) {
    ptr = (void(*)(double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glBlendEquationSeparateEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBlendEquationSeparateEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glIsRenderbufferEXT(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsRenderbufferEXT";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBindRenderbufferEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBindRenderbufferEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteRenderbuffersEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteRenderbuffersEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGenRenderbuffersEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenRenderbuffersEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glRenderbufferStorageEXT(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glRenderbufferStorageEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetRenderbufferParameterivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetRenderbufferParameterivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

char PureGL_glIsFramebufferEXT(int arg0) {
  static char(*ptr)(int) = NULL;
  static const char name[] = "glIsFramebufferEXT";
  if (!ptr) {
    ptr = (char(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glBindFramebufferEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glBindFramebufferEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDeleteFramebuffersEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glDeleteFramebuffersEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGenFramebuffersEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glGenFramebuffersEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glCheckFramebufferStatusEXT(int arg0) {
  static int(*ptr)(int) = NULL;
  static const char name[] = "glCheckFramebufferStatusEXT";
  if (!ptr) {
    ptr = (int(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glFramebufferTexture1DEXT(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glFramebufferTexture1DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glFramebufferTexture2DEXT(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glFramebufferTexture2DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glFramebufferTexture3DEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glFramebufferTexture3DEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glFramebufferRenderbufferEXT(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glFramebufferRenderbufferEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetFramebufferAttachmentParameterivEXT(int arg0, int arg1, int arg2, int* arg3) {
  static void(*ptr)(int,int,int,int*) = NULL;
  static const char name[] = "glGetFramebufferAttachmentParameterivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGenerateMipmapEXT(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glGenerateMipmapEXT";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glStringMarkerGREMEDY(int arg0, void* arg1) {
  static void(*ptr)(int,void*) = NULL;
  static const char name[] = "glStringMarkerGREMEDY";
  if (!ptr) {
    ptr = (void(*)(int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetQueryObjecti64vEXT(int arg0, int arg1, long* arg2) {
  static void(*ptr)(int,int,long*) = NULL;
  static const char name[] = "glGetQueryObjecti64vEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,long*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetQueryObjectui64vEXT(int arg0, int arg1, long* arg2) {
  static void(*ptr)(int,int,long*) = NULL;
  static const char name[] = "glGetQueryObjectui64vEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,long*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexBufferEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glTexBufferEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBeginTransformFeedbackNV(int arg0) {
  static void(*ptr)(int) = NULL;
  static const char name[] = "glBeginTransformFeedbackNV";
  if (!ptr) {
    ptr = (void(*)(int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glEndTransformFeedbackNV() {
  static void(*ptr)() = NULL;
  static const char name[] = "glEndTransformFeedbackNV";
  if (!ptr) {
    ptr = (void(*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void PureGL_glTransformFeedbackAttribsNV(int arg0, int* arg1, int arg2) {
  static void(*ptr)(int,int*,int) = NULL;
  static const char name[] = "glTransformFeedbackAttribsNV";
  if (!ptr) {
    ptr = (void(*)(int,int*,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBindBufferRangeNV(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glBindBufferRangeNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glBindBufferOffsetNV(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glBindBufferOffsetNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glBindBufferBaseNV(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glBindBufferBaseNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTransformFeedbackVaryingsNV(int arg0, int arg1, int* arg2, int arg3) {
  static void(*ptr)(int,int,int*,int) = NULL;
  static const char name[] = "glTransformFeedbackVaryingsNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glActiveVaryingNV(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glActiveVaryingNV";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glGetVaryingLocationNV(int arg0, char* arg1) {
  static int(*ptr)(int,char*) = NULL;
  static const char name[] = "glGetVaryingLocationNV";
  if (!ptr) {
    ptr = (int(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetActiveVaryingNV(int arg0, int arg1, int arg2, int* arg3, int* arg4, int* arg5, char* arg6) {
  static void(*ptr)(int,int,int,int*,int*,int*,char*) = NULL;
  static const char name[] = "glGetActiveVaryingNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*,int*,int*,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
}

void PureGL_glGetTransformFeedbackVaryingNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetTransformFeedbackVaryingNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glDepthRangedNV(double arg0, double arg1) {
  static void(*ptr)(double,double) = NULL;
  static const char name[] = "glDepthRangedNV";
  if (!ptr) {
    ptr = (void(*)(double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glClearDepthdNV(double arg0) {
  static void(*ptr)(double) = NULL;
  static const char name[] = "glClearDepthdNV";
  if (!ptr) {
    ptr = (void(*)(double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void PureGL_glDepthBoundsdNV(double arg0, double arg1) {
  static void(*ptr)(double,double) = NULL;
  static const char name[] = "glDepthBoundsdNV";
  if (!ptr) {
    ptr = (void(*)(double,double))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glColorMaskIndexedEXT(int arg0, char arg1, char arg2, char arg3, char arg4) {
  static void(*ptr)(int,char,char,char,char) = NULL;
  static const char name[] = "glColorMaskIndexedEXT";
  if (!ptr) {
    ptr = (void(*)(int,char,char,char,char))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetBooleanIndexedvEXT(int arg0, int arg1, char* arg2) {
  static void(*ptr)(int,int,char*) = NULL;
  static const char name[] = "glGetBooleanIndexedvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetIntegerIndexedvEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetIntegerIndexedvEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glEnableIndexedEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glEnableIndexedEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glDisableIndexedEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glDisableIndexedEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

char PureGL_glIsEnabledIndexedEXT(int arg0, int arg1) {
  static char(*ptr)(int,int) = NULL;
  static const char name[] = "glIsEnabledIndexedEXT";
  if (!ptr) {
    ptr = (char(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glTexParameterIivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glTexParameterIivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glTexParameterIuivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glTexParameterIuivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetTexParameterIivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetTexParameterIivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetTexParameterIuivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetTexParameterIuivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glClearColorIiEXT(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glClearColorIiEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glClearColorIuiEXT(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glClearColorIuiEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniformBufferEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glUniformBufferEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

int PureGL_glGetUniformBufferSizeEXT(int arg0, int arg1) {
  static int(*ptr)(int,int) = NULL;
  static const char name[] = "glGetUniformBufferSizeEXT";
  if (!ptr) {
    ptr = (int(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

int PureGL_glGetUniformOffsetEXT(int arg0, int arg1) {
  static int(*ptr)(int,int) = NULL;
  static const char name[] = "glGetUniformOffsetEXT";
  if (!ptr) {
    ptr = (int(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glGetUniformuivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetUniformuivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glBindFragDataLocationEXT(int arg0, int arg1, char* arg2) {
  static void(*ptr)(int,int,char*) = NULL;
  static const char name[] = "glBindFragDataLocationEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

int PureGL_glGetFragDataLocationEXT(int arg0, char* arg1) {
  static int(*ptr)(int,char*) = NULL;
  static const char name[] = "glGetFragDataLocationEXT";
  if (!ptr) {
    ptr = (int(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glUniform1uiEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glUniform1uiEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glUniform2uiEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glUniform2uiEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform3uiEXT(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glUniform3uiEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glUniform4uiEXT(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glUniform4uiEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glUniform1uivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform1uivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform2uivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform2uivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform3uivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform3uivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glUniform4uivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glUniform4uivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribI1iEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glVertexAttribI1iEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI2iEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glVertexAttribI2iEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribI3iEXT(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glVertexAttribI3iEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttribI4iEXT(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glVertexAttribI4iEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttribI1uiEXT(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glVertexAttribI1uiEXT";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI2uiEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glVertexAttribI2uiEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glVertexAttribI3uiEXT(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glVertexAttribI3uiEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glVertexAttribI4uiEXT(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glVertexAttribI4uiEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glVertexAttribI1ivEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttribI1ivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI2ivEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttribI2ivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI3ivEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttribI3ivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI4ivEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttribI4ivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI1uivEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttribI1uivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI2uivEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttribI2uivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI3uivEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttribI3uivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI4uivEXT(int arg0, int* arg1) {
  static void(*ptr)(int,int*) = NULL;
  static const char name[] = "glVertexAttribI4uivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI4bvEXT(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVertexAttribI4bvEXT";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI4svEXT(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttribI4svEXT";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI4ubvEXT(int arg0, char* arg1) {
  static void(*ptr)(int,char*) = NULL;
  static const char name[] = "glVertexAttribI4ubvEXT";
  if (!ptr) {
    ptr = (void(*)(int,char*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribI4usvEXT(int arg0, short* arg1) {
  static void(*ptr)(int,short*) = NULL;
  static const char name[] = "glVertexAttribI4usvEXT";
  if (!ptr) {
    ptr = (void(*)(int,short*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glVertexAttribIPointerEXT(int arg0, int arg1, int arg2, int arg3, void* arg4) {
  static void(*ptr)(int,int,int,int,void*) = NULL;
  static const char name[] = "glVertexAttribIPointerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,void*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glGetVertexAttribIivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetVertexAttribIivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVertexAttribIuivEXT(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetVertexAttribIuivEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glProgramParameteriEXT(int arg0, int arg1, int arg2) {
  static void(*ptr)(int,int,int) = NULL;
  static const char name[] = "glProgramParameteriEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glFramebufferTextureEXT(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glFramebufferTextureEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glFramebufferTextureLayerEXT(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glFramebufferTextureLayerEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glFramebufferTextureFaceEXT(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glFramebufferTextureFaceEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glProgramVertexLimitNV(int arg0, int arg1) {
  static void(*ptr)(int,int) = NULL;
  static const char name[] = "glProgramVertexLimitNV";
  if (!ptr) {
    ptr = (void(*)(int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1);
}

void PureGL_glProgramLocalParameterI4iNV(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glProgramLocalParameterI4iNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glProgramLocalParameterI4ivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glProgramLocalParameterI4ivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glProgramLocalParametersI4ivNV(int arg0, int arg1, int arg2, int* arg3) {
  static void(*ptr)(int,int,int,int*) = NULL;
  static const char name[] = "glProgramLocalParametersI4ivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glProgramLocalParameterI4uiNV(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glProgramLocalParameterI4uiNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glProgramLocalParameterI4uivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glProgramLocalParameterI4uivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glProgramLocalParametersI4uivNV(int arg0, int arg1, int arg2, int* arg3) {
  static void(*ptr)(int,int,int,int*) = NULL;
  static const char name[] = "glProgramLocalParametersI4uivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glProgramEnvParameterI4iNV(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glProgramEnvParameterI4iNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glProgramEnvParameterI4ivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glProgramEnvParameterI4ivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glProgramEnvParametersI4ivNV(int arg0, int arg1, int arg2, int* arg3) {
  static void(*ptr)(int,int,int,int*) = NULL;
  static const char name[] = "glProgramEnvParametersI4ivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glProgramEnvParameterI4uiNV(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glProgramEnvParameterI4uiNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glProgramEnvParameterI4uivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glProgramEnvParameterI4uivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glProgramEnvParametersI4uivNV(int arg0, int arg1, int arg2, int* arg3) {
  static void(*ptr)(int,int,int,int*) = NULL;
  static const char name[] = "glProgramEnvParametersI4uivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glGetProgramLocalParameterIivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetProgramLocalParameterIivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetProgramLocalParameterIuivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetProgramLocalParameterIuivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetProgramEnvParameterIivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetProgramEnvParameterIivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetProgramEnvParameterIuivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetProgramEnvParameterIuivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glProgramBufferParametersfvNV(int arg0, int arg1, int arg2, int arg3, float* arg4) {
  static void(*ptr)(int,int,int,int,float*) = NULL;
  static const char name[] = "glProgramBufferParametersfvNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,float*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glProgramBufferParametersIivNV(int arg0, int arg1, int arg2, int arg3, int* arg4) {
  static void(*ptr)(int,int,int,int,int*) = NULL;
  static const char name[] = "glProgramBufferParametersIivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glProgramBufferParametersIuivNV(int arg0, int arg1, int arg2, int arg3, int* arg4) {
  static void(*ptr)(int,int,int,int,int*) = NULL;
  static const char name[] = "glProgramBufferParametersIuivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glRenderbufferStorageMultisampleEXT(int arg0, int arg1, int arg2, int arg3, int arg4) {
  static void(*ptr)(int,int,int,int,int) = NULL;
  static const char name[] = "glRenderbufferStorageMultisampleEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glRenderbufferStorageMultisampleCoverageNV(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5) {
  static void(*ptr)(int,int,int,int,int,int) = NULL;
  static const char name[] = "glRenderbufferStorageMultisampleCoverageNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5);
}

void PureGL_glBlitFramebufferEXT(int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9) {
  static void(*ptr)(int,int,int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glBlitFramebufferEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
}

void PureGL_glDrawArraysInstancedEXT(int arg0, int arg1, int arg2, int arg3) {
  static void(*ptr)(int,int,int,int) = NULL;
  static const char name[] = "glDrawArraysInstancedEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3);
}

void PureGL_glDrawElementsInstancedEXT(int arg0, int arg1, int arg2, void* arg3, int arg4) {
  static void(*ptr)(int,int,int,void*,int) = NULL;
  static const char name[] = "glDrawElementsInstancedEXT";
  if (!ptr) {
    ptr = (void(*)(int,int,int,void*,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4);
}

void PureGL_glPresentFrameKeyedNV(int arg0, long arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9, int arg10) {
  static void(*ptr)(int,long,int,int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glPresentFrameKeyedNV";
  if (!ptr) {
    ptr = (void(*)(int,long,int,int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10);
}

void PureGL_glPresentFrameDualFillNV(int arg0, long arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9, int arg10, int arg11, int arg12) {
  static void(*ptr)(int,long,int,int,int,int,int,int,int,int,int,int,int) = NULL;
  static const char name[] = "glPresentFrameDualFillNV";
  if (!ptr) {
    ptr = (void(*)(int,long,int,int,int,int,int,int,int,int,int,int,int))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12);
}

void PureGL_glGetVideoivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetVideoivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVideouivNV(int arg0, int arg1, int* arg2) {
  static void(*ptr)(int,int,int*) = NULL;
  static const char name[] = "glGetVideouivNV";
  if (!ptr) {
    ptr = (void(*)(int,int,int*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVideoi64vNV(int arg0, int arg1, long* arg2) {
  static void(*ptr)(int,int,long*) = NULL;
  static const char name[] = "glGetVideoi64vNV";
  if (!ptr) {
    ptr = (void(*)(int,int,long*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

void PureGL_glGetVideoui64vNV(int arg0, int arg1, long* arg2) {
  static void(*ptr)(int,int,long*) = NULL;
  static const char name[] = "glGetVideoui64vNV";
  if (!ptr) {
    ptr = (void(*)(int,int,long*))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0,arg1,arg2);
}

