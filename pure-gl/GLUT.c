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

void Pure_glutInit(int* arg0, char** arg1)
{
  static void(APIENTRY*ptr)(int* arg0, char** arg1) = NULL;
  static const char name[] = "glutInit";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int* arg0, char** arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutInitWindowPosition(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glutInitWindowPosition";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutInitWindowSize(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glutInitWindowSize";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutInitDisplayMode(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glutInitDisplayMode";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutInitDisplayString(char const* arg0)
{
  static void(APIENTRY*ptr)(char const* arg0) = NULL;
  static const char name[] = "glutInitDisplayString";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutMainLoop()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutMainLoop";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

int Pure_glutCreateWindow(char const* arg0)
{
  static int(APIENTRY*ptr)(char const* arg0) = NULL;
  static const char name[] = "glutCreateWindow";
  if (!ptr) {
    ptr = (int(APIENTRY*)(char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutCreateSubWindow(int arg0, int arg1, int arg2, int arg3, int arg4)
{
  static int(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3, int arg4) = NULL;
  static const char name[] = "glutCreateSubWindow";
  if (!ptr) {
    ptr = (int(APIENTRY*)(int arg0, int arg1, int arg2, int arg3, int arg4))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3, arg4);
}

void Pure_glutDestroyWindow(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutDestroyWindow";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSetWindow(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutSetWindow";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutGetWindow()
{
  static int(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutGetWindow";
  if (!ptr) {
    ptr = (int(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutSetWindowTitle(char const* arg0)
{
  static void(APIENTRY*ptr)(char const* arg0) = NULL;
  static const char name[] = "glutSetWindowTitle";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSetIconTitle(char const* arg0)
{
  static void(APIENTRY*ptr)(char const* arg0) = NULL;
  static const char name[] = "glutSetIconTitle";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutReshapeWindow(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glutReshapeWindow";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutPositionWindow(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glutPositionWindow";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutShowWindow()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutShowWindow";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutHideWindow()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutHideWindow";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutIconifyWindow()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutIconifyWindow";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutPushWindow()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutPushWindow";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutPopWindow()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutPopWindow";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutFullScreen()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutFullScreen";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutPostWindowRedisplay(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutPostWindowRedisplay";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutPostRedisplay()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutPostRedisplay";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutSwapBuffers()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutSwapBuffers";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutWarpPointer(int arg0, int arg1)
{
  static void(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glutWarpPointer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutSetCursor(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutSetCursor";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutEstablishOverlay()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutEstablishOverlay";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutRemoveOverlay()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutRemoveOverlay";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutUseLayer(unsigned int arg0)
{
  static void(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glutUseLayer";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutPostOverlayRedisplay()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutPostOverlayRedisplay";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutPostWindowOverlayRedisplay(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutPostWindowOverlayRedisplay";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutShowOverlay()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutShowOverlay";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutHideOverlay()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutHideOverlay";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

int Pure_glutCreateMenu(void* arg0)
{
  static int(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutCreateMenu";
  if (!ptr) {
    ptr = (int(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutDestroyMenu(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutDestroyMenu";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutGetMenu()
{
  static int(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutGetMenu";
  if (!ptr) {
    ptr = (int(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutSetMenu(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutSetMenu";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutAddMenuEntry(char const* arg0, int arg1)
{
  static void(APIENTRY*ptr)(char const* arg0, int arg1) = NULL;
  static const char name[] = "glutAddMenuEntry";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char const* arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutAddSubMenu(char const* arg0, int arg1)
{
  static void(APIENTRY*ptr)(char const* arg0, int arg1) = NULL;
  static const char name[] = "glutAddSubMenu";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char const* arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutChangeToMenuEntry(int arg0, char const* arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, char const* arg1, int arg2) = NULL;
  static const char name[] = "glutChangeToMenuEntry";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, char const* arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glutChangeToSubMenu(int arg0, char const* arg1, int arg2)
{
  static void(APIENTRY*ptr)(int arg0, char const* arg1, int arg2) = NULL;
  static const char name[] = "glutChangeToSubMenu";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, char const* arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glutRemoveMenuItem(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutRemoveMenuItem";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutAttachMenu(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutAttachMenu";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutDetachMenu(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutDetachMenu";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutTimerFunc(unsigned int arg0, void* arg1, int arg2)
{
  static void(APIENTRY*ptr)(unsigned int arg0, void* arg1, int arg2) = NULL;
  static const char name[] = "glutTimerFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, void* arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glutIdleFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutIdleFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutKeyboardFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutKeyboardFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSpecialFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutSpecialFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutReshapeFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutReshapeFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutVisibilityFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutVisibilityFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutDisplayFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutDisplayFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutMouseFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutMouseFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutMotionFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutMotionFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutPassiveMotionFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutPassiveMotionFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutEntryFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutEntryFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutKeyboardUpFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutKeyboardUpFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSpecialUpFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutSpecialUpFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutJoystickFunc(void* arg0, int arg1)
{
  static void(APIENTRY*ptr)(void* arg0, int arg1) = NULL;
  static const char name[] = "glutJoystickFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutMenuStateFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutMenuStateFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutMenuStatusFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutMenuStatusFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutOverlayDisplayFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutOverlayDisplayFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutWindowStatusFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutWindowStatusFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSpaceballMotionFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutSpaceballMotionFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSpaceballRotateFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutSpaceballRotateFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSpaceballButtonFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutSpaceballButtonFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutButtonBoxFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutButtonBoxFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutDialsFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutDialsFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutTabletMotionFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutTabletMotionFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutTabletButtonFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutTabletButtonFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutGet(unsigned int arg0)
{
  static int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glutGet";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutDeviceGet(unsigned int arg0)
{
  static int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glutDeviceGet";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutGetModifiers()
{
  static int(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutGetModifiers";
  if (!ptr) {
    ptr = (int(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

int Pure_glutLayerGet(unsigned int arg0)
{
  static int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glutLayerGet";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutBitmapCharacter(void* arg0, int arg1)
{
  static void(APIENTRY*ptr)(void* arg0, int arg1) = NULL;
  static const char name[] = "glutBitmapCharacter";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

int Pure_glutBitmapWidth(void* arg0, int arg1)
{
  static int(APIENTRY*ptr)(void* arg0, int arg1) = NULL;
  static const char name[] = "glutBitmapWidth";
  if (!ptr) {
    ptr = (int(APIENTRY*)(void* arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutStrokeCharacter(void* arg0, int arg1)
{
  static void(APIENTRY*ptr)(void* arg0, int arg1) = NULL;
  static const char name[] = "glutStrokeCharacter";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

int Pure_glutStrokeWidth(void* arg0, int arg1)
{
  static int(APIENTRY*ptr)(void* arg0, int arg1) = NULL;
  static const char name[] = "glutStrokeWidth";
  if (!ptr) {
    ptr = (int(APIENTRY*)(void* arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

int Pure_glutBitmapLength(void* arg0, unsigned char const* arg1)
{
  static int(APIENTRY*ptr)(void* arg0, unsigned char const* arg1) = NULL;
  static const char name[] = "glutBitmapLength";
  if (!ptr) {
    ptr = (int(APIENTRY*)(void* arg0, unsigned char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

int Pure_glutStrokeLength(void* arg0, unsigned char const* arg1)
{
  static int(APIENTRY*ptr)(void* arg0, unsigned char const* arg1) = NULL;
  static const char name[] = "glutStrokeLength";
  if (!ptr) {
    ptr = (int(APIENTRY*)(void* arg0, unsigned char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutWireCube(double arg0)
{
  static void(APIENTRY*ptr)(double arg0) = NULL;
  static const char name[] = "glutWireCube";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSolidCube(double arg0)
{
  static void(APIENTRY*ptr)(double arg0) = NULL;
  static const char name[] = "glutSolidCube";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutWireSphere(double arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(double arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glutWireSphere";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glutSolidSphere(double arg0, int arg1, int arg2)
{
  static void(APIENTRY*ptr)(double arg0, int arg1, int arg2) = NULL;
  static const char name[] = "glutSolidSphere";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, int arg1, int arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glutWireCone(double arg0, double arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glutWireCone";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glutSolidCone(double arg0, double arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glutSolidCone";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glutWireTorus(double arg0, double arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glutWireTorus";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glutSolidTorus(double arg0, double arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glutSolidTorus";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glutWireDodecahedron()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutWireDodecahedron";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutSolidDodecahedron()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutSolidDodecahedron";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutWireOctahedron()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutWireOctahedron";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutSolidOctahedron()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutSolidOctahedron";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutWireTetrahedron()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutWireTetrahedron";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutSolidTetrahedron()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutSolidTetrahedron";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutWireIcosahedron()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutWireIcosahedron";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutSolidIcosahedron()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutSolidIcosahedron";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutWireTeapot(double arg0)
{
  static void(APIENTRY*ptr)(double arg0) = NULL;
  static const char name[] = "glutWireTeapot";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSolidTeapot(double arg0)
{
  static void(APIENTRY*ptr)(double arg0) = NULL;
  static const char name[] = "glutSolidTeapot";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutGameModeString(char const* arg0)
{
  static void(APIENTRY*ptr)(char const* arg0) = NULL;
  static const char name[] = "glutGameModeString";
  if (!ptr) {
    ptr = (void(APIENTRY*)(char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutEnterGameMode()
{
  static int(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutEnterGameMode";
  if (!ptr) {
    ptr = (int(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutLeaveGameMode()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutLeaveGameMode";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

int Pure_glutGameModeGet(unsigned int arg0)
{
  static int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glutGameModeGet";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutVideoResizeGet(unsigned int arg0)
{
  static int(APIENTRY*ptr)(unsigned int arg0) = NULL;
  static const char name[] = "glutVideoResizeGet";
  if (!ptr) {
    ptr = (int(APIENTRY*)(unsigned int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSetupVideoResizing()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutSetupVideoResizing";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutStopVideoResizing()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutStopVideoResizing";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutVideoResize(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glutVideoResize";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glutVideoPan(int arg0, int arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glutVideoPan";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glutSetColor(int arg0, float arg1, float arg2, float arg3)
{
  static void(APIENTRY*ptr)(int arg0, float arg1, float arg2, float arg3) = NULL;
  static const char name[] = "glutSetColor";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float arg1, float arg2, float arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

float Pure_glutGetColor(int arg0, int arg1)
{
  static float(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glutGetColor";
  if (!ptr) {
    ptr = (float(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutCopyColormap(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutCopyColormap";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutIgnoreKeyRepeat(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutIgnoreKeyRepeat";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSetKeyRepeat(int arg0)
{
  static void(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutSetKeyRepeat";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutForceJoystickFunc()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutForceJoystickFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

int Pure_glutExtensionSupported(char const* arg0)
{
  static int(APIENTRY*ptr)(char const* arg0) = NULL;
  static const char name[] = "glutExtensionSupported";
  if (!ptr) {
    ptr = (int(APIENTRY*)(char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutReportErrors()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutReportErrors";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutMainLoopEvent()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutMainLoopEvent";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutLeaveMainLoop()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutLeaveMainLoop";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutExit()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutExit";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutFullScreenToggle()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutFullScreenToggle";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutMouseWheelFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutMouseWheelFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutCloseFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutCloseFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutWMCloseFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutWMCloseFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutMenuDestroyFunc(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutMenuDestroyFunc";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutSetOption(unsigned int arg0, int arg1)
{
  static void(APIENTRY*ptr)(unsigned int arg0, int arg1) = NULL;
  static const char name[] = "glutSetOption";
  if (!ptr) {
    ptr = (void(APIENTRY*)(unsigned int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

int* Pure_glutGetModeValues(unsigned int arg0, int* arg1)
{
  static int*(APIENTRY*ptr)(unsigned int arg0, int* arg1) = NULL;
  static const char name[] = "glutGetModeValues";
  if (!ptr) {
    ptr = (int*(APIENTRY*)(unsigned int arg0, int* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void* Pure_glutGetWindowData()
{
  static void*(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutGetWindowData";
  if (!ptr) {
    ptr = (void*(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutSetWindowData(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutSetWindowData";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void* Pure_glutGetMenuData()
{
  static void*(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutGetMenuData";
  if (!ptr) {
    ptr = (void*(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutSetMenuData(void* arg0)
{
  static void(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutSetMenuData";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutBitmapHeight(void* arg0)
{
  static int(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutBitmapHeight";
  if (!ptr) {
    ptr = (int(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

float Pure_glutStrokeHeight(void* arg0)
{
  static float(APIENTRY*ptr)(void* arg0) = NULL;
  static const char name[] = "glutStrokeHeight";
  if (!ptr) {
    ptr = (float(APIENTRY*)(void* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

void Pure_glutBitmapString(void* arg0, unsigned char const* arg1)
{
  static void(APIENTRY*ptr)(void* arg0, unsigned char const* arg1) = NULL;
  static const char name[] = "glutBitmapString";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0, unsigned char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutStrokeString(void* arg0, unsigned char const* arg1)
{
  static void(APIENTRY*ptr)(void* arg0, unsigned char const* arg1) = NULL;
  static const char name[] = "glutStrokeString";
  if (!ptr) {
    ptr = (void(APIENTRY*)(void* arg0, unsigned char const* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutWireRhombicDodecahedron()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutWireRhombicDodecahedron";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutSolidRhombicDodecahedron()
{
  static void(APIENTRY*ptr)() = NULL;
  static const char name[] = "glutSolidRhombicDodecahedron";
  if (!ptr) {
    ptr = (void(APIENTRY*)())get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)();
}

void Pure_glutWireSierpinskiSponge(int arg0, double* arg1, double arg2)
{
  static void(APIENTRY*ptr)(int arg0, double* arg1, double arg2) = NULL;
  static const char name[] = "glutWireSierpinskiSponge";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, double* arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glutSolidSierpinskiSponge(int arg0, double* arg1, double arg2)
{
  static void(APIENTRY*ptr)(int arg0, double* arg1, double arg2) = NULL;
  static const char name[] = "glutSolidSierpinskiSponge";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, double* arg1, double arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glutWireCylinder(double arg0, double arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glutWireCylinder";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void Pure_glutSolidCylinder(double arg0, double arg1, int arg2, int arg3)
{
  static void(APIENTRY*ptr)(double arg0, double arg1, int arg2, int arg3) = NULL;
  static const char name[] = "glutSolidCylinder";
  if (!ptr) {
    ptr = (void(APIENTRY*)(double arg0, double arg1, int arg2, int arg3))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2, arg3);
}

void* Pure_glutGetProcAddress(char const* arg0)
{
  static void*(APIENTRY*ptr)(char const* arg0) = NULL;
  static const char name[] = "glutGetProcAddress";
  if (!ptr) {
    ptr = (void*(APIENTRY*)(char const* arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutJoystickGetNumAxes(int arg0)
{
  static int(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutJoystickGetNumAxes";
  if (!ptr) {
    ptr = (int(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutJoystickGetNumButtons(int arg0)
{
  static int(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutJoystickGetNumButtons";
  if (!ptr) {
    ptr = (int(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

int Pure_glutJoystickNotWorking(int arg0)
{
  static int(APIENTRY*ptr)(int arg0) = NULL;
  static const char name[] = "glutJoystickNotWorking";
  if (!ptr) {
    ptr = (int(APIENTRY*)(int arg0))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0);
}

float Pure_glutJoystickGetDeadBand(int arg0, int arg1)
{
  static float(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glutJoystickGetDeadBand";
  if (!ptr) {
    ptr = (float(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutJoystickSetDeadBand(int arg0, int arg1, float arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, float arg2) = NULL;
  static const char name[] = "glutJoystickSetDeadBand";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

float Pure_glutJoystickGetSaturation(int arg0, int arg1)
{
  static float(APIENTRY*ptr)(int arg0, int arg1) = NULL;
  static const char name[] = "glutJoystickGetSaturation";
  if (!ptr) {
    ptr = (float(APIENTRY*)(int arg0, int arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutJoystickSetSaturation(int arg0, int arg1, float arg2)
{
  static void(APIENTRY*ptr)(int arg0, int arg1, float arg2) = NULL;
  static const char name[] = "glutJoystickSetSaturation";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, int arg1, float arg2))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1, arg2);
}

void Pure_glutJoystickSetMinRange(int arg0, float* arg1)
{
  static void(APIENTRY*ptr)(int arg0, float* arg1) = NULL;
  static const char name[] = "glutJoystickSetMinRange";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutJoystickSetMaxRange(int arg0, float* arg1)
{
  static void(APIENTRY*ptr)(int arg0, float* arg1) = NULL;
  static const char name[] = "glutJoystickSetMaxRange";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutJoystickSetCenter(int arg0, float* arg1)
{
  static void(APIENTRY*ptr)(int arg0, float* arg1) = NULL;
  static const char name[] = "glutJoystickSetCenter";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutJoystickGetMinRange(int arg0, float* arg1)
{
  static void(APIENTRY*ptr)(int arg0, float* arg1) = NULL;
  static const char name[] = "glutJoystickGetMinRange";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutJoystickGetMaxRange(int arg0, float* arg1)
{
  static void(APIENTRY*ptr)(int arg0, float* arg1) = NULL;
  static const char name[] = "glutJoystickGetMaxRange";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}

void Pure_glutJoystickGetCenter(int arg0, float* arg1)
{
  static void(APIENTRY*ptr)(int arg0, float* arg1) = NULL;
  static const char name[] = "glutJoystickGetCenter";
  if (!ptr) {
    ptr = (void(APIENTRY*)(int arg0, float* arg1))get_proc_addr(name);
    if (!ptr) throw_unsupported(name);
  }
  return (*ptr)(arg0, arg1);
}
