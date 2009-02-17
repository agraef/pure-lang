//glu and glut include gl.h anyway, so bring them all into one translation unit.
// this version includes the freeglut extensions (if you have freeglut_ext.h)

#define GL_GLEXT_LEGACY //don't want <GL/glext.h>
#include <GL/gl.h>

#undef GL_GLEXT_LEGACY
#define GL_GLEXT_PROTOTYPES  
#include <GL/glext.h>
#include <GL/glu.h>
#include <GL/freeglut_std.h>
#include <GL/freeglut_ext.h>
