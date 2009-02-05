//glu and glut include gl.h anyway, so bring them all into one translation unit.
// this version includes the freeglut extensions (if you have freeglut_ext.h)

#define GL_GLEXT_LEGACY //don't want <GL/glext.h>
#include "./gl.h"

#undef GL_GLEXT_LEGACY
#define GL_GLEXT_PROTOTYPES  
#include "./glext.h"
#include "./glu.h"
#include "./freeglut_std.h"
#include "./freeglut_ext.h"
