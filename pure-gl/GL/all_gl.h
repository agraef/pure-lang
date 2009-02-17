//glu and glut include gl.h anyway, so bring them all into one translation unit.

#define GL_GLEXT_LEGACY //don't want <GL/glext.h>
#include <GL/gl.h>

#undef GL_GLEXT_LEGACY
#define GL_GLEXT_PROTOTYPES  
#include <GL/glext.h>
#include <GL/glu.h>
#include <GL/glut.h>
