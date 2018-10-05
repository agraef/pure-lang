
#include <stdlib.h>
#include <pure/runtime.h>
#include <m_pd.h>

extern char *pd_version_s(void)
{
  static char buf[MAXPDSTRING];
  sprintf(buf, "%d.%d", PD_MAJOR_VERSION, PD_MINOR_VERSION);
  return buf;
}

pure_expr *pd_path(void)
{
  return pure_listl(0);
}

pure_expr *pd_getdir(void)
{
  return pure_cstring_dup(".");
}

pure_expr *pd_getfile(void)
{
  return NULL;
}

void pd_setfile(char* s)
{
}

void pd_send(char* s, pure_expr* x)
{
  char *t = str(x);
  printf("%s: %s\n", s, t);
  free(t);
}

void pd_receive(char* s)
{
}

void pd_unreceive(char* s)
{
}

void pd_reload(void)
{
}

void pd_post(char* s)
{
  puts(s);
}

void pd_error_s(char* s)
{
  puts(s);
}

double pd_time(void)
{
  return 0.0;
}

float sys_getsr(void)
{
  return 48000.0;
}

int sys_getblksize(void)
{
  return 10;
}
