#include <glib.h>

GTokenValue* Pure_g_scanner_cur_value(GScanner* arg0)
{
  static GTokenValue ret;
  ret = g_scanner_cur_value(arg0); return &ret;
}

void Pure_g_assertion_message_cmpnum(char const* arg0, char const* arg1, int arg2, char const* arg3, char const* arg4, double arg5, char const* arg6, double arg7, char arg8)
{
  return g_assertion_message_cmpnum(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}
#include <glib-object.h>
#include <gio/gio.h>
#include <gmodule.h>
