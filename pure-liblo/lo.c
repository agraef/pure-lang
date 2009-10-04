#include <lo/lo.h>

int Pure_lo_message_add_timetag(void* arg0, lo_timetag* arg1)
{
  return lo_message_add_timetag(arg0, *arg1);
}

lo_timetag* Pure_lo_message_get_timestamp(void* arg0)
{
  static lo_timetag ret;
  ret = lo_message_get_timestamp(arg0); return &ret;
}

void* Pure_lo_bundle_new(lo_timetag* arg0)
{
  return lo_bundle_new(*arg0);
}

double Pure_lo_hires_val(unsigned int arg0, lo_arg* arg1)
{
  return lo_hires_val(arg0, arg1);
}

double Pure_lo_timetag_diff(lo_timetag* arg0, lo_timetag* arg1)
{
  return lo_timetag_diff(*arg0, *arg1);
}
