
/* realtime.c: Provide realtime scheduling to Pure programs. This uses the
   pthread scheduler routines and thus should be pretty portable.
   Copyright (c) 2010 by Albert Graef <Dr.Graef@t-online.de> */

#include <string.h>
#include <pthread.h>
#include <pure/runtime.h>

/* Set a scheduling policy. pol==1 means SCHED_FIFO, pol==2 SCHED_RR, any
   other value denotes SCHED_OTHER. Use pol==0 and prio==0 for normal,
   non-realtime scheduling. Valid priority ranges for the other policies can
   be determined with min_priority and max_priority below. */

int realtime(int pol, int prio)
{
  struct sched_param param;
  memset(&param, 0, sizeof(param));
  param.sched_priority = prio;
  pol = (pol==1)?SCHED_FIFO:(pol==2)?SCHED_RR:SCHED_OTHER;
  return pthread_setschedparam(pthread_self(), pol, &param);
}

/* This returns the current scheduling policy and priority. */

pure_expr *priority(void)
{
  int pol;
  struct sched_param param;
  memset(&param, 0, sizeof(param));
  if (pthread_getschedparam(pthread_self(), &pol, &param))
    return 0;
  else
    return pure_tuplel(2, pure_int(pol), pure_int(param.sched_priority));
}

int min_priority(int pol)
{
  pol = (pol==1)?SCHED_FIFO:(pol==2)?SCHED_RR:SCHED_OTHER;
  return sched_get_priority_min(pol);
}

int max_priority(int pol)
{
  pol = (pol==1)?SCHED_FIFO:(pol==2)?SCHED_RR:SCHED_OTHER;
  return sched_get_priority_max(pol);
}
