/* julianutil.c - */

/* The algorithms used in this module reflect the Gregorian proleptic
   calendar (i.e., the Gregorian calendar extended to cover dates before
   its introduction in 1582.) When expressing Gregorian dates as
   yyyy-mm-dd, astronomical year numbering is used, thus 1 BC is 0, 2 BC is
   −1, and 4714 BC is −4713. The months (mm) and days (dd) start with
   1. In contrast, Historians usually use the Julian calendar for dates
   prior to 1582-10-15.

   Please bear these factors in mind when using this module.
  
   See julian.pure and http://en.wikipedia.org/wiki/Julian_day for further
   information.*/

#include <stdlib.h>
#include <limits.h>
#include <pure/runtime.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>


const double BEG_UNIXTIME = 2440587.5;  //1970_01_01
const double MAX_UNIXTIME = 2465423.5;  //2037_12_31
const double SUB_UNIXTIME = 2451544.5;  //2000_01_01
const double SECS_PER_DAY = 86400.0;

double local_offset(double jd);
static void setYMD(double jd, int* y, int *m, int *d);
static double round3(double d);

/* Current time, unix epoch time conversions */

//Returns LONG_MIN to indicate error
time_t jd_to_unixtime(double jd){
  double secs = floor( (jd-BEG_UNIXTIME) * 86400 );
  if (secs > LONG_MIN && secs < LONG_MAX)
    return (time_t)secs;
  else {
    printf("error in jd_to_unixtime jd: %f, secs: %f\n", jd, secs);
    return LONG_MIN;
  }
}

double unixtime_to_jd(time_t t){
  return t / SECS_PER_DAY + 2440587.5;
}

double current_jd(){
  time_t secs;
  time(&secs);
  return unixtime_to_jd(secs);
}

double local_jd(double jd){
  return jd + local_offset(jd);
}

/* Compute Julian Date from year, month. ... */

double ymd_to_jd(int Y, int M, int D){
  int A, B, X1, X2;
  if( M<=2 ){
    Y--;
    M += 12;
  }
  A = Y/100;
  B = 2 - A + (A/4);
  X1 = 36525*(Y+4716)/100;
  X2 = 306001*(M+1)/10000;
  return X1 + X2 + D + B - 1524.5;
}

double ymdhms_to_jd(int Y, int M, int D, int hr, int mn, double sc){
  double jd = ymd_to_jd(Y,M,D);
  double secs = hr * 3600 + mn * 60 + sc;
  return jd + secs / SECS_PER_DAY;
}

double l_ymdhms_to_jd(int Y, int M, int D, int hr, int mn, double sc){
  struct tm ltbuf;
  time_t secs;
  ltbuf.tm_sec = sc; //ltbuf is local time
  ltbuf.tm_min = mn;
  ltbuf.tm_hour = hr;
  ltbuf.tm_mday = D;
  ltbuf.tm_mon = M-1;
  ltbuf.tm_year = Y + 1900;
  ltbuf.tm_isdst = -1;
  secs = mktime( &ltbuf ); //secs is UTC
  if ( secs == -1 ) {
    double local_jd = ymdhms_to_jd(Y,M,D,hr,mn,sc);    
    return local_jd - local_offset(local_jd);
  } else
    return unixtime_to_jd(secs); //TODO check that this is ok
}

double l_ymd_to_jd(int Y, int M, int D){
  return l_ymdhms_to_jd(Y, M, D, 0, 0, 0.0);
}


/* Return (year, month, day, ...) for Julian date */

pure_expr* jd_to_ymd(double jd){
  int y, m, d;
  setYMD(jd,&y,&m,&d);
  pure_expr *pY = pure_int(y);
  pure_expr *pM = pure_int(m);
  pure_expr *pD = pure_int(d);
  pure_expr *pret = pure_tuplel(3,pY,pM,pD);
  return pret;
}

pure_expr* jd_to_ymdhms(double jd){
  int s, y, m, d, H, M; 
  double day, day_frac, secs;
  day_frac = modf(jd, &day);
  if (day_frac < 0.5) {
    day_frac = day_frac + 0.5;
    day -= 0.5;
  }
  else {
    day_frac = day_frac - 0.5;
    day += 0.5;
  }
  secs =  86400.0 * day_frac;
  s = (int) secs;
  secs -= s;
  H = s/3600;  //TODO CHANGE TO AVOID 59:60.000
  s -= H*3600;
  M = s/60;
  s -= M*60;
  secs = round3(secs + s);
  setYMD(day, &y, &m, &d);
  pure_expr *py = pure_int(y);
  pure_expr *pm = pure_int(m);
  pure_expr *pd = pure_int(d);
  pure_expr *pH = pure_int(H);
  pure_expr *pM = pure_int(M);
  pure_expr *pS = pure_double(secs);
  pure_expr *pret = pure_tuplel(6,py,pm,pd,pH,pM,pS);
  return pret;  
}

pure_expr* l_jd_to_ymd(double jd){
  return jd_to_ymd(jd + local_offset(jd));
}

pure_expr* l_jd_to_ymdhms(double jd){
  return jd_to_ymdhms(jd + local_offset(jd));
}


/* Helper functions */

static void setYMD(double jd, int* y, int *m, int *d){
  int Z, A, B, C, D, E, M, X1;

  Z = (int) (jd+0.5);
  A = (int)((Z - 1867216.25)/36524.25);
  A = Z + 1 + A - (A/4);
  B = A + 1524;
  C = (int)((B - 122.1)/365.25);
  D = (36525*C)/100;
  E = (int)((B-D)/30.6001);
  X1 = (int)(30.6001*E);
  M = E<14 ? E-1 : E-13;

  *d = (B - D - X1);
  *m = M;
  *y = M>2 ? C - 4716 : C - 4715;
}

/* Returns the number of days to add to a JD to get a "local JD". The
   jd_to_ymd[hms] functions, when applied to a local JD, return the
   local y,m,... rather than the UTC y,m,... */

double local_offset(double jd){
  struct tm ut, *ptr_tm; 
  time_t rawtime = jd_to_unixtime(jd);
  if (rawtime == LONG_MIN)
    rawtime = jd_to_unixtime(SUB_UNIXTIME);
  ptr_tm = gmtime( &rawtime );
  if (ptr_tm == NULL) return 0.0;
  memcpy( &ut, ptr_tm, sizeof(struct tm) );
  ut.tm_isdst = -1;
  time_t utctime = mktime(&ut);
  //TO DO if crossing time zone need to adjust.
  return difftime(rawtime, utctime) / SECS_PER_DAY;
}


double round3(double d){
  return round(d*1000.0)/1000.0;
}
