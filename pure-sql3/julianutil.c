/* julianutil.c - */

/* The algorithms used in this module reflect the Gregorian proleptic
   calendar (i.e., the Gregorian calendar extended to cover dates before
   its introduction in 1582.) When expressing Gregorian dates as
   yyyy-mm-dd, astronomical year numbering is used, thus 1 BC is 0, 2 BC is
   −1, and 4714 BC is −4713. The months (mm) and days (dd) start with
   1. In contrast, Historians usually use the Julian calendar for dates
   prior to 1582-10-15.

   Daylight saving time adjustments for dates before 1971 are assumed
   to be zero. Those after 1970 are estimates that might not be
   accurate.

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

double local_jd(double jd);
static void setYMD(double jd, int* y, int *m, int *d);
static void setHMS(double jd, int* h, int *m, double *s);
static int proxy_year(int y);
static double tm_to_jd(struct tm *tmp);
static int proxy_year(int y);
static time_t proxy_utc_secs(double jd);
static int is_leap_year(int year);
static int maxday(int month, int year);

/* Current time, unix epoch time conversions */

time_t jd_to_unixtime(double jd){
  double secs = round( (jd-BEG_UNIXTIME) * SECS_PER_DAY);
  if (secs > LONG_MIN && secs < LONG_MAX)
    return (time_t)secs;
  else {
    //printf("dbg::jd_to_unixtime: %f, secs: %f\n", jd, secs);
    return LONG_MIN;
  }
}

double unixtime_to_jd(time_t t){
  return t / SECS_PER_DAY + BEG_UNIXTIME;
}

double current_jd(){
  time_t secs;
  time(&secs);
  return unixtime_to_jd(secs);
}

/* jd to ymd and back family for UTC */

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
  //printf ("dbg::ymdhms %d %d %d  | %d %d %f\n", Y,M,D,hr,mn,sc);
  double jd = ymd_to_jd(Y,M,D);
  double secs = hr * 3600 + mn * 60 + sc;
  return jd + secs / SECS_PER_DAY;
}

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
  int y, m, d, H, M; 
  double S;
  setYMD(jd, &y, &m, &d);
  setHMS(jd, &H, &M, &S);
  pure_expr *py = pure_int(y);
  pure_expr *pm = pure_int(m);
  pure_expr *pd = pure_int(d);
  pure_expr *pH = pure_int(H);
  pure_expr *pM = pure_int(M);
  pure_expr *pS = pure_double(S);
  pure_expr *pret = pure_tuplel(6,py,pm,pd,pH,pM,pS);
  return pret;  
}

double bump_ym(double jd, int y_delta, int m_delta){
  int y,m,d1,H1,M1; double S1;
  setYMD(jd, &y, &m, &d1);
  setHMS(jd, &H1, &M1, &S1);
  //add months
  int m1 = m + m_delta;
  int year_bump = m1 > 12 ? m1/12 : (m1-12)/12;
  y += year_bump;
  m1 -= 12 * year_bump;
  int m1_maxday = maxday(m1,y);
  if (d1>m1_maxday) d1 = m1_maxday;
  //add year
  int y1 = y + y_delta;
  return ymdhms_to_jd(y1,m1,d1,H1,M1,S1);
}


/* local time functions */

/* UTC jd ==> to a phoney jd that prints out local greg */

double local_jd(double jd){
  struct tm lt_tm, *ptr_tm;
  //convert to unix time for the JD or a "similar year" within
  //unixtime range
  int in_unix_range = true;
  time_t utc_secs = jd_to_unixtime(jd);
  if (utc_secs == LONG_MIN) {
    utc_secs = proxy_utc_secs(jd);
    in_unix_range = false;
  }
  // get the local time tm struct
  ptr_tm = localtime( &utc_secs );
  if (ptr_tm == NULL) { //jd utc in range but local not
    utc_secs = proxy_utc_secs(jd);
    in_unix_range = false;
    ptr_tm = localtime( &utc_secs );
    if (ptr_tm == NULL) return jd;
  }
  memcpy( &lt_tm, ptr_tm, sizeof(struct tm) );
  
  // build local jd from local tm struct
  double local_jd = tm_to_jd(&lt_tm);
  if (in_unix_range) return local_jd;
  double proxy_jd = unixtime_to_jd(utc_secs);
  return jd + (local_jd - proxy_jd);
}

/* local greg tuple ==> UTC jd. */
double l_ymdhms_to_jd(int Y, int M, int D, int hr, int mn, double sc){
  struct tm ltbuf;
  time_t secs;
  ltbuf.tm_sec = sc; //ltbuf is local time
  ltbuf.tm_min = mn;
  ltbuf.tm_hour = hr;
  ltbuf.tm_mday = D;
  ltbuf.tm_mon = M-1;
  ltbuf.tm_year = Y - 1900;
  ltbuf.tm_isdst = -1;
  secs = mktime( &ltbuf ); //secs is UTC
  if ( secs != -1 ) {
    //was within unix_range so UTC secs is valid
    return unixtime_to_jd(secs);
  } else {
    //was out of unix time range, use proxy_year to get offset
    double utc_jd = ymdhms_to_jd(Y,M,D,hr,mn,sc);
    int py = proxy_year(Y);
    //avoid imputed daylight saving adjustments for early years
    int pm = py <= 1972 ? 12 : M;
    int pd = py <= 1972 ? 31 : D;
    double proxy_loc_jd = l_ymdhms_to_jd(py,pm,pd,hr,mn,sc);
    double proxy_utc_jd = ymdhms_to_jd(py,pm,pd,hr,mn,sc);
    return utc_jd + (proxy_loc_jd - proxy_utc_jd);
  }
}

double l_ymd_to_jd(int Y, int M, int D){
  return l_ymdhms_to_jd(Y, M, D, 0, 0, 0.0);
}

/* UTC jd ==> local greg */
pure_expr* l_jd_to_ymd(double jd){
  return jd_to_ymd( local_jd(jd) );
}

pure_expr* l_jd_to_ymdhms(double jd){
  return jd_to_ymdhms( local_jd(jd) );
}


/* Helper functions */

static int is_leap_year(int year){
  int leap_year = false;
  if (year % 400 == 0)
    leap_year = true;
  else if (year % 100 == 0)
    leap_year = false;
  else if (year % 4 == 0)
    leap_year = true;
  return leap_year;
}

static int maxday(int month, int year){
  switch (month){
  case  1: 
  case  3:
  case  5:
  case  7:
  case  8:
  case 10:
  case 12: return 31;
  case  2: return is_leap_year(year) ? 29 : 28;
  default: return 30;
  }
}

static void setHMS(double jd, int *hours, int *mins, double *secs){
  double day, day_frac;
  day_frac = modf(jd, &day);
  if (day_frac < 0.5) {
    day_frac = day_frac + 0.5;
    day -= 0.5;
  }
  else {
    day_frac = day_frac - 0.5;
    day += 0.5;
  }
  double total_secs = SECS_PER_DAY * day_frac;
  int round_secs = (int)round(total_secs);
  double sec_frac = total_secs - round_secs;
  if (fabs(sec_frac) < 0.0001) sec_frac = 0.0;
  *secs = (round_secs % 60) + sec_frac;
  int day_min = round_secs / 60;
  *mins = day_min % 60;
  *hours = day_min / 60;
}

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

static double tm_to_jd(struct tm *tmp){
  return ymdhms_to_jd(
    tmp->tm_year + 1900,
    tmp->tm_mon+1,
    tmp->tm_mday,
    tmp->tm_hour,
    tmp->tm_min,
    tmp->tm_sec);
}

static int proxy_year(int y){
  int leap_year = is_leap_year(y);
  int py = 0;
  // daylight saving time shifts around, so pick accordingly
  if (y <= 1972) {
    py = 1972;
  } else {
    if (leap_year)
      py = 2036;
    else
      py = 2035;
  }
  return py;
}

time_t proxy_utc_secs(double jd){
  int y,m,d;
  double proxy_jd;
  setYMD(jd, &y, &m, &d);
  int py = proxy_year(y);
  //avoid daylight saving adjustment for early years 
  if (py <= 1972)
    proxy_jd = ymd_to_jd(py,12,31);
  else
    proxy_jd = ymd_to_jd(py,m,d);
  return jd_to_unixtime(proxy_jd);
}

