
#include <fftw3.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>

static inline double sqr(double x)
{
  return x*x;
}

static inline double myabs(double x, double y)
{
  return sqrt(sqr(x)+sqr(y));
}

static inline double myarg(double x, double y)
{
  return atan2(y, x);
}

bool fft(int n, double *wave, double *mag, double *phase)
{
  int i, n2 = n/2;
  double n_2 = ((double)n)/2.0;
  double *x;
  fftw_plan p;

  x = fftw_malloc(n*sizeof(double));
  if (!x) return false;
  p = fftw_plan_r2r_1d(n, x, x, FFTW_R2HC, FFTW_ESTIMATE);
  if (!p) {
    fftw_free(x);
    return false;
  }
  memcpy(x, wave, n*sizeof(double));
  fftw_execute(p);
  mag[0] = myabs(x[0], 0.0)/n;
  phase[0] = myarg(x[0], 0.0);
  for (i = 1; i < n2; i++) {
    mag[i] = myabs(x[i], x[n-i])/n_2;
    phase[i] = myarg(x[i], x[n-i]);
  }
  if (n > 1) {
    if (n%2 == 0) {
      mag[n2] = myabs(x[n2], 0.0)/n;
      phase[n2] = myarg(x[n2], 0.0);
    } else {
      mag[n2] = myabs(x[n2], x[n-n2])/n_2;
      phase[n2] = myarg(x[n2], x[n-n2]);
    }
  }
  fftw_destroy_plan(p);
  fftw_free(x);
  return true;
}

bool ifft(int n, double *mag, double *phase, double *wave)
{
  int i, n2 = n/2;
  double *x;
  fftw_plan p;

  x = fftw_malloc(n*sizeof(double));
  if (!x) return false;
  p = fftw_plan_r2r_1d(n, x, x, FFTW_HC2R, FFTW_ESTIMATE);
  if (!p) {
    fftw_free(x);
    return false;
  }
  x[0] = mag[0]*cos(phase[0]);
  for (i = 1; i < n2; i++) {
    x[i] = mag[i]*cos(phase[i])/2.0;
    x[n-i] = mag[i]*sin(phase[i])/2.0;
  }
  x[n2] = mag[n2]*cos(phase[n2]);
  fftw_execute(p);
  memcpy(wave, x, n*sizeof(double));
  fftw_destroy_plan(p);
  fftw_free(x);
  return true;
}
