/* file age3classp.c */
#include <R.h>
static double parms[2];
static double forc[1];

#define dim parms[0]
#define c parms[1]

#define i1 forc[0] // a(t)

/* initializers */
void initmod(void (* odeparms)(int *, double *))
{
  int N=2;
  odeparms(&N, parms);
}
void forcc(void (* odeforcs)(int *, double *))
{
  int N=1;
  odeforcs(&N, forc);
}

/* Derivatives */
  void derivs (int *neq, double *t, double *y, double *ydot,
               double *yout, int *ip)
{
    int i;
    for (i = 0; i < dim; ++i){
      ydot[i] = c[0]*(1-y[i]) - c[1]*y[i];
    }
}
/* END file age3classp.c */
