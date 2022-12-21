/* file age3classp.c */
#include <R.h>
static double parms[3];
static double forc[1];

#define gam1 parms[0]
#define gam2 parms[1]
#define gam3 parms[2]

#define i1 forc[0] // a(t)


/* initializers */
void initmod(void (* odeparms)(int *, double *))
{
  int N=3;
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
    
    ydot[0] = i1 - (gam1 + 1)*y[0];
    for (i = 1; i < 30; ++i){
        ydot[i] = y[i-1] - (gam2 + 1)*y[i];
    }
    for (i = 30; i < 50; ++i){
        ydot[i] = y[i-1] - (gam3 + 1)*y[i];
    }
}
/* END file age3classp.c */
