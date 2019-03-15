#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void r_calcB(double *bvec, double *k, int *lenk);
extern void r_calcCirf(double *cmat, double *k, double *x, double *tau, double *mu, int *lenk, int *lenx);
extern void r_calcCirf_multi(double *cmat, double *k, double *x, double *tau, double *mu, int *lenk, int *lenx);
extern void r_Conv1(double *result, double *measured, int *lenx, double *rate, double *xspace);
extern void r_Conv2(double *result, double *measured, int *lenx, double *rate, double *xspace);
extern void r_Conv3(double* source, double* reference, int *canN, double *rate, double *xspace, double *tauref);
extern void r_ShiftCurve  (double *source, double *curve, double *shiftparam, int *length);

static const R_CMethodDef CEntries[] = {
  {"r_calcB",          (DL_FUNC) &r_calcB,          3},
  {"r_calcCirf",       (DL_FUNC) &r_calcCirf,       7},
  {"r_calcCirf_multi", (DL_FUNC) &r_calcCirf_multi, 7},
  {"r_Conv1",          (DL_FUNC) &r_Conv1,          5},
  {"r_Conv2",          (DL_FUNC) &r_Conv2,          5},
  {"r_Conv3",          (DL_FUNC) &r_Conv3,          6},
  {"r_ShiftCurve",     (DL_FUNC) &r_ShiftCurve,     4},
  {NULL, NULL, 0}
};

void R_init_TIMP(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
