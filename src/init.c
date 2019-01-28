#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void Conv1(void *, void *, void *, void *, void *);
extern void Conv2(void *, void *, void *, void *, void *);
extern void Conv3(void *, void *, void *, void *, void *, void *);
extern void ShiftCurve(void *, void *, void *, void *);
extern void calcB(void *, void *, void *);
extern void calcCirf(void *, void *, void *, void *, void *, void *, void *);
extern void calcCirf_multi(void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"Conv1",          (DL_FUNC) &Conv1,          5},
    {"Conv2",          (DL_FUNC) &Conv2,          5},
    {"Conv3",          (DL_FUNC) &Conv3,          6},
    {"ShiftCurve",     (DL_FUNC) &ShiftCurve,     4},
    {"calcB",          (DL_FUNC) &calcB,          3},
    {"calcCirf",       (DL_FUNC) &calcCirf,       7},
    {"calcCirf_multi", (DL_FUNC) &calcCirf_multi, 7},
    {NULL, NULL, 0}
};

void R_init_TIMP(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
