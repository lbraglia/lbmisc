#include <R.h>
#include <Rinternals.h>

SEXP group_prog_id_slave(SEXP x, SEXP max){
    const int n = length(x);
    SEXP res = PROTECT(allocVector(INTSXP, n));
    SEXP uv = PROTECT(allocVector(INTSXP, asInteger(max)));
    int *px   = INTEGER(x);
    int *pres = INTEGER(res);
    int *puv  = INTEGER(uv);

    /* Inizialize result */
    for(int i = 0; i < n; i++)
	pres[i] = 0;

    /* Initialize unique values */
    for(int i = 0; i < asInteger(max); i++)
	puv[i] = 0;

    /* Creating the index */
    for(int i = 0; i < n; i++)
    	pres[i] = ++puv[px[i] - 1];
    
    UNPROTECT(2);
    return res;
}
