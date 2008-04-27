#include <R.h>
#include <Rmath.h>
#include <Rdefines.h> 

/* Define this macro to suppress error propagation in exp(x^2)
   by using the expx2 function.  The tradeoff is that doing so
   generates two calls to the exponential function instead of one.  */

static double P[] = {
  2.46196981473530512524E-10,
  5.64189564831068821977E-1,
  7.46321056442269912687E0,
  4.86371970985681366614E1,
  1.96520832956077098242E2,
  5.26445194995477358631E2,
  9.34528527171957607540E2,
  1.02755188689515710272E3,
  5.57535335369399327526E2
};
static double Q[] = {
  /* 1.00000000000000000000E0,*/
  1.32281951154744992508E1,
  8.67072140885989742329E1,
  3.54937778887819891062E2,
  9.75708501743205489753E2,
  1.82390916687909736289E3,
  2.24633760818710981792E3,
  1.65666309194161350182E3,
  5.57535340817727675546E2
};
static double R[] = {
  5.64189583547755073984E-1,
  1.27536670759978104416E0,
  5.01905042251180477414E0,
  6.16021097993053585195E0,
  7.40974269950448939160E0,
  2.97886665372100240670E0
};
static double S[] = {
  /* 1.00000000000000000000E0,*/
  2.26052863220117276590E0,
  9.39603524938001434673E0,
  1.20489539808096656605E1,
  1.70814450747565897222E1,
  9.60896809063285878198E0,
  3.36907645100081516050E0
};
static double T[] = {
  9.60497373987051638749E0,
  9.00260197203842689217E1,
  2.23200534594684319226E3,
  7.00332514112805075473E3,
  5.55923013010394962768E4
};
static double U[] = {
  /* 1.00000000000000000000E0,*/
  3.35617141647503099647E1,
  5.21357949780152679795E2,
  4.59432382970980127987E3,
  2.26290000613890934246E4,
  4.92673942608635921086E4
};
static double MAXLOG =  7.09782712893383996843E2;

double polevl( x, coef, N )
     double x;
     double coef[];
     int N;
{
  double ans;
  int i;
  double *p;

  p = coef;
  ans = *p++;
  i = N;

  do
    ans = ans * x  +  *p++;
  while( --i );

  return( ans );
}

/*							p1evl()	*/
/*                                          N
 * Evaluate polynomial when coefficient of x  is 1.0.
 * Otherwise same as polevl.
 */

double p1evl( x, coef, N )
     double x;
     double coef[];
     int N;
{
  double ans;
  double *p;
  int i;

  p = coef;
  ans = x + *p++;
  i = N-1;

  do
    ans = ans * x  + *p++;
  while( --i );

  return( ans );
}

/* erfc function */
   
double erfc(a)
     double a;
{
  double p,q,x,y,z;


  if( a < 0.0 )
    x = -a;
  else
    x = a;

  if( x < 1.0 )
    return( 1.0 - erf(a) );

  z = -a * a;

  if( z < -MAXLOG )
    {
    under:
	
      if( a < 0 )
	return( 2.0 );
      else
	return( 0.0 );
    }

  z = exp(z);

  if( x < 8.0 )
    {
      p = polevl( x, P, 8 );
      q = p1evl( x, Q, 8 );
    }
  else
    {
      p = polevl( x, R, 5 );
      q = p1evl( x, S, 6 );
    }
  y = (z * p)/q;

  if( a < 0 )
    y = 2.0 - y;

  if( y == 0.0 )
    goto under;

  return(y);
}

/* erf function */

double erf(x)
     double x;
{
  double y, z;

  if( fabs(x) > 1.0 )
    return( 1.0 - erfc(x) );
  z = x * x;
  y = x * polevl( z, T, 4 ) / p1evl( z, U, 5 );
  return( y );

}
/* Exponentially scaled erfc function
   exp(x^2) erfc(x)
   valid for x > 1.
   Use with ndtr and expx2.  */
static double erfce (double x)
{
  double p,q;

  if (x < 8.0) {
    p = polevl(x, P, 8);
    q = p1evl(x, Q, 8);
  } else {
    p = polevl(x, R, 5);
    q = p1evl(x, S, 6); 
  }

  return p/q;
}


/* calcCirf */

void calcCirf(double *cmat, double *k, double *x, double *tau, double *mu,
              int *lenk, int *lenx){

  double alpha, beta, thresh;
  int i, row_cnt, col_cnt, len;
  row_cnt = 0; 
  col_cnt = 0;
  len = (*lenk) * (*lenx);
        
  for(i = 0; i < len; i++){
    if(k[col_cnt] == 0)
      cmat[i] = 0;
    else {
      alpha = (k[col_cnt] * (*tau)) / sqrt(2);
      beta = (x[row_cnt] - (*mu)) / ((*tau) * sqrt(2)); 
      thresh = beta - alpha;
      if(thresh < -1) 
	cmat[i] = .5 * erfce(-thresh) * exp(- pow(beta,2));
      else 		 
	cmat[i] = .5 * (1.0 + erf(thresh)) * exp(alpha * 
						 (alpha - 2.0 * beta));
	    
    }
    if(row_cnt < ((*lenx)-1))
      row_cnt++;
    else{
      row_cnt = 0;
      col_cnt++;
    }
  }
}

/* calcCirf_multi 
 *  for convolution using a per-component irf-mu and irf-tau
 */

void calcCirf_multi(double *cmat, double *k, double *x, double *tau, 
		    double *mu, int *lenk, int *lenx){
  double alpha, beta, thresh;
  int i, row_cnt, col_cnt, len;
  row_cnt = 0; 
  col_cnt = 0;
  len = (*lenk) * (*lenx);
  
  for(i = 0; i < len; i++){
      
    if(k[col_cnt] == 0)
      cmat[i] = 0;
    else {
	   
      alpha = (k[col_cnt] * tau[col_cnt]) / sqrt(2);
      beta = (x[row_cnt] - mu[col_cnt]) / (tau[col_cnt] * sqrt(2)); 
      if(k[col_cnt] < 0) {
	alpha = - alpha;
	beta = - beta;
      }
      thresh = beta - alpha;
      if(thresh < -1) 
	cmat[i] = .5 * erfce(-thresh) * exp(- pow(beta,2));
      else 		 
	cmat[i] = .5 * (1.0 + erf(thresh)) * exp(alpha * 
						 (alpha - 2.0 * beta));
	    
      /* Rprintf("alpha %f beta %f\n", alpha, beta);*/
    }
    if(row_cnt < ((*lenx)-1))
      row_cnt++;
    else{
      row_cnt = 0;
      col_cnt++;
    }
  }
}

/* calcB */

void calcB(double *bvec, double *k, int *lenk){

  int i, j;
  float sumcol;
#define matind(i,j,l) (((j-1) * (l) + (i-1))) 
        
  bvec[0] = 1;
  for(j = 2; j <= (*lenk); j++){
    for(i = 1; i < j; i++) {
      bvec[matind(i,j,(*lenk))] = (bvec[ matind(i,(j-1), (*lenk))] 
				   * k[j-2]) /(k[j-1] - k[i-1]);
    }
    sumcol = 0;
    for(i = 1; i <= (*lenk); i++)
      sumcol += bvec[matind(i,j,(*lenk))];
    bvec[matind(j,j,(*lenk))] = -sumcol;
  }
}

/************************************************
 
  Start code for numerical convolution 
 
  Numerical convolution routine 
  convolution with a scatter
  ConvSimpleAlg	
  ConvCentrImp	
  ConvBlockFunc
  ConvTrap
  double *source - pointer to array with result 
  double *scatter - pointer to array with Scatter (IRF) 
  int canN - number of time channels 
  double tau - lifetime of component  
  double t - time window (time from first time cannel till last)
*/



/* function 1 for numerical convolution of vectors */ 

void Conv1(double *result, double *measured, int *lenx, double *rate, 
	   double *xspace){

  double tau, ChannelWidth;
  int i, z, j;
  ChannelWidth = (*xspace);
  tau = 1/(*rate);
  result[0] = 0; 

  for (z=0; z<(*lenx); z++)
    result[z]=(tau)*(exp(-z*ChannelWidth/tau)-exp(-(z+1)*ChannelWidth/tau));
	
  for(i=(*lenx)-1;i>=1;i--){
    result[i] = 0.5*(measured[0]*result[i]+measured[i]*result[0]) + 0.25*result[i]*measured[0];
    for(j=1;j<i;j++)
      result[i] += result[j]*measured[i-j];

    result[i] *= ChannelWidth;
  }
}

void Conv2(double *result, double *measured, int *lenx, double *rate, 
	   double *xspace){

  double tau, canW, eps;
  int i;
  tau = 1/(*rate);
  canW = (*xspace);
  eps=exp(-canW/tau);

  result[0]=0;
	

  for (i=1; i<(*lenx); i++){
    result[i] = (result[i-1]+0.5*canW*measured[i-1])*eps + 0.5*canW*measured[i];
  }


}

void Conv3(double* source, double* reference, int *canN, double *rate, double *xspace, double *tauref){

  double tau, canW, eps;
  int i;
  tau = 1/ (*rate);
  canW = (*xspace);
  eps=exp(-canW/tau);


  source[0]=0;
  for (i=1; i<(*canN); i++){
    source[i] = (source[i-1]+0.5*canW*(1/(*tauref)-1/tau)*reference[i-1])*eps + 0.5*canW*(1/(*tauref)-1/tau)*reference[i];
  }

  for (i=0; i<(*canN); i++){
    source[i]+=reference[i];
  }

}


void ShiftCurve  (double *source, double *curve, double *shiftparam, 
		  int *length){ 
  int shift, i;
  double neybcontrib, selfcontrib;
  shift=floor((*shiftparam));
  selfcontrib=fabs((shift+1)-(*shiftparam));
  neybcontrib=fabs((*shiftparam)-shift);
  for (i=0; i<(*length); i++){
    if (((i+shift+1)<(*length))&&(i+shift>0))
      source[i]=selfcontrib*curve[(i+shift)]+neybcontrib*curve[(i+1+shift)];
    else
      source[i]=0; /*data[(i)*imwidth+j]; */
  }

}

