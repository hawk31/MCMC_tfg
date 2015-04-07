#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <math.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

NumericVector my_dnorm( double x, NumericVector means, double sd){
    int n = means.size() ;
    NumericVector res(n) ;
    for(int i=0; i<n; i++) res[i] = R::dnorm(x, means[i], sd, 0) ;
    return res ;
}

// [[Rcpp::export]]
int nei4(IntegerMatrix x, int a, int b, int col){
  int n = x.nrow(), m = x.ncol();
  int nei = 0;
  a = a-1;
  b = b-1;
  if(a != 0){
    if(x(a-1,b) == col){nei++;}
    }
  if(b != 0){
    if(x(a,b-1) == col){nei++;}
  }
  if(a != (n-1)){
    if(x(a+1,b) == col){nei++;}
  }
  if(b != (m-1)){
    if(x(a,b+1) == col){nei++;}
  }
  return nei;}

// norm_rs(a, b)
// generates a sample from a N(0,1) RV restricted to be in the interval
// (a,b) via rejection sampling.
// ======================================================================

// [[Rcpp::export]]

double norm_rs(double a, double b)
{
   double  x;
   x = Rf_rnorm(0.0, 1.0);
   while( (x < a) || (x > b) ) x = norm_rand();
   return x;
}

// half_norm_rs(a, b)
// generates a sample from a N(0,1) RV restricted to the interval
// (a,b) (with a > 0) using half normal rejection sampling.
// ======================================================================

// [[Rcpp::export]]

double half_norm_rs(double a, double b)
{
   double   x;
   x = fabs(norm_rand());
   while( (x<a) || (x>b) ) x = fabs(norm_rand());
   return x;
}

// unif_rs(a, b)
// generates a sample from a N(0,1) RV restricted to the interval
// (a,b) using uniform rejection sampling. 
// ======================================================================

// [[Rcpp::export]]

double unif_rs(double a, double b)
{
   double xstar, logphixstar, x, logu;

   // Find the argmax (b is always >= 0)
   // This works because we want to sample from N(0,1)
   if(a <= 0.0) xstar = 0.0;
   else xstar = a;
   logphixstar = R::dnorm(xstar, 0.0, 1.0, 1.0);

   x = R::runif(a, b);
   logu = log(R::runif(0.0, 1.0));
   while( logu > (R::dnorm(x, 0.0, 1.0,1.0) - logphixstar))
   {
      x = R::runif(a, b);
      logu = log(R::runif(0.0, 1.0));
   }
   return x;
}

// exp_rs(a, b)
// generates a sample from a N(0,1) RV restricted to the interval
// (a,b) using exponential rejection sampling.
// ======================================================================

// [[Rcpp::export]]

double exp_rs(double a, double b)
{
  double  z, u, rate;

//  Rprintf("in exp_rs");
  rate = 1/a;
//1/a

   // Generate a proposal on (0, b-a)
   z = R::rexp(rate);
   while(z > (b-a)) z = R::rexp(rate);
   u = R::runif(0.0, 1.0);

   while( log(u) > (-0.5*z*z))
   {
      z = R::rexp(rate);
      while(z > (b-a)) z = R::rexp(rate);
      u = R::runif(0.0,1.0);
   }
   return(z+a);
}




// rnorm_trunc( mu, sigma, lower, upper)
//
// generates one random normal RVs with mean 'mu' and standard
// deviation 'sigma', truncated to the interval (lower,upper), where
// lower can be -Inf and upper can be Inf.
//======================================================================

// [[Rcpp::export]]
double rnorm_trunc (double mu, double sigma, double lower, double upper)
{
int change;
 double a, b;
 double logt1 = log(0.150), logt2 = log(2.18), t3 = 0.725;
 double z, tmp, lograt;

 change = 0;
 a = (lower - mu)/sigma;
 b = (upper - mu)/sigma;

 // First scenario
 if( (a == R_NegInf) || (b == R_PosInf))
   {
     if(a == R_NegInf)
       {
     change = 1;
     a = -b;
     b = R_PosInf;
       }

     // The two possibilities for this scenario
     if(a <= 0.45) z = norm_rs(a, b);
     else z = exp_rs(a, b);
     if(change) z = -z;
   }
 // Second scenario
 else if((a * b) <= 0.0)
   {
     // The two possibilities for this scenario
     if((R::dnorm(a, 0.0, 1.0,1.0) <= logt1) || (R::dnorm(b, 0.0, 1.0, 1.0) <= logt1))
       {
     z = norm_rs(a, b);
       }
     else z = unif_rs(a,b);
   }
 // Third scenario
 else
   {
     if(b < 0)
       {
     tmp = b; b = -a; a = -tmp; change = 1;
       }

     lograt = R::dnorm(a, 0.0, 1.0, 1.0) - R::dnorm(b, 0.0, 1.0, 1.0);
     if(lograt <= logt2) z = unif_rs(a,b);
     else if((lograt > logt1) && (a < t3)) z = half_norm_rs(a,b);
     else z = exp_rs(a,b);
     if(change) z = -z;
   }
   double output;
   output = sigma*z + mu;
 return (output);
}

// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();

  if(n != n){
    return 0.0;
  }

  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}

// [[Rcpp::export]]
double sumC(NumericVector x) {
  int n = x.size();
  if(n != n){
    return 0.0;
  }
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}

// [[Rcpp::export]]
NumericMatrix matrixMult(NumericMatrix x, NumericMatrix y) {
  int n = x.nrow(), m=x.ncol(), i;
  NumericMatrix res(n,m);
  for(i=0;i<n*m;i++){
    res[i] = x[i]*y[i];
  }
  return res;
}

// [[Rcpp::export]]
NumericMatrix matrixdiffSq(NumericMatrix x, double y) {
  int n = x.nrow(), m=x.ncol(), i;
  NumericMatrix res(n,m);
  for(i=0;i<n*m;i++){
    res[i] = pow(x[i]-y, 2);
  }
  return res;
}

// [[Rcpp::export]]
NumericVector flatten(NumericMatrix x) {
  int n = x.nrow(), m=x.ncol(), i;
  NumericVector res(n*m);
  for(i=0;i<n*m;i++){
    res[i] = x[i];
  }
  return res;
}

// [[Rcpp::export]]
NumericVector flatten2(IntegerMatrix x) {
  int n = x.nrow(), m=x.ncol(), i;
  NumericVector res(n*m);
  for(i=0;i<n*m;i++){
    res[i] = x[i];
  }
  return res;
}

// [[Rcpp::export]]
IntegerVector vcheck(IntegerVector x, int val){
  int n = x.size();
  int i;
  IntegerVector res(n, 0);
  for(i = 0; i < n; i++){
    if(x[i] == val){
      res[i] = 1;
    }
  }
  return res;
}

// [[Rcpp::export]]
NumericVector vcheck2(NumericVector x, int val){
  int n = x.size();
  int i;
  NumericVector res(n, 0.0);
  for(i = 0; i < n; i++){
    if(x[i] == val){
      res[i] = 1;
    }
  }
  return res;
}


// [[Rcpp::export]]
double intecxx(Function inte, NumericVector x, NumericVector y, double a, double b) {
    NumericVector res;
    res = inte(x, y, a, b);
    return res[0];
}


// [[Rcpp::export]]
NumericVector GSampler(NumericMatrix y, int ncol, int max_iter, int nrow,
	int m, IntegerMatrix x, NumericMatrix mu, NumericVector sigma2,
	NumericVector beta, IntegerMatrix xcum, NumericVector n,
	NumericVector Z, NumericVector dom){

  Rcpp::Rcout << "max_iter: " << max_iter << std::endl;

	int i, k, l, j, co, h;
	NumericVector colors(ncol);
	colors = Rcpp::seq(1, ncol);

  NumericVector prob(ncol, 0.1);

  double betaactual;


	double lvr = 0.0, sese = 0.0, betatilde, accp;


	for(i = 1; i < max_iter; i++){

    betaactual = beta[i-1];

    Rcpp::Rcout << "i: " << i << std::endl;
   
		double lvr = 0.0;
		for(k = 0; k < nrow; k++){
			for(l = 0; l < m; l++){
				for(j = 1; j <= ncol; j++){
					n[j-1] = nei4(x, k+1, l+1, j);
				}
        // Reset all values
        for(h = 0; h < ncol; h++){
          prob[h] = rnorm_trunc(y(k,l), mu(i-1, h), 0.0, 255.0);
        }
        prob = prob * 0;
        prob = exp(betaactual*n)* my_dnorm(y(k,l), mu(i-1, _), sqrt(sigma2[i-1]));


				//x(k,l) = RcppArmadillo::sample(colors, 1, 0, prob)[0];
				xcum(k*nrow, x(k,l)-1) = xcum(k*nrow, x(k,l)-1) + 1;
				lvr = lvr + n[x(k,l)-1];

			}
		}



    Rcpp::Rcout << "Done with a loop!!!!!!!  "<< i << std::endl;

		NumericVector flattened_x(nrow*m);
		flattened_x = flatten2(x);

    Rcpp::Rcout << "flatten1  "<< flattened_x << std::endl;

		NumericVector flattened_y(nrow*m);
		flattened_y = flatten(y);

    Rcpp::Rcout << "flatten2  "<< flattened_y << std::endl;

		mu(i, 0) = rnorm_trunc(meanC(flattened_y[flattened_x==1]), sqrt(sigma2[i-1]/sumC(vcheck2(flattened_x, 1))), 0.0, mu(i-1, 1));
    
    Rcpp::Rcout << "mu(i,0)"<< mu(i, 0) << std::endl;

    // Check for nans
		for(co = 1; co < (ncol-1); co++){
			mu(i, co) = rnorm_trunc(meanC(flattened_y[flattened_x==(co+1)]), sqrt(sigma2[i-1]/sumC(vcheck2(flattened_x, co+1))), mu(i,co-1) , mu(i-1, co+1));
      if(mu(i, co) != mu(i, co)){
        mu(i, co) = mu(i-1, co);
      }
      Rcpp::Rcout << "mu(i,pr)"<< meanC(flattened_y[flattened_x==(co+1)]) << std::endl;
      Rcpp::Rcout << "mu(i,pr2)"<< sumC(vcheck2(flattened_x, co+1)) << std::endl;
		}

		mu(i, (ncol-1)) = rnorm_trunc(meanC(flattened_y[flattened_x==ncol]), sqrt(sigma2[i-1]/sumC(vcheck2(flattened_x, ncol))), mu(i, ncol-2), 255.0);
    if(mu(i, (ncol-1)) != mu(i, (ncol-1))){
        mu(i, (ncol-1)) = mu(i-1, (ncol-1));
    }
    Rcpp::Rcout << "mu(i,ult)"<< mu(i, (ncol-1)) << std::endl;

		sese = sumC(flatten(matrixdiffSq(y, mu(i, 0))) * vcheck2(flattened_x,1));

		for(co = 1; co < ncol; co++){
			sese = sese + sumC(flatten(matrixdiffSq(y, mu(i, co))) * vcheck2(flattened_x, co+1));
		}
		sigma2[i] = 1/rgamma(1, nrow*m/2, sese/2)[0];
		betatilde = beta[i-1]+runif(1, -0.05, 0.05)[0];
    
     Environment myEnv = Environment::global_env();
     Function inte = myEnv["inte"];
    
		accp = lvr * (betatilde - beta[i-1]) + intecxx(inte, dom, Z, betatilde, beta[i-1]);

		if(log(runif(1)[0]) < accp){
			beta[i] = betatilde;
		}
		else{
			beta[i] = beta[i-1];
		}

	}

	List out(5);
	out[0] = beta;
	out[1] = xcum;
	out[2] = x;
	out[3] = mu;
	out[4] = sigma2;
  //return out;

}

