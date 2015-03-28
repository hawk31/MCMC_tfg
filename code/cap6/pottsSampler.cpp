#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <math.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
int nei4(NumericMatrix x, int a, int b, int col){
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

// [[Rcpp::export]]
NumericMatrix pottsSampler(NumericMatrix x, int num_col, int max_iter, double beta){

	int n = x.nrow(), m = x.ncol();
	int i;
	int xcur;
	int k, j;
	int a, b;
	int xtilde;
	double prob;
  
  NumericVector pixels(n*m);
	pixels = Rcpp::seq(1, n*m);

	for(i = 0; i < max_iter; i++){

    NumericVector permut(n*m);
		permut = RcppArmadillo::sample(pixels,n*m,1);

		for(k = 0; k < (n*m); k++){
      NumericVector colors(num_col);
			colors = Rcpp::seq(1, num_col);
			xcur = x[permut[k] - 1];
			a = remainder((permut[k]-1),n) + 1;
			b = (permut[k]-1)/n + 1;
			colors.erase(xcur - 1);
			xtilde = RcppArmadillo::sample(colors, 1, 0)[0];
			prob = beta*(nei4(x, a, b, xtilde) - nei4(x, a, b, xcur));
      double alea = runif(1)[0];
			if(log(alea) < prob){
				x[permut[k] - 1] = xtilde;
			}


		}

	}
	return x;
}