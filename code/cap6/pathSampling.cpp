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
double pathSampling(NumericMatrix x, int ncol, int max_iter,
	double beta){

	int n = x.nrow(), m = x.ncol();
	double S = 0.0, s = 0.0;
	int i, k, l, n0, n1, col;

	NumericVector rows(n);
	rows = Rcpp::seq(1, n);

	NumericVector cols(n);
	cols = Rcpp::seq(1, m);

	NumericVector colors(ncol);
	colors = Rcpp::seq(1, ncol);


	for(i = 0; i < max_iter; i++){
		s = 0.0;

		rows = RcppArmadillo::sample(rows, n, 0);
		cols = RcppArmadillo::sample(cols, m, 0);

		for(k = 0; k < n; k++){
			for(l = 0; l < n; l++){
				n0 = nei4(x, rows[k], cols[l], x(rows[k]-1, cols[l]-1));
				col = RcppArmadillo::sample(colors, 1, 0)[0];
				n1 = nei4(x, rows[k], cols[l], col);
				if(log(runif(1)[0]) < (beta*(n1-n0))){
					x(rows[k]-1, cols[l]-1) = col;
					n0 = n1;
				}
				s = s + n0;

			}
		}
		if(2*i > max_iter){
			S = S + s;
		}

	}
	return 2*S/max_iter;

}