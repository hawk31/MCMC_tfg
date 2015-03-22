#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <math.h>       /* exp */

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

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
NumericMatrix isingSampler(NumericMatrix x, int max_iter, double beta){
  int n = x.nrow(), m = x.ncol();
  int i;
  int k;
  int l;
  
  NumericVector rows(n);
  rows = Rcpp::seq(1,n);
  NumericVector cols(m);
  cols = Rcpp::seq(1,m);
  
  NumericVector pos(2);
  pos = Rcpp::seq(0,1);
  
  NumericVector permut_rows(n);
  NumericVector permut_cols(m);
  
  NumericVector prob(2, 0.5);
  
  int n0;
  int n1;
  
  int pos1;
  int pos2;

  
  double res;

  
  for(i = 0; i < max_iter; i++){
    permut_rows = RcppArmadillo::sample(rows,n,0);
    permut_cols = RcppArmadillo::sample(cols,m,0);

    
    for(k = 1; k <= n; k++){
      for(l = 1; l <= m; l++){
        n0 = nei4(x, permut_rows[k-1], permut_cols[l-1], 0);
        n1 = nei4(x, permut_rows[k-1], permut_cols[l-1], 1);
        prob[0] = exp(beta*n0);
        prob[1] = exp(beta*n1);
        
        
        pos1 = permut_rows[k-1]-1;
        pos2 = permut_cols[l-1]-1;
        
        double suma = prob[0] + prob[1];
        prob[0] = prob[0]/suma;
        prob[1] = prob[1]/suma;
        
        res = RcppArmadillo::sample(pos, 1, 0, prob)[0];
        //res = Rcpp::Function(sample(pos,1,0,prob));
        Rcpp::Function(gc());
        
        
        x(pos1, pos2) = res;
        
      }
    }
    Rcpp::Function(gc());

  }
  
  return x;

}