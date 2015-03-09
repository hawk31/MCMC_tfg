#!/usr/local/env RScript

rdirichlet <- function(n=1,params=c(1,1)){
  k = length(params)
  array = matrix(NA, nrow = n, ncol = k)
  for(i in 1:n){
    support = rgamma(k,shape=params)
    array[i,] = support/sum(support)
  }
  return(array)
}