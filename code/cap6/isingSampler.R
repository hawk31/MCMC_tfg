isingSampler <- function(max_iter, n, m=n, beta){
  x = matrix(sample(c(0,1), n*m, rep=T),ncol=n)
  
  for(i in 1:max_iter){
    sample_rows = sample(1:n)
    sample_cols = sample(1:m)
    
    for(k in 1:n){
      for(l in 1:m){
        n0 = neighbors4(x, sample_rows[k], sample_cols[l], 0)
        n1 = neighbors4(x, sample_rows[k], sample_cols[l], 1)
        x[sample_rows[k], sample_cols[l]] = sample(c(0,1), 1, prob = exp(beta*c(n0,n1)))
      }
    }
  }
  return(x)  
}