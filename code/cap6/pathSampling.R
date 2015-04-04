pathSampling <- function(x, ncol=2, max_iter = 10^3, beta){
  n = dim(x)[1]; m = dim(x)[2]
  S = 0
  
  for(i in seq_len(max_iter)){
    if(i%%100==0){
      cat("Iteración ",i,"\n")
    }
    s = 0
    rows = sample(1:n)
    cols = sample(1:m)
    
    for(k in seq_len(n)){
      for(l in seq_len(m)){
        n0 = nei4(x, rows[k], cols[l], x[rows[k], cols[l]])
        col = sample(1:ncol, 1)
        n1 = nei4(x, rows[k], cols[l], col)
        if(log(runif(1))< (beta*(n1-n0))){
          x[rows[k], cols[l]] = col
          n0 = n1
        }
        s = s+n0
      }
    }
    if(2*i > max_iter){
      S = S + s
    }
  }
  return(2*S/max_iter)
}