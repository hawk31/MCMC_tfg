generatefbeta <- function(x, max_iter=10^3, ncol=2){
  search = seq(0.1, 2, by=0.1)
  Z = seq_along(search)
  i = 1
  for(beta in search){
    cat("Beta: ", beta, "\n")
    Z[i] = pathSampling(x, ncol, max_iter, beta)
    i = i+1
  }
  plot(search, Z, main="f(beta) approx.", type="l")
  return(Z)
}