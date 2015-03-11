#!/usr/bin/env RScript

pivotalReor <- function(gibbsMix){
  mu = gibbsMix@mu
  sig = gibbsMix@sig
  p = gibbsMix@p
  logpost = gibbsMix@logpost
  n = gibbsMix@n
  k = gibbsMix@k
  data = gibbsMix@data
  max_iter = gibbsMix@max_iter
  
  map_indices = order(logpost, decreasing = T)[1]
  map = list(mu = mu[map_indices,], sig= sig[map_indices,],
             p = p[map_indices,])
  
  lili = matrix(NA, n, k)
  allocation = matrix(NA, n, k)
  
  for(t in 1:n){
    lili[t,] = map$p*dnorm(data[t], mean = map$mu, sd=sqrt(map$sig))
    lili[t,] = lili[t,]/sum(lili[t,])
  }
  
  ordered_mu = matrix(NA, ncol=k, nrow=max_iter)
  ordered_sig = matrix(NA, ncol=k, nrow=max_iter)
  ordered_p = matrix(NA, ncol=k, nrow=max_iter)
  
  require(combinat)
  
  perma = permn(k)
  
  for(t in 1:1000){
    entropies = rep(0, factorial(k))
    for(j in 1:n){
      allocation[j,] = p[t,]*dnorm(data[j], mean=mu[t,], sd=sqrt(sig[t,]))
      allocation[j,] = allocation[j,]/sum(allocation[j,])
      for(i in 1:factorial(k)){
        entropies[i] = entropies[i] + sum(lili[j,]*log(allocation[k,perma[[i]]]))
      }
    }
    best_ordering = order(entropies, decreasing=T)[1]
    ordered_mu[t,] = mu[t,perma[[best_ordering]]]
    ordered_sig[t,] = sig[t,perma[[best_ordering]]]
    ordered_p[t,] = p[t,perma[[best_ordering]]]
  }
  
  res = list(mu=ordered_mu, sig=ordered_sig, p=ordered_p)
  return(res)
  
}

piv = pivotalReor(prueba)
