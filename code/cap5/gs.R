#!/usr/bin/env RScript
# Gibbs Sampler (Gaussian mixtures)

gibbsMixture <- function(data,k,max_iter=1000){
  n = length(data)
  mu = mean(data)
  sig = var(data)
  
  # Preallocating data
  z = rep(NA,n)
  group_size = rep(NA, k)
  group_x = rep(NA, k)
  group_sum = rep(NA, k)
  
  mu_mat = sig_mat = pj_mat = matrix(NA, nrow=max_iter, ncol=k)
  mu_mat[1,] = rep(mu,k)
  pj_mat[1,] = rep(1,k)/k
  sig_mat[1,] = rep(sig,k)
  
  ## Possible chunk for likelihood
  
  #######
  
  ### Main loop
  
  for(i in 2:max_iter){
    # z estimation
    for(j in 1:n){
      p = pj_mat[i-1,]*dnorm(data[j],mu_mat[i-1,],sqrt(sig_mat[i-1,]))
      z[j] = sample(1:k, size = 1, prob = p)
    }
    
    # rest of parameters
    
    for(j in 1:k){
      group_size[j] = length(which(z == j))
      group_x[j] = sum(as.numeric(z == j)*data)
      group_sum[j] = sum(as.numeric(z==j)*(data-group_x[j]/group_size[j])^2)
    }
    
    mu_mat[i,] = rnorm(k, (mean(data) + group_x)/(group_size + 1), sqrt(sig_mat[i-1,]/group_size + 1))
    sig_mat[i,] = 1/rgamma(k, .5*(20+group_size),
                         var(data) + .5* group_sum + .5* group_size/(group_size + 1)*(mean(data) - group_x/group_size)^2)
    
    pj_mat[i,] = rdirichlet(params = group_size + 0.5)
    
    
    
  }
  class = setClass("MCMC mixture", slots=c(mu="matrix", sig="matrix", p="matrix", group="numeric"))
  res = class(mu = mu_mat, sig = sig_mat, p = pj_mat, group = z)
  return(res)
}


data(faithful)
hist(faithful$eruptions)
hist(faithful$waiting, freq = F)
curve(0.6*dnorm(x,mean=80.73,sd=sqrt(31)),add = T, from=40, to=100)

prueba = gibbsMixture(data = faithful$waiting, k = 2, max_iter = 1000)
