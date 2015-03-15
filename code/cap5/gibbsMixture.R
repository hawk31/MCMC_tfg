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
  group_x2 = rep(NA, ,k)
  group_sum = rep(NA, k)
  
  mu_mat = sig_mat = pj_mat = matrix(NA, nrow=max_iter, ncol=k)
  mu_mat[1,] = rep(mu,k)
  pj_mat[1,] = rep(1,k)/k
  sig_mat[1,] = rep(sig,k)
  
  likelihood = matrix(NA, n, k)
  logpost = rep(NA, max_iter)
  
  ## Chunk for likelihood
  
  for(j in 1:k){
    likelihood[,j] = pj_mat[1,j]*dnorm(x=data, mean=mu_mat[1,j],
                                       sd=sqrt(sig_mat[1,j]))
  }
  
  logpost[1] = sum(log(apply(likelihood,1,sum))) + sum(dnorm(mu_mat[1,],mean(data),sqrt(sig_mat[1,]),log=T)) -
    (10+1)*sum(log(sig_mat[1,])) - sum(var(data)/sig_mat[1,]) + 0.5*sum(log(pj_mat[1,]))
  #######
  
  ### Main loop
  
  for(i in 2:max_iter){
    # z estimation
    for(j in 1:n){
      p = pj_mat[i-1,]*dnorm(data[j],mu_mat[i-1,],sqrt(sig_mat[i-1,]))
      if(any(is.na(p))){p = rep(1,k)/k}
      z[j] = sample(1:k, size = 1, prob = p)
    }
    
    # rest of parameters
    
    for(j in 1:k){
      group_size[j] = length(which(z == j))
      group_x[j] = sum(as.numeric(z == j)*data)
      group_x2[j] = sum(as.numeric(z==j)*data)
      group_sum[j] = sum(as.numeric(z==j)*(data-group_x[j]/group_size[j])^2)
    }
    
    mu_mat[i,] = rnorm(k, (mean(data) + group_x)/(group_size + 1), sqrt(sig_mat[i-1,]/group_size + 1))
    if(any(is.na(mu_mat[i,]))) mu_mat[i,] = mu_mat[i-1,]
    sig_mat[i,] = 1/rgamma(k, .5*(20+group_size),
                         var(data) + .5* group_sum + .5* group_size/(group_size + 1)*(mean(data) - group_x/group_size)^2)
    if(any(is.na(sig_mat[i,]))) sig_mat[i,] = sig_mat[i-1,]
    
    pj_mat[i,] = rdirichlet(params = group_size + 0.5)
    
    #likelihood
    for(j in 1:k){
      likelihood[,j] = pj_mat[i,j]*dnorm(x=data,mean=mu_mat[i,j],sd=sqrt(sig_mat[i,j]))
    }
    
    logpost[i] = sum(log(apply(likelihood,1,sum))) + sum(dnorm(mu_mat[i,],mean(data),sqrt(sig_mat[i,]),log=T)) -
      (10+1)*sum(log(sig_mat[i,])) - sum(var(data)/sig_mat[i,]) + 0.5*sum(log(pj_mat[i,]))
    
    
  }
  class = setClass("MCMC mixture", slots=c(mu="matrix", sig="matrix", p="matrix", group="numeric", logpost="numeric", n="numeric",
                                           k="numeric", data="numeric", max_iter="numeric", group_size = "numeric",
                                           group_x="numeric", group_sum="numeric", group_x2 = "numeric"))
  res = class(mu = mu_mat, sig = sig_mat, p = pj_mat, group = z, logpost = logpost, n=n, k=k, data=data, max_iter=max_iter,
              group_size = group_size, group_x = group_x , group_sum = group_sum, group_x2 = group_x2)
  return(res)
}


data(faithful)
hist(faithful$eruptions)
hist(faithful$waiting, freq = F)
curve(0.6*dnorm(x,mean=80.73,sd=sqrt(31)),add = T, from=40, to=100)

prueba = gibbsMixture(data = faithful$waiting, k = 4, max_iter = 1000)
