gammaSampler<-function (n, a, b) 
{
  mu <- a/b
  sig <- sqrt(a/b^2)
  vec <- numeric(n)
  x <- mu
  vec[1] <- x
  for (i in 2:n) {
    can <- rnorm(1, mu, sig)
    aprob <- min(1, (dgamma(can, a, b)/dgamma(x, 
                                              a, b))/(dnorm(can, mu, sig)/dnorm(x, 
                                                                                mu, sig)))
    u <- runif(1)
    if (u < aprob) vec[i] <- can
    else vec[i]<- vec[i-1]
  }
  vec
}


