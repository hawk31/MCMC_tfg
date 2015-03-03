#! /usr/bin/env RScript
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
                if (u < aprob) 
                        x <- can
                vec[i] <- x
        }
        vec
}