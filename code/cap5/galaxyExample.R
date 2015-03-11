#!/usr/bin/env RScript 

data(faithful)

hist(faithful$waiting, breaks = 20, col="lightblue",
     freq=F, main="Mixtura Gaussiana")


mixture = gibbsMixture(faithful$waiting,k = 2,max_iter = 1000)
(mu = apply(mixture@mu, 2, mean))
(sig = apply(mixture@sig, 2, mean))
(p = apply(mixture@p, 2, mean))

curve(p[1]*dnorm(x,mu[1],sqrt(sig[1])),add=T)
curve(p[2]*dnorm(x,mu[2],sqrt(sig[2])),add=T, col="red")

