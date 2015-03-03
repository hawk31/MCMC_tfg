#!usr/bin/env RScript
set.seed(99)
x = runif(10^5,0,exp(1))
v = exp(1)
f = function(x){3*exp(-3*x)}

alea = f(x)
(est = v*mean(alea))