#!usr/bin/env RScript
y = rcauchy(10^5,0,2)
f = cos(y)*dnorm(y)/dcauchy(y,0,2)
mean(f)

mean(cos(rnorm(10^5)))