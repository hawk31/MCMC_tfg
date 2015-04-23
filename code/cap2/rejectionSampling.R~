#!usr/bin/env RScript
curve(dnorm(x),from=-3,to=3,ylim=c(0,.75))
curve(3*dcauchy(x,0,2),from=-3,to=3,add = T,col="red")
i = 1
M = 3
alea = numeric(0)
repeat{
  if(i>=n){break}
  i = i+1
  x = rcauchy(1,0,2)
  u = runif(1)
  fx = dnorm(x,0,1)
  if(u<fx/(M*dcauchy(x))){
    alea = c(alea,x)
  }
}