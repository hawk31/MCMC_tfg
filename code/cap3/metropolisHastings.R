n = 1000
alea = numeric(n)
alea[1]=0   #mu
for (i in 2:n)
{
  y = rnorm(1,alea[i-1],0.5)
  u = runif(1)
  alpha = min(1,(dnorm(y)*dnorm(alea[i-1],y,0.5))/(dnorm(alea[i-1])*
                                                      dnorm(y,alea[i-1],0.5)))
  if(u<alpha) alea[i]=y
  else alea[i]=alea[i-1]
}


