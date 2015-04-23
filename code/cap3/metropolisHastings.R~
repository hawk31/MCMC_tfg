n = 1000
i = 1
alea = numeric(n)
while(i<=n){
  prop = rnorm(1,alea[i],0.5)
  u = runif(1)
  alpha = min(1,(dnorm(prop)*dnorm(alea[i],0,0.5))/(dnorm(alea[i])*
  			dnorm(prop,alea[i],0.5)))
  if(u<alpha){
    alea[i+1]=prop
    i=i+1
  }
}