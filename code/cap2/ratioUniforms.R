u = runif(10000,0,1)
v = runif(10000,-1,1)
alea = numeric(0)

for(i in 1:length(u)){
  if(u[i]<sqrt(dnorm(v[i]/u[i]))){alea = c(alea,v[i]/u[i])}
}