niter = 10^4

theta = c(2,3,1/2)
sim = matrix(NA,nrow=niter,ncol=3)
sim[1,] = c(0.4,1.2,0.3) ## Valores iniciales (cualesquiera)

for(i in 2:niter){
  sim[i,1] = rexp(1,1+theta[1]*sim[i-1,2]+theta[2]*sim[i-1,3])
  sim[i,2] = rexp(1,1+theta[1]*sim[i,1]+theta[3]*sim[i-1,3]) 
  ## Nótese que aquí ya hemos actualizado el primer
  ## componente y lo usamos para acelerar convergencia.
  sim[i,3] = rexp(1,1+theta[2]*sim[i,1]+theta[3]*sim[i,2]) ## Idem
  
}