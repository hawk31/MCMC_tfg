generarec<- function(n, M)
{
  resul<- numeric(n)
  inten<- integer(n)
  for (j in 1:n)
  {
    i=0
    repeat
    {
    i=i+1
    x=rcauchy(1,0,2)
    u=runif(1)
    fx=dnorm(x)
    if (u< fx/(M*dcauchy(x,0,2))){
    	resul[j]<- x
    	inten[j]<- i
    	break
	}
   }
  }
 return(list(x=resul,nint=inten))

}


