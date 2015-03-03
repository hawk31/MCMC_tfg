n=1000
m=5

cadena = rexp(n*m,rate = 1/3)

batch.means <- function(chain,f,m){
  fN = mean(f(chain))
  n = length(chain)/m
  li = numeric(m)
  for(i in 1:m){
    li[i]=mean(f(chain[((i-1)*n+1):(i*n)]))
  }
  sigma = n/(m-1)*sum((li-fN)^2)
  return(sigma)
}

batch.means(cadena,identity,5)