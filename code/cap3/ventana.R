cadena = rexp(5000,rate = 1/3)

window.est <- function(cadena,weights){
  aut = as.numeric(acf(cadena,plot=F,type="covariance")$acf)
  sigma = aut[1]+2*sum(pesos*aut[2:(length(pesos)+1)])
  return(sigma)
}

pesos = rep(1/12,12)
window.est(cadena,pesos)