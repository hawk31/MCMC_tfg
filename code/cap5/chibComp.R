chibComp <- function(gibbsMix){
  
  logpost = gibbsMix@logpost
  k = gibbsMix@k
  mu = gibbsMix@mu
  sig = gibbsMix@sig
  p = gibbsMix@p
  max_iter = gibbsMix@max_iter
  data = gibbsMix@data
  
  group_size = gibbsMix@group_size
  group_x = gibbsMix@group_x
  group_x2 = gibbsMix@group_x2
  group_sum = gibbsMix@group_sum
  
  meanp = mean(data)
  varp = var(data)
  
  require(bayess)
  
  mix = list(k=k, mu=mu[1,],sig=sig[1,],p=p[1,])
  simu = gibbs(max_iter,data,mix)
  chib = simu$deno
  print(chib)
  lolik = simu$lolik
  
  lopos = order(logpost)[max_iter]
  part1 = lolik[lopos]
  part2 = sum(dnorm(mu[lopos,],
                    mean=meanp, sd=sig[lopos,],log=T)+
                dgamma(1/sig[lopos,],10,varp,log=T)-
                2*log(sig[lopos,]))+
    sum((rep(0.5,k)-1)*log(p[lopos,]))+
    lgamma(sum(rep(0.5,k)))-sum(lgamma(rep(0.5,k)))
  def = part1 + part2 - log(chib)
  return(def)
   
}

chibComp(prueba)
