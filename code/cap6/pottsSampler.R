pottsSampler <- function(x, num_col=2, max_iter=1000, beta){
  
  n = dim(x)[1]; m = dim(x)[2]
  
  for(i in 1:max_iter){
    permut = sample(1:(n*m))
    cat("Iteracion", i, "\n")
    
    for(k in 1:(n*m)){
      xcur = x[permut[k]]
      a = (permut[k]-1)%%n + 1
      b = (permut[k]-1)%/%n + 1
      xtilde = sample((1:num_col)[-xcur],1)
      prob = beta*(nei4(x,a,b,xtilde)-nei4(x,a,b,xcur))
      if(log(runif(1))<prob){
        x[permut[k]] = xtilde
      }
    }
    
  }
  return(x)
}