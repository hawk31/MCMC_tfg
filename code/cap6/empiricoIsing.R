betas = seq(0.2, 1.8, by = 0.3)

par(mfrow=c(3,2))
set.seed(39)
x = matrix(sample(c(0,1),50*50,rep=T), nrow=50)

for(beta in betas){
  set.seed(93)
  y = isingSampler(x, 1000, beta)
  image(x=1:50, y=1:50,z=y, main=paste("beta=",beta))
}