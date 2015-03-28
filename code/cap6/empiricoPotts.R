betas = seq(0.2, 1.4, by = 0.4)

par(mfrow=c(2,2))
set.seed(39)
x = matrix(sample(1:4,50*50,rep=T), nrow=50)

for(beta in betas){
  set.seed(93)
  y = pottsSampler(x, num_col=4, 1000, beta)
  image(x=1:50, y=1:50,z=y, main=paste("beta=",beta))
}