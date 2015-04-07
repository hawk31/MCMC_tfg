mat = matrix(
  sample(1:3, 20*20, rep=T), ncol=20)

pathSampling(x, 2, 1000, 0.2)

system.time(
  generatefbeta(x, ncol=3))

inte = function(x, y, a, b){
  model = approxfun(x, y)
  return(integrate(model, a,b)$value)
}

def = baySegmentation(mat, max_iter=10, ncol=3)
flatten(mat)
flatten2(mat)

def = baySegmentation(meh, 4, 10)

set.seed(93)
system.time({mod = bay(meh, ncol=6, max_iter = 100)})
set.seed(93)
mod2 = reconstruct(10, meh)


affect=function(u) order(u)[6]
aff=apply(mod$xcum,1,affect)
aff=t(matrix(aff,100,100))
image(1:100,1:100,aff,col=gray(6:1/6),xlab="",ylab="")
image(1:100,1:100,mod$,col=gray(6:1/6),xlab="",ylab="")

par(mfrow=c(2,1))
image(1:100,1:100,aff,col=gray(6:1/6),xlab="",ylab="")
image(1:100,1:100,meh,xlab="",ylab="")

install.packages("png")
library(png)

par(mfrow=c(1,1))
lena = readPNG(source = "~/TFG/code/cap6/lena-grey-flip2.png", native = FALSE, info = FALSE)

lena = floor(255*lena)

image(1:192,1:192,t(lena),xlab="",ylab="", col=gray.colors(255))

system.time({mod = bay(t(lena), ncol=6, max_iter = 4)})

affect=function(u) order(u)[6]
aff=apply(mod$xcum,1,affect)
aff=t(matrix(aff,192,192))
image(1:192,1:192,aff,col=gray(6:1/6),xlab="",ylab="")

data(Menteith)
Menteith = as.matrix(Menteith)

system.time({mod = bay(Menteith, ncol=6, max_iter = 4)})
aff=apply(mod$xcum,1,affect)
aff=t(matrix(aff,100,100))
image(1:100,1:100,aff,col=gray(6:1/6),xlab="",ylab="")
