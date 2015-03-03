library(ars)
#Muestrear de una Beta(2,3)
n=20
f2<-function(x,a,b){(a-1)*log(x)+(b-1)*log(1-x)}
f2prima<-function(x,a,b){(a-1)/x-(b-1)/(1-x)}
mysample2<-ars(20,f2,f2prima,x=c(0.3,0.6),m=2,
               lb=TRUE,xlb=0,ub=TRUE,xub=1,a=2,b=3)
mysample2