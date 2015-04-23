install.packages("mice")
library(mice)

data = mtcars
data$am = factor(data$am)
data$vs = factor(data$vs)

datosoriginales = data


n = nrow(data)
m = ncol(data)

p = 0.15

## Esperamos aprox n*m*p datos ausentes

for(i in 1:n){
  for(j in 1:m){
    u = runif(1)
    if(u < p){
      data[i,j] = NA
    }
  }
}

expected = n*m*p
length(which(is.na(data)))

imputation = mice(data, m=10, maxit=100, method = rep("fastpmm", 11))


a = as.numeric(completos[is.na(data)])
b = as.numeric(datosoriginales[is.na(data)])

rmse = sqrt(mean((a-b)^2))


data(nhanes2)
head(nhanes2)

imputation = mice(nhanes2, m=10, maxit=100, method = rep("fastpmm", 4))
completos = complete(imputation, action = 5)