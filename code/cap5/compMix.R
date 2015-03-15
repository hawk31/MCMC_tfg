#!/usr/bin/env RScript

set.seed(93)
chib = numeric(3)
data = faithful$waiting

for(i in 2:4){
  model = gibbsMixture(data,k=i)
  chib[i-1] = chibComp(model)
}