library(coda.base)
library(coda.count)
library(dirmult)
library(randtoolbox)
library(digest)

source('init_lrnm_analise.R')
source('data_generations.R')

R1 = lapply(1:9, function(s){
  X = ex01(S = s, N = 100, n = 50)
  summ = lrnm_laplace.init(X)
  summ
})

R2 = lapply(1:6, function(s){
  X = ex02(S = s, N = 100, n = 50)
  summ = lrnm_laplace.init(X)
  summ
})
lapply(R2, function(ss) sapply(ss$sigma1, function(s) eigen(s)$values))


sapply(summ$sigma1, function(s) log(prop.table(eigen(s)$values) / prop.table(rep(1, length(eigen(s)$values)))) )

X = ex03(S = 1, n = 1)
summ = lrnm_laplace.init(X)
sapply(summ$sigma1, function(s) eigen(s)$values)



