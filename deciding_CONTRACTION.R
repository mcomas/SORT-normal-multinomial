library(coda.base)
library(coda.count)
library(dirmult)
library(randtoolbox)
library(digest)
library(HardyWeinberg)
library(dplyr)
library(tidyr)

# source('init_lrnm_analise.R')
source('data_generations.R')


res_1 = replicate(100, {
  X = ex01(S = 3, N = 100, n = 50)
  d = ncol(X)-1
  chisq_ = function(X){
    X_ = X[sample(seq_len(nrow(X)), replace = T),]
    p = prop.table(colSums(X_))
    n = rowSums(X_)
    E = n %*% t(p)
    mean( 1 - pchisq(rowSums( (X_ - E)^2 / E ), d) )
  }
  quantile(replicate(100, chisq_(X)), 0.95)
})

res_3 = replicate(100, {
  X = ex03(S = 2, n = 1)
  d = ncol(X)-1
  chisq_ = function(X){
    X_ = X[sample(seq_len(nrow(X)), replace = T),]
    p = prop.table(colSums(X_))
    n = rowSums(X_)
    E = n %*% t(p)
    mean( 1 - pchisq(rowSums( (X_ - E)^2 / E ), d) )
  }
  quantile(replicate(100, chisq_(X)), 0.95)
})

apply(t(res_1), 2, quantile, 0.95)
apply(t(res_3), 2, summary)

boxplot(t(res_1)[,1], t(res_3)[,1])
boxplot(t(res_1)[,2], t(res_3)[,2])
boxplot(t(res_1)[,3], t(res_3)[,3])
boxplot(t(res_1)[,4], t(res_3)[,4])
boxplot(t(res_1)[,5], t(res_3)[,5])
