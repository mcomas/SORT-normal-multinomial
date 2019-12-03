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

evaluate_multinomial = function(X){
  d = ncol(X)-1
  n_ = rowSums(X)
  
  p = prop.table(colSums(X))
  n = rowSums(X)
  E = n %*% t(p)
  chisq.m1 = rowSums( (X - E)^2 / E )
  test1 = median(1-pchisq(chisq.m1, d))
  
  I = 1
  S = cov(X)[-I,-I]
  V0 = (mean(n_) * ( diag(p) - p %*% t(p) ))[-I,-I]
  N = nrow(X)
  LH0 = - N / 2 * log(det(V0)) - N / 2 * sum(diag(solve(V0) %*% S))
  LH1 = - N / 2 * log(det(S)) - N * d / 2
  chisq.m2 = 2 * (LH1 - LH0)
  test2 = 1-pchisq(chisq.m2, d*(d+1)/2)
  c(test1, test2)
}


res_1 = replicate(100, {
  X = ex01(S = 3, N = 1000, n = 50)
  evaluate_multinomial(X)
})

res_3 = replicate(100, {
  X = ex03(S = 2, n = 1)
  evaluate_multinomial(X)
})

apply(t(res_1), 2, quantile, 0.95)
apply(t(res_3), 2, summary)

boxplot(t(res_1)[,1], t(res_3)[,1])
boxplot(t(res_1)[,2], t(res_3)[,2])
boxplot(t(res_1)[,3], t(res_3)[,3])
boxplot(t(res_1)[,4], t(res_3)[,4])
boxplot(t(res_1)[,5], t(res_3)[,5])
