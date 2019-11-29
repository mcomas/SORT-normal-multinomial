library(coda.base)
library(coda.count)
library(dirmult)
library(randtoolbox)
library(digest)
library(HardyWeinberg)
library(dplyr)
library(tidyr)

source('init_lrnm_analise.R')
source('data_generations.R')

R1 = lapply(1:9, function(s){
  print(s)
  X = ex01(S = s, N = 100, n = 50)
  summ = lrnm_laplace.init(X)
  summ
})
lapply(R1, function(s) sapply(s$sigma, function(s_) summary(eigen(s_)$values)))

R2 = lapply(1:6, function(s){
  print(s)
  X = ex02(S = s, N = 100, n = 50)
  summ = lrnm_laplace.init(X)
  summ
})
lapply(R2, function(s) sapply(s$sigma, function(s_) summary(eigen(s_)$values)))


R3_1 = lapply(1:20, function(n_){
  X = ex03(S = 1, n = n_)
  summ = lrnm_laplace.init(X)
  summ
})
lapply(R3_1, function(s) sapply(s$sigma, function(s_) summary(eigen(s_)$values)))

R3_2 = lapply(1:20, function(n_){
  X = ex03(S = 2, n = n_)
  summ = lrnm_laplace.init(X)
  summ
})
lapply(R3_2, function(s) sapply(s$sigma, function(s_) summary(eigen(s_)$values)))




lapply(R3_1, function(s) sapply(s$mu, function(s_) s_))

lapply(R3_1[1], function(s) sapply(s$sigma1, function(s_) summary(eigen(s_)$values)))
lapply(R3_1, function(s) sapply(s$sigma2, function(s_) summary(eigen(s_)$values)))

max1 = lapply(R1, function(s) sapply(s$sigma1, function(s_) max(eigen(s_)$values)))
max2 = lapply(R2, function(s) sapply(s$sigma1, function(s_) max(eigen(s_)$values)))
max3_1 = lapply(R3_1[1:5], function(s) sapply(s$sigma1, function(s_) max(eigen(s_)$values)))

median1 = lapply(R1, function(s) sapply(s$sigma1, function(s_) median(eigen(s_)$values)))
median2 = lapply(R2, function(s) sapply(s$sigma1, function(s_) median(eigen(s_)$values)))
median3_1 = lapply(R3_1[1:5], function(s) sapply(s$sigma1, function(s_) median(eigen(s_)$values)))


sapply(max1, length)
sapply(max2, length)
sapply(max3_1, length)

