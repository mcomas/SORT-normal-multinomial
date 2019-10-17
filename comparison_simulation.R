library(coda.base)
library(coda.count)
library(randtoolbox)

DIM = length(MU)

exact = function(X, MU, SIGMA, B, order = 1000){
  Norm = coda.count::lrnm_posterior_approx(X, MU, SIGMA, B)
  
  M = c_moments_lrnm_hermite(X, 
                             Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                             MU, SIGMA, 
                             B, order = order, rep(0, DIM))
  M
}
simulation = function(N, X, MU, SIGMA, B){
  Norm = coda.count::lrnm_posterior_approx(X, MU, SIGMA, B)
  invSIGMA = solve(SIGMA)
  Z1 = matrix(rnorm(DIM*N), nrow = DIM)
  M2 = c_moments_lrnm_montecarlo(X,
                                 Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                 MU, invSIGMA, 
                                 B, Z = Z1, rep(0, DIM))
  
  Z2 = matrix(rnorm(DIM*N/2), nrow = DIM)
  M3 = c_moments_lrnm_montecarlo(X,
                                 Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                 MU, invSIGMA, 
                                 B, Z = cbind(Z2,-Z2), rep(0, DIM))

  Z3 = tryCatch(matrix(halton(n = N, normal = TRUE, init = FALSE), DIM),
                error = function(e) matrix(halton(n = N, normal = TRUE, init = TRUE), DIM))
  M4 = c_moments_lrnm_montecarlo(X,
                                 Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                 MU, invSIGMA, 
                                 B, Z = Z3, rep(0, DIM))
  
  h = c_rlrnm_posterior(N, X, MU, SIGMA, B, r = 10)
  M5 = cbind(crossprod(h,h)/N, colMeans(h))
  
  res = list(M2,M3,M4,M5)
  names(res) = c('MC', 'MC-AV', 'QMC', 'MCMC')
  res
}



##############

set.seed(SEED)
M0 = exact(X, MU, SIGMA, B)
R = replicate(1000, simulation(N = 1000, X, MU, SIGMA, B), simplify = FALSE)

RESULTS = list()
M_1 = sapply(R, function(r) sapply(r, function(r_) r_[,-(1:DIM)] - M0[,-(1:DIM)], simplify = "array"), simplify = "array")
M_2 = sapply(R, function(r) sapply(r, function(r_) r_[,1:DIM] - M0[,1:DIM], simplify = "array"), simplify = "array")

if(DIM == 1){
  M_1_mean = apply(M_1, 1, mean)
  M_1_sd = apply(M_1, 1, sd)
  
  M_2_mean = apply(M_2, 1, mean)
  M_2_sd = apply(M_2, 1, sd)
}else{
  M_1_mean = apply(M_1, 1:2, mean)
  M_1_sd = apply(M_1, 1:2, sd)
  
  M_2_mean = apply(M_2, 1:3, mean)
  M_2_sd = apply(M_2, 1:3, sd)
}

RESULTS[['M1']] = list('mean' = M_1_mean, 'sd' = M_1_sd)
RESULTS[['M2']] = list('mean' = M_2_mean, 'sd' = M_2_sd)

if(FALSE){
  boxplot(moment_1)
  abline(h = M0[,2])
  
  boxplot(moment_2)
  abline(h = M0[,1])
}



# d0 = dlrnm(X, MU, SIGMA, hermite.order = 2000)
# dd = function(h) dnorm(h, MU, sqrt(SIGMA)) * dbinom(X[1], sum(X), prob = composition(cbind(h))[,1]) / d0
# dd_1 = function(h) h * dd(h)
# dd_2 = function(h) h * h * dd(h)
# 
# integrate(dd, lower = -100, upper = 100)
# integrate(dd_1, lower = -100, upper = 100)
# integrate(dd_2, lower = -100, upper = 100)
