library(coda.base)
library(coda.count)
library(randtoolbox)


# QUASI_INIT = FALSE

exact_hermite = function(X, MU, SIGMA, B, order){
  Norm = coda.count::lrnm_posterior_approx(X, MU, SIGMA, B)
  
  R_exact = c_moments_lrnm_hermite(X, 
                                   Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                   MU, SIGMA, 
                                   B, order = order, rep(0, DIM))
  R_exact
}
exact_mcmc = function(X, MU, SIGMA, B, SIM = 100000){
  h = c_rlrnm_posterior(SIM, X, MU, SIGMA, B, r = 10)
  R_approx = cbind(crossprod(h,h)/SIM, colMeans(h))
  R_approx
}
simulation = function(N, X, MU, SIGMA, B){
  Norm = coda.count::lrnm_posterior_approx(X, MU, SIGMA, B)
  invSIGMA = solve(SIGMA)
  Z_pseudo = matrix(rnorm(DIM*N), nrow = DIM)
  R_MC = c_moments_lrnm_montecarlo(X,
                                   Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                   MU, invSIGMA, 
                                   B, Z = Z_pseudo, rep(0, DIM))
  
  Z_av = matrix(rnorm(DIM*N/2), nrow = DIM)
  Z_av = cbind(Z_av,-Z_av)
  R_MC.AV = c_moments_lrnm_montecarlo(X,
                                      Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                      MU, invSIGMA, 
                                      B, Z = Z_av, rep(0, DIM))
  
  # if(!QUASI_INIT){
  #   Z_quasi = matrix(halton(n = N, normal = TRUE, init = TRUE), DIM)
  #   QUASI_INIT <<- TRUE
  # }else{
  #   Z_quasi = matrix(halton(n = N, normal = TRUE, init = FALSE), DIM)
  # }
  
  Z_quasi = matrix(halton(n = N, normal = TRUE), DIM)
  R_QMC = c_moments_lrnm_montecarlo(X,
                                    Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                    MU, invSIGMA, 
                                    B, Z = Z_quasi, rep(0, DIM))
  
  h = c_rlrnm_posterior(N, X, MU, SIGMA, B, r = 10)
  R_MCMC = cbind(crossprod(h,h)/N, colMeans(h))
  
  res = list(R_MC,R_MC.AV,R_QMC,R_MCMC)
  names(res) = c('MC', 'MC-AV', 'QMC', 'MCMC')
  res
}



##############

set.seed(SEED)
if(DIM <= 3){
  M0 = exact_hermite(X, MU, SIGMA, B, ORDER)
}else{
  M0 = exact_mcmc(X, MU, SIGMA, B)
}
R = replicate(1000, simulation(N = 1000, X, MU, SIGMA, B), simplify = FALSE)


extract = function(what) sapply(R, function(r, what_) sapply(r, what_, simplify = "array"), what, simplify = "array")
M_1 = extract(function(r_) r_[,-(1:DIM)] - M0[,-(1:DIM)])
M_2 = extract(function(r_) r_[,1:DIM] - M0[,1:DIM])

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

RESULTS = list()
if(DIM == 1){
  RESULTS[['M1']] = list('mean' = abs(M_1_mean), 'sd' = M_1_sd)
  RESULTS[['M2']] = list('mean' = abs(M_2_mean), 'sd' = M_2_sd)
}else{
  RESULTS[['M1']] = list('mean' = apply(abs(M_1_mean), 2, max), 
                         'sd' = apply(abs(M_1_sd), 2, max))
  RESULTS[['M2']] = list('mean' = apply(abs(M_2_mean), 3, max), 
                         'sd' = apply(abs(M_2_sd), 3, max))
}


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
