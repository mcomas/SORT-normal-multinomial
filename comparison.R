library(coda.base)
library(coda.count)
library(randtoolbox)

exact = function(X, MU, SIGMA, B){
  Norm = coda.count::lrnm_posterior_approx(X, MU, SIGMA, B)
  
  M = c_moments_lrnm_hermite(X, 
                              Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                              MU, SIGMA, 
                              B, order = 10000)
  M
}
simulation = function(N, X, MU, SIGMA, B){
  Norm = coda.count::lrnm_posterior_approx(X, MU, SIGMA, B)
  invSIGMA = solve(SIGMA)
  Z1 = matrix(rnorm(1*N), nrow = 1)
  M2 = c_moments_lrnm_montecarlo(X,
                                 Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                 MU, invSIGMA, 
                                 B, Z = Z1)
  
  Z2 = matrix(rnorm(1*N/2), nrow = 1)
  M3 = c_moments_lrnm_montecarlo(X,
                                 Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                 MU, invSIGMA, 
                                 B, Z = cbind(Z2,-Z2))

  Z3 = matrix(halton(n = N, normal = TRUE, init = FALSE), nrow = 1)
  M4 = c_moments_lrnm_montecarlo(X,
                                 Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                 MU, invSIGMA, 
                                 B, Z = Z3)
  
  h = c_rlrnm_posterior(N, X, MU, SIGMA, B, r = 10)
  M5 = cbind(mean((h-MU)^2), mean(h))
  
  res = rbind(M2,M3,M4,M5)
  rownames(res) = c('MC', 'MC-AV', 'QMC', 'MCMC')
  res
}

########

B = ilr_basis(2)
X = matrix(c(100, 0), nrow = 1)
MU = 2
SIGMA = as.matrix(10)

##############

set.seed(1)
M0 = exact(X, MU, SIGMA, B)
R = replicate(1000, simulation(N = 1000, X, MU, SIGMA, B), simplify = FALSE)

moment_1 = t(sapply(R, function(r) r[,2]))
boxplot(moment_1)
abline(h = M0[,2])
t_m1 = apply(moment_1, 2, function(x) sprintf("%.7f (%0.6f)", mean(x - M0[,2]), sd(x - M0[,2])))

moment_2 = t(sapply(R, function(r) r[,1]))
boxplot(moment_2)
abline(h = M0[,1])
t_m2 = apply(moment_2, 2, function(x) sprintf("%.7f (%0.6f)", mean(x - M0[,1]), sd(x - M0[,1])))

data.frame(
  'M1' = c(sprintf("%.7f", M0[,2]), t_m1),
  'M2' = c(sprintf("%.7f", M0[,1]), t_m2))

d0 = dlrnm(X, MU, SIGMA)
dd = function(h) dnorm(h, MU, sqrt(SIGMA)) * dbinom(X[1], sum(X), prob = composition(cbind(h))[,1]) / d0
dd_1 = function(h) h * dd(h)
dd_2 = function(h) h * h * dd(h)

integrate(dd, lower = -100, upper = 100)
integrate(dd_1, lower = -100, upper = 100)
integrate(dd_2, lower = -100, upper = 100)
