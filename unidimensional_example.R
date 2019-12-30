library(coda.count)
library(coda.base)
library(randtoolbox)

x = c(1,0)
MU = 0
SIGMA = matrix(1)

B = ilr_basis(2)

MOMENTS = c_moments_lrnm_hermite(matrix(x,nrow=1), MU, SIGMA, MU, SIGMA, B, order = 100, 0)
MU_EXP = MOMENTS[,2]
SIGMA_EXP = MOMENTS[,1]

NSIM = 1000

fit = list()
SEED = 1

set.seed(SEED)
fit_mc = function(){
  Z_pseudo = matrix(rnorm(NSIM), nrow = 1)
  c_moments_lrnm_montecarlo(matrix(x,nrow=1), MU_EXP, matrix(1), MU, SIGMA, B, Z_pseudo, 0)
}
fit[['MC']] = replicate(500, fit_mc(), simplify = TRUE)

set.seed(SEED)
fit_mc.av = function(){
  Z_av = matrix(rnorm(NSIM/2), nrow = 1)
  Z_av = cbind(Z_av, -Z_av)
  c_moments_lrnm_montecarlo(matrix(x,nrow=1), MU_EXP, matrix(1), MU, SIGMA, B, Z_av, 0)
}
fit[['MC-AV']] = replicate(500, fit_mc.av(), simplify = TRUE)

set.seed(SEED)
init0 = halton(n = 1, normal = TRUE, dim = 1)
fit_qmc = function(){
  Z_quasi = matrix(halton(n = NSIM, normal = TRUE, dim = 1, init = FALSE), nrow = 1)
  c_moments_lrnm_montecarlo(matrix(x,nrow=1), MU_EXP, matrix(1), MU, SIGMA, B, Z_quasi, 0)
}
fit[['QMC']] = replicate(500, fit_qmc(), simplify = TRUE)

set.seed(SEED)
fit_mcmc = function(){
  h = c_rlrnm_posterior(NSIM, matrix(x,nrow=1), MU, SIGMA, B, r = 10)
  cbind(crossprod(h,h)/NSIM, colMeans(h))
}
fit[['MCMC']] = replicate(500, fit_mcmc(), simplify = TRUE)

set.seed(SEED)
library(rstan)
rstan_options(auto_write = TRUE)
load('uni-lnm.RData')
fit_hmc = function(){
  hs = suppressMessages(expected_hamiltonian(x = x, mu_ilr = MU, sigma_ilr = SIGMA, nsim = NSIM))
  cbind(hs$m2, hs$m1)
}
fit[['HMC']] = replicate(500, fit_hmc(), simplify = TRUE)

tab.m = t(sapply(fit, function(fit_){
  sprintf("%0.7f (%0.5f)",rowMeans(fit_), apply(fit_, 1, sd))
}))[,2:1]
colnames(tab.m) = c('First moment', 'Second moment')


library(microbenchmark)
m.res = microbenchmark(fit_mc(), fit_mc.av(), fit_qmc(), fit_mcmc(), fit_hmc())
tm = tapply(m.res$time, m.res$expr, mean)
res.m = rbind(cbind('First moment' = sprintf("%0.7f", MOMENTS[,2]), 
                    "Second moment" = sprintf("%0.7f", MOMENTS[,1]), "Time" = ""),
              cbind(tab.m, 'Time' = sprintf("x %0.2f", tm/tm[1])))

knitr::kable(res.m, format = 'latex')

IND = 300:NSIM
set.seed(1)
Z_pseudo = matrix(rnorm(NSIM), nrow = 1)
h.mc = sapply(IND, function(i){
  c_moments_lrnm_montecarlo(matrix(x,nrow=1), MU_EXP, matrix(1), MU, SIGMA, B, Z_pseudo[,1:i,drop=FALSE], 0)
})

set.seed(1)
Z_av = matrix(0, nrow = 1, ncol = NSIM)
Z_av[,seq(1, NSIM, 2)] = matrix(rnorm(NSIM/2), nrow = 1)
Z_av[,seq(2, NSIM, 2)] = -Z_av[,seq(1, NSIM, 2)]
h.mc.av = sapply(IND, function(i){
  c_moments_lrnm_montecarlo(matrix(x,nrow=1), MU_EXP, matrix(1), MU, SIGMA, B, Z_av[,1:i,drop=FALSE], 0)
})

Z_quasi = matrix(halton(n = NSIM, normal = TRUE, dim = 1), nrow = 1)
h.qmc = sapply(IND, function(i){
  c_moments_lrnm_montecarlo(matrix(x,nrow=1), MU_EXP, matrix(1), MU, SIGMA, B, Z_quasi[,1:i,drop=FALSE], 0)
})

set.seed(1)
h = c_rlrnm_posterior(NSIM, matrix(x,nrow=1), MU, SIGMA, B, r = 10)
h.mcmc = sapply(IND, function(i){
  cbind(crossprod(h[1:i,,drop=F],h[1:i,,drop=F])/i, colMeans(h[1:i,,drop=F]))
})

h = expected_hamiltonian(x = x, mu_ilr = MU, sigma_ilr = SIGMA, nsim = NSIM)$h
h.hmc = sapply(IND, function(i){
  cbind(crossprod(h[1:i,,drop=F],h[1:i,,drop=F])/i, colMeans(h[1:i,,drop=F]))
})

pdf(file = 'unidimensional_example.pdf', width = 9.8, height = 5.3)
par(mfrow=c(1,2))
plot(IND, h.mc[2,], type='l', ylim = c(.45,0.6), 
     ylab = 'Estimation', xlab = 'Iteration', main = 'First moment')
abline(h = MOMENTS[,2], lty=2)
points(IND, h.mc.av[2,], col = 2, type='l')
points(IND, h.qmc[2,], col = 3, type='l')
points(IND, h.mcmc[2,], col = 4, type='l')
points(IND, h.hmc[2,], col = 'purple', type='l')
legend('bottomright', 
       c('MC', 'MC (Antithetic variates)', 'QMC', 'MCMC (Metropolis)', 'MCMC (Hamiltonian)'),
       col=c(1:4,'purple'), bty = 'n', lty=1, lwd = 2, cex = 0.8)

plot(IND, h.mc[1,], type='l', ylim = c(.75,1.1), 
     ylab = 'Estimation', xlab = 'Iteration', main = 'Second moment')
abline(h = MOMENTS[,1], lty=2)
points(IND, h.mc.av[1,], col = 2, type='l')
points(IND, h.qmc[1,], col = 3, type='l')
points(IND, h.mcmc[1,], col = 4, type='l')
points(IND, h.hmc[1,], col = 'purple', type='l')
legend('bottomright', 
       c('MC', 'MC (Antithetic variates)', 'QMC', 'MCMC (Metropolis)', 'MCMC (Hamiltonian)'),
       col=c(1:4,'purple'), bty = 'n', lty=1, lwd = 2, cex = 0.8)
dev.off()
