library(rstan)
rstan_options(auto_write = TRUE)

m = stan_model('uni-lnm.stan')

expected_hamiltonian = function(x, mu_ilr, sigma_ilr, nsim){
  K = as.integer(length(x))
  x = matrix(x, ncol = 1)
  mu = matrix(mu_ilr, ncol = 1)
  sigma = matrix(sigma_ilr)
  B = ilr_basis(K)
  MCONSTANT = 0
  data = list(K = K, x = x, mu = mu, sigma = sigma, B = B, MCONSTANT = MCONSTANT)
  res = sampling(m, iter = nsim + 1000, chain = 1, 
                 warmup = 1000, check_data = FALSE, verbose = FALSE, show_messages = FALSE,
                 data = data, seed = sample.int(.Machine$integer.max, 1))
  rh = matrix(rstan::extract(res)$h, ncol = K-1)
  list(
    h = rh,
    m1 = colMeans(rh),
    m2 = (t(rh) %*% rh) / NROW(rh) )
}

save(m, expected_hamiltonian, file='uni-lnm.RData')

if(FALSE){
  library(rstan)
  load('uni-lnm.RData')
  K = 2
  x = c(1,0)
  mu = rep(0,K-1)
  sigma = diag(1,K-1)
  NSIM = 1000
  expected_hamiltonian(x = x, mu_ilr = mu, sigma_ilr = sigma, nsim = NSIM)
}
