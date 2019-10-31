exact_hermite = function(X, MU, SIGMA, B, order = 50){
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

if(DIM <= 3){
  exact_calculation = exact_hermite
}else{
  exact_calculation = exact_mcmc
}

if(DIM <= 6){
  quasi_random = halton
}else{
  quasi_random = sobol
}

simulation = function(DIM, SIZE, NORM, VAR, AGREEMENT, B = ilr_basis(DIM+1), N = 1000){
  MU = rnorm(DIM)
  MU = MU/sqrt(sum(MU^2)) * NORM
  CORR = cov2cor(as.matrix(rWishart(1, DIM^2, diag(DIM))[,,1]))
  SIGMA = CORR * VAR
  if(AGREEMENT){
    X = rmultinomial(1, SIZE, composition(MU))
  }else{
    X = rmultinomial(1, SIZE, composition(-MU))
  }
  
  ## EXACT
  R_0 = exact_calculation(X, MU, SIGMA, B)
  
  ## MC with importance sampling centered at Laplace approximation
  T_MC = Sys.time()
  Norm = coda.count::lrnm_posterior_approx(X, MU, SIGMA, B)
  invSIGMA = solve(SIGMA)
  Z_pseudo = matrix(rnorm(DIM*N), nrow = DIM)
  R_MC = c_moments_lrnm_montecarlo(X,
                                   Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                   MU, invSIGMA, 
                                   B, Z = Z_pseudo, rep(0, DIM))
  T_MC = as.numeric(Sys.time() - T_MC)
  
  ## MC with importance sampling centered at Laplace approximation and antithetic variates
  T_MC.AV = Sys.time()
  Norm = coda.count::lrnm_posterior_approx(X, MU, SIGMA, B)
  invSIGMA = solve(SIGMA)
  Z_av = matrix(rnorm(DIM*N/2), nrow = DIM)
  Z_av = cbind(Z_av,-Z_av)
  R_MC.AV = c_moments_lrnm_montecarlo(X,
                                      Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                      MU, invSIGMA, 
                                      B, Z = Z_av, rep(0, DIM))
  T_MC.AV = as.numeric(Sys.time() - T_MC.AV)
  
  ## MC with importance sampling centered at Laplace approximation and quasi random generation
  T_QMC = Sys.time()
  Norm = coda.count::lrnm_posterior_approx(X, MU, SIGMA, B)
  invSIGMA = solve(SIGMA)
  Z_quasi = t(matrix(quasi_random(n = N, normal = TRUE, dim = DIM), ncol=DIM))
  R_QMC = c_moments_lrnm_montecarlo(X,
                                    Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                    MU, invSIGMA, 
                                    B, Z = Z_quasi, rep(0, DIM))
  T_QMC = as.numeric(Sys.time() - T_QMC)
  
  ## MCMC
  T_MCMC = Sys.time()
  h = c_rlrnm_posterior(N, X, MU, SIGMA, B, r = 10)
  R_MCMC = cbind(crossprod(h,h)/N, colMeans(h))
  T_MCMC = as.numeric(Sys.time() - T_MCMC)
  
  res = list(R_0, R_MC,R_MC.AV,R_QMC,R_MCMC, c('MC' = T_MC, 'MC-AV' = T_MC.AV, 'QMC' = T_QMC, 'MCMC' = T_MCMC))
  names(res) = c('Exact', 'MC', 'MC-AV', 'QMC', 'MCMC', 'Times')
  res
  
}

do_simulations = function(NSIM, DIM, SIZE, NORM, VAR, AGREEMENT){
  sims = replicate(NSIM, simulation(DIM, SIZE, NORM, VAR, AGREEMENT), simplify = FALSE)
  
  m1 = sapply(sims, function(sim){
    sapply(sim[c('MC', 'MC-AV', 'QMC', 'MCMC')], function(m){
      err = abs(m-sim$Exact)
      max(err[, ncol(err)])
    })
  })
  m2 = sapply(sims, function(sim){
    sapply(sim[c('MC', 'MC-AV', 'QMC', 'MCMC')], function(m){
      err = abs(m-sim$Exact)
      max(err[, 1:nrow(err)])
    })
  })
  times = sapply(sims, function(sim) sim$Times)
  
  list('m1' = as.data.frame(t(m1)), 
       'm2' = as.data.frame(t(m2)), 
       'times' = as.data.frame(t(times)))
}

RESULTS = do_simulations(50, DIM, SIZE, NORM, VAR, AGREEMENT)
