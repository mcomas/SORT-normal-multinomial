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
  if(DIM <= 3){
    R_0 = exact_hermite(X, MU, SIGMA, B)
  }else{
    R_0 = exact_mcmc(X, MU, SIGMA, B)
  }
  
  ## MC with importance sampling centered at Laplace approximation
  Norm = coda.count::lrnm_posterior_approx(X, MU, SIGMA, B)
  invSIGMA = solve(SIGMA)
  Z_pseudo = matrix(rnorm(DIM*N), nrow = DIM)
  R_MC = c_moments_lrnm_montecarlo(X,
                                   Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                   MU, invSIGMA, 
                                   B, Z = Z_pseudo, rep(0, DIM))
  
  ## MC with importance sampling centered at Laplace approximation and antithetic variates
  Z_av = matrix(rnorm(DIM*N/2), nrow = DIM)
  Z_av = cbind(Z_av,-Z_av)
  R_MC.AV = c_moments_lrnm_montecarlo(X,
                                      Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                      MU, invSIGMA, 
                                      B, Z = Z_av, rep(0, DIM))
  
  ## MC with importance sampling centered at Laplace approximation and quasi random generation
  Z_quasi = t(matrix(halton(n = N, normal = TRUE, dim = DIM), ncol=DIM))
  R_QMC = c_moments_lrnm_montecarlo(X,
                                    Norm[[1]]$mu, as.matrix(Norm[[1]]$sigma), 
                                    MU, invSIGMA, 
                                    B, Z = Z_quasi, rep(0, DIM))
  ## MCMC
  h = c_rlrnm_posterior(N, X, MU, SIGMA, B, r = 10)
  R_MCMC = cbind(crossprod(h,h)/N, colMeans(h))
  
  res = list(R_0, R_MC,R_MC.AV,R_QMC,R_MCMC)
  names(res) = c('Exact', 'MC', 'MC-AV', 'QMC', 'MCMC')
  res
  
}

do_simulations = function(NSIM, DIM, SIZE, NORM, VAR, AGREEMENT){
  sims = replicate(100, simulation(DIM, SIZE, NORM, VAR, AGREEMENT), simplify = FALSE)
  
  results = lapply(sims, function(sim){
    res = sapply(sim[c('MC', 'MC-AV', 'QMC', 'MCMC')], function(m){
      err = abs(m-sim$Exact)
      c(max(err[,  ncol(err)]), max(err[, 1:nrow(err)]))
    })
    d_ = as.data.frame(res, row.names = FALSE)
    d_$moment = c('m1', 'm2')
    d_
  })
  do.call(`rbind`, results)
}

RESULTS = do_simulations(100, DIM, SIZE, NORM, VAR, AGREEMENT)
