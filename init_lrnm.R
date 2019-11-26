lrnm_dm.init = function(X){
  fit.dm_init = dirmult(X, trace = FALSE)
  coordinates(t(t(X) + fit.dm_init$gamma))
}
lrnm_laplace.init = function(X, B = ilr_basis(ncol(X))){
  mu_ = coordinates(colSums(X), B)

  Binv = t(MASS::ginv(B))
  d = ncol(X)-1
  cov_ = diag(d)
  iter = 0
  
  while(iter < 1000){
    iter  = iter + 1
    
    # Avoid contraction during initialisation
    eig = eigen(cov_)
    # print(eig$values)
    eig_values = pmax(quantile(eig$values, 0.1), 1e-05) / min(eig$values) * eig$values # quantile(eig$values, 0.5)/d + eig$values #pmax(eig$values, quantile(eig$values, 0.75))
    # print(eig_values)
    S_ = eig$vectors %*% diag(eig_values)  %*% t(eig$vectors)
    
    # Posterior laplace approximation
    A = lapply(split(X,seq(NROW(X))), c_posterior_approximation_vec, mu_, solve(S_), Binv)
    MU = sapply(A, function(a) a[,d+1], simplify = 'array')
    SIGMA = sapply(A, function(a) a[,1:d], simplify = 'array')
    MU_rnd = t(sapply(1:nrow(X), function(i) mvtnorm::rmvnorm(1, MU[,i], SIGMA[,,i])))
    
    # MU = t(apply(X, 1, l_lrnm_join_maximum, mu_, solve(S), Binv))
    mu_new = apply(MU, 1, mean)
    sigma_new = cov(MU_rnd) / d
    
    # sigma_new = apply(SIGMA, 1:2, mean)
    # print(mu_new)
    if(max(abs(mu_new - mu_)) < 0.001){ # avoid degenerate cases
      mu_ = mu_new
      break
    }
    mu_ = mu_new
    cov_ = sigma_new
  }
  t(MU)
}
