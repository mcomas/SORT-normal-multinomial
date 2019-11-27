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
  div = 1
  summ = list('mean' = list(), 'cov' = list())
  while(iter < 100){
    iter  = iter + 1
    # Avoid contraction during initialisation
    eig = eigen(cov_)
    # print(eig$values)
    if(max(eig$values) < 1e-5) break
    eig_values = pmax(quantile(eig$values, 0.1), 1e-05) / min(eig$values) * eig$values # quantile(eig$values, 0.5)/d + eig$values #pmax(eig$values, quantile(eig$values, 0.75))
    # print(eig_values)
    S_ = eig$vectors %*% diag(eig_values)  %*% t(eig$vectors)
    
    # Posterior laplace approximation
    A = lapply(split(X,seq(NROW(X))), c_posterior_approximation_vec, mu_, solve(S_), Binv)
    MU = sapply(A, function(a) a[,d+1], simplify = 'array')
    SIGMA = sapply(A, function(a) a[,1:d], simplify = 'array')
    MU_rnd = t(sapply(1:nrow(X), function(i) mvtnorm::rmvnorm(1, MU[,i], SIGMA[,,i])))
    
    # MU = t(apply(X, 1, l_lrnm_join_maximum, mu_, solve(S), Binv))
  
    summ$mu[[iter]] = apply(MU, 1, mean)
    summ$sigma1[[iter]] = cov(MU_rnd) / (d^div)
    summ$sigma2[[iter]] = apply(SIGMA, 1:2, mean)
    
    mu_new = summ$mu[[iter]]
    sigma_new = summ$sigma1[[iter]]
    
    # sigma_new = apply(SIGMA, 1:2, mean)
    # print(mu_new)
    if(max(abs(mu_new - mu_)) < 0.001){ # avoid degenerate cases
      mu_ = mu_new
      break
    }
    
    if(iter %% 10 == 0){
      # mu_ = coordinates(colSums(X), B)
      # cov_ = diag(d)
      div = div + 1
    }
    mu_ = mu_new
    cov_ = sigma_new
    
    
  }
  summ$iter = iter
  summ$H = t(MU)
  summ
}
