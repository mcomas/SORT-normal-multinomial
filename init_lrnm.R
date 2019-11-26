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
    eig = eigen(cov_)
    S = eig$vectors %*% diag(pmax(eig$values, mean(eig$values)))  %*% t(eig$vectors)
    MU = t(apply(X, 1, l_lrnm_join_maximum, mu_, solve(S), Binv))
    mu_new = colMeans(MU)
    if(max(abs(mu_new - mu_)) < 0.001){ # avoid degenerate cases
      mu_ = mu_new
      break
    }
    mu_ = mu_new
    cov_ = cov(MU)
  }
  MU
}