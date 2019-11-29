lrnm_dm.init = function(X){
  fit.dm_init = dirmult(X, trace = FALSE)
  coordinates(t(t(X) + fit.dm_init$gamma))
}
lrnm_laplace.init = function(X, B = ilr_basis(ncol(X))){
  Binv = t(MASS::ginv(B))
  d = ncol(X)-1
  
  N = nrow(X)
  
  chisq_ = function(X){
    X_ = X[sample(seq_len(nrow(X)), replace = T),]
    p = prop.table(colSums(X_))
    n = rowSums(X_)
    E = n %*% t(p)
    mean( 1 - pchisq(rowSums( (X_ - E)^2 / E ), d) )
  }
  q95 = quantile(replicate(100, chisq_(X)), 0.95)
  cat('contract:', q95, '\n')
  CONTRACT = q95 >= 0.5
  
  mu_ = coordinates(colSums(X), B)
  cov_ = diag(d)
  Snew = cov_
  
  iter = 0
  summ = list()
  while(iter < 100){
    iter  = iter + 1
    
    # Avoid contraction during initialisation
    eig = eigen(cov_)
    # print(head(eig$values, n = 5))
    if(max(eig$values) < 1e-3) break
    eig_values = pmax(quantile(eig$values, 0.1), 1e-05) / min(eig$values) * eig$values
    S_ = eig$vectors %*% diag(eig_values)  %*% t(eig$vectors)
    
    # Posterior laplace approximation
    A = lapply(split(X,seq(NROW(X))), c_posterior_approximation_vec, mu_, solve(S_), Binv)
    
    MU = sapply(A, function(a) a[,d+1], simplify = 'array')
    SIGMA = sapply(A, function(a) a[,1:d], simplify = 'array')
    MU_rnd = t(sapply(1:nrow(X), function(i) mvtnorm::rmvnorm(1, MU[,i], SIGMA[,,i])))
    
    # M2 = sapply(1:N, function(i) SIGMA[,,i] + MU[,i] %*% t(MU[,i]), simplify = 'array')
    # 
    # Mnew = apply(MU, 1, mean)
    # COV = apply(M2, 1:2, mean) - Mnew %*% t(Mnew)
    
    if(CONTRACT){
      Snew = cov(MU_rnd) / d
    }else{
      # I = I + 1
      Snew = ((iter-1) * Snew + cov(MU_rnd)) / iter
    }
  
    summ$mu[[iter]] = apply(MU, 1, mean)
    summ$sigma[[iter]] = Snew
    
    mu_new = summ$mu[[iter]]
    sigma_new = summ$sigma[[iter]]
    
    # if( CONTRACT & iter > 4 ){
    #   CONTRACT = FALSE
    #   I = 0
    #   mu_new = coordinates(colSums(X), B)
    #   sigma_new = diag(d)
    # }
    if(max(abs(mu_new - mu_)) < 0.001){ 
      break
    }

    mu_ = mu_new
    cov_ = sigma_new
  }
  summ$iter = iter
  summ$H = t(MU)
  summ
}
