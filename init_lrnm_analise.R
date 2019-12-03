lrnm_dm.init = function(X){
  fit.dm_init = dirmult(X, trace = FALSE)
  coordinates(t(t(X) + fit.dm_init$gamma))
}
evaluate_multinomial = function(X){
  d = ncol(X)-1
  n_ = rowSums(X)
  
  p = prop.table(colSums(X))
  n = rowSums(X)
  E = n %*% t(p)
  chisq.m1 = rowSums( (X - E)^2 / E )
  test1 = median(1-pchisq(chisq.m1, d))
  
  S = cov(X)[-1,-1]
  V0 = (mean(n_) * ( diag(p) - p %*% t(p) ))[-1,-1]
  N = nrow(X)
  LH0 = - N / 2 * log(det(V0)) - N / 2 * sum(diag(solve(V0) %*% S))
  LH1 = - N / 2 * log(det(S)) - N * d / 2
  chisq.m2 = 2 * (LH1 - LH0)
  test2 = 1-pchisq(chisq.m2, d*(d+1)/2)
  c(test1, test2)
}
lrnm_laplace.init = function(X, B = ilr_basis(ncol(X))){
  Binv = t(MASS::ginv(B))
  d = ncol(X)-1
  
  N = nrow(X)
  
  multinomial_test = evaluate_multinomial(X)
  cat('contract:', multinomial_test, '\n')
  CONTRACT = all(multinomial_test > 0.01/nrow(X))
  
  mu_ = coordinates(colSums(X), B)
  cov_ = diag(d)
  Snew = cov_
  
  iter = 0
  summ = list()
  while(iter < 100){
    iter  = iter + 1
    
    # Avoid contraction during initialisation
    eig = eigen(cov_)
    if(CONTRACT) eig_values = rep(mean(eig$values), d)
    else eig_values = pmax(eig$values, 1e-08)
    
    S_ = eig$vectors %*% diag(eig_values) %*% t(eig$vectors)
    
    # Posterior laplace approximation
    A = lapply(split(X,seq(NROW(X))), c_posterior_approximation_vec, mu_, solve(S_), Binv)
    
    MU = sapply(A, function(a) a[,d+1], simplify = 'array')
    SIGMA = sapply(A, function(a) a[,1:d], simplify = 'array')
    MU_rnd = t(sapply(1:nrow(X), function(i) mvtnorm::rmvnorm(1, MU[,i], SIGMA[,,i])))
    
    if(CONTRACT){
      Snew = cov(t(MU))
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
