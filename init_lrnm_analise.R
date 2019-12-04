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
  d = ncol(X)-1
  
  N = nrow(X)
  
  p = prop.table(colMeans(X))
  n_ = rowSums(X)
  V0 = (mean(n_) * ( diag(p) - p %*% t(p) ))
  EIG0 = eigen(t(B) %*% V0 %*% B)
  
  multinomial_test = evaluate_multinomial(X)
  cat('contract:', multinomial_test, '\n')
  CONTRACT = multinomial_test[1] > 0.01 & multinomial_test[2] > 1e-14
  
  mu_ = coordinates(colSums(X), B)
  cov_ = diag(d)
  Snew = cov_
  
  iter = 0
  summ = list()
  while(iter < 100){
    iter  = iter + 1
    
    # Avoid contraction during initialisation
    eig = eigen(cov_)
    if(CONTRACT){
      
      eig_values = rep(median(eig$values), d)
      S_ = EIG0$vectors %*% diag(eig_values * prop.table(EIG0$values)) %*% t(EIG0$vectors)
    }else{
      eig_values = pmax(eig$values, 1e-08)
      S_ = eig$vectors %*% diag(eig_values) %*% t(eig$vectors)
    } 
    
    # Posterior laplace approximation
    A = c_posterior_approximation(X, mu_, S_, B)
    
    MU = A[,d+1,]
    SIGMA = A[,1:d,]
    MU_rnd = t(sapply(1:nrow(X), function(i) mvtnorm::rmvnorm(1, MU[,i], SIGMA[,,i])))
    
    if(CONTRACT){
      Mnew = rowMeans(MU)
      Snew = (MU - mu_) %*% t(MU - mu_) / N
    }else{
      Mnew = rowMeans(MU)
      Snew = ((iter-1) * Snew + cov(MU_rnd)) / iter
    }
    
    summ$mu[[iter]] = Mnew
    summ$sigma[[iter]] = Snew
    
    mu_new = summ$mu[[iter]]
    sigma_new = summ$sigma[[iter]]
    
    if(max(abs(mu_new - mu_)) < 0.001 & iter > 1){ 
      break
    }
    
    mu_ = mu_new
    cov_ = sigma_new
  }
  summ$iter = iter
  summ$H = t(MU)
  summ
}
