evaluate = function(H_pi, H_p){
  mean(sqrt(rowSums(H_p - H_pi)^2 ))
}
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
simulation = function(N, n, S){
  generator = params[[S]]
  p = generator(N)
  H = coordinates(t(sapply(p, function(p_) c('AA' = p_^2, 'AB' = 2*p_*(1-p_),  'BB' = (1-p_)^2))))
  d = ncol(H)
  
  XZ = HWData(N, n, p = p)
  isZERO = XZ == 0
  row.zeros = table(apply(isZERO, 1, sum))
  col.zeros = apply(isZERO, 2, sum)
  
  ## Dirichlet-multinomial fitting
  t.dm = proc.time()
  fit.dm = dirmult(XZ, trace = FALSE)
  t.dm = proc.time() - t.dm
  
  ## LRNM initialisation with Dirichlet-multinomial
  t.lrnm_dm = proc.time()
  quasi_random = sobol
  H.lrnm_dm = lrnm_dm.init(XZ)
  Z_quasi = matrix(quasi_random(n = 10000, normal = TRUE, dim = d), ncol=d)
  fit.lrnm_dm = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = H.lrnm_dm)
  t.lrnm_dm = proc.time() - t.lrnm_dm
  
  ## LRNM initialisation with Laplace
  t.lrnm_laplace = proc.time()
  quasi_random = sobol
  H.lrnm_laplace = lrnm_laplace.init(XZ)
  Z_quasi = matrix(quasi_random(n = 10000, normal = TRUE, dim = d), ncol=d)
  fit.lrnm_laplace = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = H.lrnm_laplace)
  t.lrnm_laplace = proc.time() - t.lrnm_laplace

  ### Evaluation
  H.dm = coordinates(t(t(XZ) + fit.dm$gamma))
  e.dm = evaluate(H, H.dm)
  
  H.lrnm_dm = coordinates(fit.lrnm_dm$P)
  e.lrnm_dm = evaluate(H, H.lrnm_dm)
  
  H.lrnm_laplace = coordinates(fit.lrnm_laplace$P)
  e.lrnm_laplace = evaluate(H, H.lrnm_laplace)

  list('dm' = e.dm,
       'lrnm-dm' = e.lrnm_dm,
       'lrnm-laplace' = e.lrnm_laplace,
       'times' = list('dm' = t.dm,
                      'lrnm-dm' = t.lrnm_dm,
                      'lrnm-laplace' = t.lrnm_laplace))
}

do_simulations = function(NSIM, N, n, S){
  sims = replicate(NSIM, simulation(N, n, S), simplify = FALSE)
  sims
}

RESULTS = do_simulations(NSIM = 10, N, n, S)

# ld = lapply(list(
#   'dm' = 'dm',
#   'lrnm-dm' = 'lrnm-dm',
#   'lrnm-aitchison' = 'lrnm-aitchison'
# ), function(v)
#   sapply(RESULTS, function(res, v)
#     res[[v]], v))
# 
# boxplot(as.data.frame(ld))
