aitchison_approximation = function(XZ){
  G = colMeans(XZ)
  G = G / sum(G)
  
  
  K = cov(XZ/rowSums(XZ))
  
  TAU = matrix(0, nrow=nrow(K), ncol=ncol(K))
  tau = function(i,j){
    K[i,i]/G[i]^2 - 2 * K[i,j]/(G[i]*G[j]) + K[j,j]/G[j]^2 + 1/4 * (K[i,i]/G[i]^2 - K[j,j]/G[j]^2)^2
  }
  for(i in 1:nrow(K)){for(j in 1:ncol(K)){ TAU[i,j] = tau(i,j) }}
  Fn = cbind(diag(1,nrow(K)-1), -1)
  S = -0.5 * Fn %*% TAU %*% t(Fn)

  alrTOilr = MASS::ginv(alr_basis(ncol(XZ))) %*% ilr_basis(ncol(XZ))
  
  MU = coordinates(G)
  SIGMA = alrTOilr %*% S %*% t(alrTOilr)
  t(sapply(coda.count::lrnm_posterior_approx(XZ, MU, SIGMA, B = ilr_basis(ncol(XZ))), function(l) l$mu))
}

simulation = function(N, n, S){
  p = params[[S]]$p
  h = coordinates(p)
  
  cat(sprintf("%s\nN=%d\nn=%d\np=(%s)\n----\n", date(), N, n, paste(p, collapse=', ')))
  
  XZ = rmultinomial(N, n, p)
  isZERO = XZ == 0
  row.zeros = table(apply(isZERO, 1, sum))
  col.zeros = apply(isZERO, 2, sum)
  
  ## Dirichlet-multinomial fitting
  t.dm = proc.time()
  fit.dm <- fit_dm(XZ)
  t.dm = proc.time() - t.dm
  
  H.dm = coordinates(t(t(XZ) + fit.dm[,1]))
  e.dm = sqrt(sum( rowMeans(t(H.dm) - h)^2 ))
  
  ## LRNM initialisation with Dirichlet
  t.lrnm_dm = proc.time()
  quasi_random = sobol
  Z_quasi = matrix(quasi_random(n = 1000, normal = TRUE, dim = length(p)-1), ncol=length(p)-1)
  fit.lrnm_dm = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi)
  t.lrnm_dm = proc.time() - t.lrnm_dm
  
  H.lrnm_dm = coordinates(fit.lrnm_dm$P)
  e.lrnm_dm = sqrt(sum( rowMeans(t(H.lrnm_dm) - h)^2 ))

  ## LRNM initialisation with Aitchison approach
  t.lrnm_aitchison = proc.time()
  quasi_random = sobol
  Z_quasi = matrix(quasi_random(n = 1000, normal = TRUE, dim = length(p)-1), ncol=length(p)-1)
  fit.lrnm_aitchison = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = aitchison_approximation(XZ))
  t.lrnm_aitchison = proc.time() - t.lrnm_aitchison
  
  H.lrnm_aitchison = coordinates(fit.lrnm_aitchison$P)
  e.lrnm_aitchison = sqrt(sum( rowMeans(t(H.lrnm_aitchison) - h)^2 ))
  
  ## LRNM with laplacian approximation
  t.lrnm_laplace = proc.time()
  fit.lrnm_laplace = fit_lrnm(XZ, probs = TRUE, method = 'laplace')
  t.lrnm_laplace = proc.time() - t.lrnm_laplace
  
  H.lrnm_laplace = coordinates(fit.lrnm_laplace$P)
  e.lrnm_laplace = sqrt(sum( rowMeans(t(H.lrnm_laplace) - h)^2 ))

  list('dm' = e.dm,
       'lrnm-dm' = e.lrnm_dm,
       'lrnm-aitchison' = e.lrnm_aitchison,
       'lrnm-laplace' = e.lrnm_laplace,
       'times' = list('dm' = t.dm,
                      'lrnm-dm' = t.lrnm_dm,
                      'lrnm-aitchison' = t.lrnm_aitchison,
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
