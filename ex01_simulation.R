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

  ## LRNM initialisation with laplacian approximation
  t.lrnm_laplace = proc.time()
  t.lrnm_laplace_init = proc.time()
  fit.lrnm_laplace_init = fit_lrnm(XZ, probs = TRUE, method = 'laplace')
  t.lrnm_laplace_init = proc.time() - t.lrnm_laplace_init
  
  quasi_random = sobol
  Z_quasi = matrix(quasi_random(n = 1000, normal = TRUE, dim = length(p)-1), ncol=length(p)-1)
  fit.lrnm_laplace = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = coordinates(fit.lrnm_laplace_init$P))
  t.lrnm_laplace = proc.time() - t.lrnm_laplace
  
  H.lrnm_laplace = coordinates(fit.lrnm_laplace$P)
  e.lrnm_laplace = sqrt(sum( rowMeans(t(H.lrnm_laplace) - h)^2 ))
  
  ## LRNM with laplacian approximation
  H.lrnm_laplace_init = coordinates(fit.lrnm_laplace_init$P)
  e.lrnm_laplace_init = sqrt(sum( rowMeans(t(H.lrnm_laplace_init) - h)^2 ))

  list('dm' = e.dm,
       'lrnm-dm' = e.lrnm_dm,
       'lrnm-laplace' = e.lrnm_laplace,
       'lrnm-laplace-initialisation' = e.lrnm_laplace_init,
       'times' = list('dm' = t.dm,
                      'lrnm-dm' = t.lrnm_dm,
                      'lrnm-laplace' = t.lrnm_laplace,
                      'lrnm-laplace-init' = t.lrnm_laplace_init))
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
