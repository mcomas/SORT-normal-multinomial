source('init_lrnm.R')

evaluate = function(h_pi, H_p){
  mean(sqrt(colSums( (t(H_p) - h_pi)^2 )))
}

simulation = function(N, n, S){
  p = params[[S]]$p
  h = coordinates(p)
  d = length(h)
  
  cat(sprintf("%s\nN=%d\nn=%d\np=(%s)\n----\n", date(), N, n, paste(p, collapse=', ')))
  
  XZ = rmultinomial(N, n, p)
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
  Z_quasi = matrix(quasi_random(n = 10000, normal = TRUE, dim = length(p)-1), ncol=length(p)-1)
  fit.lrnm_dm = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = H.lrnm_dm)
  t.lrnm_dm = proc.time() - t.lrnm_dm

  ## LRNM initialisation with Laplace
  t.lrnm_laplace = proc.time()
  quasi_random = sobol
  H.lrnm_laplace = lrnm_laplace.init(XZ)$H
  Z_quasi = matrix(quasi_random(n = 10000, normal = TRUE, dim = length(p)-1), ncol=length(p)-1)
  fit.lrnm_laplace = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = H.lrnm_laplace)
  t.lrnm_laplace = proc.time() - t.lrnm_laplace
  
  ### Evaluation
  H.dm = coordinates(t(t(XZ) + fit.dm$gamma))
  e.dm = evaluate(h, H.dm)
  
  H.lrnm_dm = coordinates(fit.lrnm_dm$P)
  e.lrnm_dm = evaluate(h, H.lrnm_dm)
  
  H.lrnm_laplace = coordinates(fit.lrnm_laplace$P)
  e.lrnm_laplace = evaluate(h, H.lrnm_laplace)

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
