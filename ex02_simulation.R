evaluate = function(H_pi, H_p){
  mean(sqrt(rowSums(H_p - H_pi)^2 ))
}

simulation = function(N, n, S){
  generator = params[[S]]
  p = generator(N)
  H = coordinates(t(sapply(p, function(p_) c('AA' = p_^2, 'AB' = 2*p_*(1-p_),  'BB' = (1-p_)^2))))
  
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
  fit.dm_init = dirmult(XZ, trace = FALSE)
  Z_quasi = matrix(quasi_random(n = 1000, normal = TRUE, dim = length(p)-1), ncol=2)
  fit.lrnm_dm = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = coordinates(t(t(XZ) + fit.dm_init$gamma)))
  t.lrnm_dm = proc.time() - t.lrnm_dm

  ## LRNM initialisation with laplacian approximation
  ## Initialisation
    t.lrnm_laplace = proc.time()
    t.lrnm_laplace_init = proc.time()
    fit.lrnm_laplace_init = fit_lrnm(XZ, probs = TRUE, method = 'laplace', eps = 0.1)
    t.lrnm_laplace_init = proc.time() - t.lrnm_laplace_init
  ###
  quasi_random = sobol
  Z_quasi = matrix(quasi_random(n = 1000, normal = TRUE, dim = length(p)-1), ncol=2)
  fit.lrnm_laplace = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = coordinates(fit.lrnm_laplace_init$P))
  t.lrnm_laplace = proc.time() - t.lrnm_laplace
  
  ### Evaluation
  H.dm = coordinates(t(t(XZ) + fit.dm$gamma))
  e.dm = evaluate(H, H.dm)
  
  H.lrnm_dm = coordinates(fit.lrnm_dm$P)
  e.lrnm_dm = evaluate(H, H.lrnm_dm)
  
  H.lrnm_laplace = coordinates(fit.lrnm_laplace$P)
  e.lrnm_laplace = evaluate(H, H.lrnm_laplace)
  
  H.lrnm_laplace_init = coordinates(fit.lrnm_laplace_init$P)
  e.lrnm_laplace_init = evaluate(H, H.lrnm_laplace_init)

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
