evaluate = function(h_pi, H_p){
  mean(sqrt(colSums( (t(H_p) - h_pi)^2 )))
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
  fit.dm = dirmult(XZ, trace = FALSE)
  t.dm = proc.time() - t.dm
  
  ## LRNM initialisation with Dirichlet-multinomial
  t.lrnm_dm = proc.time()
  quasi_random = sobol
  fit.dm_init = dirmult(XZ, trace = FALSE)
  Z_quasi = matrix(quasi_random(n = 1000, normal = TRUE, dim = length(p)-1), ncol=length(p)-1)
  fit.lrnm_dm = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = coordinates(t(t(XZ) + fit.dm_init$gamma)))
  t.lrnm_dm = proc.time() - t.lrnm_dm

  ## LRNM initialisation with laplacian approximation
  ## Initialisation
    t.lrnm_laplace = proc.time()
    t.lrnm_laplace_init = proc.time()
    fit.lrnm_laplace_init = fit_lrnm(XZ, probs = TRUE, method = 'laplace')
    t.lrnm_laplace_init = proc.time() - t.lrnm_laplace_init
    
    mu_ = coordinates(colSums(XZ))
    cov_ = diag(length(p)-1)
    iter = 0
    Binv = ilr_basis(length(p))
    while(iter < 100){
      iter  = iter + 1
      MU = t(apply(XZ, 1, l_lrnm_join_maximum, mu_, solve(cov_), Binv))
      mu_new = colMeans(MU)
      cov_ = cov(MU)
      if(max(abs(mu_new - mu_)) < 0.001){
        mu_ = mu_new
        break
      }
      mu_ = mu_new
    }
  ###
  quasi_random = sobol
  Z_quasi = matrix(quasi_random(n = 1000, normal = TRUE, dim = length(p)-1), ncol=length(p)-1)
  fit.lrnm_laplace = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = MU)
  t.lrnm_laplace = proc.time() - t.lrnm_laplace
  
  ### Evaluation
  H.dm = coordinates(t(t(XZ) + fit.dm$gamma))
  e.dm = evaluate(h, H.dm)
  
  H.lrnm_dm = coordinates(fit.lrnm_dm$P)
  e.lrnm_dm = evaluate(h, H.lrnm_dm)
  
  H.lrnm_laplace = coordinates(fit.lrnm_laplace$P)
  e.lrnm_laplace = evaluate(h, H.lrnm_laplace)
  
  H.lrnm_laplace_init = coordinates(fit.lrnm_laplace_init$P)
  e.lrnm_laplace_init = evaluate(h, H.lrnm_laplace_init)

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
