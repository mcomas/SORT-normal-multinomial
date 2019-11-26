source('init_lrnm.R')

evaluate = function(H_gs, H_p){
  c('paired.dist' = mean(apply(H_gs-H_p, 1, function(x) sqrt(sum(x^2)))),
  'cov.frobenius' = norm(cov(H_gs) - cov(H_p), type = 'F'),
  'stress' = sqrt(sum((as.matrix(dist(H_gs)) -  as.matrix(dist(H_p)))^2) / sum(as.matrix(dist(H_gs))^2)))
}

simulation = function(n, S){
  load('ex03_parliament2015.RData')
  nvotes = parliament2015 %>% 
    select(name, jxsi:cup) %>%
    gather(key=party, value=votes, -name) %>%
    group_by(name) %>%
    summarise(
      total = sum(votes)
    )
  nvotes.sub = nvotes %>% 
    subset(total > 1000) %>%
    left_join(parliament2015 %>% select(name, jxsi:cup), by='name')
  
  X0 = nvotes.sub %>% 
    select(jxsi:cup)
  
  size = params[[S]](nvotes.sub$total, n)
  
  XZ = lapply(1:nrow(X0), function(i, X, size){
    x = X0[i,]
    x.all = rep(1:length(x), x)
    x.sample = sample(x.all, size[i])
    d = as.data.frame(lapply(1:length(x), function(i) sum(x.sample == i)))
    names(d) = 1:6
    d
  }, X0, size) %>% bind_rows
  XZ = as.matrix(XZ)
  colnames(XZ) = names(X0)
  
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
  Z_quasi = matrix(quasi_random(n = 10000, normal = TRUE, dim = ncol(XZ)-1), ncol=ncol(XZ)-1)
  fit.lrnm_dm = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = H.lrnm_dm)
  t.lrnm_dm = proc.time() - t.lrnm_dm

  ## LRNM initialisation with Laplace
  t.lrnm_laplace = proc.time()
  quasi_random = sobol
  H.lrnm_laplace = lrnm_laplace.init(XZ)
  Z_quasi = matrix(quasi_random(n = 10000, normal = TRUE, dim = ncol(XZ)-1), ncol=ncol(XZ)-1)
  fit.lrnm_laplace = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi, H.ini = H.lrnm_laplace)
  t.lrnm_laplace = proc.time() - t.lrnm_laplace
  
  ### Evaluation
  H.gs = coordinates(X0)
  
  H.dm = coordinates(t(t(XZ) + fit.dm$gamma))
  e.dm = evaluate(H.gs, H.dm)
  
  H.lrnm_dm = coordinates(fit.lrnm_dm$P)
  e.lrnm_dm = evaluate(H.gs, H.lrnm_dm)
  
  H.lrnm_laplace = coordinates(fit.lrnm_laplace$P)
  e.lrnm_laplace = evaluate(H.gs, H.lrnm_laplace)

  list('dm' = e.dm,
       'lrnm-dm' = e.lrnm_dm,
       'lrnm-laplace' = e.lrnm_laplace,
       'times' = list('dm' = t.dm,
                      'lrnm-dm' = t.lrnm_dm,
                      'lrnm-laplace' = t.lrnm_laplace))
}

do_simulations = function(NSIM, n, S){
  sims = replicate(NSIM, simulation(n, S), simplify = FALSE)
  sims
}

RESULTS = do_simulations(NSIM = 10, n, S)

# ld = lapply(list(
#   'dm' = 'dm',
#   'lrnm-dm' = 'lrnm-dm',
#   'lrnm-aitchison' = 'lrnm-aitchison'
# ), function(v)
#   sapply(RESULTS, function(res, v)
#     res[[v]], v))
# 
# boxplot(as.data.frame(ld))
