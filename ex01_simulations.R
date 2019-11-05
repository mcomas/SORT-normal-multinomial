simulation = function(N, n, S){
  p = params[[S]]$p
  h = coordinates(p)
  
  cat(sprintf("%s\nN=%d\nn=%d\np=(%s)\n----\n", date(), N, n, paste(p, collapse=', ')))
  
  XZ = rmultinomial(N, n, p)
  isZERO = XZ == 0
  row.zeros = table(apply(isZERO, 1, sum))
  col.zeros = apply(isZERO, 2, sum)
  
  ## Dirichlet-multinomial fitting
  t.start = proc.time()
  fit1 <- fit_dm(XZ)
  t.diff = proc.time() - t.start
  H1 = coordinates(t(t(XZ) + fit1[,1]))
  e1 = sqrt(sum( rowMeans(t(H1) - h)^2 ))
  
  ##
  library(randtoolbox)
  quasi_random = sobol
  Z_quasi = matrix(quasi_random(n = 1000, normal = TRUE, dim = length(p)-1), ncol=length(p)-1)
  fit2 = fit_lrnm(XZ, probs = TRUE, Z = Z_quasi)
  H2 = coordinates(fit2$P)
  e2 = sqrt(sum( rowMeans(t(H2) - h)^2 ))
  
  fit3 = fit_lrnm(XZ, probs = TRUE, method = 'laplace')
  H3 = coordinates(fit3$P)
  e3 = sqrt(sum( rowMeans(t(H3) - h)^2 ))

  list('dm' = e1, 'lrnm-qmc' = e2, 'lrnm-laplace' = e3)
}

do_simulations = function(NSIM, N, n, S){
  sims = replicate(NSIM, simulation(N, n, S), simplify = FALSE)
  
}
  

sqrt(sum((coordinates(p)-coordinates(p1))^2))
sqrt(sum((coordinates(p)-coordinates(p2))^2))
sqrt(sum((coordinates(p)-coordinates(p3))^2))

#save(list = ls(), file = sprintf('datasets/dataset-%s.RData', build))
