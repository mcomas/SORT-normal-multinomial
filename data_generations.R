ex01 = function(S, N, n){
  source('ex01_scenarios.R')
  p = params[[S]]$p
  h = coordinates(p)
  d = length(h)
  
  X = rmultinomial(N, n, p)
  X
}
ex02 = function(S, N, n){
  source('ex02_scenarios.R')
  generator = params[[S]]
  p = generator(N)
  H = coordinates(t(sapply(p, function(p_) c('AA' = p_^2, 'AB' = 2*p_*(1-p_),  'BB' = (1-p_)^2))))
  d = ncol(H)
  
  X = HWData(N, n, p = p)
  X
}
ex03 = function(S, n){
  source('ex03_scenarios.R')
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
  
  X = lapply(1:nrow(X0), function(i, X, size){
    x = X0[i,]
    x.all = rep(1:length(x), x)
    x.sample = sample(x.all, size[i])
    d = as.data.frame(lapply(1:length(x), function(i) sum(x.sample == i)))
    names(d) = 1:6
    d
  }, X0, size) %>% bind_rows
  X = as.matrix(X)
  colnames(X) = names(X0)
  X
}