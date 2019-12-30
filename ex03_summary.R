library(data.table)
library(dplyr)
library(tidyr)
PATH = 'ex03'
fls = list.files(PATH, pattern = "*.RData")
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

pattern_build = "n_([0-9]+)-s_([0-9]+)-seed_([0-9]+)"
data = rbindlist(lapply(fls, function(fl){
  # print(fl)
  load(file.path(PATH, fl))
  
  PATTERN = gsub('.RData', '', fl)
  ld = lapply(list(
    'dm' = 'dm',
    'lrnm-dm' = 'lrnm-dm',
    'lrnm-laplace' = 'lrnm-laplace'
  ), function(v)
    sapply(RESULTS, function(res, v)
      res[[v]], v))
  
  n = as.integer(sub(pattern_build, "\\1", PATTERN))
  s = as.numeric(sub(pattern_build, "\\2", PATTERN))
  seed = as.numeric(sub(pattern_build, "\\3", PATTERN))
  size = mean(params[[s]](rowSums(X0), n))
  
  d = rbindlist(lapply(ld, function(d) as.data.table(t(d))), idcol = 'method')
  d = melt(d, measure.vars = c('paired.dist', 'cov.frobenius',  'stress'))
  d[,n := n]
  d[,s := s]
  d[,seed := seed]
  d[,size := size]
  d
}))

dplot = data[, .(m = mean(value)), .(method, variable, size, s)][,method := factor(method, 
                                                                                   levels = c('dm', 'lrnm-laplace', 'lrnm-dm'))]

library(ggplot2)
dplot$variable = factor(dplot$variable, levels = c('paired.dist', 'cov.frobenius', 'stress'),
                 labels = c('Average Aitchison distance',
                            'Frobenius distance',
                            'STRESS'))

l_s = as_labeller(function(string) sprintf("Scenario %s", string))
g = ggplot(data=dplot) +
  # geom_point(aes(x=size, y=m, col=method), stat = 'identity') +
  geom_line(aes(x=size, y=m, col=method, linetype = method, group=method), stat = 'identity') +
  facet_grid(variable~s, scale = 'free', labeller = labeller(s = l_s)) +
  theme_bw() +
  theme(legend.position = 'top', strip.background = element_rect(fill = 'white')) +
  labs(x = 'Number of trials (n)', y = 'Average differences', fill = 'Models:') +
  scale_color_manual(name="",
                     values = rainbow(3),
                     breaks=c("dm", 'lrnm-laplace', "lrnm-dm"),
                     labels=c("DM", "LNM (SP1)", 'LNM (SP2)')) +
  scale_linetype_manual(name="",
                        values = c("longdash", "solid", "dotted"),
                     breaks=c("dm", 'lrnm-laplace', "lrnm-dm"),
                     labels=c("DM", "LNM (SP1)", 'LNM (SP2)'))

ggsave('ex03-parliament.pdf', g, width = 6, height = 7)

########
## Times 
times = rbindlist(lapply(fls, function(fl){
  # print(fl)
  load(file.path(PATH, fl))
  
  PATTERN = gsub('.RData', '', fl)
  ld = lapply(list(
    'dm' = 'dm',
    'lrnm-dm' = 'lrnm-dm',
    'lrnm-laplace' = 'lrnm-laplace'
  ), function(v)
    sapply(RESULTS, function(res, v)
      res$times[[v]][1], v))
  
  d = as.data.table(ld)
  d = melt(d, measure.vars = names(ld))
  d[,n := as.integer(sub(pattern_build, "\\1", PATTERN))]
  d[,s := as.numeric(sub(pattern_build, "\\2", PATTERN))]
  d[,seed := as.numeric(sub(pattern_build, "\\3", PATTERN))]
  d
}))
ex03_times = dcast(times[,.(m = sprintf("%0.1f [%0.1f, %0.1f]", median(value),
                                        quantile(value, 0.25),
                                        quantile(value, 0.75))), .(variable, s)], s~variable)

save(times, ex03_times, file = 'ex03-parliament.RData')

