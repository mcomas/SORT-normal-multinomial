library(data.table)
PATH = 'ex02'
fls = list.files(PATH, pattern = "*.RData")

pattern_build = "N_([0-9]+)-n_([0-9]+)-s_([0-9]+)-seed_([0-9]+)"
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
  
  d = as.data.table(ld)
  d = melt(d, measure.vars = names(ld))
  d[,N := as.integer(sub(pattern_build, "\\1", PATTERN))]
  d[,n := as.integer(sub(pattern_build, "\\2", PATTERN))]
  d[,s := as.numeric(sub(pattern_build, "\\3", PATTERN))]
  d[,seed := as.numeric(sub(pattern_build, "\\4", PATTERN))]
  d
}))

dplot = data[, .(m = mean(value)), .(variable, N, n, s)][,variable := factor(variable, 
                                                                             levels = c('dm', 'lrnm-laplace', 'lrnm-dm'))]

library(ggplot2)
# ggplot(data=data) +
#   geom_boxplot(aes(x=factor(n),y=value, fill=variable), position=position_dodge()) +
#   facet_wrap(~s, scale = 'free_y') +
#   theme_minimal()

l_s = as_labeller(function(string) sprintf("Scenario %s", string))
g = ggplot(data=dplot) +
  geom_bar(aes(x=factor(n),y=m, fill=variable), col='black', stat = 'identity', position=position_dodge()) +
  facet_wrap(~s, scale = 'free_y', ncol = 3, labeller = labeller(s = l_s)) +
  theme_classic() +
  labs(x = 'Number of trials (n)', y = 'Average of Aitchison distances', fill = 'Models:') +
  theme(legend.position = 'top') +
  scale_fill_manual(values = rainbow(3),
                    breaks = c('dm', 'lrnm-laplace', 'lrnm-dm'),
                    labels=c("DM", "LNM (SP1)", "LNM (SP2)"))

ggsave('ex02-hardyweinberg.pdf', g, width = 7, height = 4)

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
  d[,N := as.integer(sub(pattern_build, "\\1", PATTERN))]
  d[,n := as.integer(sub(pattern_build, "\\2", PATTERN))]
  d[,s := as.numeric(sub(pattern_build, "\\3", PATTERN))]
  d[,seed := as.numeric(sub(pattern_build, "\\4", PATTERN))]
  d
}))
ex02_times = dcast(times[,.(m = sprintf("%0.1f [%0.1f, %0.1f]", median(value),
                                        quantile(value, 0.25),
                                        quantile(value, 0.75))), .(variable, s)], s~variable)

save(times, ex02_times, file = 'ex02-hardyweinberg.RData')

