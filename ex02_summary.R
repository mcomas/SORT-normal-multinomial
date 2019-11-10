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

dplot = data[N==1000, .(m = mean(value)), .(variable, N, n, s)]

library(ggplot2)
# ggplot(data=data) +
#   geom_boxplot(aes(x=factor(n),y=value, fill=variable), position=position_dodge()) +
#   facet_wrap(~s, scale = 'free_y') +
#   theme_minimal()

ggplot(data=dplot) +
  geom_bar(aes(x=factor(n),y=m, fill=variable), stat = 'identity', position=position_dodge()) +
  facet_wrap(~s, scale = 'free_y', ncol = 3) +
  theme_minimal()


