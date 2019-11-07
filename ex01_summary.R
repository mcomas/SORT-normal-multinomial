library(data.table)
PATH = 'ex01'
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

dplot = data[, .(m = mean(value)), .(variable, N, n, s)]

library(ggplot2)
ggplot(data=data) +
  geom_boxplot(aes(x=factor(n),y=value, col=variable), position=position_dodge()) +
  coord_cartesian(ylim = c(0, 0.06))

ggplot(data=data) +
  geom_hline(yintercept = 0, col = 'red') +
  geom_boxplot(aes(x=factor(SIZE),y=value, fill=variable)) +
  facet_grid(NORM+VAR~DIM, scales = 'free_y') +
  theme_minimal()

dplot1 = data[,.(m=median(value, na.rm=TRUE), q1=quantile(value, 0.025, na.rm=TRUE), q3=quantile(value, 0.975, na.rm=TRUE)),
             .(variable, exact, DIM, SIZE, NORM, VAR, AGREEMENT)]

ggplot(data=dplot1) +
  geom_hline(yintercept = 0, col = 'red') +
  geom_errorbar(aes(x=variable,y=m,col=factor(SIZE), ymin=q1,ymax=q3), position = position_dodge()) +
  facet_grid(DIM+NORM~VAR, scales = 'free_y') +
  theme_minimal()

summary(glm(value~variable+DIM+SIZE+NORM+VAR, data=data, subset = exact == 'mc'))
summary(glm(value~variable+DIM+SIZE+NORM+VAR, data=data, subset = exact == 'mcmc'))


library(ggplot2)
dplot = data[,.(m=median(value, na.rm=TRUE), q1=quantile(value, 0.25, na.rm=TRUE), q3=quantile(value, 0.75, na.rm=TRUE)),
             .(variable, exact, DIM, SIZE, NORM, VAR, AGREEMENT)]

ggplot(data=dplot) +
  geom_hline(yintercept = 0, col = 'red') +
  geom_boxplot(aes(x=variable,y=m,fill=factor(NORM))) +
  facet_grid(DIM~VAR, scales = 'free_y') +
  theme_minimal()

ggplot(data=dplot) +
  geom_hline(yintercept = 0, col = 'red') +
  geom_boxplot(aes(x=variable,y=m,fill=exact)) +
  facet_grid(DIM~SIZE, scales = 'free_y') +
  theme_minimal()

ggplot(data=dplot) +
  geom_hline(yintercept = 0, col = 'red') +
  geom_boxplot(aes(x=variable,y=m,fill=factor(VAR))) +
  facet_grid(SIZE~NORM, scales = 'free_y') +
  theme_minimal()

ggplot(data=dplot) +
  geom_hline(yintercept = 0, col = 'red') +
  geom_boxplot(aes(x=rn,y=m)) +
  facet_grid(SIGMA~MU, scales = 'free_y') +
  theme_minimal()

dplot.summ = dplot[,.(m = mean(m), s = sd(m)), .(rn, MU, SIGMA, AGREEMENT)]
ggplot(data=dplot.summ) +
  geom_hline(yintercept = 0, col = 'red') +
  geom_point(aes(x=rn, y=m, col = AGREEMENT), position = position_dodge(width = 1)) +
  geom_errorbar(aes(x=rn,ymin =m-s,ymax=m+s,col=AGREEMENT), width=0.2, position = position_dodge(width = 1)) +
  facet_grid(SIGMA~MU) +
  theme_minimal()

ggplot(data=dplot.summ) +
  geom_hline(yintercept = 0, col = 'red') +
  geom_point(aes(x=rn, y=m, col = AGREEMENT), position = position_dodge(width = 1)) +
  geom_errorbar(aes(x=rn,ymin =m-s,ymax=m+s, col = AGREEMENT), width=0.2, position = position_dodge(width = 1)) +
  facet_grid(SIGMA~MU) +
  theme_minimal() +
  coord_cartesian(ylim = c(-0.001, 0.001))

# The lower the variance 