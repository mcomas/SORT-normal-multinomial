library(data.table)

fls = list.files('comparison')

pattern_build = "SIZE_(.+)-MU_(.+)-SIGMA_(.+)-AGREEMENT_(.+)-SEED_([0-9]+)"
data = rbindlist(lapply(fls, function(fl){
  load(file.path('comparison', fl))
  
  PATTERN = gsub('.RData', '', fl)
  d = data.table(as.data.frame(RESULTS), keep.rownames = TRUE)
  d[,SIZE := as.integer(sub(pattern_build, "\\1", PATTERN))]
  d[,MU := as.numeric(sub(pattern_build, "\\2", PATTERN))]
  d[,SIGMA := as.matrix(as.numeric(sub(pattern_build, "\\3", PATTERN)))]
  d[,AGREEMENT := as.logical(sub(pattern_build, "\\4", PATTERN))]
  d[,SEED := as.integer(sub(pattern_build, "\\5", PATTERN))]
  d
}))

library(ggplot2)
dplot = data[,.(m=median(M1.mean), q1=quantile(M1.mean, 0.25), q3=quantile(M1.mean, 0.75)),
             .(rn, SIZE, MU, SIGMA, AGREEMENT)]

ggplot(data=dplot) +
  geom_hline(yintercept = 0, col = 'red') +
  geom_boxplot(aes(x=rn,y=m)) +
  facet_wrap(~SIGMA, scales = 'free_y') +
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