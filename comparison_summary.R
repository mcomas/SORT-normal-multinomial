library(data.table)
PATH = 'comparison'
fls = list.files(PATH)

pattern_build = "DIM_(.+)-SIZE_(.+)-NORM_(.+)-VAR_(.+)-AGREEMENT_(.+)-SEED_([0-9]+)"
data = rbindlist(lapply(fls, function(fl){
  # print(fl)
  load(file.path(PATH, fl))
  
  PATTERN = gsub('.RData', '', fl)
  d = as.data.table(RESULTS$m1.mc)[,exact := 'mc']
  # d = rbindlist(list(
  #   as.data.table(RESULTS$m1.mc)[,exact := 'mc'],
  #   as.data.table(RESULTS$m1.mcmc)[,exact := 'mcmc']))
  d = melt(d, measure.vars = c('MC','MC-AV','QMC','MCMC'))
  d[,DIM := as.integer(sub(pattern_build, "\\1", PATTERN))]
  d[,SIZE := as.integer(sub(pattern_build, "\\2", PATTERN))]
  d[,NORM := as.numeric(sub(pattern_build, "\\3", PATTERN))]
  d[,VAR := as.matrix(as.numeric(sub(pattern_build, "\\4", PATTERN)))]
  d[,AGREEMENT := as.logical(sub(pattern_build, "\\5", PATTERN))]
  d[,SEED := as.integer(sub(pattern_build, "\\6", PATTERN))]
  d
}))

summary(glm(value~variable+DIM+SIZE+NORM+VAR, data=data))

# ggplot(data=data) +
#   geom_hline(yintercept = 0, col = 'red') +
#   geom_boxplot(aes(x=factor(SIZE),y=value,fill=variable)) +
#   facet_grid(NORM+VAR~DIM, scales = 'free_y') +
#   theme_minimal()
# 
# dplot1 = data[, .(
#   m = median(value, na.rm = TRUE),
#   q1 = quantile(value, 0.025, na.rm = TRUE),
#   q3 = quantile(value, 0.975, na.rm = TRUE)
# ),
# .(variable, exact, DIM, SIZE, NORM, VAR, AGREEMENT)]
# 
# ggplot(data=dplot1) +
#   geom_hline(yintercept = 0, col = 'red') +
#   geom_errorbar(aes(x=variable,y=m,col=factor(SIZE), ymin=q1,ymax=q3), position = position_dodge()) +
#   facet_grid(DIM+NORM~VAR, scales = 'free_y') +
#   theme_minimal()
# 
# summary(glm(value~variable+DIM+SIZE+NORM+VAR, data=data, subset = exact == 'mc'))
# summary(glm(value~variable+DIM+SIZE+NORM+VAR, data=data, subset = exact == 'mcmc'))


library(ggplot2)
dplot = data[, .(
  n = length(value) - sum(is.na(value)),
  m = median(value, na.rm = TRUE),
  q1 = quantile(value, 0.25, na.rm = TRUE),
  q3 = quantile(value, 0.75, na.rm = TRUE)
),
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