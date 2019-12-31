library(data.table)
PATH = 'comparison'
fls = list.files(PATH)

pattern_build = "DIM_(.+)-SIZE_(.+)-NORM_(.+)-VAR_(.+)-AGREEMENT_(.+)-SEED_([0-9]+)"
data.m1 = rbindlist(lapply(fls, function(fl){
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
data.m2 = rbindlist(lapply(fls, function(fl){
  # print(fl)
  load(file.path(PATH, fl))
  
  PATTERN = gsub('.RData', '', fl)
  d = as.data.table(RESULTS$m2.mc)[,exact := 'mc']
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

m1_all = lm(log(value)~variable+DIM+SIZE+NORM+VAR+AGREEMENT, 
            data=data.m1)
m2_all = lm(log(value)~variable+DIM+SIZE+NORM+VAR+AGREEMENT, 
            data=data.m2)

stargazer::stargazer(list(m1_all, m2_all), single.row = TRUE, apply.coef = exp, 
                     ci = TRUE, type = 'latex', star.cutoffs = NA)

library(ggplot2)
dplot.m1 = data.m1[, .(
  n = length(value) - sum(is.na(value)),
  m = median(value, na.rm = TRUE),
  q1 = quantile(value, 0.25, na.rm = TRUE),
  q3 = quantile(value, 0.75, na.rm = TRUE)
),
.(variable, exact, DIM, SIZE, NORM, VAR, AGREEMENT)][,moment := 'First moment']
dplot.m2 = data.m2[, .(
  n = length(value) - sum(is.na(value)),
  m = median(value, na.rm = TRUE),
  q1 = quantile(value, 0.25, na.rm = TRUE),
  q3 = quantile(value, 0.75, na.rm = TRUE)
),
.(variable, exact, DIM, SIZE, NORM, VAR, AGREEMENT)][,moment := 'Second moment']
dplot = rbind(dplot.m1, dplot.m2)

library(latex2exp)
ggplot(data=dplot) +
  # geom_hline(yintercept = 0, col = 'red') +
  geom_boxplot(aes(x=factor(DIM),y=m,fill=variable)) +
  facet_grid(moment+VAR~factor(NORM), scales = 'free_y') +
  theme_minimal() +
  scale_y_continuous(trans = 'log', breaks = 10^c(-4, -3,-2,-1,0)) +
  labs(y = 'Absolute error (logarithmic scale)') +
  theme(legend.position = 'top') +
  labs(fill = 'Method:', x = latex2exp::TeX("Dimension ($\\delta$)"))

l_lambda = as_labeller(function(string) TeX(sprintf("$\\lambda = %s$", string)), 
                       default = label_parsed)
l_nu = as_labeller(function(string) TeX(sprintf("$\\nu = %s$", string)), 
                       default = label_parsed)

p1 = ggplot(data=dplot.m1) +
  # geom_hline(yintercept = 0, col = 'red') +
  geom_boxplot(aes(x=factor(DIM),y=m,fill=variable)) +
  facet_grid(VAR~NORM, scales = 'free_y', labeller = labeller(NORM = l_lambda,
                                                              VAR = l_nu)) +
  theme_minimal() +
  scale_y_continuous(trans = 'log', breaks = 10^c(-4, -3,-2,-1,0), limits = c(10^-3.5, 10^0.8)) +
  theme(legend.position = 'right', legend.justification = "right", panel.spacing.y = unit(1, 'lines')) +
  labs(fill = 'Method:', x = "", subtitle = 'First moment',
       y = 'Absolute error (logarithmic scale)')

p2 = ggplot(data=dplot.m2) +
  # geom_hline(yintercept = 0, col = 'red') +
  geom_boxplot(aes(x=factor(DIM),y=m,fill=variable)) +
  facet_grid(VAR~NORM, scales = 'free_y', labeller = labeller(NORM = l_lambda,
                                                              VAR = l_nu)) +
  theme_minimal() +
  scale_y_continuous(trans = 'log', breaks = 10^c(-4, -3,-2,-1,0), limits = c(10^-3.5, 10^0.8)) +
  theme(legend.position = 'none', legend.justification = "right", panel.spacing.y = unit(1, 'lines')) +
  labs(fill = 'Method:', x = "Dimension (d)", subtitle = 'Second moment',
       y = "Absolute error (logarithmic scale)")

library(egg)
g = ggarrange(p1, p2, ncol = 1)
ggsave(g, width = 7, height = 9.5, filename = 'mvfigure.pdf')


# ggplot(data=dplot) +
#   geom_hline(yintercept = 0, col = 'red') +
#   geom_boxplot(aes(x=variable,y=m,fill=factor(NORM))) +
#   facet_grid(DIM~VAR, scales = 'free_y') +
#   theme_minimal()
# 
# ggplot(data=dplot) +
#   geom_hline(yintercept = 0, col = 'red') +
#   geom_boxplot(aes(x=variable,y=m,fill=exact)) +
#   facet_grid(DIM~SIZE, scales = 'free_y') +
#   theme_minimal()
# 
# ggplot(data=dplot) +
#   geom_hline(yintercept = 0, col = 'red') +
#   geom_boxplot(aes(x=variable,y=m,fill=factor(VAR))) +
#   facet_grid(SIZE~NORM, scales = 'free_y') +
#   theme_minimal()
# 
# ggplot(data=dplot) +
#   geom_hline(yintercept = 0, col = 'red') +
#   geom_boxplot(aes(x=variable,y=m)) +
#   facet_grid(VAR~NORM, scales = 'free_y') +
#   theme_minimal()
# 
