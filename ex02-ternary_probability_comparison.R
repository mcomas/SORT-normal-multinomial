library(coda.count)
library(coda.base)
library(dplyr)
library(ggtern)
library(bayesm)
library(latex2exp)
library(HardyWeinberg)
library(dirmult)
library(randtoolbox)

source('ex02_scenarios.R')

N = 1000
n = 50
s = 1

set.seed(1)
generator = params[[s]]
p = generator(N)
H = coordinates(t(sapply(p, function(p_) c('AA' = p_^2, 'AB' = 2*p_*(1-p_),  'BB' = (1-p_)^2))))
d = ncol(H)

XZ = HWData(N, n, p = p)

X.orig = as.data.frame(XZ[1:100,])
fit.dm = dirmult(XZ, trace = FALSE)
fit.lrnm_hermite = fit_lrnm(XZ, probs = TRUE, method = 'hermite')

P.gs = lapply(p, function(q) tibble('AA' = q^2, 'AB' = 2*q*(1-q),  'BB' = (1-q)^2)) %>% bind_rows


E.dm = t(t(XZ) + fit.dm$gamma)
E.dm = as.data.frame(E.dm)
names(E.dm) = names(P.gs)

E.nm = fit.lrnm_hermite$P
E.nm = as.data.frame(E.nm)
names(E.nm) = names(P.gs)

i = 1:50
L = list('Original' = P.gs[i,],
         'DM' = E.dm[i,],
         'LNM' = E.nm[i,])

df = bind_rows(L, .id='Method')
df$Method = factor(df$Method, level = c('Original', 'DM', 'LNM'))

g = ggtern() + 
  geom_mask() +
  geom_point(data=df, aes(x = AA, y = AB, z = BB), color = 'blue', alpha=0.8) +
  scale_T_continuous(breaks=seq(0.2,1,0.2),minor_breaks=seq(0,1,0.1), labels = seq(0.2,1,0.2)) +
  scale_L_continuous(breaks=seq(0.2,1,0.2),minor_breaks=seq(0,1,0.1), labels = seq(0.2,1,0.2)) +
  scale_R_continuous(breaks=seq(0.2,1,0.2),minor_breaks=seq(0,1,0.1), labels = seq(0.2,1,0.2)) +
  facet_wrap(~Method, nrow=1) +
  theme_classic()

ggsave(g, filename = 'ex02-ternary_probability_comparison.pdf', width = 8.44, height = 2.74)
