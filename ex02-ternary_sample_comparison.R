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

alpha = fit.dm$gamma
X.dm = as.data.frame(t(replicate(100, rmultinom(1, 50, bayesm::rdirichlet(alpha)), simplify = TRUE)))
names(X.dm) = names(X.orig)

load(sprintf('datasets/replacement-N_%05d-n_%05d-s_%05d-seed_%05d-method_nm-maximum3.RData', N, n, s, seed))
mu = fit.lrnm_hermite$mu
sigma = sqrt(diag(fit.lrnm_hermite$sigma))
omega = cov2cor(fit.lrnm_hermite$sigma)[1,2]
X.nm = as.data.frame(rlrnm(50, 100, mu = fit.lrnm_hermite$mu, sigma = fit.lrnm_hermite$sigma))
names(X.nm) = names(X.orig)

L = list('Original' = X.orig,
         'DM' = X.dm,
         'LNM' = X.nm)

df = bind_rows(L, .id='Method')
df$Method = factor(df$Method, 
                   level = c('Original', 'DM', 'LNM'))

g = ggtern() + 
  geom_mask() +
  geom_point(data=df, aes(x = AA, y = AB, z = BB), color = 'blue', alpha=0.8) +
  facet_wrap(~Method, nrow=1) + #, labeller = label_parsed) +
  theme_classic()

ggsave(g, filename = 'ex02-ternary_sample_comparison.pdf', width = 8.44, height = 2.74)
