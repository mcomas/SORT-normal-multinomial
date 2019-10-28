library(coda.base)
library(coda.count)

# PATTERN = "DIM_2-SIZE_100-NORM_0-VAR_4-AGREEMENT_TRUE-SEED_1"
pattern_build = "DIM_(.+)-SIZE_(.+)-NORM_(.+)-VAR_(.+)-AGREEMENT_(.+)-SEED_([0-9]+)"

DIM = as.integer(sub(pattern_build, "\\1", PATTERN))
SIZE = as.integer(sub(pattern_build, "\\2", PATTERN))
NORM = as.numeric(sub(pattern_build, "\\3", PATTERN))
VAR = as.numeric(as.numeric(sub(pattern_build, "\\4", PATTERN)))
AGREEMENT = as.logical(sub(pattern_build, "\\5", PATTERN))
SEED = as.integer(sub(pattern_build, "\\6", PATTERN))

set.seed(DIM*SIZE*VAR+2*NORM+3*AGREEMENT+SEED)
n_sphere = function(d){
  x = rnorm(d)
  x/sqrt(sum(x^2))
}
MU = n_sphere(DIM) * NORM
CORR = cov2cor(as.matrix(rWishart(1, DIM^2, diag(DIM))[,,1]))
SIGMA = CORR * VAR
if(AGREEMENT){
  X = rmultinomial(1, SIZE, composition(MU))
}else{
  X = rmultinomial(1, SIZE, composition(-MU))
}
B = ilr_basis(DIM+1)

ORDER = 50
source("comparison_simulation.R")

