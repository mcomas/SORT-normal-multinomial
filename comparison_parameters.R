library(coda.base)
library(coda.count)

pattern_build = "DIM_(.+)-SIZE_(.+)-MU_(.+)-SIGMA_(.+)-AGREEMENT_(.+)-SEED_([0-9]+)"

DIM = as.integer(sub(pattern_build, "\\1", PATTERN))
SIZE = as.integer(sub(pattern_build, "\\2", PATTERN))

MU = as.numeric(sub(pattern_build, "\\3", PATTERN))
SIGMA = as.matrix(as.numeric(sub(pattern_build, "\\4", PATTERN)))
AGREEMENT = as.logical(sub(pattern_build, "\\5", PATTERN))
SEED = as.integer(sub(pattern_build, "\\6", PATTERN))

set.seed(SEED)
B = ilr_basis(2)
CMU = composition(MU, B)
X = rmultinomial(n = 1, size = SIZE, CMU)
if(!AGREEMENT){
  X = X[,ncol(X):1, drop = FALSE]
}
