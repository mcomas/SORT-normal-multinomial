library(coda.base)
library(coda.count)
library(randtoolbox)
library(digest)

# PATTERN = "DIM_2-SIZE_100-NORM_0-VAR_4-AGREEMENT_TRUE-SEED_1"
pattern_build = "DIM_(.+)-SIZE_(.+)-NORM_(.+)-VAR_(.+)-AGREEMENT_(.+)-SEED_([0-9]+)"

DIM = as.integer(sub(pattern_build, "\\1", PATTERN))
SIZE = as.integer(sub(pattern_build, "\\2", PATTERN))
NORM = as.numeric(sub(pattern_build, "\\3", PATTERN))
VAR = as.numeric(as.numeric(sub(pattern_build, "\\4", PATTERN)))
AGREEMENT = as.logical(sub(pattern_build, "\\5", PATTERN))
SEED = as.integer(sub(pattern_build, "\\6", PATTERN))

HASH = sha1(PATTERN)
SEED_0 = strtoi(substr(HASH,  10, 15), 16L) + strtoi(substr(HASH,  15, 20), 16L)
set.seed(SEED_0 + SEED)

