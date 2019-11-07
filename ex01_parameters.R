library(coda.base)
library(coda.count)
library(randtoolbox)
library(digest)

source('ex01_scenarios.R')

if(!exists('build')) build = 'N_01000-n_00050-s_00001-seed_00001'
pattern_build = "N_([0-9]+)-n_([0-9]+)-s_([0-9]+)-seed_([0-9]+)*"

N = as.numeric(sub(pattern_build, "\\1", build))
n = as.numeric(sub(pattern_build, "\\2", build))
S = as.numeric(sub(pattern_build, "\\3", build))
SEED = as.numeric(sub(pattern_build, "\\4", build))

HASH = sha1(build)
SEED_0 = strtoi(substr(HASH,  10, 15), 16L) + strtoi(substr(HASH,  15, 20), 16L)
set.seed(SEED_0 + SEED)

