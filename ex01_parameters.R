library(coda.base)
library(coda.count)
library(dirmult)
library(randtoolbox)
library(digest)

source('ex01_scenarios.R')

if(!exists('PATTERN')) PATTERN = 'N_01000-n_00050-s_00001-seed_00001'
pattern_build = "N_([0-9]+)-n_([0-9]+)-s_([0-9]+)-seed_([0-9]+)*"

N = as.numeric(sub(pattern_build, "\\1", PATTERN))
n = as.numeric(sub(pattern_build, "\\2", PATTERN))
S = as.numeric(sub(pattern_build, "\\3", PATTERN))
SEED = as.numeric(sub(pattern_build, "\\4", PATTERN))

HASH = sha1(PATTERN)
SEED_0 = strtoi(substr(HASH,  10, 15), 16L) + strtoi(substr(HASH,  15, 20), 16L)
set.seed(SEED_0 + SEED)

