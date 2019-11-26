library(coda.base)
library(coda.count)
library(dirmult)
library(randtoolbox)
library(digest)
library(dplyr)
library(tidyr)

source('ex03_scenarios.R')

if(!exists('PATTERN')) PATTERN = 'n_00001-s_00001-seed_00001'
# PATTERN="n_00001-s_00002-seed_00001"

pattern_build = "n_([0-9]+)-s_([0-9]+)-seed_([0-9]+)*"

n = as.numeric(sub(pattern_build, "\\1", PATTERN))
S = as.numeric(sub(pattern_build, "\\2", PATTERN))
SEED = as.numeric(sub(pattern_build, "\\3", PATTERN))

HASH = sha1(PATTERN)
SEED_0 = strtoi(substr(HASH,  10, 15), 16L) + strtoi(substr(HASH,  15, 20), 16L)
set.seed(SEED_0 + SEED)

