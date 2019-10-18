dir.create('comparison', showWarnings = FALSE)

# Generated with
# make -f comparison.make FIRST=1 LAST=1 -n | sed -e "s/Rscript -e //" -e  "s/'//g" -e $'s/; /\\\n/g'
PATTERN="SIZE_1-MU_0-SIGMA_0.2-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_0-SIGMA_0.2-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_0-SIGMA_0.2-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_0-SIGMA_0.2-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_1-MU_0-SIGMA_1-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_0-SIGMA_1-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_0-SIGMA_1-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_0-SIGMA_1-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_1-MU_0-SIGMA_5-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_0-SIGMA_5-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_0-SIGMA_5-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_0-SIGMA_5-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_1-MU_0-SIGMA_25-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_0-SIGMA_25-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_0-SIGMA_25-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_0-SIGMA_25-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_1-MU_1-SIGMA_0.2-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_1-SIGMA_0.2-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_1-SIGMA_0.2-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_1-SIGMA_0.2-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_1-MU_1-SIGMA_1-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_1-SIGMA_1-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_1-SIGMA_1-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_1-SIGMA_1-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_1-MU_1-SIGMA_5-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_1-SIGMA_5-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_1-SIGMA_5-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_1-SIGMA_5-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_1-MU_1-SIGMA_25-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_1-SIGMA_25-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_1-SIGMA_25-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_1-SIGMA_25-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_1-MU_2-SIGMA_0.2-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_2-SIGMA_0.2-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_2-SIGMA_0.2-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_2-SIGMA_0.2-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_1-MU_2-SIGMA_1-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_2-SIGMA_1-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_2-SIGMA_1-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_2-SIGMA_1-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_1-MU_2-SIGMA_5-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_2-SIGMA_5-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_2-SIGMA_5-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_2-SIGMA_5-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_1-MU_2-SIGMA_25-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_2-SIGMA_25-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_1-MU_2-SIGMA_25-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_1-MU_2-SIGMA_25-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_0-SIGMA_0.2-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_0-SIGMA_0.2-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_0-SIGMA_0.2-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_0-SIGMA_0.2-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_0-SIGMA_1-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_0-SIGMA_1-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_0-SIGMA_1-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_0-SIGMA_1-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_0-SIGMA_5-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_0-SIGMA_5-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_0-SIGMA_5-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_0-SIGMA_5-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_0-SIGMA_25-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_0-SIGMA_25-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_0-SIGMA_25-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_0-SIGMA_25-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_1-SIGMA_0.2-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_1-SIGMA_0.2-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_1-SIGMA_0.2-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_1-SIGMA_0.2-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_1-SIGMA_1-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_1-SIGMA_1-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_1-SIGMA_1-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_1-SIGMA_1-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_1-SIGMA_5-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_1-SIGMA_5-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_1-SIGMA_5-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_1-SIGMA_5-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_1-SIGMA_25-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_1-SIGMA_25-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_1-SIGMA_25-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_1-SIGMA_25-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_2-SIGMA_0.2-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_2-SIGMA_0.2-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_2-SIGMA_0.2-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_2-SIGMA_0.2-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_2-SIGMA_1-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_2-SIGMA_1-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_2-SIGMA_1-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_2-SIGMA_1-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_2-SIGMA_5-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_2-SIGMA_5-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_2-SIGMA_5-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_2-SIGMA_5-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_10-MU_2-SIGMA_25-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_2-SIGMA_25-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_10-MU_2-SIGMA_25-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_10-MU_2-SIGMA_25-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_0-SIGMA_0.2-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_0-SIGMA_0.2-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_0-SIGMA_0.2-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_0-SIGMA_0.2-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_0-SIGMA_1-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_0-SIGMA_1-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_0-SIGMA_1-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_0-SIGMA_1-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_0-SIGMA_5-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_0-SIGMA_5-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_0-SIGMA_5-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_0-SIGMA_5-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_0-SIGMA_25-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_0-SIGMA_25-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_0-SIGMA_25-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_0-SIGMA_25-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_1-SIGMA_0.2-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_1-SIGMA_0.2-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_1-SIGMA_0.2-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_1-SIGMA_0.2-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_1-SIGMA_1-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_1-SIGMA_1-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_1-SIGMA_1-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_1-SIGMA_1-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_1-SIGMA_5-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_1-SIGMA_5-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_1-SIGMA_5-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_1-SIGMA_5-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_1-SIGMA_25-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_1-SIGMA_25-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_1-SIGMA_25-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_1-SIGMA_25-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_2-SIGMA_0.2-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_2-SIGMA_0.2-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_2-SIGMA_0.2-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_2-SIGMA_0.2-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_2-SIGMA_1-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_2-SIGMA_1-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_2-SIGMA_1-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_2-SIGMA_1-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_2-SIGMA_5-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_2-SIGMA_5-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_2-SIGMA_5-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_2-SIGMA_5-AGREEMENT_TRUE-SEED_1.RData")

PATTERN="SIZE_100-MU_2-SIGMA_25-AGREEMENT_FALSE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_2-SIGMA_25-AGREEMENT_FALSE-SEED_1.RData")

PATTERN="SIZE_100-MU_2-SIGMA_25-AGREEMENT_TRUE-SEED_1"
source("comparison_parameters.R")
source("comparison_simulation.R")
save(X, MU, SIGMA, SEED, RESULTS, file = "comparison/SIZE_100-MU_2-SIGMA_25-AGREEMENT_TRUE-SEED_1.RData")
