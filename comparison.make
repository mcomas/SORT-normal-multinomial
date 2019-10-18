SIGMA := 0.2 1 5 25
MU := 0 1 2
SIZE := 1 10 100
AGREEMENT := FALSE TRUE
#FIRST := 1
#LAST := 1
SEED := $(shell seq ${FIRST} ${LAST})

GENERATION = $(foreach size,$(SIZE),$(foreach mu,$(MU),$(foreach sigma,$(SIGMA),$(foreach agreement,$(AGREEMENT),$(foreach seed,$(SEED), $(shell printf 'SIZE_%s-MU_%s-SIGMA_%s-AGREEMENT_%s-SEED_%s' $(size) $(mu) $(sigma) $(agreement) $(seed)))))))

RDATA = $(foreach generation,$(GENERATION),$(shell printf 'comparison/%s.RData' $(generation)))

all : $(RDATA)

comparison/%.RData : comparison_parameters.R comparison_simulation.R
	Rscript -e 'PATTERN="$*"; source("comparison_parameters.R"); source("comparison_simulation.R"); save(X, MU, SIGMA, SEED, RESULTS, file = "$@")'
