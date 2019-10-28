DIM := 1 5 15 30 60
SIZE := 1 10 100
NORM := 0
VAR := 0.5 1 2
AGREEMENT := FALSE TRUE
#FIRST := 1
#LAST := 1
SEED := $(shell seq ${FIRST} ${LAST})

GENERATION = $(foreach dim,$(DIM), $(foreach size,$(SIZE),$(foreach norm,$(NORM),$(foreach var,$(VAR),$(foreach agreement,$(AGREEMENT),$(foreach seed,$(SEED), $(shell printf 'DIM_%s-SIZE_%s-NORM_%s-VAR_%s-AGREEMENT_%s-SEED_%s' $(dim) $(size) $(norm) $(var) $(agreement) $(seed))))))))

RDATA = $(foreach generation,$(GENERATION),$(shell printf 'comparison/%s.RData' $(generation)))

all : $(RDATA)

comparison/%.RData : comparison_parameters.R comparison_simulation.R
	Rscript -e 'PATTERN="$*"; source("comparison_parameters.R"); source("comparison_simulation.R"); save(X, NORM, MU, SIGMA, SEED, RESULTS, file = "$@")'
