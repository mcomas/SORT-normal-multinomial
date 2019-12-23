########## DEFAULT ARGUMENTS
OUTPUT = comparison
FIRST = 1
LAST = 2
CFLAGS = -c -g -D $(OUTPUT) -D $(FIRST) -D $(LAST)

########## PARAMETERS
DIM := 1 5 25 125
SIZE := 10 100
NORM := 0 1 2
VAR := 0.5 1 2
AGREEMENT := TRUE FALSE
SEED := $(shell seq ${FIRST} ${LAST})

GENERATION = $(foreach dim,$(DIM), $(foreach size,$(SIZE),$(foreach norm,$(NORM),$(foreach var,$(VAR),$(foreach agreement,$(AGREEMENT),$(foreach seed,$(SEED), $(shell printf 'DIM_%s-SIZE_%s-NORM_%s-VAR_%s-AGREEMENT_%s-SEED_%s' $(dim) $(size) $(norm) $(var) $(agreement) $(seed))))))))

RDATA = $(foreach generation,$(GENERATION),$(shell printf '$(OUTPUT)/%s.RData' $(generation)))

all : $(RDATA)

$(OUTPUT)/%.RData : comparison_parameters.R comparison_simulation.R
	Rscript -e 'PATTERN="$*"; source("comparison_parameters.R"); source("comparison_simulation.R"); save.image(file = "$@")'
