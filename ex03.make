########## DEFAULT ARGUMENTS
OUTPUT = ex03
FIRST = 1
LAST = 1
CFLAGS = -c -g -D $(OUTPUT) -D $(FIRST) -D $(LAST)

L_n = 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
L_s = 1 2
L_seed = $(shell seq ${FIRST} ${LAST})

GENERATION = $(foreach seed,$(L_seed),$(foreach s,$(L_s),$(foreach n,$(L_n),$(shell printf 'n_%05d-s_%05d-seed_%05d' $(N) $(n) $(s) $(seed)))))

RDATA = $(foreach generation,$(GENERATION),$(shell printf '$(OUTPUT)/%s.RData' $(generation)))

all : $(RDATA)

$(OUTPUT)/%.RData : ex03_parameters.R ex03_simulation.R ex03_scenarios.R
	Rscript -e 'PATTERN="$*"; source("ex03_parameters.R"); source("ex03_simulation.R"); save.image(file = "$@")' > $(OUTPUT)/$*.log
