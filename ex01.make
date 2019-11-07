########## DEFAULT ARGUMENTS
OUTPUT = ex01
FIRST = 1
LAST = 1
CFLAGS = -c -g -D $(OUTPUT) -D $(FIRST) -D $(LAST)

L_N = 1000
L_n = 50 100 200 500
L_s =  1 2 3 4 5 6 7 8 9
L_seed = $(shell seq ${FIRST} ${LAST})

GENERATION = $(foreach seed,$(L_seed),$(foreach s,$(L_s),$(foreach n,$(L_n),$(foreach N,$(L_N),$(shell printf 'N_%05d-n_%05d-s_%05d-seed_%05d.RData' $(N) $(n) $(s) $(seed))))))

RDATA = $(foreach generation,$(GENERATION),$(shell printf '$(OUTPUT)/%s.RData' $(generation)))

all : $(RDATA)

$(OUTPUT)/%.RData : ex01_parameters.R ex01_simulations.R ex01_scenarios.R
	Rscript -e 'PATTERN="$*"; source("ex01_parameters.R"); source("ex01_simulation.R"); save.image(file = "$@")' > $(OUTPUT)/$*.log
