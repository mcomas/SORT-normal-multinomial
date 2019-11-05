L_N = 1000
L_n = 50 100 200 500
L_s = 1 
#2 3 4 5 6 7 8 9
L_seed = 1 2 3 4 5 6 7 8 9 10
L_method = dm nm-dm nm-maximum3

DATASETS = $(foreach seed,$(L_seed),$(foreach s,$(L_s),$(foreach n,$(L_n),$(foreach N,$(L_N),$(shell printf 'datasets/dataset-N_%05d-n_%05d-s_%05d-seed_%05d.RData' $(N) $(n) $(s) $(seed))))))

REPLACEMENT = $(foreach seed,$(L_seed),$(foreach s,$(L_s),$(foreach n,$(L_n),$(foreach N,$(L_N),$(foreach method,$(L_method),$(shell printf 'datasets/replacement-N_%05d-n_%05d-s_%05d-seed_%05d-method_%s.RData' $(N) $(n) $(s) $(seed) $(method)))))))

all : $(DATASETS) $(REPLACEMENT)
# figures/multinomial.pdf

datasets/dataset-%.RData : simulation_create_datasets.R scenarios.R
	Rscript	-e 'build = "$*"; source("$<")'

datasets/replacement-%-method_dm.RData : simulation_dm_replace.R $(DATASETS)
	Rscript	-e 'build = "$*"; source("$<")'

datasets/replacement-%-method_nm-dm.RData : simulation_nm-dm_replace.R $(DATASETS)
	Rscript	-e 'build = "$*"; init.method="dm"; source("$<")' > outputs/output_$*_method_nm-dm

datasets/replacement-%-method_nm-maximum3.RData : simulation_nm-maximum3_replace.R $(DATASETS)
	Rscript	-e 'build = "$*"; init.method="maximum3";  source("$<")' > outputs/output_$*_method_nm-maximum3

figures/multinomial.pdf : results.R $(REPLACEMENT)
	Rscript $<
