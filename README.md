---
title: "Modelling count data using the logratio-normal-multinomial distribution"
author: Marc Comas-Cufí, Josep-Antoni Martín-Fernández, Glòria Mateu-Figueras, Javier Palarea-Albaladejo
---

This repository contains the scripts for the article Modelling count data using the logratio- normal-multinomial distribution published in SORT: Statistics and Operational Research Transactions

The R script `installing_packages.R` contains commands to install the packages used in the simulations. If you already have the correct versions of each package you don't need to execute this script.

* _unidimensional_example_: unidimensional example
* _comparison_: multidimensional examples
* _ex01_: Multinomial simulation
* _ex02_: Hardy-Weinberg simulation
* _ex03_: Catalan elections simulation


## Session info

```
> sessionInfo()
R version 3.6.1 (2019-07-05)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 19.10

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.8.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.8.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
 [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] tidyr_1.0.0          rstan_2.19.2         StanHeaders_2.19.0   randtoolbox_1.30.0  
 [5] rngWELL_0.10-5       microbenchmark_1.4-7 latex2exp_0.4.0      HardyWeinberg_1.6.3 
 [9] Rsolnp_1.16          mice_3.7.0           lattice_0.20-38      egg_0.4.5           
[13] ggplot2_3.2.1        gridExtra_2.3        dplyr_0.8.3          dirmult_0.1.3-4     
[17] digest_0.6.23        data.table_1.12.2    coda.count_0.2.1     coda.base_0.2.1     

loaded via a namespace (and not attached):
 [1] tidyselect_0.2.5   purrr_0.3.2        splines_3.6.1      colorspace_1.4-1   vctrs_0.2.0       
 [6] generics_0.0.2     stats4_3.6.1       loo_2.2.0          yaml_2.2.0         pkgbuild_1.0.6    
[11] pan_1.6            survival_2.44-1.1  rlang_0.4.0        jomo_2.6-10        pillar_1.4.2      
[16] nloptr_1.2.1       glue_1.3.1         withr_2.1.2        matrixStats_0.55.0 lifecycle_0.1.0   
[21] stringr_1.4.0      munsell_0.5.0      gtable_0.3.0       inline_0.3.15      callr_3.4.0       
[26] ps_1.3.0           parallel_3.6.1     broom_0.5.3        Rcpp_1.0.3         backports_1.1.4   
[31] scales_1.1.0       truncnorm_1.0-8    lme4_1.1-21        stringi_1.4.3      processx_3.4.1    
[36] grid_3.6.1         cli_1.1.0          tools_3.6.1        magrittr_1.5       lazyeval_0.2.2    
[41] tibble_2.1.3       crayon_1.3.4       pkgconfig_2.0.2    zeallot_0.1.0      MASS_7.3-51.4     
[46] Matrix_1.2-17      prettyunits_1.0.2  assertthat_0.2.1   minqa_1.2.4        mitml_0.3-7       
[51] R6_2.4.0           boot_1.3-23        rpart_4.1-15       nnet_7.3-12        nlme_3.1-141      
[56] compiler_3.6.1      
```
