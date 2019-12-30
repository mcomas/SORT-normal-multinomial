load('ex01-multinomial.RData')
knitr::kable(ex01_times[,c(1,2,4,3)], format = 'latex')

load('ex02-hardyweinberg.RData')
knitr::kable(ex02_times[,c(1,2,4,3)], format = 'latex')

load('ex03-parliament.RData')
knitr::kable(ex03_times[,c(1,2,4,3)], format = 'latex')
