params = list(
  function(nm) runif(nm, min= 0,  max = 1),
  function(nm) runif(nm, min= 0,  max = 0.5),
  function(nm) runif(nm, min= 0,  max = 0.25),
  function(nm) runif(nm, min= 0.25,  max = 0.5),
  function(nm) boot::inv.logit(rnorm(nm, mean = 0, sd = 1)),
  function(nm) boot::inv.logit(rnorm(nm, mean = 1, sd = 1)))