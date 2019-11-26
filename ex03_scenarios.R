params = list(
  function(N, n, len = 20) floor(N * seq(0.005, 0.025, length.out = len)[n]),
  function(N, n, len = 20) rep(seq(10, 200, length.out = len)[n], length(N)))