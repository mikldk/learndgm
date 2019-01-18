# https://oeis.org/A036506
# Number of labeled k-trees on n nodes is binomial(n, k) * (k(n-k)+1)^(n-k-2).
expected_num_cherrytrees <- function(k, n) {
  k <- k - 1 # k'th order t-cherry is (k-1)-tree
  choose(n, k)*(k*(n-k)+1)^(n-k-2)
}
#expected_num_cherrytrees(k = 5, n = 7)

if (FALSE) {
  d <- NULL
  
  for (k in 2L:5L) {
    n <- k
    
    repeat {
      exp_n <- expected_num_cherrytrees(k = k, n = n)
      d <- rbind(d, data.frame(k = k, n = n, no_models = exp_n))
      
      if (exp_n > 1e7) {
        break
      }
      
      n <- n + 1L
    }
  }
  
  library(ggplot2)
  ggplot(d, aes(n, no_models, color = factor(k))) +
    geom_point() + 
    geom_line()
}

if (FALSE) {
  d <- NULL
  
  for (k in 2L:5L) {
    n <- k
    
    repeat {
      exp_n <- expected_num_cherrytrees(k = k, n = n)

      if (exp_n > 1e6) {
        break
      }
      
      tm <- system.time(ms <- all_tcherries(n = n, k = k))
      obs_n <- length(ms$models)
      
      d <- rbind(d, data.frame(k = k, n = n, no_models = exp_n, time_sec = as.numeric(tm["user.self"])))
      
      n <- n + 1L
    }
  }
  
  print.data.frame(d, row.names = FALSE)
}