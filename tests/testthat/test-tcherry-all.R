context("Generate all k'th order t-cherry junction trees")

test_that("2nd order t-cherry junction trees", {
  ms <- remove_equal_models(all_tcherries(4, 2))
  trees <- all_prufer_sequences(4)
  
  expect_equal(length(ms), nrow(trees))
  
  for (n in 3:5) {
    ms <- remove_equal_models(all_tcherries(n, 2))
    trees <- all_prufer_sequences(n)
    expect_equal(length(ms), nrow(trees), info = paste0("n = ", n))
    expect_equal(length(ms), n^(n-2), info = paste0("n = ", n))
  }
})

test_that("all_tcherries(): R/C++", {
  configs <- list(
    list(k = 2, ns = 3:5),
    list(k = 3, ns = 3:5)
  )
  
  for (config in configs) {
    for (n in config$ns) {
      ms_r <- all_tcherries__r(n, config$k)
      ms_cpp <- all_tcherries__cpp(n, config$k)
      expect_equal(length(ms_r), length(ms_cpp), info = paste0("raw: n = ", n, "; k = ", config$k))
      
      um_r <- remove_equal_models(ms_r)
      um_cpp <- remove_equal_models(ms_cpp)
      expect_equal(length(um_r), length(um_cpp), info = paste0("unique: n = ", n, "; k = ", config$k))
    }
  }
})

if (FALSE) {
  ms <- all_tcherries(n = 5, k = 3)
  length(ms)
  
  length(remove_equal_models___old_implementation_n2(ms))
  length(remove_equal_models___old_implementation_strhash(ms))
  length(remove_equal_models___rcpp(ms))

  microbenchmark::microbenchmark(
    r_n2_comp = remove_equal_models___old_implementation_n2(ms),
    r_strhash = remove_equal_models___old_implementation_strhash(ms),
    cpp_intvechash = remove_equal_models___rcpp(ms),
    times = 10
  )
  
  ##############
  
  ms <- all_tcherries(n = 6, k = 3)
  length(remove_equal_models___old_implementation_strhash(ms))
  length(remove_equal_models___rcpp(ms))
  
  microbenchmark::microbenchmark(
    r_strhash = remove_equal_models___old_implementation_strhash(ms),
    cpp_intvechash = remove_equal_models___rcpp(ms),
    times = 10
  )
  
  ##############
  
  ms <- all_tcherries(n = 7, k = 3)
  length(ms)
  
  length(remove_equal_models___old_implementation_strhash(ms))
  length(remove_equal_models___rcpp(ms))
  
  microbenchmark::microbenchmark(
    r_strhash = remove_equal_models___old_implementation_strhash(ms),
    cpp_intvechash = remove_equal_models___rcpp(ms),
    times = 1
  )
}

if (FALSE) {
  r <- c()
  for (n in 3:8) {
    p <- n^(n-2)
    cat("k = 2; n = ", n, "; # models = ", p, "\n")
    r <- c(r, p)
  }
  paste0(r, collapse = ", ")
  
  #' k = 2:
  #' n^(n-2):
  #' k = 2; n =  3 ; # models =  3 
  #' k = 2; n =  4 ; # models =  16 
  #' k = 2; n =  5 ; # models =  125 
  #' k = 2; n =  6 ; # models =  1296 
  #' k = 2; n =  7 ; # models =  16807 
  #' k = 2; n =  8 ; # models =  262144 
  #' 3, 16, 125, 1296, 16807, 262144
  
  for (k in 3:4) {
  #for (k in 4) {
    r <- c()
    for (n in 3:7) {
      if (k > n) {
        next
      }
      #ms <- remove_equal_models(all_tcherries(n = n, k = k))
      ms <- all_tcherries(n = n, k = k)
      ms <- remove_equal_models(ms)
      p <- length(ms)
      cat("k = ", k, "; n = ", n, "; # models = ", p, "\n")
      r <- c(r, p)
    }
    print(paste0(r, collapse = ", "))
  }
  
  #' k =  3
  #' k =  3 ; n =  3 ; # models =  1 
  #' k =  3 ; n =  4 ; # models =  6 
  #' k =  3 ; n =  5 ; # models =  70 
  #' k =  3 ; n =  6 ; # models =  1095
  #' k =  3 ; n =  7 ; # models =  21651
  #' 1, 6, 70, 1095, 21651
  #' https://oeis.org found none
  
  length(remove_equal_models(all_tcherries(n = 7, k = 4)))
  length(remove_equal_models(all_tcherries(n = 8, k = 4)))
  #' k =  4:
  #' k =  4 ; n =  4 ; # models =  1 
  #' k =  4 ; n =  5 ; # models =  10 
  #' k =  4 ; n =  6 ; # models =  200 
  #' k =  4 ; n =  7 ; # models =  5075 
  #' 1, 10, 200, 5075
  #' https://oeis.org found none
  #' 
  
  # k =  3 ; n =  3 ; # models =  1 
  # k =  3 ; n =  4 ; # models =  8 
  # k =  3 ; n =  5 ; # models =  76 
  # k =  3 ; n =  6 ; # models =  881 
  # k =  3 ; n =  7 ; # models =  11822 
  # [1] "1, 8, 76, 881, 11822"
  # k =  4 ; n =  4 ; # models =  1 
  # k =  4 ; n =  5 ; # models =  13 
  # k =  4 ; n =  6 ; # models =  203 
  # k =  4 ; n =  7 ; # models =  3847 
  # [1] "1, 13, 203, 3847"
  
  
  ms <- all_tcherries(n = 8, k = 3)
  length(ms)
  ms <- all_tcherries(n = 8, k = 4)
  length(ms)
  
  
  Rprof()
  m <- all_tcherries(n = 7, k = 3, verbose = TRUE)
  length(m)
  m <- remove_equal_models(m)
  length(m)
  Rprof(NULL)
  summaryRprof()$by.self
  
  
  Rprof()
  m <- all_tcherries(n = 7, k = 3, verbose = TRUE)
  length(m)
  #m <- remove_equal_models(m)
  #length(m)
  Rprof(NULL)
  summaryRprof()$by.self
}

if (FALSE) {
  #remotes::install_github("r-prof/jointprof")
  
  library(jointprof)
  out_file <- tempfile("jointprof", fileext = ".out")
  start_profiler(out_file)
  
  ms_cpp <- all_tcherries__cpp(n = 6, k = 3)

  profile_data <- stop_profiler()
  
  summary <- summaryRprof(out_file)
  summary$by.self
}

if (FALSE) {
  ms_r <- all_tcherries__r(n = 6, k = 3)
  ms_cpp <- all_tcherries__cpp(n = 6, k = 3)
  
  length(ms_r)
  length(ms_cpp)
  
  length(remove_equal_models(ms_r))
  length(remove_equal_models(ms_cpp))
  
  microbenchmark::microbenchmark(
    all_tcherries__r(n = 6, k = 3),
    all_tcherries__cpp(n = 6, k = 3),
    times = 10
  )
}

