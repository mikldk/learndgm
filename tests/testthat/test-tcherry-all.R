context("Generate all k'th order t-cherry junction trees")

# test_that("2nd order t-cherry junction trees", {
#   ms <- all_tcherries(n = 4, k = 2, remove_duplicates = TRUE)
#   trees <- all_prufer_sequences(n = 4)
#   expect_equal(length(ms), nrow(trees))
#   
#   #for (n in 3:7) {
#   for (n in 3:6) {
#     #https://en.wikipedia.org/wiki/Cayley%27s_formula
#     ms <- all_tcherries(n = n, k = 2, remove_duplicates = TRUE)
#     trees <- all_prufer_sequences(n = n)
#     expect_equal(length(ms), nrow(trees), info = paste0("n = ", n))
#     expect_equal(length(ms), n^(n-2), info = paste0("n = ", n))
#   }
# })

test_that("2nd order t-cherry junction trees", {
  # https://oeis.org/search?q=1%2C+3%2C+16%2C+125%2C+1296%2C+15967&language=english&go=Search
  # Important up to 7, because that is where this is different from Cayley's formula
  expected_ns <- c("2" = 1, 
                   "3" = 3, 
                   "4" = 16,
                   "5" = 125,
                   "6" = 1296,
                   "7" = 15967)
  
  for (n in 2:7) {
    ms <- all_tcherries(n = n, k = 2, remove_duplicates = TRUE)
    expected_n <- expected_ns[[as.character(n)]]
    expect_equal(length(ms), expected_n, info = paste0("n = ", n))
  }
})

test_that("k'th order t-cherry junction trees with n = k", {
  for (n in 2:7) {
    ms <- all_tcherries(n = n, k = n, remove_duplicates = TRUE)
    expect_equal(length(ms), 1, info = paste0("n = ", n))
  }
})

test_that("all_tcherries(): comparing R and C++ version", {
  configs <- list(
    list(k = 2, ns = 3:5),
    list(k = 3, ns = 3:5)
  )
  
  for (config in configs) {
    for (n in config$ns) {
      ms_r <- all_tcherries_r(n, config$k)
      ms_cpp <- all_tcherries_cpp_pure(n, config$k, remove_duplicates = FALSE)
      expect_equal(length(ms_r), length(ms_cpp), info = paste0("raw: n = ", n, "; k = ", config$k))
      
      um_r <- remove_equal_models(ms_r)
      um_cpp <- remove_equal_models(ms_cpp)
      um_cpp2 <- all_tcherries_cpp_pure(n, config$k, remove_duplicates = TRUE)
      expect_equal(length(um_r), length(um_cpp), info = paste0("unique: n = ", n, "; k = ", config$k))
      expect_equal(length(um_r), length(um_cpp2), info = paste0("unique2: n = ", n, "; k = ", config$k))
    }
  }
})

test_that("all_tcherries(): gives correct number of models", {
  if (FALSE) {
    for (k in 2:5) {
      cat("# k = ", k, "\n", sep = "")
      r <- c()
      #for (n in 3:7) {
      #for (n in k:(k+4)) {
      for (n in k:(k+4)) {
        if (k > n) {
          next
        }
        ms <- all_tcherries(n = n, k = k)
        p <- length(ms)
        cat("list(k = ", k, ", n = ", n, ", expected_models = ", p, "), \n", sep = "")
        r <- c(r, p)
      }
      cat("# ", paste0(r, collapse = ", "), "\n", sep = "")
      cat("\n")
    }
  }
  
  if (FALSE) {
    k <- 2
    for (n in 2:8) {
      if (k > n) {
        next
      }
      ms <- all_tcherries(n = n, k = k)
      p <- length(ms)
      cat("list(k = ", k, ", n = ", n, ", expected_models = ", p, "), \n", sep = "")
      r <- c(r, p)
    }
    cat("# ", paste0(r, collapse = ", "), "\n", sep = "")
    cat("\n")
  }
  
  configs <- list(
    # k = 2
    list(k = 2, n = 2, expected_models = 1), 
    list(k = 2, n = 3, expected_models = 3), 
    list(k = 2, n = 4, expected_models = 16), 
    list(k = 2, n = 5, expected_models = 125), 
    list(k = 2, n = 6, expected_models = 1296),
    #list(k = 2, n = 7, expected_models = 15967), 
    # 1, 3, 16, 125, 1296, 15967 
    # https://oeis.org/search?q=1%2C+3%2C+16%2C+125%2C+1296%2C+15967&language=english&go=Search
    
    # k = 3
    list(k = 3, n = 3, expected_models = 1), 
    list(k = 3, n = 4, expected_models = 6), 
    list(k = 3, n = 5, expected_models = 70), 
    list(k = 3, n = 6, expected_models = 1095), 
    #list(k = 3, n = 7, expected_models = 21651), 
    # 1, 6, 70, 1095, 21651
    
    # k = 4
    list(k = 4, n = 4, expected_models = 1), 
    list(k = 4, n = 5, expected_models = 10), 
    list(k = 4, n = 6, expected_models = 200), 
    list(k = 4, n = 7, expected_models = 5075), 
    #list(k = 4, n = 8, expected_models = 157136), 
    # 1, 10, 200, 5075, 157136
    
    # k = 5
    list(k = 5, n = 5, expected_models = 1), 
    list(k = 5, n = 6, expected_models = 15), 
    list(k = 5, n = 7, expected_models = 455), 
    list(k = 5, n = 8, expected_models = 16870)#, 
    #list(k = 5, n = 9, expected_models = 743526),
    # 1, 15, 455, 16870, 743526
  )
  
  for (config in configs) {
    ms <- all_tcherries(n = config$n, k = config$k, remove_duplicates = TRUE)
    expect_equal(length(ms), config$expected_models, info = paste0("n = ", config$n, "; k = ", config$k))
  }
})