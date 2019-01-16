context("Generate all k'th order t-cherry junction trees")

test_that("2nd order t-cherry junction trees: R/C++", {
  #for (n in 3:7) {
  for (n in 3:5) {
    #https://en.wikipedia.org/wiki/Cayley%27s_formula
    #ms <- all_tcherries(n = n, k = 2)
    ms_r <- .remove_equal_models_r_strhash(.all_tcherries_r(n = n, k = 2))
    ms_cpp <- all_tcherries_cpp_pure(n = n, k = 2)
    trees <- all_prufer_sequences(n = n)
    expect_equal(length(ms_r$models), length(ms_cpp$models), info = paste0("n = ", n))
    expect_equal(length(ms_r$models), nrow(trees), info = paste0("n = ", n))
    expect_equal(length(ms_r$models), n^(n-2), info = paste0("n = ", n))
  }
})

test_that("cliques/parents/seps", {
  for (k in 2:4) {
    for (n in (k+1):(k+2)) { 
      ms_r <- .remove_equal_models_r_strhash(.all_tcherries_r(n = n, k = k))
      ms_cpp <- all_tcherries_cpp_pure(n = n, k = k)
      
      expect_equal(length(ms_r$models), length(ms_cpp$models), 
                   info = paste0("n = ", n, "; k = ", k))
      
      for (i in seq_along(ms_r$models)) {
        inf_txt <- paste0("r: n = ", n, "; k = ", k, ", i = ", i)
        
        # Cliques
        r_k_tmp <- unique(unlist(lapply(ms_r$models[[i]]$cliques, length)))
        cpp_k_tmp <- nrow(ms_cpp$models[[i]]$cliques)
        expect_equal(length(r_k_tmp), 1, info = inf_txt)
        expect_equal(r_k_tmp, k, info = inf_txt)
        expect_equal(cpp_k_tmp, k, info = inf_txt)
        
        # Seps
        r_km1_tmp <- unique(unlist(lapply(ms_r$models[[i]]$seps, length)))
        cpp_km1_tmp <- nrow(ms_cpp$models[[i]]$seps)
        expect_equal(length(r_km1_tmp), 1, info = inf_txt)
        expect_equal(r_km1_tmp, k-1L, info = inf_txt)
        expect_equal(cpp_km1_tmp, k-1L, info = inf_txt)
      }
    }    
  }
})

test_that("2nd order t-cherry junction trees: C++", {
  # 7 is important: here number of trees become different from caterpillar trees
  for (n in 3:7) { 
    ms_std <- all_tcherries(n = n, k = 2)
    ms_cpp <- all_tcherries_cpp_pure(n = n, k = 2)
    trees <- all_prufer_sequences(n = n)
    expect_equal(length(ms_std$models), length(ms_cpp$models), info = paste0("n = ", n))
    expect_equal(length(ms_cpp$models), nrow(trees), info = paste0("n = ", n))
    expect_equal(length(ms_cpp$models), n^(n-2), info = paste0("n = ", n))
  }
})

test_that("k'th order t-cherry junction trees with n = k", {
  for (n in 2:7) {
    ms <- all_tcherries(n = n, k = n)
    expect_equal(length(ms$models), 1, info = paste0("n = ", n))
  }
})

test_that("all_tcherries(): comparing R and C++ version", {
  configs <- list(
    list(k = 2, ns = 3:5),
    list(k = 3, ns = 3:5)
  )
  
  for (config in configs) {
    for (n in config$ns) {
      ms_r <- .remove_equal_models(.all_tcherries_r(n, config$k))
      ms_cpp <- all_tcherries_cpp_pure(n, config$k)
      expect_equal(length(ms_r$models), length(ms_cpp$models), 
                   info = paste0("raw: n = ", n, "; k = ", config$k))
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
      ns <- k:(k+4)
      
      if (k == 2L) {
        ns <- 2L:8L
      }
      
      for (n in ns) {
        if (k > n) {
          next
        }
        ms <- all_tcherries(n = n, k = k)
        p <- length(ms$models)
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
      p <- length(ms$models)
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
    list(k = 2, n = 7, expected_models = 16807),
    #list(k = 2, n = 8, expected_models = 262144), 
    # 1, 3, 16, 125, 1296, 16807, 262144
    
    # k = 3
    list(k = 3, n = 3, expected_models = 1), 
    list(k = 3, n = 4, expected_models = 6), 
    list(k = 3, n = 5, expected_models = 70), 
    list(k = 3, n = 6, expected_models = 1215), 
    list(k = 3, n = 7, expected_models = 27951), 
    # 1, 6, 70, 1215, 27951
    
    # k = 4
    list(k = 4, n = 4, expected_models = 1), 
    list(k = 4, n = 5, expected_models = 10), 
    list(k = 4, n = 6, expected_models = 200), 
    list(k = 4, n = 7, expected_models = 5915), 
    #list(k = 4, n = 8, expected_models = 229376), 
    # 1, 10, 200, 5915, 229376
    
    # k = 5
    list(k = 5, n = 5, expected_models = 1), 
    list(k = 5, n = 6, expected_models = 15), 
    list(k = 5, n = 7, expected_models = 455), 
    list(k = 5, n = 8, expected_models = 20230)
    #list(k = 5, n = 9, expected_models = 1166886)
    # 1, 15, 455, 20230, 1166886
  )
  
  for (config in configs) {
    ms <- all_tcherries(n = config$n, k = config$k)
    expect_equal(length(ms$models), config$expected_models, 
                 info = paste0("n = ", config$n, "; k = ", config$k))
  }
})
