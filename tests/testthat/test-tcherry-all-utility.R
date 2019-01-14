context("t-cherry junction trees utilities")

test_that("mat_indices_to_vec_index()", {
  for (n in 2:6) {
    size_upper_tri <- n * (n - 1) / 2
    
    A <- matrix(NA, n, n)
    
    for (i1 in 1:(n - 1L)) {
      for (i2 in (i1 + 1):n) {
        # C++ based indexing starting at 0
        idx <- mat_indices_to_vec_index(i1 - 1L, i2 - 1L, n, size_upper_tri)
        A[i1, i2] <- idx
        
        #cat("(", i1, ", ", i2, ") = ", idx, "\n", sep = "")
      }
    }
    #print(A)
    
    # R's upper.tri() is by column; 
    # make it by row:
    AT <- t(A)
    entries_by_row <- AT[lower.tri(AT)]
    
    expect_equal(length(entries_by_row), size_upper_tri, info = paste0("n = ", n))
    expect_equal(entries_by_row, seq(0L, size_upper_tri - 1L), info = paste0("n = ", n))
  }
})
