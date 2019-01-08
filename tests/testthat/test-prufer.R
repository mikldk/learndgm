context("Prüfer")

test_that("Input validation", {
  expect_error(prufer_to_adjacency_matrix(NULL))
  expect_error(prufer_to_adjacency_matrix(c())) # is.null(c()) == TRUE
  
  # <= 0 invalid
  expect_error(prufer_to_adjacency_matrix(c(0)))
  expect_error(prufer_to_adjacency_matrix(c(-1)))
  expect_error(prufer_to_adjacency_matrix(c(1, 2, 0)))
  
  
  # > length(s) + 2 invalid
  
  # length(s) = 0; no entries to be invalid
  
  # length(s) = 1; n = length(s) + 2 s.t. > 3 invalid
  expect_error(prufer_to_adjacency_matrix(c(4)))
  
  # length(s) = 2; n = length(s) + 2 s.t. > 4 invalid
  expect_error(prufer_to_adjacency_matrix(c(3, 5)))
  expect_true(is.matrix(prufer_to_adjacency_matrix(c(4, 4))))
  
  # length(s) = 3; n = length(s) + 2 s.t. > 5 invalid
  expect_error(prufer_to_adjacency_matrix(c(3, 6, 2)))
  expect_true(is.matrix(prufer_to_adjacency_matrix(c(5, 5, 5))))
})


# http://mathworld.wolfram.com/PrueferCode.html
test_that("{} (empty)", {
  Aprufer <- prufer_to_adjacency_matrix(integer(0))
  
  A <- matrix(0, nrow = 2, ncol = 2)
  A[1, 2] <- 1

  expect_equal(Aprufer, A)
})

# http://mathworld.wolfram.com/PrueferCode.html
test_that("{1}", {
  Aprufer <- prufer_to_adjacency_matrix(1)
  
  A <- matrix(0, nrow = 3, ncol = 3)
  A[1, 2] <- A[1, 3] <- 1
  
  expect_equal(Aprufer, A)
})

# http://mathworld.wolfram.com/PrueferCode.html
test_that("{1, 2, 1, 3, 3, 5}", {
  Aprufer <- prufer_to_adjacency_matrix(c(1, 2, 1, 3, 3, 5))
  
  if (FALSE) {
    library(igraph)
    Aprufer <- prufer_to_adjacency_matrix(c(1, 2, 1, 3, 3, 5))
    g <- graph_from_adjacency_matrix(Aprufer, mode = "undirected")
    g
    plot(g)
  }
  
  A <- matrix(0, nrow = 8, ncol = 8)
  A[1, 2] <- A[1, 3] <- A[1, 4] <- 1
  A[2, 6] <- 1
  A[3, 5] <- A[3, 7] <- 1
  A[5, 8] <- 1
  
  expect_equal(Aprufer, A)
})




# https://en.wikipedia.org/wiki/Pr%C3%BCfer_sequence
test_that("{4, 4, 4, 5}", {
  Aprufer <- prufer_to_adjacency_matrix(c(4, 4, 4, 5))
  
  if (FALSE) {
    library(igraph)
    g <- graph_from_adjacency_matrix(Aprufer, mode = "undirected")
    g
    plot(g)
  }
  
  A <- matrix(0, nrow = 6, ncol = 6)
  A[1, 4] <- 1
  A[2, 4] <- 1
  A[3, 4] <- 1
  A[4, 5] <- 1
  A[5, 6] <- 1
  
  expect_equal(Aprufer, A)
})


test_that("all_prufer_sequences", {
  expect_error(all_prufer_sequences(1))
  expect_error(all_prufer_sequences(2))
  
  expect_equal(all_prufer_sequences(3), 
               matrix(1:3, ncol = 1))
  
  expect_equal(all_prufer_sequences(4), 
               matrix(unlist(expand.grid(1:4, 1:4)), ncol = 2))
  
  expect_equal(all_prufer_sequences(5), 
               matrix(unlist(expand.grid(1:5, 1:5, 1:5)), ncol = 3))
})

