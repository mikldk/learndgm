#' Generate all Prüfer sequences
#' 
#' @param n Number of variables, n >= 3 (n = 2 is the empty Prüfer sequence)
#' 
#' @return A matrix with n^(n-2) rows and each row is a Prüfer sequence
#' 
#' @examples
#' all_prufer_sequences(3)
#' 
#' @export
all_prufer_sequences <- function(n) {
  require_single_integer(n, "n")
  n <- as.integer(n)
  
  if (n <= 2) {
    stop("n >= 3 required (as stated in the manual)")
  }
  
  vars <- seq_len(n)
  
  # All spanning trees (Chow-Liu candidates)
  vs <- lapply(seq_len(n-2), function(ignore) vars)
  ts <- do.call(expand.grid, vs)
  stopifnot(ncol(ts) == n - 2L)
  stopifnot(nrow(ts) == n^(n-2))
  
  ts <- as.matrix(ts)
  dimnames(ts) <- NULL
  
  return(ts)
}

#' Convert Prüfer sequences in matrix rows to graphs
#' 
#' @return List with graphs
#' 
#' @importFrom igraph graph_from_adjacency_matrix
#' 
#' @export
prufer_sequences_as_graphs <- function(s) {
  
  ret <- vector("list", nrow(s))
  
  for (i in 1L:nrow(s)) {
    A <- prufer_to_adjacency_matrix(s[i, ])
    ret[[i]] <- igraph::graph_from_adjacency_matrix(A, mode = "undirected")
  }
  
  return(ret)
}


