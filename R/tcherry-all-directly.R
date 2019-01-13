#' Remove entries that give the same model
#' 
#' Currently by constructing adjacency matrix and 
#' checking equality.
#' 
#' Only models that are finished.
#' 
#' @param models List of models, e.g. from [all_tcherries()]
#' 
#' @examples 
#' m <- all_tcherries(4, 2)
#' remove_equal_models(m)
#' 
#' @export
remove_equal_models <- function(models) {
  #' Construct adjacency matrices and check equality by hash
  
  #return(remove_equal_models___rcpp(models))
  return(remove_equal_models___old_implementation_strhash(models))
}

remove_equal_models___rcpp <- function(models) {
  #' Construct adjacency matrices and check equality by hash
  
  n <- length(models)
  
  if (n <= 1L) {
    return(models)
  }
  
  # Only final models
  stopifnot(is.null(models[[1L]]$unused))
  
  As <- lapply(models, model_to_adjacency_matrix)
  As_uppertri <- lapply(models, function(m) {
    A <- model_to_adjacency_matrix(m)
    A[upper.tri(A)]
  })
  
  indices_to_keep <- rcpp_remove_equal_models_worker(As_uppertri)
  new_models <- models[indices_to_keep]
  
  return(new_models)
}

remove_equal_models___old_implementation_strhash <- function(models) {
  #' Construct adjacency matrices and check equality by hash
  
  n <- length(models)
  
  if (n <= 1L) {
    return(models)
  }
  
  # Only final models
  stopifnot(is.null(models[[1L]]$unused))
  
  As <- lapply(models, model_to_adjacency_matrix)
  
  As_hash <- list()
  new_models <- list()
  
  for (i in 2L:n) {
    Ai <- As[[i]]
    Ai_hash <- paste0(Ai[upper.tri(Ai)], collapse = ";")
    
    As_hash[[ Ai_hash ]] <- Ai
    new_models[[ Ai_hash ]] <- models[[i]]
  }
  
  length(As)
  length(As_hash)
  
  return(new_models)
}

remove_equal_models___old_implementation_n2 <- function(models) {
  #' Construct adjacency matrices and check equality
  
  # FIXME: All binary n x n matrices (symmetric/upper triangular).
  #        Nice hash function?
  
  n <- length(models)
  
  if (n <= 1L) {
    return(models)
  }
  
  # Only final models
  stopifnot(is.null(models[[1L]]$unused))
  
  As <- lapply(models, model_to_adjacency_matrix)
  
  seen_before <- c()
  
  # n >= 2L, cf. above
  for (i in 2L:n) {
    Ai <- As[[i]]
    
    for (j in 1L:(i-1L)) {
      if (isTRUE(all.equal(Ai, As[[j]]))) {
        seen_before <- c(seen_before, i)
        break
      }
    }
  }
  
  if (length(seen_before) > 0L) {
    models <- models[-seen_before]
  }
  
  return(models)
}

#' Generate all k'th order t-cherry junction trees
#'
#' @param n Number of variables
#' @param k Order of t-cherry junction tree (clique size)
#'
#' @examples all_tcherries(4, 2)
#'
#' @export
all_tcherries <- function(n, k, verbose = FALSE) {
  return(all_tcherries__r(n = n, k = k, verbose = verbose))
}

#' @examples all_tcherries__cpp(3, 2)
all_tcherries__cpp <- function(n, k, verbose = FALSE) {
  require_single_integer(n)
  n <- as.integer(n)
  
  if (n <= 1L) {
    stop("Please provide at least two variables, i.e. n >= 2")
  }
  
  require_single_integer(k)
  k <- as.integer(k)
  
  if (k > n) {
    stop("k > n")
  }
  
  var_idx <- seq_len(n)
  
  # Initial cliques
  first_cliques_mat <- combn(n, k)
  models <- lapply(seq_len(ncol(first_cliques_mat)), 
                   function(i) {
                     x <- first_cliques_mat[, i]
                     
                     list(
                       cliques = list(x),
                       seps = list(),
                       unused = setdiff(var_idx, x)
                     )
                   })
  
  #str(models, 3)
  
  if (verbose) {
    cat("All initial ", ncol(first_cliques_mat), " cliques created.\n", sep = "")
  }
  
  #' Now, extend by this strategy:
  #' 
  #' With RIP (running intersection property) in mind,
  #' Take last clique of size k and take all (k-1) subsets
  #' together with each of of the unused variables, hence
  #' there are choose(k, k-1)*length(unused) new possibilities
  kmin1_subsets_idx <- combn(k, k - 1L)
  
  # All candidate models always have the same number of
  # unused variables (variables still not in the model)
  n_unused <- n - k
  
  if (verbose) {
    cat("Unused variables that needs to be added = ", n_unused, ".\n", sep = "")
  }
  
  models <- rcpp_all_tcherries_worker(models = models, 
                                      kmin1_subsets_idx = kmin1_subsets_idx, 
                                      n_unused = n_unused,
                                      verbose = verbose)
  
  # FIXME: Remove $unused entries?
  for (i_m in seq_along(models)) {
    stopifnot( length(models[[i_m]]$unused) == 0L )
    models[[i_m]]$unused <- NULL
  }
  
  return(models)
}

#' @examples all_tcherries__r(3, 2)
all_tcherries__r <- function(n, k, verbose = FALSE) {
  require_single_integer(n)
  n <- as.integer(n)
  
  if (n <= 1L) {
    stop("Please provide at least two variables, i.e. n >= 2")
  }
  
  require_single_integer(k)
  k <- as.integer(k)
  
  if (k > n) {
    stop("k > n")
  }
  
  var_idx <- seq_len(n)
  
  # Initial cliques
  first_cliques_mat <- combn(n, k)
  models <- lapply(seq_len(ncol(first_cliques_mat)), 
                   function(i) {
                     x <- first_cliques_mat[, i]
                     # NOTE: x is sorted by design of combn!
                     
                     list(
                       cliques = list(x),
                       seps = list(),
                       unused = setdiff(var_idx, x)
                     )
                   })
  
  #str(models, 3)
  
  if (verbose) {
    cat("All initial ", ncol(first_cliques_mat), " cliques created.\n", sep = "")
  }
  
  #' Now, extend by this strategy:
  #' 
  #' With RIP (running intersection property) in mind,
  #' Take last clique of size k and take all (k-1) subsets
  #' together with each of of the unused variables, hence
  #' there are choose(k, k-1)*length(unused) new possibilities
  kmin1_subsets_idx <- combn(k, k - 1L)
  
  # All candidate models always have the same number of
  # unused variables (variables still not in the model)
  n_unused <- n - k
  
  if (verbose) {
    cat("Unused variables that needs to be added = ", n_unused, ".\n", sep = "")
  }
  
  for (iter in seq_len(n_unused)) {
    if (verbose) {
      cat("Adding unused variable #", iter, ".\n", sep = "")
    }
    
    new_models <- list()
    
    for (i_m in seq_along(models)) {
      #i_m <- 1L
      m <- models[[i_m]]
      
      last_clique <- m$cliques[[ length(m$cliques) ]]
      
      for (i_unused in seq_along(m$unused)) {
        # i_unused <- 1L
        x_unused <- m$unused[i_unused]
        
        for (i_sep in seq_len(ncol(kmin1_subsets_idx))) {
          # i_sep <- 1L
          sep_idx <- kmin1_subsets_idx[, i_sep]
          sep <- last_clique[sep_idx]
          
          new_clique <- c(sep, x_unused)
          
          # sort for easier comparing (removing duplicates) later in:
          # model_to_adjacency_matrix(): A[ clique[j1], clique[j2] ]
          new_clique <- sort(new_clique)
          sep <- sort(sep)
          
          new_model <- m
          new_model$cliques[[ length(new_model$cliques) + 1L ]] <- 
            new_clique
          new_model$seps[[ length(new_model$seps) + 1L ]] <- 
            sep
          new_model$unused <- m$unused[-i_unused] # setdiff(m$unused, x_unused)
          
          new_models[[ length(new_models) + 1L ]] <- new_model
        }
      }
    }
    
    # FIXME: Some of these models may be equivalant! Remove?
    # How to check equivalence now?
    
    models <- new_models
  }
  
  # FIXME: Remove $unused entries?
  for (i_m in seq_along(models)) {
    stopifnot( length(models[[i_m]]$unused) == 0L )
    models[[i_m]]$unused <- NULL
  }
  
  return(models)
}


#' Model to adjacency matrix
#' 
#' @examples 
#' m <- all_tcherries(c("A", "B", "C", "D"), 2)
#' model_to_adjacency_matrix( m[[1]] )
#' model_to_adjacency_matrix( m[[2]] )
#' 
#' @export
model_to_adjacency_matrix <- function(model) {
  # Only final models
  stopifnot(is.null(model$unused))
  
  n <- length( unique(unlist(model$cliques)) )
  A <- matrix(0L, nrow = n, ncol = n)
  
  for (i in seq_along(model$cliques)) {
    clique <- model$cliques[[ i ]]
    k <- length(clique)
    
    for (j1 in seq_len(k - 1L)) {
      for (j2 in (j1 + 1L):k) {
        
        # Sorted: Important for A[ clique[j1], clique[j2] ]
        A[ clique[j1], clique[j2] ] <- 1L
      }
    }
  }
  
  return(A)
}



