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
  # Construct adjacency matrices and check equality by hash
  
  return(remove_equal_models_r_strhash(models))
}


remove_equal_models_r_strhash <- function(models) {
  # Construct adjacency matrices and check equality by hash
  
  n <- length(models)
  
  if (n <= 1L) {
    return(models)
  }
  
  # Only final models
  stopifnot(is.null(models[[1L]]$unused))
  
  As <- lapply(models, model_to_adjacency_matrix)
  
  As_hash <- list()
  new_models <- list()
  
  for (i in 1L:n) {
    Ai <- As[[i]]
    Ai_hash <- paste0(Ai[upper.tri(Ai)], collapse = ";")
    
    As_hash[[ Ai_hash ]] <- Ai
    new_models[[ Ai_hash ]] <- models[[i]]
  }
  
  length(As)
  length(As_hash)
  
  names(new_models) <- NULL
  
  return(new_models)
}

remove_equal_intermediate_models_r_strhash <- function(models) {
  # Construct adjacency matrices and check equality by hash
  
  n <- length(models)
  
  if (n <= 1L) {
    return(models)
  }

  stopifnot(is.null(models[[1L]]$unused) == FALSE)
  
  # Adjecency matrix and unused_variables must be equal
  # FIXME: Separators?
  
  As <- lapply(models, model_to_adjacency_matrix)
  
  #As_hash <- list()
  new_models <- list()
  
  for (i in 1L:n) {
    Ai <- As[[i]]
    Ai_hash <- paste0(Ai[upper.tri(Ai)], collapse = ";")
    unused_hash <- paste0(models[[i]]$unused, collapse = ",")
    #print(unused_hash)
    
    model_hash <- paste0(Ai_hash, "/", unused_hash)
    #print(model_hash)
    
    #As_hash[[ Ai_hash ]] <- Ai
    new_models[[ model_hash ]] <- models[[i]]
  }
  
  #length(As)
  #length(As_hash)
  
  names(new_models) <- NULL
  
  return(new_models)
}


#' Generate all k'th order t-cherry junction trees
#'
#' @param n Number of variables
#' @param k Order of t-cherry junction tree (clique size)
#'
#' @examples all_tcherries(4, 2)
#'
#' @export
all_tcherries <- function(n, k, 
                          remove_duplicates = TRUE,
                          verbose = FALSE) {
  
  #m <- all_tcherries_r(n = n, k = k, verbose = verbose)
  #if (remove_duplicates) {
    #m <- remove_equal_models_r_strhash(m)
  #}
  
  return(all_tcherries_cpp_pure(n = n, k = k, 
                                remove_duplicates = remove_duplicates, 
                                verbose = verbose))
}

# @examples all_tcherries_cpp_pure(3, 2)
all_tcherries_cpp_pure <- function(n, k, 
                                   remove_duplicates = TRUE,
                                   verbose = FALSE) {
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
  
  models <- rcpp_new_all_tcherries_worker(
    models = models, 
    kmin1_subsets_idx = kmin1_subsets_idx, 
    n = n,
    n_unused = n_unused,
    verbose = verbose,
    remove_duplicates = remove_duplicates)
  
  # $unused entries removed in Rcpp
  
  return(models)
}

# @examples all_tcherries_r(3, 2)
all_tcherries_r <- function(n, k, verbose = FALSE) {
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
      
      for (i_unused in seq_along(m$unused)) {
        # i_unused <- 1L
        x_unused <- m$unused[i_unused]
        
        for (last_clique in m$cliques) {
          
          for (i_sep in seq_len(ncol(kmin1_subsets_idx))) {
            # i_sep <- 1L
            sep_idx <- kmin1_subsets_idx[, i_sep]
            sep <- last_clique[sep_idx]
            
            new_clique <- c(sep, x_unused)
            
            # sort for comparing (removing duplicates) later in:
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
    }
    
    # FIXME: Some of these models may be equivalant! Remove?
    # How to check equivalence now?
    
    #models <- new_models
    models <- remove_equal_intermediate_models_r_strhash(new_models)
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
  #stopifnot(is.null(model$unused))
  
  #n <- length( unique(unlist(model$cliques)) )
  n <- length( unique(c(unlist(model$cliques), model$unused) ))
  #print(n)
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



