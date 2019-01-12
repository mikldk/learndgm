#' Generate all k'th order t-cherry junction trees
#'
#' @param variables Variables
#' @param k Order of t-cherry junction tree (clique size)
#'
#' @examples all_tcherries(c("A", "B", "C", "D"), 2)
#'
#' @export
all_tcherries <- function(variables, k) {
  if (length(variables) <= 1L) {
    stop("Please provide at least two variables")
  }
  
  require_single_integer(k)
  k <- as.integer(k)
  
  n <- length(variables)
  var_idx <- seq_len(n)

  # From now on, work with variable indices
  # instead of names

  # Initial cliques
  first_cliques_mat <- combn(n, k)
  models <- lapply(seq_len(ncol(first_cliques_mat)), 
                   function(i) {
                     x <- first_cliques_mat[, i]
                     # NOTE: x is sorted by design!
                    
                     list(
                       cliques = list(x),
                       seps = list(),
                       unused = setdiff(var_idx, x)
                     )
                   })
  
  #str(models, 3)
  
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
  
  for (iter in seq_len(n_unused)) {
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
          
          # sort for easier comparing (removing duplicates) later
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
    
    # str(new_models, 3)
    
    # FIXME: Some of these models may be equivalant! Remove?

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
  vars <- sort(unique(unlist(model$cliques)))
  n <- length(vars)
  A <- matrix(0L, nrow = n, ncol = n)
  
  for (i in seq_along(model$cliques)) {
    clique <- model$cliques[[ i ]]
    k <- length(clique)
    
    for (j1 in seq_len(k - 1L)) {
      for (j2 in (j1 + 1L):k) {
        A[ clique[j1], clique[j2] ] <- 1L
      }
    }
  }
  
  return(A)
}


#' Remove entries that give the same model
#' 
#' Currently by constructing adjacency matrix and 
#' checking equality
#' 
#' @param models List of models, e.g. from [all_tcherries()]
#' 
#' @examples 
#' m <- all_tcherries(c("A", "B", "C", "D"), 2)
#' remove_equal_models(m)
#' 
#' @export
remove_equal_models <- function(models) {
  #' Construct adjacency matrices and check equality
  
  n <- length(models)
  As <- vector("list", n)
  
  for (i in seq_len(n)) {
    
  }
  
}

