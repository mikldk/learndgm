get_first_cherries <- function(A, triplets) {
  first_cherries <- list()
  
  #' 1. The first cherry of the t-cherry tree let be defined by any three vertices of the
  #'    spanning tree which are connected by two edges.
  # -> Find all triplets connected by two edges: first cherry
  for (j in seq_len(ncol(triplets))) {
    # j <- 1L
    vs <- triplets[, j]
    vs_A <- A[vs, vs]
    vs_no_edges <- sum(vs_A)
    
    if (vs_no_edges != 2L) {
      next
    }
    
    first_cherries[[length(first_cherries) + 1L]] <- vs
  }
  
  return(first_cherries)
}


add_cherry <- function(A, cherry) {
  m <- length(cherry) # 3L
  stopifnot(m == 3L) # order t-cherry junction tree
  stopifnot(all.equal(cherry, sort(cherry))) # combn() sorts...
  
  A_j <- A
  
  for (k1 in 1L:(m-1L)) {
    for (k2 in (k1+1):m) {
      # k1 < k2
      # and 
      # cherry[k1], cherry[k2]
      # Important as A/A_j is also stored in upper diagonal
      A_j[ cherry[k1], cherry[k2] ] <- 1
    }
  }
  
  return(A_j)
}

get_cherry_neighbours <- function(A, cherries) {
  nodes_in_cherry <- unique(unlist(cherries))
  
  A_edges <- which(A == 1L, arr.ind = TRUE)
  
  # either row or col must be in nodes_in_cherry, but
  # not both => 
  # XOR
  x <- !is.na(match(A_edges[, 1L], nodes_in_cherry))
  y <- !is.na(match(A_edges[, 2L], nodes_in_cherry))
  
  ns_cand <- as.integer(A_edges[xor(x, y), ])
  
  # Remove nodes that are already cherries
  ns <- setdiff(ns_cand, nodes_in_cherry)
  return(ns)
}


get_possible_new_cherries <- function(A, cherries, cherry_neighbour) {
  possible_cherries <- list()
  
  #' 1. The first cherry of the t-cherry tree let be defined by any three vertices of the
  #'    spanning tree which are connected by two edges.
  # -> Find all triplets connected by two edges: first cherry
  for (j in seq_len(ncol(triplets))) {
    # j <- 1L
    vs <- triplets[, j]
    vs_A <- A[vs, vs]
    vs_no_edges <- sum(vs_A)
    
    if (vs_no_edges != 2L) {
      next
    }
    
    first_cherries[[length(first_cherries) + 1L]] <- vs
  }
  
  return(first_cherries)
}

generate_starting_models_with_one_cherry <- function(n) {
  ps <- all_prufer_sequences(n = n)
  As <- lapply(seq_len(nrow(ps)), function(i) {
    prufer_to_adjacency_matrix(ps[i, ])
  })
  
  #Ts <- prufer_sequences_as_graphs(ps)
  
  #cand_As <- list()
  triplets <- combn(n, 3L)
  
  starting_models_with_one_cherry <- list()
  
  for (i in seq_along(As)) {
    # i <- 1L
    A <- As[[i]]
    
    #g <- Ts[[i]]
    # plot(g)
    
    first_cherries <- get_first_cherries(A, triplets)
    # first_cherries
    
    for (j in seq_along(first_cherries)) {
      first_cherry <- first_cherries[[j]]
      cherries <- list(first_cherry) 
      A_j <- add_cherry(A, first_cherry)
      #plot(graph_from_adjacency_matrix(A_j, mode = "undirected"))
      
      # Check if it already exists:
      already_included <- FALSE

      for (k in seq_along(starting_models_with_one_cherry)) {
        if (isTRUE(all.equal(starting_models_with_one_cherry[[k]]$A, A_j))) {
          already_included <- TRUE
          break
        }
      }
      
      if (already_included == FALSE) {
        starting_models_with_one_cherry[[length(starting_models_with_one_cherry) + 1L]] <- 
          list(A = A_j, cherries = cherries)
      }
    }
  }
  
  return(starting_models_with_one_cherry)
}

#' Generate all 3rd order t-cherry junction trees
#' 
#' @param variables Variables
#' 
#' @examples all_tcherry_3(c("A", "B", "C", "D"))
#' 
#' @export
all_tcherry_3 <- function(variables) {
  if (length(variables) <= 3L) {
    stop("Please provide at least four variables")
  }
  
  n <- length(variables)
  
  # From now on, work with variable indices instead of names
  
  #' Strategy:
  #' 1) Generate all possible spanning trees, T
  #' 2) For each T: Algorithm 2 from Kovacs and Szantai (2010) (chapter 3 of lecture notes).
  #'     1. The first cherry of the t-cherry tree let be defined by any three vertices of the
  #'        spanning tree which are connected by two edges.
  #'     2. We add a new cherry to the t-cherry tree by taking a new vertex of the spanning
  #'        tree adjacent to the so far constructed t-cherry tree.
  #'     3. We repeat step 2 till all vertices from the spanning tree become included in the
  #'        t-cherry tree.
  
  # Spanning tree made and first cherry selected
  start_models <- generate_starting_models_with_one_cherry(n)
  
  # Plotting
  if (FALSE) {
    if (!exists("f_plot_tmp")) {
      f_plot_tmp <- tempfile(fileext = ".pdf")
    }
    
    pdf(f_plot_tmp)
    for (i in seq_along(start_models)) {
      plot(graph_from_adjacency_matrix(start_models[[i]]$A, mode = "undirected"))#, layout = layout_as_tree)
    }
    dev.off()
    system(paste0("evince ", f_plot_tmp), wait = FALSE)
  }
  

}




#' #' Possible extensions
#' #' 
#' 
#' possible_extensions <- function(model, i) {
#'   #' Generate all possible ways that i
#'   #' can be added to the model keeping the model
#'   #' a t-cherry junction tree
#'   
#'   # model <- m
#'   # i <- 4
#'   cls <- model$cliques
#' }
#' 
#' #' Generate all 3rd order t-cherry junction trees
#' #' 
#' #' @param variables Variables
#' #' 
#' #' @examples all_tcherry_3(c("A", "B", "C", "D"))
#' #' 
#' #' @export
#' all_tcherry_3 <- function(variables) {
#'   if (length(variables) <= 3L) {
#'     stop("Please provide at least four variables")
#'   }
#'   
#'   n <- length(variables)
#'   
#'   # From now on, work with variable indices
#'   # instead of names
#'   
#'   # First take all triplets to start with:
#'   triplets <- combn(n, 3L)
#'   
#'   # All candidate models always have the same number of
#'   # unused variables (variables still not in the model)
#'   n_unused <- n - 3L
#'   
#'   # Use lists from now on... Slower, but easier...
#'   candidate_models <- lapply(seq_len(ncol(triplets)), 
#'                              function(i) { 
#'                                x <- triplets[, i]
#'                                list(cliques = list(x), 
#'                                     seps = list(),
#'                                     unused_vars = setdiff(seq_len(n), x))
#'                              })
#'   str(candidate_models, 3)
#'   
#'   while (n_unused > 0L) {
#'     for (i in seq_along(candidate_models)) {
#'       #i <- 1L
#'       m <- candidate_models[[i]]
#'       stopifnot(length(m$unused_vars) == n_unused)
#'       
#'       
#'     }
#'   }
#'   
#' }
#' 
