#' 
#' # R -e "devtools::load_all('/home/mikl/gits/software/learndgm'); all_tcherries__cpp_new(n = 6, k = 3, TRUE)"
#' # R -d gdb -e "devtools::load_all('.'); all_tcherries__cpp_new(n = 6, k = 3, TRUE)"
#' # R -d valgrind -e "devtools::load_all('.'); all_tcherries__cpp_new(n = 6, k = 3, TRUE)"
#' # 
#' # R -e "devtools::load_all('/home/mikl/gits/software/learndgm'); all_tcherries__cpp(3, 2, TRUE)"
#' # R -d gdb -e "devtools::load_all('/home/mikl/gits/software/learndgm'); all_tcherries__cpp(3, 2, TRUE)"
#' # R -d valgrind -e "devtools::load_all('/home/mikl/gits/software/learndgm'); all_tcherries__cpp(3, 2, TRUE)"
#' # R -d lldb
#' # devtools::load_all('/home/mikl/gits/software/learndgm'); all_tcherries__cpp(3, 2, TRUE)
#' # 
#' # R -d "valgrind --tool=callgrind" -e "devtools::load_all('/home/mikl/gits/software/learndgm'); all_tcherries__cpp(6, 3, TRUE)"
#' 
#' 
#' if (FALSE) {
#'   
#'   
#'   # @examples all_tcherries_cpp(3, 2)
#'   all_tcherries_cpp <- function(n, k, verbose = FALSE) {
#'     require_single_integer(n)
#'     n <- as.integer(n)
#'     
#'     if (n <= 1L) {
#'       stop("Please provide at least two variables, i.e. n >= 2")
#'     }
#'     
#'     require_single_integer(k)
#'     k <- as.integer(k)
#'     
#'     if (k > n) {
#'       stop("k > n")
#'     }
#'     
#'     var_idx <- seq_len(n)
#'     
#'     # Initial cliques
#'     first_cliques_mat <- combn(n, k)
#'     models <- lapply(seq_len(ncol(first_cliques_mat)), 
#'                      function(i) {
#'                        x <- first_cliques_mat[, i]
#'                        
#'                        list(
#'                          cliques = list(x),
#'                          seps = list(),
#'                          unused = setdiff(var_idx, x)
#'                        )
#'                      })
#'     
#'     #str(models, 3)
#'     
#'     if (verbose) {
#'       cat("All initial ", ncol(first_cliques_mat), " cliques created.\n", sep = "")
#'     }
#'     
#'     #' Now, extend by this strategy:
#'     #' 
#'     #' With RIP (running intersection property) in mind,
#'     #' Take last clique of size k and take all (k-1) subsets
#'     #' together with each of of the unused variables, hence
#'     #' there are choose(k, k-1)*length(unused) new possibilities
#'     kmin1_subsets_idx <- combn(k, k - 1L)
#'     
#'     # All candidate models always have the same number of
#'     # unused variables (variables still not in the model)
#'     n_unused <- n - k
#'     
#'     if (verbose) {
#'       cat("Unused variables that needs to be added = ", n_unused, ".\n", sep = "")
#'     }
#'     
#'     models <- rcpp_all_tcherries_worker(models = models, 
#'                                         kmin1_subsets_idx = kmin1_subsets_idx, 
#'                                         n_unused = n_unused,
#'                                         verbose = verbose)
#'     
#'     # FIXME: Remove $unused entries?
#'     for (i_m in seq_along(models)) {
#'       stopifnot( length(models[[i_m]]$unused) == 0L )
#'       models[[i_m]]$unused <- NULL
#'     }
#'     
#'     return(models)
#'   }
#'   
#'   remove_equal_models_rcpp_vechash <- function(models) {
#'     # Construct adjacency matrices and check equality by hash
#'     
#'     n <- length(models)
#'     
#'     if (n <= 1L) {
#'       return(models)
#'     }
#'     
#'     # Only final models
#'     stopifnot(is.null(models[[1L]]$unused))
#'     
#'     As <- lapply(models, model_to_adjacency_matrix)
#'     As_uppertri <- lapply(models, function(m) {
#'       A <- model_to_adjacency_matrix(m)
#'       A[upper.tri(A)]
#'     })
#'     
#'     indices_to_keep <- rcpp_remove_equal_models_worker(As_uppertri)
#'     new_models <- models[indices_to_keep]
#'     
#'     return(new_models)
#'   }
#'   
#'   
#' }
#' if (FALSE) {
#'   ms_r <- remove_equal_models___old_implementation_strhash(all_tcherries__r(n = 6, k = 3))
#'   ms_cpp <- remove_equal_models___rcpp(all_tcherries__cpp(n = 6, k = 3))
#'   ms_cpp_new <- all_tcherries__cpp_new(n = 6, k = 3, remove_duplicates = TRUE)
#'   
#'   length(ms_r)
#'   length(ms_cpp)
#'   length(ms_cpp_new)
#'   
#'   str(ms_r[1:1], 3)
#'   str(ms_cpp[1:1], 3)
#'   str(ms_cpp_new[1:1], 3)
#'   
#'   microbenchmark::microbenchmark(
#'     r = remove_equal_models_r_strhash(all_tcherries__r(n = 6, k = 3)),
#'     cpp = remove_equal_models_rcpp_vechash(all_tcherries__cpp(n = 6, k = 3)),
#'     cpp_new = all_tcherries__cpp_new(n = 6, k = 3, remove_duplicates = TRUE),
#'     times = 10
#'   )
#' }
#' 
#' if (FALSE) {
#'   ms <- all_tcherries(n = 5, k = 3)
#'   length(ms)
#'   
#'   length(remove_equal_models___old_implementation_n2(ms))
#'   length(remove_equal_models___old_implementation_strhash(ms))
#'   length(remove_equal_models___rcpp(ms))
#'   
#'   microbenchmark::microbenchmark(
#'     r_n2_comp = remove_equal_models___old_implementation_n2(ms),
#'     r_strhash = remove_equal_models___old_implementation_strhash(ms),
#'     cpp_intvechash = remove_equal_models___rcpp(ms),
#'     times = 10
#'   )
#'   
#'   ##############
#'   
#'   ms <- all_tcherries(n = 6, k = 3)
#'   length(remove_equal_models___old_implementation_strhash(ms))
#'   length(remove_equal_models___rcpp(ms))
#'   
#'   microbenchmark::microbenchmark(
#'     r_strhash = remove_equal_models___old_implementation_strhash(ms),
#'     cpp_intvechash = remove_equal_models___rcpp(ms),
#'     times = 10
#'   )
#'   
#'   ##############
#'   
#'   ms <- all_tcherries(n = 7, k = 3)
#'   length(ms)
#'   
#'   length(remove_equal_models___old_implementation_strhash(ms))
#'   length(remove_equal_models___rcpp(ms))
#'   
#'   microbenchmark::microbenchmark(
#'     r_strhash = remove_equal_models___old_implementation_strhash(ms),
#'     cpp_intvechash = remove_equal_models___rcpp(ms),
#'     times = 1
#'   )
#' }
#' 
#' if (FALSE) {
#'   r <- c()
#'   for (n in 3:8) {
#'     p <- n^(n-2)
#'     cat("k = 2; n = ", n, "; # models = ", p, "\n")
#'     r <- c(r, p)
#'   }
#'   paste0(r, collapse = ", ")
#'   
#'   #' k = 2:
#'   #' n^(n-2):
#'   #' k = 2; n =  3 ; # models =  3 
#'   #' k = 2; n =  4 ; # models =  16 
#'   #' k = 2; n =  5 ; # models =  125 
#'   #' k = 2; n =  6 ; # models =  1296 
#'   #' k = 2; n =  7 ; # models =  16807 
#'   #' k = 2; n =  8 ; # models =  262144 
#'   #' 3, 16, 125, 1296, 16807, 262144
#'   
#'   for (k in 3:4) {
#'     #for (k in 4) {
#'     r <- c()
#'     for (n in 3:7) {
#'       if (k > n) {
#'         next
#'       }
#'       #ms <- remove_equal_models(all_tcherries(n = n, k = k))
#'       ms <- all_tcherries(n = n, k = k)
#'       ms <- remove_equal_models(ms)
#'       p <- length(ms)
#'       cat("k = ", k, "; n = ", n, "; # models = ", p, "\n")
#'       r <- c(r, p)
#'     }
#'     print(paste0(r, collapse = ", "))
#'   }
#'   
#'   #' k =  3
#'   #' k =  3 ; n =  3 ; # models =  1 
#'   #' k =  3 ; n =  4 ; # models =  6 
#'   #' k =  3 ; n =  5 ; # models =  70 
#'   #' k =  3 ; n =  6 ; # models =  1095
#'   #' k =  3 ; n =  7 ; # models =  21651
#'   #' 1, 6, 70, 1095, 21651
#'   #' https://oeis.org found none
#'   
#'   length(remove_equal_models(all_tcherries(n = 7, k = 4)))
#'   length(remove_equal_models(all_tcherries(n = 8, k = 4)))
#'   #' k =  4:
#'   #' k =  4 ; n =  4 ; # models =  1 
#'   #' k =  4 ; n =  5 ; # models =  10 
#'   #' k =  4 ; n =  6 ; # models =  200 
#'   #' k =  4 ; n =  7 ; # models =  5075 
#'   #' 1, 10, 200, 5075
#'   #' https://oeis.org found none
#'   #' 
#'   
#'   # k =  3 ; n =  3 ; # models =  1 
#'   # k =  3 ; n =  4 ; # models =  8 
#'   # k =  3 ; n =  5 ; # models =  76 
#'   # k =  3 ; n =  6 ; # models =  881 
#'   # k =  3 ; n =  7 ; # models =  11822 
#'   # [1] "1, 8, 76, 881, 11822"
#'   # k =  4 ; n =  4 ; # models =  1 
#'   # k =  4 ; n =  5 ; # models =  13 
#'   # k =  4 ; n =  6 ; # models =  203 
#'   # k =  4 ; n =  7 ; # models =  3847 
#'   # [1] "1, 13, 203, 3847"
#'   
#'   
#'   ms <- all_tcherries(n = 8, k = 3)
#'   length(ms)
#'   ms <- all_tcherries(n = 8, k = 4)
#'   length(ms)
#'   
#'   
#'   Rprof()
#'   m <- all_tcherries(n = 7, k = 3, verbose = TRUE)
#'   length(m)
#'   m <- remove_equal_models(m)
#'   length(m)
#'   Rprof(NULL)
#'   summaryRprof()$by.self
#'   
#'   
#'   Rprof()
#'   m <- all_tcherries(n = 7, k = 3, verbose = TRUE)
#'   length(m)
#'   #m <- remove_equal_models(m)
#'   #length(m)
#'   Rprof(NULL)
#'   summaryRprof()$by.self
#' }
#' 
#' if (FALSE) {
#'   #remotes::install_github("r-prof/jointprof")
#'   
#'   library(jointprof)
#'   out_file <- tempfile("jointprof", fileext = ".out")
#'   start_profiler(out_file)
#'   
#'   ms_cpp <- all_tcherries__cpp(n = 6, k = 3)
#'   
#'   profile_data <- stop_profiler()
#'   
#'   summary <- summaryRprof(out_file)
#'   summary$by.self
#' }
#' 
#' if (FALSE) {
#'   ms_r <- all_tcherries__r(n = 6, k = 3)
#'   ms_cpp <- all_tcherries__cpp(n = 6, k = 3)
#'   
#'   length(ms_r)
#'   length(ms_cpp)
#'   
#'   length(remove_equal_models(ms_r))
#'   length(remove_equal_models(ms_cpp))
#'   
#'   microbenchmark::microbenchmark(
#'     all_tcherries__r(n = 6, k = 3),
#'     all_tcherries__cpp(n = 6, k = 3),
#'     times = 10
#'   )
#' }
#' 
#' 
#' 
#' # remove_equal_models___old_implementation_n2 <- function(models) {
#' #   # Construct adjacency matrices and check equality
#' #   
#' #   # FIXME: All binary n x n matrices (symmetric/upper triangular).
#' #   #        Nice hash function?
#' #   
#' #   n <- length(models)
#' #   
#' #   if (n <= 1L) {
#' #     return(models)
#' #   }
#' #   
#' #   # Only final models
#' #   stopifnot(is.null(models[[1L]]$unused))
#' #   
#' #   As <- lapply(models, model_to_adjacency_matrix)
#' #   
#' #   seen_before <- c()
#' #   
#' #   # n >= 2L, cf. above
#' #   for (i in 2L:n) {
#' #     Ai <- As[[i]]
#' #     
#' #     for (j in 1L:(i-1L)) {
#' #       if (isTRUE(all.equal(Ai, As[[j]]))) {
#' #         seen_before <- c(seen_before, i)
#' #         break
#' #       }
#' #     }
#' #   }
#' #   
#' #   if (length(seen_before) > 0L) {
#' #     models <- models[-seen_before]
#' #   }
#' #   
#' #   return(models)
#' # }