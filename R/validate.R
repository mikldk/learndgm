require_single_integer <- function(n, name) {
  if (!is.vector(n)) {
    stop(paste0(name, " not a vector"))
  }
  
  if (length(n) != 1L) {
    stop(paste0("length(", name, ") != 1L"))
  }
  
  if (!isTRUE(all.equal(n, round(n)))) {
    stop(paste0(name, " != round(", name, "), i.e. not integer"))
  }
}
