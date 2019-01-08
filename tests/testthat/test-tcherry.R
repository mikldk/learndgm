context("Generate all third order t-cherry junction trees")

#if (FALSE) {
  library(igraph)
  
  n <- 4
  
  if (!exists("f_tmp")) {
    f_tmp <- tempfile()
  }
  
  
  pdf(f_tmp, width = 10, height = 5)
  s <- all_prufer_sequences(n)
  par(mfrow = rev(n2mfrow(nrow(s))))
  par(mar = c(0, 0, 0, 0))
  gs <- prufer_sequences_as_graphs(s)
  for (i in seq_along(gs)) {
    plot(gs[[i]], vertex.size = 30)
  }
  par(mfrow = c(1, 1))
  dev.off()
  
  system(paste0("evince ", f_tmp), wait = FALSE)
  
#}

