context("Generate all third order t-cherry junction trees")

#' 
m <- all_tcherries(c("A", "B", "C", "D"), 2)
m <- remove_equal_models(m)
length(m) == lenght(prufer(4))

