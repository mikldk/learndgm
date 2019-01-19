context("Mutual information")

data(danedb)
danedb_fact_src <- data.frame(apply(danedb, 2, as.character))
danedb_fact <- do.call(cbind, lapply(danedb_fact, as.integer))

test_that("Simple", {
  n <- 5L
  db_tmp <- danedb_fact[, seq_len(n)]
  m <- all_tcherries(n = n, k = 3L)
  m_mi <- cpp_annotate_mi(modellist = m, d = db_tmp)
  
})
