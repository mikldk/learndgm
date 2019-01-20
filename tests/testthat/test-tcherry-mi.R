context("Mutual information")

data(danedb)
danedb_fact_src <- data.frame(apply(danedb, 2, as.character))
danedb_fact <- do.call(cbind, lapply(danedb_fact_src, as.integer))

test_that("Simple", {
  n <- 5L
  db_tmp <- danedb_fact[, seq_len(n)]
  m <- all_tcherries(n = n, k = 3L)
  m_mi <- annotate_with_MI(modellist = m, d = db_tmp)
  
  expect_equal(length(m_mi$models_mi), length(m$models))
  expect_equal(length(m_mi$models_mi), length(m_mi$models))
  
  # FIXME
  #expect_equal(max(m_mi$models_mi), 4.06862822866555)
  
  if (FALSE) {
    hist(m_mi$models_mi)
  }
})
