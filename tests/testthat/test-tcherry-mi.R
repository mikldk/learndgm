context("Mutual information")

data(danedb)
danedb_fact_src <- data.frame(apply(danedb, 2, as.character))
danedb_fact <- do.call(cbind, lapply(danedb_fact_src, as.integer))

test_that("Simple", {
  n <- 6L
  k <- 3L
  db_tmp <- danedb_fact[, seq_len(n)]
  m <- all_tcherries(n = n, k = k)
  expect_equal(length(m$models), expected_num_cherrytrees(k = k, n = n))
  
  m_mi <- annotate_with_MI(modellist = m, d = db_tmp)
  expect_equal(length(m_mi$models_mi), length(m$models))
  expect_equal(length(m_mi$models_mi), length(m_mi$models))
  
  expect_equal(min(m_mi$models_mi), 1.83822292451204)
  expect_equal(max(m_mi$models_mi), 3.36984375602067)
  
  m_best <- m_mi$models[[which.max(m_mi$models_mi)]]
  expect_equal(sum(m_best$cliques_mi) - sum(m_best$seps_mi), max(m_mi$models_mi))
  
  
  if (FALSE) {
    dput(min(m_mi$models_mi))
    dput(max(m_mi$models_mi))
    hist(m_mi$models_mi)
  }
})
