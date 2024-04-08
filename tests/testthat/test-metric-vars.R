test_that("calculation of metric variables works", {
  # setup test data
  test_mat <- matrix(1:9, nrow = 3)
  n_mat <- normalize_glcm(test_mat)

  # p
  for (i in seq_len(nrow(n_mat))) {
    for (j in seq_len(ncol(n_mat))) {
      expect_equal(p(n_mat, i, j), n_mat[i, j])
    }
  }

  # p_x
  for (i in seq_len(nrow(n_mat))) {
    expect_equal(p_x(n_mat, i), sum(n_mat[i, ]))
  }

  # p_y
  for (j in seq_len(ncol(n_mat))) {
    expect_equal(p_y(n_mat, j), sum(n_mat[, j]))
  }

  # mu_x; manually calculated
  expect_equal(mu_x(n_mat), 2.1 + 1 / 30)

  # mu_y; manually calculated
  expect_equal(mu_y(n_mat), 2.4)

  # var_x

  # var_y

  # p_xplusy

  # p_xminusy

  # mu_xplusy

  # mu_xminusy

  # HX

  # HY

  # HXY

  # HXY1

  # HXY2

  # Q

})
