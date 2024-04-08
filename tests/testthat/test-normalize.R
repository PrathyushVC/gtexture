test_that("GLCM normalization works", {
  # make sure it works for an easy, arbitrary matrix
  test_mat <- matrix(1:9, nrow = 3)
  ans_mat <- test_mat / sum(test_mat)
  n_test_mat <- normalize_glcm(test_mat)

  expect_equal(sum(n_test_mat), 1)
  expect_identical(ans_mat, n_test_mat)

  # make sure errors are thrown when they should be
  test_mat2 <- matrix(c(1:8, NA), nrow = 3)
  expect_error(normalize_glcm(test_mat2))

  test_mat3 <- matrix("a", nrow = 3, ncol = 3)
  expect_error(normalize_glcm(test_mat3))
})
