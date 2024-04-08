test_that("discretization works", {
  # create test data
  num_dim <- 2
  num_val <- 3
  test_integer <- 1:(num_val ^ num_dim)
  test_numeric <- as.numeric(test_integer)
  test_fl <- fitscape::FitLandDF(array(test_integer, dim = rep(num_val, num_dim)))

  ## HARDCODED for num_dim := 2 and num_val := 3
  ans1 <- c(rep(1, 3), rep(2, 3), rep(3, 3))
  ans2 <- 1:9
  ans3 <- c(rep(1, 5), rep(2, 4))
  ##

  # test various discretizations of integer
  expect_equal(discretize(test_integer, 3), ans1)
  expect_equal(discretize(test_integer, 9), ans2)
  expect_equal(discretize(test_integer, 2), ans3)

  # test various discretizations of numeric
  expect_equal(discretize(test_numeric, 3), ans1)
  expect_equal(discretize(test_numeric, 9), ans2)
  expect_equal(discretize(test_numeric, 2), ans3)

  ## test various discretizations of FitLandDF (with rest of df unchanged)
  # calculate discretization
  test1 <- discretize(test_fl, 3)
  test2 <- discretize(test_fl, 9)
  test3 <- discretize(test_fl, 2)

  # ensure rest of DF didn't change
  initial_df <- fitscape::extract_df(test_fl)[, 1:num_dim]
  expect_identical(initial_df, fitscape::extract_df(test1)[, 1:num_dim])
  expect_identical(initial_df, fitscape::extract_df(test2)[, 1:num_dim])
  expect_identical(initial_df, fitscape::extract_df(test3)[, 1:num_dim])

  # confirm discretization worked correctly
  expect_equal(test1$Value, ans1)
  expect_equal(test2$Value, ans2)
  expect_equal(test3$Value, ans3)
})
