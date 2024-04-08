# CHECKED that value doesn't change for different S3 generic formats
# NOT CHECKED that actual value is correct for each metric

#####AUTOCORRELATION#####
test_that("autocorrelation metric works", {
  # create fitness landscape using FitLandDF object
  vals <- runif(27)
  vals <- array(vals, dim = rep(3, 3))
  my_landscape <- fitscape::FitLandDF(vals)

  # calculate metric for fitness landscape, assuming 2 discrete gray levels
  a <- autocorrelation(my_landscape, nlevels = 2)

  # extract normalized GLCM from fitness landscape
  my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

  # calculate metric for extracted GLCM
  b <- autocorrelation(my_glcm)

  # test
  expect_equal(a, b)
})

#####CLUSTER PROMINENCE#####
test_that("cluster prominence metric works", {
  # create fitness landscape using FitLandDF object
  vals <- runif(27)
  vals <- array(vals, dim = rep(3, 3))
  my_landscape <- fitscape::FitLandDF(vals)

  # calculate metric for fitness landscape, assuming 2 discrete gray levels
  a <- cluster_prom(my_landscape, nlevels = 2)

  # extract normalized GLCM from fitness landscape
  my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

  # calculate metric for extracted GLCM
  b <- cluster_prom(my_glcm)

  # test
  expect_equal(a, b)
})

#####CLUSTER SHADE#####
test_that("cluster shade metric works", {
  # create fitness landscape using FitLandDF object
  vals <- runif(27)
  vals <- array(vals, dim = rep(3, 3))
  my_landscape <- fitscape::FitLandDF(vals)

  # calculate metric for fitness landscape, assuming 2 discrete gray levels
  a <- cluster_shade(my_landscape, nlevels = 2)

  # extract normalized GLCM from fitness landscape
  my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

  # calculate metric for extracted GLCM
  b <- cluster_shade(my_glcm)

  # test
  expect_equal(a, b)
})

#####CONTRAST#####
test_that("contrast metric works", {
  # create fitness landscape using FitLandDF object
  vals <- runif(27)
  vals <- array(vals, dim = rep(3, 3))
  my_landscape <- fitscape::FitLandDF(vals)

  # calculate metric for fitness landscape, assuming 2 discrete gray levels
  a <- contrast(my_landscape, nlevels = 2)

  # extract normalized GLCM from fitness landscape
  my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

  # calculate metric for extracted GLCM
  b <- contrast(my_glcm)

  # test
  expect_equal(a, b)
})

#####ENERGY#####
test_that("energy metric works", {
  # create fitness landscape using FitLandDF object
  vals <- runif(27)
  vals <- array(vals, dim = rep(3, 3))
  my_landscape <- fitscape::FitLandDF(vals)

  # calculate metric for fitness landscape, assuming 2 discrete gray levels
  a <- energy(my_landscape, nlevels = 2)

  # extract normalized GLCM from fitness landscape
  my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

  # calculate metric for extracted GLCM
  b <- energy(my_glcm)

  # test
  expect_equal(a, b)
})

#####ENTROPY#####
test_that("entropy metric works", {
  # create fitness landscape using FitLandDF object
  vals <- runif(27)
  vals <- array(vals, dim = rep(3, 3))
  my_landscape <- fitscape::FitLandDF(vals)

  # calculate metric for fitness landscape, assuming 2 discrete gray levels
  a <- entropy(my_landscape, nlevels = 2)

  # extract normalized GLCM from fitness landscape
  my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

  # calculate metric for extracted GLCM
  b <- entropy(my_glcm)

  # test
  expect_equal(a, b)
})

#####HOMOGENEITY#####
test_that("homogeneity metric works", {
  # create fitness landscape using FitLandDF object
  vals <- runif(27)
  vals <- array(vals, dim = rep(3, 3))
  my_landscape <- fitscape::FitLandDF(vals)

  # calculate metric for fitness landscape, assuming 2 discrete gray levels
  a <- homogeneity(my_landscape, nlevels = 2)

  # extract normalized GLCM from fitness landscape
  my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

  # calculate metric for extracted GLCM
  b <- homogeneity(my_glcm)

  # test
  expect_equal(a, b)
})

#####INVERSE DIFFERENCE#####
test_that("inverse difference metric works", {
  # create fitness landscape using FitLandDF object
  vals <- runif(27)
  vals <- array(vals, dim = rep(3, 3))
  my_landscape <- fitscape::FitLandDF(vals)

  # calculate metric for fitness landscape, assuming 2 discrete gray levels
  a <- inv_diff(my_landscape, nlevels = 2)

  # extract normalized GLCM from fitness landscape
  my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

  # calculate metric for extracted GLCM
  b <- inv_diff(my_glcm)

  # test
  expect_equal(a, b)
})

#####MAXIMUM PROBABILITY#####
test_that("maximum probability metric works", {
  # create fitness landscape using FitLandDF object
  vals <- runif(27)
  vals <- array(vals, dim = rep(3, 3))
  my_landscape <- fitscape::FitLandDF(vals)

  # calculate metric for fitness landscape, assuming 2 discrete gray levels
  a <- max_prob(my_landscape, nlevels = 2)

  # extract normalized GLCM from fitness landscape
  my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

  # calculate metric for extracted GLCM
  b <- max_prob(my_glcm)

  # test
  expect_equal(a, b)
})

#####SUM OF SQUARES#####
test_that("sum of squares metric works", {
  # create fitness landscape using FitLandDF object
  vals <- runif(27)
  vals <- array(vals, dim = rep(3, 3))
  my_landscape <- fitscape::FitLandDF(vals)

  # calculate metric for fitness landscape, assuming 2 discrete gray levels
  a <- sum_squares(my_landscape, nlevels = 2)

  # extract normalized GLCM from fitness landscape
  my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

  # calculate metric for extracted GLCM
  b <- sum_squares(my_glcm)

  # test
  expect_equal(a, b)
})
