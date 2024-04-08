# Each of these metrics normalizes the GLCM, then applies the metric itself

#####AUTOCORRELATION#####
#' Autocorrelation Metric for a GLCM or GLCM equivalent matrix
#'
#' Calculate the autocorrelation feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' \doi{10.1371/journal.pone.0212110}.
#'
#' @name autocorrelation
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @return double 
#' @export
#' @examples
#' ## calculate autocorrelation of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate autocorrelation
#' autocorrelation(n_x)
#'
#' ## calculate autocorrelation of arbitrary fitness landscape
#' # create fitness landscape using FitLandDF object
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' # calculate autocorrelation of fitness landscape, assuming 2 discrete gray levels
#' autocorrelation(my_landscape, nlevels = 2)
#'
#' ## confirm value of autocorrelation for fitness landscape
#' # extract normalized GLCM from fitness landscape
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#'
#' # calculate autocorrelation of extracted GLCM
#' autocorrelation(my_glcm)  # should match value of above autocorrelation function call
autocorrelation <- function(x, ...) {
  UseMethod("autocorrelation")
}

# default behavior not defined
#' @rdname autocorrelation
#' @export
autocorrelation.default <- function(x, ...) {
  stop("default behavior has not been defined for autocorrelation")
}

#' @rdname autocorrelation
#' @export
autocorrelation.matrix <- function(x, ...) {
  # normalization step
  n_x <- normalize_glcm(x)

  # calculation step
  for (i in seq_len(nrow(n_x))) {
    n_x[i, ] <- i * n_x[i, ]
  }
  for (j in seq_len(ncol(n_x))) {
    n_x[, j] <- j * n_x[, j]
  }
  sum(n_x)
}

#' @rdname autocorrelation
#' @export
autocorrelation.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  autocorrelation.matrix(x_glcm)
}


#####CORRELATION#####
#' Correlation Metric for a GLCM
#'
#' Calculate the correlation feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' \doi{10.1371/journal.pone.0212110}.
#'
#' @name correlation
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @return double
#' @export
#' @examples
#' ## calculate correlation of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate correlation
#' correlation(n_x)
#'
#' ## calculate autocorrelation of arbitrary fitness landscape
#' # create fitness landscape using FitLandDF object
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' # calculate correlation of fitness landscape, assuming 2 discrete gray levels
#' correlation(my_landscape, nlevels = 2)
#'
#' ## confirm value of correlation for fitness landscape
#' # extract normalized GLCM from fitness landscape
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#'
#' # calculate correlation of extracted GLCM
#' correlation(my_glcm)  # should match value of above correlation function call
correlation <- function(x, ...) {
  UseMethod("correlation")
}

# default behavior not defined
#' @rdname correlation
#' @export
correlation.default <- function(x, ...) {
  stop("default behavior has not been defined for correlation")
}

# matrix correlation
#' @rdname correlation
#' @export
correlation.matrix <- function(x, ...) {
  glcm <- normalize_glcm(x)
  sum <- 0
  mean <- glcm_mean(glcm)
  variance <- glcm_variance(glcm)
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + glcm[i,j]*(((i-1) - mean)*((j-1) - mean))/variance
    }
  }
  return(sum)
}

#' @rdname correlation
#' @export
correlation.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  correlation.matrix(x_glcm)

}


#####CLUSTER PROMINENCE#####
#' Cluster Prominence Metric for a GLCM
#'
#' Calculate the cluster prominence feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' \doi{10.1371/journal.pone.0212110}.
#'
#' @name cluster_prom
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @return double
#' @export
#' @examples
#' ## calculate cluster prominence of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate cluster prominence
#' cluster_prom(n_x)
#'
#' ## calculate cluster prominence of arbitrary fitness landscape
#' # create fitness landscape using FitLandDF object
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' # calculate cluster prominence of fitness landscape, assuming 2 discrete gray levels
#' cluster_prom(my_landscape, nlevels = 2)
#'
#' ## confirm value of cluster prominence for fitness landscape
#' # extract normalized GLCM from fitness landscape
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#'
#' # calculate cluster prominence of extracted GLCM
#' cluster_prom(my_glcm)  # should match value of above cluster_prom function call
cluster_prom <- function(x, ...) {
  UseMethod("cluster_prom")
}

# default behavior not defined
#' @rdname cluster_prom
#' @export
cluster_prom.default <- function(x, ...) {
  stop("default behavior has not been defined for cluster prominence")
}

#' @rdname cluster_prom
#' @export
cluster_prom.matrix <- function(x, ...) {
  # normalization step
  n_x <- normalize_glcm(x)

  # calculation step
  counter <- 0
  mu_val <- mean(n_x)
  for (i in seq_len(nrow(n_x))) {
    for (j in seq_len(ncol(n_x))) {
      counter <- counter + p(n_x, i, j) * (i + j - 2 * mu_val) ^ 3
    }
  }
  counter
}

#' @rdname cluster_prom
#' @export
cluster_prom.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  cluster_prom.matrix(x_glcm)
}

#####CLUSTER SHADE#####
#' Cluster Shade Metric for a GLCM
#'
#' Calculate the cluster shade feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' \doi{10.1371/journal.pone.0212110}.
#'
#' @name cluster_shade
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @return double
#' @export
#' @examples
#' ## calculate cluster shade of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate cluster shade
#' cluster_shade(n_x)
#'
#' ## calculate cluster shade of arbitrary fitness landscape
#' # create fitness landscape using FitLandDF object
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' # calculate cluster shade of fitness landscape, assuming 2 discrete gray levels
#' cluster_shade(my_landscape, nlevels = 2)
#'
#' ## confirm value of cluster shade for fitness landscape
#' # extract normalized GLCM from fitness landscape
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#'
#' # calculate cluster shade of extracted GLCM
#' cluster_shade(my_glcm)  # should match value of above cluster_shade function call
cluster_shade <- function(x, ...) {
  UseMethod("cluster_shade")
}

# default behavior not defined
#' @rdname cluster_shade
#' @export
cluster_shade.default <- function(x, ...) {
  stop("default behavior has not been defined for cluster shade")
}

#' @rdname cluster_shade
#' @export
cluster_shade.matrix <- function(x, ...) {
  # normalization step
  n_x <- normalize_glcm(x)

  # calculation step
  counter <- 0
  mu_val <- mean(n_x)
  for (i in seq_len(nrow(n_x))) {
    for (j in seq_len(ncol(n_x))) {
      counter <- counter + p(n_x, i, j) * (i + j - 2 * mu_val) ^ 4
    }
  }
  counter
}

#' @rdname cluster_shade
#' @export
cluster_shade.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  cluster_shade.matrix(x_glcm)
}

#####CONTRAST#####
#' Contrast Metric for a GLCM
#'
#' Calculate the contrast feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' \doi{10.1371/journal.pone.0212110}.
#'
#' @name contrast
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @return double
#' @export
#' @examples
#' ## calculate contrast of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate contrast
#' contrast(n_x)
#'
#' # calculate contrast of fitness landscape, assuming 2 discrete gray levels
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#' contrast(my_landscape, nlevels = 2)
#'
#' ## confirm value of contrast for fitness landscape
#' # extract normalized GLCM from fitness landscape
#'
#' contrast(my_glcm)  # should match value of above contrast function call
contrast <- function(x, ...) {
  UseMethod("contrast")
}

# default behavior not defined
#' @rdname contrast
#' @export
contrast.default <- function(x, ...) {
  stop("default behavior has not been defined for contrast")
}

#' @rdname contrast
#' @export
contrast.matrix <- function(x, ...) {
  # normalization step
  n_x <- normalize_glcm(x)

  # calculation step
  counter <- 0
  for (i in seq_len(nrow(n_x))) {
    for (j in seq_len(ncol(n_x))) {
      counter <- counter + (i - j) ^ 2 * p(n_x, i, j)
    }
  }
  counter
}

#' @rdname contrast
#' @export
contrast.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  contrast.matrix(x_glcm)
}

#####ENERGY#####
# S3 generic for GLCM energy metric
# aim to implement for matrix and FitLandDF
#' Energy Metric for a GLCM
#'
#' Calculate the energy feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' \doi{10.1371/journal.pone.0212110}.
#'
#' @name energy
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @return double
#' @export
#' @examples
#' ## calculate energy of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate energy
#' energy(n_x)
#'
#' ## calculate energy of arbitrary fitness landscape
#' # create fitness landscape using FitLandDF object
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' # calculate energy of fitness landscape, assuming 2 discrete gray levels
#' energy(my_landscape, nlevels = 2)
#'
#' ## confirm value of energy for fitness landscape
#' # extract normalized GLCM from fitness landscape
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#'
#' # calculate energy of extracted GLCM
#' energy(my_glcm)  # should match value of above energy function call
energy <- function(x, ...) {
  UseMethod("energy")
}

# default behavior not defined
#' @rdname energy
#' @export
energy.default <- function(x, ...) {
  stop("default behavior has not been defined for entropy")
}

#' @rdname energy
#' @export
energy.matrix <- function(x, ...) {
  # normalization step
  n_x <- normalize_glcm(x)

  # calculation step
  sum(n_x * n_x)
}

#' @rdname energy
#' @export
energy.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  energy.matrix(x_glcm)
}

#####ENTROPY#####
# S3 generic for GLCM entropy metric
# current goal to implement only for FitLandDF class; default throws error
#' Entropy Metric for a GLCM
#'
#' Calculate the entropy feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' \doi{10.1371/journal.pone.0212110}.
#'
#' @name entropy
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @return double
#' @export
#' @examples
#' ## calculate entropy of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate entropy
#' entropy(n_x)
#'
#' ## calculate entropy of arbitrary fitness landscape
#' # create fitness landscape using FitLandDF object
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' # calculate entropy of fitness landscape, assuming 2 discrete gray levels
#' entropy(my_landscape, nlevels = 2)
#'
#' ## confirm value of entropy for fitness landscape
#' # extract normalized GLCM from fitness landscape
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#'
#' # calculate entropy of extracted GLCM
#' entropy(my_glcm)  # should match value of above entropy function call
entropy <- function(x, ...) {
  UseMethod("entropy")
}

# worry about this when generalizing entropy past fitness landscapes
#' @rdname entropy
#' @export
entropy.default <- function(x, ...) {
  stop("default behavior has not been defined for entropy")
}

#' @rdname entropy
#' @export
entropy.matrix <- function(x, ...) {
  # normalization
  n_x <- normalize_glcm(x)

  zero_correction=1e-8
  # calculation
  -sum(n_x * log(n_x + zero_correction))
}

#' @rdname entropy
#' @export
entropy.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  entropy.matrix(x_glcm)
}

#####HOMOGENEITY#####
#' Homogeneity Metric for a GLCM
#'
#' Calculate the homogeneity feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' \doi{10.1371/journal.pone.0212110}.
#'
#' @name homogeneity
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @returns double 
#' @export
#' @examples
#' ## calculate homogeneity of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate homogeneity
#' homogeneity(n_x)
#'
#' ## calculate homogeneity of arbitrary fitness landscape
#' # create fitness landscape using FitLandDF object
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' # calculate homogeneity of fitness landscape, assuming 2 discrete gray levels
#' homogeneity(my_landscape, nlevels = 2)
#'
#' ## confirm value of homogeneity for fitness landscape
#' # extract normalized GLCM from fitness landscape
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#'
#' # calculate homogeneity of extracted GLCM
#' homogeneity(my_glcm)  # should match value of above homogeneity function call
homogeneity <- function(x, ...) {
  UseMethod("homogeneity")
}

# default behavior not defined
#' @rdname homogeneity
#' @export
homogeneity.default <- function(x, ...) {
  stop("default behavior has not been defined for homogeneity")
}

#' @rdname homogeneity
#' @export
homogeneity.matrix <- function(x, ...) {
  # normalization step
  n_x <- normalize_glcm(x)

  # calculation step
  counter <- 0
  for (i in seq_len(nrow(n_x))) {
    for (j in seq_len(ncol(n_x))) {
      counter <- counter + p(n_x, i, j) / (1 + (i - j) ^ 2)
    }
  }
  counter
}

#' @rdname homogeneity
#' @export
homogeneity.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  homogeneity.matrix(x_glcm)
}

#####INVERSE DIFFERENCE#####
#' Inverse Difference Metric for a GLCM
#'
#' Calculate the inverse difference feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' \doi{10.1371/journal.pone.0212110}.
#'
#' @name inv_diff
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @returns double
#' @export
#' @examples
#' ## calculate inverse difference of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate inverse difference
#' inv_diff(n_x)
#'
#' ## calculate inverse difference of arbitrary fitness landscape
#' # create fitness landscape using FitLandDF object
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' # calculate inverse difference of fitness landscape, assuming 2 discrete gray levels
#' inv_diff(my_landscape, nlevels = 2)
#'
#' ## confirm value of inverse difference for fitness landscape
#' # extract normalized GLCM from fitness landscape
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#'
#' # calculate inverse difference of extracted GLCM
#' inv_diff(my_glcm)  # should match value of above inv_diff function call
inv_diff <- function(x, ...) {
  UseMethod("inv_diff")
}

# default behavior not defined
#' @rdname inv_diff
#' @export
inv_diff.default <- function(x, ...) {
  stop("default behavior has not been defined for inverse difference")
}

#' @rdname inv_diff
#' @export
inv_diff.matrix <- function(x, ...) {
  # normalization step
  n_x <- normalize_glcm(x)

  # calculation step
  counter <- 0
  for (i in seq_len(nrow(n_x))) {
    for (j in seq_len(ncol(n_x))) {
      counter <- counter + p(n_x, i, j) / (1 + abs(i - j))
    }
  }
  counter
}

#' @rdname inv_diff
#' @export
inv_diff.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  inv_diff.matrix(x_glcm)
}

#####MAXIMUM PROBABILITY#####
#' Maximum Probability Metric for a GLCM
#'
#' Calculate the maximum probability feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' \doi{10.1371/journal.pone.0212110}.
#'
#' @name max_prob
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @return double 
#' @export
#' @examples
#' ## calculate maximum probability of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate maximum probability
#' max_prob(n_x)
#'
#' ## calculate maximum probability of arbitrary fitness landscape
#' # create fitness landscape using FitLandDF object
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' # calculate maximum probability of fitness landscape, assuming 2 discrete gray levels
#' max_prob(my_landscape, nlevels = 2)
#'
#' ## confirm value of maximum probability for fitness landscape
#' # extract normalized GLCM from fitness landscape
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#'
#' # calculate maximum probability of extracted GLCM
#' max_prob(my_glcm)  # should match value of above max_prob function call
max_prob <- function(x, ...) {
  UseMethod("max_prob")
}

# default behavior not defined
#' @rdname max_prob
#' @export
max_prob.default <- function(x, ...) {
  stop("default behavior has not been defined for maximum probability")
}

#' @rdname max_prob
#' @export
max_prob.matrix <- function(x, ...) {
  # normalization step
  n_x <- normalize_glcm(x)

  # calculation step
  max(n_x)
}

#' @rdname max_prob
#' @export
max_prob.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  max_prob.matrix(x_glcm)
}

#####SUM OF SQUARES#####
#' Sum of Squares Metric for a GLCM
#'
#' Calculate the sum of squares feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' \doi{10.1371/journal.pone.0212110}.
#'
#' @name sum_squares
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @returns double 
#' @export
#' @examples
#' ## calculate sum of squares of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate sum of squares
#' sum_squares(n_x)
#'
#' ## calculate sum of squares of arbitrary fitness landscape
#' # create fitness landscape using FitLandDF object
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' # calculate sum of squares of fitness landscape, assuming 2 discrete gray levels
#' sum_squares(my_landscape, nlevels = 2)
#'
#' ## confirm value of sum of squares for fitness landscape
#' # extract normalized GLCM from fitness landscape
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#'
#' # calculate sum of squares of extracted GLCM
#' sum_squares(my_glcm)  # should match value of above sum_squares function call
sum_squares <- function(x, ...) {
  UseMethod("sum_squares")
}

# default behavior not defined
#' @rdname sum_squares
#' @export
#' 
sum_squares.default <- function(x, ...) {
  stop("default behavior has not been defined for sum of squares")
}

#' @rdname sum_squares
#' @export
sum_squares.matrix <- function(x, ...) {
  # normalization step
  n_x <- normalize_glcm(x)

  # calculation step
  counter <- 0
  mu_val <- mean(n_x)
  for (i in seq_len(nrow(n_x))) {
    for (j in seq_len(ncol(n_x))) {
      counter <- counter + p(n_x, i, j) * (i - mu_val) ^ 2
    }
  }
  counter
}

#' @rdname sum_squares
#' @export
sum_squares.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  sum_squares.matrix(x_glcm)
}

#' Convenience function to compute all haralick texture features
#'
#' @description
#' Calculate set of the following Haralick texture features; contrast, entropy, energy,
#' autocorrelation, correlation, cluster prominence, cluster shade, homogeneity,
#' inverse difference, max probability, and sum of squares.
#' @param x Input glcm matrix
#' @export
#' @returns data.frame with 11 columns
#' 
compute_all_metrics <- function(x) {

  df = data.frame(
    contrast = contrast(x),
    entropy =entropy(x),
    energy = energy(x),
    autocorrelation = autocorrelation(x),
    correlation = correlation(x),
    cluster_prominence = cluster_prom(x),
    cluster_shade = cluster_shade(x),
    homogeneity = homogeneity(x),
    inverse_difference = inv_diff(x),
    max_probability = max_prob(x),
    sum_of_squares = sum_squares(x)
  )
  return(df)
}
