#' Discretize Numeric Variable Into Categories
#'
#' Takes a numeric variable (could be of class `numeric` or `integer`) and
#' returns a discretized version, in which each element has been replaced by a
#' single integer between `1` and `nlevels`, inclusive.
#'
#' @name discretize
#' @param x either a vector (`numeric` or `integer`) or `FitLandDF` object
#' @param nlevels positive integer indicating number of discrete categories
#' @param method method by which to discretize; split into equal sections by
#'   default (`"equal"` value for parameter)
#' @param ... potential additional arguments, currently unnecessary
#' @return discretized form of `x`
#' @import fitscape
#' @export
#' @examples
#'
#' ## discretize a numeric vector
#' vec <- 1:10
#' discretize(vec, nlevels = 5) # discretize into 5 categories
#' discretize(vec, 2)           # discretize into 2 categories
#'
#' ## discretize a fitness landscape
#' # create a 3x3x3 fitness landscape with values 1 through 27
#' fl_data <- array(1:27, dim = rep(3, 3))
#' my_fl <- fitscape::FitLandDF(fl_data)
#' discretize(my_fl, nlevels = 2) # discretize landscape into 2 categories
#' discretize(my_fl, 5)           # discretize landscape into 5 categories
discretize <- function(x, ...) {
  UseMethod("discretize")
}

# discretize numeric vectors
#' @rdname discretize
#' @export
discretize.numeric <- function(x, nlevels, method = "equal", ...) {
  # check parameter validity
  stopifnot(valid_pos_int(nlevels))

  # decide which function to use to discretize
  disc_func <- NULL
  if (method == "equal") {
    disc_func <- equal_discrete(nlevels)
  } else {
    stop("invalid parameter for `method` passed to `discretize` function")
  }

  # split into levels and return discrete value
  disc_func(x)
}

# discretize list 
#' @rdname discretize
#' @export
discretize.list <- function(x, nlevels, ...) {
  discretize.numeric(unlist(x), nlevels, ...)
}


# discretize integer vectors
#' @rdname discretize
#' @export
discretize.integer <- function(x, nlevels, ...) {
  discretize.numeric(x, nlevels, ...)
}

# discretize fitness landscape
#' @rdname discretize
#' @export
discretize.FitLandDF <- function(x, nlevels, ...) {
  x$Value <- discretize.numeric(x$Value, nlevels, ...)

  return(x)
}

#' Function Factory for Even Discretization Functions
#'
#' Returns a function that converts a continuous numeric vector into an integer
#' vector with discrete levels.
#'
#' @param nlevels number of levels to split continuous vector into
#' @return function that makes a numeric vector discrete
#' @export
#' @examples
#' # test data
#' x <- 1:10
#'
#' # create and apply function to split x into 2 discrete levels
#' split_2 <- equal_discrete(2)
#' split_2(x)
#'
#' # create and apply function to split x into 5 discrete levels
#' split_5 <- equal_discrete(5)
#' split_5(x)
equal_discrete <- function(nlevels) {
  function(x) {
    stopifnot(("numeric" %in% class(x)) | ("integer" %in% class(x)))

    x %>%
      cut(breaks = nlevels) %>%
      as.integer
  }
}



#' Function to discretize based on quantiles
#' @param nlevels number of levels to split continuous vector into
#' @return function that makes a numeric vector discrete
#' @export
#' @examples
#' # test data
quantile_discrete <- function(nlevels) {
  function(x) {
    stopifnot(("numeric" %in% class(x)) | ("integer" %in% class(x)))

    x %>%
      ntile(n= nlevels) %>%
      as.integer
  }
}



#' Kmeans clustering discretization 
#' 
#' @description Splitting of a vector of continuous values into k groups
#' using kmeans. Used to discretize node labels/weights of a fitness landscape
#' or node-weighted graph. 
#'
#' @param nlevels number of levels to split continuous vector into
#' @return function that makes a numeric vector discrete
#' @export
kmeans_discrete <- function(nlevels){
  function(x){
    stopifnot(("numeric" %in% class(x)) | ("integer" %in% class(x)))

    bins = dlookr::binning(x, nlevels, type="kmeans")
    levels(bins) = seq(1,nlevels)
    dlookr::extract(bins) %>% as.integer
  }

}
