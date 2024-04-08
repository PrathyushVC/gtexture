#####MANHATTAN DISTANCE#####
# Manhattan distance function factory (checks Manhattan distance < x)
#' Manhattan Distance Function Factory
#'
#' Returns a function that checks whether the Manhattan distance between two
#' numeric vectors is less than or equal to a given threshold.
#'
#' @param dist threshold above which the function will return `FALSE`
#' @return function that checks if Manhattan distance between two vectors
#'   exceeds `dist`
#' @export
#' @examples
#' # test data: Manhattan distance equals 2
#' x <- rep(0, 5)
#' y <- c(0, 1, 0, 0, 1)
#'
#' # should return TRUE when checking Manhattan distance <= 3
#' dist_3 <- manhattan(3)
#' dist_3(x, y)
#'
#' # should return FALSE when checking Manhattan distance <= 1
#' dist_1 <- manhattan(1)
#' dist_1(x, y)
#'
manhattan <- function(dist = 1) {
  function(x, y) {
    stopifnot(length(x) == length(y))

    sum(abs(x - y)) <= dist
  }
}

#####EUCLIDEAN DISTANCE#####
# Euclidean distance function factory (checks Euclidean distance < x)
#' Euclidean Distance Function Factory
#'
#' Returns a function that checks whether the Euclidean distance between two
#' numeric vectors is less than or equal to a given threshold.
#'
#' @param dist threshold above which the function will return `FALSE`
#' @return function that checks if Euclidean distance between two vectors
#'   exceeds `dist`
#' @export
#' @examples
#' # test data: Euclidean distance equals sqrt(2) ~ 1.414
#' x <- rep(0, 5)
#' y <- c(0, 1, 0, 0, 1)
#'
#' # should return TRUE when checking Manhattan distance <= 2
#' dist_2 <- euclidean(2)
#' dist_2(x, y)
#'
#' # should return FALSE when checking Manhattan distance <= 1
#' dist_1 <- euclidean(1)
#' dist_1(x, y)
#'
euclidean <- function(dist = 1) {
  function(x, y) {
    stopifnot(length(x) == length(y))

    sqrt(sum((x - y) ^ 2)) <= dist
  }
}
