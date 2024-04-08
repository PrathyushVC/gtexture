#' Normalize a GLCM
#'
#' Function that normalizes a gray-level co-occurrence matrix (GLCM) so that the
#' sum of all the elements equals unity. This has the added benefit of
#' converting the GLCM to a probability distribution.
#'
#' @param mat gray-level co-occurrence matrix
#' @return numeric matrix (same dimensions as input GLCM)
#' @export
#' @examples
#' # normalize an arbitrary matrix
#' a <- matrix(1:9, nrow = 3)
#' n_a <- normalize_glcm(a)
#'
#' print(a)
#' print(n_a)
normalize_glcm <- function(mat) {
  # ensure valid GLCM is passed in mat
  stopifnot(is.numeric(mat))
  stopifnot(sum(is.na(mat)) == 0)
  stopifnot(nrow(mat) == ncol(mat))

  # normalize and return
  mat / sum(mat)
}
