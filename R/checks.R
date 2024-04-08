#####INTEGER SIGN FUNCTIONS#####
# either x is an integer or is a numeric that is very close to an integer
valid_int <- function(x, eps = 1e-7) {
  if ("integer" %in% class(x)) return(TRUE)

  else if ("numeric" %in% class(x)) {
    x_int <- round(x, digits = 0)

    return(abs(x - x_int) <= eps)

  # neither numeric nor integer, can't be a valid int (per this pkg)
  } else return(FALSE)
}

# x is a non-negative integer
valid_nonneg_int <- function(x) {
  return(valid_int(x) & x >= 0)
}

# x is a positive integer
valid_pos_int <- function(x) {
  return(valid_int(x) & x > 0)
}
