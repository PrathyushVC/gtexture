# generic to get co-occurrence matrix from objects of various classes
#' Calculate Co-Occurrence Matrix
#'
#' Calculate generalized co-occurrence matrix from a variety of objects,
#' currently including fitness landscapes stored as a `FitLandDF` instance from
#' the `fitscape` package.
#'
#' @name comat
#' @param x object upon which co-occurrence matrix will be calculated
#' @param discrete function that discretizes object
#' @param neighbor function that returns TRUE if two numeric vectors are within
#'   acceptable distance of one another or a single-element `character` vector
#'   that describes how to identify acceptable neighbors/offsets
#' @param normalize function that normalizes the co-occurrence matrix
#' @param values named numeric with values corresponding to the nodes in x.
#' @param verbose bool
#' @param nlevels int number of levels to discretize into
#' @param ... additional arguments
#' @return matrix (co-occurrence matrix)
#' @export
#' @examples
#' # create fitness landscape as instance of FitLandDF object
#' a <- round(runif(64))
#' a <- array(a, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(a)
#'
#' # calculate co-occurrence matrix using:
#' #   Manhattan distance of 1
#' #   discretization into 2 equal-sized buckets
#' #   normalization: multiply all elements so that sum of matrix equals unity
#' comat <- get_comatrix(my_landscape,
#'                       discrete = equal_discrete(2),
#'                       neighbor = manhattan(1))
#'
#' # print co-occurrence matrix
#' print(comat)
#'
get_comatrix <- function(x, ...) {
  UseMethod("get_comatrix")
}

# default not defined
#' @rdname comat
#' @export
get_comatrix.default <- function(x, ...) {
  warning("default behavior for get_comatrix not yet defined")
}

# for fitness landscapes
# 1. discretize FL to `nlevels` levels
# 2. initialize co-occurrence matrix with all -1 (zero if actually processed)
# 3. count co-occurrences
# 4. normalize and return
#' @rdname comat
#' @export
#'
get_comatrix.FitLandDF <- function(x,
                                   discrete = equal_discrete(2),             # currently a function from factory - need to export factory
                                   neighbor = manhattan(1),                  # currently a function from factory - need to export factory
                                   normalize = normalize_glcm,               # currently a function
                                   ...) { # doesn't do anything
  # discretize FL (`x`) to `nlevels` levels, equal to integers 1:nlevels
  x$Value <- discrete(x$Value)

  # initialize co-occurrence matrix with all -1 (zero if actually processed)
  nlevels <- x$Value %>% unique %>% length
  comat <- matrix(-1, nrow = nlevels, ncol = nlevels)

  # count co-occurrences
  for (i in seq_len(nrow(comat))) {
    for (j in seq_len(ncol(comat))) {
      comat[i, j] <- count_element_occur(x, i, j, neighbor = neighbor)
    }
  }

  # make symmetric
  comat <- comat + t(comat)

  # normalize
  comat <- normalize(comat)

  # return co-occurrence matrix
  return(comat)
}

#'Method to get comatrix from igraph object + named values
#
#' @rdname comat
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
get_comatrix.igraph <- function(x, values, nlevels=length(unique(values)),
                                normalize = normalize_glcm, verbose = TRUE, ...){


  names_bool <- (!is.null(names(values)) & !is.null(names(igraph::V(x))))
  #handle a situation where the results would be super confusing
  if(length(igraph::V(x)) != length(values)) {
    if(!names_bool) {
      stop("Mismatched number of nodes in provided graph and node attributes.
           Either provide an equal number of nodes and node attributes in values or
           provide a graph with named vertices and named values")
    }
    if(verbose) {
      message("node values not provided for every node, removing nodes without provided attributes from graph")
    }
  }


  #this is gross but we need to join the same values on different columns so I'm going to call them different things
  Value <- discretize(values, nlevels)
  if(names_bool) {
    names(Value) <- names(values)
    val_df1 <- data.frame(V1 = names(Value), val1 = Value)
    val_df2 <- data.frame(V2 = names(Value), val2 = Value)
  } else {
    val_df1 <- data.frame(V1 = 1:length(Value), val1 = Value)
    val_df2 <- data.frame(V2 = 1:length(Value), val2 = Value)
  }

  #now create edge df from edge list
  edge_list <- igraph::as_edgelist(x)
  edge_df <- as.data.frame(edge_list)

  #join the discretised values onto each node
  comat <- dplyr::left_join(edge_df, val_df1, by = c("V1")) %>%
    dplyr::left_join(val_df2, by = c("V2")) %>%
    dplyr::filter(!is.na(.data$val1), !is.na(.data$val2)) %>%
    dplyr::group_by(.data$val1, .data$val2) %>%
    dplyr::summarise(n = dplyr::n())

  #figure out which vals aren't represented in val1 and then also val2
  notin1 <- (1:nlevels)[!(1:nlevels %in% unique(comat$val1))]
  notin2 <- (1:nlevels)[!(1:nlevels %in% unique(comat$val2))]

  if(length(notin1) + length(notin2) != 0) {
    #Need to handle situations where there are mismatches between notin1 and notin2
    notin <- unique(c(notin1,notin2))
    notin_df <- data.frame(val1 = notin, val2 = notin, n = 0)
    comat <- comat %>% bind_rows(notin_df)
  }

  #join with comat
  comat <- comat %>%
    dplyr::arrange(-dplyr::desc(.data$val1)) %>%
    tidyr::pivot_wider(names_from = .data$val2, values_from = n,
                       names_sort = TRUE, values_fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$val1) %>%
    as.matrix()

  comat <- normalize(comat)
  return(comat)
}
