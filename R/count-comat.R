#####CO-OCCURRENCE COUNTS#####
#' Co-occurrence calculation for a fitness landscape
#' 
#' Converting fitness landscape to a matrix containing counts of
#' equivalent neighboring values. 
#' 
#' @import dplyr
#' @param fitland fitness landscape
#' @param neighbor neighbor value
#' @param from_val from 
#' @param to_val to 
#' @noRd
#' 
count_element_occur <- function(fitland, from_val, to_val,
                                neighbor) { # function to determine neighbors
  # counter
  counter <- 0
  ndim <- length(fitscape::dims(fitland))
  
  # get list of elements in fitscape equal to from_val
  from_coord <- fitland %>%
    dplyr::filter(.data$Value == from_val)
  
  # go through each from_coord and filter list of to_coord,
  #   then count co-occurrences
  for (i in seq_len(nrow(from_coord))) {
    curr_from <- from_coord[i, seq_len(ndim)] %>% as.integer
    
    curr_to <- fitscape::extract_df(fitland) %>%
      dplyr::filter(.data$Value == to_val) %>%
      dplyr::mutate(near = FALSE)
    for (j in seq_len(nrow(curr_to))) {
      curr_to_vec <- curr_to[j, seq_len(ndim)] %>% as.integer
      
      # ignore if looking at own spot
      if (all(curr_from == curr_to_vec)) next
      
      if (neighbor(curr_from, curr_to_vec)) {
        curr_to$near[j] <- TRUE
      }
    }
    curr_to <- dplyr::filter(curr_to, near)
    if (nrow(curr_to) > 0) counter <- counter + 1
    
    #print(curr_from)
    #print(curr_to)
    #print("\n")
  }
  return(counter)
}
