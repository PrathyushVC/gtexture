#' Marginal distributions of the GLCM 
#' 
#' @description
#' Functions for the calculation of marginal distributions from the GLCM matrix.
#' 
#' @param glcm A normalized co-occurrence matrix as generated by previous
#' @param k summation equal to k 
#' @export
#' @name glcm_marginals

#### All GLCM functions are applied to normalised co-occurrence matrices
# These are square matrices of size n_levels x n_levels
# where n_levels is the number of discretized fitness levels
# To generate a random co-occurrence matrix in a system with 3 levels
# x = matrix(sample.int(9, size = 9, replace = TRUE), ncol = 3)
# x = x / sum

#### Sum over
# Sum each element where the indices of the matrix sum to k
#' @description
#' The partial sum of the matrix is used to determine the distribution over 
#' the sum of neighbor pairs, returns a value for a given sum k. 
#' @param glcm square co-occurrence matrix
#' @param k real integer (to sum)
#' @returns int or double (xplusy_k: sum of matrix entries with given index sum)
#' @rdname glcm_marginals
xplusy_k <- function(glcm, k){
  sum <- 0
  map <- as.numeric(colnames(glcm))
  for(value in map){
    target <- k - value
    #check if target in map
    targetInMap <- which(map == target)
    if( length(targetInMap > 0) ) 
      sum <- sum + glcm[targetInMap, which(map == value)]
    #Stop early to avoid checking values which
    #are larger than k (i.e. could never sum to k)
    if(value > k) break
  }
  sum
  }

#### Sum of difference pairs
#Sum of matrix elements where the indices of the entry have a difference of k
#' Conditional sum of matrix entries with given difference 
#' 
#' @description The partial sum of the matrix is used to determine the distribution over 
#' the sum of neighbor pairs, returns a value for a given difference of k. 
#' @param glcm square co-occurrence matrix
#' @param k real integer (given difference)
#' @returns int or double (sum of matrix entries with given index difference)
#' @rdname glcm_marginals 
#' 
xminusy_k <- function(glcm, k){
  #Sum each element where the indices of the matrix sum to k
  sum <- 0
  map <- as.numeric(colnames(glcm))
  for(value in map){
    target <- k + value
    #check if target in map
    targetInMap <- which(map == target)
    if( length(targetInMap > 0) ) sum <- sum + glcm[targetInMap, which(map == value)]
  }

  #This ensures that values on both sides of the symmetrical matrix are accounted for
  #if k = 0 it's only values along the main diagonal, and thus doesn't need doubling
  if(k == 0){ return(sum) }
  return(sum*2)
}

#' @param glcm co-occurrence matrix
#' @noRd
#' 
p_xsuby.matrix <- function(glcm) {
  nlevels=dim(glcm)[1]
  #p_k=c()
  p_k = rep(0, nlevels)
  for (i in 1:nlevels){
    for (j in 1:nlevels){
      k = abs(i-j)
      p_k[k+1] = p_k[k+1] + glcm[i,j]
    }
  }
  return(p_k)
}

### Matrix PXSubY
#' @describeIn glcm_metrics psubxy
#' @noRd
#' @param x landscape
p_xsuby.FitLandDF <- function(x) {
  nlevels=dim(x)[1]
  #p_k=c()
  p_k = rep(0, nlevels)
  for (i in 1:nlevels){
    for (j in 1:nlevels){
      k = abs(i-j)
      p_k[k+1] = p_k[k+1] + x[i,j]
    }
  }
  return(p_k)
}

### FitLand PXPlusY
#' @describeIn glcm_metrics plusxy
#' @noRd
#' @param x matrix 
p_xplusy.matrix <- function(x) {
  nlevels=dim(x)[1]
  #p_k=c()
  p_k = rep(0, 2*nlevels)
  for (i in 1:nlevels){
    for (j in 1:nlevels){
      k = i+j
      p_k[k] = p_k[k] + x[i,j]
    }
  }
  return(p_k)
}

### FitLand PXPlusY
#' landscape xpy 
#' 
#' @describeIn glcm_metrics plusxy landscape
#' @noRd
#' @param x landscape 
p_xplusy.FitLandDF <- function(x) {
    nlevels=dim(x)[1]
    #p_k=c()
    p_k = rep(0, 2*nlevels)
    for (i in 1:nlevels){
      for (j in 1:nlevels){
        k = i+j
        p_k[k] = p_k[k] + x[i,j]
      }
    }
    return(p_k)
}

#' Statistics of GLCM 
#' 
#' @description
#' Functions for the calculation of summary statistics upon the GLCM matrix.
#' 
#' @param glcm A normalized co-occurrence matrix as generated by previous
#' @export
#' @name glcm_statistics


#' @description
#'  glcm_mean: GLCM mean of a symmetric GLCM matrix.
#'  The GLCM Mean is not simply the average of all the original node values
#'  in the network/graph. It is expressed in terms of the GLCM. The node 
#'  value is weighted not by its frequency of occurrence by itself (as in a
#'   "regular" or familiar mean but by its frequency of its occurrence in 
#'   combination with a certain neighbour node value; see 
#'   \url{https://prism.ucalgary.ca/server/api/core/bitstreams/8f9de234-cc94-401d-b701-f08ceee6cfdf/content}
#' 
#' @param glcm 
#' @returns int or double (glcm_mean: single value, mean of symmetric glcm)
#' @rdname glcm_statistics
#' 
glcm_mean <- function(glcm){
  #Mean for symmetric glcm
  if (isSymmetric(glcm)){return(sum(seq(1,dim(glcm)[1]) * colSums(glcm)))}
  else {return(sum(seq(1,dim(glcm)[2]) * rowSums(glcm)))}
}

#' Mean df 
#' 
#' @describeIn glcm_metrics glcm_mean.df
#' @noRd
glcm_mean.df <- function(glcm){
  return(sum(as.numeric(colnames(glcm)) * colSums(glcm)))
}

#' Mean matrix 
#' 
#' @describeIn glcm_metrics glcm_mean.matrix
#' @noRd
glcm_mean.matrix <- function(glcm){
  return(sum(seq(1,dim(glcm)[1]) * colSums(glcm)))
}

#' Mean over the column values (x/i) 
#' 
#' @param glcm co-occurrence matrix
#' @returns int or double (mu_x: weighted mean of reference node values)
#' @rdname glcm_statistics
mu_x.matrix <- function(glcm){
  return(sum(seq(1,dim(glcm)[1]) * colSums(glcm)))
}

#' Mean over the row values (y/j) 
#' 
#' @param glcm co-occurrence matrix
#' @returns int or double (mu_y: weighted mean of neighbor node values)
#' @rdname glcm_statistics
#' 
mu_y.matrix <- function(glcm){
  return(sum(seq(1,dim(glcm)[2]) * rowSums(glcm)))
}

#### Variance #####
#' Variance of gray-level differences
#' @description
#' glcm_variance: The variance of the GLCM values
#' @returns int or double (glcm_variance: glcm variance)
#' @param glcm gray level co-occurrence matrix 
#' @rdname glcm_statistics 
glcm_variance <- function(glcm){
  sum <- 0
  mean <- mu_x.matrix(glcm)
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + ((((i-1) - mean)^2) * glcm[i,j])
    }
  }
  return(sum)
}


#### SUM AVERAGE #####
# Sum average is the mean value of the sum in marginal distribution px+y
#' @describeIn glcm_metrics Sum Average
#' @noRd
#' @param x landscape
sum_avg.FitLandDF <- function(x) {
  p_xplusy = p_xplusy.FitLandDF(x)[-1]
  nlevels=dim(x)[1]
  i = 2:(2*nlevels)
  sum_avg = i * p_xplusy
  return(sum(sum_avg))
}

#### SUM ENTROPY #####
# Sum entropy is the entropy of marginal distribution px+y
#' @describeIn glcm_metrics SumEntropy
#' @param x gray level co-occurrence matrix 
#' @noRd
sum_entropy.matrix<- function(x) {
  xplusy = p_xplusy.matrix(x)[-1] #Starts at index 2
  sum_entropy = - xplusy * log (xplusy)
  return(sum(sum_entropy, na.rm=TRUE))
}

### SumEntropyLandscape
#' Sum Entropy of Landscape
#' 
#' @describeIn glcm_metrics landscape SumEntropyLandscape
#' @noRd
#' @param x landscape 
#' 
# Sum entropy is the entropy of marginal distribution px+y
sum_entropy.FitLandDF <- function(x) {
  xplusy = p_xplusy.FitLandDF(x)[-1] #Starts at index 2
  sum_entropy = - xplusy * log (xplusy)
  return(sum(sum_entropy, na.rm=TRUE))
}

#### DIFFERENCE ENTROPY #####
#' Difference entropy is the entropy of marginal distribution of the 
#' difference in gray-level value equivalents x-y
#' 
#' @returns float (single value: the entropy of the marginal distribution) 
#' @param glcm gray level co-occurrence matrix
#' @param base Base of the logarithm in differenceEntropy.
#' @export
#' @examples
#' # Calculate difference entropy of a given glcm (e.g. uniform matrix)
#' differenceEntropy.matrix(matrix(1,3,3))
#' 
differenceEntropy.matrix <- function(glcm, base=2){
  sum <- 0
  for(i in 1:(nrow(glcm)-1)){
    pxy <- xminusy_k(glcm, i-1)
    sum <- sum + ifelse(pxy > 0, pxy*logb(pxy,base=base),0)
  }
  return(-1*sum)
}

#### DISSIMILARITY #####
#' Dissimilarity of a co-occurrence matrix 
#' 
#' @description
#' Dissimilarity is the weighted sum of all of the absolute differences in the gray-levels assigned
#' to neighboring nodes across the network or graph. For example, a diagonal matrix would represent 
#' only identical neighbors and no dissimilarity.
#' 
#' @returns int or double (the weighted sum of differences)
#' @param glcm gray-level co-occurrence matrix
#' @export
#' @examples
#' # Calculate dissimilarity of a 2x2 uniform matrix
#' dissimilarity.matrix(matrix(1,2,2))
#' 
#' # Calculate dissimilarity of a diagonal matrix
#' dissimilarity.matrix(diag(1,5,5))
#' 
#' # Calculate dissimilarity of a sequential matrix
#' dissimilarity.matrix(matrix(1:16,4,4))
#' 
dissimilarity.matrix <- function(glcm){
  sum <- 0
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + (abs(i - j))*glcm[i,j]
    }
  }
  return(sum)
}
