% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics-generics.R
\name{cluster_prom}
\alias{cluster_prom}
\alias{cluster_prom.default}
\alias{cluster_prom.matrix}
\alias{cluster_prom.FitLandDF}
\title{Cluster Prominence Metric for a GLCM}
\usage{
cluster_prom(x, ...)

\method{cluster_prom}{default}(x, ...)

\method{cluster_prom}{matrix}(x, ...)

\method{cluster_prom}{FitLandDF}(x, nlevels, ...)
}
\arguments{
\item{x}{gray-level co-occurrence matrix}

\item{...}{additional parameters}

\item{nlevels}{desired number of discrete gray levels}
}
\value{
double
}
\description{
Calculate the cluster prominence feature or metric for a gray-level co-occurrence
matrix. For definition and application, see Lofstedt et al. (2019)
\doi{10.1371/journal.pone.0212110}.
}
\examples{
## calculate cluster prominence of arbitrary GLCM
# define arbitrary GLCM
x <- matrix(1:16, nrow = 4)

# normalize
n_x <- normalize_glcm(x)

# calculate cluster prominence
cluster_prom(n_x)

## calculate cluster prominence of arbitrary fitness landscape
# create fitness landscape using FitLandDF object
vals <- runif(64)
vals <- array(vals, dim = rep(4, 3))
my_landscape <- fitscape::FitLandDF(vals)

# calculate cluster prominence of fitness landscape, assuming 2 discrete gray levels
cluster_prom(my_landscape, nlevels = 2)

## confirm value of cluster prominence for fitness landscape
# extract normalized GLCM from fitness landscape
my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

# calculate cluster prominence of extracted GLCM
cluster_prom(my_glcm)  # should match value of above cluster_prom function call
}
