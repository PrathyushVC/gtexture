% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discrete.R
\name{kmeans_discrete}
\alias{kmeans_discrete}
\title{Kmeans clustering discretization}
\usage{
kmeans_discrete(nlevels)
}
\arguments{
\item{nlevels}{number of levels to split continuous vector into}
}
\value{
function that makes a numeric vector discrete
}
\description{
Splitting of a vector of continuous values into k groups
using kmeans. Used to discretize node labels/weights of a fitness landscape
or node-weighted graph.
}
