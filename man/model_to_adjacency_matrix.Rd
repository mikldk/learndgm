% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcherry-generate-all.R
\name{model_to_adjacency_matrix}
\alias{model_to_adjacency_matrix}
\title{Model to adjacency matrix}
\usage{
model_to_adjacency_matrix(model)
}
\description{
Model to adjacency matrix
}
\examples{
modellist <- all_tcherries(4, 2)
m <- modellist$models
model_to_adjacency_matrix( m[[1]] )
model_to_adjacency_matrix( m[[2]] )

}
