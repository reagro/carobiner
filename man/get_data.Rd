\name{get_data}

\alias{get_data}

\title{
Download a data set
}

\description{
Download the files associated with a URI. 
}


\usage{
get_data(uri, path, group, cache=TRUE)
}
 
\arguments{
  \item{uri}{character. The URI of a dataset}
  \item{path}{character. The path to the Carob repo}
  \item{group}{character. The group name}
  \item{cache}{logical. if \code{TRUE} the data are only downloaded again if they are not locally available}
}

\value{
character
}
