\name{get_data}

\alias{get_data}

\title{
Download a data set
}

\description{
Download the data file associated with a URI. This function calls \code{agro::get_data_from_uri}
}


\usage{
get_data(uri, path, group="")
}
 
\arguments{
  \item{uri}{character. A dataset URI}
  \item{path}{character. The path to the Carob repo}
  \item{group}{character. Optional, a group name}
}

\value{
character
}
