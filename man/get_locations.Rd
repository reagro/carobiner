\name{get_locations}

\alias{get_locations}

\title{
Get the compiled location data
}

\description{
Get the compiled location data for use in georeferencing. The dataset only exists after you have run \code{\link{make_carob}}. 
}

\usage{
get_locations(path, country = NULL)
}
 
\arguments{
  \item{path}{the path to the Carob repo}
  \item{country}{character or NULL to subset the data to a country}
}

\value{
data.frame
}

