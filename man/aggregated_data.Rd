\name{aggregated_data}

\alias{aggregated_data}

\title{
Get the an aggregated dataset
}

\description{
Get the an aggregated dataset. The dataset only exists after you have run \code{\link{make_carob}}
}

\usage{
aggregated_data(path, group, cc=FALSE)
}
 
\arguments{
  \item{path}{the path to the Carob repo}
  \item{group}{optional, a group name}
  \item{cc}{logical. If \code{TRUE} only CC licensed data is returned}
}

\value{
data.frame
}

