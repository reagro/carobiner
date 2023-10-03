\name{get_metadata}

\alias{get_metadata}

\title{
Get the dataset level meta-data
}

\description{
Get the meta-data file associated with a data set (from the associated json file) and check if the data set major and minor versions have changed.
}


\usage{
get_metadata(cleanuri, path, group="", major=1, minor=0)
}
 
\arguments{
  \item{cleanuri}{character. A "clean" URI made with \code{\link{simple_uri}}}
  \item{path}{the path to the Carob repo}
  \item{group}{optinal, a group name}
  \item{major}{major version to check for, alerts when there's a new major version}
  \item{minor}{minor version to check for, alerts when there's a new minor version}
}

\value{
list
}

