\name{get_metadata}

\alias{get_metadata}

\title{
Get the dataset level meta-data
}

\description{
Get the meta-data file associated with a data set (from the json file) and check if the data set major and minor versions have changed.
}


\usage{
get_metadata(cleanuri, path, group="", major=1, minor=0)
}
 
\arguments{
  \item{cleanuri}{character. A "clean" uri made with \code{\link[agro]{get_simple_URI}}}
  \item{path}{the path to the Carob repo}
  \item{group}{optinal, a group name}
  \item{major}{major version to check for}
  \item{minor}{minor version to check for}
}

\value{
list
}

