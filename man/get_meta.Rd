\name{get_metadata}

\alias{get_metadata}

\title{
Get the dataset level metadata
}

\description{
Get the metadata file associated with a data set (from the json file) and check if the data set major and minor versions have changed.
}


\usage{
get_metadata(cleanuri, path, major, minor)
}
 
\arguments{
  \item{cleanuri}{character. A "clean" uri made with \code{\link{clean_uri}}}
  \item{path}{the path to the Carob repo}
  \item{major}{major version to check for}
  \item{minor}{minor version to check for}
}

\value{
list
}

