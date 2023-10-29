\name{metadata}

\alias{get_metadata}
\alias{get_license}

\title{
Get the dataset level meta-data and license
}

\description{
Get the meta-data file associated with a data set (from the associated json file) and check if the data set major and minor versions have changed. 

And get the license information from the object created in \code{\link{get_metadata}}. The fields "license" and "termsOfUse" are concatenated, except if license is "NONE". Create Common license descriptions are abbreviated to their standard form. 
}

\usage{
get_metadata(cleanuri, path, group="", major=1, minor=0)

get_license(x)
}
 
\arguments{
  \item{cleanuri}{character. A "clean" URI made with \code{\link{simple_uri}}}
  \item{path}{the path to the Carob repo}
  \item{group}{optinal, a group name}
  \item{major}{major version to check for, alerts when there's a new major version}
  \item{minor}{minor version to check for, alerts when there's a new minor version}
  \item{x}{list. Created with \code{\link{get_metadata}}}
}

\value{
list (get_metadata) or character (get_license)
}

