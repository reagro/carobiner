\name{get_more_data}

\alias{get_more_data}

\title{
Download additional files
}

\description{
Download additional files associated with a dataset from a different source (e.g. a pdf file). 
}


\usage{
get_more_data(url, dataset_id, path, group)
}
 
\arguments{
  \item{url}{character. The URL where of the resource to be downloaded}
  \item{dataset_id}{character. The dataset ID (see \code{\link{simple_uri}}}
  \item{path}{character. The path to the Carob repo}
  \item{group}{character. The group name}
}

\value{
character (the local filename)
}
