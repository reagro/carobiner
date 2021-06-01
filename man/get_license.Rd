\name{get_license}

\alias{get_license}

\title{
Get the JSON file
}

\description{
Get the license associated with a data set. The fields "license" and "termsOfUse" are concatenated, except if license is "NONE". Create Common license descriptions are abbreviated to their standard form. 
}


\usage{
get_license(x)
}
 
\arguments{
  \item{x}{list. Created with \code{\link{get_metadata}}}
}

\value{
character
}

