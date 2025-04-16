\name{on_github}

\alias{on_github}

\title{
What datasets have been processed?
}

\description{
Get a data.frame with the datasets that have been processed and are on the reagro/carob github site
}


\usage{
on_github(uri=NULL)
}
 
\arguments{
  \item{uri}{character. A standard URI to check for}
}

\value{
data.frame with variables: dataset, group, and pending

\code{dataset} is the identifier derived from the URI, see \code{\link[yuri]{simpleURI}}.
if a dataset is \code{pending}, the script is in the \code{_pending} folder and needs more work before it can be included.
}


\examples{
\dontrun{
all <- on_github()

test <- on_github("doi:10.25502/07RT-7A40/D")
}
}

