\name{clean_uri}

\alias{clean_uri}

\title{
Clean an URI so that it can be used as a filename
}

\description{
Replace "/" and ":" in an URI with "_" so that they can be used as filenames
}


\usage{
clean_uri(x, reverse=FALSE)
}
 
\arguments{
  \item{x}{character, a URI}
  \item{reverse}{logical. If \code{TRUE}, the reverse operation is done}
}

\value{
character
}


\examples{
uri <- "doi:10.7910/DVN/UNLRGC"
u <- clean_uri(uri)
u
clean_uri(u, reverse=TRUE)

}


