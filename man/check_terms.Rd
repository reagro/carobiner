\name{check_terms}

\alias{check_terms}

\title{
Check the terms uses in a dataset
}

\description{
Check the terms used in a dataset. A warning is given if bad or missing terms or vocabulary are encountered. 
}


\usage{
check_terms(x, type, path)
}
 
\arguments{
  \item{x}{data.frame}
  \item{type}{character. "dataset" or "records"}
  \item{path}{character. Local path to the Carob repo}
}

\value{
logical (invisibly). \code{FALSE} is a check did not pass.
}

