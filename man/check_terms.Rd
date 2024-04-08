\name{check_terms}

\alias{check_terms}

\title{
Check the terms uses in a dataset
}

\description{
Check the terms used in a dataset. A warning is given if bad or missing terms or vocabulary are encountered. 
}


\usage{
check_terms(dataset, records, path=NULL, group="", check = "all")
}
 
\arguments{
  \item{dataset}{data.frame}
  \item{records}{data.frame}
  \item{path}{character. Local path to the Carob repo}
  \item{group}{character. Optional group}
  \item{check}{character. "none" to do no checking; "nogeo" to not check coordinates. "all" and all other values to do all checks}
}

\value{
logical (invisibly). \code{FALSE} is a check did not pass.
}

