\name{check_terms}

\alias{check_terms}

\title{
Check the terms uses in a dataset
}

\description{
Check the terms used in a dataset. A warning is given if bad or missing terms or vocabulary are encountered. 
}

\usage{
check_terms(metadata, records, timerecs=NULL, wth=NULL, group="", check="all")
}
 
\arguments{
  \item{metadata}{data.frame with metadata}
  \item{records}{data.frame with observations}
  \item{timerecs}{data.frame with temporal data}
  \item{wth}{data.frame with weather data}
  \item{group}{character. Optional group name}
  \item{check}{character. "none" to do no checking; "nogeo" to not check coordinates. "all" and all other values to do all checks}
}

\value{
data.frame
}

