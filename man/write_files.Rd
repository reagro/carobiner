\name{write_files}

\alias{write_files}

\title{
Write Carob dataset files
}

\description{
Write Carob dataset files. This calls \code{\link{check_terms}}
}


\usage{
write_files(path=NULL, metadata, records, timerecs=NULL, wth=NULL, id=NULL, options=NULL)
}
 
\arguments{
  \item{path}{character. \code{NULL}, or the path to the local Carob repo}
  \item{metadata}{data.frame. One row with dataset level descriptive variables}
  \item{records}{data.frame. Dataset records}
  \item{timerecs}{data.frame. Dataset with temporal records}
  \item{wth}{data.frame with weather data}
  \item{id}{character. Optional identifies if a dataset has multiple subdatasets}
  \item{options}{list}
}

\value{
if \code{path=NULL} a list of data.frames (metadata and records). Otherwise a logical value indicating if the standardized dataset was succesfully written to the Carob repo.

