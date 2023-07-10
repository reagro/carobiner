\name{write_files}

\alias{write_files}

\title{
Write Carob dataset files
}

\description{
Write Carob dataset files. This calls \code{\link{check_terms}}
}


\usage{
write_files(path, dataset, records, timerecs=NULL, id=NULL)
}
 
\arguments{
  \item{path}{character. Local path to the Carob repo}
  \item{dataset}{data.frame. One row with dataset level variables}
  \item{records}{data.frame. Dataset records}
  \item{timerecs}{data.frame. Dataset with temporal records}
  \item{id}{character. Optional identifies if a dataset has multiple subdatasets}
}

\value{
none. Used for the side-effect of writing to disk.
}

