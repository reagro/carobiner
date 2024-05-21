\name{write_files}

\alias{write_files}

\title{
Write Carob dataset files
}

\description{
Write Carob dataset files. This calls \code{\link{check_terms}}
}


\usage{
write_files(path, dataset, records, timerecs=NULL, wth=NULL, id=NULL, options=NULL)
}
 
\arguments{
  \item{path}{character. Local path to the Carob repo}
  \item{dataset}{data.frame. One row with dataset level variables (metadata)}
  \item{records}{data.frame. Dataset records}
  \item{timerecs}{data.frame. Dataset with temporal records}
  \item{wth}{data.frame with weather data}
  \item{id}{character. Optional identifies if a dataset has multiple subdatasets}
  \item{options}{list}
}

\value{
none. Used for the side-effect of writing to disk.
}

