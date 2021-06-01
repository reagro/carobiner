\name{write_files}

\alias{write_files}

\title{
Write Carob dataset files
}

\description{
Write Carob dataset files. This calls \code{\link{check_terms}}
}


\usage{
write_files(dataset, records, path, cleanuri, id=NULL)
}
 
\arguments{
  \item{dataset}{data.frame. One row with dataset level variables}
  \item{records}{data.frame. Dataset records}
  \item{path}{chacter. Local path to the Carob repo}
  \item{cleanuri}{character. A "clean" uri made with \code{\link{clean_uri}}}
  \item{id}{character. Optional identifies if a dataset has multiple subdatasets}
}

\value{
none. Used for the side-effect of writing to disk.
}

	