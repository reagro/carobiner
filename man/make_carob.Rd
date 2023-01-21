\name{make_carob}

\alias{make_carob}
\alias{compile_carob}
\alias{process_carob}
\alias{run_carob}

\title{
Make the Carob database
}

\description{
Make the Carob database. 

\code{run_carob} runs a single Carob script for a dataset. Carob scripts download data to "carob/data/raw", re-organize th data, check the variable names and controlled vocabulary used, and write clean data to "carob/data/clean"

\code{process_carob} runs all the Carob scripts. 

\code{compile_carob} reads all the clean data and combines them into two files, one for the dataset descriptions, one for all records, and saves these to "carob/data")

\code{make_carob} is a convenience function that first runs \code{process_carob} and then runs \code{clean_carob} 
}


\usage{
run_carob(cleanuri, path, group="", quiet=FALSE)
process_carob(path, group="", quiet=FALSE)
compile_carob(path, group="")
make_carob(path, group="", quiet=FALSE)
}
 
\arguments{
  \item{cleanuri}{character. A "clean" URI made with \code{\link[agro]{get_simple_URI}}}
  \item{path}{character. Local path to the Carob repo}
  \item{group}{character. group name}
  \item{quiet}{logical. Be quiet?}
}

\value{
none. Run for the side effects.
}

