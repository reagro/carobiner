\name{make_carob}

\alias{make_carob}
\alias{compile_carob}
\alias{process_carob}

\title{
Make the Carob database
}

\description{
Make the Carob database. 

\code{process_carob} runs all the dataset scripts (download to "carob/data/raw", format, check and write clean data to "carob/data/clean")

\code{compile_carob} reads all the clean data and combines them into two files, one for the dataset descirptions, one for all records, and saves these to "carob/data")

\code{make_carob} is a convenience function that first runs \code{process_carob} and then runs \code{clean_carob} 
}


\usage{
process_carob(path, quiet=FALSE)
compile_carob(path)
make_carob(path, quiet=FALSE)
}
 
\arguments{
  \item{path}{character. Local path to the Carob repo}
  \item{quiet}{logical. Be quiet?}
}

\value{
none. Run for the side effects.
}

