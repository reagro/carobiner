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

\code{run_carob} runs a single Carob script for a dataset. Carob scripts download data to "carob/data/raw", re-organize th data, check the variable names and controlled vocabulary used, check data types, ranges, and location of the coordinates, and write clean data to "carob/data/clean"

\code{process_carob} runs all the Carob scripts. 

\code{compile_carob} reads all the clean data and combines them into two files, one for the dataset descriptions, one for all records, and saves these to "carob/data")

\code{make_carob} is a convenience function that first runs \code{process_carob} and then runs \code{compile_carob} 
}


\usage{
run_carob(cleanuri, path, group="", quiet=FALSE)
process_carob(path, group="", quiet=FALSE, check=NULL, cache=TRUE)
compile_carob(path, group="", split_license=FALSE, zip=FALSE, excel=FALSE, cache=FALSE)
make_carob(path, group="", quiet=FALSE, check="all", report=FALSE, cache=TRUE, ...)
}
 
\arguments{
  \item{cleanuri}{character. A "clean" URI made with \code{\link[yuri]{simpleURI}}}
  \item{path}{character. Local path to the Carob repo}
  \item{group}{character. group name}
  \item{quiet}{logical. Be quiet?}
  \item{check}{character. "none" to do no checking; "nogeo" to not check coordinates. "all" and all other values to do all checks}
  \item{cache}{logical. If \code{TRUE}, datasets are only recompiled if the R script has changed after the last compilation}
  \item{report}{logical. Should a html report be created for the datasets?}
  \item{split_license}{logical. Make a separate aggregation for data with a CC license?}
  \item{excel}{logical. Also write the results into an excel file?}
  \item{zip}{logical. zip the aggregated data?}
  \item{...}{additional arguments passed to compile_carob}
}

\value{
none. Run for the side effects.
}

