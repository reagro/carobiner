\name{read.excel}

\alias{read.excel}

\title{
Read an excel file
}

\description{
Read an excel file and return a data.frame without generating warnings.
}


\usage{
read.excel(f, ...)
}
 
\arguments{
  \item{f}{character (filename)}
  \item{...}{additional arguments passed on to \code{\link{readxl::read_excel}}}.
}

\value{
data.frame
}

