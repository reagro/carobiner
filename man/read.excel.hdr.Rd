\name{read.excel.hdr}

\alias{read.excel.hdr}

\title{
Read an excel file
}

\description{
Read an excel file where the variable names are spread over more than one column (for other cases use \code{\link{read.excel}}.
}


\usage{
read.excel.hdr(f, skip, hdr=1, fix_names=TRUE, ...) {

}
 
\arguments{
  \item{f}{character (filename)}
  \item{skip}{non-negative integer. How many rows to skip to reach the values}
  \item{hdr}{positive integer. How many rows are used for the variable names}
  \item{fix.names}{logical. If \code{TRUE} variable names are made valid. Spaces and other characters such as parenthesis are replaced by a dot. Multiple dots are replaced by a single dot}
  \item{...}{additional arguments passed on to \code{\link[readxl]{read_excel}}}.
}

\value{
data.frame
}

\seealso{
\code{\link{read.excel}}
}