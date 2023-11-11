\name{read.excel}

\alias{read.excel}

\title{
Read an excel file
}

\description{
Read an excel file and return a data.frame without generating warnings.
}


\usage{
read.excel(f, fix_names=FALSE, lower=FALSE, ...)
}
 
\arguments{
  \item{f}{character (filename)}
  \item{fix_names}{logical. If \code{TRUE} variable names are made valid. Spaces and other characters such as parenthesis are replaced by a dot. Multiple dots are replaced by a single dot}
  \item{lower}{logical. If \code{TRUE}, returned column names are all in lowercase}
  \item{...}{additional arguments passed on to \code{\link[readxl]{read_excel}}}.
}

\value{
data.frame
}

\seealso{
\code{\link{read.excel.hdr}}
}
