\name{bindr}

\alias{bindr}

\title{
rbind data.frames that do not necessarily have the same column names
}

\description{
rbind data.frames possibly with different column names
}


\usage{
bindr(...)
}
 
\arguments{
  \item{...}{data.frames}
}

\value{
data.frame
}


\examples{
x <- data.frame(a=1, b=2)
y <- data.frame(b=4, d=5)
bindr(x, y)
}


