\name{find_outliers}

\alias{find_outliers}

\title{
Find outliers
}

\description{
Find outliers with the IQR (numbers should be less than 1.5 times the interquartile range away from the quartiles) or with the STD (numbers should be less than 3 times the standard deviation away from the mean) 
}


\usage{
find_outliers(x, fields, method="IQR")
}
 
\arguments{
  \item{x}{data.frame}
  \item{fields}{character. variable names in x}
  \item{method}{"IQR" or "STD"}
}

\value{
list
}


\examples{
set.seed(3)
d <- data.frame(a = runif(50), b=rnorm(50))
d$a[1] <- 100
find_outliers(d, c("a", "b"))

}


