\name{replace_values}

\alias{replace_values}

\title{
Change values in a vector
}

\description{
Change the values in a vector
}

\usage{
replace_values(x, from, to, must_have=TRUE)
}
 
\arguments{
  \item{x}{vector}
  \item{from}{values to be changed}
  \item{to}{what values need to be changed to}
  \item{must_have}{logical. Give an error if a value is not present}
}

\value{
vector
}


\examples{
v <- LETTERS[1:5]
replace_values(v, c("A", "B", "C"), c("first", "abc", "third"))
}


