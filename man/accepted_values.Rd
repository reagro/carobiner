\name{accepted_values}

\alias{accepted_values}

\title{
Get accepted values for a variable
}

\description{
Get the accepted values for a variable. 
}


\usage{
accepted_values(name)
}
 
\arguments{
  \item{name}{character. The variable name. For example, "country", "crop", or "fertilizer_type"}
}

\value{
character
}

\examples{
v <- accepted_values("fertilizer_type")
head(v)
}