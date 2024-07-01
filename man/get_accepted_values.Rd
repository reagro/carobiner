\name{get_accepted_values}

\alias{get_accepted_values}

\title{
Get accepted values for a variable
}

\description{
Get the accepted values for a variable. 
}


\usage{
get_accepted_values(name)
}
 
\arguments{
  \item{name}{character. The variable name. For example, "country", "crop", or "fertilizer_type"}
}

\value{
character
}

\examples{
v <- get_accepted_values("fertilizer_type")
head(v)
}