\name{read_carob}

\alias{read_carob}

\title{
Read a processed dataset
}

\description{
Get the processed dataset for a URI
}


\usage{
read_carob(uri)
}
 
\arguments{
  \item{uri}{character. The URI of the dataset of interest}
}

\value{
list with three (or more) elements: metadata, records, and long
}


\examples{
\dontrun{
test <- read_carob("doi:10.25502/07RT-7A40/D")
}
}


