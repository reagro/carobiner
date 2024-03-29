\name{geocode}

\alias{geocode}


\title{
Find coordinates for geographic locations
}

\description{
Find coordinates for geographic locations
}


\usage{
geocode(country, location, adm1=NULL, adm2=NULL, adm3=NULL, adm4=NULL, adm5=NULL, service="nominatim", ...)

}
 
\arguments{
  \item{country}{character or vector. Cannot be missing or contain \code{NA}s}
  \item{location}{character or vector. Cannot be missing or contain \code{NA}s}
  \item{adm1}{character or NULL}
  \item{adm2}{character or NULL}
  \item{adm3}{character or NULL}
  \item{adm4}{character or NULL}
  \item{adm5}{character or NULL}
  \item{service}{character. one of "nominatim" or "geonames"}
  \item{...}{character,additional arguments to pass to the service (e.g., "username=myname")}
}

\value{
list
}

\examples{
 h <- geocode (country="United States", adm1="California", location="San Francisco") 
}


