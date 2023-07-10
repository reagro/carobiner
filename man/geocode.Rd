\name{geocode}

\alias{geocode}
\alias{geocode_duplicates}


\title{
Find coordinates for geographic locations
}

\description{
Find coordinates for geographic locations
}


\usage{
geocode(country, location, adm1=NULL, adm2=NULL, adm3=NULL, adm4=NULL, adm5=NULL, service="nominatim", ...)

geocode_duplicates(x, vars=NULL)
}
 
\arguments{
  \item{country}{character. Cannot be missing}
  \item{location}{character or NULL}
  \item{adm1}{character or NULL}
  \item{adm2}{character or NULL}
  \item{adm3}{character or NULL}
  \item{adm4}{character or NULL}
  \item{adm5}{character or NULL}
  \item{service}{character. One of "nominatim" (more to follow)}
  \item{...}{additional arguments to pass to the service (e.g., "username=myname")}

  \item{x}{data.frame with carob records}
  \item{vars}{variable names to use. If not supplied all available information is used}
}

\value{
list
}

