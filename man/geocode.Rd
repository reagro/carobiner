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

}
 
\arguments{
  \item{country}{character or vector. cannot be missing, cannot contain NAs}
  \item{location}{character or vector.cannot be missing,cannot contain NAs}
  \item{adm1}{character, vector or NULL}
  \item{adm2}{character, vector or NULL}
  \item{adm3}{character, vector or NULL}
  \item{adm4}{character, vector or NULL}
  \item{adm5}{character, vector or NULL}
  \item{service}{character. two "nominatim","geonames" (more to follow)}
  \item{...}{character,additional arguments to pass to the service (e.g., "username=myname")}

  \item{x}{data.frame with carob records}
  \item{vars}{variable names to use in identifying duplicates, if not supplied all the available information is used}
}

\value{
list
}

\examples{
  x <- “wheat.csv” 

  h <- geocode (country = x$country, location = x$adm2, service = “nominatim”) 

  h <- geocode (country = x$country, location = x$adm2, service = “geonames”, username = “myname”) 
  
  d <- geocode_duplicates(x, vars = NULL)
  
}


