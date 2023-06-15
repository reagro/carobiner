
# function to retrieve latitude and longitude data based on the name of a place
# Author: Effie Ochieng' (efyrouwa)
  

geocode_geonames <- function(place, username=NULL) { 
	stopifnot(!is.null(username))
    u <- paste0("http://api.geonames.org/searchJSON?q=", place, "&maxRows=1&username=", username) 
	out <- rep("", length(u))
	for (i in 1:length(u)) {
		print(place[i]); flush.console()
		response <- try( httr::GET(u[i]) )
		if (!inherits(response, "try-error")) {
			out[i] <- httr::content(response, as="text")  
		} 
	}
	out
}
  
  
geocode_nominatim <- function(place) {
	url <- "https://nominatim.openstreetmap.org/search?q="
	u <- paste0(url, place, "&format=geojson&polygon_geojson=1")
	out <- rep("", length(u))
	for (i in 1:length(u)) {
		print(place[i]); flush.console()
		response <- try( httr::GET(u[i]) )
		if (!inherits(response, "try-error")) {
			out[i] <- httr::content(response, as="text")  
		} 
	}
	out
} 


geocode <- function(service="nominatim", country, place, adm1=NULL, adm2=NULL, adm3=NULL, adm4=NULL, adm5=NULL, ...) {
	service <- tolower(service)
	service <- match.arg(service, c("nominatim", "geonames"))

	addr <- cbind(country, adm1, adm2, adm3, adm4, adm4, place)
	addr <- apply(addr, 1, \(v) paste(v, collapse=","))
	addr <- gsub("(,)\\1+", "\\1", addr)
	addr <- gsub("( )\\1+", "\\1", addr)
	addr <- gsub(", ,", ",", addr)
	addr <- gsub(" ", "+", addr)
	if (service == "nominatim") {
		x <- geocode_nominatim(addr)
	} else {
		x <- geocode_geonames(addr, ...)	
	}
	## to do
	## further processing of x
	x
}

#g <- geocode("nominatim", "Kenya", c("Machakos", "Nairobi"))
  