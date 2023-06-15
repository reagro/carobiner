
  

geocode_geonames <- function(place, username=NULL) { 
	# Author: Effie Ochieng' (efyrouwa)
	stopifnot(!is.null(username))
    u <- paste0("http://api.geonames.org/searchJSON?q=", place, "&username=", username) 
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
	x <- sapply(out, terra::svc)
	names(x) <- place
	
	y <- lapply(x, \(s) {
			if (length(s) == 0) {
				cbind(NA, NA)
			} else {
				v <- s[1]
				v <- v[which.min(v$place_rank)]
				crds(centroids(v))
			}
		})
	y <- do.call(rbind, y)
	colnames(y) <- c("longitude", "latitude")
	y <- data.frame(place=place, y)		
	list(df=y, svc=x)
} 


geocode <- function(country, place, adm1=NULL, adm2=NULL, adm3=NULL, adm4=NULL, adm5=NULL, service="nominatim", ...) {
	service <- tolower(service)
	service <- match.arg(service, c("nominatim", "geonames"))

	stopifnot(!any(is.na(country)))

	addr <- trimws(cbind(country, adm1, adm2, adm3, adm4, adm4, place))
	addr[is.na(addr)] <- ""
	noc <- addr[,-1, drop=FALSE]
	noc <- apply(noc, 1, \(i) paste0(i, collapse=""))
	if (any(noc=="")) {
		stop("all records must include a country and a place or admin name")
	}
	addr <- apply(addr, 1, \(v) paste(v, collapse=","))
	addr <- gsub("(,)\\1+", "\\1", addr)
	addr <- gsub("( )\\1+", "\\1", addr)
	addr <- gsub(", ,", ",", addr)
	addr <- gsub(" ", "+", addr)
	addr <- utils::URLencode(addr)
	
	if (service == "nominatim") {
		geocode_nominatim(addr)
	} else {
		geocode_geonames(addr, ...)	
	}
}

#g <- geocode("nominatim", "Kenya", c("Machakos", "Nairobi"))
  