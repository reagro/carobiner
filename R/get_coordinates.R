
# function to retrieve latitude and longitude data based on the name of a place
# Author: Effie Ochieng' (efyrouwa)
  
 
  get_coordinates <- function(place) {
     rslt<- data.frame(Place = character(),latitude = numeric(),longitude = numeric(), stringsAsFactors = FALSE)
     
     for (place in places) {
       
      url <- paste0("http://api.geonames.org/searchJSON?q=", place, "&maxRows=1&username=", username = "carobiner") 
      response <-httr::GET(url)
    
      if (http_status(response)$category == "Success") {
      data <- content(response, as = "parsed")  
      if (data$totalResultsCount > 0) {
        lat <- data$geonames[[1]]$lat
        lng <- data$geonames[[1]]$lng
        result <- data.frame(Place = place, latitude = lat, longitude = lng)
        results <- rbind(result, rslt)

      } else {
        message("Place not found.", place) 
        result <- data.frame(Place = place, latitude = NA, longitude = NA, stringsAsFactors = FALSE)
        results <- rbind(result, rslt)
      }
    } else {
      message("Request failed.",place)
      result <- data.frame(Place = place, latitude = NA, longitude = NA, stringsAsFactors = FALSE)
      results <- rbind(result, rslt)
    }
      print(results)
  }     
}
  
  
  