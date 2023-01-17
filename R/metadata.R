
get_metadata <- function(cleanuri, path, group="", major=1, minor=0) {
	jf <- file.path(path, "data", "raw", group, cleanuri, paste0(cleanuri, ".json"))
	x <- jsonlite::fromJSON(readLines(jf))
	jmajor <- x$data$latestVersion$versionNumber 
	if (!is.null(jmajor)) {
		jminor <- x$data$latestVersion$versionMinorNumber 
		if (jmajor != major) stop(paste("new major version", jmajor, "for", cleanuri), call.=FALSE)
		if (jminor != minor) warning(paste("new minor version", jminor, "for", cleanuri), call.=FALSE)
	} else { # ckan
		v <- x$result$version
		if (!is.null(v)) {
			if (v != major) stop(paste("new version", v, "for", cleanuri), call.=FALSE)
		}
	}
	x
}

get_license <- function(x) {
	lic <- x$data$latestVersion$license
	trm <- x$data$latestVersion$termsOfUse
	if ((is.null(lic) || (lic[1] == "NONE")) && (!is.null(trm))) {
		trm <- strsplit(trm, '\"')[[1]]
		g <- grep("/creativecommons.org/", tolower(trm))
		if (length(g) == 0) {
			g <- grep("Creative Commons", trm, value=TRUE, ignore.case=TRUE)
			if (length(g) == 0) {
				return("Unknown")
			}
  			g <- regmatches(g, gregexpr('Creative (.+?) license', g, ignore.case=TRUE))[[1]]
			if (tolower(g) == "creative commons attribution 4.0 international license") {
				g <- "CC-BY (4.0)"
			}
			return(g)
		}
		if (length(g) > 0) {
			trm <- trm[g[1]]
			trm <- gsub("http://", "", trm)
			trm <- gsub("https://", "", trm)
			trm <- gsub("creativecommons.org/licenses", "CC", trm)
			trm <- gsub("/", "-", trm)
			trm <- toupper(gsub("-$", "", trm))
			trm <- toupper(trm)
		} 
		if (nchar(trm) > 0) {
			if (lic == "NONE") {
					lic <- trm	
			} else {
				lic <- paste0(lic, "; ", trm)
			}
		}
	} else if (is.null(lic)) { #ckan
		lic <- x$result$license_id 	
		if (is.null(lic)) lic <- "?"
		lic <- toupper(lic)
	}
	lic
}

