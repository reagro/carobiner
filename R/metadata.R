
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
	trms <- x$data$latestVersion$termsOfUse
	if (is.null(trms)) trms <- x$license
	if (isTRUE(grepl("This dataset is made available without information", trms))) {
		return("unknown")
	}
	if ((is.null(lic) || (lic[1] == "NONE")) && (!is.null(trms))) {
		trm <- strsplit(trms, '\"')[[1]]
		g <- grep("/creativecommons.org/", tolower(trm), value=TRUE)
		if (length(g) == 0) {
			g <- grep("Creative Commons", trm, value=TRUE, ignore.case=TRUE)
			if (length(g) == 0) {
				g <- grep("by-nc-nd", trm, value=TRUE, ignore.case=TRUE)
				if (length(g) > 0) {
					return("CC-BY-NC-ND")
				}
				if (grepl("CIMMYT", trms)) {
					return("CIMMYT license")
				}
			}
  			gg <- regmatches(g, gregexpr('Creative (.+?) license', g, ignore.case=TRUE)) |> unlist()
			if (any(tolower(gg) == "creative commons attribution 4.0 international license")) {
				gg <- "CC-BY-4.0"
			} 
			return(gg[1])
		} else {
			trm <- g[1]
			trm <- gsub("http://", "", trm)
			trm <- gsub("https://", "", trm)
			trm <- gsub("creativecommons.org/licenses", "CC", trm)
			trm <- gsub("creativecommons.org/publicdomain/zero", "CC0", trm)		
			trm <- gsub("/", "-", trm)
			trm <- toupper(gsub("-$", "", trm))
			trm <- toupper(trm)
			trm <- gsub(" ", "-", trm)			
		} 
		if (nchar(trm) > 0) {
			if (is.null(lic) || (lic == "NONE")) {
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
	if (is.list(lic)) {
		lic <- lapply(lic, \(x) gsub(" ", "-", gsub("CC-ZERO", "CC-0", x)))
	} else {
		lic <- gsub(" ", "-", gsub("CC-ZERO", "CC-0", lic))
	}
	if (is.null(lic)) lic <- "unknown"
	lic
}


get_title <- function(x) {
	out <- x$data$latestVersion$metadataBlocks$citation$fields$value[[1]]
	if (is.null(out)) {
		out <- js$result$name
	}
	if (is.null(out)) {
		out <- ""
	}
	out
}

