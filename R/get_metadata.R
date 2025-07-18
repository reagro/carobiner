
.old_check_version <- function(x, cleanuri, major=1, minor=0) {
	jmajor <- x$data$latestVersion$versionNumber 
	if (!is.null(jmajor)) {
		jminor <- x$data$latestVersion$versionMinorNumber 
		if (jmajor > major) {
		  warning(paste("using newer major version", jmajor, "for", cleanuri), call.=FALSE)
		} else if (jmajor < major) {
		  warning(paste("wrong major version in script", jmajor, "for", cleanuri), call.=FALSE)
		}
		if (jminor != minor) warning(paste("different minor version", jminor, "for", cleanuri), call.=FALSE)
	} else { # ckan
		v <- x$result$version
		if (!is.null(v)) {
			if (v != major) stop(paste("different version", v, "for", cleanuri), call.=FALSE)
		}
	}
}

checkVersion <- function(vmeta, major, minor) {
	if (is.na(vmeta)) {
		if (!(is.na(major) && is.na(minor))) {
			stop(paste("version supplied where there is none? You can use 'NA'"), call.=FALSE)			
		}
	} else {
		v <- c(unlist(strsplit(vmeta, "\\.")), NA)
		if (!identical(v[1:2], as.character(c(major, minor)))) {
			if (!is.na(minor)) {
				major <- paste0(major, ".", minor)
			}
			stop(paste("version", major, "provided but expected", vmeta), call.=FALSE)	
		}
	}
}

get_metadata <- function(uri, path, group, major, minor, ...) {
	if (isTRUE(grepl("doi:10.48529", uri))) {
		return(LSMS_metadata(uri, group, path, major, minor, ...))
	}
	dataset_id <- yuri::simpleURI(uri)
	jpath <- file.path(path, "data", "raw", group)
	m <- yuri::extract_metadata(uri, jpath)
	m$group <- group
	
	d <- data.frame(list(...))
	
	draft <- isTRUE(d$draft)	
	if (!draft) {
		checkVersion(m$version, major=major, minor=minor)
		#m$publisher <- NULL 
	}
	d$draft <- NULL
	
	if (nrow(d) == 1) {
		m[names(d)] <- d
	} else if (nrow(d) > 1) {
		warning("additional arguments must all have length 1")
	}
	m
}

