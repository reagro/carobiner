
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

check_version <- function(m, major=0, minor=0) {
	v <- paste0(major, ".", minor)
	mv <- m$version 
	if (!is.na(mv)) {
		if (!grepl("\\.", mv)) {
			mv <- paste0(mv, ".0")
		}
		if (mv != v) {
			stop(paste("version", v, "expected but version", mv, "found"), call.=FALSE)	
		}
	}
}

get_metadata <- function(uri, path, group, major=1, minor=0, check=TRUE, reduce=TRUE) {
	if (group == "LSMS") {
		return(LSMS_metadata(uri, path, major, minor))
	}
	dataset_id <- yuri::simpleURI(uri)
	jpath <- file.path(path, "data", "raw", group, dataset_id)
	m <- yuri::extract_metadata(uri, jpath)
	if (check) check_version(m, major=major, minor=minor)
	if (reduce) {
		m$data_organization <- NULL
		m$publication <- NULL
	}
	m$group <- group
	m
}

