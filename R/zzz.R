
.onLoad <- function(libname, pkgname) {
	httr::timeout(15)
	options(timeout = max(300, getOption("timeout")))
	try(update_terms(quiet=TRUE), silent=TRUE)
}


