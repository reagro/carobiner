
.onLoad <- function(libname, pkgname) {
	httr::timeout(15)
	options(timeout = max(300, getOption("timeout")))
	try(vocal::update_terms("carob-data/terminag", quiet=TRUE), silent=TRUE)
}


