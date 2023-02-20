
.onLoad <- function(libname, pkgname) {
	httr::timeout(30)
	options(timeout = max(6000, getOption("timeout")))
}

