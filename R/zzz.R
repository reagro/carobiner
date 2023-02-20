
.onLoad <- function(libname, pkgname) {
	httr::timeout(30)
	options(timeout = max(600, getOption("timeout")))
}

