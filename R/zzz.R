
.onLoad <- function(libname, pkgname) {
	httr::timeout(15)
	options(timeout = max(300, getOption("timeout")))
	
#	f <- system.file("terms/variables_all.csv", package="carobiner")
#	if (!file.exists(f)) {
#		message("\nPlease first install the standard terms with 'carobiner::update_terms()'\n")
#	}
}


