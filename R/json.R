# Author: Robert Hijmans
# January 2010
# License GPL3

clean_uri <- function(uri) {
	gsub(":", "_", gsub("/", "_", uri))
}


get_json <- function(cleanuri, path, major, minor) {
	jf <- file.path(path, cleanuri, paste0(cleanuri, ".json"))
	x <- jsonlite::fromJSON(readLines(jf))
	jmajor <- x$data$latestVersion$versionNumber 
	jminor <- x$data$latestVersion$versionMinorNumber 
	if (jmajor != major) stop(paste("new major version", major, "for", cleanuri))
	if (jminor != minor) warning(paste("new minor version", minor, "for", cleanuri))
	x
}

get_terms <- function(js, d) {
	lic <- js$data$latestVersion$license
	trm <- js$data$latestVersion$termsOfUse
	trm <- tolower(strsplit(trm, '\"')[[1]])
	g <- grep("/creativecommons.org/", trm)
	if (length(g) > 0) {
		trm <- trm[g[1]]
		trm <- gsub("http://", "", trm)
		trm <- gsub("https://", "", trm)
		trm <- gsub("creativecommons.org/licenses", "CC", trm)
		trm <- gsub("/", "-", trm)
		trm <- toupper(gsub("-$", "", trm))
		if (lic == "NONE") lic <- trm
	}
	d$license <- lic
	d$termsOfUse <- trm
	d
}


write_files(dset, records, id, path, cleanuri) {
	stopifnot(nrow(dset) == 1)
	stopifnot(id > 0)
	outf <- file.path(path, "data", "clean", paste0(cleanuri, "_", id, ".csv"))
	write.csv(records, outf, row.names=FALSE)

	mf <- gsub(".csv$", "_meta.csv", outf)
	write.csv(dset, mf, row.names=FALSE)
}


