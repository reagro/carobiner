

is_current <- function(jf ) {
	
#	uname <- yuri::simpleURI(uri)
#	jf <- file.path(path, "data/raw", group, uname, paste0(uname, ".json"))
	if (!file.exists(jf)) {
		warning("json does not exist")
		return(NA)
	}
	x <- jsonlite::fromJSON(jf)
	old_major <- x$data$latestVersion$versionNumber
	if (is.null(old_major)) {
		warning("not dataverse?")
		return(NA)
	}
	old_minor <- x$data$latestVersion$versionMinorNumber
	uri <- x$data$latestVersion$datasetPersistentId
	url <- x$data$persistentUrl
	
	httr::set_config(httr::config(ssl_verifypeer = 0L))

	g <- httr::GET(url)
	if (g$status_code %in% c(200,202)) {
		u <- g$url
	} else {
		warning("bad response")
		return(NA)
	}

	domain <- yuri:::.getdomain(u)
	protocol <- yuri:::.getprotocol(u)
	baseu <- paste0(protocol, domain)

	pid <- unlist(strsplit(u, "\\?"))[2]
	uu <- paste0(baseu, "/api/datasets/:persistentId?", pid)
	
	# the nice way
	#r <- httr::GET(uu)
	#httr::stop_for_status(r)
	#js <- httr::content(r, as = "text", encoding = "UTF-8")
	# but for cimmyt...
	tmpf <- tempfile()
	if (grepl("worldagroforestry", uu) || grepl("cirad.fr", uu) || grepl("cipotato", uu)) {
		# temporary fix because WorldAgroFor https cert has expired
		# not sure why for CIP on Ubuntu (cert expired)
		utils::download.file(uu, tmpf, quiet=TRUE, method="curl", extra="-k", mode="wb")
	} else {
		utils::download.file(uu, tmpf, quiet=TRUE, mode="wb")
	}
	njs <- readLines(tmpf, encoding = "UTF-8", warn=FALSE)
	njs <- jsonlite::fromJSON(njs)

	new_major <- njs$data$latestVersion$versionNumber
	new_minor <- njs$data$latestVersion$versionMinorNumber

	if ((new_major == old_major) && (new_minor == old_minor)) {
		return(TRUE)
	} else {
		return(c(paste0("old_", old_major, ".", old_minor), paste0(new_major, ".", new_minor)))
	}

}	



update_carob <- function(path, group="") {

	jff <- list.files(file.path(path, "data/raw/", group), pattern=".json$", recursive=TRUE, full=TRUE)
	jff <- grep("/old_", jff, value=TRUE, invert=TRUE)
	
	for (jf in jff) {
		test <- try(is_current(jf))
		if (inherits(test, "try-error")) next
		if (!is.na(test[1]) && (!isTRUE(test[1]))) {
			d <- dirname(jf)
			group <- basename(dirname(d))

			vold <- test[1]
			vnew <- test[2]
			message(paste(group, gsub(".json$", "", basename(jf)), vold, "->", vnew)); flush.console()

			old <- list.files(d, full.names=TRUE, recursive=TRUE)
			old <- grep(file.path(d, "old_"), old, value=TRUE, invert=TRUE)
			p <- file.path(d, vold)
			new <- gsub(d, p, old)
			dir.create(p, FALSE, FALSE)
			file.rename(old, new)
			pat <- basename(d)
			dd <- gsub("data/raw/", "data/clean/", dirname(d))
			ff <- list.files(dd, pattern=pat, full=TRUE)
			file.remove(ff)
		}
	}
}
