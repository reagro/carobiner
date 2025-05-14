# Author: Robert J. Hijmans
# Sept 2019
# version 1
# license 	


filter_files <- function(x) { 
	x <- grep("\\.json$|ok\\.txt$|\\.pdf$|_files.txt$|\\.zip$|\\.doc$|\\.docx$", x, value=TRUE, invert=TRUE)
	# remove opened excel files
	grep("/~$", x, fixed=TRUE, invert=TRUE, value=TRUE)
}


.download_files <- function(path, files, cache) {

	outf <- gsub("%20", "_", basename(files))
	outf <- file.path(path, outf)
	oks <- rep(1, length(outf))

	if (cache && file.exists(file.path(path, "ok.txt"))) {
		have <- file.exists(outf)
		if (all(have)) {
			outf <- list.files(path, full.names=TRUE)
			return(filter_files(outf)) 
		}
		oks[have] <- 0
	} 

	for (i in 1:length(files)) {
		if (oks[i] == 1) {
			oks[i] <- utils::download.file(files[i], outf[i], mode="wb", quiet=TRUE)
		}
	}
	
	zf <- grep("\\.zip$", outf, value=TRUE)
	if (length(zf) > 0) {
		for (z in zf) {
			yuri:::.dataverse_unzip(z, path, TRUE)
		}
		outf <- list.files(path, full.names=TRUE)
	}
	
	if (all(oks == 0)) {
		writeLines(c(utils::timestamp(quiet=TRUE), files), file.path(path, "ok.txt"))
	}

	filter_files(outf) 
} 

.copy_files <- function(path, files, cache) {

	outf <- file.path(path, basename(files))

	if (cache && file.exists(file.path(path, "ok.txt"))) {
		have <- file.exists(outf)
		if (all(have)) {
			return(outf)
		}
		ok <- file.copy(files[!have], outf[!have])
	} else {
		ok <- file.copy(files, outf)
	}
	
	if (all(ok)) {
		writeLines(c(utils::timestamp(quiet=TRUE), files), file.path(path, "ok.txt"))
	}
	outf
}


file_downloads <- function(files, path, cache) {
	http <- grepl("^http", files)
	if (all(http)) {
		.download_files(path, files, cache)
	} else if (all(!http)) {
		.copy_files(path, files, cache)
	} else {
		stop("Either all files, or no files should start with 'http'" )
	}
}


get_data <- function(uri, path, group, files=NULL, cache=TRUE) {
	if (is.null(path)) {
		path <- file.path(tempdir(), "carob", fsep="/")
	} else {
		path <- file.path(path, "data/raw", group)
	}
	if (is.null(files)) {	
		uname <- yuri::simpleURI(uri)
	} else {
		uname <- gsub("/|:", "_", uri)
	}
	if (!file.exists(file.path(path, uname, "ok.txt"))) {
		cache <- FALSE
	}
	dir.create(path, FALSE, TRUE)
	if (!is.null(files)) {
		file_downloads(files, path, cache)
	} else {
		if (group == "LSMS") {
			path <- file.path(path, uname)
			ff <- get_LSMS(uri, path, cache=cache)		
		} else {
			ff <- yuri::dataURI(uri, path, unzip=TRUE, cache=cache)
		}
		filter_files(ff)
	}
}

# uri <- "doi:10.5061/dryad.pj76g30"
# path <- getwd()
# group <- "fertilizer"
# ff <- get_data(uri, path, group)
