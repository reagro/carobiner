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

get_license <- function(js, d) {
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
		if (lic == "NONE") {
			lic <- trm	
		} else {
			lic <- paste0(lic, "; ", trm)
		}
	}
	d$license <- lic
	d
}

check_terms <- function(x, type, path) {
	if (type == "records") {
		trms <- read.csv(file.path(path, "terms", "records.csv"))
		x <- x[!(x %in% trms[,1])]
		if (length(x) > 0) {
			warning("unknown record variable names: ", paste(x, collapse=", "))
		}
	}
	if (type == "dataset") {
		trms <- read.csv(file.path(path, "terms", "dataset.csv"))
		x <- x[!(x %in% trms[,1])]
		if (length(x) > 0) {
			warning("unknown dataset variable names: ", paste(x, collapse=", "))
		}
	}
}


write_files <- function(dset, records, path, cleanuri, id=NULL) {
	stopifnot(nrow(dset) == 1)
	check_terms(names(records), "records", path)
	check_terms(names(dset), "dataset", path)

	if (!is.null(id)) {
		outf <- file.path(path, "data", "clean", paste0(cleanuri, "-", id, ".csv"))
	} else {
		outf <- file.path(path, "data", "clean", paste0(cleanuri, ".csv"))
	}
	dir.create(dirname(outf), FALSE, FALSE)
	write.csv(records, outf, row.names=FALSE)
	mf <- gsub(".csv$", "_meta.csv", outf)
	write.csv(dset, mf, row.names=FALSE)
}



get_references <- function(x, path, format=TRUE) {
	uri <- unique(x$uri)
	if (length(uri == 0)) {
		stop("no `uri` field")
	}
	refs <- list.files(file.path(path, "references"), full.names=TRUE)
	rn <- tools::file_path_sans_ext(basename(refs))
	i <- sapply(uri, function(u) grep(u, rn))
	if (length(i == 0)) {
		return("")
	} 
	d <- revtools::read_bibliography(refs[i])
	if (format) {
		dd <- revtools::format_citation(d)
		names(dd) <- NULL
		return(dd)
	}
	rownames(d) <- NULL
	d$filename <- NULL
	d$ID <- NULL
	d$DA <- NULL
	d$url <- NULL
	d$label <- NULL
	d
}

.binder <- function(ff) {
	x <- lapply(ff, read.csv)
	nms <- unique(unlist(lapply(x, names)))
	x <- lapply(x, function(x) data.frame(c(x, sapply(setdiff(nms, names(x)), function(y) NA))))
	x$make.row.names <- FALSE
	do.call(rbind, x)
}


carob_aggregate <- function(path) {
	ff <- list.files(file.path(path, "data", "clean"), pattern=".csv$", full.names=TRUE)
	i <- grepl("_meta.csv$", ff)
	mf <- ff[i]
	ff <- ff[!i]
	x <- .binder(mf)
	write.csv(x, file.path(path, "data", "metadata.csv"), row.names=FALSE)

	y <- .binder(ff)
	write.csv(y, file.path(path, "data", "carob.csv"), row.names=FALSE)
}



make_carob <- function(path, quiet=FALSE) {
	ff <- list.files(file.path(path, "scripts"), pattern="R$", full.names=TRUE)
	for (f in ff) {
		if (!quiet) print(basename(f)); flush.console()
		source(f, local=TRUE)
		if (!exists("carob_script")) {
			stop(basename(f), "does not have a `carob_script` function")
		}
		if (!carob_script(path)) {
			stop(basename(f), "failed")
		}
	}
	carob_aggregate(path)
}


capitalize_words <- function(s) {
	s <- tolower(s)
	s <- gsub("-", "- ", s)
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  { s <- substring(s, 2); s}, sep = "", collapse = " " )
    s <- sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
	gsub("- ", "-", s)
}


change_names <- function(x, from, to) {
	stopifnot(length(from) == length(to))
	for (i in 1:length(from)) {
		w <- which(colnames(x) == from[i])
		if (length(w) != 1) {
			stop(paste(from[i], "is missing or duplicated"))
		}
		names(x)[w] <- to[i]
	}
	x
}
