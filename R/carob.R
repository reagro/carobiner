# Author: Robert Hijmans
# May 2021
# License GPL3

clean_uri <- function(x, reverse=FALSE) {
	warning("use agro::get_simple_URI() instead")
	agro::get_simple_URI(x, reverse)
}




get_data <- function(uri, path) {
	path=file.path(path, "data/raw")
	agro::get_data_from_uri(uri, path)
}


get_metadata <- function(cleanuri, path, major, minor) {
	jf <- file.path(path, "data", "raw", cleanuri, paste0(cleanuri, ".json"))
	x <- jsonlite::fromJSON(readLines(jf))
	jmajor <- x$data$latestVersion$versionNumber 
	jminor <- x$data$latestVersion$versionMinorNumber 
	if (jmajor != major) stop(paste("new major version", jmajor, "for", cleanuri), call.=FALSE)
	if (jminor != minor) warning(paste("new minor version", jminor, "for", cleanuri), call.=FALSE)
	x
}

get_license <- function(x) {
	lic <- x$data$latestVersion$license
	trm <- x$data$latestVersion$termsOfUse
	trm <- tolower(strsplit(trm, '\"')[[1]])
	g <- grep("/creativecommons.org/", trm)
	if (length(g) > 0) {
		trm <- trm[g[1]]
		trm <- gsub("http://", "", trm)
		trm <- gsub("https://", "", trm)
		trm <- gsub("creativecommons.org/licenses", "CC", trm)
		trm <- gsub("/", "-", trm)
		trm <- toupper(gsub("-$", "", trm))
	} 
	if (nchar(trm) > 0) {
		if (lic == "NONE") {
				lic <- trm	
		} else {
			lic <- paste0(lic, "; ", trm)
		}
	}
	lic
}

check_terms <- function(x, type, path) {
	type <- match.arg(type, c("records", "dataset"))
	nms <- names(x)
	answ <- TRUE

	if (type == "records") {
		trms <- utils::read.csv(file.path(path, "terms", "records.csv"))
	} else {
		trms <- utils::read.csv(file.path(path, "terms", "dataset.csv"))
	}
	names(trms)[1] <- "name" # excel seems to mess this up

	xnms <- nms[!(nms %in% trms$name)]
	if (length(xnms) > 0) {
		print(paste("  unknown", type, "variable names: "), paste(xnms, collapse=", "))
		answ <- FALSE
	}
	req <- trms[trms$required == "yes", ]
	r <- req$name[!(req$name %in% nms)]
	if (length(r) > 0) {
		print(paste("  required", type, "variable name(s) missing: "), paste(r, collapse=", "))
		answ <- FALSE
	}
	req <- req[req$vocabulary != "", ]
	if (nrow(req) > 0) {
		for (i in 1:nrow(req)) {
			accepted <- utils::read.csv(file.path(path, "terms", paste0(req$vocabulary[i], ".csv")))[,1]
			provided <- unique(x[, req$name[i]])
			bad <- provided[!(provided %in% accepted)]
			if (length(bad) > 0) {
				print(paste("  ", req$name[i], "contains invalid terms: "), paste(bad, collapse=", "))
				answ <- FALSE
			}
		}
	}

	if ((type=="dataset") & (!(is.null(x$publication) | is.null(x$dataset_id) ))) {
		if (nchar(x$publication[1]) > 0) {
			pubs <- list.files(file.path(path, "references"))
			id <- unlist(strsplit(x$dataset_id, "-"))[1]
			i <- grep(id, pubs)
			if (length(i) == 0) {
				print("  reference file missing")
				answ <- FALSE
			}
		}
	}
	invisible(answ)
}


write_files <- function(dataset, records, path, cleanuri, id=NULL) {
	stopifnot(nrow(dataset) == 1)
	check_terms(records, "records", path)
	check_terms(dataset, "dataset", path)

	if (!is.null(id)) {
		outf <- file.path(path, "data", "clean", paste0(cleanuri, "-", id, ".csv"))
	} else {
		outf <- file.path(path, "data", "clean", paste0(cleanuri, ".csv"))
	}
	dir.create(dirname(outf), FALSE, FALSE)
	utils::write.csv(records, outf, row.names=FALSE)
	mf <- gsub(".csv$", "_meta.csv", outf)
	utils::write.csv(dataset, mf, row.names=FALSE)
	TRUE
}



get_references <- function(x, path, format=TRUE) {
	uri <- unique(x$uri)
	if (length(uri == 0)) {
		stop("no `uri` field", call.=FALSE)
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
	x <- lapply(ff, utils::read.csv)
	nms <- unique(unlist(lapply(x, names)))
	x <- lapply(x, function(x) data.frame(c(x, sapply(setdiff(nms, names(x)), function(y) NA))))
	x$make.row.names <- FALSE
	do.call(rbind, x)
}



get_packages <- function(path) {
	libfun1 <- function(x) {
		d <- readLines(x, warn=FALSE)
		i <- grep('^library\\(', d)
		pkgs <- d[unique(i)]
		pkgs <- pkgs[!grepl("#", pkgs)]
		pkgs <- pkgs[nchar(pkgs) < 100]
		pkgs <- gsub("library\\(", "", pkgs)
		pkgs <- trimws(gsub(")", "", pkgs))
		pkgs <- sort(unique(pkgs))
		pkgs <- gsub('\"', "", pkgs)
	}
	
	libfun2 <- function(x) {
		d <- readLines(x, warn=FALSE)
		i <- grep('::', d)
		if (length(i) ==0) {
			return(NULL)
		}
		d <- d[unique(i)]
		d <- d[!grepl("#", d)]
		if (length(i) ==0) {
			return(NULL)
		}

		d <- strsplit(d, "<-")
		d <- sapply(d, function(e) ifelse(length(e)>1, e[2], e[1]))
		d <- d[!is.na(d)]
		if (length(i) ==0) {
			return(NULL)
		}

		d <- strsplit(d, "=")
		d <- sapply(d, function(e) ifelse(length(e)>1, e[2], e[1]))
		d <- d[!is.na(d)]
		if (length(i) ==0) {
			return(NULL)
		}

		d <- strsplit(d, "::")
		d <- sapply(d, function(e) e[1])
		d <- unique(trimws(d[!is.na(d)]))
		d <- d[!(grepl(",", d) | grepl("\\(", d))]
		d
	}

	ff <- list.files(file.path(path, "scripts"), pattern='\\.R$', full.names=TRUE, ignore.case=TRUE)
	pkgs1 <- unique(unlist(sapply(ff, libfun1)))
	pkgs2 <- unique(unlist(sapply(ff, libfun2)))
	
	pkgs <- unique(c(pkgs1, pkgs2, "readxl", "jsonlite", "reshape2", "stringr"))
	
	ipkgs <- rownames(utils::installed.packages())

	if (!("remotes" %in% ipkgs)) {
		print(paste("installing remotes"))
		utils::install.packages("remotes", repos="https://cloud.r-project.org/")
	}
	
	gpkgs <- c("agro", "carobiner")
	for (pk in gpkgs) {
		if (!(pk %in% ipkgs)) {
			print(paste("installing", pk))
			remotes::install_github(paste0("reagro/", pk))
		}
	}
	ipkgs <- rownames(utils::installed.packages())
	for (pk in pkgs) {
	  if (!(pk %in% ipkgs)) {
		print(paste("installing", pk))
		utils::install.packages(pkgs=pk, repos="https://cloud.r-project.org/", quiet=TRUE)
	  }
	}
}



compile_carob <- function(path) {
	ff <- list.files(file.path(path, "data", "clean"), pattern=".csv$", full.names=TRUE)
	i <- grepl("_meta.csv$", ff)
	mf <- ff[i]
	ff <- ff[!i]
	x <- .binder(mf)
	outmf <- file.path(path, "data", "metadata.csv")
	utils::write.csv(x, outmf, row.names=FALSE)

	y <- .binder(ff)
	outff <- file.path(path, "data", "carob.csv")
	utils::write.csv(y, outff, row.names=FALSE)
	
	return(c(outmf, outff))
}


process_carob <- function(path, quiet=FALSE) {
	get_packages(path)
	ff <- list.files(file.path(path, "data", "clean"), pattern=".csv$", full.names=TRUE)
	file.remove(ff)
	ff <- list.files(file.path(path, "scripts"), pattern="R$", full.names=TRUE)
	carob_script <- function() {FALSE}
	for (f in ff) {
		rm(carob_script)
		if (!quiet) print(basename(f)); utils::flush.console()
		source(f, local=TRUE)
		if (!exists("carob_script")) {
			stop(basename(f), "does not have a `carob_script` function", call.=FALSE)
		}
		if (!carob_script(path)) {
			stop(basename(f), "failed", call.=FALSE)
		}
		flush.console()
	}
	print("done")
	invisible(TRUE)
}


make_carob <- function(path, quiet=FALSE) {
	process_carob(path, quiet)
	compile_carob(path)
}


capitalize_words <- function(x, skip="") {
	x <- paste("", tolower(x), "")
	
	skip <- c("and", "of", "the", tolower(skip))
	skip <- trimws(skip)
	skip <- skip[skip != ""]
	for (w in skip) {
		x <- gsub(paste0(" ", w, " "), paste0(" #", w, " "), x)
	}
	x <- gsub("-", "- ", x)
	x <- gsub(" d'", " #d' ", x)
	x <- gsub("\\.", ".# ", x)
	
    cap <- function(x) paste(toupper(substring(x, 1, 1)),
                  { x <- substring(x, 2); x}, sep = "", collapse = " " )
    x <- sapply(strsplit(x, split = " "), cap, USE.NAMES = FALSE)
	x <- paste("", x, "")

	for (w in skip) {
		x <- gsub(paste0(" #", w, " "), paste0(" ", w, " "), x)
	}

	x <- gsub("- ", "-", x)
	x <- gsub(" #d' ", " d'", x)
	x <- gsub("\\.# ", "\\.", x)
	trimws(x)
}



change_names <- function(x, from, to) {
	stopifnot(length(from) == length(to))
	for (i in 1:length(from)) {
		w <- which(colnames(x) == from[i])
		if (length(w) != 1) {
			stop(paste(from[i], "is missing or duplicated"), call.=FALSE)
		}
		names(x)[w] <- to[i]
	}
	x
}

