# Author: Robert Hijmans
# May 2021
# License GPL3


get_data <- function(uri, path, group="") {
	path=file.path(path, "data/raw", group)
	agro::get_data_from_uri(uri, path)
}

get_terms <- function(type, group) {
	if (type == "records") {
		trms <- utils::read.csv(file.path(path, "terms", "records.csv"))
		if (group != "") {
			trms2 <- utils::read.csv(file.path(path, "terms", group, "records.csv"))
			trms <- rbind(trms, trms2)
			tab <- table(trms[,1])
			if (any(tab > 1)) {
				print(paste("duplicated terms:", names(tab[tab>1])))
			}
		}
	} else {
		trms <- utils::read.csv(file.path(path, "terms", "dataset.csv"))
	}
	names(trms)[1] <- "name" # excel seems to mess this up
	trms
}



check_terms <- function(x, type, path, group="") {

	type <- match.arg(type, c("records", "dataset"))
	nms <- names(x)
	answ <- TRUE
	trms <- get_terms(type, group)

	xnms <- nms[!(nms %in% trms$name)]
	if (length(xnms) > 0) {
		print(paste("  unknown", type, "variable names: ", paste(xnms, collapse=", ")))
		answ <- FALSE
	}
	req <- trms[trms$required == "yes", ]
	r <- req$name[!(req$name %in% nms)]
	if (length(r) > 0) {
		print(paste("  required", type, "variable name(s) missing: ", paste(r, collapse=", ")))
		answ <- FALSE
	}
	req <- req[!is.na(req$vocabulary) & (req$vocabulary != ""), ]
	if (nrow(req) > 0) {
		for (i in 1:nrow(req)) {
			accepted <- utils::read.csv(file.path(path, "terms", paste0(req$vocabulary[i], ".csv")))[,1]
			provided <- unique(x[, req$name[i]])
			bad <- provided[!(provided %in% accepted)]
			if (length(bad) > 0) {
				print(paste("  ", req$name[i], "contains invalid terms: ", paste(bad, collapse=", ")))
				answ <- FALSE
			}
		}
	}

	if ((type=="dataset") & isTRUE(nchar(x$publication) > 0 )) {
		allpubs <- list.files(file.path(path, "references"))
		pubs <- unlist(strsplit(x$publication, ";"))
		pubs <- agro::get_simple_URI(pubs)
		for (pub in pubs) {
			where <- grep(pub, allpubs)
			if (length(where) == 0) {
				cat("  reference file missing:", pub, "\n")	
				answ <- FALSE
			}
		}
	}
	invisible(answ)
}



write_files <- function(dataset, records, path, cleanuri, group="", id=NULL) {

	stopifnot(nrow(dataset) == 1)
	check_terms(records, "records", path, group)
	check_terms(dataset, "dataset", path, group)

	if (!is.null(id)) {
		outf <- file.path(path, "data", "clean", group, paste0(cleanuri, "-", id, ".csv"))
	} else {
		outf <- file.path(path, "data", "clean", group, paste0(cleanuri, ".csv"))
	}
	dir.create(dirname(outf), FALSE, FALSE)
	utils::write.csv(records, outf, row.names=FALSE)
	mf <- gsub(".csv$", "_meta.csv", outf)
	utils::write.csv(dataset, mf, row.names=FALSE)
	TRUE
}



bindr <- function( ...) {
	x <- list(...)
	nms <- unique(unlist(lapply(x, names)))
	x <- lapply(x, function(x) data.frame(c(x, sapply(setdiff(nms, names(x)), function(y) NA))))
	x$make.row.names <- FALSE
	do.call(rbind, x)
}

get_function <- function(name, path, group="") {
	f <- file.path(path, "scripts", group, "_functions.R")
	source(f, local=TRUE)
	get(name)
}


.binder <- function(ff) {
	x <- lapply(ff, utils::read.csv)
	nms <- unique(unlist(lapply(x, names)))
	x <- lapply(x, function(x) data.frame(c(x, sapply(setdiff(nms, names(x)), function(y) NA))))
	x$make.row.names <- FALSE
	do.call(rbind, x)
}



sort_by_terms <- function(x, type, group) {
	trms <- get_terms(type, ifelse(group == "doi", "", group))
	trms <- trms$name[trms$name %in% names(x)]
	x[, trms]
}


compile_carob <- function(path) {
	dir.create(file.path(path, "data", "compiled"), FALSE, FALSE)
	fff <- list.files(file.path(path, "data", "clean"), pattern=".csv$", recursive=TRUE)
	grps <- unique(sapply(strsplit(fff, "/"), function(i) ifelse(length(i) > 1, i[1], "doi")))
	ret <- NULL
	for (grp in grps) {
		wgroup <- ifelse(grp == "doi", "", paste0("-", grp))

		ff <- file.path(path, "data", "clean", grep(paste0("^", grp), fff, value=TRUE))
		mi <- grepl("_meta.csv$", ff)
		
		x <- sort_by_terms(.binder(ff[mi]), "dataset", grp)
		outmf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_metadata.csv"))
		utils::write.csv(x, outmf, row.names=FALSE)

		y <- sort_by_terms(.binder(ff[!mi]), "records", grp)
		outff <- file.path(path, "data", "compiled", paste0("carob", wgroup, ".csv"))
		utils::write.csv(y, outff, row.names=FALSE)
		ret <- c(ret, outmf, outff)
	}
	utils::flush.console()
	return(ret)
}


run_carob <- function(cleanuri, path, quiet=FALSE) {
	ff <- list.files(file.path(path, "scripts"), pattern="R$", full.names=TRUE, recursive=TRUE)
	f <- grep(cleanuri, ff, value=TRUE)
	carob_script <- function() {FALSE}
	rm(carob_script)
	if (!quiet) print(basename(f)); utils::flush.console()
	source(f, local=TRUE)
	if (!exists("carob_script")) {
		stop(basename(f), "does not have a `carob_script` function", call.=FALSE)
	}
	if (!carob_script(path)) {
		cat(basename(f), " failed\n")
	}
	invisible(TRUE)
}




process_carob <- function(path, quiet=FALSE) {
	get_packages(path)
	ff <- list.files(file.path(path, "data", "clean"), pattern=".csv$", full.names=TRUE)
	file.remove(ff)
	ff <- list.files(file.path(path, "scripts"), pattern="R$", full.names=TRUE, recursive=TRUE)
	ffun <- grepl("^_", basename(ff))
	ff <- ff[!ffun]
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
		utils::flush.console()
	}
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



change_names <- function(x, from, to, must_have=TRUE) {
	stopifnot(length(from) == length(to))
	for (i in 1:length(from)) {
		w <- which(colnames(x) == from[i])
		if (length(w) > 1) {
			stop(paste(from[i], "is duplicated"), call.=FALSE)
		} else if (must_have && length(w) == 0) {
			stop(paste(from[i], "is absent"), call.=FALSE)
		}
		names(x)[w] <- to[i]
	}
	x
}

