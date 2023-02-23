# Author: Robert Hijmans
# May 2021
# License GPL3

get_data <- function(uri, path, group="") {
	path <- file.path(path, "data/raw", group)
	data_from_uri(uri, path)
}


get_terms <- function(type, group, path) {
	if (type == "records") {
		trms <- utils::read.csv(file.path(path, "terms", "records.csv"))
		if (group != "") {
			grp_terms <- file.path(path, "terms", group, "records.csv")
			if (file.exists(grp_terms)) {
				trms2 <- utils::read.csv(grp_terms)
				trms <- rbind(trms, trms2)
				tab <- table(trms[,1])
				if (any(tab > 1)) {
					print(paste("duplicated terms:", names(tab[tab>1])))
				}
			}
		}
	} else {
		trms <- utils::read.csv(file.path(path, "terms", "dataset.csv"))
	}
	names(trms)[1] <- "name" # excel seems to mess this up
	trms
}



write_files <- function(dataset, records, path, cleanuri, group="", id=NULL) {

	stopifnot(nrow(dataset) == 1)

	check_terms(dataset, records, path, group)
	dir.create(file.path(path, "data", "clean"), FALSE, FALSE)
	dir.create(file.path(path, "data", "other"), FALSE, FALSE)
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


get_function <- function(name, path, group="") {
	f <- file.path(path, "scripts", group, "_functions.R")
	source(f, local=TRUE)
	get(name)
}


sort_by_terms <- function(x, type, group, path) {
	trms <- get_terms(type, ifelse(group == "doi", "", group), path)
	trms <- trms$name[trms$name %in% names(x)]
	x[, trms]
}


compile_carob <- function(path, group="") {
	w <- options("warn")
	if (w$warn < 1) {
		on.exit(options(warn=w$warn))
		options(warn=1)
	}
	dir.create(file.path(path, "data", "compiled", group), FALSE, FALSE)
	fff <- list.files(file.path(path, "data", "clean", group), pattern=".csv$", recursive=TRUE)
	if (group == "") {
		grps <- unique(sapply(strsplit(fff, "/"), function(i) ifelse(length(i) > 1, i[1], "doi")))
	} else {
		grps <- group
	}
		
	ret <- NULL
	for (grp in grps) {
		wgroup <- ifelse(grp == "doi", "", paste0("-", grp))

		ff <- file.path(path, "data", "clean", grep(paste0("^", grp), fff, value=TRUE))
		mi <- grepl("_meta.csv$", ff)
		
		x <- sort_by_terms(.binder(ff[mi]), "dataset", grp, path)
		outmf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_metadata.csv"))
		utils::write.csv(x, outmf, row.names=FALSE)

		y <- sort_by_terms(.binder(ff[!mi]), "records", grp, path)
		outff <- file.path(path, "data", "compiled", paste0("carob", wgroup, ".csv"))
		utils::write.csv(y, outff, row.names=FALSE)
		ret <- c(ret, outmf, outff)
	}
	utils::flush.console()
	return(ret)
}


run_carob <- function(cleanuri, path, group="", quiet=FALSE) {
	w <- options("warn")
	if (w$warn < 1) {
		on.exit(options(warn=w$warn))
		options(warn=1)
	}

	ff <- list.files(file.path(path, "scripts", group), pattern="R$", full.names=TRUE, recursive=TRUE)
	f <- grep(cleanuri, ff, value=TRUE)
	carob_script <- function() {FALSE}
	rm(carob_script)
	if (!quiet) cat(basename(f), "\n"); utils::flush.console()
	source(f, local=TRUE)
	if (!exists("carob_script")) {
		stop(" ", basename(f), "does not have a `carob_script` function", call.=FALSE)
	}
	if (!carob_script(path)) {
		cat(paste(" ", basename(f), " failed\n"))
	}
	invisible(TRUE)
}



process_carob <- function(path, group="", quiet=FALSE) {
	w <- options("warn")
	if (w$warn < 1) {
		on.exit(options(warn=w$warn))
		options(warn=1)
	}
	
	ff <- list.files(file.path(path, "data", "clean", group), pattern=".csv$", full.names=TRUE)
	file.remove(ff)
	base <- file.path(path, "scripts")
	ff <- list.files(file.path(base, group), pattern="R$", full.names=TRUE, recursive=TRUE)
	ffun <- grepl("^_", basename(ff))
	ff <- ff[!ffun]
	ff <- ff[!grepl("/_pending/", ff)]
	ff <- ff[!grepl("/_removed/", ff)]
	ff <- sort(ff)

	tab <- table(basename(ff))
	if (any(tab > 1)) {
		dups <- names(tab[tab>1])
		message(paste("duplicate files: ", paste(dups, collapse=", ")))
	}


	carob_script <- function() {FALSE}
	for (f in ff) {
		rm(carob_script)
		if (!quiet) cat(gsub(base, "", f), "\n"); utils::flush.console()
		source(f, local=TRUE)
		if (!exists("carob_script")) {
			stop(basename(f), "does not have a `carob_script` function", call.=FALSE)
		}
		ok <- FALSE
		try(ok <- carob_script(path), silent=TRUE)
		if (!ok) {
			message(paste("processing failed for:\n", basename(f)))
		}
		utils::flush.console()
	}
	invisible(TRUE)
}


make_carob <- function(path, group="", quiet=FALSE) {
	get_packages(group)
	process_carob(path, group, quiet)
	compile_carob(path, group)
}


