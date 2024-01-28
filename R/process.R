# Author: Robert Hijmans
# May 2021
# License GPL3

get_data <- function(uri, path, group) {
	path <- file.path(path, "data/raw", group)
	data_from_uri(uri, path)
}

get_more_data <- function(url, dataset_id, path, group) {
	f <- basename(url)
	path <- file.path(path, "data/raw", group, dataset_id, f)
	for (i in seq_along(f)) {
		if (!file.exists(path[i])) {
			utils::download.file(path[i], f[i], mode="wb")
		}
	}
	f
}


write_files <- function(path, dataset, records, timerecs=NULL, id=NULL) {

	stopifnot(nrow(dataset) == 1)

	group <- dataset$group
	if (!check_group(group, path)) {
		stop(paste(group, "is not a known group"))
	}

	cleanuri <- dataset$dataset_id
	stopifnot(all(records$dataset_id == cleanuri))

	if (nrow(records) > 0) {
		dir.create(file.path(path, "data", "messages", group), FALSE, TRUE)

		opt <- options("carobiner_check")
		answ <- check_terms(dataset, records, path, group, check=opt)	
		fmsg <- file.path(path, "data", "messages", group, paste0(cleanuri, ".csv"))
		if (nrow(answ) > 0) {
			answ$group <- group
			answ$dataset_id <- cleanuri
			answ$contributor <- dataset$carob_contributor

			fign <- file.path(path, "scripts", group, "ignore.csv")
			if (file.exists(fign)) {
				ign <- utils::read.csv(fign)
				ign <- apply(ign, 1, \(i) paste(i, collapse="#"))
				ans <- apply(answ, 1, \(i) paste(i, collapse="#"))
				m <- stats::na.omit(match(ign, ans))
				if (length(m)  > 0) {
					answ <- answ[-m, ]
				}
			}
			if (nrow(answ) > 0) {
				data.table::fwrite(answ, fmsg, row.names=FALSE)
				for (i in 1:nrow(answ)) {
					message(paste("   ", answ$msg[i]))
				}
				message(paste("    contributor:", dataset$carob_contributor))
			}
		} 
	}
	
	dir.create(file.path(path, "data", "clean"), FALSE, FALSE)
	dir.create(file.path(path, "data", "other"), FALSE, FALSE)
	if (!is.null(id)) {
		outf <- file.path(path, "data", "clean", group, paste0(cleanuri, "-", id, ".csv"))
	} else {
		outf <- file.path(path, "data", "clean", group, paste0(cleanuri, ".csv"))
	}
	dir.create(dirname(outf), FALSE, FALSE)
	records <- carobiner:::sort_by_terms(records, "records", group, path)
#	utils::write.csv(records, outf, row.names=FALSE)
	data.table::fwrite(records, outf, row.names=FALSE)
	
	dataset <- carobiner:::sort_by_terms(dataset, "dataset", group, path)
	mf <- gsub(".csv$", "_meta.csv", outf)
#	utils::write.csv(dataset, mf, row.names=FALSE)
	if (is.null(dataset$carob_date)) dataset$carob_date <- ""
	data.table::fwrite(dataset, mf, row.names=FALSE)

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


compile_carob <- function(path, group="", split_license=FALSE, zip=FALSE) {
	w <- options("warn")
	if (w$warn < 1) {
		on.exit(options(warn=w$warn))
		options(warn=1)
	}
	dir.create(file.path(path, "data", "compiled"), showWarnings = FALSE, recursive = TRUE)
	fff <- list.files(file.path(path, "data", "clean", group), pattern=".csv$", recursive=TRUE)

	if (group == "") {
		grps <- unique(sapply(strsplit(fff, "/"), function(i) ifelse(length(i) > 1, i[1], "doi")))
	} else {
		fff <- file.path(group, fff)
		grps <- group
	}
		
	ret <- NULL
	if (zip) {
		pzip <- Sys.getenv("R_ZIPCMD")
		if (pzip == "") {
			pzip <- "zip"
		}
		zipflags <- "-jq9"		
	}
	for (grp in grps) {
		wgroup <- ifelse(grp == "doi", "", paste0("_", grp))

		ff <- file.path(path, "data", "clean", grep(paste0("^", grp), fff, value=TRUE))
		mi <- grepl("_meta.csv$", ff)
		
		x <- sort_by_terms(.binder(ff[mi]), "dataset", grp, path)
		x[is.na(x)] <- ""
		x[] <- sapply(x, \(i) gsub("\n", " ", i))
		x[] <- sapply(x, \(i) gsub("\t", " ", i))
		y <- sort_by_terms(.binder(ff[!mi]), "records", grp, path)
		if ("reference" %in% colnames(y)) {
			y$reference <- gsub("\n", " ", y$reference)
			y$reference <- gsub("\t", " ", y$reference)
		}
		
		gterms <- get_terms("records", grp, path)
		gterms <- gterms[, c("name", "type", "unit", "description")]

		outft <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_terms.csv"))
#		utils::write.csv(gterms, outft, row.names=FALSE)
		data.table::fwrite(gterms, outft, row.names=FALSE)
		if (split_license) {
			xx <- x[grepl("CC", x[,"license"]), ]
			yy <- y[y$dataset_id %in% xx[, "dataset_id"], ]
			if (nrow(xx) > 0) {
				outmf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_metadata-cc.csv"))
				#utils::write.csv(xx, outmf, row.names=FALSE)
				data.table::fwrite(xx, outmf, row.names=FALSE)
				outff <- file.path(path, "data", "compiled", paste0("carob", wgroup, "-cc.csv"))
				#utils::write.csv(yy, outff, row.names=FALSE)
				data.table::fwrite(yy, outff, row.names=FALSE)
				if (zip) {
					fzip <- gsub(".csv$", ".zip", outff)
					if (file.exists(fzip)) file.remove(fzip)
					utils::zip(fzip, c(outft, outmf, outff), zipflags, zip=pzip)
					fxls <- gsub(".csv$", ".xlsx", outff)
					dx <- list(sources=xx, terms=gterms, data=yy)
					writexl::write_xlsx(dx, fxls)
				}
			}
		}
		outmf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_metadata.csv"))
		#utils::write.csv(x, outmf, row.names=FALSE)
		data.table::fwrite(x, outmf, row.names=FALSE)
		outff <- file.path(path, "data", "compiled", paste0("carob", wgroup, ".csv"))
#		utils::write.csv(y, outff, row.names=FALSE)
		data.table::fwrite(y, outff, row.names=FALSE)
		if (zip) {
			fzip <- gsub(".csv$", ".zip", outff)
			if (file.exists(fzip)) file.remove(fzip)
			utils::zip(fzip, c(outft, outmf, outff), flags=zipflags, zip=pzip)
			fxls <- gsub(".csv$", ".xlsx", outff)
			dx <- list(sources=x, terms=gterms, data=y)
			writexl::write_xlsx(dx, fxls)
		}
		
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



process_carob <- function(path, group="", quiet=FALSE, check=NULL, cache=TRUE) {

	options(carobiner_check=check)
	on.exit(options(carobiner_check=NULL))

	w <- options("warn")
	if (w$warn < 1) {
		on.exit(options(warn=w$warn), add=TRUE)
		options(warn=1)
	}


	if (group != "") {
		check_group(group, path)
	}

	ff <- list.files(file.path(path, "data", "clean", group), pattern=".csv$", full.names=TRUE, recursive=TRUE)
	if (cache) {
		csv_mtime <- data.frame(uri=gsub(".csv$", "", basename(ff)), csv=file.mtime(ff))
	} else {
		file.remove(ff)
		ff <- list.files(file.path(path, "data", "messages", group), pattern="\\.csv$", recursive=TRUE, full.names=TRUE)
		file.remove(ff)
	}
	
	base <- file.path(path, "scripts")
	ff <- list.files(file.path(base, group), pattern="R$", full.names=TRUE, recursive=TRUE)
	ffun <- grepl("^_", basename(ff))
	ff <- ff[!ffun]
	ff <- ff[!grepl("/_pending/", ff)]
	ff <- ff[!grepl("/_removed/", ff)]

	if (cache) {
		R_mtime <- data.frame(uri=tolower(gsub(".R$", "", basename(ff))), 
								R=file.mtime(ff), id=1:length(ff))
		csv_mtime$uri <- tolower(csv_mtime$uri)

		# remove csv files that no longer belong to the group
		old <- merge(csv_mtime, R_mtime, by="uri", all.x=TRUE)
		old <- old[!grepl("_meta$", old$uri), ]
		old <- old[is.na(old$R), ]$uri
		if (length(old) > 0) {
			for (fold in old) {
				file.remove(list.files(file.path(path, "data/clean", group), 
							pattern=fold, recursive=TRUE))
			}
		}
		
		mtime <- merge(csv_mtime, R_mtime, by="uri", all.y=TRUE)
		keep <- which(is.na(mtime$csv) | (mtime$R > mtime$csv))
		ff <- ff[mtime$id[keep]]
		if (length(ff) == 0) {
			message("no changes (cache=TRUE)")
			invisible(TRUE)
		}
	}
	ff <- sort(ff)
	

	tab <- table(basename(ff))
	if (any(tab > 1)) {
		dups <- names(tab[tab>1])
		message(paste("duplicate files: ", paste(dups, collapse=", ")))
	}

	carob_script <- function() {FALSE}
	#rm(list=ls(globalenv()))
	for (f in ff) {
		rm(carob_script)
		if (!quiet) cat(gsub(base, "", f), "\n"); utils::flush.console()
		source(f, local=TRUE)
		if (!exists("carob_script")) {
			stop(basename(f), "does not have a `carob_script` function", call.=FALSE)
		}
		ok <- FALSE
		try(ok <- carob_script(path))
		if (!ok) {
			message(paste("  processing failed for:\n", basename(f)))
		}
		utils::flush.console()
	}
	
	ff <- list.files(file.path(path, "data", "messages"), pattern=".csv$", full.names=TRUE, recursive=TRUE)
	msg <- lapply(ff, utils::read.csv)
	msg <- do.call(rbind, msg)
	utils::write.csv(msg, file.path(path, "data", "messages.csv"), row.names=FALSE)
	
	invisible(TRUE)
}


make_carob <- function(path, group="", quiet=FALSE, check="all", cache=TRUE, ...) {
	get_packages(group)
	process_carob(path, group=group, quiet=quiet, check=check, cache=cache)
	message(" === compiling ===")
	compile_carob(path, group=group, ...)
}


