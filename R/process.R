# Author: Robert Hijmans
# May 2021
# License GPL3



write_files <- function(path, metadata, records, timerecs=NULL, wth=NULL, id=NULL, options=NULL) {

	if (path=="ignore" & missing(metadata)) return(TRUE)

	stopifnot(nrow(metadata) == 1)

	to_mem <- FALSE
	if (is.null(path)) {
		path <- file.path(tempdir(), "carob")
		to_mem <- TRUE
	}

	group <- metadata$group
	check_group(group)

	cleanuri <- metadata$dataset_id
	stopifnot(all(records$dataset_id == cleanuri))

	if (nrow(records) > 0) {
		dir.create(file.path(path, "data", "messages", group), FALSE, TRUE)
		records$dataset_id <- metadata$dataset_id
		opt <- options("carobiner_check")
		answ <- carobiner:::check_terms(metadata, records, group, check=opt)	
		fmsg <- file.path(path, "data", "messages", group, paste0(cleanuri, ".csv"))

		if (!to_mem) {
			asnw <- check_pubs(metadata, path, answ)
		}

		if (nrow(answ) > 0) {
			answ$group <- group
			answ$dataset_id <- cleanuri
			answ$contributor <- metadata$carob_contributor

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
				message(paste("    contributor:", metadata$carob_contributor))
			}
		} 
	}

	if (is.null(records$record_id) && (nrow(records) > 0)) {
		records$record_id <- 1:nrow(records)
	}
	
	records <- sort_by_terms(records, "records", group)
	metadata <- sort_by_terms(metadata, "metadata", group)


	if (to_mem) {
		return(list(data=records, meta=metadata))
	}

	
	dir.create(file.path(path, "data", "clean"), FALSE, FALSE)
#?	dir.create(file.path(path, "data", "other"), FALSE, FALSE)
	if (!is.null(id)) {
		outf <- file.path(path, "data", "clean", group, paste0(cleanuri, "-", id, ".csv"))
	} else {
		outf <- file.path(path, "data", "clean", group, paste0(cleanuri, ".csv"))
	}
	dir.create(dirname(outf), FALSE, FALSE)
#	utils::write.csv(records, outf, row.names=FALSE)
	data.table::fwrite(records, outf, row.names=FALSE)
	
	mf <- gsub(".csv$", "_meta.csv", outf)
#	utils::write.csv(metadata, mf, row.names=FALSE)
	data.table::fwrite(metadata, mf, row.names=FALSE)
	
# Update todo/to-do.csv list
# RH: perhaps too much to run this for each dataset 
#	update_todo(path)

	TRUE
}


get_function <- function(name, path, group="") {
	f <- file.path(path, "scripts", group, "_functions.R")
	source(f, local=TRUE)
	get(name)
}


sort_by_terms <- function(x, type, group) {
	trms <- accepted_variables(type, ifelse(group == "doi", "", group))
	trms <- trms$name[trms$name %in% names(x)]
	x[, trms]
}


compile_carob <- function(path, group="", split_license=FALSE, zip=FALSE, cache=FALSE) {
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
	
	fgrp <- dirname(fff)

	for (grp in grps) {

		wgroup <- ifelse(grp == "doi", "", paste0("_", grp))

		ff <- file.path(path, "data", "clean", fff[fgrp == grp])
		outft <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_terms.csv"))
		if (file.exists(outft) && cache) {
			ft <- file.info(outft)$mtime
			fftime <- file.info(ff)$mtime
			if (all(fftime < ft)) next
		}

		mi <- grepl("_meta.csv$", ff)
		x <- sort_by_terms(.binder(ff[mi]), "metadata", grp)
		x[is.na(x)] <- ""
		x[] <- sapply(x, \(i) gsub("\n", " ", i))
		x[] <- sapply(x, \(i) gsub("\t", " ", i))
		y <- sort_by_terms(.binder(ff[!mi]), "records", grp)
		if ("reference" %in% colnames(y)) {
			y$reference <- gsub("\n", " ", y$reference)
			y$reference <- gsub("\t", " ", y$reference)
		}
		
		gterms <- accepted_variables("records", grp)
		gterms <- gterms[, c("name", "type", "unit", "description")]

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
		stop(" ", basename(f), " does not have a 'carob_script' function", call.=FALSE)
	}
	if (!carob_script(path)) {
		cat(paste(" ", basename(f), " failed\n"))
	}
	invisible(TRUE)
}



process_carob <- function(path, group="", quiet=FALSE, check=NULL, cache=TRUE) {

	if (!file.exists(path)) {
		stop("path does not exist")
	}

	options(carobiner_check=check)
	on.exit(options(carobiner_check=NULL))

	w <- options("warn")
	if (w$warn < 1) {
		on.exit(options(warn=w$warn), add=TRUE)
		options(warn=1)
	}


	if (group != "") {
		check_group(group)
	}

	base <- file.path(path, "scripts")
	ffR <- list.files(file.path(base, group), pattern="R$", full.names=TRUE, recursive=TRUE)
	ff_un <- grepl("^_", basename(ffR))
	ffR <- ffR[!ff_un]
	ffR <- ffR[!grepl("/_pending/", ffR)]
	ffR <- ffR[!grepl("/_removed/", ffR)]
	ffR <- ffR[basename(ffR) != "template.R"]

	fcsv <- list.files(file.path(path, "data", "clean", group), pattern="_meta.csv$", full.names=TRUE, recursive=TRUE)
	if (length(fcsv) == 0) cache = FALSE
	
	if (cache) {
		### remove compiled data that is no longer in the group
		have_csv = do.call(rbind, sapply(strsplit(gsub("_meta.csv$", "", fcsv, ignore.case = TRUE), 
				paste0(file.path(path, "data", "clean"), "/")), 
				\(i) strsplit(i[2], "/"))) |> data.frame()
		colnames(have_csv) <- c("group", "URI")
		have_R = do.call(rbind, sapply(strsplit(gsub("\\.R$", "", ffR, ignore.case = TRUE),  
				paste0(file.path(base), "/")), 
				\(i) strsplit(tolower(i[2]), "/"))) |> data.frame()
		colnames(have_R) <- c("group", "uri")
		have_R$script <- TRUE
		have_csv$uri <- tolower(have_csv$URI)
		have <- merge(have_csv, have_R, by=c("group", "uri"), all.x=TRUE)
		have <- have[is.na(have$script), ]
		if (nrow(have) > 0) {
			fr <- file.path(path, "data", "clean", have$group, have$URI)
			fr <- c(paste0(fr, ".csv"), paste0(fr, "_meta.csv"))
			fr <- fr[file.exists(fr)]
			if (length(fr) > 0) file.remove(fr)
		}

		csv_mtime <- data.frame(uri=gsub("_meta.csv$", "", basename(fcsv)), csv=file.mtime(fcsv))

	
		R_mtime <- data.frame(uri=tolower(gsub(".R$|.r$", "", basename(ffR))), 
								R=file.mtime(ffR), id=1:length(ffR))
		csv_mtime$uri <- tolower(csv_mtime$uri)

		mtime <- merge(csv_mtime, R_mtime, by="uri", all.y=TRUE)
		keep <- which(is.na(mtime$csv) | (mtime$R > mtime$csv))
		ffR <- ffR[mtime$id[keep]]
		if (length(ffR) == 0) {
			return(invisible(TRUE))
		}
	} else {
		fcsv <- list.files(file.path(path, "data", "clean", group), pattern=".csv$", full.names=TRUE, recursive=TRUE)
		file.remove(fcsv)
		fmsg <- list.files(file.path(path, "data", "messages", group), pattern="\\.csv$", recursive=TRUE, full.names=TRUE)
		file.remove(fmsg)
	}


	ffR <- sort(ffR)
	
	tab <- table(basename(ffR))
	if (any(tab > 1)) {
		dups <- names(tab[tab>1])
		message(paste("duplicate files: ", paste(dups, collapse=", ")))
	}

	carob_script <- function() {FALSE}
	#rm(list=ls(globalenv()))
	for (f in ffR) {
		rm(carob_script)
		if (!quiet) cat(gsub(base, "", f), "\n"); utils::flush.console()
		source(f, local=TRUE)
		if (!exists("carob_script")) {
			stop(basename(f), " does not have a 'carob_script' function", call.=FALSE)
		}
		ok <- FALSE
		try(ok <- carob_script(path))
		if (!ok) {
			message(paste("  processing failed for:\n", basename(f)))
		}
		utils::flush.console()
	}
	
	ffm <- list.files(file.path(path, "data", "messages"), pattern=".csv$", full.names=TRUE, recursive=TRUE)
	msg <- lapply(ffm, utils::read.csv)
	msg <- do.call(rbind, msg)
	utils::write.csv(msg, file.path(path, "data", "messages.csv"), row.names=FALSE)

	update_todo(path)
	invisible(TRUE)
}


make_carob <- function(path, group="", quiet=FALSE, check="all", report=FALSE, cache=TRUE, ...) {
	get_packages(group)
	message(" === process ===")
	process_carob(path, group=group, quiet=quiet, check=check, cache=cache)
	message(" === compile ===")
	compile_carob(path, group=group, cache=cache, ...)
	if (report) {
		message(" === report ===")
		make_reports(path, group="", cache=TRUE)
	}
	message(" === done ===")
}


