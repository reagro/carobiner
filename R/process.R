# Author: Robert Hijmans
# May 2021
# License GPL3



write_files <- function(path=NULL, metadata, records, timerecs=NULL, wth=NULL, options=NULL) {

	group <- metadata$group
#	check_group(group)
	cleanuri <- metadata$dataset_id
	stopifnot(nrow(metadata) == 1)
	if (!is.null(timerecs)) {
		timerecs$dataset_id <- cleanuri
	}
	if (!is.null(wth)) {
		wth$dataset_id <- cleanuri
	}

	to_mem <- FALSE
	if (is.null(path)) {
		path <- file.path(tempdir(), "carob")
		to_mem <- TRUE
	}

	if (!to_mem) {
		dir.create(file.path(path, "data", "clean"), FALSE, FALSE)
		list.files(file.path(path, "data", "clean", group), cleanuri, full.names=TRUE) |> file.remove()

		if (missing(records)) {
			if (!grepl("_nodata$", cleanuri)) {
				stop("records missing")
			}
			d <- data.frame(ignore=TRUE)
			outf <- file.path(path, "data", "clean", group, paste0(cleanuri, ".csv"))
			write.csv(d, outf, row.names=FALSE)	
			mf <- gsub(".csv$", "_meta.csv", outf)
			utils::write.csv(metadata, mf, row.names=FALSE)
			return(TRUE)
		}
	}

	if (nrow(records) > 0) {
		dir.create(file.path(path, "data", "messages", group), FALSE, TRUE)
		dir.create(file.path(path, "data", "evaluation", group), FALSE, TRUE)
		records$dataset_id <- metadata$dataset_id
		opt <- options("carobiner_check")
		answ <- check_terms(metadata, records, timerecs, wth, group, check=opt)	
		fmsg <- file.path(path, "data", "messages", group, paste0(cleanuri, ".csv"))
		if (file.exists(fmsg)) file.remove(fmsg)
		
		feval <- file.path(path, "data", "evaluation", group, paste0(cleanuri, ".csv"))
		if (file.exists(feval)) file.remove(feval)
		e <- evaluate_quality(records, group)	
		data.table::fwrite(e, feval, row.names=FALSE)		
		
		if (!to_mem) {
			answ <- check_pubs(metadata, path, answ)
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
	timerecs <- sort_by_terms(timerecs, "timerecs", group)

	if (to_mem) {
		return(list(meta=metadata, data=records, long=timerecs))
	}
	
	outf <- file.path(path, "data", "clean", group, paste0(cleanuri, ".csv"))
	dir.create(dirname(outf), FALSE, FALSE)
	data.table::fwrite(records, outf, row.names=FALSE)

	if (!is.null(timerecs)) {
		outfw <- file.path(path, "data", "clean", group, paste0(cleanuri, "_long.csv"))
		data.table::fwrite(timerecs, outfw, row.names=FALSE)
	}

	if (!is.null(wth)) {
		wthf <- file.path(path, "data", "clean", group, paste0(cleanuri, "_wth.csv"))
		data.table::fwrite(wth, wthf, row.names=FALSE)
	}

	mf <- gsub(".csv$", "_meta.csv", outf)
	data.table::fwrite(metadata, mf, row.names=FALSE)
	
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


compile_carob <- function(path, group="", split_license=FALSE, zip=FALSE, excel=FALSE, cache=FALSE) {
	warn <- options("warn")
	if (warn$warn < 1) {
		on.exit(options(warn=warn$warn))
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

		print(grp)

		mi <- grepl("_meta.csv$", ff)
		li <- grepl("_long.csv$", ff)
		x <- sort_by_terms(.binder(ff[mi]), "metadata", grp)
		x[is.na(x)] <- ""
		x[] <- sapply(x, \(i) gsub("\n", " ", i))
		x[] <- sapply(x, \(i) gsub("\t", " ", i))
		y <- sort_by_terms(.binder(ff[!(mi|li)]), "records", grp)
# should be dealt with in source file
#		if ("reference" %in% colnames(y)) {
#			y$reference <- gsub("\n", " ", y$reference)
#			y$reference <- gsub("\t", " ", y$reference)
#		}
		z <- sort_by_terms(.binder(ff[li]), "records", grp)
		
		wf <- list.files(file.path(path, "data", "messages", grp), full.names=TRUE)
		if (length(wf) > 0) {
			wrn <- .binder(wf)
			have_warnings <- TRUE
		} else {
			have_warnings <- FALSE
		}
		
		gterms <- accepted_variables("records", grp)
		gterms <- gterms[, c("name", "type", "unit", "description")]

#		utils::write.csv(gterms, outft, row.names=FALSE)
		data.table::fwrite(gterms, outft, row.names=FALSE)
		if (split_license) {
			xx <- x[grepl("CC|ETALAB", x[,"license"]), ]
			yy <- y[y$dataset_id %in% xx[, "dataset_id"], ]

			if (have_warnings) wwrn <- wrn[wrn$dataset_id %in% xx[, "dataset_id"], ]
			if (nrow(xx) > 0) {
				outmf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_metadata-cc.csv"))
				#utils::write.csv(xx, outmf, row.names=FALSE)
				data.table::fwrite(xx, outmf, row.names=FALSE)
				outff <- file.path(path, "data", "compiled", paste0("carob", wgroup, "-cc.csv"))
				data.table::fwrite(yy, outff, row.names=FALSE)
				outwf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_warnings-cc.csv"))
				if (have_warnings) data.table::fwrite(wwrn, outwf, row.names=FALSE)
				
				if (length(z) > 0) {
					zz <- z[z$dataset_id %in% xx[, "dataset_id"], ]
					if (nrow(zz) > 0) {
						outlf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_long-cc.csv"))
						data.table::fwrite(zz, outlf, row.names=FALSE)
					} else {
						outlf <- NULL
					}
				} else {
					outlf <- NULL
				}
				if (zip) {
					fzip <- gsub(".csv$", ".zip", outff)
					if (file.exists(fzip)) file.remove(fzip)
					utils::zip(fzip, c(outft, outmf, outff, outlf, outwf), zipflags, zip=pzip)
				} 
				if (excel) {
					fxls <- gsub(".csv$", ".xlsx", outff)
					if (have_warnings) {
						dx <- list(sources=xx, terms=gterms, data=yy, warnings=wwrn)
					} else {
						dx <- list(sources=xx, terms=gterms, data=yy) # warnings=NULL)?
					}
					writexl::write_xlsx(dx, fxls)
				}
			}
		}
		if (have_warnings) {
			outwf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_warnings.csv"))
			data.table::fwrite(wrn, outwf, row.names=FALSE)
		}
		outmf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_metadata.csv"))
		data.table::fwrite(x, outmf, row.names=FALSE)
		outff <- file.path(path, "data", "compiled", paste0("carob", wgroup, ".csv"))
		data.table::fwrite(y, outff, row.names=FALSE)
		if (length(z) > 0) {
			outlf <- file.path(path, "data", "compiled", paste0("carob", wgroup, "_long.csv"))
			data.table::fwrite(z, outlf, row.names=FALSE)
		} else {
			outlf <- NULL
		}
		if (zip) {
			fzip <- gsub(".csv$", ".zip", outff)
			if (file.exists(fzip)) file.remove(fzip)
			utils::zip(fzip, c(outft, outmf, outff, outlf, outwf), flags=zipflags, zip=pzip)
		}
		if (excel) {
			fxls <- gsub(".csv$", ".xlsx", outff)
			dx <- list(sources=x, terms=gterms, data=y, warnings=wrn)
			writexl::write_xlsx(dx, fxls)
		}
		
		ret <- c(ret, outmf, outff)
	}
	utils::flush.console()
	ret
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

#	if (group != "") {
#		check_group(group)
#	}

	base <- file.path(path, "scripts")
	ffR <- list.files(file.path(base, group), pattern="\\.R$", full.names=TRUE, recursive=TRUE)
	ff_un <- grepl("^_", basename(ffR))
	ffR <- ffR[!ff_un]
	ffR <- ffR[!grepl("/_pending/", ffR)]
	#ffR <- ffR[!grepl("/_removed/", ffR)]
	#ffR <- ffR[basename(ffR) != "template.R"]

	if (length(ffR) == 0) {
		return(invisible(TRUE))
	}

	fcsv <- list.files(file.path(path, "data", "clean", group), pattern="_meta.csv$", full.names=TRUE, recursive=TRUE)
	fcsv <- gsub("_nodata", "", fcsv)
	
	if (length(fcsv) == 0) cache = FALSE
	
	if (cache) {
		have_csv <- data.frame(
			group = basename(dirname(fcsv)), 
			URI=tolower(gsub("_meta.csv$", "", basename(fcsv), ignore.case = TRUE)),
			csvfile = fcsv,
			csvtime = file.mtime(fcsv),
			data = TRUE) 
					
		have_R <- data.frame(
			group = basename(dirname(ffR)), 
			URI= tolower(gsub("\\.R$", "", basename(ffR), ignore.case = TRUE)),
			Rfile = ffR,
			Rtime = file.mtime(ffR),
			script = TRUE) 

		have <- merge(have_csv, have_R, by=c("group", "URI"), all=TRUE)

		i <- which(is.na(have$script))
		if (length(i) > 0) {
			# remove compiled data for which there is no matching script
			file.remove(have$csvfile[i])
			file.remove(gsub("_meta", "", have$csvfile[i]))
			have <- have[-i, ,drop=FALSE]
		}
		
		keep <- which(is.na(have$data) | (have$Rtime > have$csvtime))
		ffR <- have$Rfile[keep]
		
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


