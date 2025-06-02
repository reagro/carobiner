

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
		file.remove(list.files(file.path(path, "data", "clean", group), cleanuri, full.names=TRUE))

		if (missing(records)) {
			if (!grepl("_nodata$", cleanuri)) {
				stop("records missing")
			}
			d <- data.frame(ignore=TRUE)
			outf <- file.path(path, "data", "clean", group, paste0(cleanuri, ".csv"))
			utils::write.csv(d, outf, row.names=FALSE)	
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
				ign <- apply(ign, 1, function(i) paste(i, collapse="#"))
				ans <- apply(answ, 1, function(i) paste(i, collapse="#"))
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

	if (nrow(records) > 0) {
		metadata$crops <- paste(sort(unique(records$crop)), collapse=";")
		metadata$countries <- paste(sort(unique(records$country)), collapse=";")
	}
	
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

