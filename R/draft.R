
quotes <- function(x) paste0("\"", x, "\"") 

draft <- function(uri, path, group="draft", overwrite=FALSE) {

#uri <- "hdl:11529/10548230"
#group <- "survey"
#overwrite <- TRUE

	did <- yuri::simpleURI(uri)
	## check on_carob ...
	
	fscript <- file.path(path, "scripts/_draft", group, paste0(did, ".R"))

	if (file.exists(fscript) && (!overwrite)) {
		stop(paste(fscript, "exists. Use 'overwrite=TRUE' to overwrite it"))
	}
	ff  <- carobiner::get_data(uri, path, group)

	meta <-	carobiner::get_metadata(uri, path, group, major=0, minor=0, draft=TRUE)
	v <- c(unlist(strsplit(meta$version, "\\.")), 0, 0)

	s <- readLines(system.file("tmp/tmp", package="carobiner"))

	s <- gsub("_description_", meta$description, s)
	s <- gsub("_major_", v[1], s)
	s <- gsub("_minor_", v[2], s)
	s <- gsub("_uri_", uri, s)
	s <- gsub("_group_", group, s)
	s <- gsub("_dataorg_", meta$data_organization, s)
	s <- gsub("_pub_", ifelse(is.na(meta$publication), "", meta$publication), s)
	s <- gsub("_today_", as.character(as.Date(Sys.time())), s)
	

	f <- NULL
	n <- 0
	is_xls <- grepl("\\.xlsx$|\\.xls$", ff)
	if (any(is_xls)) {
		fxls <- ff[is_xls]
		n <- length(fxls)
		f <- paste0(paste0("\tf", 1:n), ' <- ff[basename(ff) == "', basename(fxls), '"]')
	}
	is_csv <- grepl("\\.csv$", ff)
	if (any(is_csv)) {
		fcsv <- ff[is_csv]
		n <- n+length(fcsv)
		f <- c(f, paste0(paste0("\tf", (length(f)+1):n), ' <- ff[basename(ff) == "', basename(fcsv), '"]'))
	}
	is_other <- !(is_xls | is_csv)
	if (any(is_other)) {
		foth <- ff[is_other]
		n <- n+length(fcsv)
		f <- c(f, paste0(paste0("\tf", (length(f)+1):n), ' <- ff[basename(ff) == "', basename(foth), '"]'))
	}
	wf <- paste(f, collapse="\n")
	s <- gsub("_filename_", wf, s)

	r <- NULL
	n <- 1
	if (any(is_xls)) {
		for (i in 1:length(fxls)) {
			sheets <- readxl::excel_sheets(fxls[i])
			if (length(sheets) == 1) {	
				r <- c(r, paste0(paste0("\tr", n), " <- carobiner::read.excel(f", n, ")"))
			} else {
				dst <- utils::adist("data", sheets, ignore.case=TRUE)
				j <- which.min(dst)
				sels <- sheets[j]
				r <- c(r, paste0(paste0("\tr", n), " <- carobiner::read.excel(f", n, ", sheet=\"", sels, "\")"))
				r <- c(r, paste("# other sheets: ", paste(sheets[-j], collapse=", ")))
			}
			n <- n + 1
		}
	}

	if (any(is_csv)) {
		nn <- sum(is_csv)
		sq <- (n+1):(n+nn)
		r <- c(r, paste0(paste0("\tr", sq), " <- read.csv(f", sq, ")"))
		n <- n + nn
	}
	
	if (any(is_other)) {
		nn <- sum(is_other)
		sq <- (n+1):(n+nn)
		r <- c(r, paste0(paste0("\tr", sq), " <- read.???(f", sq, ")"))	
	}
	r <- paste(r, collapse="\n")
	s <- gsub("_read_", r, s)
	
	dir.create(dirname(fscript), FALSE, TRUE)
	writeLines(s, fscript)
	message(fscript)
	invisible(fscript)
}

#draft("hdl:11529/10548230", path, "survey", overwrite=TRUE)


