

draft <- function(uri, path, group, overwrite=FALSE) {

uri <- "hdl:11529/10548230"
group <- "survey"
overwrite <- TRUE

	did <- yuri::simpleURI(uri)
	## check on_carob ...
	
	fscript <- file.path(path, "scripts/_draft", group, paste0(did, ".R"))
	
	if (file.exists(fscript) && (!overwrite)) {
		error(paste(fscript, "exists. Use 'overwrite=TRUE' to overwrite it"))
	}
	ff  <- carobiner::get_data(uri, path, group)
	meta <-	carobiner::get_metadata(uri, path, group, major=0, minor=0, FALSE)
	v <- c(unlist(strsplit(meta$version, "\\.")), 0, 0)

	s <- readLines(system.file("tmp/tmp", package="carobiner"))

	s <- gsub("_description_", meta$description, s)
	s <- gsub("_major_", v[1], s)
	s <- gsub("_minor_", v[2], s)
	s <- gsub("_uri_", uri, s)
	s <- gsub("_group_", group, s)
	s <- gsub("_today_", as.character(as.Date(Sys.time())), s)

	n <- length(ff)
	f <- paste(paste0(paste0("\tf", 1:n), ' <- ff[basename(ff) == "', basename(ff), '"]'), collapse="\n")
	s <- gsub("_filename_", f, s)

	fcsv <- grepl("\\.csv$", ff) * 1
	fxls <- grepl("\\.xlsx$|\\.xls$", ff) * 2
	i <- pmax(fcsv, fxls) + 1
	rd <- c("read.???(", "read.csv(", "carobiner::read.excel(")[i]
	r <- paste(paste0(paste0("\tr", 1:n), " <- ", rd, paste0("f", 1:n), ")"), collapse="\n")
	s <- gsub("_read_", r, s)

	dir.create(dirname(fscript), FALSE, TRUE)
	writeLines(s, fscript)
	message(fscript)
	invisible(fscript)
}

#draft("hdl:11529/10548230", path, "survey", overwrite=TRUE)


