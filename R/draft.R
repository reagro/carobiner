
quotes <- function(x) {
	paste0("\"", x, "\"") 
}

grepaste <- function(pattern, x, ignore.case=TRUE) {
	v <- grep(pattern, x, ignore.case=ignore.case, value=TRUE)
	if (length(v) > 1) {
		v <- paste0(v[1], "\"]], #", paste(v[-1], collapse=", "))
	} else if (length(v) > 0) {
		v <- paste0(v, "\"]],")
	}
	v
}	

grepr <- function(x) {
	r <- list(
		country = grepaste("country", x),
		adm1 = grepaste("adm1|region|state|estado", x),
		adm2 = grepaste("adm2|provinc|distri", x),
		adm3 = grepaste("adm3|ward|commun", x),
		location = grepaste("locat|village|site", x),
		site = grepaste("hamlet", x),
		latitude = grepaste("latitude|^lat", x),
		longititude = grepaste("longitude|^long", x),
		elevation = grepaste("^elev|altitude", x),
		treatment = grepaste("treat", x),
		crop = grepaste("crop", x),
		variety = grepaste("variety|variedad|cultivar|clone", x),
		planting_date = grepaste("plant.*date", x),
		harvest_date = grepaste("harv.*date", x),
		flowering_date = grepaste("flow.*date", x),
		N_fertilizer = grepaste("^N$", x, FALSE),
		P_fertilizer = grepaste("^P$", x, FALSE),
		K_fertilizer = grepaste("^K$", x, FALSE),
		S_fertilizer = grepaste("^S$", x, FALSE),
		B_fertilizer = grepaste("^B$", x, FALSE),
		Mg_fertilizer = grepaste("^Mg$", x, FALSE),
		Zn_fertilizer = grepaste("^Zn$", x, FALSE),
		yield = grepaste("yield", x)
	)
	empty <- character(0)
	e <- sapply(r, \(i) identical(i, empty))
	r[!e]
}

wdist <- function(x, trms) {
	d <- utils::adist(x, trms)
	a <- apply(d, 1, which.min)
	z <- data.frame(from=x, to=trms[a], dist=d[cbind(1:nrow(d), a)])
}


draft <- function(uri, path, group="draft", overwrite=FALSE) {

#uri <- "hdl:11529/10548230"
#group <- "survey"
#overwrite <- TRUE

	did <- yuri::simpleURI(uri)
	## check on_carob ...
	
	fscript <- file.path(path, "scripts/_draft", group, paste0(did, ".R"))
	dir.create(dirname(fscript), FALSE, TRUE)

	if (file.exists(fscript) && (!overwrite)) {
		stop(paste(fscript, "exists. Use 'overwrite=TRUE' to overwrite it"))
	}
	ff  <- carobiner::get_data(uri, path, group)

	meta <-	carobiner::get_metadata(uri, path, group, major=0, minor=0, draft=TRUE)
	v <- c(unlist(strsplit(meta$version, "\\.")), NA, NA)
	v <- as.character(v)
	v[is.na(v)] <- "NA"

	s <- readLines(system.file("tmp/tmp", package="carobiner"))

	s <- gsub("_description_", meta$description, s)
	s <- gsub("_major_", v[1], s)
	s <- gsub("_minor_", v[2], s)
	s <- gsub("_uri_", uri, s)
	s <- gsub("_group_", group, s)
	s <- gsub("_dataorg_", meta$data_organization, s)
	s <- gsub("_pub_", ifelse(is.na(meta$publication), "NA", quotes(meta$publication)), s)
	s <- gsub("_today_", as.character(as.Date(Sys.time())), s)
	s <- gsub("_design_", ifelse(is.na(meta$design), "NA", quotes(meta$design)), s)
	

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
	ef <- f

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
	# if there is only one xls, and it has multiple sheets, these should be evaluated. 
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
	er <- r
	
	if (any(is_other)) {
		nn <- sum(is_other)
		sq <- (n+1):(n+nn)
		r <- c(r, paste0(paste0("\tr", sq), " <- read.???(f", sq, ")"))	
	}
	r <- paste(r, collapse="\n")
	s <- gsub("_read_", r, s)
	
	if (is.null(ef)) {
		writeLines(s, fscript)
		message(fscript)
		return(invisible(fscript))
	}
	
	f <- gsub("\t", "", ef)
	r <- gsub("\t", "", er)
	r <- r[!grepl("#", r)]
	for (i in 1:length(f)) {
		eval(parse(text = f[i]))
		eval(parse(text = r[i]))
	}
	nms <- lapply(1:length(f), \(i) names(eval(parse(text = paste0("r", i)))))			
	g <- lapply(nms, grepr)
	txt <- NULL 
	for (i in 1:length(g)) {
		if (length(g[[i]]) > 0) {
			a <- data.frame(g[[i]])
			bod <- paste0("\t\t", names(a), " = r", i, "[[\"", a)
			j <- grep("crop", bod)
			bod[j] <- gsub("]]", "]])", gsub("= r", "= tolower(r", bod[j]))
			j <- length(bod)
			bod[j] <- gsub("]],", "]]", bod[j])
			bod <- paste(bod, collapse= "\n")
			txti <- paste0("\td", i, " <- data.frame(\n", bod, "\n\t)\n")
			txt <- c(txt, txti)
		}
	}
	
	if (is.null(txt)) {
		s <- gsub("_replacements_", "", s)
	} else {
		txt <- paste(txt, collapse="\n")
		s <- gsub("_replacements_", txt, s)	
	}

	#trms <- vocal::accepted_variables()	
	#trms <- trms[!(trms$group %in% c("all", "metadata")), "name"]
	
	
	writeLines(s, fscript)
	message(fscript)
	invisible(fscript)
}

#draft("hdl:11529/10548230", path, "survey", overwrite=TRUE)


