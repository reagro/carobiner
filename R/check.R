

check_consistency <- function(x, answ) {
	#e.g. if OM is used, then the type and amount should be specified 
	if (!is.null(x$crop_price)) {
		if (is.null(x$currency)) {
			answ[nrow(answ)+1, ] <- c("no currency", "crop_price variable used, but currency variable missing")
		} else {
			if (any(is.na(x$currency) & !is.na(x$crop_price))) {
				answ[nrow(answ)+1, ] <- c("currency missing", "crop_price values without currency values found")
			}
		}
	}
	answ
}

check_date <- function(x, name) {

	x <- stats::na.omit(x[[name]])
	if (length(x) == 0) return(TRUE)
	n <- nchar(x)
	if (any(!(n %in% c(4, 7, 10)))) {
		return(FALSE)
	}
	ok <- TRUE
	today <- as.Date(Sys.time())
	ymd <- x[n==10]
	if (length(ymd) > 0) {
		d <- as.Date(ymd)
		if (any((ymd < as.Date("1960-01-01")) | (ymd > today))) {
			ok <- FALSE
		}
	}
	thisyear <- as.numeric(format(today, "%Y"))
	today <- as.character(today)
	ym <- x[n==7]
	if (length(ym) > 0) {
		if (any((ym < "1960-01") | (ym > substr(today, 1, 7)))) ok <- FALSE
		d <- substr(ym, 5, 5)
		if (any(d != "-")) ok <- FALSE
		y <- as.numeric(substr(ym, 1, 4))
		if (any((y < 1960) | (y > thisyear))) ok <- FALSE
		m <- as.numeric(substr(ym, 6, 7))
		if (any((m < 1) | (m > 12))) ok <- FALSE
		
	}

	y <- x[n==4]
	if (length(y) > 0) {
		y <- as.numeric(y)
		if (any((y < 1960) | (y > thisyear))) {
			ok <- FALSE
		}
	}
	ok
}


check_start_end_dates <- function(x) {
	if (is.null(x$planting_date) || is.null(x$harvest_date)) {
		return(NULL)
	}
	i <- which((nchar(x$planting_date) == 10) & (nchar(x$harvest_date) == 10))
	if (length(i) == 0) return(NULL)
	s <- as.Date(x$planting_date[i])
	e <- as.Date(x$harvest_date[i])
	d <- as.numeric(e - s)
	if (any(d < 45)) {
		return(c("invalid", "harvest date within 45 days after planting"))
	} else if (any(d > 365)) {
		return(c("invalid", "harvest date more than 1 year after planting"))
	} else {
		return(NULL)
	}
}


check_lonlat <- function(x, path, answ) {

	if (!all(c("longitude", "latitude") %in% colnames(x))) {
		# there is already a message for missing required variables
		return(answ)
	}
	if (is.null(path)) {
		path <- tempdir()
	}

#	wres <- ifelse(res=="high", 1, 5)
	wres <- 1
	w <- geodata::world(path=file.path(path, "data"), res=wres)
	x <- unique(stats::na.omit(x[, c("country", "longitude", "latitude")]))
	e <- terra::extract(w, x[, c("longitude", "latitude")])
	e$country <- x$country
	e <- e[, c("NAME_0", "country")]
	i <- is.na(e$NAME_0)
	if (any(i)) {
		u <- unique(e$country[i])
		bad <- paste(u, collapse=", ")
		answ[nrow(answ)+1, ] <- c("not on land",
				paste0("coordinates not on land for: ", bad))
	} 
	e <- unique(stats::na.omit(e))
	i <- e$NAME_0 != e$country
	if (any(i)) {
		u <- apply(e[i, ,drop=FALSE], 1, paste, collapse="/")
		bad <- paste(u, collapse=", ")
		answ[nrow(answ)+1, ] <- c("wrong country",
				paste0("coordinates/country conflict: ", bad))
	}
	
	locvars <- c("country", paste0("adm", 1:5), "location", "site", "longitude", "latitude")
	i <- which(locvars %in% colnames(x))
	locs <- unique(x[, locvars[i]])
	xy <- unique(x[, c("longitude", "latitude")])
	if (nrow(xy) < (0.9 * nrow(locs))) {
		answ[nrow(answ)+1, ] <- c("duplicate coordinates", "fewer coordinates than locations")
	}
	return(answ)		
}

check_cropyield <- function(x, path, answ) {
	if (!all(c("crop", "yield") %in% names(x))) return(answ)
	x <- x[, c("crop", "yield")]
	a <- suppressWarnings(
			stats::aggregate(x[,"yield", drop=FALSE], x[, "crop", drop=FALSE], max, na.rm=TRUE)
		)
	a <- a[a$yield < 100, ]
	if (nrow(a) > 0) {
		crops <- unique(a$crop)
		bad <- paste(crops, collapse=", ")
		answ[nrow(answ)+1, ] <- c("low yield",
				paste0("crop yield too low (tons not kg?): ", bad))
		return(answ)
	}
	trms <- get_accepted_values("crop", path)
	trms <- trms[match(unique(x$crop), trms$name), c("name", "max_yield")]
	trms <- stats::na.omit(trms)
	if (nrow(trms) == 0) return(answ)
	x <- stats::na.omit(merge(x, trms, by=1))
	i <- x$yield > x$max_yield
	if (any(i)) {
		crops <- unique(x$crop[i])
		bad <- paste(crops, collapse=", ")
		answ[nrow(answ)+1, ] <- c("low yield",
				paste0("crop yield too high?: ", bad))
	}
	#check_outliers_iqr(x, "yield", TRUE)

	answ
}


check_ranges <- function(x, trms, path, answ) {
	nms <- colnames(x)
	trms <- trms[match(nms, trms[,1]), ]

	trmsna <- trms[!is.na(trms$NAok), ]
	trmsna <- trmsna[trmsna$NAok == "no", ]

	bad <- NULL
	for (i in 1:nrow(trmsna)) {
		v <- x[[trmsna$name[i]]]
		if (any(is.na(v))) {
			bad <- c(bad, trmsna$name[i])
		}
	}
	if (!is.null(bad)) {
		answ[nrow(answ)+1, ] <- c("NA terms", paste("NA in:", paste(bad, collapse=", ")))
		bad <- NULL
	}
	
	trms <- stats::na.omit(trms[, c("name", "valid_min", "valid_max"), ])
	if (nrow(trms) == 0) return(answ)
	for (i in 1:nrow(trms)) {
		rng <- unlist(trms[i,c("valid_min", "valid_max")])
		v <- stats::na.omit(x[[trms$name[i]]])
 		if ( any((v < rng[[1]]) | (v > rng[2])) ) {
			ok <- FALSE
			vrng <- range(v, na.rm=TRUE)
			if (is.numeric(vrng)) vrng <- round(vrng, 3)
			msg  <- paste0(trms$name[i], " (", vrng[1], ", ", vrng[2], ")")
			bad  <- c(bad, msg)
		}
	}
	if (!is.null(bad)) {
		answ[nrow(answ)+1, ] <- c("bounds", paste("out of bounds:", paste(bad, collapse=", ")))
		bad <- NULL
	}
	
	dats <- grep("_date", nms, value=TRUE)
	for (dat in dats) {
		if (!check_date(x, dat)) {
			bad <- c(bad, dat)
		}
	} 
	
	if (!is.null(bad)) {
		bad <- paste(bad, collapse=", ")
		answ[nrow(answ)+1, ] <- c("invalid", paste0("invalid: ", bad))
		bad <- NULL
	}

	bad <- check_start_end_dates(x)
	if (!is.null(bad)) {
		answ[nrow(answ)+1, ] <- bad
	}

	answ <- check_consistency(x, answ)
	check_cropyield(x, path, answ)
}


check_datatypes <- function(x, trms, path, answ) {
	nms <- colnames(x)
	trs <- trms[match(nms, trms[,1]), ]
	cls <- sapply(x, class)
	if (is.list(cls)) {
		i <- sapply(cls, length)
		i <- names(i[i>1])
		stop(paste("    bad datatype:", paste(i, collapse=", ")))
	}
	cls <- cbind(cls, trs$type, nms)
	cls <- cls[cls[,2] != "", ]
	i <- (cls[,1] == "integer") & (cls[,2] == "numeric")
	cls[i, 1] <- "numeric"
	i <- cls[,1] != cls[,2]
	if (any(i)) {
		bad <- paste(cls[i,3], collapse=", ")
		answ[nrow(answ)+1, ] <- c("bad datatype", paste("bad datatype:", bad))
	} else {
	#	answ <- check_ranges(x[, nms], trms, path, answ)
		answ <- check_ranges(x, trms, path, answ)
	}
	
	answ
}



.check_empty <- function(x, answ) {
	bad <- rep(FALSE, ncol(x))
	chars <- sapply(x, is.character)
	for (i in which(chars)) {
		x[,i] <- trimws(x[,i])
		bad[i] <- any(stats::na.omit(x[,i]) == "")
	}
	if (any(bad)) {
		b <- paste0(colnames(x)[bad], collapse= ", ")
		answ[nrow(answ)+1, ] <- c("whitespace variable", paste("   whitespace in variable: ", b))
	}
	answ
}
#d = data.frame(a = 1:3, b=letters[1:3], c=c(" A ", "", "D"))
#x = check_empty(d)


check_group <- function(name, path) {
	grp <- get_groups(path)
	ok <- all(name %in% grp$name)
	if (!ok) {
		stop(paste("    invalid group:", paste(name, collapse=", ")))
	}
	ok
}


check_dataset <- function(x, trms, path, answ) {
	if (grepl("http", x$uri)) {
		answ[nrow(answ)+1, ] <- c("uri", "http in uri")
	}
	nms <- trms$name[trms$NAok == "no"]
	j <- is.na(x[,nms])
	if (any(j)) {
		answ[nrow(answ)+1, ] <- c("NA values", paste0("NA in ", paste(nms[j], collapse=", ")))
	}

	if (isTRUE(nchar(x$publication) > 0 )) {

		if (grepl("http", x$publication)) {
			answ[nrow(answ)+1, ] <- c("publication", "http in publication")
		}

		allpubs <- list.files(file.path(path, "references"))
		pubs <- unlist(strsplit(x$publication, ";|; "))
		pubs <- simple_uri(pubs)
		for (pub in pubs) {
			where <- grep(pub, allpubs)
			if (length(where) == 0) {
				answ[nrow(answ)+1, ] <- c("reference", paste("citation reference file missing:", pub))	
			}
		}
	} 	
	answ
}


check_d_terms <- function(answ, x, path, type, group, check) {

	bad <- rep(FALSE, ncol(x))
	chars <- sapply(x, is.character)
	for (i in which(chars)) {
		x[,i] <- trimws(x[,i])
		bad[i] <- any(stats::na.omit(x[,i]) == "")
	}
	if (any(bad)) {
		b <- paste0(colnames(x)[bad], collapse= ", ")
		answ[nrow(answ)+1, ] <- c("whitespace", 
				paste0("whitespace in variable: ", b))
	}
	nms <- names(x)
	trms <- get_terms(type, group, path)

	xnms <- nms[!(nms %in% trms$name)]
	if (length(xnms) > 0) {
		answ[nrow(answ)+1, ] <- c("unknown variables", 
				paste("unknown variables: ", paste(xnms, collapse=", ")))
	}
	
	req <- trms[trms$required == "yes" | trms$required == group, "name"]
	r <- req[!(req %in% nms)]
	if (length(r) > 0) {
		answ[nrow(answ)+1, ] <- c("required variable missing",
				paste("required", type, "variable name(s) missing: ", paste(r, collapse=", ")))
	}

	nms <- nms[nms %in% trms$name]
	trms <- trms[trms$name %in% nms, ]

	voc <- trms[!is.na(trms$vocabulary) & (trms$vocabulary != ""), ]
	voc <- voc[voc$name %in% nms, ]
	if (nrow(voc) > 0) {
		for (i in 1:nrow(voc)) {
			accepted <- get_accepted_values(voc$vocabulary[i], path)[,1]
			provided <- unique(x[, voc$name[i]])
			if (voc$required[i] != "yes") {
				provided <- stats::na.omit(provided)
			} 
			if (length(provided) > 0) {
				if (!is.null(voc$multiple_allowed)) {
					if (voc$multiple_allowed[i] == "yes") {
						if (!is.na(provided[1])) {
							provided <- unlist(strsplit(provided, ";|; "))
						}
					}
				}
				if (voc$NAok[i]=="yes") {
					provided <- stats::na.omit(provided)
				}
				if (length(provided) > 0) {
					bad <- provided[!(provided %in% accepted)]
					if (length(bad) > 0) {
						bad <- sort(unique(bad))
						answ[nrow(answ)+1, ] <- c("invalid terms",
							paste(voc$name[i], "contains invalid terms: ", paste(bad, collapse=", ")))
					}
				}
			}
		}
	}
	
	if (type=="records") {
		answ <- check_datatypes(x[, nms], trms, path, answ)
		if (check != "nogeo") {
			answ <- check_lonlat(x, path, answ)	
		}
	} else {
		answ <- check_dataset(x, trms, path, answ)
	}
	answ
}


check_exp <- function(answ, treatment, data_type, vars) {
	if (is.na(treatment)) {
		answ[nrow(answ)+1, ] <- c("exp_treatment", 
			"dataset exp_treatment cannot be NA")
		return(answ)
	}
	
	treat <- trimws(unlist(strsplit(treatment, ";")))
	if ((length(treat) == 1) && (treat == "none")) {
		if (grepl("experiment|trial", data_type)) {
			answ[nrow(answ)+1, ] <- c("treatment_vars", 
				"dataset treatment_vars cannot be 'none' for experiments")
		}
		return(answ)
	}
	
	i <- !(treat %in% vars)
	if (any(i)) {
		answ[nrow(answ)+1, ] <- c("treatment_vars", 
			paste("treatment_vars is not a variable:",  paste(treat[i], collapse=", ")))
	}
	answ
}


check_terms <- function(dataset, records, path=NULL, group="", check="all") {
	answ <- data.frame(check="", msg="")[0,]
	if (check == "none") {
		return(answ)
	}
	if (!missing(dataset)) {
		answ <- check_d_terms(answ, dataset, path, "dataset", group=group, check=check)
		if (!missing(records)) {
			if (!is.null(dataset$treatment_vars)) {
				answ <- check_exp(answ, dataset$treatment_vars, dataset$data_type, names(records))
			}
		}
	}
	if (!missing(records)) {
		answ <- check_d_terms(answ, records, path, "records", group=group, check=check)
	}
	answ
}



find_outliers <- function(x, fields, method="iqr") {
	method <- match.arg(tolower(method), c("iqr", "std"))
	if (method == "iqr") {
		out <- lapply(fields, function(f) check_outliers_iqr(x, f))
	} else {
		out <- lapply(fields, function(f) check_outliers_std(x, f))	
	}
	names(out) <- fields
	out
}

