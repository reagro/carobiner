
evaluate_quality <- function(x, group) {
	# are required variables present?
	reqs <- c("planting_date", "harvest_date", "N_fertilizer", "P_fertilizer", "K_fertilizer", "irrigated", "latitude", "longitude", "location")
	if (group == "survey") {
		reqs <- reqs[-c(1:2)]
	}
	if (is.null(x[["geo_from_source"]])) x[["geo_from_source"]] <- NA
	out <- data.frame(matrix(nrow=1, ncol=length(reqs)))
	names(out) <- reqs
	for (r in reqs) {
		if (is.null(x[[r]])) x[[r]] <- NA
	}
	if (group != "survey") {
		if (r %in% reqs[1:2]) {
			# not a full date
			x[[r]][nchar(x[[r]]) != 8] <- NA
		}
	}
	
	if (is.null(x$geo_from_source)) x$geo_from_source <- TRUE
	x$longitude[!x$geo_from_source] <- NA
	x$latitude[!x$geo_from_source] <- NA

	for (r in reqs) {
		out[[r]] <- mean(!is.na(x[[r]]))
	}
	
	data.frame(dataset_id = x$dataset_id[1], out)
}



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

check_date <- function(x, name, answ, voc) {

	x <- stats::na.omit(x[[name]])
	if (length(x) == 0) return(answ)
	if (any(grepl(";", x))) {
		i <- match(name, voc$name)
		if (length(i) == 1) {
			if (isTRUE(voc$multiple_allowed[i] != "yes")) {
				answ[nrow(answ)+1, ] <- c("date", paste0("multiple dates in: ", name))
			}
			x <- unlist(strsplit(x, ";|; "))
		}
	}
	
	n <- nchar(x)
	if (any(!(n %in% c(4, 7, 10)))) {
		answ[nrow(answ)+1, ] <- c("date", paste0("invalid date format(s) in: ", name))
	}
	today <- as.Date(Sys.time())
	ymd <- x[n==10]
#	if (any(is.na(ymd))) {
#		return(FALSE)
#	}
	if (length(ymd) > 0) {
		d <- as.Date(ymd)
		if (any(ymd < as.Date("1960-01-01"))) {
			answ[nrow(answ)+1, ] <- c("date", paste0("date(s) before 1960 in: ", name))
		}
		if (any(ymd > today)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("future date(s) in: ", name))
		}
		m <- as.numeric(substr(ymd, 6, 7))
		if (any((m < 1) | (m > 12))) {
			answ[nrow(answ)+1, ] <- c("date", paste0("months not between 1 and 12): ", name))
		} 
	}
	
	thisyear <- as.numeric(format(today, "%Y"))
	today <- as.character(today)
	ym <- x[n==7]
	if (length(ym) > 0) {
		d <- substr(ym, 5, 5)
		if (any(d != "-")) {
			answ[nrow(answ)+1, ] <- c("date", paste0("bad date(s) in: ", name))
		}
		y <- as.numeric(substr(ym, 1, 4))
		if (any(y < 1960)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("date(s) before 1960 in: ", name))
		} 
		if (any(y > thisyear)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("date(s) after ", thisyear, " in: ", name))
		}

		m <- as.numeric(substr(ym, 6, 7))
		if (any((m < 1) | (m > 12))) {
			answ[nrow(answ)+1, ] <- c("date", paste0("months not between 1 and 12): ", name))
		} 
	}

	y <- x[n==4]
	if (length(y) > 0) {
		y <- as.numeric(y)
		if (any(y < 1960)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("date(s) before 1960 in: ", name))
		} 
		if (any(y > thisyear)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("date(s) after ", thisyear, " in: ", name))
		}
	}
	answ
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


not_all_caps <- function(x, vars, answ) {
	locvars <- vars[vars %in% names(x)]
	for (v in locvars) {
		m <- na.omit(x[v])[,1]
		if (sum(toupper(m) == m) > (0.25 * length(m))) {
			answ[nrow(answ)+1, ] <- c("all uppercase",
				paste0("names in uppercase: ", v))
		}
	}
	if (("site" %in% locvars) & (!("location" %in% locvars))) {
		answ[nrow(answ)+1, ] <- c("location/site",
			"variable 'site' is not allowed if variable 'location' is absent")
	}
	answ
}



check_lonlat <- function(x, answ) {

	if (!all(c("longitude", "latitude") %in% colnames(x))) {
		# there is already a message for missing required variables
		return(answ)
	}
#	path <- system.file(package="carobiner")
	p <- file.path(rappdirs::user_data_dir(), ".carob")

#	wres <- ifelse(res=="high", 1, 5)
	wres <- 1
	w <- geodata::world(path=p, res=wres)
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

check_cropyield <- function(x, answ) {
	if (!all(c("crop", "yield") %in% names(x))) return(answ)
	if (all(is.na(x$yield))) return(answ)
	x <- x[, c("crop", "yield")]
	a <- suppressWarnings(
			stats::aggregate(x[,"yield", drop=FALSE], x[, "crop", drop=FALSE], max, na.rm=TRUE)
		)
	a <- a[which(a$yield < 100), ]
	if (nrow(a) > 0) {
		crops <- unique(a$crop)
		bad <- paste(crops, collapse=", ")
		answ[nrow(answ)+1, ] <- c("low yield",
				paste0("crop yield too low (tons not kg?): ", bad))
		return(answ)
	}
	trms <- accepted_values("crop")
	trms <- trms[match(unique(x$crop), trms$name), c("name", "max_yield")]
	trms <- stats::na.omit(trms)
	if (nrow(trms) == 0) return(answ)
	x <- stats::na.omit(merge(x, trms, by=1))
	i <- x$yield > x$max_yield
	if (any(i)) {
		crops <- unique(x$crop[i])
		if (length(crops) == 1) {
			bad <- paste0(crops, ": ", max(x$yield, na.rm=TRUE))
		} else {
			bad <- paste(crops, collapse=", ")
		}
		answ[nrow(answ)+1, ] <- c("high yield",
				paste0("crop yield too high? ", bad))
	}
	#check_outliers_iqr(x, "yield", TRUE)

	answ
}


check_ranges <- function(x, trms, answ) {
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
	check_cropyield(x, answ)
}


check_datatypes <- function(x, trms, answ) {
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
		answ <- check_ranges(x, trms, answ)
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


.check_group <- function(name) {
	grp <- get_groups()
	ok <- all(name %in% grp$name)
	if (!ok) {
		message(paste("    unknown group:", paste(name, collapse=", ")))
	}
	ok
}


check_metadata <- function(x, trms, answ) {
	if (grepl("http", x$uri)) {
		answ[nrow(answ)+1, ] <- c("uri", "http in uri")
	}
	nms <- trms$name[trms$NAok == "no"]
	j <- is.na(x[,nms])
	if (any(j)) {
		answ[nrow(answ)+1, ] <- c("NA values", paste0("NA in ", paste(nms[j], collapse=", ")))
	}
	answ
}


check_pubs <- function(x, path, answ) {
	if (isTRUE(nchar(x$publication) > 0 )) {
		if (!grepl("http", x$publication)) {
			allpubs <- list.files(file.path(path, "references"))
			pubs <- unlist(strsplit(x$publication, ";|; "))
			pubs <- carobiner:::simple_uri(pubs)
			for (pub in pubs) {
				where <- grep(pub, allpubs, fixed=TRUE)
				if (length(where) == 0) {
					answ[nrow(answ)+1, ] <- c("reference", paste("citation reference file missing:", pub))	
				}
			}
		}
	} 	
	answ
}



check_d_terms <- function(answ, x, type, group, check) {

	bad <- rep(FALSE, ncol(x))
	chars <- sapply(x, is.character)
	for (i in which(chars)) {
		x[,i] <- trimws(x[,i])
		bad[i] <- any(stats::na.omit(x[,i]) == "")
	}
	if (any(bad)) {
		b <- paste0(colnames(x)[bad], collapse= ", ")
		answ[nrow(answ)+1, ] <- c("whitespace", paste0("whitespace in variable: ", b))
	}
	nms <- names(x)
	tnms <- table(nms)
	if (any(tnms>1)) {
		tnms <- paste(tnms[tnms>1], collapse=", ")
		answ[nrow(answ)+1, ] <- c("duplicates", paste0("duplicate variable names: ", tnms))		
	}

	trms <- accepted_variables(type, group)

	xnms <- nms[!(nms %in% trms$name)]
	if (length(xnms) > 0) {
		answ[nrow(answ)+1, ] <- c("unknown variables", 
				paste("unknown variables: ", paste(xnms, collapse=", ")))
	}

	if (type == "weather") {
		if (!("date" %in% nms)) {
			answ[nrow(answ)+1, ] <- c("required variable missing",
					paste("required", type, "variable name(s) missing: ", paste(r, collapse=", ")))
		}
	} else if (type != "timerecs") {
		req <- trms[trms$required == "yes" | trms$required == group, "name"]
		r <- req[!(req %in% nms)]
		if (length(r) > 0) {
			answ[nrow(answ)+1, ] <- c("required variable missing",
					paste("required", type, "variable name(s) missing: ", paste(r, collapse=", ")))
		}
	}
	
	nms <- nms[nms %in% trms$name]
	trms <- trms[trms$name %in% nms, ]

	voc <- trms[!is.na(trms$vocabulary) & (trms$vocabulary != ""), ]
	voc <- voc[voc$name %in% nms, ]
	if (NROW(voc) > 0) {
		for (i in 1:nrow(voc)) {
			accepted <- accepted_values(voc$vocabulary[i])[,1]
			provided <- unique(x[, voc$name[i]])
			if (voc$required[i] != "yes") {
				provided <- stats::na.omit(provided)
			} 
			if (length(provided) > 0) {
				if (!is.null(voc$multiple_allowed)) {
					if (voc$multiple_allowed[i] == "yes") {
						if (!is.na(provided[1])) {
							provided <- unique(unlist(strsplit(provided, ";|; ")))
						}
					}
				}
				if (voc$vocabulary[i] == "crop") {
					if (any(grepl("_", provided))) {
						provided <- unique(unlist(strsplit(provided, "_")))
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
		dats <- grep("_date", nms, value=TRUE)
		for (dat in dats) {
			answ <- carobiner:::check_date(x, dat, answ, voc)
		} 
		
	}
	
	if (type=="metadata") {
		answ <- check_metadata(x, trms, answ)
	} else {
		answ <- check_datatypes(x[, nms], trms, answ)
		if (check != "nogeo") {
			answ <- check_lonlat(x, answ)	
		}
		if ((type == "records") & (!is.null(x$record_id))) {
			if (nrow(x) != length(unique(x$record_id))) {
				answ[nrow(answ)+1, ] <- c("duplicates", "duplicates in record_id")
			}		
		}
		answ <- not_all_caps(x, c(paste0("adm", 1:5), "site", "location"), answ)	
	}
	
	answ
}


## needs fixing. duplicates need to be considered together for recs and timerecs
find_duplicates <- function(answ, x, tmr=NULL) {
	if (is.null(tmr)) {
		if (nrow(x) != nrow(unique(x))) {
			answ[nrow(answ)+1, ] <- c("duplicates", "duplicate records detected")
		}
	}
	answ
}

check_treatment <- function(answ, treatment, data_type, vars) {
	if (is.na(treatment)) {
		answ[nrow(answ)+1, ] <- c("exp_treatment", 
			"metadata variable treatment_vars cannot be NA")
		return(answ)
	}
	
	treat <- trimws(unlist(strsplit(treatment, ";")))
	if ((length(treat) == 1) && (treat == "none")) {
		if (grepl("experiment|trial", data_type)) {
			answ[nrow(answ)+1, ] <- c("treatment_vars", 
				"metadata variable treatment_vars cannot be 'none' for experiments")
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


check_terms <- function(metadata, records, timerecs=NULL, wth=NULL, group="", check="all") {
	answ <- data.frame(check="", msg="")[0,]
	if (check == "none") {
		return(answ)
	}
	if (!missing(metadata)) {
		answ <- check_d_terms(answ, metadata, "metadata", group=group, check=check)
		if (!missing(records)) {
			if (!is.null(metadata$treatment_vars)) {
				answ <- check_treatment(answ, metadata$treatment_vars, metadata$data_type, names(records))
			}
		}
	}
	if (!missing(records)) {
		answ <- check_d_terms(answ, records, "records", group=group, check=check)
		answ <- find_duplicates(answ, records, timerecs)
	}
	if (!is.null(timerecs)) {
	
	  rcid <- !is.null(timerecs$record_id)
	  trid <- !is.null(timerecs$trial_id)
	  if ((rcid + trid) != 1) {
	    answ[nrow(answ)+1, ] <- c("id", "timerecs must have either record_id or trial_id")
	  } else if (rcid) {
	    if (is.null(records$record_id)) {
	      answ[nrow(answ)+1, ] <- c("id", "record_id in 'timerecs' but not in other records")
	    } else if (any(!(timerecs$record_id %in% records$record_id))) {
	      answ[nrow(answ)+1, ] <- c("id", "record_id(s) do not match")
	    }
	  } else {
	    if (is.null(records$trial_id)) {
	      answ[nrow(answ)+1, ] <- c("id", "trial_id in 'timerecs' but not in other records")
	    } else if (any(!(timerecs$trial_id %in% records$trial_id))) {
	      answ[nrow(answ)+1, ] <- c("id", "trial_id(s) do not match")
	    }
	  }
		answ <- check_d_terms(answ, timerecs, "timerecs", group=group, check=check)
		cns <- c(colnames(records), colnames(timerecs))
		cns <- cns[!(cns %in% c("dataset_id", "record_id", "trial_id"))]  # date?
		cns <- table(cns)
		if (any(cns>1)) {
			dups <- paste(names(cns[cns>1]), collapse=", ")
			answ[nrow(answ)+1, ] <- c("duplicates", paste("duplicate variables in records and timerecs:", dups))
		}
	}
	if (!is.null(wth)) {
		if (is.null(wth$date)) {
			answ[nrow(answ)+1, ] <- c("weather", "weather data does not have variable 'date'")			
		}
		answ <- check_d_terms(answ, wth, "records", group="weather", check=check)
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

