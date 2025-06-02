
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



check_cropyield <- function(x, answ) {
	if (!all(c("crop", "yield") %in% names(x))) return(answ)
	if (all(is.na(x$yield))) return(answ)
	x <- x[, c("crop", "yield")]
	x <- stats::na.omit(x)
	if (nrow(x) == 0) {
		return(answ)
	}
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
	trms <- vocal::accepted_values("crop", voc="carob-data/terminag")
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
			pubs <- yuri::simpleURI(pubs)
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



check_packages <- function(name, version) {
	if (packageVersion(name) < version) {
		stop(paste0('please update package ', name, " with:\n   remotes::install.github('carob-data/", name, "')"))
	}
}


check_d_terms <- function(answ, x, type, group, check) {

	check_packages("yuri", "0.1-2")
	check_packages("vocal", "0.1-0")
	
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

	trms <- vocal::accepted_variables(type, group, voc="carob-data/terminag")

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
			accepted <- vocal::accepted_values(voc$vocabulary[i], voc="carob-data/terminag")[,1]
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
			aw <- vocal::check_date(x, dat, voc) 
			answ <- rbind(answ, aw)
		} 
		
	}
	
	if (type=="metadata") {
		answ <- check_metadata(x, trms, answ)
	} else {
		aw <- vocal::check_type_range(x[, nms], trms)
		answ <- rbind(answ, aw)
		aw <- vocal::check_datespan(x, "planting_date", "harvest_date", smin=45, smax=366)
		answ <- rbind(answ, aw)

		answ <- check_consistency(x, answ)
		answ <- check_cropyield(x, answ)
		
		if (check != "nogeo") {
			if (all(c("longitude", "latitude") %in% colnames(x))) {
				aw <- vocal::check_lonlat(x)	
				answ <- rbind(answ, aw)
			}
		}
		if ((type == "records") & (!is.null(x$record_id))) {
			if (nrow(x) != length(unique(x$record_id))) {
				answ[nrow(answ)+1, ] <- c("duplicates", "duplicates in record_id")
			}		
		}
		
		locvars <- c(paste0("adm", 1:5), "site", "location")
		locvars <- locvars[locvars %in% names(x)]
		answ <- rbind(answ, vocal::check_caps(x, locvars, minchar=5, frac=0.1))
		if (("site" %in% locvars) & (!("location" %in% locvars))) {
			answ[nrow(answ)+1, ] <- c("location/site",
				"variable 'site' is not allowed if variable 'location' is absent")
		}
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

