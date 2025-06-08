
.carobiner_environment <- new.env(parent=emptyenv())


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
		answ[nrow(answ)+1, ] <- c("low yield (tons not kg?)", bad)
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
		answ[nrow(answ)+1, ] <- c("high crop yield", bad)	
	}
	#check_outliers_iqr(x, "yield", TRUE)

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
					answ[nrow(answ)+1, ] <- c("reference file missing", pub)
				}
			}
		}
	} 	
	answ
}



check_packages <- function(name, version) {
	if (utils::packageVersion(name) < version) {
		stop(paste0('please update package ', name, " with:\n   remotes::install.github('carob-data/", name, "')"))
	}
}



check_timerecs <- function(timerecs, records, answ) {
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
	cns <- c(colnames(records), colnames(timerecs))
	cns <- cns[!(cns %in% c("dataset_id", "record_id", "trial_id"))]  # date?
	cns <- table(cns)
	if (any(cns>1)) {
		dups <- paste(names(cns[cns>1]), collapse=", ")
		answ[nrow(answ)+1, ] <- c("duplicates", paste("duplicate variables in records and timerecs:", dups))
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


check_treatments <- function(answ, treatment, exp_type, vars, type) {
	if (is.na(treatment)) {
		answ[nrow(answ)+1, ] <- c("metadata", paste(type, "cannot be NA"))
		return(answ)
	}
	
	treat <- trimws(unlist(strsplit(treatment, ";")))
	if (isTRUE(any(treat == "none"))) {
		if (type == "treatment") {
			if (grepl("experiment|trial", exp_type)) {
				answ[nrow(answ)+1, ] <- c("metadata", "treatment_vars cannot be 'none' for experiments")
			}
			return(answ)
		} else {
			answ[nrow(answ)+1, ] <- c("metadata", "response_vars cannot be 'none'")
		}
	}
	i <- !(treat %in% vars)
	if (any(i)) {
		answ[nrow(answ)+1, ] <- c("metadata", 
			paste("not a variable in the data:",  paste(treat[i], collapse=", ")))
	}
	answ
}


check_combined <- function(x, trms, voc) {
	a1 <- vocal::check_variables(x, trms)
	a2 <- vocal::check_values(x, trms, voc)
	dats <- grep("_date", names(x), value=TRUE)
	if (length(dats) > 0) {
		a3 <- do.call(rbind, lapply(dats, \(dat) vocal::check_date(x, dat, trms)))
		return( rbind(a1, a2, a3) )
	}
	return( rbind(a1, a2) )
}

check_weather <- function(x, answ) {
	voc="carob-data/terminag"
	trms <- vocal::accepted_variables(voc, c("all", "location", "weather"))	
	answ <- check_combined(x, trms, voc)
	if (is.null(x$date)) {
		answ[nrow(answ)+1, ] <- c("weather", "variable 'date' is missing")			
	}
	answ
}


check_metadata <- function(x, voc="carob-data/terminag") {
	trms <- vocal::accepted_variables(voc, "metadata")	
	answ <- check_combined(x, trms, voc)
	if (grepl("http", x$uri)) {
		answ[nrow(answ)+1, ] <- c("uri", "http in uri")
	}
	answ
}


get_groupvars <- function(group) {		
	vars <- c("all", "location", "crop", "soil")
	if (grepl("maize", group)) vars <- c(vars, "maize")
	if (grepl("wheat", group)) vars <- c(vars, "wheat")
	if (grepl("survey", group)) vars <- c(vars, "survey")
	vars
}


check_records <- function(answ, x, group, check) {
	voc <- "carob-data/terminag"
	vars <- get_groupvars(group)
	trms <- vocal::accepted_variables(voc, vars)
	answ <- check_combined(x, trms, voc)

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
	if (!is.null(x$record_id)) {
		if (nrow(x) != length(unique(x$record_id))) {
			answ[nrow(answ)+1, ] <- c("duplicates", "duplicates in record_id")
		}		
	}
		
	locvars <- c(paste0("adm", 1:5), "site", "location")
	locvars <- locvars[locvars %in% names(x)]
	answ <- rbind(answ, vocal::check_caps(x, locvars, minchar=5, frac=0.1))
	if (("site" %in% locvars) & (!("location" %in% locvars))) {
		answ[nrow(answ)+1, ] <- c("location/site", "variable 'site' is not allowed if variable 'location' is absent")
	}
	answ
}



check_terms <- function(metadata=NULL, records=NULL, timerecs=NULL, wth=NULL, group="", check="all") {

	check_packages("yuri", "0.1-5")
	check_packages("vocal", "0.2-1")

	answ <- data.frame(check="", msg="")[0,]
	if (check == "none") {
		return(answ)
	}
	if (!is.null(metadata)) {
		answ <- check_metadata(metadata)
		if (!missing(records)) {
			if (!is.null(metadata$treatment_vars)) {
				answ <- check_treatments(answ, metadata$treatment_vars, metadata$data_type, names(records), "treatment")
			}
			if (!is.null(metadata$response_vars)) {
				answ <- check_treatments(answ, metadata$response_vars, metadata$data_type, names(records), "response")
			}
		}
	}
	if (!is.null(records)) {
		answ <- check_records(answ, records, group=group, check=check)
		answ <- find_duplicates(answ, records, timerecs)
	}
	if (!is.null(timerecs)) {
		answ <- check_timerecs(timerecs, records, answ)
	}
	
	if (!is.null(wth)) {
		answ <- check_weather(wth, answ)
	}
	answ
}

