
check_date <- function(x, name) {

	x <- stats::na.omit(x[[name]])
	if (length(x) == 0) return(TRUE)
	n <- nchar(x)
	if (any(!(n %in% c(4, 7, 10)))) {
		return(FALSE)
	}
	ans <- TRUE
	today <- as.Date(Sys.time())
	ymd <- x[n==10]
	if (length(ymd) > 0) {
		d <- as.Date(ymd)
		if (any((ymd < as.Date("1960-01-01")) | (ymd > today))) {
			ans <- FALSE
		}
	}
	thisyear <- as.numeric(format(today, "%Y"))
	today <- as.character(today)
	ym <- x[n==7]
	if (length(ym) > 0) {
		if (any((ym < "1960-01") | (ym > substr(today, 1, 7)))) ans <- FALSE
		d <- substr(ym, 5, 5)
		if (any(d != "-")) ans <- FALSE
		y <- as.numeric(substr(ym, 1, 4))
		if (any((y < 1960) | (y > thisyear))) ans <- FALSE
		m <- as.numeric(substr(ym, 6, 7))
		if (any((m < 1) | (m > 12))) ans <- FALSE
		
	}

	y <- x[n==4]
	if (length(y) > 0) {
		y <- as.numeric(y)
		if (any((y < 1960) | (y > thisyear))) {
			ans <- FALSE
		}
	}
	ans
}


check_outliers_iqr <- function(x, field, verbose=FALSE) {
	x <- x[[field]]
	if (is.null(x)) return(invisible(NULL))
	q <- quantile(x, c(0.25, 0.75), na.rm=TRUE)
	qrn <- diff(q)
	mn <- q[1] - qrn
	mx <- q[2] + qrn
	i <- (x < mn | x > mx)
	if (verbose && any(i)) {
		message(paste("   iq outliers:", field))
	}
	invisible(which(i))
}

check_outliers_std <- function(x, field, verbose=FALSE) {
	x <- x[[field]]
	if (is.null(x)) return(invisible(NULL))
	x <- na.omit(x)
	m <- mean(x)
	sd3 <- sd(x) * 3
	mn <- m - sd3
	mx <- m + sd3
	i <- (x < mn | x > mx)
	if (verbose && any(i)) {
		message(paste("   sd outliers:", field))
	}
	invisible(which(i))
}


check_ranges <- function(x, trms) {
	nms <- colnames(x)
	trms <- trms[match(nms, trms[,1]), ]
	trms <- trms[!(is.na(trms$valid_min) & is.na(trms$valid_max)), ]
	if (nrow(trms) == 0) return(TRUE)
	answ <- TRUE
	bad <- NULL
	for (i in 1:nrow(trms)) {
		rng <- trms[i,c("valid_min", "valid_max")]
		v <- stats::na.omit(x[[trms$name[i]]])
		if ( any(stats::na.omit(v < rng[1])) || any(stats::na.omit(v > rng[2])) ) {
			answ <- FALSE
			bad <- c(bad, trms$name[i])
		}
	}
	dats <- grep("_date", nms, value=TRUE)
	for (dat in dats) {
		if (!check_date(x, dat)) {
			answ <- FALSE
			bad <- c(bad, dat)
		}
	} 

	if (!answ) {
		bad <- paste(bad, collapse=", ")
		message(paste("   invalid:", bad))
	}
	answ
}


check_datatypes <- function(x, trms) {
	nms <- colnames(x)
	trs <- trms[match(nms, trms[,1]), ]
	cls <- cbind(sapply(x, class), trs$type, nms)
	cls <- cls[cls[,2] != "", ]
	i <- cls[,1] != cls[,2]
	answ <- TRUE
	if (any(i)) {
		bad <- paste(cls[i,3], collapse=", ")
		message(paste("   bad datatype:", bad))
		answ <- FALSE
	}
	answ
}



check_empty <- function(x) {
	bad <- rep(FALSE, ncol(x))
	chars <- sapply(x, is.character)
	for (i in which(chars)) {
		x[,i] <- trimws(x[,i])
		bad[i] <- any(stats::na.omit(x[,i]) == "")
	}
	if (any(bad)) {
		b <- paste0(colnames(x)[bad], collapse= ", ")
		message("   whitespace in variable: ", b)
	}
	x
}
#d = data.frame(a = 1:3, b=letters[1:3], c=c(" A ", "", "D"))
#x = check_empty(d)



check_terms <- function(x, type, path, group) {

	type <- match.arg(type, c("records", "dataset"))
	nms <- names(x)
	answ <- TRUE
	
	trms <- get_terms(type, group, path)

	xnms <- nms[!(nms %in% trms$name)]
	if (length(xnms) > 0) {
		message(paste("  unknown", type, "variable names: ", paste(xnms, collapse=", ")))
		answ <- FALSE		
	}
	
	req <- trms[trms$required == "yes" | trms$required == group, ]
	r <- req$name[!(req$name %in% nms)]
	if (length(r) > 0) {
		message(paste("  required", type, "variable name(s) missing: ", paste(r, collapse=", ")))
		answ <- FALSE
	}

	nms <- nms[nms %in% trms$name]
	trms <- trms[trms$name %in% nms, ]

	voc <- trms[!is.na(trms$vocabulary) & (trms$vocabulary != ""), ]
	voc <- voc[voc$name %in% nms, ]
	if (nrow(voc) > 0) {
		for (i in 1:nrow(voc)) {
			accepted <- utils::read.csv(file.path(path, "terms", paste0(voc$vocabulary[i], ".csv")))[,1]
			provided <- unique(x[, voc$name[i]])
			if (voc$required[i] != "yes") {
				provided <- stats::na.omit(provided)
			} 
			if (!is.null(voc$multiple_allowed)) {
				if (voc$multiple_allowed[i] == "yes") {
					provided <- unlist(strsplit(provided, "; "))
				}
			}
			bad <- provided[!(provided %in% accepted)]
			if (length(bad) > 0) {
				message(paste("  ", voc$name[i], "contains invalid terms: ", paste(bad, collapse=", ")))
				answ <- FALSE
			}
		}
	}

	if ((type=="dataset") & isTRUE(nchar(x$publication) > 0 )) {
		allpubs <- list.files(file.path(path, "references"))
		pubs <- unlist(strsplit(x$publication, ";"))
		pubs <- simple_uri(pubs)
		for (pub in pubs) {
			where <- grep(pub, allpubs)
			if (length(where) == 0) {
				cat("  reference file missing:", pub, "\n")	
				answ <- FALSE
			}
		}
	} 
	
	if (type=="records") {
		if (!check_datatypes(x[, nms], trms)) {
			answ <- FALSE
		} else {
			if (!check_ranges(x[, nms], trms)) answ <- FALSE
		}
		#check_outliers_iqr(x, "yield", TRUE)
	}
	invisible(answ)
}


#a = check_terms(x, type, path)

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

