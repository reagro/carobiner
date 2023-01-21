
check_ranges <- function(x, trms) {
	nms <- colnames(x)
	trms <- trms[match(nms, trms[,1]), ]
	trms <- trms[!(is.na(trms$valid_min) & is.na(trms$valid_max)), ]
	if (nrow(trms) == 0) return(TRUE)
	answ <- TRUE
	bad <- NULL
	for (i in 1:nrow(trms)) {
		rng <- trms[i,c("valid_min", "valid_max")]
		v <- na.omit(x[[trms$name[i]]])
		if ( any(na.omit(v < rng[1])) || any(na.omit(v > rng[2])) ) {
			answ <- FALSE
			bad <- c(bad, trms$name[i])
		}
	}
	if (!answ) {
		bad <- paste(bad, collapse=", ")
		message(paste("out of range:", bad))
	}
	answ
}


check_datatypes <- function(x, trms) {
	nms <- colnames(x)
	trs <- trms[match(nms, trms[,1]), ]
	types <- trs$type
	cls <- cbind(sapply(x, class), trs$type)
	cls <- cls[cls[,2] != "", ]
	i <- which(cls[,1] != cls[,2])
	answ <- TRUE
	if (!all(i)) {
		bad <- paste(cls[i,1], collapse=", ")
		message(paste("bad datatype:", bad))
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



check_terms <- function(x, type, path, group="") {

	type <- match.arg(type, c("records", "dataset"))
	nms <- names(x)
	answ <- TRUE
	
	trms <- get_terms(type, group, path)

	xnms <- nms[!(nms %in% trms$name)]
	if (length(xnms) > 0) {
		message(paste("  unknown", type, "variable names: ", paste(xnms, collapse=", ")))
		answ <- FALSE		
	}
	
	req <- trms[trms$required == "yes", ]
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
			# split by ; for the case there are multiple, if allowed
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
	}
	
	invisible(answ)
}


#a = check_terms(x, type, path)
