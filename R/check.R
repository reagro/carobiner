
check_empty <- function(x) {
	bad <- rep(FALSE, ncol(x))
	for (i in 1:ncol(x)) {
		x[,i] <- trimws(x[,i])
		bad[i] <- any(na.omit(x[,i]) == "")
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
	trms <- get_terms(type, group)

	xnms <- nms[!(nms %in% trms$name)]
	if (length(xnms) > 0) {
		print(paste("  unknown", type, "variable names: ", paste(xnms, collapse=", ")))
		answ <- FALSE
	}
	req <- trms[trms$required == "yes", ]
	r <- req$name[!(req$name %in% nms)]
	if (length(r) > 0) {
		print(paste("  required", type, "variable name(s) missing: ", paste(r, collapse=", ")))
		answ <- FALSE
	}
	req <- req[!is.na(req$vocabulary) & (req$vocabulary != ""), ]
	if (nrow(req) > 0) {
		for (i in 1:nrow(req)) {
			accepted <- utils::read.csv(file.path(path, "terms", paste0(req$vocabulary[i], ".csv")))[,1]
			provided <- unique(x[, req$name[i]])
			# split by ; for the case there are multiple, if allowed
			
			bad <- provided[!(provided %in% accepted)]
			if (length(bad) > 0) {
				message(paste("  ", req$name[i], "contains invalid terms: ", paste(bad, collapse=", ")))
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
	invisible(answ)
}

