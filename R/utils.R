

eng_months_to_nr <- function(x) {
	mnths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
	for (i in 1:12) {
		x <- sub(mnths[i], i, x, ignore.case=TRUE)
	}
	x
}



change_names <- function(x, from, to, must_have=TRUE) {
	stopifnot(length(from) == length(to))
	for (i in 1:length(from)) {
		w <- which(colnames(x) == from[i])
		if (length(w) > 1) {
			stop(paste(from[i], "is duplicated"), call.=FALSE)
		} else if (must_have && length(w) == 0) {
			stop(paste(from[i], "is absent"), call.=FALSE)
		}
		names(x)[w] <- to[i]
	}
	x
}



bindr <- function( ...) {
	d <- list(...)
	nms <- unique(unlist(lapply(d, names)))
	out <- lapply(d, function(x) {
		x <- x[, colnames(x)!="", drop=FALSE]
			data.frame(c(x, 
				sapply(setdiff(nms, names(x)), function(y) NA)), check.names=FALSE)
		})
	out$make.row.names <- FALSE
	do.call(rbind, out)
}

.binder <- function(ff) {
	#suppress "incomplete final line found by readTableHeader"
	x <- suppressWarnings(lapply(ff, utils::read.csv))
	nr <- sapply(x, nrow)
	x <- x[nr > 0]
	nms <- unique(unlist(lapply(x, names)))
	x <- lapply(x, function(x) data.frame(c(x, sapply(setdiff(nms, names(x)), function(y) NA))))
	x$make.row.names <- FALSE
	do.call(rbind, x)
}

fix_varnames <- function(x) {
	nms <- gsub("%", "pct", x)
	nms <- make.names(nms, unique=TRUE)
	nms <- gsub("\\.\\.\\.\\.", ".", nms)	
	nms <- gsub("\\.\\.\\.", ".", nms)	
	nms <- gsub("\\.\\.", ".", nms)	
	nms <- gsub("\\.$", "", nms)	
	make.names(nms, unique=TRUE)
}



replace_values <- function(x, from, to, must_have=TRUE) {
	stopifnot(length(from) == length(to))
	for (i in 1:length(from)) {
		if (must_have) {
			if (!all(from[i] %in% x)) {
				stop("not all names in 'from' are in 'x'")
			}
		}
		x[x==from[i]] <- to[i]
	}
	x
}

