
is_excel <- function(f) {
	v <- readBin(f, "raw", n=4)
	all(as.numeric(v) == c(80, 75, 3, 4))
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
	x <- list(...)
	nms <- unique(unlist(lapply(x, names)))
	x <- lapply(x, function(x) data.frame(c(x, sapply(setdiff(nms, names(x)), function(y) NA))))
	x$make.row.names <- FALSE
	do.call(rbind, x)
}

.binder <- function(ff) {
	#suppress "incomplete final line found by readTableHeader"
	x <- suppressWarnings(lapply(ff, utils::read.csv))
	nms <- unique(unlist(lapply(x, names)))
	x <- lapply(x, function(x) data.frame(c(x, sapply(setdiff(nms, names(x)), function(y) NA))))
	x$make.row.names <- FALSE
	do.call(rbind, x)
}

fix_varnames <- function(x) {
	nms <- make.names(x, unique=TRUE)
	nms <- gsub("\\.\\.\\.\\.", ".", nms)	
	nms <- gsub("\\.\\.\\.", ".", nms)	
	nms <- gsub("\\.\\.", ".", nms)	
	nms <- gsub("\\.$", "", nms)	
	make.names(nms, unique=TRUE)
}


read.excel <- function(f, fix_names=FALSE, ...) {
	suppressMessages(x <- as.data.frame(readxl::read_excel(f, ...)))
	if (fix_names) {
		colnames(x) <- fix_varnames(colnames(x))
	}
	x
}

read.excel.hdr <- function(f, skip, hdr=1, fix_names=TRUE, ...) {
	skip = as.integer(skip)
	hdr = as.integer(hdr)
	if ((skip==0) && (hdr==1)) {
		return(read.excel(f, fix_names=fix_names, ...))
	}
	stopifnot(skip >= 0)
	stopifnot(hdr >= 1)
	stopifnot(skip >= hdr)
	suppressMessages(x <- as.data.frame(readxl::read_excel(f, skip=skip, ...)))
	suppressMessages(nms <- as.data.frame(readxl::read_excel(f, skip=skip-hdr, n_max=hdr+1, col_names=FALSE, ...)))
	nms <- apply(nms, 2, \(i) paste(i[!is.na(i)], collapse="_"))
	if (fix_names) {
		nms <- fix_varnames(nms)
	}
	colnames(x) <- nms
	x
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

