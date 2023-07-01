
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


read.excel <- function(f, ...) {
  suppressMessages(
		as.data.frame(
			readxl::read_excel(f, ...)
		)
	)
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

