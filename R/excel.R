
is_excel <- function(f) {
	v <- readBin(f, "raw", n=4)
	all(as.numeric(v) == c(80, 75, 3, 4))
}



read.excel <- function(f, fix_names=FALSE, lower=FALSE, ...) {
	suppressMessages(x <- as.data.frame(readxl::read_excel(f, ...)))
	if (fix_names) {
		colnames(x) <- fix_varnames(colnames(x))
	}
	if (lower) {
		colnames(x) <- tolower(colnames(x))
	}
	x
}

read.excel.hdr <- function(f, skip, hdr=1, fix_names=TRUE, lower=FALSE, ...) {
	skip <- as.integer(skip)
	hdr <- as.integer(hdr)
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
	if (lower) {
		nms <- tolower(nms)
	}
	colnames(x) <- nms[1:ncol(x)]
	x
}


