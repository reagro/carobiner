
get_references <- function(x, path, format=TRUE) {
	uri <- unique(x$uri)
	if (length(uri == 0)) {
		stop("no `uri` field", call.=FALSE)
	}
	refs <- list.files(file.path(path, "references"), full.names=TRUE)
	rn <- tools::file_path_sans_ext(basename(refs))
	i <- sapply(uri, function(u) grep(u, rn))
	if (length(i == 0)) {
		return("")
	} 
	d <- revtools::read_bibliography(refs[i])
	if (format) {
		dd <- revtools::format_citation(d)
		names(dd) <- NULL
		return(dd)
	}
	rownames(d) <- NULL
	d$filename <- NULL
	d$ID <- NULL
	d$DA <- NULL
	d$url <- NULL
	d$label <- NULL
	d
}
