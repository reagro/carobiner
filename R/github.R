

update_todo <- function(path) {

	ftodo <- file.path(path, "misc", "todo", "to-do.csv")
	if (!file.exists(ftodo)) return(invisible(FALSE))
	
	todo <- utils::read.csv(ftodo)
	todo$uri <- trimws(todo$uri)
	uri <- gsub("https://doi.org/", "doi:", tolower(todo$uri))
	uri <- gsub("https://hdl.handle.net/", "hdl:", uri)

	id <- tolower(sapply(uri, simple_uri, USE.NAMES=F))
	done <- basename(list.files(file.path(path, "scripts"), pattern="^doi|^hdl.*\\.R$", recursive=TRUE))
	done <- gsub("\\.r$", "", tolower(done))
	i <- which(id %in% done)

#	fdone <- list.files(file.path(path, "data", "compiled"), pattern="_metadata.csv$", full.names=TRUE)
#	done <- do.call(bindr, lapply(fdone, utils::read.csv))
#	i <- unique(stats::na.omit(match(tolower(done$uri), uri)))
	if (length(i) > 0) {
#		message(paste("removed", length(i), "datasets from to-do"))
		todo <- todo[-i,]
		utils::write.csv(todo, ftodo, row.names=FALSE)
	}
	invisible(TRUE)
}



on_github <- function(uri=NULL) {
	req <- httr::GET("https://api.github.com/repos/reagro/carob/git/trees/master?recursive=1")
	httr::stop_for_status(req)
	ff <- sapply(httr::content(req)$tree, \(i) i$path)
	ff <- grep("scripts/", ff, value = TRUE)
	ff <- grep("\\.R$", ff, value = TRUE)
	ff <- gsub("scripts/", "", ff)
	ff <- ff[!grepl("_functions.R", ff)]
	fb <- gsub("\\.R$", "", basename(ff))
	fd <- dirname(ff)	
	pd <- grepl("_pending", fd)
	fd <- gsub("_pending/", "", fd)
	d <- data.frame(dataset=fb, group=fd, pending=pd)
	d <- d[order(d$group), ]
	if (!is.null(uri)) {
		uri <- simple_uri(uri)
		i <- stats::na.omit(match(uri, d$dataset))
		return(d[i,])
	} 
	d
}

