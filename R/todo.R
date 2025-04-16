
update_todo <- function(path) {

	ftodo <- file.path(path, "misc", "todo", "to-do.csv")
	if (!file.exists(ftodo)) return(invisible(FALSE))
	
	todo <- utils::read.csv(ftodo)
	todo$uri <- trimws(todo$uri)
	uri <- gsub("https://doi.org/", "doi:", tolower(todo$uri))
	uri <- gsub("https://hdl.handle.net/", "hdl:", uri)

	id <- tolower(sapply(uri, yuri::simpleURI, USE.NAMES=F))
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

