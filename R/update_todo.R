

update_todo <- function(path) {

	ftodo <- file.path(path, "todo", "to-do.csv")
	todo <- utils::read.csv(ftodo)
	todo$uri <- trimws(todo$uri)
	uri <- gsub("https://doi.org/", "doi:", todo$uri)
	uri <- gsub("https://hdl.handle.net/", "hdl:", uri)

	fdone <- list.files(file.path(path, "data", "compiled"), pattern="_metadata.csv$", full.names=TRUE)
	done <- do.call(rbind, lapply(fdone, utils::read.csv))

	i <- unique(stats::na.omit(match(done$uri, uri)))
	if (length(i) > 0) {
		message(paste("removed", i, "datasets from to-do"))
		todo <- todo[-i,]
		utils::write.csv(todo, ftodo, row.names=FALSE)
	}
}

