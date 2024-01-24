
make_reports <- function(path, group="") {

	if (group[1] == "") {
		group <- get_groups(path)$name
	} 
	for (grp in group) {
		if (grepl("_trials$", grp)) {
			rmd <- file.path(path, "reports", "trials.Rmd")	
		} else {
			rmd <- file.path(path, "reports", paste0(grp, ".Rmd"))
		}
		if (!file.exists(rmd)) next
		rmd <- readLines(rmd, warn=FALSE)

		gpath <- file.path(path, "/data/clean/", grp)
		ff <- list.files(gpath, pattern="meta.csv$", full=TRUE)
		uri <- grep("^uri <- ", rmd)
		igrp <- grep("^group <- ", rmd)
		rmd[igrp] <- paste0("group <- '", grp, "'")
		
		on.exit(file.remove(file.path(path, "temp.Rmd")))
		for (f in ff) {
			outf <- gsub("_meta.csv", ".html", f)
			print(outf)
			m <- read.csv(f)
			rmd[uri] <- paste0("uri <- '", m$uri, "'")
			frmd <- file.path(path, "temp.Rmd")
			writeLines(rmd, frmd)
			rmarkdown::render(frmd, "html_document", "temp", envir=new.env())
			file.rename(file.path(path, "temp.html"), outf)
		}
	}
}

