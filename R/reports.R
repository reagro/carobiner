

make_reports <- function(path, group="", cache=TRUE) {

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
		outf <- gsub("_meta.csv", ".html", ff)
		if (cache) {
			i <- file.exists(outf)
			j <- which(i)
			ptm <- file.info(ff[j])$mtime
			ftm <- file.info(outf[j])$mtime
			i[j[ptm > ftm]] <- FALSE
			ff <- ff[!i]
			outf <- outf[!i]
			if (length(ff) == 0) next
		}
		uri <- grep("^uri <- ", rmd)
		igrp <- grep("^group <- ", rmd)
		rmd[igrp] <- paste0("group <- '", grp, "'")
		
		on.exit(file.remove(file.path(path, "temp.Rmd")))
		for (i in 1:length(ff)) {
			print(outf[i])
			m <- read.csv(ff[i])
			rmd[uri] <- paste0("uri <- '", m$uri, "'")
			frmd <- file.path(path, "temp.Rmd")
			writeLines(rmd, frmd)
			rmarkdown::render(frmd, "html_document", "temp", envir=new.env(), quiet=TRUE)
			file.rename(file.path(path, "temp.html"), outf[i])
		}
	}
}

