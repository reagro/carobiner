
aggregated_data <- function(path, group, cc=FALSE) {
#	g <- get_groups()
#	if (!(group %in% g$name)) {
#		stop("this group does not exist")
#	}

	if (cc) {
		f <- file.path(path, "data", "compiled", paste0("carob_", group, "-cc.csv"))	
	} else {
		f <- file.path(path, "data", "compiled", paste0("carob_", group, ".csv"))
	}
	if (!file.exists(f)) {
		stop("these data do not exist. First run 'make_carob'?")
	}
	data.frame(data.table::fread(f))	
}


get_locations <- function(path, country=NULL) {
	ff <- list.files(file.path(path, "data/compiled"), pattern=".csv$", full.names=TRUE)
	ff <- ff[!grepl("-cc.csv$", ff)]
	ff <- ff[!grepl("_metadata.csv$", ff)]
	ff <- ff[!grepl("_terms.csv$", ff)]
	fields <- c("country", "adm1", "adm2", "adm3", "adm4", "location", "site", "longitude", "latitude")
	d <- lapply(ff, function(f) {
			x <- data.frame(data.table::fread(f))
			unique(x[,fields[fields %in% colnames(x)]])
		})
	d <- data.frame(do.call(carobiner::bindr, d))
	d <- d[!is.na(d$longitude), ]
	d <- d[order(d$country), ]
	if (!is.null(country)) {
		d <- d[d$country == country, ]
	}
	d
}
