


find_outliers <- function(x, fields, method="iqr") {
	method <- match.arg(tolower(method), c("iqr", "std"))
	if (method == "iqr") {
		out <- lapply(fields, function(f) check_outliers_iqr(x, f))
	} else {
		out <- lapply(fields, function(f) check_outliers_std(x, f))	
	}
	names(out) <- fields
	out
}



evaluate_quality <- function(x, group) {
	# are required variables present?
	reqs <- c("planting_date", "harvest_date", "N_fertilizer", "P_fertilizer", "K_fertilizer", "irrigated", "latitude", "longitude", "location")
	if (group == "survey") {
		reqs <- reqs[-c(1:2)]
	}
	if (is.null(x[["geo_from_source"]])) x[["geo_from_source"]] <- NA
	out <- data.frame(matrix(nrow=1, ncol=length(reqs)))
	names(out) <- reqs
	for (r in reqs) {
		if (is.null(x[[r]])) x[[r]] <- NA
	}
	if (group != "survey") {
		if (r %in% reqs[1:2]) {
			# not a full date
			x[[r]][nchar(x[[r]]) != 8] <- NA
		}
	}
	
	if (is.null(x$geo_from_source)) x$geo_from_source <- TRUE
	x$longitude[!x$geo_from_source] <- NA
	x$latitude[!x$geo_from_source] <- NA

	for (r in reqs) {
		out[[r]] <- mean(!is.na(x[[r]]))
	}
	
	data.frame(dataset_id = x$dataset_id[1], out)
}


