
update_terms <- function() {
	req <- httr::GET("https://api.github.com/repos/reagro/terminag/git/trees/main?recursive=1")
	httr::stop_for_status(req)
	ff <- sapply(httr::content(req)$tree, \(i) i$path)
	ff <- grep("\\.csv$", ff, value = TRUE)
	p <- system.file("terms", package="carobiner")
	ff <- file.path("https://raw.githubusercontent.com/reagro/terminag/main", ff)
	for (f in ff) {
		download.file(f, file.path(p, basename(f)), quiet=TRUE)
	}
	invisible()
}


get_groups <- function(path) {
#	f <- file.path(path, "terms", "groups.csv")
#	if (!isTRUE(file.exists(f))) {
		path <- system.file("terms", package="carobiner")
		f <- file.path(path, "groups.csv")
#	}
	if (!file.exists(f)) {
		stop("the groups file is missing")
	}
	utils::read.csv(f)	
}


get_variables <- function(path, group) {
	path <- system.file("terms", package="carobiner")
	f <- file.path(path, paste0("variables_", group, ".csv"))		
	if (file.exists(f)) {
		utils::read.csv(f)	
	} else {
		NULL
	}
}


get_terms <- function(type, group, path) {
	if (type == "records") {
		trms <- get_variables(path, "all")
		if (is.null(trms)) {
			stop("Please first install the standard terms with 'carobiner::update_terms()'", call. = FALSE)
		}
		grps <- get_groups(path)
		include <- grps$include[grps$name == group]
		if (length(include) == 0) {
			include <- c("crop;soil")
		} 
		if (include != "") {
			include <- trimws(unlist(strsplit(include, ";")))
			for (inc in include) {
				add <- get_variables(path, inc)
				trms <- rbind(trms, add)
			}
		}
		if (group != "") {
			trms2 <- get_variables(path, group)
			if (!is.null(trms2)) {
				trms <- rbind(trms, trms2)
				tab <- table(trms[,1])
				if (any(tab > 1)) {
					print(paste("duplicated terms:", names(tab[tab>1])))
				}
			}
		}
	} else if (type=="dataset") {
		trms <- get_variables(path, "dataset")
		if (is.null(trms)) {
			stop("Please first install the standard terms with 'carobiner::update_terms()'", call. = FALSE)
		}

	} else {
		stop("invalid 'type' argument; should be 'records' or 'dataset'") 
	}
	trms
}



get_accepted_values <- function(name, path=NULL) {
	path <- system.file("terms", package="carobiner")
	f <- file.path(path, paste0("values_", name, ".csv"))
	if (file.exists(f)) {
		utils::read.csv(f)	
	} else {
		NULL
	}
}

