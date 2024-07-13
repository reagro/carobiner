
update_terms <- function(quiet=FALSE, force=FALSE) {

	p <- system.file("terms", package="carobiner")
	dir.create(file.path(p, "variables"), FALSE, TRUE)
	dir.create(file.path(p, "values"), FALSE, TRUE)

	v <- readLines("https://api.github.com/repos/reagro/terminag/commits/main")
	gsha <- jsonlite::fromJSON(v)$sha

	f <- file.path(p, "sha.txt")
	if (!force && file.exists(f)) {
		rsha <- readLines(f)
		if (gsha == rsha) {
			if (!quiet) message("terms were up to date")
			return(invisible())
		}
	}
	writeLines(gsha, file.path(p, "sha.txt"))

	req <- httr::GET("https://api.github.com/repos/reagro/terminag/git/trees/main?recursive=1")
	httr::stop_for_status(req)
	ff <- sapply(httr::content(req)$tree, \(i) i$path)
	ff <- grep("\\.csv$", ff, value = TRUE)
	ff <- file.path("https://raw.githubusercontent.com/reagro/terminag/main", ff)
	i <- grepl("variables_", ff)
	pva <- c("values", "variables")[i+1]
	for (i in 1:length(ff)) {
		utils::download.file(ff[i], file.path(p, pva[i], basename(ff[i])), quiet=TRUE)
	}

	#gv <- readLines("https://raw.githubusercontent.com/reagro/terminag/main/version.txt", warn = FALSE)
	#gv <- trimws(unlist(strsplit(gv[grep("version", gv)], "="))[2])
	#f <- system.file("terms/version.txt", package="carobiner")
	#if (!file.exist(f)) return(TRUE)
	#rv <- readLines(f)
	#rv <- trimws(unlist(strsplit(rv[grep("version", rv)], "="))[2])

	if (!quiet) message("terms were updated")
	invisible()
}


#get_groups <- function(path) {
get_groups <- function() {
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


get_variables <- function(group) {
	path <- system.file("terms", package="carobiner")
	f <- file.path(path, "variables", paste0("variables_", group, ".csv"))		
	if (file.exists(f)) {
		utils::read.csv(f)	
	} else {
		NULL
	}
}


#get_terms <- function(type, group, path) {
accepted_variables <- function(type, group) {
	if (type == "records") {
		trms <- get_variables("all")
		if (is.null(trms)) {
			stop("Please first install the standard terms with 'carobiner::update_terms()'", call. = FALSE)
		}
		grps <- get_groups()
		include <- grps$include[grps$name == group]
		if (length(include) == 0) {
			include <- c("crop;soil")
		} 
		if (!all(include == "")) {
			include <- trimws(unlist(strsplit(include, ";")))
			for (inc in include) {
				add <- get_variables(inc)
				trms <- rbind(trms, add)
			}
		}
#		if (group != "") {
#			trms2 <- get_variables(group)
#			if (!is.null(trms2)) {
#				trms <- rbind(trms, trms2)
#				tab <- table(trms[,1])
#				if (any(tab > 1)) {
#					print(paste("duplicated terms:", names(tab[tab>1])))
#				}
#			}
#		}
	} else if (type=="metadata") {
		trms <- get_variables("metadata")
		if (is.null(trms)) {
			stop("Please first install the standard terms with 'carobiner::update_terms()'", call. = FALSE)
		}

	} else {
		stop("invalid 'type' argument; should be 'records' or 'metadata'") 
	}
	trms
}


accepted_values <- function(name) {
	path <- system.file("terms", package="carobiner")
	f <- file.path(path, "values", paste0("values_", name, ".csv"))
	if (file.exists(f)) {
		utils::read.csv(f)	
	} else {
		NULL
	}
}

