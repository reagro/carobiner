
term_paths <- function(local_terms=NULL) {
   list(
	   git_path="reagro/terminag",
       local_path= NULL
   )
}


update_terms <- function(quiet=FALSE, force=FALSE) {

    org <- term_paths()
    if ((is.null(org$git_path)) & (is.null(org$local_path))) {
		warn("terms paths are empty")
		return(NULL)
	}


	p <- system.file("terms", package="carobiner")
	dir.create(file.path(p, "variables"), FALSE, TRUE)
	dir.create(file.path(p, "values"), FALSE, TRUE)
	
    if (!is.null(org$git_path)) {
		burl <- file.path("https://api.github.com/repos", org$git_path)
  	   	v <- readLines(file.path(burl, "commits/main"))
		gsha <- jsonlite::fromJSON(v)$sha

		f <- file.path(p, "sha.txt")
		continue <- TRUE
		if (!force && file.exists(f)) {
			rsha <- readLines(f)
			if (gsha == rsha) {
				if (!quiet) message("terms were up to date")
				continue <- FALSE
			}
		}
		if (continue) {
			writeLines(gsha, file.path(p, "sha.txt"))	
			req <- httr::GET(file.path(burl, "git/trees/main?recursive=1"))
			httr::stop_for_status(req)
			ff <- sapply(httr::content(req)$tree, \(i) i$path)
			ff <- grep("\\.csv$", ff, value = TRUE)
    		rurl <- file.path("https://raw.githubusercontent.com", org$git_path)
			ff <- file.path(rurl, "main", ff)
			i <- grepl("variables_", ff)
			pva <- c("values", "variables")[i+1]
			pva <- file.path(p, pva, basename(ff))
			for (i in 1:length(ff)) {
				utils::download.file(ff[i], pva[i], quiet=TRUE)
			}
			#gv <- readLines("https://raw.githubusercontent.com/reagro/terminag/main/version.txt", warn = FALSE)
			#gv <- trimws(unlist(strsplit(gv[grep("version", gv)], "="))[2])
			#f <- system.file("terms/version.txt", package="carobiner")
			#if (!file.exist(f)) return(TRUE)
			#rv <- readLines(f)
			#rv <- trimws(unlist(strsplit(rv[grep("version", rv)], "="))[2])
		}
	}
    if (!is.null(org$local_path)) {
    	lf <- list.files(org$local_path) 
		if (length(lf) > 0) {
		   	pf <- list.files(p)
			i <- grepl("variables_", pf)
			pva <- c("values", "variables")[i+1]
			pva <- file.path(p, pva, basename(ff))
		   	add <- tolower(basename(lf)) %in% tolower(basename(pf))    
			for (i in 1:length(lf)) {
				if (add[i]) {
					v1 <- read.csv(pf[i])
					v2 <- read.csv(lf[i])
					v <- NULL
					v <- try(rbind(v1, v2))
					if (!is.null(v)) write.csv(v, lf[i], row.names=FALSE)
				} else {
					file.copy(pf[i], pva[i], overwrite=TRUE)
				}
			}




			if (any(!i)) {

			}
		}
	}

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
	if (type=="metadata") {
		trms <- get_variables("metadata")
	} else if (type=="weather") {
		trms <- get_variables("all")
		if (is.null(trms)) {
			stop("Please first install the standard terms with 'carobiner::update_terms()'", call. = FALSE)
		}
		add <- get_variables("weather")
		trms <- rbind(trms, add)
	} else { #"records", "timerecs"
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

