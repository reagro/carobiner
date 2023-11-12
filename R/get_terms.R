
get_groups <- function(path) {
	f <- file.path(path, "terms", "groups.csv")
	if (!isTRUE(file.exists(f))) {
		path <- system.file("terms", package="carobiner")
		f <- file.path(path, "groups.csv")
	}
	if (!file.exists(f)) {
		stop("the groups file is missing")
	}
	utils::read.csv(f)	
}


get_variables <- function(path, group) {
	f <- file.path(path, "terms", paste0("variables_", group, ".csv"))	
	if (!isTRUE(file.exists(f))) {
		path <- system.file("terms", package="carobiner")
		f <- file.path(path, paste0("variables_", group, ".csv"))		
	}	
	if (file.exists(f)) {
		utils::read.csv(f)	
	} else {
		if (trimws(group) == "") {
			stop("the records file is missing")
		}
		NULL
	}
}


get_terms <- function(type, group, path) {
	if (type == "records") {
		trms <- get_variables(path, "all")
		grps <- get_groups(path)
		include <- grps$include[grps$name == group]
		if (include != "") {
			include <- trimws(unlist(strsplit(include, ";")))
			for (inc in include) {
				add <- get_variables(path, inc)
				trms <- rbind(trms, add)
			}
		}
		trms2 <- get_variables(path, group)
		if (!is.null(trms2)) {
			trms <- rbind(trms, trms2)
			tab <- table(trms[,1])
			if (any(tab > 1)) {
				print(paste("duplicated terms:", names(tab[tab>1])))
			}
		}
	} else if (type=="dataset") {
		trms <- get_variables(path, "dataset")
	} else {
		stop("invalid 'type' argument; should be 'records' or 'dataset'") 
	}
	#names(trms)[1] <- "name" # excel seems to mess this up
	trms
}



get_accepted_values <- function(name, path=NULL) {
	f <- file.path(path, "terms", paste0("values_", name, ".csv"))
	if (!isTRUE(file.exists(f))) {
		path <- system.file("terms", package="carobiner")
		f <- file.path(path, paste0("values_", name, ".csv"))
	}
	if (file.exists(f)) {
		utils::read.csv(f)	
	} else {
		NULL
	}
}

