
all_github_scripts <- function() {
	req <- httr::GET("https://api.github.com/repos/reagro/carob/git/trees/master?recursive=1")
	httr::stop_for_status(req)
	ff <- sapply(httr::content(req)$tree, \(i) i$path)
	ff <- grep("scripts/", ff, value = TRUE)
	ff <- grep("\\.R$", ff, value = TRUE)
	ff <- gsub("scripts/", "", ff)
	ff <- ff[!grepl("_functions.R", ff)]
	fb <- gsub("\\.R$", "", basename(ff))
	fd <- dirname(ff)	
	pd <- grepl("_pending", fd)
	fd <- gsub("_pending/", "", fd)
	data.frame(dataset=fb, group=fd, pending=pd)
}


on_github <- function(uri=NULL) {
	d <- all_github_scripts()
	if (!is.null(uri)) {
		uri <- simple_uri(uri)
		i <- stats::na.omit(match(uri, d$dataset))
		if (length(i) == 0) return("not found")
		return(d[i,])
	} 
	d <- d[order(d$group), ]
	d <- d[d$group != ".", ]
	d$group[d$group == "_pending"] <- ""
	d
}

read_carob <- function(uri) {

# todo: vectorize over uri

	d <- all_github_scripts()
	uri <- carobiner:::simple_uri(uri)
	i <- stats::na.omit(match(uri, d$dataset))
	if (length(i) == 0) {
		stop("this URI is not in Carob")
	}
	if (d$pending[i]) {
		stop("this dataset is pending and this function won't run it")	
	}
	# first try to read from server 
	
	# else:
	u <- "https://raw.githubusercontent.com/reagro/carob/refs/heads/master/scripts/"
	u <- paste0(u, d$group[i], "/", d$dataset[i], ".R")
	
	r <- readLines(u)
	rm(carob_script)
	carob_script <- eval(parse(text=r)) 
	carob_script(NULL)
}

