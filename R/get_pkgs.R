

get_packages <- function(path, group="") {
	libfun1 <- function(x) {
		d <- readLines(x, warn=FALSE)
		i <- grep('^library\\(', d)
		pkgs <- d[unique(i)]
		pkgs <- pkgs[!grepl("#", pkgs)]
		pkgs <- pkgs[nchar(pkgs) < 100]
		pkgs <- gsub("library\\(", "", pkgs)
		pkgs <- trimws(gsub(")", "", pkgs))
		pkgs <- sort(unique(pkgs))
		pkgs <- gsub('\"', "", pkgs)
	}
	
	libfun2 <- function(x) {
		d <- readLines(x, warn=FALSE)
		i <- grep('::', d)
		if (length(d) ==0) {
			return(NULL)
		}
		d <- d[unique(i)]
		d <- d[!grepl("#", d)]
		if (length(d) ==0) {
			return(NULL)
		}

		d <- strsplit(d, "<-")
		d <- sapply(d, function(e) ifelse(length(e)>1, e[2], e[1]))
		d <- d[!is.na(d)]
		if (length(d) ==0) {
			return(NULL)
		}
		d <- trimws(d)

		#d <- strsplit(d, "=")
		#d <- sapply(d, function(e) ifelse(length(e)>1, e[2], e[1]))
		#d <- d[!is.na(d)]
		if (length(i) ==0) {
			return(NULL)
		}

		d <- strsplit(d, "::")
		d <- sapply(d, function(e) e[1])
		d <- unique(trimws(d[!is.na(d)]))
		d[!(grepl(",", d) | grepl("\\(", d))]
	}

	ff <- list.files(file.path(path, "scripts", group), pattern='\\.R$', full.names=TRUE, ignore.case=TRUE)
	pkgs1 <- unique(unlist(sapply(ff, libfun1)))
	pkgs2 <- unique(unlist(sapply(ff, libfun2)))
	
	pkgs <- unique(c(pkgs1, pkgs2, "terra", "geodata", "data.table", "writexl", "readxl", "jsonlite", "reshape2", "stringr", "revtools", "httr"))	
	ipkgs <- rownames(utils::installed.packages())
	for (pk in pkgs) {
	  if (!(pk %in% ipkgs)) {
		print(paste("installing", pk))
		utils::install.packages(pkgs=pk, repos="https://cloud.r-project.org/", quiet=TRUE)
	  }
	}
}
