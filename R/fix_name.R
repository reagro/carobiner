
.fix_adm <- function(x, country="", level="") {
	# country and level could be used in the future to compare with GADM spelling
	x <- tools::toTitleCase(tolower(trimws(x)))
	x[x==""] <- NA
	x
}

.capitalize_words <- function(x, skip="") {
	x <- paste("", tolower(x), "")
	
	skip <- c("and", "of", "the", tolower(skip))
	skip <- trimws(skip)
	skip <- skip[skip != ""]
	for (w in skip) {
		x <- gsub(paste0(" ", w, " "), paste0(" #", w, " "), x)
	}
	x <- gsub("-", "- ", x)
	x <- gsub(" d'", " #d' ", x)
	x <- gsub("\\.", ".# ", x)
	
    cap <- function(x) paste(toupper(substring(x, 1, 1)),
                  { x <- substring(x, 2); x}, sep = "", collapse = " " )
    x <- sapply(strsplit(x, split = " "), cap, USE.NAMES = FALSE)
	x <- paste("", x, "")

	for (w in skip) {
		x <- gsub(paste0(" #", w, " "), paste0(" ", w, " "), x)
	}

	x <- gsub("- ", "-", x)
	x <- gsub(" #d' ", " d'", x)
	x <- gsub("\\.# ", "\\.", x)
	trimws(x)
}



fix_name <- function(x, case="", skip="") {
	x <- trimws(x)
	x <- gsub("   ", " ", x)
	x <- gsub("  ", " ", x)
	x[x==""] <- NA
	if (case == "first") {
		s <- strsplit(x, "")
		x <- sapply(s, \(i) if (is.na(i[1])) NA  else paste0(c(toupper(i[1]), i[-1]), collapse=""))
	} else if (case=="lower") {
		x <- tolower(x)
	} else if (case=="title") {
		x <- .capitalize_words(x)
		#x <- tools::toTitleCase(tolower(x))
	}
	x
}

