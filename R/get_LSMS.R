
get_LSMS <- function(uri, path, pwds, cache=TRUE) {

	fok <- file.path(path, "ok.txt")
	if (cache && file.exists(fok)) {
		return(list.files(path, recursive=TRUE))
	}

	if (!isTRUE(nchar(pwds$LSMS_email) > 4)) {
		stop("required user name 'LSMS_email' not found; see 'passwords_template.R'")
	}
	if (!isTRUE(nchar(pwds$LSMS_password) > 1)) {
		stop("required password 'LSMS_password' not found; see 'passwords_template.R'")
	}

	dir.create(path, FALSE, FALSE)
	lsms_login = "https://microdata.worldbank.org/index.php/auth/login"
	#html <- httr::content(GET(lsms_login), "text")
	p <- httr::POST(lsms_login, body = list('email' = pwds$LSMS_email, 'password' = pwds$LSMS_password))
	uhtml <- paste0("https://doi.org/", gsub("doi:", "", uri))
	z <- httr::GET(uhtml)
	zurl <- gsub("0;url=", "", z$headers$refresh)
	url <- paste0(zurl, "/get-microdata")
	r <- httr::content(httr::GET(url), as="text")
	x <- unlist(strsplit(gsub("\t|\r", "", r), "\n"))
	i <- grep("Data in CSV", x)
	if (length(i) == 0) {
		i <- grep("Data in ASCII", x)
		if (length(i) == 0) {
			stop("no data here (or a barrier?)")
		}
	}
	x <- x[i:(i+10)]
	i <- grep("href=", x)
	x <- x[i:(i+1)]
	x <- gsub("href=|title=", "", x)
	durl <- paste0(trimws(gsub('\"', "", x)), collapse="/")
	g <- httr::GET(durl)
	bin <- httr::content(g, "raw")
	fout <- file.path(path, basename(durl))
	writeBin(bin, fout)
	utils::unzip(fout, exdir = path)
	writeLines(c(utils::timestamp(quiet=TRUE), uri), fok)

	url <- paste0(zurl, "/study-description")
	r <- httr::content(httr::GET(url), as="text")
	writeLines(r, file.path(path, "study-description.html"))

	url <- paste0(zurl, "/related-materials")
	r <- httr::content(httr::GET(url), as="text")
	x <- trimws(unlist(strsplit(gsub("\t|\r", "", r), "\n")))
	x <- x[grep(paste0('href=\"', zurl, "/download"), x)]
	p <- grep("pdf", x, value=TRUE)
	s <- sapply(strsplit(p, 'title=\"'), \(i) i[[2]])
	z <- strsplit(s, '\" href=\"')	
	pdf <- sapply(z, \(i) i[[1]])
	updf <- sapply(z, \(i) strsplit(i[[2]], '\">')[[1]][1])	
	fpdf <- gsub(" ", "%20", paste0(updf, "/", pdf))
	dp <- file.path(path, "docs")
	dir.create(dp, FALSE, FALSE)
	for (f in fpdf) {
		download.file(f, file.path(dp, basename(f)), mode="wb", quiet=TRUE)
	}
	
	return(list.files(path, recursive=TRUE))	
}



LSMS_metadata <- function(uri, path, major, minor) {

	suri <- yuri::simpleURI(uri)
	mf <- readLines(file.path(path, "data/raw/LSMS", suri, "study-description.html"))
	tit <- gsub("<title>|</title>", "", grep("<title>", mf, value=TRUE))
	dat <- trimws(mf[grep("Date of Metadata Production", mf)+2])
	dat <- gsub("<p>|</p>|<span>|</span>", "", dat)
	des <- trimws(grep("Abstract", mf, value=TRUE)[1])
	des <- strsplit(des, "Geographic coverage")[[1]][1]
	des <- gsub('\"|\\\\r|\\\\n', "", des)
	des <- gsub("description: Abstract---------------------------", "", des)

	data.frame(
		dataset_id = suri,
		group="LSMS",
		uri=uri,
		license=NA,
		title= tit,
		authors=NA,
		data_published=dat,
		description=des,
		data_citation=NA,
		data_institute = "World Bank",
		project = "LSMS",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none"
	)
}

