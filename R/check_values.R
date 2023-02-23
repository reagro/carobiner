
check_outliers_iqr <- function(x, field, verbose=FALSE) {
	x <- x[[field]]
	if (is.null(x)) return(invisible(NULL))
	q <- quantile(x, c(0.25, 0.75), na.rm=TRUE)
	qrn <- diff(q)
	mn <- q[1] - qrn
	mx <- q[2] + qrn
	i <- (x < mn | x > mx)
	if (verbose && any(i)) {
		message(paste("   iq outliers:", field))
	}
	invisible(which(i))
}

check_outliers_std <- function(x, field, verbose=FALSE) {
	x <- x[[field]]
	if (is.null(x)) return(invisible(NULL))
	x <- na.omit(x)
	m <- mean(x)
	sd3 <- sd(x) * 3
	mn <- m - sd3
	mx <- m + sd3
	i <- (x < mn | x > mx)
	if (verbose && any(i)) {
		message(paste("   sd outliers:", field))
	}
	invisible(which(i))
}

