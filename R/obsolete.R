
.get_more_data <- function(url, dataset_id, path, group) {
	f <- basename(url)
	path <- file.path(path, "data/raw", group, dataset_id, f)
	for (i in seq_along(f)) {
		if (!file.exists(path[i])) {
			utils::download.file(path[i], f[i], mode="wb")
		}
	}
	f
}
