calc_availability <- function(DT, col, type, split_by, return_summary = TRUE) {
	DT_avail <- DT[!(case_)]

	if (type == 'proportion') {
		DT[, n_split_by := .N, c(split_by)]
		out_col <- paste0('prop_', col)
		DT[, (out_col) := .N / n_split_by, by = c(col, split_by)]

		out_col <- c(out_col, col)

	} else if (type == 'mean') {
		out_col <- paste0('mean_', col)
		DT[, (out_col) := mean(x), by = c(split_by), env = list(x = col)]

	} else if (type == 'median') {
		out_col <- paste0('median_', col)
		DT[, (out_col) := median(x), by = c(split_by), env = list(x = col)]
	}

	if (return_summary) {
		unique(DT[, .SD, .SDcols = c(split_by, out_col)])
	}

}
