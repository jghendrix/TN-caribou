old_burn_dist <- function(DT, crs, old_burn) {

	setDT(DT)

	start <- c('x1_', 'y1_')
	end <- c('x2_', 'y2_')


	extract_distance_to(DT[!is.na(x2_) & !is.na(y2_)],
											old_burn, end, crs)

	# It will only give me one of the two distances, if I include both in a single function, have to run it as two separate targets? Fine.
}
