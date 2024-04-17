new_burn_dist <- function(DT, crs, new_burn) {

	setDT(DT)

	start <- c('x1_', 'y1_')
	end <- c('x2_', 'y2_')


	extract_distance_to(DT[!is.na(x2_) & !is.na(y2_)],
											new_burn, end, crs)

	# If I run it as the same function with a different input, it just overwrites itself! ugh I know there's an easier solution but I can't think of it rn
}
