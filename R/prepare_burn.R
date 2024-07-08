prepare_burn <- function(burn, crs, drop_z = TRUE) {

	#burn %<>% mutate(YEAR = ifelse(Burn_ID == 7, 1999, YEAR),
	#								 YEAR = ifelse(Burn_ID == 8, 1977, YEAR))
	# a couple fires were missing dates, swap these in courtesy of John

	burn_reproj <- st_transform(burn, crs)

	if (drop_z) {
		burn_reproj_wo_z <- st_zm(burn_reproj, drop = TRUE)
		return(burn_reproj_wo_z)
	} else {
		return(burn_reproj)
	}
}
