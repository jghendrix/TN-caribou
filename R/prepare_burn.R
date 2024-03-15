prepare_burn <- function(burn, crs, drop_z = TRUE) {
	burn_reproj <- st_transform(burn, crs)

	if (drop_z) {
		burn_reproj_wo_z <- st_zm(burn_reproj, drop = TRUE)
		return(burn_reproj_wo_z)
	} else {
		return(burn_reproj)
	}
}
