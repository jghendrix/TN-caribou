dist_to_roads <- function(DT, crs, roads) {

	setDT(DT)
	DT[!is.na(x2_) & !is.na(y2_)]
	tch <- roads %>% dplyr::filter(name == "Trans Canada Highway")
	minor <- roads %>% dplyr::filter(!osm_id %in% c(tch$osm_id))

	start <- c('x1_', 'y1_')
	end <- c('x2_', 'y2_')

	extract_distance_to(DT, tch, end, crs)

	extract_distance_to(DT, minor, end, crs)



}
