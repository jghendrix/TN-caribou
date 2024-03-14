#' @title Extract layers
#' @export
#' @author Alec L. Robitaille, Julie W. Turner
extract_layers <- function(DT, crs, lc, legend, burn) {

	setDT(DT)
	DT[!is.na(x2_)]
# there were some NAs in the end coords so the dist_to wasn't working

	start <- c('x1_', 'y1_')
	end <- c('x2_', 'y2_')

	if (st_crs(lc) != crs) {
		lc <- raster::projectRaster(lc, crs = crs$wkt, method = 'ngb')
	}

	extract_pt(DT, lc, end)
	DT[legend, lc_description := label, on = .(pt_lc = class)]

	extract_distance_to(DT, burn, end, crs)
	# distance to ecotone of burn, but points 100m inside the burn and 100m outside the burn will have the same values...
	# can we extract if each point is inside or outside the burn polygons, and then if that binomial == "inside", multiple distance by -1
}
