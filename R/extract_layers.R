#' @title Extract layers
#' @export
#' @author Alec L. Robitaille, Julie W. Turner
extract_layers <- function(DT, crs, lc, legend, burn) {

	setDT(DT)
	DT <- DT[!is.na(x2_)]
	# there were some NAs in the end coords so the dist_to wasn't working

	start <- c('x1_', 'y1_')
	end <- c('x2_', 'y2_')

	if (st_crs(lc) != crs) {
		lc <- raster::projectRaster(lc, crs = crs$wkt, method = 'ngb')
	}

	extract_pt(DT, lc, end)
	DT[legend, lc_description := label, on = .(pt_lc = class)]

	# landcover works fine, i can get habitat at each point, but trying to get distance-to-burn is causing errors

	extract_distance_to(DT, burn, end, crs)

	# Error message: number of columns of matrices must match
	# tried shrinking the burns down to just two columns, like the fisher water example... didn't really think that would help, and indeed it did not
	# maybe the multipolygons for each fire is complicating things? The water has polygons > double for each feature, but the fires are multipolygons > polygons > double? I assume this worked with the original data so my troubleshooting just consists of trying to get the burns data to look the same as the water
	# potential very janky solution: split up the burns so that each polygon of the burn is a separate row in the overall sf, with identical values for all the other columns?
}
