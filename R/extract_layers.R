#' @title Extract layers
#' @export
#' @author Alec L. Robitaille, Julie W. Turner
extract_layers <- function(DT, crs, lc, legend, burn) {
	# Eventually also want to have 'burn' in the function call, so we can calculate distance to burns/whether points are inside burn polygons, but leaving that alone for the time being and just trying to extract landcover

	setDT(DT)

	start <- c('x1_', 'y1_')
	end <- c('x2_', 'y2_')

	extract_pt(DT, lc, end)
	DT[legend, lc_description := label, on = .(pt_lc = class)]

	extract_distance_to(DT[!is.na(x2_) & !is.na(y2_)],
											burn, end, crs)

	return(DT)
}
