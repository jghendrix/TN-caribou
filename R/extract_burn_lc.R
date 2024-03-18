## Extracting random points from each burn polygon
# Jack G. Hendrix, modified from ALR & JWT

extract_burn_lc <- function(DT, crs, lc, legend) {

		setDT(DT)

		pt <- c('X', 'Y')

		if (st_crs(lc) != crs) {
			lc <- raster::projectRaster(lc, crs = crs$wkt, method = 'ngb')
		}

		extract_pt(DT, lc, pt)
		DT[legend, lc_description := label, on = .(pt_lc = class)]

	}
