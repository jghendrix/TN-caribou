#' @title Prepare speed calcs
#' @export
#' @author Julie W. Turner
prep_speed <- function(DT, summary, params) {
	sum.distwater <- DT[,.(mean.water = mean(dist_to_water, na.rm = T),
																 median.water = median(dist_to_water, na.rm = T),
																 max.water = max(dist_to_water, na.rm = T)),
															by = .(id)]

	dat.wide <- dcast(summary[term %like% 'sl'], id~ term, value.var = 'estimate')

	dat.wide <- setDT(merge(dat.wide, setDT(params)[,.(id = as.character(id),shape, scale, kappa)], by = 'id', all.x = T))
	dat.wide <- setDT(merge(dat.wide, sum.distwater, by = 'id', all.x = T))
	dat.wide
}
