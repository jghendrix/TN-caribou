#' @title Prepare speed calcs
#' @export
#' @author Julie W. Turner
prepare_speed <- function(DT, summary, params) {
	sum.distburn <- DT[,.(mean.burn = mean(dist_to_burn, na.rm = T),
																 median.burn = median(dist_to_burn, na.rm = T),
																 max.burn = max(dist_to_burn, na.rm = T)),
															by = .(id)]

	dat.wide <- dcast(summary[term %like% 'sl'], id~ term, value.var = 'estimate')

	dat.wide <- setDT(merge(dat.wide, setDT(params)[,.(id = as.character(id),shape, scale, kappa)], by = 'id', all.x = T))
	dat.wide <- setDT(merge(dat.wide, sum.distburn, by = 'id', all.x = T))
	dat.wide
}
