#' @title Prepare speed seasonally
#' @export
#' @author Jack G Hendrix
prepare_speed_seasonal <- function(DT, summary, model, params, season_key) {

	if(model == "fire")
	sum <- DT[,.(mean = mean(dist_to_new_burn, na.rm = T),
											  median = median(dist_to_new_burn, na.rm = T),
											  max = max(dist_to_new_burn, na.rm = T)),
															by = .(id)]

	if(model == "road")
		sum <- DT[,.(mean = mean(dist_to_tch, na.rm = T),
								 median = median(dist_to_tch, na.rm = T),
								 max = max(dist_to_tch, na.rm = T)),
							by = .(id)]

	dat.wide <- dcast(summary[term %like% 'sl'], id~ term, value.var = 'estimate')

	dat.wide <- setDT(merge(dat.wide, setDT(params)[,.(id = as.character(id),shape, scale, kappa)], by = 'id', all.x = T))
	dat.wide <- setDT(merge(dat.wide, sum, by = 'id', all.x = T))
	dat.wide[, season := season_key$season]
	dat.wide
}
