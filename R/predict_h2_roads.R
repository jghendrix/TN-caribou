#' @title Predict H2 roads
#' @export
#' @author Jack G Hendrix
predict_h2_roads <- function(DT, model, season_key) {
	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = 0,
		open = 0,
		dist_to_tch = median(dist_to_tch, na.rm = TRUE),
		dist_to_minor = median(dist_to_minor, na.rm = TRUE),
		indiv_step_id = NA
	), by = id]

	new_data[, h2_roads := predict(model, .SD, type = 'link', re.form = NULL)]
	new_data[, season := season_key$season]
}
