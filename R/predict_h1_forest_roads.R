#' @title Predict H1 forest for roads model
#' @export
#' @author Jack G Hendrix
predict_h1_forest_roads <- function(DT, model, season_key) {
	N <- 100L

	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = seq(from = 0, to = 1, length.out = N),
		open = 0,
		dist_to_tch = median(dist_to_tch, na.rm = TRUE),
		dist_to_minor = median(dist_to_minor, na.rm = TRUE),
		indiv_step_id = NA
	), by = id]

	new_data[, h1_forest_roads := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x := seq(from = 0, to = 1, length.out = N), by = id]
	new_data[, season := season_key$season]
}
