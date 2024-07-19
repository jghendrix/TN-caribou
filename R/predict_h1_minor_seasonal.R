#' @title Predict H1 response to minor roads from road model
#' @export
#' @author Jack G Hendrix
predict_h1_minor_seasonal <- function(DT, model, season_key) {
	N <- 100L

	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = 0,
		open = 0,
		dist_to_minor = seq(from = 0, to = 20000, length.out = N),
		dist_to_tch = median(dist_to_tch, na.rm = T),
		indiv_step_id = NA
	), by = id]

	new_data[, h1_minor := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x :=  seq(from = 0, to = 20000, length.out = N), by = id]
	new_data[, season := season_key$season]
}
