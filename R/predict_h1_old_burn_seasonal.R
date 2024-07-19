#' @title Predict H1 trans canada
#' @export
#' @author Jack G Hendrix
predict_h1_old_burn_seasonal <- function(DT, model, season_key) {
	N <- 100L

	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = 0,
		open = 0,
		dist_to_old_burn = seq(from = 0, to = 20000, length.out = N),
		dist_to_new_burn = median(dist_to_new_burn, na.rm = T),
		indiv_step_id = NA
	), by = id]

	new_data[, h1_old_burn_s := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x :=  seq(from = 0, to = 20000, length.out = N), by = id]

	new_data[, season := season_key$season]
}
