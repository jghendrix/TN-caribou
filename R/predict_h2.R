#' @title Predict H2
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
predict_h2 <- function(DT, model) {
	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = 0,
		open = 0,
		dist_to_new_burn = median(dist_to_new_burn, na.rm = TRUE),
		dist_to_old_burn = median(dist_to_old_burn, na.rm = TRUE),
		indiv_step_id = NA
	), by = id]

	new_data[, h2 := predict(model, .SD, type = 'link', re.form = NULL)]
}
