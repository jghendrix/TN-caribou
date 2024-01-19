#' @title Predict H1 forest
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
predict_h1_forest <- function(DT, model) {
	N <- 100L

	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = seq(from = 0, to = 1, length.out = N),
		disturbed = 0,
		dist_to_water= median(dist_to_water, na.rm = TRUE),
		indiv_step_id = NA
	), by = id]

	new_data[, h1_forest := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x := seq(from = 0, to = 1, length.out = N), by = id]
}
