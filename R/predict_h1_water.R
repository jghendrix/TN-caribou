#' @title Predict H1 distance to water
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
predict_h1_water <- function(DT, model) {
	N <- 100L

	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = 0,
		disturbed = 0,
		dist_to_water = seq(from = 0, to = 1500, length.out = N),
		indiv_step_id = NA
	), by = id]

	new_data[, h1_water := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x :=  seq(from = 0, to = 1500, length.out = N), by = id]
}
