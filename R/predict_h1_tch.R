#' @title Predict H1 forest
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
predict_h1_forest <- function(DT, model) {
	N <- 100L

	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = 0,
		open = 0,
		dist_to_tch = seq(from = 0, to = 20000, length.out = N),
		dist_to_minor = median(dist_to_minor, na.rm = T),
		indiv_step_id = NA
	), by = id]

	new_data[, h1_forest := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x :=  seq(from = 0, to = 20000, length.out = N), by = id]
}
