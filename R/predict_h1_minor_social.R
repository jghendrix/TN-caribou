#' @title Predict H1 response to minor roads from road model
#' @export
#' @author Jack G Hendrix
predict_h1_minor_social <- function(DT, model, social) {
	N <- 100L

if(social == "alone")
	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = 0,
		open = 0,
		in_group = "alone",
		dist_to_tch = seq(from = 0, to = 20000, length.out = N),
		dist_to_minor = median(dist_to_minor, na.rm = T),
		indiv_step_id = NA
	), by = id]

if(social == "dyad")
	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = 0,
		open = 0,
		in_group = "dyad",
		dist_to_minor = seq(from = 0, to = 20000, length.out = N),
		dist_to_tch = median(dist_to_tch, na.rm = T),
		indiv_step_id = NA
	), by = id]

	new_data[, h1_minor := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x :=  seq(from = 0, to = 20000, length.out = N), by = id]
}
