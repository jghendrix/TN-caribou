#' @title Predict H1 distance to burn
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
predict_h1_new_burn_social <- function(DT, model, social) {
	N <- 100L

if(social == "alone")
	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = 0,
		open = 0,
		in_group = "alone",
		dist_to_new_burn = seq(from = 0, to = 27000, length.out = N),
		dist_to_old_burn = median(dist_to_old_burn, na.rm = TRUE),
		indiv_step_id = NA
	), by = id]

if(social == "dyad")
	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = 0,
		open = 0,
		in_group = "dyad",
		dist_to_new_burn = seq(from = 0, to = 27000, length.out = N),
		dist_to_old_burn = median(dist_to_old_burn, na.rm = TRUE),
		indiv_step_id = NA
	), by = id]

	new_data[, h1_new_burn := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x :=  seq(from = 0, to = 27000, length.out = N), by = id]
}
