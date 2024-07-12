#' @title Predict H1 forest for each season
#' @export
#' @author Jack G Hendrix

predict_h1_forest_seasonal <- function(DT, model, predictor, season_key)
	{

	if(predictor == "fire")
	new_data <-		DT[, .(
			sl_ = mean(sl_),
			forest = seq(from = 0, to = 1, length.out = 100L),
			open = 0,
			dist_to_new_burn = median(dist_to_new_burn, na.rm = TRUE),
			dist_to_old_burn = median(dist_to_old_burn, na.rm = TRUE),
			indiv_step_id = NA),
			by = id]

	if(predictor == "road")
		new_data <-	DT[, .(
		sl_ = mean(sl_),
		forest = seq(from = 0, to = 1, length.out = 100L),
		open = 0,
		dist_to_tch = median(dist_to_tch, na.rm = TRUE),
		dist_to_minor = median(dist_to_minor, na.rm = TRUE),
		indiv_step_id = NA),
		by = id]

	new_data[, paste0('h1_forest_s_', predictor) := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x := seq(from = 0, to = 1, length.out = 100L), by = id]
	new_data[, season := season_key$season]
}
