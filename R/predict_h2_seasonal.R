#' @title Predict H2 roads
#' @export
#' @author Jack G Hendrix
predict_h2_seasonal <- function(DT, model, predictor, season_key) {

	if(predictor == "fire")
		new_data <-		DT[, .(
			sl_ = mean(sl_),
			forest = 0,
			open = 0,
			dist_to_new_burn = median(dist_to_new_burn, na.rm = TRUE),
			dist_to_old_burn = median(dist_to_old_burn, na.rm = TRUE),
			indiv_step_id = NA),
			by = id]

	if(predictor == "roads")
		new_data <-	DT[, .(
			sl_ = mean(sl_),
			forest = 0,
			open = 0,
			dist_to_tch = median(dist_to_tch, na.rm = TRUE),
			dist_to_minor = median(dist_to_minor, na.rm = TRUE),
			indiv_step_id = NA),
			by = id]


	new_data[, paste0("h2_s_", predictor) := predict(model, .SD, type = 'link', re.form = NULL)]
	new_data[, season := season_key$season]
}
