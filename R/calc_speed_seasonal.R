#' @title Calculate speed seasonally
#' @export
#' @author Jack G Hendrix
calc_speed_seasonal <- function(DT, covariate, model, seq, season_key) {

	if(covariate == "forest" & model == "road")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*seq +
																	`I(log(sl_)):open` +
																	`I(log(dist_to_tch + 1)):I(log(sl_))`*median
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "open" & model == "road")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest` +
																	`I(log(sl_)):open`*seq +
																	`I(log(dist_to_tch + 1)):I(log(sl_))`*median
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

		if(covariate == "forest" & model == "fire")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*seq +
																	`I(log(sl_)):open` +
																	`I(log(dist_to_new_burn + 1)):I(log(sl_))`*median
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "open" & model == "fire")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest` +
																	`I(log(sl_)):open`*seq +
																	`I(log(dist_to_new_burn + 1)):I(log(sl_))`*median
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]


	if(covariate == "dist_to_new_burn")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest` +
																	`I(log(sl_)):open`+
																	`I(log(dist_to_new_burn + 1)):I(log(sl_))`*seq
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "dist_to_old_burn")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest` +
																	`I(log(sl_)):open`+
																	`I(log(dist_to_old_burn + 1)):I(log(sl_))`*seq
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "dist_to_tch")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest` +
																	`I(log(sl_)):open`+
																	`I(log(dist_to_tch + 1)):I(log(sl_))`*seq
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	move <- DT[, .(spd = unlist(spd), x = unlist(x)), by=.(id)]
	move[, season := season_key$season]
}
