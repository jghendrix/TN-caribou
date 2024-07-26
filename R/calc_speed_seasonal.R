#' @title Calculate speed seasonally
#' @export
#' @author Jack G Hendrix
calc_speed_seasonal <- function(DT, covariate, model, seq, season_key) {


	DT %<>% mutate(new_minor_sl = (.[[2]]) + (.[[5]]),
								 old_tch_sl = (.[[3]]) + (.[[6]]))

	# fire model estimates:

	if(covariate == "forest" & model == "fire")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*seq +
																	`I(log(sl_)):open`*mean_open +
																	new_minor_sl*log(med_new) +
																	old_tch_sl*log(med_old)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "open" & model == "fire")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*mean_forest +
																	`I(log(sl_)):open`*seq +
																	new_minor_sl*log(med_new) +
																	old_tch_sl*log(med_old)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]


	if(covariate == "dist_to_new_burn")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*mean_forest +
																	`I(log(sl_)):open`*mean_open +
																	new_minor_sl*log(1 + seq) +
																	old_tch_sl*log(med_old)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "dist_to_old_burn")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*mean_forest +
																	`I(log(sl_)):open`*mean_open +
																	new_minor_sl*log(med_new) +
																	old_tch_sl*log(1 + seq)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	# road model estimates:


	if(covariate == "forest" & model == "road")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*seq +
																	`I(log(sl_)):open`*mean_open +
																	new_minor_sl*log(med_minor) +
																	old_tch_sl*log(med_tch)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "open" & model == "road")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*mean_forest +
																	`I(log(sl_)):open`*seq +
																	new_minor_sl*log(med_minor) +
																	old_tch_sl*log(med_tch)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "dist_to_tch")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*mean_forest +
																	`I(log(sl_)):open`*mean_open +
																	new_minor_sl*log(med_minor) +
																	old_tch_sl*log(1 + seq)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "dist_to_minor")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*mean_forest +
																	`I(log(sl_)):open`*mean_open +
																	new_minor_sl*log(1 + seq) +
																	old_tch_sl*log(med_tch)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	move <- DT[, .(spd = unlist(spd), x = unlist(x)), by=.(id)]
	move[, season := season_key$season]
}
