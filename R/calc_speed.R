#' @title Calculate speed
#' @export
#' @author Julie W. Turner
calc_speed <- function(DT, covariate, seq) {


	DT %<>% mutate(new_minor_sl = (.[[2]]) + (.[[5]]),
								 old_tch_sl = (.[[3]]) + (.[[6]]))

	## fire model estimates:

	if(covariate == "forest fire")
	DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																`I(log(sl_)):forest`*seq +
																`I(log(sl_)):open`*mean_open +
																new_minor_sl*log(med_new) +
																old_tch_sl*log(med_old)
	)*(scale))),
	x = list(list(seq))),
	by=.(id)]

	if(covariate == "open fire")
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

		## road model estimates:

		if(covariate == "forest road")
			DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																		`I(log(sl_)):forest`*seq +
																		`I(log(sl_)):open`*mean_open +
																		new_minor_sl*log(med_minor) +
																		old_tch_sl*log(med_tch)
			)*(scale))),
			x = list(list(seq))),
			by=.(id)]

		if(covariate == "open road")
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
	move
}
