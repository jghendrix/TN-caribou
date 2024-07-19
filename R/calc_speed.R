#' @title Calculate speed
#' @export
#' @author Julie W. Turner
calc_speed <- function(DT, covariate, seq) {
	if(covariate == "forest")
	DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																			`I(log(sl_)):forest`*seq +
																			`I(log(sl_)):open` +
																			`I(log(dist_to_new_burn + 1)):I(log(sl_))`*median
															)*(scale))),
									 x = list(list(seq))),
					 by=.(id)]
	if(covariate == "open fire")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest` +
																	`I(log(sl_)):open`*seq +
																	`I(log(dist_to_new_burn + 1)):I(log(sl_))`*median
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]
	if(covariate == "open road")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest` +
																	`I(log(sl_)):open`*seq +
																	`I(log(dist_to_tch + 1)):I(log(sl_))`*median
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
	move
}
