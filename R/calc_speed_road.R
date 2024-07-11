#' @title Calculate speed w/ roads
#' @export
#' @author Jack G Hendrix
calc_speed_road <- function(DT, covariate, seq, season_key) {
	if(covariate == "forest")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*seq +
																	`I(log(sl_)):open` +
																	`I(log(dist_to_tch + 1)):I(log(sl_))`*median.burn
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]
	if(covariate == "open")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest` +
																	`I(log(sl_)):open`*seq +
																	`I(log(dist_to_tch + 1)):I(log(sl_))`*median.burn
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
