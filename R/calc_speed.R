#' @title Calculate speed
#' @export
#' @author Julie W. Turner
calc_speed <- function(DT, covariate, seq) {
	if(covariate == "forest")
	DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																			`I(log(sl_)):forest`*seq +
																			`I(log(sl_)):disturbed` +
																			`I(log(dist_to_water + 1)):I(log(sl_))`*median.water
															)*(scale))),
									 x = list(list(seq))),
					 by=.(id)]
	if(covariate == "disturbed")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest` +
																	`I(log(sl_)):disturbed`*seq +
																	`I(log(dist_to_water + 1)):I(log(sl_))`*median.water
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]
	if(covariate == "dist_to_water")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest` +
																	`I(log(sl_)):disturbed`+
																	`I(log(dist_to_water + 1)):I(log(sl_))`*seq
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	move <- DT[, .(spd = unlist(spd), x = unlist(x)), by=.(id)]
	move
}
