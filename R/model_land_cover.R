#' @title Model land cover
#' @export
#' @author Julie W. Turner
model_land_cover <- function(DT) {
	glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
			I(log(sl_)):lc_adj +
			lc_adj +
			#I(log(dist_to_water + 1)) +
			#I(log(dist_to_water + 1)):I(log(sl_)) +
			(1 | indiv_step_id) +
			(0 + I(log(sl_)) | id) +
			(0 + I(log(sl_)):lc_adj | id) +
			(0 + lc_adj | id) #+
			#(0 + I(log(dist_to_water + 1)) | id) +
			#(0 + I(log(dist_to_water + 1)):I(log(sl_)) | id)
			,
		data = DT,
		family = poisson(),
		map = list(theta = factor(c(NA, 1:31))),
		start = list(theta = c(log(1000), seq(0, 0, length.out = 31)))
	)
}
