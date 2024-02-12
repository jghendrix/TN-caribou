#' @title Model forest
#' @export
#' @author Julie W. Turner
model_forest_bin <- function(DT) {
#	DT <- DT %>% dplyr::mutate(forest = as.factor(forest), disturbed = as.factor(disturbed))
	glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
			I(log(sl_)):forest +
			forest +
			I(log(sl_)):disturbed +
			disturbed +
			I(log(dist_to_water + 1)) +
			I(log(dist_to_water + 1)):I(log(sl_)) +
			(1 | indiv_step_id) +
			(0 + I(log(sl_)) | id) +
			(0 + I(log(sl_)):disturbed | id) +
			(0 + I(log(sl_)):forest | id) +
			(0 + forest | id) +
			(0 + disturbed | id) +
			(0 + I(log(dist_to_water + 1)) | id) +
			(0 + I(log(dist_to_water + 1)):I(log(sl_)) | id),
		data = DT,
		family = poisson(),
		map = list(theta = factor(c(NA, 1:7))),
		start = list(theta = c(log(1000), seq(0, 0, length.out = 7)))
	)
}
