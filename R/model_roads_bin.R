## response to linear features by season

model_roads_bin <- function(DT) {

	glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
			I(log(sl_)):forest +
			forest +
			I(log(sl_)):open +
			open +
			I(log(dist_to_tch + 1)) +
			I(log(dist_to_tch + 1)):I(log(sl_)) +
			I(log(dist_to_minor + 1)) +
			I(log(dist_to_minor + 1)):I(log(sl_)) +
			(1 | indiv_step_id) +
			(0 + I(log(sl_)) | id) +
			(0 + I(log(sl_)):open | id) +
			(0 + I(log(sl_)):forest | id) +
			(0 + forest | id) +
			(0 + open | id) +
			(0 + I(log(dist_to_tch + 1)) | id) +
			(0 + I(log(dist_to_tch + 1)):I(log(sl_)) | id) +
			(0 + I(log(dist_to_minor + 1)) | id) +
			(0 + I(log(dist_to_minor + 1)):I(log(sl_)) | id)
		,
		data = DT,
		family = poisson(),
		map = list(theta = factor(c(NA, 1:9))),
		start = list(theta = c(log(1000), seq(0, 0, length.out = 9)))
	)
}
