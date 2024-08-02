## social response to linear features

model_road_social <- function(DT) {

	glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
			I(log(sl_)):forest +
			forest +
			I(log(sl_)):open +
			open +
			forest:in_group +
			open:in_group +
			I(log(dist_to_tch + 1)) +
			I(log(dist_to_tch + 1)):in_group +
			I(log(dist_to_tch + 1)):I(log(sl_)) +
			I(log(dist_to_minor + 1)) +
			I(log(dist_to_minor + 1)):in_group +
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
		data = subset(DT, season == "winter"),
		family = poisson(),
		map = list(theta = factor(c(NA, 1:9))),
		start = list(theta = c(log(1000), seq(0, 0, length.out = 9)))
	)
}
