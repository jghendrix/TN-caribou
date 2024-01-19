#' @title Prepare model data
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
prepare_model <- function(DT) {
	DT[, indiv_step_id := paste(id, step_id_, sep = '_')]

	DT[lc_description == 'forest', lc_adj := 'forest']
	DT[lc_description %in% c('cultivated', 'developed'), lc_adj := 'disturbed']
	DT[lc_description %in% c('barren', 'herbaceous', 'shrubland', 'water'), lc_adj := 'open']
	DT[lc_description == 'wetlands', lc_adj := 'wetlands']

	DT[, forest := ifelse(lc_description == 'forest', 1, 0)]
	DT[, disturbed := ifelse(lc_adj == 'disturbed', 1, 0)]

	DT[, pt_lc := as.factor(pt_lc)]
	DT[, lc_adj := as.factor(lc_adj)]
	DT[, indiv_step_id := as.factor(indiv_step_id)]
	DT[, id := as.factor(id)]
}
