#' @title Prepare model data
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
prepare_model <- function(DT) {
	DT[, indiv_step_id := paste(Animal_ID, step_id_, sep = '_')]

	# Grouping together landcover categories into more meaningful levels
	# Herbs and Rock/Rubble are extremely rare (0.005%), group together w/ exposed

	DT[lc_description %in% c('Coniferous', 'Mixedwood'), lc_adj := 'forest']
	DT[lc_description %in% c('Shrubs'), lc_adj := 'scrub']
	DT[lc_description %in% c('Exposed/Barren Land', 'Rock/Rubble', 'Herbs'), lc_adj := 'open']
	DT[lc_description == 'Wetland', lc_adj := 'wetland']
	DT[lc_description == 'Water', lc_adj := 'water']
	# any points outside of the lc raster, aka the long excursion to the southwest, let's try ignoring those for now
	DT[!is.na(lc_description)]

	DT[, forest := ifelse(lc_adj == 'forest', 1, 0)]
	DT[, open := ifelse(lc_adj %in% c('open', 'wetland', 'scrub'), 1, 0)]

	DT[, pt_lc := as.factor(pt_lc)]
	DT[, lc_adj := as.factor(lc_adj)]
	DT[, indiv_step_id := as.factor(indiv_step_id)]
	DT[, id := as.factor(Animal_ID)]

	# one step length is exactly 0, errors out when running the model
	# next shortest step is 1.072510e-06, substitute that?
	DT[, sl_ := ifelse(sl_ == 0, 1.072510e-06, sl_)]


}
