#' @title Plotting current lc of fires by age since burn
#' @export
#' @author Jack G. Hendrix
burn_lc_status <- function(DT) {

	DT[, forest := ifelse(lc_description %in% c("Coniferous", "Mixedwood"), 1, 0)]
	DT[, conifer := ifelse(lc_description %in% c("Coniferous"), 1, 0)]
	DT[, shrub := ifelse(lc_description %in% c('Shrubs', 'Herbs'), 1, 0)]

	# Shrubs isn't that abundant, Herbs is only a single point
	# but barrens + water + wetland is 33% of points, so only looking at those two helps avoid the inherent correlation between open vs. closed

	DT %<>% group_by(id, year) %>%
		summarise(all_forest = mean(forest),
							shrub = mean(shrub),
							conifer = mean(conifer),
							points = n()) %>%
		tidyr::pivot_longer(cols = 3:5, names_to = "landcover", values_to = "prop")

	ggplot(subset(DT, landcover != "conifer"), aes(x = year, y = prop, colour = landcover, size = points)) +
		geom_point() +
		geom_smooth() +
		ylab("Proportion landcover") +
		scale_colour_viridis(discrete = TRUE, option = "A", begin = 0.8, end = 0.15) +
	#	ylim(c(0,1)) +
		plot_theme()

	ggsave('graphics/post-burn-lc_all-forest.png',
				 width = 12,
				 height = 8,
				 dpi = 320)

	# It doesn't actually look like more recent burns are any less likely to be forested in 2020

	# Maybe mixedwood shouldn't be included?

	ggplot(DT, aes(x = year, y = prop, colour = landcover, size = points)) +
		geom_point() +
		geom_smooth() +
		ylab("Proportion landcover") +
		scale_colour_viridis(discrete = TRUE, option = "A", begin = 0.8, end = 0.15) +
		#	ylim(c(0,1)) +
		plot_theme()
	ggsave('graphics/post-burn-lc-w_mixedwood.png',
				 width = 12,
				 height = 8,
				 dpi = 320)

}
