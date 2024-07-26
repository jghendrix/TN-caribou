#' @title Plot distribution seasonally
#' @export
#' @author Jack G. Hendrix
plot_dist_seasonal <- function(DT, theme, predictor) {

	DT %<>% mutate(s_code = ifelse(season == "winter", 1, 2),
								 s_code = ifelse(season == "calving", 3, s_code),
								 s_code = ifelse(season == "autumn", 4, s_code)) %>%
		group_by(x, season) %>%
		mutate(pop = mean(spd)) %>%
		ungroup()


	for(i in 1:4) {

	g <- ggplot(data = subset(DT, s_code == i),
				 aes(x, spd, colour = id)) +
		geom_line(aes(colour = id),
							linetype = 5,
							show.legend = F) +
		geom_line(aes(x, pop), linewidth = 1.25, colour = "black") +
		scale_colour_viridis(discrete = TRUE) +
	  plot_theme() +
		labs(x = paste0('Distance to ', predictor, ' (m)'), y = 'Speed (m/2hr)') +
		ggtitle(subset(DT, s_code == i)$season)

	ggsave(
		filename = paste0('graphics/speed/speed_by_', predictor, "_", i, '.png'),
		g,
		width = 10,
		height = 10,
		dpi = 320
	)
}
}
