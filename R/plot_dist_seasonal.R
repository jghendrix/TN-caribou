#' @title Plot distribution seasonally
#' @export
#' @author Jack G. Hendrix
plot_dist_seasonal <- function(DT, theme, predictor) {

	DT %<>% mutate(s_code = ifelse(season == "winter", 1, 2),
								 s_code = ifelse(season == "calving", 3, s_code),
								 s_code = ifelse(season == "autumn", 4, s_code))

	for(i in 1:4) {

	g <- ggplot(data = subset(DT, s_code == i),
				 aes(x, spd)) +
		geom_point() +
		scale_color_colorblind()  +
		scale_fill_colorblind() +
	  plot_theme() +
		labs(x = 'Distance to TCH (m)', y = 'Speed (m/2hr)') +
		ggtitle(subset(DT, s_code == i)$season)

	ggsave(
		filename = paste0('graphics/speed_by_', predictor, "_", i, '.png'),
		g,
		width = 10,
		height = 10,
		dpi = 320
	)
}
}
