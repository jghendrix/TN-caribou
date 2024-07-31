#' @title Plot boxplots of speed from roads model
#' @export
#' @author Jack G Hendrix
plot_box_seasonal_speed <- function(DT, theme, predictor) {

	# this is the laziest workaround version of this but oh well

	DT %<>% mutate(s_code = ifelse(season == "winter", 1, 2),
								 s_code = ifelse(season == "calving", 3, s_code),
								 s_code = ifelse(season == "autumn", 4, s_code))

	for(i in 1:4) {

	gbox <- ggplot(subset(DT, s_code == i), aes(as.factor(x), spd)) +
		geom_boxplot(aes(color = as.factor(x)),
								 show.legend = F) +
		geom_jitter() +
		geom_hline(yintercept = 0, lty = 'dashed') +
		plot_theme() +
		labs(x = 'Closed (0) vs open (1)', y = 'Speed (m/2hr)') +
		ggtitle(subset(DT, s_code == i)$season)

	ggsave(
		filename = paste0('graphics/speed/', predictor, '_speed_in_open_', i, '.png'),
		gbox,
		width = 10,
		height = 10,
		dpi = 320
	)
	}

}
