#' @title Plot RSS seasonally for forest cover
#' @export
#' @author Jack G Hendrix
plot_rss_seasonal_forest <- function(rss, theme, predictor) {

	rss %<>% mutate(s_code = ifelse(season == "winter", 1, 2),
								 s_code = ifelse(season == "calving", 3, s_code),
								 s_code = ifelse(season == "autumn", 4, s_code))

	for(i in 1:4) {
		data <- subset(rss, s_code == i)

ggplot(data, aes(x, rss)) +
		geom_line(aes(group = id , alpha = .0001),
							linetype = 'twodash',
							show.legend = F) +
	geom_smooth(size = 1.2,
							colour = "black",
							method = "lm",
							formula = y ~ log(x+1)) +
		geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			size = .7
		) +
		scale_color_colorblind()  +
		scale_fill_colorblind() +
		#scale_x_continuous(limits = c(0, 5000)) +
		plot_theme() +
		labs(x = 'Forest', y = 'logRSS') +
		ggtitle(paste0(predictor, ' model - RSS compared to 0 forest - ', data$season))

ggsave(
	filename = paste0('graphics/rss/rss_forest_', predictor, '_', i, '.png'),
	width = 10,
	height = 10,
	dpi = 320
)
}
}
