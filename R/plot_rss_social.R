#' @title Plot RSS
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
plot_rss_social <- function(rss, theme) {


	rss %<>% mutate(idsoc = paste0(id, "_", social))
	ggplot(data = rss, aes(x, rss, colour = social)) +
		geom_line(aes(group = idsoc, alpha = .00001),
							linetype = 'dashed',
							show.legend = F
		)+
		geom_smooth(size = 1.5) +
		geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			size = .7
		) +
		scale_color_viridis(discrete = "TRUE", option = "D", begin = 0.85, end = 0.2)  +
	#scale_fill_viridis() +
	#scale_x_continuous(limits = c(0, 5000)) +
	#facet_wrap(. ~ social) +
		plot_theme()
}
