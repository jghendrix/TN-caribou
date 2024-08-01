#' @title Plot RSS
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
plot_rss_social <- function(rss, theme) {
ggplot(data = rss, aes(x, rss, group = social)) +
		geom_line(aes(group = id, colour = id, alpha = .0001),
							linetype = 'twodash', show.legend = F
							)+
		geom_smooth(colour = "black", size = 1.5) +
		geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			size = .7
		) +
		scale_color_viridis(discrete = "TRUE")  +
		scale_fill_viridis() +
		#scale_x_continuous(limits = c(0, 5000)) +
		facet_wrap(. ~ social) +
		plot_theme()
}
