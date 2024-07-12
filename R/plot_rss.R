#' @title Plot RSS
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
plot_rss <- function(rss, theme) {
ggplot(data = rss, aes(x, rss)) +
		geom_line(aes(group = id, colour = id, alpha = .0001),
							linetype = 'twodash',
							show.legend = F) +
		geom_smooth(size = 1.5) +
		geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			size = .7
		) +
		scale_color_viridis(discrete = "TRUE")  +
		scale_fill_viridis() +
		#scale_x_continuous(limits = c(0, 5000)) +
		plot_theme()
}
