#' @title Plot RSS
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
plot_rss <- function(rss, theme) {
ggplot(data = rss, aes(x, rss)) +
		geom_line(aes(group = id , alpha = .0001),
							linetype = 'twodash',
							show.legend = F) +
		geom_smooth(size = 1.5) +
		geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			size = .7
		) +
		scale_color_colorblind()  +
		scale_fill_colorblind() +
		plot_theme()
}
