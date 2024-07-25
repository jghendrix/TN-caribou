#' @title Plot distribution
#' @export
#' @author Jack G. Hendrix
plot_dist <- function(DT, theme) {

	ggplot(data = DT,
				 aes(x, spd, colour = id)) +
		geom_point(alpha = 0.5,
							 show.legend = F) +
		scale_colour_viridis(discrete = TRUE) +
		plot_theme()
}
