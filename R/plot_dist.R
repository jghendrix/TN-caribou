#' @title Plot distribution
#' @export
#' @author Jack G. Hendrix
plot_dist <- function(DT, theme) {

	ggplot(data = DT,
				 aes(x, spd)) +
		geom_point(size = 0.1) +
		scale_color_colorblind()  +
		scale_fill_colorblind() +
		plot_theme()
}
