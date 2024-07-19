#' @title Plot boxplot
#' @export
#' @author Julie W. Turner
plot_box <- function(DT, theme) {

	ggplot(data = DT,
				 aes(as.logical(x), spd)) +
		geom_boxplot(aes(color = as.factor(x)),
								 show.legend = F) +
		geom_jitter(aes(color = as.factor(x)),
								show.legend = F) +
		scale_color_colorblind()  +
		scale_fill_colorblind() +
		plot_theme()
}
