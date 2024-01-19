#' @title Plot boxplot
#' @export
#' @author Julie W. Turner
plot_box<- function(DT, theme) {

	ggplot(data = DT,
				 aes(as.factor(x), spd)) +
		geom_boxplot(aes(color = as.factor(x))) +
		geom_jitter(aes(color = as.factor(x))) +
		scale_color_colorblind()  +
		scale_fill_colorblind() +
		plot_theme()
}
