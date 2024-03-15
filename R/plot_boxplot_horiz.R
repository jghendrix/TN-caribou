#' @title Plot boxplot
#' @export
#' @author Julie W. Turner
plot_box_horiz <- function(DT, theme) {

gbox <- 	ggplot(data = DT[term !='(Intercept)' & term != 'lc_adjother'],
				 # other landcover is all over the place, not possible to see the other effects
				 aes(term, estimate)) +
		geom_boxplot(aes(color = term)) +
		geom_jitter() +
		geom_hline(yintercept = 0, lty = 'dashed') +
		coord_flip() +
		plot_theme()

ggsave(
	'graphics/indiv_selection.png',
	gbox,
	width = 10,
	height = 10,
	dpi = 320
)
}
