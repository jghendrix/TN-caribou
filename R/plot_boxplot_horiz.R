#' @title Plot boxplot of annual model estimates
#' @export
#' @author Jack G Hendrix
plot_box_horiz <- function(DT, theme, predictor) {

gbox <- 	ggplot(data = DT[term !='(Intercept)' & term != 'lc_adjother'],
				 # other landcover is all over the place, not possible to see the other effects
				 aes(term, estimate)) +
		geom_boxplot(aes(color = term)) +
		geom_jitter() +
		geom_hline(yintercept = 0, lty = 'dashed') +
		coord_flip() +
		plot_theme() +
		ggtitle(paste0(predictor, ' model'))

ggsave(
	paste0('graphics/', predictor, '_indiv_selection.png'),
	gbox,
	width = 10,
	height = 10,
	dpi = 320
)
}
