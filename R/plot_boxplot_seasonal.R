#' @title Plot seasonal boxplots for model estimates
#' @export
#' @author Jack G Hendrix
plot_boxplot_seasonal <- function(DT, theme, predictor) {

	gbox <- ggplot(data = DT[term !='(Intercept)' & term != 'lc_adjother'],
									aes(term, estimate)) +
		geom_boxplot(aes(color = term),
								  show.legend = FALSE) +
		geom_jitter() +
		geom_hline(yintercept = 0, lty = 'dashed') +
		coord_flip() +
		plot_theme() +
		ggtitle(paste0(DT$season," ", predictor, " model"))

	ggsave(
		filename = paste0('graphics/', predictor, '_indiv_responses_', DT$season, '.png'),
		gbox,
		width = 10,
		height = 10,
		dpi = 320
	)
}
