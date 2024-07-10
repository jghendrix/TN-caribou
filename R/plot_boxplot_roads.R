#' @title Plot horizontal boxplots for roads model
#' @export
#' @author Jack G Hendrix
plot_boxplot_roads <- function(DT, theme) {

	gbox <- ggplot(data = DT[term !='(Intercept)' & term != 'lc_adjother'],
									# other landcover is all over the place, not possible to see the other effects
									aes(term, estimate)) +
		geom_boxplot(aes(color = term)) +
		geom_jitter() +
		geom_hline(yintercept = 0, lty = 'dashed') +
		coord_flip() +
		plot_theme() +
		ggtitle(DT$season)

	ggsave(
		filename = paste0('graphics/roads_indiv_responses_', DT$season, '.png'),
		gbox,
		width = 10,
		height = 10,
		dpi = 320
	)
}
