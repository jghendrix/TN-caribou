#' @title Plot distribution
#' @export
#' @author Jack G. Hendrix
plot_dist <- function(DT, theme) {

	DT %<>% group_by(x) %>% mutate(pop = mean(spd)) %>% ungroup()

	ggplot(data = DT,
				 aes(x, spd)) +
		geom_line(aes(colour = id),
							linetype = 5,
							show.legend = F) +
		geom_line(aes(x, pop), linewidth = 1.25, colour = "black") +
		scale_colour_viridis(discrete = TRUE) +
		plot_theme()
}
