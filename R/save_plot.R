save_plot <- function(plot_a, name_a, plot_b, name_b)

{

	ggsave(paste0("graphics/", name_a, ".png"),
				 plot = plot_a,
				 width = 2000,
				 height = 1500,
				 dpi = 320,
				 units="px")

	ggsave(paste0("graphics/", name_b, ".png"),
				 plot = plot_b,
				 width = 2000,
				 height = 1500,
				 dpi = 320,
				 units="px")

}
