save_rss_plot <- function(plot_rss_forest,
													plot_rss_old_burn,
													plot_rss_new_burn)

{

ggsave("graphics/rss_forest.png",
			 plot = plot_rss_forest,
			 width = 2000,
			 height = 1500,
			 dpi = 320,
			 units="px")

ggsave("graphics/rss_old_burn.png",
			 plot = plot_rss_old_burn,
			 width = 2000,
			 height = 1500,
			 dpi = 320,
			 units="px")

ggsave("graphics/rss_new_burn.png",
			 plot = plot_rss_new_burn,
			 width = 2000,
			 height = 1500,
			 dpi = 320,
			 units="px")

}
