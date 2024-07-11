#' @title Individual estimates by season
#' @export
#' @author Jack G Hendrix
indiv_seasonal <- function(model, season_key) {
	as.data.table(
		coef(model)$cond$id %>% rownames_to_column("id") %>%
			pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
			mutate(method = "ME") %>%
			mutate(seasonality = season_key$season)
	)

}
