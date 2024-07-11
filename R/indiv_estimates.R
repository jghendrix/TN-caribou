#' @title Individual estimates
#' @export
#' @author Jack G Hendrix
indiv_estimates <- function(model) {
	as.data.table(
		coef(model)$cond$id %>% rownames_to_column("id") %>%
			pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
			mutate(method = "ME")
	)

}
