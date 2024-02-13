#' @title Individual estimates
#' @export
#' @author Julie W. Turner
indiv_estimates <- function(model) {
	as.data.table(
		coef(model)$cond$id %>% rownames_to_column("id") %>%
			pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
			mutate(method = "ME")
	)
}
