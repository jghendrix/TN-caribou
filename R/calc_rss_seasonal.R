#' @title Calculate RSS for each season
#' @export
#' @author Jack G Hendrix
calc_rss_seasonal <- function(pred_h1, h1_col, pred_h2, h2_col, season_key) {
	log_rss <- merge(pred_h1[, .SD, .SDcols = c('id', 'x', 'season', h1_col)],
									 pred_h2[, .SD, .SDcols = c('id', 'season', h2_col)],
									 by = c('id', 'season'), all.x = TRUE,
									 allow.cartesian=TRUE)
	log_rss[, rss := h1 - h2,
					env = list(h1 = h1_col, h2 = h2_col)]
	log_rss[, season := season_key$season]
}
