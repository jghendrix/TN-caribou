join_rss <- function(alone, dyad) {


	alone %<>% mutate(social = "alone")
	dyad %<>% mutate(social = "in_dyad")

	DT <- rbind(alone, dyad)
	return(DT)

}
