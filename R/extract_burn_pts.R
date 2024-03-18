## Extracting random points from each burn polygon

extract_burn_pts <- function(burn_prep) {

	burn_prep %<>% mutate(n = Hectares1*10, #approximates one point per 30x30m pixel?
												n = round(n, digits = 0),
												n = ifelse(n == 0, 1, n),
												n = ifelse(n > 100, 100, n))
	# need at least 1 point per fire, but 8214 points is excessive... cut it at 100 points?

	# only 21 unique Burn_IDs, six fires are split into inside and outside the park
	# have to generate new IDs for 2, 4, 5, 11, 14, 15
	burn_test <- burn_prep %>% mutate(id = ifelse(Loc_toPark == "Outside" &
																	 	Burn_ID %in% c(2, 4, 5, 11, 14, 15),
																	 	NA, Burn_ID))
	NAs <- burn_test %>% filter(is.na(id)) %>% mutate(id = seq(22, 27))
	burn_prep <- rbind(subset(burn_test, !is.na(id)), NAs)
	# this is such a laborious and ineffective way of getting there, but it worked lol


	# Generate random points within each burn
	pts <- data.frame()
	all_pts <- data.frame()

	for(i in 1:27) {

		print(i)

		polygon <- burn_prep %>% dplyr::filter(burn_prep$id == i)
		points = sf::st_sample(polygon, size = polygon$n)
		pts <- points %>% sf::st_coordinates() %>% as.data.frame()
		pts %<>% mutate(id = i)

		all_pts <- rbind(all_pts, pts)

	}

	return(all_pts)
}
