split_burn <- function(burn_prep, age) {

	if(age == "old")
		burn_prep %<>% dplyr::filter(YEAR < 1992)

	if(age == "new")
		burn_prep %<>% dplyr::filter(YEAR > 1992)

	return(burn_prep)
}
