#' @title Comparing variation between individuals with how social they are
#' @export
#' @author Jack G Hendrix
social_variation <- function(dyads, fire_summary, road_summary) {

## How social was each individual during each season
	props <- dyads %>% group_by(Animal_ID, season, in_group) %>%
		summarise(n = n()) %>%
		ungroup(in_group) %>%
		mutate(prop = n/sum(n))

	noNA <- props %>% filter(!is.na(in_group)) %>%
		group_by(Animal_ID, season) %>%
		mutate(prop_noNA = n/sum(n)) %>%
		select(-c(n, prop))

props <- left_join(props, noNA, by = c("Animal_ID", "season", "in_group")) %>%
	filter(in_group == "alone")
	# if you filter by = "dyad", some individuals were totally asocial and the zeros get dropped out

# Where did we see the biggest individual variation?

# TCH during calving and old burn during spring migration, especially
# old burn calving, one outlier
# new burn spring, calving, and autumn, one outlier each (and not the same??)

coefs <- fire_summary %>% dplyr::filter(term == "I(log(dist_to_old_burn + 1))") %>%
	select(c(Animal_ID = id, season = seasonality, fire_coef = estimate))

df <- left_join(props, coefs, by = c("Animal_ID", "season"))

ggplot(subset(df, season == "spring_migration"), aes(x = prop, y = fire_coef)) +
	geom_point() +
	geom_smooth(method = "lm")

# not that clear of a relationship for old burns during spring migration
# what about TCH calving?
coefs <- road_summary %>% dplyr::filter(term == "I(log(dist_to_tch + 1))") %>%
	select(c(Animal_ID = id, season = seasonality, road_coef = estimate))

df <- left_join(props, coefs, by = c("Animal_ID", "season"))

ggplot(subset(df, season == "calving"), aes(x = prop, y = road_coef)) +
	geom_point() +
	geom_smooth(method = "lm")


# Not really any clear relationship between degree of sociality and the selection coefficients from the asocial model. Nevermind, ignore this side quest



# If we care about the sl_ coefficients? probably should
#sl_coefs <- fire_summary %>% dplyr::filter(grepl("sl_", term)) %>%
#	filter(grepl("old_burn", term)) %>%
#	group_by(id, seasonality) %>%
#	summarise(sl_int = sum(estimate))

# need step length averages for each season-ID
#sls <- dyads %>% filter(case_ == TRUE) %>%
#	group_by(Animal_ID, season) %>%
#	summarise(mean_sl = mean(sl_))

#sl_coefs <- left_join(sl_coefs, sls, by = c("id" = "Animal_ID", "seasonality" = "season")) %>%
#	mutate(sel_w_sl = sl_int*mean_sl) %>%
#	select(-c(sl_int, mean_sl))

#full_coef <- left_join(coefs, sl_coefs, by = c("Animal_ID" = "id", "season" = "seasonality")) %>%
#	mutate(full_coef = sel_w_sl + fire_coef)

#df2 <- left_join(props, full_coef, by = c("Animal_ID", "season"))

#ggplot(subset(df2, season == "spring_migration"), aes(x = prop, y = full_coef)) +
#	geom_point() +
#	geom_smooth(method = "lm")

}
