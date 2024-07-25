# Why is speed negative in some situations? ----

tar_load(calc_speed_s_road)
tch_speeds <- calc_speed_s_road %>% group_by(id, season) %>%
	summarise(speed = mean(spd))
tar_load(calc_speed_s_fire)
fire_speeds <- calc_speed_s_fire %>% group_by(id, season) %>%
	summarise(speed = mean(spd))
tar_load(calc_speed_new_burn)
annual_fire_speed <- calc_speed_new_burn %>% group_by(id) %>% summarise(speed = mean(spd))
## speed ~ dist to TCH during calving is negative for 7/10 indivs 45, 46, 47, 48, 49, 52, 53
## speed ~ dist to new burn annual is negative for 6/10 indivs 44, 45, 46, 47, 48, 53
## new burn seasonal: calving negative for the same 7/10 indivs = 45, 46, 47, 48, 49, 52, 53
# spring migration negative for 44, 45, 47, 48, 50, 53

# is this because we're extending the x-axis out too far??
tar_load(season_prep)
ggplot(season_prep, aes(x = dist_to_new_burn, fill = id, group = season)) +
	geom_histogram() +
	facet_wrap( ~ season)

ggplot(subset(season_prep, season == "calving"), aes(x = Animal_ID, y = dist_to_tch)) +
	geom_boxplot()
# so the indivs with negative slopes, they don't tend ever get further than 10k from the TCH... but I don't think it's just the extended x-axis that is the issue here
steps <- season_prep %>% filter(case_ == TRUE)
ggplot(steps, aes(x = season, y = sl_)) +
	geom_boxplot() +
	scale_y_log10()

#calving and winter might be a bit slower than autumn & spring

steps %>% group_by(season) %>% summarise(mean = mean(sl_), median = median(sl_), sd = sd(sl_))

# median is much lower for calving and winter, but means are comparable... skewed? How do indivs look during these seasons?
ggplot(subset(steps, season == "winter"), aes(x = Animal_ID, y = sl_)) +
	geom_boxplot() +
	geom_jitter(alpha = 0.3) +
	scale_y_log10()

# Ok so the animals that go into negative speeds might be slightly less fast?? It's not a huge difference tho tbh

# Distance to new burn predicted values?
calc_speed <- function(DT, covariate, seq) {
if(covariate == "dist_to_new_burn")
	DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																`I(log(sl_)):forest` +
																`I(log(sl_)):open`+
																`I(log(dist_to_new_burn + 1)):I(log(sl_))
															`*seq
	)*(scale))),
	x = list(list(seq))),
	by=.(id)]
}
tar_load(prep_speed_fire)
# some of these estimates are negative... should they be?
# the indivs that go below 0 are the ones with negative slopes for new_burn:sl
# they're all negative for sl though
# and all positive (and identical) for sl:new_burn? Why are there reciprocal interactions? And why do we use one and not the other

# what do actual step lengths in response to dist_to_new_burn look like?
ggplot(steps, aes(x = dist_to_new_burn, y = sl_, colour = Animal_ID)) +
	geom_point(alpha = 0.3) +
	geom_smooth() +
	coord_cartesian(xlim = c(0, 5000),
									ylim = c(0, 2000))

# it is a negative trend. Just not sure how to get it to curve down to zero and not surpass it?

# there are points >80km away, but we're omitting all those right?

## Mapping/distance-to-feature exploration ----

points <- season_prep %>% filter(case_ == TRUE)
coords <- SpatialPoints(data.frame(points[, c("x1_","y1_")]),
												proj4string=CRS(wgs84))
cord.UTM <- spTransform(coords, CRS(utm21N))

coords.UTM <- as.data.table(cord.UTM)
setnames(coords.UTM, c("coords.x1", "coords.x2"), c("X", "Y"))

car <- cbind(caribou, coords.UTM) %>%
	select(-c(6:7))

saveRDS(car, "output/caribou_UTM.Rds")

car_sf <- st_as_sf(x = points,
									 coords = c("x1_", "y1_"),
									 crs = utm)



ggplot() +
	geom_sf(fill = "lightgrey", size = 0.3, color = "grey25", data = nlcrop) +
	geom_sf(fill = "lightgreen", size = 0.3, color = "darkgreen", data = tn) +
	#geom_sf(fill = "steelblue1", color = NA, data = streamPols) +
	#geom_sf(color = "cornflowerblue", size = 0.4, data = streamLns) +
#	geom_sf(fill = "goldenrod", colour = "darkred", size = 0.3,	data = tnburns) +
	geom_sf(aes(colour = dist_to_tch), alpha = 1, size = 1,
					data = car_sf) +
	geom_sf(colour = "black", data = highway) +
	scale_color_viridis(option = "B") +
	scale_fill_viridis(option = "C") +
	coord_sf(xlim = c(bb['xmin'], bb['xmax']),
					 ylim = c(bb['ymin'], bb['ymax'])) +
	#guides("none") +
	themeMap

# On this map, there isn't much beyond 20km from the TCH... what are all these super high values??
ggplot(subset(season_prep, id != "tn85845"), aes(x = dist_to_tch, fill = id)) +
	geom_histogram()

ggplot(season_prep, aes(x = dist_to_tch, fill = id)) +
	geom_histogram()

ggplot(subset(season_prep, case_ == TRUE), aes(x = dist_to_new_burn, fill = id)) +
	geom_histogram()

points %>% filter(dist_to_tch > 20000) #191 obs among 10216 = 1.9%
points %>% filter(dist_to_tch > 20000 & id != "tn85845") # of 8229 obs excluding that one animal, only 19 (0.2%) are above 20km - it's really just that one animal

ggplot(points, aes(x = dist_to_minor, fill = id)) +
	geom_histogram() #distance to minor roads hardly goes above 10k
points %>% filter(dist_to_minor > 10000) #159 obs among 10216 = 1.6%
points %>% filter(dist_to_minor > 10000 & id != "tn85845") # of 8229 obs excluding that one animal, only 7 (0.08%) are above 10km - it's really just that one animal

ggplot(points, aes(x = dist_to_new_burn, fill = id)) +
	geom_histogram()
points %>% filter(dist_to_new_burn > 20000) #300 obs among 10216 = 2.9%
points %>% filter(dist_to_new_burn > 20000 & id != "tn85845") # 31 of 8229 obs (0.4%) excluding that one animal

ggplot(points, aes(x = dist_to_old_burn, fill = id)) +
	geom_histogram()
points %>% filter(dist_to_old_burn > 20000) #268 obs among 10216 = 2.6%
points %>% filter(dist_to_old_burn > 20000 & id != "tn85845") # 29 of 8229 obs (0.4%) excluding that one animal

# IN CONCLUSION:
## For all the distance metrics, we can comfortably only predict/look up to 20km. don't worry about those larger distances, they're meaningless. For distance to minor roads, we could even do 10km without worrying


