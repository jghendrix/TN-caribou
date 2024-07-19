### Install Packages ----

library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)
library(cowplot)
library(viridis)
library(ggspatial)
library(sf)
library(raster)




### Overview of the`rnaturalearth` packages ----
## Set world country polygons at a specified scale

spdf_world <- ne_countries(scale = "medium", returnclass = "sf")
# Make sure to include returnclass = "sf"`
## "sp" used to be the default data structure for everything in R, but it has its own utility plotting functions in spplot that does not talk with ggplot very well
# structured as a list of lists, and harder to manipulate without losing metadata
# sf is the recommended alternative, same functionality but more flexibility - objects don't change class when you apply spatial manipulations, aka works with ggplot

class(spdf_world) # should return both a sf and data.frame

spdf_na <- ne_countries(scale = "medium", continent = "north america", returnclass = "sf")
spdf_canada <- ne_countries(scale = "medium", country = "canada", returnclass = "sf")
# Note: these don't return borders within a country

## Use ne_states to get provincial/territorial borders
canada_map <- ne_states(country = 'canada', returnclass = "sf")

### Steps to building a map ----
## Set the theme for the map to a dark-on-light
theme_set(theme_bw())

## Begin with a general map of Canada
ggplot() +
	geom_sf(data = canada_map)

## Changing fill/colours
# Set boundaries to black and fill to white
ggplot() +
	geom_sf(data = canada_map, colour = "black", fill = "white")

# Can customize based on data in the accompanying data.frame
ggplot() +
	geom_sf(data = canada_map, colour = "black", aes(fill = name)) +
	scale_fill_viridis(discrete = TRUE)

# Highlight one province
ggplot() +
	geom_sf(data = canada_map, colour = "black",
					fill = ifelse(canada_map$name == "Newfoundland and Labrador",
												'darkgreen', 'white'))

# Change theme and add blue background
ggplot() +
	geom_sf(data = canada_map, colour = "black",
					fill = ifelse(canada_map$name == "Newfoundland and Labrador",
												'darkgreen', 'white')) +
													theme_bw() +
	theme(panel.background = element_rect(fill = "aliceblue"))

### Changing the projection and extent ----
## Using Canada’s favorite - the Lambert Conformal Conic projection

ggplot() +
	geom_sf(data = canada_map, colour = "black",
					fill = ifelse(canada_map$name == "Newfoundland and Labrador",
												'blue', 'white')) +
													coord_sf(crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
# just copy-paste this text for this specific projection


## Fix the x axis
ggplot() +
	geom_sf(data = canada_map, colour = "black",
					fill = ifelse(canada_map$name == "Newfoundland and Labrador",
												'darkgreen', 'white')) +
													coord_sf(crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0
           +datum=NAD83 +units=m +no_defs") +
	scale_x_continuous(breaks = c(-120, -105, -90, -75, -60))

# Bring it all together and save as an object
can_map <- ggplot() +
	geom_sf(data = canada_map, colour = "black",
					fill = ifelse(canada_map$name == "Newfoundland and Labrador",
												'darkgreen', 'white')) +
													coord_sf(crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0
           +datum=NAD83 +units=m +no_defs") +
	scale_x_continuous(breaks = c(-120, -100, -80, -60)) +
	theme_bw() +
	theme(panel.background = element_rect(fill = "aliceblue"))

can_map

### Subset to Newfoundland and Labrador----
ggplot(data = canada_map) +
	geom_sf(colour = "black", fill = "white") +
	coord_sf(xlim = c(-67.5, -53), ylim = c(47, 60.5), expand = TRUE)
# setting axis limits within coord_sf
# this keeps the other provinces around the perimeter
## OR
# subset within the geom_sd to exclusively NL (the 10th province)
map_nl <- canada_map[10,]
ggplot() +
	geom_sf(data = map_nl, colour = "black", fill = "white")

## Can change the projection to lcc or any others if you prefer
ggplot() +
	geom_sf(data = map_nl, colour = "black", fill = "white") +
	coord_sf(crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0
           +datum=NAD83 +units=m +no_defs")

#ggsave("NL_Subset.png", height = 7, width = 5, dpi = "screen")

### Plot NL cities on the Avalon ---
## Get list of NL cities and populations from the 2021 census report
## Statistics Canada. Table 98-10-0002-01  Population and dwelling counts: Canada and census subdivisions (municipalities)
## https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810000201

nl.cities <- fread("Input/2021_Can_Census_w_Locs.csv")

nl.cities[, Population_2021 := as.numeric(Population_2021)]
nl.cities[, Population_2016 := as.numeric(Population_2016)]

# Plot NL cities/towns as `geom_point()` by 2021 Population
ggplot() +
	geom_sf(data = map_nl, colour = "black", fill = "white") +
	geom_point(data = nl.cities, aes(x = Longitude, y = Latitude, colour = Population_2021, alpha = 0.5)) +
	scale_x_continuous(breaks=seq(-68, -52, 3))

cities <- ggplot() +
	geom_sf(data = map_nl, colour = "black", fill = "white") +
	geom_point(data = nl.cities, aes(x = Longitude, y = Latitude, colour = Population_2021)) +
	#xlim(-70, -50) +
	scale_x_continuous(breaks=seq(-68, -52, 3)) +
	scale_color_viridis_c(option = "plasma", trans = "log10") +
	labs(colour = "Population 2021")

# Add scale bar and north arrow
final <- cities +
	annotation_scale(location = "tr", width_hint = 0.5) +
	# tr = top right, width_hint = proportion of map that the scale bar will fill (aka it's 50% of the whole map here)
	annotation_north_arrow(location = "tl", which_north = "true",
												 height = unit(1, "cm"),
												 width = unit(1, "cm"),
												 pad_x = unit(0.25, "cm"),
												 # margin from corner
												 pad_y = unit(0.25, "cm"),
												 style = north_arrow_fancy_orienteering) +
	theme_bw() +
	theme(panel.background = element_rect(fill = "aliceblue"),
				legend.position = c(0.8, 0.75), legend.background = element_rect(fill = "transparent"),
				legend.title = element_text(size = 12, face = "bold"))
final

# Create a second map to add as your inset
inset <- can_map +
	annotation_scale(location = "bl", width_hint = 0.25) +
	theme_void() + # simplifying this Canada map so it's less busy as the inset
	theme(panel.background = element_rect(fill = "aliceblue", size = 1), #colour = "black",
				panel.border = element_rect(colour = "black", fill=NA, size = 1))

inset

# Add inset using ggdraw from the cowplot package
NL.full <- ggdraw(final) +
	draw_plot(inset, width = 0.3, height = 0.3, x = 0.25, y = 0.05)

NL.full

ggsave("Output/map_final.png", plot = NL.full, width = 7, height = 7, dpi = "screen")


# Mapping rasters -------
install.packages("rgdal")

# Load Packages
libs <- c('ggplot2', "dplyr", "magrittr", "sf",
					"ggspatial", "raster", "dplyr")
lapply(libs, require, character.only = TRUE)

# Mapping Shapefiles within the aesthetic
Nl.Eco <- readOGR("GIS_Layers/Ecoregions/Ecoregions.shp")
#Nl.Eco <- spTransform(Nl.Eco, wgs84)

# first step: change into a simple feature, easier to work with
NL.Eco <- st_as_sf(Nl.Eco)

# filter the data down to just island of NL
NL.Eco <- NL.Eco %>%
	filter(ECOREGION >= 106 & ECOREGION <= 116) %>%
	mutate(ECOREGION = as.factor(ECOREGION))

ggplot() +
	annotation_spatial(data = NL.Eco, aes(fill = REGION_NAM)) +
	# annotation_spatial() knows how to interpret the "geometry" column of this data type with dozens of coordinates
	layer_spatial(data = map_nl, colour = "black", size = 0.5, fill = "transparent")  +
	coord_sf(xlim = c(-60, -52.5), ylim = c(46, 52.5), expand = TRUE) +
	labs(fill = "Region Name")

# Mapping Shapefiles without aesthetic
GHA26 <- readOGR("GIS_Layers/Simple_Polygon/Study_area_Eastern_MB.shp")
GHA26 <- spTransform(GHA26, wgs84)

ggplot() +
	geom_polygon(data=GHA26, mapping = aes(x=long, y=lat, group=group),               fill= "forestgreen", color="black", size=0.25, alpha = 0.2)
# we can use geom_polygon() here because it's a simple shapefile, unlike annotation_spatial() in the above graph for the sf data type

# Mapping Rasters ----
## DON'T RUN THIS FIRST PART
mb.lc <- raster("GIS_Layers/Landcover/Landcover_2015_30.tif")
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
mb.lc <- projectRaster(mb.lc, crs = wgs84)

# Crop raster as small as you can
lc.GHA26 <- crop(mb.lc, GHA26)

# Turn raster into as spatial pixels data frame and save as a data.frame
test_spdf <- as(lc.GHA26, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("Value", "x", "y")

# Merge legend with raster data.frame and save for the future
legend <- read.csv("GIS_Layers/CoverType_2015_Legend.csv")
dat <- merge(test_df, legend, by = "Value")
fwrite(dat, "MB_Landcover_SPDF.Rds")

# START FROM HERE

landcover <- fread("Input/MB_Landcover_SPDF.Rds")

ggplot() +
	geom_tile(data=landcover, aes(x=x, y=y, fill= Cover)) +
	geom_polygon(data=GHA26, mapping = aes(x=long, y=lat, group=group),
							 fill= "grey", color="black", size=0.25, alpha = 0.2)

ggsave("Mapping_Rasters.png", width = 7, height = 7, dpi = "screen")





### EXTRA ----
## Adding burn layer to this, and zooming in ----
# Project to UTM
utmBurn <- st_transform(burns, utm)
st_write(utmBurn, 'output/burn-polygons.gpkg')
burnUTM <- st_read('output/burn-polygons.gpkg')


(gnl <- ggplot(nl) +
		geom_sf(fill = "lightgreen", color = "darkgreen", size = 0.3) +
		geom_sf(data = burnUTM, fill = "goldenrod", colour = "darkred", size = 0.5) +
		themeMap)




ggsave(
	'graphics/04-newfoundland.png',
	gnl,
	width = 7,
	height = 7,
	dpi = 320
)


#### What landcover surface do we want? ----
# SDSS landcover, restricted to TN area?

sums <- tracks_random %>% summarise(
	minx = min(x1_),
	miny = min(y1_),
	maxx = max(x1_),
	maxy = max(y1_))

bbox <- tracks_random %>% summarise(
	minX = min(x2_, na.rm = T),
	minY = min(y2_, na.rm = T),
	maxX = max(x2_, na.rm = T),
	maxY = max(y2_, na.rm = T))

## All the 2_'s are further than the 1_'s, use those as the bounding box
LLbbox <- tracks_random %>% summarise(
minLat = min(Lat),
maxLat = max(Lat),
minLong = min(Longitude),
maxLong = max(Longitude))

## Exploring distribution of fixes and steps through time ----

tar_load(locs_raw)
# 1.05 million GPS locs
tar_load(tracks_extract)
steps <- tracks_extract %>% dplyr::filter(case_ == "TRUE")
# 12.6 k steps ...
length(steps$x1_) / length(locs_raw$X)
# 1.2% ratio ... where are all the data going??

summary(locs_raw)
missing <- locs_raw %>% dplyr::filter(is.na(X))
# 1 008 528 rows are just blank, with no data??
# Animal_ID isn't NA, b/c there's a space there, but it's not real data
real <- locs_raw %>% dplyr::filter(!is.na(X))

# 40 047 actual fixes across the dataset

locs_raw$date <- lubridate::as_datetime(locs_raw$DATETIME)
ggplot(real, aes(x = Animal_ID, y = date)) +
	geom_point(alpha = 0.3, colour = "grey") +
	geom_point(data = steps, aes(x = Animal_ID, y = t1_), alpha = 0.3) +
	coord_flip()

# hard to see with so many points... summarise to day?

steps %<>% mutate(day = date(t1_), type = "step") %>% group_by(Animal_ID, ANIMAL_SN, day, type) %>% summarise(n = n())

real %<>% mutate(day = date(date), type = "fix") %>% group_by(Animal_ID, ANIMAL_SN, day, type) %>% summarise(n = n())

dates <- rbind(real, steps)

ggplot(dates, aes(x = Animal_ID, y = day, colour = type)) +
	geom_point(alpha = 0.4, position = position_dodge(width = -0.4)) +
	scale_colour_viridis(discrete = TRUE, begin = 0.2, end = 0.85) +
	coord_flip()

# seasonal coverage?
id1 <- dates %>% filter(Animal_ID == "tn85849", year(day) == 2021)
seasons <- id1 %>% group_by(ANIMAL_SN) %>% summarise(min = min(day),
																					max = max(day))
# W winter 14 Dec - 31 March
# SM spring migration 1 April - 19 May
# C calving 20 May - 10 June
# PC post calving 11 June - 30 June
# PCR post calving... range(?) 1 July - 31 August
# FR fall... range(?) 1 Sept - 31 Oct
# FM fall migration 1 Nov - 13 Dec

seasons %<>% mutate(days = max - min) %>%
	select(-c(ANIMAL_SN))

seasons$season <- factor(seasons$season, levels = c("W", "SM", "C", "PC", "PCR", "FR", "FM"))

# Now to get the steps per season for each indiv
library(stringr)
seasons %<>% mutate(season = str_extract(ANIMAL_SN, "(?<=_).*"),
										season = str_extract(season, "(?<=_).*"))
# there are two underscores, so we have to do this twice...
# can't call for the last two characters or whatever b/c the seasons are different lengths
# regardless, this worked

steps %<>% mutate(season = str_extract(ANIMAL_SN, "(?<=_).*"),
									season = str_extract(season, "(?<=_).*"))

steps <- tracks_extract %>% filter(case_ == "TRUE") %>%
							mutate(season = str_extract(ANIMAL_SN, "(?<=_).*"),
										season = str_extract(season, "(?<=_).*"))
steps$season <- factor(steps$season, levels = c("W", "SM", "C", "PC", "PCR", "FR", "FM"))

d <- steps %>%
	mutate(year = year(t1_), doy = yday(t1_)) %>%
	mutate(year = ifelse(season == "W" & doy > 335, year + 1, year)) %>%
	group_by(Animal_ID, season, year) %>%
	summarise(n = n()) %>%
	pivot_wider(names_from = season, values_from = n)

write.csv(d, 'output/steps_by_season.csv')


# where is the long range travel by that one animal, tn85845
NAs <- steps %>% filter(!is.na(pt_lc), Animal_ID == "tn85845") %>%
	mutate(year = year(t1_), doy = yday(t1_)) %>%
	filter(year == 2022)

# between 13 Jan and 8 April, she was off on an adventure away from the park
# mostly winter 2022, then
# she has 261 steps in winter 2022, this isn't going to be the same as in practice
# removing NAs, it's only 57 steps

noNA <- steps %>% filter(!is.na(pt_lc)) %>%
	mutate(year = year(t1_), doy = yday(t1_)) %>%
	mutate(year = ifelse(season == "W" & doy > 335, year + 1, year)) %>%
	group_by(Animal_ID, season, year) %>%
	summarise(n = n()) %>%
	pivot_wider(names_from = season, values_from = n)
write.csv(noNA, 'output/steps_by_season_with_lc.csv')

# Any conclusions here on whether these are enough steps per season to do anything time-of-year specific?? TBD ----

# How much are they even using the burns? ----
# looking just since 1990
new_burns %<>% filter(case_ == "TRUE") %>% mutate(in_fire = ifelse(dist_to_new_burn == 0, 1, 0))
new_sum <- new_burns %>% group_by(Animal_ID, in_fire) %>% summarise(n = n()) %>% ungroup() %>% group_by(Animal_ID) %>% mutate(prop = n/sum(n))

tracks_extract %<>% filter(case_ == "TRUE") %>% mutate(in_fire = ifelse(dist_to_burn_prep == 0, 1, 0))
all_sum <- tracks_extract %>% group_by(Animal_ID, in_fire) %>% summarise(n = n()) %>% ungroup() %>% group_by(Animal_ID) %>% mutate(prop = n/sum(n))


# Assessing extant landcover w/i burn polygons ----

# want to randomly generate a number of points within each fire
# fires vary in size, so sampling 100 points/fire wouldn't even be close to proportional

ggplot(burn_prep, aes(x = Burn_ID, y = Hectares1)) +
	geom_point() +
	ylim(c(0, 5))

# 3 big fires > 200ha, the majority are less than that though
# 10 smallest fires are less than 20 ha
# 4 smallest are 1ha or less
burn_prep %<>% mutate(area = 10000*Hectares1)

# I was going to say 1 point per ha for each fire, but that leaves us with only one point for the smaller ones... 10 points minimum? And for fires > 10ha, 1 point per ha?

# smallest fire, Rocky Pond 2002, is 1m2... unlikely to even show up in CFS landcover tbh
# the next biggest are 500 and 800m2, still less than a 30x30m pixel
# one point per 1000m2, 10points/ha? Up to a maximum of 100 points maybe?

# how do you even randomly generate points inside a polygon?

polygon <- subset(burn_prep, Burn_ID == 15)
points = sf::st_sample(polygon, size=10)
# Plot using the ggplot geom_sf function
ggplot() +
	geom_sf(aes(), data=polygon) +
	geom_sf(aes(), data=points)

pts <- points %>% sf::st_coordinates() %>% as.data.frame()















# Targets: annual road model ----------------------------------------------------------
targets_model <- c(
	tar_target(
		model_prep,
		prepare_model(tracks_roads)
	),
	tar_target(
		fire_model,
		model_fire_bin(model_prep)
	),
	tar_target(
		fire_model_check,
		model_check(fire_model)
	)
)

# Targets: seasonal road model ----------------------------------------
targets_roads <- c(

	tar_target(
		season_prep,
		model_prep[, tar_group := .GRP, by = c('season')],
		iteration = 'group'
	),
	tar_target(
		season_key,
		unique(season_prep[, .SD, .SDcols = c(seasonal_split, 'tar_group')])
	),
	tar_target(
		roads_model,
		model_roads_bin(season_prep),
		map(season_prep)
	),
	tar_target(
		roads_model_check,
		model_check(roads_model),
		map(roads_model)
	)
)

# Targets: road output and effects ------------------------------------------------------------
targets_effects <- c(
	tar_target(
		indiv_fire,
		indiv_estimates(fire_model)
	),
	tar_target(
		fire_boxplot,
		plot_box_horiz(indiv_fire, plot_theme())
	),

	tar_target(
		indiv_roads,
		indiv_seasonal(roads_model, season_key),
		pattern = map(roads_model, season_key)
	),
	tar_target(
		roads_boxplot,
		plot_boxplot_roads(indiv_roads, plot_theme()),
		pattern = map(indiv_roads)
	)
)

# Targets: speed from annual road ------------------------------------------------------------
targets_speed_f <- c(
	tar_target(
		prep_speed_fire,
		prepare_speed(
			DT = model_prep,
			summary = indiv_fire,
			params = dist_parameters
		)
	),
	tar_target(
		calc_speed_open_fire,
		calc_speed(prep_speed_fire, 'open', seq = 0:1)
	),
	tar_target(
		plot_speed_open_fire,
		plot_box(calc_speed_open_fire, plot_theme()) +
			labs(x = 'Closed vs open', y = 'Speed (m/2hr)')
	),
	tar_target(
		calc_speed_burn,
		calc_speed(prep_speed_fire, 'dist_to_new_burn', seq(1, 27000, length.out = 100L))
	),
	tar_target(
		plot_speed_burn,
		plot_dist(calc_speed_burn, plot_theme()) +
			labs(x = 'Distance to new burn (m)', y = 'Speed (m/2hr)')
	)
)

# Targets: speed from seasonal road --------------------------
targets_speed_r <- c(

	tar_target(
		prep_speed_roads,
		prepare_speed_seasonal(
			DT = season_prep,
			summary = indiv_roads,
			params = dist_parameters,
			season_key = season_key
		),
		map(indiv_roads, season_key)
	),
	tar_target(
		calc_speed_open_roads,
		calc_speed_road(prep_speed_roads, 'open', seq = 0:1, season_key),
		map(prep_speed_roads, season_key)
	),
	tar_target(
		plot_speed_open_roads,
		plot_box_roads_speed(calc_speed_open_roads, plot_theme())
	),
	# ^ does response to open vs. closed vary between our fire model and roads model? theoretically it shouldn't matter much

	tar_target(
		calc_speed_roads,
		calc_speed_road(prep_speed_roads, 'dist_to_tch', seq(1, 60000, length.out = 100L), season_key),
		map(prep_speed_roads, season_key)
	),
	tar_target(
		plot_speed_roads,
		plot_dist_seasonal(calc_speed_roads, plot_theme())
	)
)
## ^ these seasonal plots look like garbage? but maybe that's what they're meant to look like?


# Targets: RSS from annual road model -----------------------------------------------------------
targets_rss <- c(
	tar_target(
		pred_h1_new_burn,
		predict_h1_new_burn(model_prep, fire_model)
	),
	tar_target(
		pred_h1_old_burn,
		predict_h1_old_burn(model_prep, fire_model)
	),
	tar_target(
		pred_h1_forest,
		predict_h1_forest(model_prep, fire_model)
	),
	tar_target(
		pred_h2,
		predict_h2(model_prep, fire_model)
	),
	tar_target(
		rss_forest,
		calc_rss(pred_h1_forest, 'h1_forest', pred_h2, 'h2')
	),
	tar_target(
		rss_old_burn,
		calc_rss(pred_h1_old_burn, 'h1_old_burn', pred_h2, 'h2')
	),
	tar_target(
		rss_new_burn,
		calc_rss(pred_h1_new_burn, 'h1_new_burn', pred_h2, 'h2')
	),
	tar_target(
		plot_rss_forest_fire,
		plot_rss(rss_forest, plot_theme()) +
			labs(x = 'Forest', y = 'logRSS',
					 title = 'RSS compared to 0 forest (fire model)')
	),
	tar_target(
		plot_rss_new_burn,
		plot_rss(rss_new_burn, plot_theme()) +
			labs(x = 'Distance to new burn (m)', y = 'logRSS',
					 title = 'RSS compared to median distance from post-1992 burns')
	),
	tar_target(
		plot_rss_old_burn,
		plot_rss(rss_old_burn, plot_theme()) +
			labs(x = 'Distance to old burn (m)', y = 'logRSS',
					 title = 'RSS compared to median distance from pre-1992 burns')
	),
	tar_target(
		rss_plots,
		save_rss_plot(plot_rss_forest_fire,
									plot_rss_old_burn,
									plot_rss_new_burn)
	)
)

# Targets: RSS from seasonal road model -----------------------------------------------------------
targets_rss_roads <- c(
	# Ideally I would like to have a four-panel figure looking at RSS of distance to the highway (tch) and distance to minor roads, separating out the four seasons

	tar_target(
		pred_h1_forest_roads,
		predict_h1_forest_roads(season_prep, roads_model, season_key),
		pattern = map(season_prep, roads_model, season_key)
	),
	tar_target(
		pred_h1_tch,
		predict_h1_tch(season_prep, roads_model, season_key),
		pattern = map(season_prep, roads_model, season_key)
	),
	tar_target(
		pred_h1_minor,
		predict_h1_minor(season_prep, roads_model, season_key),
		pattern = map(season_prep, roads_model, season_key)
	),
	tar_target(
		pred_h2_roads,
		predict_h2_roads(season_prep, roads_model, season_key),
		pattern = map(season_prep, roads_model, season_key)
	),

	tar_target(
		rss_forest_roads,
		calc_rss_seasonal(pred_h1_forest_roads, 'h1_forest_roads', pred_h2_roads, 'h2_roads', season_key),
		map(pred_h1_forest_roads, season_key)
	),

	tar_target(
		rss_tch,
		calc_rss_seasonal(pred_h1_tch, 'h1_tch', pred_h2_roads, 'h2_roads', season_key),
		pattern = map(pred_h1_tch, season_key)
	),
	tar_target(
		rss_minor,
		calc_rss_seasonal(pred_h1_minor, 'h1_minor', pred_h2_roads, 'h2_roads', season_key),
		pattern = map(pred_h1_minor, season_key)
	),

	tar_target(
		plot_rss_forest_roads,
		plot_rss_seasonal_forest(rss_forest_roads, plot_theme())
	),
	tar_target(
		plot_rss_tch,
		plot_rss_seasonal_tch(rss_tch, plot_theme())
	),
	tar_target(
		plot_rss_minor,
		plot_rss_seasonal_minor(rss_minor, plot_theme())
	)
)





