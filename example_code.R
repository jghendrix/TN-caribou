### Install Packages ----
## An R package to hold and facilitate interaction with Natural Earth map data.
install.packages("rnaturalearth")

## Data to support much of the package functionality are stored in two additional data packages
devtools::install_github("ropensci/rnaturalearthdata")
devtools::install_github("ropensci/rnaturalearthhires")

## Additional data (coastlines, roads, etc) can be found here:
# http://www.naturalearthdata.com/downloads/50m-physical-vectors/

### Packages ----
library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)
library(cowplot)
library(viridis)
library(ggspatial)
library(sf)
#library(rgdal)
library(raster)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)




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
