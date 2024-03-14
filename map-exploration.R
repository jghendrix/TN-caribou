### Packages ----
library(data.table)
library(sf)
library(sp)
library(zip)
library(osmdata)
library(dplyr)
library(magrittr)
library(ggplot2)
library(cowplot)
library(viridis)
library(ggspatial)

## What do the burn areas look like?
burns <- read_sf("input/Burn_Areas.shp")

# Need a base map to plot these on

# === Terra Nova - Prep ---------------------------------------------------
# Alec L. Robitaille, Isabella Richmond

# Download TN data --------------------------------------------------------
## Polygon from Open Canada
# https://clss.nrcan-rncan.gc.ca/data-donnees/nplb_llpn/
unzip('input/CLAB_NL_2023-09-08.zip', exdir = 'input/national-parks')

parks <- st_read('input/national-parks/CLAB_NL_2023-09-08.shp')
tn <- parks[parks$CLAB_ID == 'NOVA', ]

## Roads
# Need latlong
bb <- st_bbox(st_transform(st_buffer(tn, 5e4), 4326))
routes <- opq(bb) %>%
	add_osm_feature(key = 'highway') %>%
	osmdata_sf()

# Grab roads
roads <- routes$osm_lines

# Water (internal)
water <- opq(bb) %>%
	add_osm_feature(key = 'natural', value = 'water') %>%
	osmdata_sf()

# Grab polygons
mpols <- water$osm_multipolygons

# Union and combine
waterpols <- st_union(st_combine(mpols))
## TODO: "Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : Loop 4111 edge 1463 has duplicate near loop 5211 edge 1361"
# water bodies/polygons are giving an error, but streams are fine... let's just move on for now

# Streams
waterways <- opq(bb) %>%
	add_osm_feature(key = 'waterway') %>%
	osmdata_sf()

streamsPol <- st_cast(st_polygonize(st_union(waterways$osm_lines)))
streamsLns <- waterways$osm_lines


# Reprojection ----
utm <- st_crs('+proj=utm +zone=21 ellps=WGS84')

# Project to UTM
utmTN <- st_transform(tn, utm)
utmRoads <- st_transform(roads, utm)
#utmWater <- st_transform(waterpols, utm)
utmStreamsLns <- st_transform(streamsLns, utm)
utmStreamsPol <- st_transform(streamsPol, utm)

# Output ----
st_write(utmTN, 'output/terra-nova-polygons.gpkg')
st_write(utmBurn, 'output/TN_burns.gpkg')
st_write(utmRoads, 'output/terra-nova-roads.gpkg')
#st_write(utmWater, 'output/terra-nova-water.gpkg')
st_write(utmStreamsLns, 'output/terra-nova-streams-lns.gpkg')
st_write(utmStreamsPol, 'output/terra-nova-streams-pols.gpkg')

# Data ----
tn <- st_read('output/terra-nova-polygons.gpkg')
roads <- st_read('output/terra-nova-roads.gpkg')
nl <- st_read('output/newfoundland-polygons.gpkg')
#water <- st_read('output/terra-nova-water.gpkg')
streamLns <- st_read('output/terra-nova-streams-lns.gpkg')
streamPols <- st_read('output/terra-nova-streams-pols.gpkg')

# Only main highway and primary
selroads <- c('trunk', 'primary')
highway <- roads[roads$highway %in% selroads,]

# Map theme ----

roadcols <- data.table(highway = selroads)
roadcols[, cols := gray.colors(.N, start = 0.1, end = 0.4)]
roadpal <- roadcols[, setNames(cols, highway)]

# Theme
themeMap <- theme(panel.border = element_rect(size = 1, fill = NA),
									panel.background = element_rect(fill = "lightblue"),
									panel.grid = element_line(color = "grey", size = 0.2),
									axis.text = element_text(size = 11, color = 'black'),
									axis.title = element_blank())

# x/y limits
bb <- st_bbox(tn) - rep(c(1e3, -1e3), each = 2)


# Plot ----
# Crop NL
nlcrop <- st_crop(nl, bb + rep(c(-5e4, 5e4), each = 2))

# Base terra-nova
(gtn <- ggplot() +
		geom_sf(fill = "lightgrey", size = 0.3, color = "grey25", data = nlcrop) +
		geom_sf(fill = "lightgreen", size = 0.3, color = "darkgreen", data = tn) +
		#geom_sf(fill = "lightblue", size = 0.2, color = "darkblue", data = water) +
		geom_sf(fill = "steelblue1", color = NA, data = streamPols) +
		geom_sf(color = "cornflowerblue", size = 0.4, data = streamLns) +
		geom_sf(aes(color = highway), data = highway) +
		#geom_sf_label(aes(label = 'Terra Nova National Park'), size = 5, fontface = 'bold', data = tn, nudge_x = 100, nudge_y = 100) +
		scale_color_manual(values = roadpal) +
		coord_sf(xlim = c(bb['xmin'], bb['xmax']),
						 ylim = c(bb['ymin'], bb['ymax'])) +
		guides("none") +
		themeMap)

ggsave(
	'graphics/base-terra-nova.png',
	gtn,
	width = 10,
	height = 10,
	dpi = 320
)


# Adding burn layer? ----
utmBurn <- st_transform(burns, utm)

st_write(utmBurn, 'output/terra-nova-burns.gpkg')
tnburns <- st_read('output/terra-nova-burns.gpkg')

# If I try to add as is, returns an error message
# need to remove the z-coord from the burns, apparently? Not sure why that works but it solves the problem
tnburns <- st_zm(tnburns)

# Fire map
(gburn <- ggplot() +
 	geom_sf(fill = "lightgrey", size = 0.3, color = "grey25", data = nlcrop) +
 	geom_sf(fill = "lightgreen", size = 0.3, color = "darkgreen", data = tn) +
 	geom_sf(fill = "steelblue1", color = NA, data = streamPols) +
 	geom_sf(color = "cornflowerblue", size = 0.4, data = streamLns) +
 	geom_sf(fill = "goldenrod", size = 0.3, color = "darkred",
 					data = tnburns) +
 	geom_sf(aes(color = highway), data = highway) +
 	scale_color_manual(values = roadpal) +
 	coord_sf(xlim = c(bb['xmin'], bb['xmax']),
 					 ylim = c(bb['ymin'], bb['ymax'])) +
 	guides("none") +
 	themeMap)

### Importing caribou data ----
caribou <- read.csv('input/TNNP_ALL_Caribou.csv')
caribou %<>% mutate(year = str_sub(DATETIME, start = 0, end = 4),
										date = str_sub(DATETIME, start = 0, end = 10),
										Animal_ID = as.factor(Animal_ID))
caribou$date <- gsub("\\.", "-", caribou$date)
caribou %<>% mutate(date = as.Date(date))


sum <- caribou %>% group_by(Animal_ID, year) %>%
	summarise(n = n())
caribou %>% group_by(Animal_ID) %>%
	summarise(start = min(date),
						end = max(date),
						length = (end - start),
						months = length/30.5)
# 10 indiv, between 7 months and 2.5y

# Can we add these to the map?

# Long and Lat are correct, but the X Y coords seem to be in an entirely different format than the other layers...

# convert these to new easting and northings

utm21N <- '+proj=utm +zone=21 ellps=WGS84 +datum=WGS84 +units=m +no_defs'
wgs84 <- '+proj=longlat +datum=WGS84 +no_defs +type=crs'

coords <- SpatialPoints(data.frame(caribou[, c("Longitude","Lat")]),
												proj4string=CRS(wgs84))
cord.UTM <- spTransform(coords, CRS(utm21N))

coords.UTM <- as.data.table(cord.UTM)
setnames(coords.UTM, c("coords.x1", "coords.x2"), c("X", "Y"))

car <- cbind(caribou, coords.UTM) %>%
	select(-c(6:7))

saveRDS(car, "output/caribou_UTM.Rds")

car_sf <- st_as_sf(x = car,
												coords = c("X", "Y"),
												crs = utm)


# Final map caribou + burns ----
(gcar <- ggplot() +
 	geom_sf(fill = "lightgrey", size = 0.3, color = "grey25", data = nlcrop) +
 	geom_sf(fill = "lightgreen", size = 0.3, color = "darkgreen", data = tn) +
 	geom_sf(fill = "steelblue1", color = NA, data = streamPols) +
 	geom_sf(color = "cornflowerblue", size = 0.4, data = streamLns) +
 	geom_sf(fill = "goldenrod", colour = "darkred", size = 0.3,	data = tnburns) +
 	geom_sf(colour = "black", alpha = 0.3, size = 0.3,
 					data = car_sf) +
 	geom_sf(aes(color = highway), data = highway) +
 	scale_color_manual(values = roadpal) +
 	scale_fill_viridis(option = "C") +
 	coord_sf(xlim = c(bb['xmin'], bb['xmax']),
 					 ylim = c(bb['ymin'], bb['ymax'])) +
 	#guides("none") +
 	themeMap)

ggsave(
	'graphics/terra-nova-caribou-burns.png',
	gcar,
	width = 10,
	height = 10,
	dpi = 320
)

# Map caribou + burns, but colour coded by age ----
# had to omit Ochre Hill fires where year == 0

(gcar <- ggplot() +
		geom_sf(fill = "lightgrey", size = 0.3, color = "grey25", data = nlcrop) +
		geom_sf(fill = "lightgreen", size = 0.3, color = "darkgreen", data = tn) +
		geom_sf(fill = "steelblue1", color = NA, data = streamPols) +
		geom_sf(color = "cornflowerblue", size = 0.4, data = streamLns) +
		geom_sf(aes(fill = YEAR, colour = "darkred"), size = 0.3,
						data = subset(tnburns, YEAR > 1)) +
		geom_sf(colour = "black", alpha = 0.3, size = 0.3,
						data = car_sf) +
		geom_sf(aes(color = highway), data = highway) +
		scale_color_manual(values = roadpal) +
		scale_fill_viridis(option = "C") +
		coord_sf(xlim = c(bb['xmin'], bb['xmax']),
						 ylim = c(bb['ymin'], bb['ymax'])) +
		#guides("none") +
		themeMap)

ggsave(
	'graphics/terra-nova-caribou-burns-by-age.png',
	gcar,
	width = 10,
	height = 10,
	dpi = 320
)
