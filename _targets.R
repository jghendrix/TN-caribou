# === Targets: iSSA workflow ----------------------------------------------
# Jack G Hendrix
# 11 July
# Trying to do a response-to-fire iSSA split by season
# in the end, would like two parallel workflows, one looking at fire and one looking at roads, each of which with a total model and then one branched over season


# Source ------------------------------------------------------------------
targets::tar_source('R')



# Options -----------------------------------------------------------------
tar_option_set(format = 'qs')



# Data --------------------------------------------------------------------
# Path to caribou locs data
locs_path <- file.path('input', 'TNNP_ALL_Caribou.csv')

# Path to land covers
lc_path <- file.path('input/lc', '2024-04-24_Hermosilla_2019_land_cover.tif')
legend_path <- file.path('input', 'cfs_legend.csv')

# Path to burns and roads
burn_path <- file.path('input', 'updated_fire.gpkg')
road_path <- file.path('output', 'terra-nova-roads.gpkg')


# Variables ---------------------------------------------------------------
# Targets: prepare
id_col <- 'Animal_ID'
datetime_col <- 'DATETIME'
x_col <- 'Longitude'
y_col <- 'Lat'
epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)
tz <- 'America/St_Johns'

# Targets: tracks
# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
split_by <- id_col
seasonal_split <- "season"

# Resampling rate
rate <- minutes(120)

# Tolerance
tolerance <- minutes(5)

# Number of random steps
n_random_steps <- 10



# Targets: data -----------------------------------------------------------
targets_data <- c(
	tar_file_read(
		locs_raw,
		locs_path,
		fread(!!.x)
	),

	tar_file_read(
		lc,
		lc_path,
		raster(!!.x)
	),

	tar_file_read(
		legend,
		legend_path,
		fread(!!.x)
	),

	tar_file_read(
		burn,
		burn_path,
		st_read(!!.x)
	),

tar_file_read(
	roads,
	road_path,
	st_read(!!.x)
)
)



# Targets: prep -----------------------------------------------------------
targets_prep <- c(
	tar_target(
		locs_prep,
		prepare_locs(locs_raw, id_col, datetime_col, tz, x_col, y_col, split_by),
		iteration = 'group'
	),
	tar_target(
		split_key,
		unique(locs_prep[, .SD, .SDcols = c(split_by, 'tar_group')])
	),
	tar_target(
		burn_prep,
		prepare_burn(burn, crs, drop_z = TRUE)
	),

	tar_target(
		old_burn,
		split_burn(burn_prep, "old")
	),
	tar_target(
		new_burn,
		split_burn(burn_prep, "new")
	)
)


# Targets: tracks ---------------------------------------------------------
targets_tracks <- c(
	tar_target(
		tracks,
		make_track(locs_prep, x_, y_, t_, all_cols = TRUE, crs = 4326) |>
			transform_coords(crs_to = crs),
		pattern = map(locs_prep)
	),
	tar_target(
		tracks_resampled,
		resample_tracks(tracks, rate = rate, tolerance = tolerance),
		pattern = map(tracks)
	),
	tar_target(
		tracks_random,
		random_steps(tracks_resampled, n = n_random_steps),
		pattern = map(tracks_resampled)
	)
)



# Targets: extract --------------------------------------------------------
targets_extract <- c(
	tar_target(
		tracks_extract,
		extract_layers(
			tracks_random,
			crs,
			lc,
			legend,
			burn_prep
		)
	),

	tar_target(
		tracks_w_old,
		old_burn_dist(
			tracks_extract,
			crs,
			old_burn
			)
	),

	tar_target(
		tracks_w_both,
		new_burn_dist(
			tracks_w_old,
			crs,
			new_burn
		)
	),

	tar_target(
		tracks_roads,
		dist_to_roads(
			tracks_w_both,
			crs,
			roads
		)
		),

	tar_target(
		avail_lc,
		calc_availability(tracks_w_both, 'lc_description', 'proportion', split_by)
	)
)



# Targets: distributions --------------------------------------------------
targets_distributions <- c(
	tar_target(
		dist_parameters,
		calc_distribution_parameters(tracks_random),
		pattern = map(tracks_random)
	),
	tar_target(
		dist_sl_plots,
		plot_distributions(tracks_resampled, 'sl_'),
		pattern = map(tracks_resampled),
		iteration = 'list'
	),
	tar_target(
		dist_ta_plots,
		plot_distributions(tracks_resampled, 'ta_'),
		pattern = map(tracks_resampled),
		iteration = 'list'
	)
)

# Targets: annual fire model ----------------------------------------------------------
targets_fire <- c(
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

# Targets: seasonal fire model ----------------------------------------
targets_fire_seasonal <- c(

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
		s_fire_model,
		model_fire_bin(season_prep),
		map(season_prep)
	),
	tar_target(
		s_fire_model_check,
		model_check(s_fire_model),
		map(s_fire_model)
	)
)

# Targets: fire output and effects ------------------------------------------------------------
targets_fire_effects <- c(
	tar_target(
		indiv_fire,
		indiv_estimates(fire_model)
	),
	tar_target(
		fire_boxplot,
		plot_box_horiz(indiv_fire, plot_theme(), "fire")
	),
	tar_target(
		s_indiv_fire,
		indiv_seasonal(s_fire_model, season_key),
		pattern = map(s_fire_model, season_key)
	),
	tar_target(
		s_fire_boxplot,
		plot_boxplot_seasonal(s_indiv_fire, plot_theme(), "fire"),
		pattern = map(s_indiv_fire)
	)
)

# Targets: speed from annual fire ------------------------------------------------------------
targets_speed_fire <- c(
	tar_target(
		prep_speed_fire,
		prepare_speed(
			DT = model_prep,
			summary = indiv_fire,
			model = "fire",
			params = dist_parameters
		)
	),
	tar_target(
		calc_speed_open_fire,
		calc_speed(prep_speed_fire, 'open fire', seq = 0:1)
	),
	tar_target(
		plot_speed_open_fire,
		plot_box(calc_speed_open_fire, plot_theme()) +
			labs(x = 'Open', y = 'Speed (m/2hr)')
	),
	tar_target(
		calc_speed_new_burn,
		calc_speed(prep_speed_fire, 'dist_to_new_burn', seq(1, 20000, length.out = 100L))
	),
	tar_target(
		plot_speed_new_burn,
		plot_dist(calc_speed_new_burn, plot_theme()) +
			labs(x = 'Distance to new burn (m)', y = 'Speed (m/2hr)')
	),
	tar_target(
		calc_speed_old_burn,
		calc_speed(prep_speed_fire, 'dist_to_old_burn', seq(1, 20000, length.out = 100L))
	),
	tar_target(
		plot_speed_old_burn,
		plot_dist(calc_speed_old_burn, plot_theme()) +
			labs(x = 'Distance to old burn (m)', y = 'Speed (m/2hr)')
	),
	tar_target(
		fire_plots,
		save_plot(plot_speed_open_fire, "fire_model_speed_open",
							plot_speed_new_burn, "fire_model_speed_new_burn",
							plot_speed_old_burn, "fire_model_speed_old_burn")
	)
)

# Targets: speed from seasonal fire --------------------------
targets_speed_fire_seasonal <- c(

	tar_target(
		prep_speed_s_fire,
		prepare_speed_seasonal(
			DT = season_prep,
			summary = s_indiv_fire,
			model = "fire",
			params = dist_parameters,
			season_key = season_key
		),
		map(s_indiv_fire, season_key)
	),
	tar_target(
		calc_speed_open_s_fire,
		calc_speed_seasonal(prep_speed_s_fire, 'open', "fire", seq = 0:1, season_key),
		map(prep_speed_s_fire, season_key)
	),
	tar_target(
		plot_speed_open_s_fire,
		plot_box_seasonal_speed(calc_speed_open_s_fire, plot_theme(), "fire")
	),

	tar_target(
		calc_speed_s_new_fire,
		calc_speed_seasonal(prep_speed_s_fire, 'dist_to_new_burn', "fire", seq(1, 20000, length.out = 100L), season_key),
		map(prep_speed_s_fire, season_key)
	),
	tar_target(
		plot_speed_s_new_fire,
		plot_dist_seasonal(calc_speed_s_new_fire, plot_theme(), "new burn")
	),

	tar_target(
		calc_speed_s_old_fire,
		calc_speed_seasonal(prep_speed_s_fire, 'dist_to_old_burn', "fire", seq(1, 20000, length.out = 100L), season_key),
		map(prep_speed_s_fire, season_key)
	),
	tar_target(
		plot_speed_s_old_fire,
		plot_dist_seasonal(calc_speed_s_old_fire, plot_theme(), "old burn")
	)
)


## There is something funky going on with the speed estimates... not sure if that is indicative of an issue with the model itself? ----

# Targets: RSS from annual fire model -----------------------------------------------------------
targets_rss_fire <- c(
	tar_target(
		pred_h1_new_burn,
		predict_h1_new_burn(model_prep, fire_model)
	),
	tar_target(
		pred_h1_old_burn,
		predict_h1_old_burn(model_prep, fire_model)
	),
	tar_target(
		pred_h1_forest_fire,
		predict_h1_forest(model_prep, fire_model, "fire")
	),
	tar_target(
		pred_h2_fire,
		predict_h2(model_prep, fire_model, "fire")
	),
	tar_target(
		rss_forest_fire,
		calc_rss(pred_h1_forest_fire, 'h1_forest', pred_h2_fire, 'h2')
	),
	tar_target(
		rss_old_burn,
		calc_rss(pred_h1_old_burn, 'h1_old_burn', pred_h2_fire, 'h2')
	),
	tar_target(
		rss_new_burn,
		calc_rss(pred_h1_new_burn, 'h1_new_burn', pred_h2_fire, 'h2')
	),
	tar_target(
		plot_rss_forest_fire,
		plot_rss(rss_forest_fire, plot_theme()) +
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
		fire_rss_plots,
		save_rss_plot(plot_rss_forest_fire, "rss_forest_fire-model",
									plot_rss_old_burn, "rss_dist_to_old_burn",
									plot_rss_new_burn, "rss_dist_to_new_burn")
	)
)

# Targets: RSS from seasonal fire model -----------------------------------------------------------
targets_rss_fire_seasonal <- c(

	tar_target(
		pred_h1_forest_s_fire,
		predict_h1_forest_seasonal(season_prep, s_fire_model, "fire", season_key),
		pattern = map(s_fire_model, season_prep, season_key)
	),
	tar_target(
		pred_h1_s_new_burn,
		predict_h1_new_burn_seasonal(season_prep, s_fire_model, season_key),
		pattern = map(season_prep, s_fire_model, season_key)
	),
	tar_target(
		pred_h1_s_old_burn,
		predict_h1_old_burn_seasonal(season_prep, s_fire_model, season_key),
		pattern = map(season_prep, s_fire_model, season_key)
	),
	tar_target(
		pred_h2_s_fire,
		predict_h2_seasonal(season_prep, s_fire_model, "fire", season_key),
		pattern = map(season_prep, s_fire_model, season_key)
	),

	tar_target(
		rss_forest_s_fire,
		calc_rss_seasonal(pred_h1_forest_s_fire, 'h1_forest_s_fire', pred_h2_s_fire, 'h2_s_fire', season_key),
		map(pred_h1_forest_s_fire, season_key)
	),
	tar_target(
		rss_s_new_burn,
		calc_rss_seasonal(pred_h1_s_new_burn, 'h1_new_burn_s', pred_h2_s_fire, 'h2_s_fire', season_key),
		pattern = map(pred_h1_s_new_burn, season_key)
	),
	tar_target(
		rss_s_old_burn,
		calc_rss_seasonal(pred_h1_s_old_burn, 'h1_old_burn_s', pred_h2_s_fire, 'h2_s_fire', season_key),
		pattern = map(pred_h1_s_old_burn, season_key)
	),


	tar_target(
		plot_rss_forest_s_fire,
		plot_rss_seasonal_forest(rss_forest_s_fire, plot_theme(), "fire")
	),
	tar_target(
		plot_rss_s_new_burn,
		plot_rss_seasonal(rss_s_new_burn, plot_theme(), "new burns")
	),
	tar_target(
		plot_rss_s_old_burn,
		plot_rss_seasonal(rss_s_old_burn, plot_theme(), "old burns")
	)
)

# Roads models: This is idential to everything above, but now running through an annual & seasonally-mapped model looking at distances to the Trans Canada and minor roads, instead of old & new burns --------

# Targets: annual road model ----------------------------------------------------------
targets_road <- c(

	tar_target(
		road_model,
		model_roads_bin(model_prep)
	),
	tar_target(
		road_model_check,
		model_check(road_model)
	)
)

# Targets: seasonal road model ----------------------------------------
targets_road_seasonal <- c(

	tar_target(
		s_road_model,
		model_roads_bin(season_prep),
		map(season_prep)
	),
	tar_target(
		s_road_model_check,
		model_check(s_road_model),
		map(s_road_model)
	)
)

# Targets: road output and effects ------------------------------------------------------------
targets_road_effects <- c(
	tar_target(
		indiv_road,
		indiv_estimates(road_model)
	),
	tar_target(
		road_boxplot,
		plot_box_horiz(indiv_road, plot_theme(), "road")
	),
	tar_target(
		s_indiv_road,
		indiv_seasonal(s_road_model, season_key),
		pattern = map(s_road_model, season_key)
	),
	tar_target(
		s_road_boxplot,
		plot_boxplot_seasonal(s_indiv_road, plot_theme(), "road"),
		pattern = map(s_indiv_road)
	)
)

# Targets: speed from annual road model ------------------------------------------------------------
targets_speed_road <- c(
	tar_target(
		prep_speed_road,
		prepare_speed(
			DT = model_prep,
			summary = indiv_road,
			model = "road",
			params = dist_parameters
		)
	),
	tar_target(
		calc_speed_open_road,
		calc_speed(prep_speed_road, 'open road', seq = 0:1)
	),
	tar_target(
		plot_speed_open_road,
		plot_box(calc_speed_open_road, plot_theme()) +
			labs(x = 'Open', y = 'Speed (m/2hr)')
	),
	tar_target(
		calc_speed_road,
		calc_speed(prep_speed_road, 'dist_to_tch', seq(1, 20000, length.out = 100L))
	),
	tar_target(
		plot_speed_road,
		plot_dist(calc_speed_road, plot_theme()) +
			labs(x = 'Distance to TCH (m)', y = 'Speed (m/2hr)')
	),
# ^ this looks like a bunch of horizontal lines... bc it is. the slopes are x10^-8, the differences between individuals just absolutely swamp any effects
	tar_target(
		road_plots,
		save_plot(plot_speed_open_road, "road_model_speed_open",
							plot_speed_road, "road_model_speed_tch")
	)
)

# Targets: speed from seasonal roads --------------------------
targets_speed_road_seasonal <- c(

	tar_target(
		prep_speed_s_road,
		prepare_speed_seasonal(
			DT = season_prep,
			summary = s_indiv_road,
			model = "road",
			params = dist_parameters,
			season_key = season_key
		),
		map(s_indiv_road, season_key)
	),
	tar_target(
		calc_speed_open_s_road,
		calc_speed_seasonal(prep_speed_s_road, 'open', "road", seq = 0:1, season_key),
		map(prep_speed_s_road, season_key)
	),
	tar_target(
		plot_speed_open_s_road,
		plot_box_seasonal_speed(calc_speed_open_s_road, plot_theme(), "road")
	),

	tar_target(
		calc_speed_s_road,
		calc_speed_seasonal(prep_speed_s_road, 'dist_to_tch', "road", seq(1, 20000, length.out = 100L), season_key),
		map(prep_speed_s_road, season_key)
	),
	tar_target(
		plot_speed_s_road,
		plot_dist_seasonal(calc_speed_s_road, plot_theme(), "road")
	)
)


# Targets: RSS from annual road model -----------------------------------------------------------
targets_rss_road <- c(
	tar_target(
		pred_h1_tch,
		predict_h1_tch(model_prep, road_model)
	),
	tar_target(
		pred_h1_minor,
		predict_h1_minor(model_prep, road_model)
	),
	tar_target(
		pred_h1_forest_road,
		predict_h1_forest(model_prep, road_model, "road")
	),
	tar_target(
		pred_h2_road,
		predict_h2(model_prep, road_model, "road")
	),
	tar_target(
		rss_forest_road,
		calc_rss(pred_h1_forest_road, 'h1_forest', pred_h2_road, 'h2')
	),
	tar_target(
		rss_tch,
		calc_rss(pred_h1_tch, 'h1_tch', pred_h2_road, 'h2')
	),
	tar_target(
		rss_minor,
		calc_rss(pred_h1_minor, 'h1_minor', pred_h2_road, 'h2')
	),
	tar_target(
		plot_rss_forest_road,
		plot_rss(rss_forest_road, plot_theme()) +
			labs(x = 'Forest', y = 'logRSS',
					 title = 'RSS compared to 0 forest (road model)')
	),
	tar_target(
		plot_rss_tch,
		plot_rss(rss_tch, plot_theme()) +
			labs(x = 'Distance to TCH (m)', y = 'logRSS',
					 title = 'RSS compared to median distance from TCH')
	),
	tar_target(
		plot_rss_minor,
		plot_rss(rss_minor, plot_theme()) +
			labs(x = 'Distance to minor roads (m)', y = 'logRSS',
					 title = 'RSS compared to median distance from minor roads')
	),
	tar_target(
		road_rss_plots,
		save_rss_plot(plot_rss_forest_road, "rss_forest_road-model",
									plot_rss_tch, "rss_dist_to_tch",
									plot_rss_minor, "rss_dist_to_minor_roads")
	)
)


# Targets: RSS from seasonal road model -----------------------------------------------------------
targets_rss_road_seasonal <- c(

	tar_target(
		pred_h1_forest_s_road,
		predict_h1_forest_seasonal(season_prep, s_road_model, "road", season_key),
		pattern = map(s_road_model, season_prep, season_key)
	),
	tar_target(
		pred_h1_s_tch,
		predict_h1_tch_seasonal(season_prep, s_road_model, season_key),
		pattern = map(season_prep, s_road_model, season_key)
	),
	tar_target(
		pred_h1_s_minor,
		predict_h1_minor_seasonal(season_prep, s_road_model, season_key),
		pattern = map(season_prep, s_road_model, season_key)
	),
	tar_target(
		pred_h2_s_road,
		predict_h2_seasonal(season_prep, s_road_model, "road", season_key),
		pattern = map(season_prep, s_road_model, season_key)
	),

	tar_target(
		rss_forest_s_road,
		calc_rss_seasonal(pred_h1_forest_s_road, 'h1_forest_s_road', pred_h2_s_road, 'h2_s_road', season_key),
		map(pred_h1_forest_s_road, season_key)
	),
	tar_target(
		rss_s_tch,
		calc_rss_seasonal(pred_h1_s_tch, 'h1_tch', pred_h2_s_road, 'h2_s_road', season_key),
		pattern = map(pred_h1_s_tch, season_key)
	),
	tar_target(
		rss_s_minor,
		calc_rss_seasonal(pred_h1_s_minor, 'h1_minor', pred_h2_s_road, 'h2_s_road', season_key),
		pattern = map(pred_h1_s_minor, season_key)
	),


	tar_target(
		plot_rss_forest_s_road,
		plot_rss_seasonal_forest(rss_forest_s_road, plot_theme(), "road")
	),
	tar_target(
		plot_rss_s_tch,
		plot_rss_seasonal(rss_s_tch, plot_theme(), "TCH")
	),
	tar_target(
		plot_rss_s_minor,
		plot_rss_seasonal(rss_s_minor, plot_theme(), "minor roads")
	)
)

# Targets: all ------------------------------------------------------------
# Automatically grab and combine all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
