# === Targets: iSSA workflow ----------------------------------------------
# Alec L. Robitaille, Julie W. Turner



# Source ------------------------------------------------------------------
targets::tar_source('R')



# Options -----------------------------------------------------------------
tar_option_set(format = 'qs')



# Data --------------------------------------------------------------------
# Path to fisher locs data
locs_path <- file.path('input', 'TNNP_ALL_Caribou.csv')

# Path to land covers
lc_path <- file.path('input', '2024-03-12_Hermosilla_2019_land_cover.tif')
legend_path <- file.path('input', 'cfs_legend.csv')

# Alternative landcover from NALCMS, but CFS is better I think
# nalcms_path <- file.path('input', '2024-03-12_NALCMS_30_m.tif')
# nalcms_legend_path <- file.path('input', 'nalcms_legend.csv')

# Path to burns
burn_path <- file.path('input', 'Burn_Areas.gpkg')

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

#	tar_file_read(
#		nalcms_lc,
#		nalcms_path,
#		raster(!!.x)
#	),

#	tar_file_read(
#		nalcms_legend,
#		nalcms_legend_path,
#		fread(!!.x)
#	),

	tar_file_read(
		burn,
		burn_path,
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
	)#,

	#tar_target(
#		old_burn,
#		split_burn(burn_prep, "old")
#	),
#	tar_target(
#		new_burn,
#		split_burn(burn_prep, "new")
#	)
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
		avail_lc,
		calc_availability(tracks_extract, 'lc_description', 'proportion', split_by)
	)#,

#	tar_target(
#		burn_points,
#		generate_burn_pts(burn_prep)
#	),

#	tar_target(
#		extract_burn,
#		extract_burn_lc(burn_points, crs, lc, legend)
#	)
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




# Targets: model ----------------------------------------------------------
targets_model <- c(
	tar_target(
		model_prep,
		prepare_model(tracks_extract)
	),
#	tar_target(
#		model_lc,
#		model_land_cover(model_prep)
#	),
	tar_target(
		model_forest,
		model_forest_bin(model_prep)
	),
#	tar_target(
#		model_check_lc,
#		model_check(model_lc)
#	),
	tar_target(
		model_check_forest,
		model_check(model_forest)
	),

	tar_target(
		current_burn_lc,
		burn_lc_status(extract_burn))
)

# Targets: output and effects ------------------------------------------------------------
targets_effects <- c(
	tar_target(
		indiv_summary,
		indiv_estimates(model_forest)
	),
	tar_target(
		plot_boxplot,
		plot_box_horiz(indiv_summary, plot_theme())
	)
)

# Targets: speed ------------------------------------------------------------
targets_speed <- c(
	tar_target(
		prep_speed,
		prepare_speed(
			DT = model_prep,
			summary = indiv_summary,
			params = dist_parameters
		)
	),
	tar_target(
		calc_speed_open,
		calc_speed(prep_speed, 'open', seq = 0:1)
	),
	tar_target(
		plot_speed_open,
		plot_box(calc_speed_open, plot_theme()) +
			labs(x = 'Closed vs open', y = 'Speed (m/2hr)')
	),

	tar_target(
		calc_speed_burn,
		calc_speed(prep_speed, 'dist_to_new_burn', seq(1, 27000, length.out = 100L))
	),
	tar_target(
		plot_speed_burn,
		plot_dist(calc_speed_burn, plot_theme()) +
			labs(x = 'Distance to new burn (m)', y = 'Speed (m/2hr)')
	)

)

# Targets: RSS ------------------------------------------------------------
targets_rss <- c(
	tar_target(
		pred_h1_burn,
		predict_h1_burn(model_prep, model_forest)
	),
	tar_target(
		pred_h1_forest,
		predict_h1_forest(model_prep, model_forest)
	),
	tar_target(
		pred_h2,
		predict_h2(model_prep, model_forest)
	),
	tar_target(
		rss_forest,
		calc_rss(pred_h1_forest, 'h1_forest', pred_h2, 'h2')
	),
	tar_target(
		rss_burn,
		calc_rss(pred_h1_burn, 'h1_burn', pred_h2, 'h2')
	),
	tar_target(
		plot_rss_forest,
		plot_rss(rss_forest, plot_theme()) +
			labs(x = 'Forest', y = 'logRSS',
					 title = 'RSS compared to 0 forest')
	),
	tar_target(
		plot_rss_burn,
		plot_rss(rss_burn, plot_theme()) +
			labs(x = 'Distance to new burn (m)', y = 'logRSS',
					 title = 'RSS compared to mean distance from burn')
	)
)



# Targets: all ------------------------------------------------------------
# Automatically grab and combine all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
