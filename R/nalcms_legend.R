# NALCMS / 2020 Canada Land Cover
nalcms_legend <- data.table::data.table(
	class = seq(0, 19),
	label = c(
	'Mask',
	'Temperate or sub-polar needleleaf forest',
	'Sub-polar taiga needleleaf forest',
	'Tropical or sub-tropical broadleaf evergreen forest',
	'Tropical or sub-tropical broadleaf deciduous forest',
	'Temperate or sub-polar broadleaf deciduous forest',
	'Mixed forest',
	'Tropical or sub-tropical shrubland',
	'Temperate or sub-polar shrubland',
	'Tropical or sub-tropical grassland',
	'Temperate or sub-polar grassland',
	'Sub-polar or polar shrubland-lichen-moss',
	'Sub-polar or polar grassland-lichen-moss',
	'Sub-polar or polar barren-lichen-moss',
	'Wetland',
	'Cropland',
	'Barren land',
	'Urban and built-up',
	'Water',
	'Snow and ice'
))
