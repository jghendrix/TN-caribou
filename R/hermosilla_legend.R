# Hermosilla, T., Wulder, M.A., White, J.C., Coops, N.C., 2022. Land cover
#  classification in an era of big and open data: Optimizing localized
#  implementation and training data selection to improve mapping outcomes.
#  Remote Sensing of Environment. No. 112780.
hermosilla_legend <- data.table::data.table(
	class = c(0, 20, 31, 32, 33, 40, 50, 80, 81, 100, 210, 220, 230),
	label = c(
		'Unclassified',
		'Water',
		'Snow/Ice',
		'Rock/Rubble',
		'Exposed/Barren Land',
		'Bryoids',
		'Shrubs',
		'Wetland',
		'Wetland Treed',
		'Herbs',
		'Coniferous',
		'Broad Leaf',
		'Mixedwood'
	))
