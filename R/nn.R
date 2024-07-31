
nn <- function(DT) {

# pilfered from Morgane's fission-fusion code, just want to see whether being in a dyad affects selection for burns or roads?
setDT(DT)
spatsoc::group_times(DT, datetime = 't_', threshold = '5 minutes')

	# Nearest neighbour -------------------------------------------------------
edges <-
	spatsoc::edge_nn(
		DT = DT,
		id = 'Animal_ID',
		coords = c('X', 'Y'),
		timegroup = 'timegroup',
		returnDist = TRUE,
		threshold = NULL
	)


# Threshold neighbours----------------------------------------------------
### If I care about neighbour identity, maybe this is worthwhile - otherwise ignore for now
# maxdist <- 500
# edges[distance > maxdist, NN := NA]

# Set dyad id
# dyad_id(edges, 'ID', 'NN')

# Merge -------------------------------------------------------------------
m <- left_join(
	DT,
	edges,
	by = c('Animal_ID' = 'ID', 'timegroup')
)

m %<>% mutate(in_group = ifelse(distance < 50, "dyad", "alone"))

return(m)

}
