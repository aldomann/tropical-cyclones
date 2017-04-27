# Clean up wind column -------------------------------------

# Get storms_ids of storms with wind = -99
hurr_na_wind <- hurr_obs %>%
	dplyr::filter(as.numeric(wind) == -99)

na_list <- c(unique(hurr_na_wind$storm_id))

for(i in 1:length(na_list)){
	hurr_new <- hurr_obs %>%
		dplyr::filter(storm_id == as.character(na_list[i]))
	print(na_list[i])
	print(hurr_new$wind)
}