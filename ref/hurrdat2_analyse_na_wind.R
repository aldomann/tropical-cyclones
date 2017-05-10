# Clean up wind column -------------------------------------

# Get storms_ids of storms with wind = -99
hurr_na_wind <- hurr.natl.obs %>%
	dplyr::filter(as.numeric(wind) == -99)

na_list <- c(unique(hurr_na_wind$storm.id))

for(i in 1:length(na_list)){
	hurr_new <- hurr.natl.obs %>%
		dplyr::filter(storm.id == as.character(na_list[i]))
	print(na_list[i])
	print(hurr_new$wind)
}
