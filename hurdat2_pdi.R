# Code to study the PDI probability density (DPDI)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

source("hurdat2_pdi_base.R")

# Calculate the PDI ----------------------------------------

# Create data frame with PDI and year of the storm
hurr_obs_pdi <- hurr_obs %>%
	group_by(storm_id, storm_name, n_obs) %>%
	summarise(storm_pdi = sum(conv_unit(wind, "knot", "m_per_sec")^3 * conv_unit(6, "hr", "sec"))) %>%
	mutate(storm_year = substring(storm_id, 5, 9)) %>%
	filter(storm_pdi != "NA") %>%
	filter(storm_pdi != 0)

# Data visualisation -------------------------------------------------

plot_dpdi(2004:2007)

# get_pdi("katrina", "2005")
track_storm("Katrina", "2005")
# track_storm("Betsy", "1956")
# track_storm_by_id("AL191976")
