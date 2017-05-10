# Code to study the PDI probability density (DPDI)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

source("hurdat2_pdi_base.R")

# Calculate the PDI ----------------------------------------

# Create data frame with PDI and year of the storm
hurr.obs.pdi <- hurr.obs %>%
	group_by(storm.id, storm.name, n.obs) %>%
	summarise(storm.pdi = sum(conv_unit(wind, "knot", "m_per_sec")^3 * conv_unit(6, "hr", "sec"))) %>%
	mutate(storm.year = substring(storm.id, 5, 9)) %>%
	filter(storm.pdi != "NA") %>%
	filter(storm.pdi != 0)

# Data visualisation -------------------------------------------------

plot_dpdi(2004:2007)

# get_pdi("katrina", "2005")
track_storm("Katrina", "2005")
# track_storm("Betsy", "1956")
# track_storm_by_id("AL191976")
