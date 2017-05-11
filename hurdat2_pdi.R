# Code to study the PDI probability density (DPDI)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

source("hurdat2_pdi_base.R")

# Calculate the PDI ----------------------------------------

# Create data frame with PDI and year of the storm
hurr.natl.pdi <- get_pdi_df(hurr.natl.obs)
hurr.epac.pdi <- get_pdi_df(hurr.epac.obs)
hurr.all.pdi <- get_pdi_df(hurr.all.obs)

# Data visualisation -------------------------------------------------

test.dpdi <- get_dpdi(hurr.natl.pdi, 2004:2007)
plot_dpdi(hurr.all.pdi, 2004:2007)

get_pdi(hurr.natl.pdi, "katrina", 2005)
track_storm(hurr.natl.obs, "Katrina", 2005)
track_storm(hurr.natl.obs, "Betsy", "1956")
track_storm_by_id(hurr.natl.obs, "AL191976")
