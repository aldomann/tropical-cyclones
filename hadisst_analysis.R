# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández

# Set region and range of years before sourcing ------------

startYear <- 1931; endYear <- 2007;
startLat <- -105; endLat <- 20;
startLon <- 0; endLon <- 60;

source("hadisst_base.R")

# Data visualisation ---------------------------------------

# Plot time series
plot_annual_sst <- function(data_df){
	ggplot(data_df) +
	 	geom_line(aes(x = year, y = sst.norm), colour = "red") +
		geom_point(aes(x = year, y = sst.norm, colour = sst.class)) +
		geom_hline(aes(yintercept=1), color = "green") +
		labs(title = paste0("N. Atl. SST between ", startYear, "-", endYear ),
				 x = "Time (year)", y = "SST/⟨SST⟩", colour = "SST Class")
}

plot_annual_sst(annual.ssts.df)
