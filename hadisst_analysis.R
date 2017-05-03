# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández

# Set region and range of years before sourcing ------------

start.year <- 1966; end.year <- 2007;
start.lat <- "20E"; end.lat <- "90E";
start.lon <- "5N"; end.lon <- "25N";
basin.coords <- c(start.lat, end.lat, start.lon, end.lon)

morph_coord <- function(coords){
	coords[1] = ifelse(str_extract(coords[1], "[A-Z]") == "W", - as.numeric(str_extract(coords[1], "[^A-Z]+")), as.numeric(str_extract(coords[1], "[^A-Z]+")) )
	coords[2] = ifelse(str_extract(coords[2], "[A-Z]") == "W", - as.numeric(str_extract(coords[2], "[^A-Z]+")), as.numeric(str_extract(coords[2], "[^A-Z]+")) )
	coords[3] = ifelse(str_extract(coords[3], "[A-Z]") == "S", - as.numeric(str_extract(coords[3], "[^A-Z]+")), as.numeric(str_extract(coords[3], "[^A-Z]+")) )
	coords[4] = ifelse(str_extract(coords[4], "[A-Z]") == "S", - as.numeric(str_extract(coords[2], "[^A-Z]+")), as.numeric(str_extract(coords[4], "[^A-Z]+")) )
	return(coords)
}

basin.coords <- morph_coord(basin.coords)
start.lat <- as.numeric(basin.coords[1]); end.lat <- as.numeric(basin.coords[2]);
start.lon <- as.numeric(basin.coords[3]); end.lon <- as.numeric(basin.coords[4]);

# Source base code -----------------------------------------

source("hadisst_base.R")

# Data visualisation ---------------------------------------

# Plot time series
plot_annual_sst <- function(data_df){
	ggplot(data_df) +
	 	geom_line(aes(x = year, y = sst.norm, linetype = "Annual"), colour = "black") +
		geom_hline(aes(yintercept=1, linetype = "Mean"), colour = "blueviolet") +
		scale_linetype_manual(values = c("solid", "twodash")) +
		geom_point(aes(x = year, y = sst.norm, colour = sst.class)) +
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		labs(title = paste0("N. Atl. SST between ", start.year, "-", end.year ),
				 x = "Time (year)", y = "SST/⟨SST⟩",
				 linetype = "SST", colour = "SST Class") +
		guides(linetype = guide_legend(override.aes=list(colour = c("black", "blueviolet"))))
}

plot_annual_sst(annual.ssts.df)
