# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández

# Source base code -----------------------------------------

# source("hadisst_base_test.R")
source("hadisst_base.R")

# Create data frames ---------------------------------------
hadsst.raster <- load_hadsst(file = "/home/aldomann/Downloads/Hadley/HadISST_sst.nc")

# start.year <- 1966; end.year <- 2007;
natl.years <- 1966:2007
natl.coords <- c("90W", "20E", "5N", "25N")

annual.ssts.df <- get_mean_ssts(years = natl.years, range = 6:10, coords = natl.coords)
# annual.ssts.df <- get_mean_ssts2(years = start.year:end.year, first.range = 12, second.range = 1:4, coords = natl.coords)

# Data visualisation ---------------------------------------

# Plot time series
plot_annual_sst <- function(data_df){
	ggplot(data_df) +
	 	geom_line(aes(x = year, y = sst.norm, linetype = "Annual"), colour = "black") +
		geom_hline(aes(yintercept=1, linetype = "Mean"), colour = "blueviolet") +
		scale_linetype_manual(values = c("solid", "twodash")) +
		geom_point(aes(x = year, y = sst.norm, colour = sst.class)) +
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		labs(title = paste0("N. Atl. SST between ",
												year(annual.ssts.df$year[1]), "-",
												year(annual.ssts.df$year[length(annual.ssts.df$year)]) ),
				 x = "Time (year)", y = "SST/⟨SST⟩",
				 linetype = "SST", colour = "SST Class") +
		guides(linetype = guide_legend(override.aes=list(colour = c("black", "blueviolet"))))
}

plot_annual_sst(annual.ssts.df)
table(annual.ssts.df$sst.class)
