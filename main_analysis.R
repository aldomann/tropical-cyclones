# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("hadisst_base.R")
source("hurdat2_pdi_base.R")

hadsst.raster <- load_hadsst(file = "/home/aldomann/Downloads/Hadley/HadISST_sst.nc")


# Functions ------------------------------------------------

# Function to plot the DPDI data frame
plot_dpdi_by_sst_class <- function(ssts.df){
	years <- year(ssts.df$year[1]):year(ssts.df$year[length(ssts.df$year)])

	high.years <- get_high_years(ssts.df)
	low.years <- get_low_years(ssts.df)
	dpdi.high.df <- get_dpdi(high.years)
	dpdi.low.df <- get_dpdi(low.years)

	ggplot() +
		geom_line(data = dpdi.high.df, aes(x = pdi.star, y = dpdi, colour = "high"), linetype = "dotted") +
		geom_point(data = dpdi.high.df, aes(x = pdi.star, y = dpdi, colour = "high")) +
		geom_line(data = dpdi.low.df, aes(x = pdi.star, y = dpdi, colour = "low"), linetype = "dotted") +
		geom_point(data = dpdi.low.df, aes(x = pdi.star, y = dpdi, colour = "low")) +
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		scale_x_log10() +
		scale_y_log10() +
		labs(title = paste0("PDI probability density for ",
												years[1], "-", years[length(years)],
												" (", attr(ssts.df, "title"), ")"),
				 x = "PDI (m^3/s^2)", y = "D(PDI) (s^2/m^3)", colour = "SST Class")
}

# Create PDI data frame ------------------------------------

# Create data frame with PDI and year of the storm
hurr.obs.pdi <- hurr.obs %>%
	group_by(storm.id, storm.name, n.obs) %>%
	summarise(storm.pdi = sum(conv_unit(wind, "knot", "m_per_sec")^3 * conv_unit(6, "hr", "sec"))) %>%
	mutate(storm.year = substring(storm.id, 5, 9)) %>%
	filter(storm.pdi != "NA") %>%
	filter(storm.pdi != 0)

# Create SST data frames -----------------------------------

# Windows of activity
natl.years <- 1966:2007
natl.range <- 6:10
natl.coords <- c("90W", "20E", "5N", "25N")

wpac.years <- 1966:2007
wpac.range <- 5:12
wpac.coords <- c("120E", "180E", "5N", "20N")

epac.years <- 1966:2007
epac.range <- 6:10
epac.coords <- c("120W", "90W", "5N", "20N")

spac.years <- 1966:2007
spac.first.range <- 12
spac.second.range <- 1:4
spac.coords <- c("155E", "180E", "20S", "5S")

nio.years <- 1966:2007
nio.range <- as.integer(c(4,5,9,10,11))
nio.coords <- c("55E", "90E", "5N", "20N")

sio.years <- 1966:2007
sio.first.range <- 11:12
sio.second.range <- 1:4
sio.coords <- c("50E", "115E", "20S", "5S")

# Construct SST data frames
natl.ssts.df <- get_mean_ssts(years = natl.years, range = natl.range, coords = natl.coords)
attr(natl.ssts.df, "title") <- "N. Atl."

wpac.ssts.df <- get_mean_ssts(years = wpac.years, range = wpac.range, coords = wpac.coords)
attr(wpac.ssts.df, "title") <- "W. Pac."

epac.ssts.df <- get_mean_ssts(years = epac.years, range = epac.range, coords = epac.coords)
attr(epac.ssts.df, "title") <- "E. Pac."

spac.ssts.df <- get_mean_ssts2(years = spac.years, first.range = spac.first.range,
															 second.range = spac.second.range, coords = spac.coords)
attr(spac.ssts.df, "title") <- "S.W. Pac."

nio.ssts.df <- get_mean_ssts(years = nio.years, range = nio.range, coords = nio.coords)
attr(nio.ssts.df, "title") <- "N. Ind."

sio.ssts.df <- get_mean_ssts2(years = sio.years, first.range = sio.first.range,
															second.range = sio.second.range, coords = sio.coords)
attr(sio.ssts.df, "title") <- "S. Ind."

# Get number of years per SST class
# table(natl.ssts.df$sst.class)
# table(epac.ssts.df$sst.class)

# Data visualisation ---------------------------------------

plot_annual_sst(natl.ssts.df)
# plot_annual_sst(wpac.ssts.df)
# plot_annual_sst(epac.ssts.df)
# plot_annual_sst(spac.ssts.df)
# plot_annual_sst(nio.ssts.df)
# plot_annual_sst(sio.ssts.df, save = T, pdf = T, lmodern = T)

# DPDI plots by SST class ----------------------------------

plot_dpdi_by_sst_class(natl.ssts.df)
