# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("hadisst_base.R")
source("hurdat2_pdi_base.R")

hadsst.raster <- load_hadsst(file = "/home/aldomann/Downloads/Hadley/HadISST_sst.nc")

# Functions ------------------------------------------------

# Function to plot the DPDI data frame
plot_dpdi_by_sst_class <- function(hurr.obs.pdi, ssts.df){
	years <- year(ssts.df$year[1]):year(ssts.df$year[length(ssts.df$year)])

	high.years <- get_high_years(ssts.df)
	low.years <- get_low_years(ssts.df)
	dpdi.high.df <- get_dpdi(hurr.obs.pdi, high.years)
	dpdi.low.df <- get_dpdi(hurr.obs.pdi, low.years)

	ggplot() +
		aes(x = pdi.star, y = dpdi, ymin = dpdi-pdi.error, ymax = dpdi+pdi.error) +
		geom_line(data = dpdi.high.df, aes(colour = "high"), linetype = "dotted") +
		geom_point(data = dpdi.high.df, aes(colour = "high")) +
		geom_errorbar(data = dpdi.high.df, aes(colour = "high"), width = 0.1) +
		geom_line(data = dpdi.low.df, aes(colour = "low"), linetype = "dotted") +
		geom_point(data = dpdi.low.df, aes(colour = "low")) +
		geom_errorbar(data = dpdi.low.df, aes(colour = "low"), width = 0.1) +
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
hurr.natl.pdi <- get_pdi_df(hurr.natl.obs)
hurr.epac.pdi <- get_pdi_df(hurr.epac.obs)
hurr.all.pdi <- get_pdi_df(hurr.all.obs)

# Create SST data frames -----------------------------------

# Windows of activity
years.natl <- 1966:2016
coords.natl <- c("90W", "20E", "5N", "25N")

years.epac <- 1966:2016
coords.epac <- c("120W", "90W", "5N", "20N")

# Construct SST data frames
ssts.natl <- get_mean_ssts(years = years.natl, coords = coords.natl)
attr(ssts.natl, "title") <- "N. Atl."

ssts.epac <- get_mean_ssts(years = years.epac, coords = coords.epac)
attr(ssts.epac, "title") <- "E. Pac."

# Create vector of low & high SST years
# years.low.natl <- get_low_years(ssts.natl)
# years.high.natl <- get_high_years(ssts.natl)
#
# years.low.epac <- get_low_years(ssts.natl)
# years.high.epac <- get_high_years(ssts.natl)

# Get number of years per SST class
# table(ssts.natl$sst.class)
# table(ssts.epac$sst.class)

# Data visualisation ---------------------------------------

plot_annual_sst(ssts.natl, save = T)
plot_annual_sst(ssts.epac, save = T)
# plot_annual_sst(ssts.epac, save = T, pdf = T, lmodern = T)

# DPDI plots by SST class ----------------------------------

plot_dpdi_by_sst_class(hurr.natl.pdi, ssts.natl)
plot_dpdi_by_sst_class(hurr.epac.pdi, ssts.epac)
