# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("hadisst_base.R")
source("hurdat2_pdi_base.R")
source("analysis_base.R")

# Create PDI data frame ------------------------------------

# Get hurricanes data frames
hurr.natl.obs <- get_hurr_obs("hurdat2-1851-2016-apr2017.txt")
hurr.epac.obs <- get_hurr_obs("hurdat2-nepac-1949-2016-apr2017.txt")
hurr.all.obs <- rbind(hurr.natl.obs, hurr.epac.obs)

# Create data frame with PDI and year of the storm
hurr.natl.pdi <- get_pdis(hurr.natl.obs)
hurr.epac.pdi <- get_pdis(hurr.epac.obs)
hurr.all.pdi <- get_pdis(hurr.all.obs)

# Create SST data frames -----------------------------------

hadsst.raster <- load_hadsst(file = "data/HadISST_sst.nc")

# Windows of activity
years.natl <- 1966:2016
coords.natl <- c("90W", "20E", "5N", "25N")
coords.natl.map <- c("100W", "20E", "10N", "60N")

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
table(ssts.natl$sst.class)
table(ssts.epac$sst.class)

# Data visualisation ---------------------------------------

plot_annual_sst(ssts.natl)
plot_annual_sst(ssts.epac)
# plot_annual_sst(ssts.epac, save = T, pdf = T, lmodern = T)

# DPDI plots by SST class ----------------------------------

plot_dpdi_by_sst_class(hurr.natl.pdi, ssts.natl)
plot_dpdi_by_sst_class(hurr.epac.pdi, ssts.epac)

map_region_hurrs(hurr.natl.obs, years.natl, coords.natl.map, steps = c(20, 10), xtra.lims = c(3,2))
map_region_hurrs(hurr.epac.obs, years.epac, coords.epac)

# New analysis ---------------------------------------------

# PDI scatterplots
plot_pdi_scatter <- function(hurr.pdi, ssts){
	hurr.high.pdi <- hurr.pdi %>% filter(storm.year %in% get_high_years(ssts))
	hurr.low.pdi <- hurr.pdi %>% filter(storm.year %in% get_low_years(ssts))
	years.str <- paste0(year(ssts$year[1]), "-", year(ssts$year[length(ssts$year)]))

	ggplot() +
		aes(x = conv_unit(storm.duration, "sec", "hr"), y = storm.pdi) +
		geom_point(data = hurr.high.pdi, aes(colour = "high"), size = 0.3) +
		geom_smooth(data = hurr.high.pdi, aes(colour = "high"), method = glm, size = 0.4) +
		geom_point(data = hurr.low.pdi, aes(colour = "low"), size = 0.3) +
		geom_smooth(data = hurr.low.pdi, aes(colour = "low"), method = glm, size = 0.4) +
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		labs(title = paste0("PDI Scatterplot", " (", attr(ssts, "title"), "; ", years.str ,")"),
				 x = "Storm duration (h)", y = "PDI (m^3/s^2)", colour = "SST Class")
}

plot_pdi_scatter(hurr.natl.pdi, ssts.natl)
plot_pdi_scatter(hurr.epac.pdi, ssts.epac)

# Max wind scatterplots
plot_maxwind_scatter <- function(hurr.pdi, ssts){
	hurr.high.pdi <- hurr.pdi %>% filter(storm.year %in% get_high_years(ssts))
	hurr.low.pdi <- hurr.pdi %>% filter(storm.year %in% get_low_years(ssts))
	years.str <- paste0(year(ssts$year[1]), "-", year(ssts$year[length(ssts$year)]))

	ggplot() +
		aes(x = conv_unit(storm.duration, "sec", "hr"), y = max.wind) +
		geom_point(data = hurr.high.pdi, aes(colour = "high"), size = 0.3) +
		geom_smooth(data = hurr.high.pdi, aes(colour = "high"), method = glm, size = 0.4) +
		geom_point(data = hurr.low.pdi, aes(colour = "low"), size = 0.3) +
		geom_smooth(data = hurr.low.pdi, aes(colour = "low"), method = glm, size = 0.4) +
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		labs(title = paste0("Wind Scatterplot", " (", attr(ssts, "title"), "; ", years.str ,")"),
				 x = "Storm duration (h)", y = "Max wind (knot)", colour = "SST Class")
}

plot_maxwind_scatter(hurr.natl.pdi, ssts.natl)
plot_maxwind_scatter(hurr.epac.pdi, ssts.epac)
