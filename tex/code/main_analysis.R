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
hurr.natl.pdi <- get_pdis(hurr.natl.obs) %>%
	filter(storm.id != "AL171988")
attr(hurr.natl.pdi, "title") <- "N. Atl."
hurr.epac.pdi <- get_pdis(hurr.epac.obs) %>%
	filter(storm.id != "EP231989")
attr(hurr.epac.pdi, "title") <- "E. Pac."
hurr.all.pdi <- get_pdis(hurr.all.obs)

# Create SST data frames -----------------------------------

hadsst.raster <- load_hadsst(file = "data/HadISST_sst.nc")

# Windows of activity
years.natl <- 1966:2016
coords.natl <- c("90W", "20E", "5N", "25N")
coords.natl.map <- c("100W", "20E", "0N", "60N")

years.epac <- 1966:2016
coords.epac <- c("120W", "90W", "5N", "20N")
coords.epac.map <- c("160W", "90W", "5N", "35N")

# Construct SST data frames
ssts.natl <- get_mean_ssts(years = years.natl, coords = coords.natl)
attr(ssts.natl, "title") <- "N. Atl."

ssts.epac <- get_mean_ssts(years = years.epac, coords = coords.epac)
attr(ssts.epac, "title") <- "E. Pac."

# Get list of low & high SST years
get_low_years(ssts.natl)
get_high_years(ssts.natl)
get_low_years(ssts.epac)
get_high_years(ssts.epac)

# Get number of years per SST class
table(ssts.natl$sst.class)
table(ssts.epac$sst.class)

# Get number of storms per SST class
length((hurr.natl.pdi %>% filter(storm.year %in% get_high_years(ssts.natl)))$storm.pdi)
length((hurr.natl.pdi %>% filter(storm.year %in% get_low_years(ssts.natl)))$storm.pdi)
length((hurr.epac.pdi %>% filter(storm.year %in% get_high_years(ssts.epac)))$storm.pdi)
length((hurr.epac.pdi %>% filter(storm.year %in% get_low_years(ssts.epac)))$storm.pdi)

# Basins maps ----------------------------------------------

# Maps of the basins (full)
map_region_hurrs(hurr.natl.obs, years.natl, coords.natl.map, coords.natl, steps = c(20, 10), xtra.lims = c(3,2))cairo_pdf)
map_region_hurrs(hurr.epac.obs, years.epac, coords.epac.map, coords.epac, steps = c(10, 10), xtra.lims = c(3,2))cairo_pdf)

# SST map of a raster layer
map_global_sst(hadsst.raster, 12, 2015)

# Plot SSTs and PDIs ---------------------------------------

# Plot annual SSTs
plot_annual_sst(ssts.natl)
plot_annual_sst(ssts.epac)

# PDI time series
plot_pdi_tempseries(hurr.natl.pdi, ssts.natl)
plot_pdi_tempseries(hurr.epac.pdi, ssts.epac)

# DPDI plots
plot_dpdi(hurr.natl.pdi, years.natl)
plot_dpdi(hurr.epac.pdi, years.epac)

# DPDI plots by SST class
plot_dpdi_by_sst_class(hurr.natl.pdi, ssts.natl)
plot_dpdi_by_sst_class(hurr.epac.pdi, ssts.epac)

# Scatterplots analysis ------------------------------------

# PDI scatterplots (duration)
plot_pdi_scatter_by_status(hurr.natl.pdi, ssts.natl, "ds")
plot_pdi_scatter_by_status(hurr.natl.pdi, ssts.natl, "nds")
plot_pdi_scatter_by_status(hurr.epac.pdi, ssts.epac, "ds")
plot_pdi_scatter_by_status(hurr.epac.pdi, ssts.epac, "nds")

# PDI scatterplots (wind)
plot_pdi_scatter_wind(hurr.natl.pdi, ssts.natl)
plot_pdi_scatter_wind(hurr.epac.pdi, ssts.epac)



