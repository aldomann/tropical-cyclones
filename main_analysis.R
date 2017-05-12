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

hadsst.raster <- load_hadsst(file = "/home/aldomann/Downloads/Hadley/HadISST_sst.nc")

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
table(ssts.natl$sst.class)
table(ssts.epac$sst.class)

# Data visualisation ---------------------------------------

plot_annual_sst(ssts.natl)
plot_annual_sst(ssts.epac)
# plot_annual_sst(ssts.epac, save = T, pdf = T, lmodern = T)

# DPDI plots by SST class ----------------------------------

plot_dpdi_by_sst_class(hurr.natl.pdi, ssts.natl)
plot_dpdi_by_sst_class(hurr.epac.pdi, ssts.epac)
