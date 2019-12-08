# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("hadisst_base.R")

hadsst.raster <- load_hadsst(file = "data/HadISST_sst.nc")

# Create SST data frames -----------------------------------

# Windows of activity
natl.years <- 1966:2007
natl.range <- 6:10
natl.coords <- c("90W", "20E", "5N", "25N")

epac.years <- 1966:2007
epac.range <- 6:10
epac.coords <- c("120W", "90W", "5N", "20N")

# Construct SST data frames
natl.ssts.df <- get_mean_ssts(years = natl.years, range = natl.range, coords = natl.coords)
attr(natl.ssts.df, "title") <- "N. Atl."

epac.ssts.df <- get_mean_ssts(years = epac.years, range = epac.range, coords = epac.coords)
attr(epac.ssts.df, "title") <- "E. Pac."

# Get number of years per SST class
# table(natl.ssts.df$sst.class)
# table(epac.ssts.df$sst.class)

# Create vector of low & high SST years
natl.low.years <- get_low_years(natl.ssts.df)
natl.high.years <- get_high_years(natl.ssts.df)

epac.low.years <- get_low_years(natl.ssts.df)
epac.high.years <- get_high_years(natl.ssts.df)

# Data visualisation ---------------------------------------

plot_annual_sst(natl.ssts.df)
plot_annual_sst(epac.ssts.df)
# plot_annual_sst(epac.ssts.df, save = T, pdf = T, lmodern = T)


