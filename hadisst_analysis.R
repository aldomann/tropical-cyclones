# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("hadisst_base.R")

hadsst.raster <- load_hadsst(file = "/home/aldomann/Downloads/Hadley/HadISST_sst.nc")

# Create data frames ---------------------------------------

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
plot_annual_sst(wpac.ssts.df)
plot_annual_sst(epac.ssts.df)
plot_annual_sst(spac.ssts.df)
plot_annual_sst(nio.ssts.df)
plot_annual_sst(sio.ssts.df)
