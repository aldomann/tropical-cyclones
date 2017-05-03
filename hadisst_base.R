# Base code to study the SST data from the Hadley Centre (HadISST)
# Author: Alfredo Hernández

# library(devtools)
# install_github("jebyrnes/hadsstR")
library(hadsstr)
library(raster) # Crop raster data
library(tidyverse)
library(lubridate)

# Read and clean raw data ----------------------------------

# hadsst.url <- paste0("http://www.metoffice.gov.uk/hadobs/hadisst/data/", "HadISST_sst.nc.gzt")
hadsst.raster <- load_hadsst(file = "/home/aldomann/Downloads/Hadley/HadISST_sst.nc")

# Get annual and mean SSTs
# start.year <- 1966; end.year <- 2007;
annual.ssts.raster <- get_annual_ssts(hadsst.raster, years = start.year:end.year)
mean.sst.raster <- get_average_sst(annual.ssts.raster, years = start.year:end.year)

# Crop SST data to a certain region/basin
# start.lat <- 20; end.lat <- 90;
# start.lon <- 5; end.lon <- 25;
annual.ssts.raster <- crop(annual.ssts.raster, extent(start.lat, end.lat, start.lon, end.lon))
mean.sst.raster <- crop(mean.sst.raster, extent(start.lat, end.lat, start.lon, end.lon))

# Get mean SST of the whole basin
mean.sst.df <- data.frame( rasterToPoints( mean.sst.raster) )
mean.sst <- mean(mean.sst.df$layer)

# Create and clean data frames -----------------------------

# Get annual SST mean of the whole basin
annual.ssts.df <- data.frame( t( rasterToPoints( annual.ssts.raster) ) )
annual.ssts.df <- data.frame(sst = rowMeans(annual.ssts.df[-(1:2),]))
annual.ssts.df <- annual.ssts.df %>%
	mutate(year = substring(rownames(annual.ssts.df), 2),
				 year = ymd(paste(year, "01", "01", sep="-")),
				 sst.norm = sst/mean.sst)
annual.ssts.df <- annual.ssts.df[c("year", "sst", "sst.norm")]

# Separation by SST
annual.ssts.df <- annual.ssts.df %>%
	mutate(sst.class = ifelse(sst.norm >= 1, "hight", "low"))
