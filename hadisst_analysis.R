# library(devtools)
# install_github("jebyrnes/hadsstR")
library(raster)
library(hadsstr)
library(tidyverse)
library(lubridate)

# Read raw data --------------------------------------------

# Open file
hadsst.raster <- load_hadsst(file = "/home/aldomann/Downloads/Hadley/HadISST_sst.nc")

# Get annual and mean SSTs:
startYear <- 1931;
endYear <- 2007;
annual.ssts.raster <- get_annual_ssts(hadsst.raster, years = startYear:endYear)
mean.sst.raster <- get_average_sst(annual.ssts.raster, years = startYear:endYear)

# Crop SST data to a certain region:
startLat <- -60;
endLat <- 60;
startLon <- -40;
endLon <- 40;
annual.ssts.raster <- crop(annual.ssts.raster, extent(startLat, endLat, startLon, endLon))
mean.sst.raster <- crop(mean.sst.raster, extent(startLat, endLat, startLon, endLon))

# Get mean SST of the whole region:
mean.sst.df <- data.frame( rasterToPoints( mean.sst.raster) )
mean.sst <- mean(mean.sst.df$layer)

# Get annual SST mean of the whole region:
annual.ssts.df <- data.frame( t( rasterToPoints( annual.ssts.raster) ) )
annual.ssts.df <- data.frame(mean.sst = rowMeans(annual.ssts.df[-(1:2),]))
rownames(annual.ssts.df) <- substring(rownames(annual.ssts.df), 2)
annual.ssts.df <- tibble::rownames_to_column(annual.ssts.df, var = "year")
annual.ssts.df$year <- ymd(paste(annual.ssts.df$year, "06", "01", sep="-"))
annual.ssts.df$mean.sst.norm <- (annual.ssts.df$mean.sst)/mean.sst


# Data visualisation ---------------------------------------

# Plot the result using ggplot:
ggplot(annual.ssts.df) +
	labs(title = paste0("HadISST between ", startYear, "-", endYear ),
			 x = "Time (year)", y = "SST/<SST>") +
 	geom_line(mapping = aes(x = year, y = mean.sst.norm), colour = "red")
