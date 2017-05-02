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
startYear <- 1966;
endYear <- 2007;
annual.ssts.raster <- get_annual_ssts(hadsst.raster, years = startYear:endYear)
mean.sst.raster <- get_average_sst(annual.ssts.raster, years = startYear:endYear)

# Crop SST data to a certain region:
startLat <- -105;
endLat <- 20;
startLon <- 0;
endLon <- 60;
annual.ssts.raster <- crop(annual.ssts.raster, extent(startLat, endLat, startLon, endLon))
mean.sst.raster <- crop(mean.sst.raster, extent(startLat, endLat, startLon, endLon))

# Get mean SST of the whole region:
mean.sst.df <- data.frame( rasterToPoints( mean.sst.raster) )
mean.sst <- mean(mean.sst.df$layer)

# Get annual SST mean of the whole region:
annual.ssts.df <- data.frame( t( rasterToPoints( annual.ssts.raster) ) )
annual.ssts.df <- data.frame(sst = rowMeans(annual.ssts.df[-(1:2),]))
annual.ssts.df <- annual.ssts.df %>%
	mutate(year = substring(rownames(annual.ssts.df), 2)) %>%
	mutate(year = ymd(paste(year, "01", "01", sep="-"))) %>%
	mutate(sst.norm = sst/mean.sst)
annual.ssts.df <- annual.ssts.df[c("year", "sst", "sst.norm")]

# Separation by SST:
annual.ssts.df <- annual.ssts.df %>%
	mutate(sst.class = ifelse(sst.norm >= 1, "hight", "low"))

# Data visualisation ---------------------------------------

# Plot time series using ggplot:
ggplot(annual.ssts.df) +
	labs(title = paste0("N. Atl. SST between ", startYear, "-", endYear ),
			 x = "Time (year)", y = "SST/⟨SST⟩") +
 	geom_line(aes(x = year, y = sst.norm), colour = "red") +
	geom_point(aes(x = year, y = sst.norm, colour = sst.class)) +
	geom_hline(aes(yintercept=1), color = "green")
