# Base code to study the SST data from the Hadley Centre (HadISST)
# Author: Alfredo Hernández

library(raster) # Crop raster data
library(tidyverse)
library(lubridate)

# hadsstR functions ----------------------------------------

load_hadsst <- function(file = "./HadISST_sst.nc") {
	b <- brick(file)
	NAvalue(b) <- -32768 # Land
	# NAvalue(b) <- -1000 # Ice
	return(b)
}

hadSSTmean <- function(x, years, first.range = 9:12, second.range = 1:4){
	nms <- names(x)
	mts <- c("01","02","03","04","05","06","07","08","09","10","11","12")
	xMeans <- vector(length = length(years)-1,mode='list')
	for (ix in seq_along(years[2:length(years)])){
		xMeans[[ix]] <- mean(x[[c(sapply(first.range,function(x) grep(paste0(years[ix-1],'.',mts[x]),nms)),sapply(second.range,function(x) grep(paste0(years[ix],'.',mts[x]),nms)))]], na.rm = T)
	}
	mean_brick <- do.call(brick,xMeans)
	return(mean_brick)
}

# Read and clean raw data ----------------------------------

hadsst.raster <- load_hadsst(file = "/home/aldomann/Downloads/Hadley/HadISST_sst.nc")
annual.ssts.raster <- hadSSTmean(hadsst.raster, 1969:1973)

annual.ssts.df <- data.frame( t( rasterToPoints(annual.ssts.raster) ) )
annual.ssts.df <- data.frame(sst = rowMeans(annual.ssts.df[-(1:2),]))
mean.sst <- mean(annual.ssts.df$sst)
annual.ssts.df <- annual.ssts.df %>%
	mutate(year = as.numeric(substring(rownames(annual.ssts.df), 7)) + 1900, # This should read the starting year
				 year = ymd(paste(year, "01", "01", sep="-")),
				 sst.norm = sst/mean.sst)
annual.ssts.df <- annual.ssts.df[c("year", "sst", "sst.norm")]
