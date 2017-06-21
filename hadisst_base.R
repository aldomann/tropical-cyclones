# Base code to study the SST data from the Hadley Centre (HadISST)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(tidyverse)
library(stringr)
library(raster)
library(rasterVis)
library(scales)
library(lubridate)

# SST manipulation functions -------------------------------

load_hadsst <- function(file = "./HadISST_sst.nc") {
	b <- brick(file)
	NAvalue(b) <- -32768 # Land
	return(b)
}

# Transform basin coordinates into numbers
morph_coords <- function(coords){
	coords[1] <- ifelse(str_extract(coords[1], "[A-Z]") == "W",
										 - as.numeric(str_extract(coords[1], "[^A-Z]+")),
										   as.numeric(str_extract(coords[1], "[^A-Z]+")) )
	coords[2] <- ifelse(str_extract(coords[2], "[A-Z]") == "W",
										 - as.numeric(str_extract(coords[2], "[^A-Z]+")),
										   as.numeric(str_extract(coords[2], "[^A-Z]+")) )
	coords[3] <- ifelse(str_extract(coords[3], "[A-Z]") == "S",
										 - as.numeric(str_extract(coords[3], "[^A-Z]+")),
										   as.numeric(str_extract(coords[3], "[^A-Z]+")) )
	coords[4] <- ifelse(str_extract(coords[4], "[A-Z]") == "S",
										 - as.numeric(str_extract(coords[4], "[^A-Z]+")),
										   as.numeric(str_extract(coords[4], "[^A-Z]+")) )
	return(coords)
}

# Get mean SSTs data frame filtering by spatial and temporal window of activity
get_mean_ssts <- function(x = hadsst.raster, years, range = 6:10,
													coords = c("180W", "180E", "90S", "90N")){
	coords <- morph_coords(coords)
	area <- extent(as.numeric(coords))
	nms <- names(x)
	x <- crop(x, area)

	months <- c("01","02","03","04","05","06","07","08","09","10","11","12")
	xMeans <- vector(length = length(years), mode = 'list')
	for (ix in 1:length(years)){
		xMeans[[ix]] <- mean(x[[c(sapply(range,function(x) grep(paste0(years[ix],'.',months[x]),nms)))]], na.rm = T)
	}
	mean.brick <- do.call(brick,xMeans)
	mean.brick <- lapply(1:nlayers(mean.brick),function(ix) mean(as.matrix(mean.brick[[ix]]), na.rm = T))

	mean.df <- unlist(mean.brick)
	mean.df <- data.frame(sst = mean.df)
	mean.df <- normalise_ssts(mean.df, years)
	return(mean.df)
}

# Normalise SSTs and divide by class
normalise_ssts <- function(data.df, years){
	mean.sst <- mean(data.df$sst)
	data.df <- data.df %>%
		mutate(year = as.numeric(substring(rownames(data.df), 1)) + years[1] - 1,
					 year = ymd(paste(year, "01", "01", sep = "-")),
					 sst.norm = sst/mean.sst,
					 sst.class = ifelse(sst.norm >= 1, "high", "low"))
	data.df <- data.df[c("year", "sst", "sst.norm", "sst.class")]
	return(data.df)
}

get_low_years <- function(data.df) {
	low.years <- year(data.df[data.df$sst.class == "low", ]$year)
	return(low.years)
}

get_high_years <- function(data.df) {
	low.years <- year(data.df[data.df$sst.class == "high", ]$year)
	return(low.years)
}

# Data visualisation functions -----------------------------

# SST map of a single time layer
map_global_sst <- function(x = hadsst.raster, month, year){
	time.layer = (year - 1870) * 12 + month
	gplot(raster(hadsst.raster, layer = time.layer)) +
		geom_tile(aes(fill = value)) +
		scale_fill_gradientn(colours = c("#760200", "#b10e00", "#ec3b00", "#f9a100", "#d3ed0a", "#88ec6a", "#6cd2a8", "#4ca6e8", "#3276fb", "#214FBB", "#010546"),
												 values = rescale(c(35, 30, 25, 20, 15, 10, 7.5, 5, 2.5, 0, -1000)),
												 breaks=c(35, 30, 25, 20, 15, 10, 5,  0, -5, -1000),
												 labels=c(35, 30, 25, 20, 15, 10, 5,  0, -5, "Ice"),
												 guide="legend", na.value = "white") +
		labs(fill = "SST") +
		theme(plot.margin=margin(c(0,10,0,0)),
					legend.margin=margin(c(1,1,5,-15)),
					axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank())
}

# Plot SST time series
# library(extrafont)
# font_import(paths = "~/TTF") # Only once
plot_annual_sst <- function(data.df){
	title <- attr(data.df, "title")
	years.str <- paste0(year(data.df$year[1]), "-", year(data.df$year[length(data.df$year)]))

	ggplot(data.df, aes(x = year, y = sst.norm)) +
		geom_line(aes(linetype = "Annual"), colour = "black") +
		geom_hline(aes(yintercept = 1, linetype = "Mean"), colour = "blueviolet") +
		scale_linetype_manual(values = c("solid", "twodash")) +
		geom_point(aes(colour = sst.class)) +
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		labs(title = paste0(title, " SST between ", years.str),
				 x = "Time (year)", y = "SST/⟨SST⟩",
				 linetype = "SST", colour = "SST Class") +
		guides(linetype = guide_legend(override.aes = list(colour = c("black", "blueviolet"))))
}

plot_annual_sst_alt <- function(data.df){
	mean.sst <- mean(data.df$sst)
	mean.sd.sst <- sd(data.df$sst)

	title <- attr(data.df, "title")
	years.str <- paste0(year(data.df$year[1]), "-", year(data.df$year[length(data.df$year)]))

	ggplot(data.df, aes(x = year, y = sst)) +
		annotate("rect", fill = "blueviolet", alpha = 0.1,
						 xmin = as.Date(-Inf, origin = data.df$year[1]),
						 xmax = as.Date(Inf, origin = data.df$year[1]),
						 ymin = mean.sst - mean.sd.sst, ymax = mean.sst + mean.sd.sst) +
		geom_line(aes(linetype = "Annual"), colour = "black") +
		geom_hline(aes(yintercept = mean.sst, linetype = "Mean"), colour = "blueviolet") +
		scale_linetype_manual(values = c("solid", "twodash")) +
		geom_point(aes(colour = sst.class)) +
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		labs(title = paste0(title, " SST between ", years.str),
				 x = "Time (year)", y = "SST/⟨SST⟩",
				 linetype = "SST", colour = "SST Class") +
		guides(linetype = guide_legend(override.aes = list(colour = c("black", "blueviolet"))))
}
