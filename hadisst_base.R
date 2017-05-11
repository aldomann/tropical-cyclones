# Base code to study the SST data from the Hadley Centre (HadISST)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(raster)
library(stringr)
library(tidyverse)
library(lubridate)

# Data manipulation functions ------------------------------

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
	aoi <- extent(as.numeric(coords))
	nms <- names(x)
	x <- crop(x, aoi)

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

# Plot time series
# library(extrafont)
# font_import(paths = "~/TTF") # Only once
plot_annual_sst <- function(data.df, save = F, pdf = F, lmodern = F){
	title <- attr(data.df, "title")
	years.str <- paste0(year(data.df$year[1]), "-", year(data.df$year[length(data.df$year)]))

	sst.plot <- ggplot(data.df, aes(x = year, y = sst.norm)) +
		geom_line(aes(linetype = "Annual"), colour = "black") +
		geom_hline(aes(yintercept = 1, linetype = "Mean"), colour = "blueviolet") +
		scale_linetype_manual(values = c("solid", "twodash")) +
		geom_point(aes(colour = sst.class)) +
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		labs(title = paste0(title, " SST between ", years.str),
				 x = "Time (year)", y = "SST/⟨SST⟩",
				 linetype = "SST", colour = "SST Class") +
		guides(linetype = guide_legend(override.aes = list(colour = c("black", "blueviolet"))))

	# Use Latin Modern Roman
	if (lmodern == T) {
		sst.plot <- sst.plot + theme(text = element_text(family = "LM Roman 10"))
	}
	# Save into image/PDF (optional)
	if (save == T) {
		save.title <- deparse(substitute(data.df))
		save.title <- substr(save.title, 6, nchar(save.title))
		save.title <- toupper(save.title)
		if (pdf == T) {
			ggsave(filename = paste0("SSTs", "-", save.title, "-", years.str, ".pdf"),
						 width = 7.813, height = 4.33, dpi = 96, device = cairo_pdf)
		} else {
			ggsave(filename = paste0("SSTs", "-", save.title, "-", years.str, ".png"),
						 width = 7.813, height = 4.33, dpi = 96)
		}
	}
	sst.plot
}
