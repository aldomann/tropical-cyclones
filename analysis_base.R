# Base code with misc functions needed in main_analysis
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(maps)
library(ggalt)

# Data visualisation functions -----------------------------

# Function to plot the DPDI data frame
plot_dpdi_by_sst_class <- function(hurr.obs.pdi, ssts.df){
	years <- year(ssts.df$year[1]):year(ssts.df$year[length(ssts.df$year)])

	high.years <- get_high_years(ssts.df)
	low.years <- get_low_years(ssts.df)
	dpdi.high.df <- get_dpdi(hurr.obs.pdi, high.years)
	dpdi.low.df <- get_dpdi(hurr.obs.pdi, low.years)

	ggplot() +
		aes(x = pdi.star, y = dpdi, ymin = dpdi-pdi.error, ymax = dpdi+pdi.error) +
		geom_line(data = dpdi.high.df, aes(colour = "high"), linetype = "dotted") +
		geom_point(data = dpdi.high.df, aes(colour = "high")) +
		geom_errorbar(data = dpdi.high.df, aes(colour = "high"), width = 0.1) +
		geom_line(data = dpdi.low.df, aes(colour = "low"), linetype = "dotted") +
		geom_point(data = dpdi.low.df, aes(colour = "low")) +
		geom_errorbar(data = dpdi.low.df, aes(colour = "low"), width = 0.1) +
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		scale_x_log10() +
		scale_y_log10() +
		labs(title = paste0("PDI probability density for ",
												years[1], "-", years[length(years)],
												" (", attr(ssts.df, "title"), ")"),
				 x = "PDI (m^3/s^2)", y = "D(PDI) (s^2/m^3)", colour = "SST Class") +
		theme(legend.position = c(0.92, 0.85))
}

# Map showing the hurricanes in specified window

# SRC: http://stackoverflow.com/questions/33302424/format-latitude-and-longitude-axis-labels-in-ggplot
scale_x_longitude <- function(xmin = -180, xmax = 180, step = 1, xtra.lim = 1.5, ...) {
	xbreaks <- seq(xmin,xmax,step)
	xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(-x,"^o", "*W")),
																											 ifelse(x > 0, parse(text=paste0(x,"^o", "*E")), x))))
	return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels,
														expand = c(0, 0), limits = c(xmin-xtra.lim, xmax+xtra.lim), ...))
}

scale_y_latitude <- function(ymin = -90, ymax = 90, step = 0.5, xtra.lim = 1.5, ...) {
	ybreaks <- seq(ymin,ymax,step)
	ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(-x,"^o", "*S")),
																											 ifelse(x > 0, parse(text=paste0(x,"^o", "*N")), x))))
	return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels,
														expand = c(0, 0), limits = c(ymin-xtra.lim, ymax+xtra.lim), ...))
}

# Install legacy version of ggalt (see https://github.com/hrbrmstr/ggalt/issues/33)
# devtools::install_github("rplzzz/ggalt", ref = "ggp221")
map_region_hurrs <- function(hurr.obs, years, coords, steps = c(5,5), xtra.lims = c(1.5,1.5)){
	# devtools::install_github("rplzzz/ggalt", ref = "ggp221")
	coords <- morph_coords(coords)
	hurr.obs <- hurr.obs %>%
		filter(storm.year %in% years)
	world_map <- map_data("world")
	world_map <- subset(world_map, region!="Antarctica")

	# title <- "asd"
	# years.str <- paste0(years[1], "-", years[length(years)])

	map <- ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
		geom_cartogram(map = world_map, aes(map_id = region)) +
		scale_x_longitude(xmin = as.numeric(coords[1]), xmax = as.numeric(coords[2]), step = steps[1], xtra.lim = xtra.lims[1]) +
		scale_y_latitude(ymin = as.numeric(coords[3]), ymax = as.numeric(coords[4]), step = steps[2], xtra.lim = xtra.lims[2]) +
		# coord_trans() +
		coord_proj("+proj=merc") +
		geom_path(data = hurr.obs, aes(x = long.num, y = lat.num, group = storm.id),
							color = "red", alpha = 0.2, size = 0.2)
		# + labs(title = paste0(title, " from ", years.str)
	return(map)
}

map_region_hurrs_full <- function(hurr.obs, years, coords, rect.coords, steps = c(5,5), xtra.lims = c(1.5,1.5)){
	# devtools::install_github("rplzzz/ggalt", ref = "ggp221")
	coords <- morph_coords(coords)
	rect.coords <- morph_coords(rect.coords)
	hurr.obs <- hurr.obs %>%
		filter(storm.year %in% years)
	world_map <- map_data("world")
	world_map <- subset(world_map, region!="Antarctica")

	# title <- "asd"
	# years.str <- paste0(years[1], "-", years[length(years)])

	map <- ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
		geom_cartogram(map = world_map, aes(map_id = region)) +
		scale_x_longitude(xmin = as.numeric(coords[1]), xmax = as.numeric(coords[2]), step = steps[1], xtra.lim = xtra.lims[1]) +
		scale_y_latitude(ymin = as.numeric(coords[3]), ymax = as.numeric(coords[4]), step = steps[2], xtra.lim = xtra.lims[2]) +
		# coord_trans() +
		coord_proj("+proj=merc") +
		geom_path(data = hurr.obs, aes(x = long.num, y = lat.num, group = storm.id),
							color = "red", alpha = 0.2, size = 0.2) +
		annotate("rect", xmin = as.integer(rect.coords[1]), xmax = as.integer(rect.coords[2]),
						 ymin = as.integer(rect.coords[3]), ymax = as.integer(rect.coords[4]),
						 color = "green", alpha = 0.2)
	# + labs(title = paste0(title, " from ", years.str)
	return(map)
}
