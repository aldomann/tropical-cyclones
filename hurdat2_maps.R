# Code to track storms in a map
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

source("hurdat2_base.R")

# Data visualisation ---------------------------------------

# US East States Map Data
east.states <- c("florida", "georgia", "south carolina", "north carolina", "virginia", "maryland",
								 "delaware", "new jersey", "new york", "connecticut", "massachusetts",
								 "rhode island", "vermont", "new hampshire", "maine", "pennsylvania", "west virginia",
								 "tennessee", "kentucky", "alabama", "arkansas", "texas", "mississippi",  "louisiana")
east.us <- map_data("state", region = east.states)

ggplot(east.us, aes(x = long, y = lat, group = group)) +
	geom_polygon(fill = "cornsilk", color = "cornsilk") +
	theme_void() +
	xlim(c(-108, -65)) + ylim(c(23, 48)) +
	geom_path(data = hurr.obs, aes(x = -longitude, y = latitude, group = storm.id),
						color = "red", alpha = 0.2, size = 0.2) +
	facet_wrap(~ decade)

# Function to track a single hurricane
map_track <- function(storm, year, map_data = east.us, hurr_data = hurr.obs){
	to.plot <- hurr.obs %>%
		filter(storm.name == toupper(storm) & storm.year == year)
	out <- ggplot(east.us, aes(x = long, y = lat, group = group)) +
		geom_polygon(fill = "cornsilk") +
		theme_void() +
		xlim(c(-108, -65)) + ylim(c(23, 48)) +
		geom_path(data = to.plot,
							aes(x = -longitude, y = latitude, group = NULL)) +
		geom_point(data = to.plot,
							 aes(x = -longitude, y = latitude, group = NULL,
							 		color = status, size = wind), alpha = 0.5)
	return(out)
}

map_track(storm = "Katrina", year = "2005")


# Test maps ------------------------------------------------

# Running in dev mode
# devtools::dev_mode(on=T)
# # install_github("hadley/ggplot2")
# # install_github("hrbrmstr/ggalt")
#
# library(maps)
# library(ggplot2)
# library(ggalt)
#
# world_map <- map_data("world")
# world_map <- subset(world_map, region!="Antarctica")
#
# gg <- ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
# 	geom_cartogram(map = world_map, aes(map_id = region))
#
# gg + xlim(c(-180, -90)) + ylim(c(5, 25)) + coord_proj("+proj=merc") +
# 	geom_path(data = hurr.obs, aes(x = long.num, y = lat.num, group = storm.id), color = "red", alpha = 0.2, size = 0.2)
#
# devtools::dev_mode(on=F)
