# Code to track storms in a map
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

source("hurdat2_base.R")

# Data visualisation -------------------------------------------------

# US East States Map Data
east_states <- c("florida", "georgia", "south carolina",
								 "north carolina", "virginia", "maryland",
								 "delaware", "new jersey", "new york",
								 "connecticut", "massachusetts",
								 "rhode island", "vermont", "new hampshire",
								 "maine", "pennsylvania", "west virginia",
								 "tennessee", "kentucky", "alabama",
								 "arkansas", "texas", "mississippi",
								 "louisiana")
east_us <- map_data("state", region = east_states)
world_map <- map_data("world2", region = ".")

ggplot(world_map, aes(x = long, y = lat, group = group)) +
	geom_polygon(fill = "cornsilk", color = "cornsilk") +
	theme_void() +
	xlim(c(-108, -65)) + ylim(c(23, 48)) +
	geom_path(data = hurr_obs,
						aes(x = -longitude, y = latitude,
								group = storm_id),
						color = "red", alpha = 0.2, size = 0.2) +
	# geom_path(data = filter(hurr_obs, cat_5),
	# 					aes(x = -longitude, y = latitude,
	# 							group = storm_id),
	# 					color = "red") +
	facet_wrap(~ decade)


# Function to track a single hurricane
map_track <- function(storm, year, map_data = east_us, hurr_data = hurr_obs){
	to_plot <- hurr_obs %>%
		filter(storm_name == toupper(storm) & storm_year == year)
	out <- ggplot(east_us, aes(x = long, y = lat, group = group)) +
		geom_polygon(fill = "cornsilk") +
		theme_void() +
		xlim(c(-108, -65)) + ylim(c(23, 48)) +
		geom_path(data = to_plot,
							aes(x = -longitude, y = latitude, group = NULL)) +
		geom_point(data = to_plot,
							 aes(x = -longitude, y = latitude, group = NULL,
							 		color = status, size = wind), alpha = 0.5)
	return(out)
}

map_track(storm = "Katrina", year = "2005")

# Empty map
world_map <- map_data("world", region = ".")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
	geom_polygon(fill = "blue", color = "cornsilk") +
	theme_void() +
	xlim(c(-8, 180)) + ylim(c(-90, 90))
