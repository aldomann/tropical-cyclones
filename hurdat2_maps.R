# Code to track storms in a map
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

source("hurdat2_base.R")

# Get hurricanes data frames
hurr.natl.obs <- get_hurr_obs("hurdat2-1851-2016-apr2017.txt")
# hurr.epac.obs <- get_hurr_obs("hurdat2-nepac-1949-2016-apr2017.txt")
# hurr.all.obs <- rbind(hurr.natl.obs, hurr.epac.obs)

# Data visualisation ---------------------------------------

# US East States Map Data
east.states <- c("florida", "georgia", "south carolina", "north carolina", "virginia", "maryland",
								 "delaware", "new jersey", "new york", "connecticut", "massachusetts",
								 "rhode island", "vermont", "new hampshire", "maine", "pennsylvania", "west virginia",
								 "tennessee", "kentucky", "alabama", "arkansas", "texas", "mississippi",  "louisiana")
east.us <- map_data("state", region = east.states)

# ggplot(east.us, aes(x = long, y = lat, group = group)) +
# 	geom_polygon(fill = "cornsilk", color = "cornsilk") +
# 	theme_void() +
# 	xlim(c(-108, -65)) + ylim(c(23, 48)) +
# 	geom_path(data = hurr.natl.obs, aes(x = long.num, y = lat.num, group = storm.id),
# 						color = "red", alpha = 0.2, size = 0.2) +
# 	facet_wrap(~ decade)

# Function to track a single hurricane
map_track <- function(storm, year, map.data = east.us, hurr.obs = hurr.natl.obs){
	to.plot <- hurr.obs %>%
		filter(storm.name == toupper(storm) & storm.year == year)

	out <- ggplot(map.data, aes(x = long, y = lat, group = group)) +
		geom_polygon(fill = "cornsilk") +
		theme_void() +
		xlim(c(-108, -65)) + ylim(c(23, 48)) +
		geom_path(data = to.plot,
							aes(x = long.num, y = lat.num, group = NULL)) +
		geom_point(data = to.plot,
							 aes(x = long.num, y = lat.num, group = NULL,
							 		color = status, size = wind), alpha = 0.5)

	return(out)
}

map_track(storm = "Katrina", year = "2005")
