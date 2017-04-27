# This is code to replicate the analyses and figures from a paper.
# Code developed by Alfredo Hernández

# rm(list = ls())
library(tidyverse)
library(stringr) # To split lines
library(lubridate) # Use dates
# library(dplyr) # Data manipulation
# library(tidyr) # Tidy dataframes
# library(ggplot2) # Fancy plots

# Read and split raw data --------------------------------------------

# tracks_url <- paste0("http://www.nhc.noaa.gov/data/hurdat/", "hurdat2-1851-2015-070616.txt")
tracks_file <- paste0("hurdat2-1851-2015-070616.txt")
hurr_tracks <- readLines(tracks_file)

hurr_tracks <- lapply(hurr_tracks, str_split,
					  pattern = ",",
					  simplify = TRUE)
length(hurr_tracks)

# Clean the raw data -------------------------------------------------

# Split the hurr_tracks into meta and observation lists
hurr_lengths <- sapply(hurr_tracks, length)
# unique(hurr_lengths) # To see the different lenghts
hurr_meta <- hurr_tracks[hurr_lengths == 4]
hurr_obs <- hurr_tracks[hurr_lengths == 21]

# Converting to dataframes 
hurr_meta <- lapply(hurr_meta, tibble::as_tibble)
hurr_meta <- bind_rows(hurr_meta)

hurr_meta <- hurr_meta %>%
	dplyr::select(-V4) %>%
	dplyr::rename(storm_id = V1, storm_name = V2, n_obs = V3) %>%
	dplyr::mutate(storm_name = str_trim(storm_name),
				  n_obs = as.numeric(n_obs))
hurr_meta %>% 
	slice(1:3)

storm_id <- rep(hurr_meta$storm_id, times = hurr_meta$n_obs)
head(storm_id, 3)

hurr_obs <- lapply(hurr_obs, tibble::as_tibble)
hurr_obs <- dplyr::bind_rows(hurr_obs) %>%
	dplyr::mutate(storm_id = storm_id) %>%
	dplyr::select(storm_id, V1, V2, V4:V7) %>%
	dplyr::rename(date = V1, time = V2, status = V4, latitude = V5, longitude = V6, wind = V7)

# Change date and time & unite them
hurr_obs <- hurr_obs %>%
	tidyr::unite(date_time, date, time) %>% 
	dplyr::mutate(date_time = ymd_hm(date_time)) %>%
	dplyr::mutate(decade = substring(year(date_time), 1, 3),
				  decade = paste0(decade, "0s"))

# Meaningful status names
storm_levels <- c("TD", "TS", "HU", "EX", "SD", "SS", "LO", "WV", "DB")
storm_labels <- c("Tropical depression", "Tropical storm", "Hurricane", 
				  "Extratropical cyclone", "Subtropical depression", "Subtropical storm", 
				  "Other low", "Tropical wave", "Disturbance")
hurr_obs <- hurr_obs %>%
	dplyr::mutate(status = factor(str_trim(status), 
								  levels = storm_levels, 
								  labels = storm_labels))

# Split the numeric latitude from the direction of that latitude
hurr_obs <- hurr_obs %>%
	dplyr::mutate(lat_dir = str_extract(latitude, "[A-Z]"),
				  latitude = as.numeric(str_extract(latitude, "[^A-Z]+")),
				  lon_dir = str_extract(longitude, "[A-Z]"),
				  longitude = as.numeric(str_extract(longitude, "[^A-Z]+")))

# Clean up the wind column
hurr_obs <- hurr_obs %>% 
	dplyr::mutate(wind = ifelse(wind == " -99", NA, as.numeric(wind)))


# Add useful info to hurr_obs dataframe --------------------

# Category 5 hurricanes
hurr_obs <- hurr_obs %>%
	dplyr::group_by(storm_id) %>%
	dplyr::mutate(cat_5 = max(wind) >= 137) %>%
	dplyr::ungroup()

# Create new useful dataframes -----------------------------

# Dataframe with named hurricanes only
hurr_obs_named <- hurr_obs %>%
	dplyr::left_join(hurr_meta, by = "storm_id") %>%
	dplyr::filter(storm_name != "UNNAMED") %>%
	dplyr::mutate(storm_year = year(date_time)) %>%
	
	# Dataframe with with hurricanes from a certain year range
	hurr_obs_2k <- hurr_obs %>%
	dplyr::left_join(hurr_meta, by = "storm_id") %>%
	dplyr::filter(year(date_time) >= 2000 & year(date_time) <= 2020) %>%
	dplyr::mutate(storm_year = year(date_time))

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

ggplot(east_us, aes(x = long, y = lat, group = group)) +
	geom_polygon(fill = "cornsilk", color = "cornsilk") +
	theme_void() +
	xlim(c(-108, -65)) + ylim(c(23, 48)) +
	geom_path(data = hurr_obs,
						aes(x = -longitude, y = latitude,
								group = storm_id),
						color = "red", alpha = 0.2, size = 0.2) +
	geom_path(data = filter(hurr_obs, cat_5),
						aes(x = -longitude, y = latitude,
								group = storm_id),
						color = "red") +
	facet_wrap(~ decade)

# Wind histogram
ggplot(hurr_obs, aes(x = wind)) +
	geom_histogram(binwidth = 5)

# Function to track a single hurricane
map_track <- function(storm, year, map_data = east_us,hurr_data = hurr_obs_named){
	to_plot <- hurr_obs_named %>%
		dplyr::filter(storm_name == toupper(storm) & storm_year == year)
	out <- ggplot(east_us, aes(x = long, y = lat,
							   group = group)) + 
		geom_polygon(fill = "cornsilk") + 
		theme_void() + 
		xlim(c(-108, -65)) + ylim(c(23, 48)) + 
		geom_path(data = to_plot,
				  aes(x = -longitude, y = latitude,
				  	group = NULL)) + 
		geom_point(data = to_plot,
				   aes(x = -longitude, y = latitude,
				   	group = NULL, color = status,
				   	size = wind), alpha = 0.5)
	return(out)
	}

map_track(storm = "Katrina", year = "2005")
