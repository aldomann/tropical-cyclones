# Base code to study the PDI of hurricane data from the National Hurricane Center (HURDAT)
# Author: Alfredo Hernández

library(tidyverse)
library(stringr) # To split lines
library(lubridate) # Use dates

# Read and split raw data --------------------------------------------

# tracks_url <- paste0("http://www.nhc.noaa.gov/data/hurdat/", "hurdat2-1851-2015-070616.txt")
tracks_file <- paste0("hurdat2-1851-2015-070616.txt")
hurr_tracks <- readLines(tracks_file)

hurr_tracks <- lapply(hurr_tracks, str_split,
											pattern = ",",
											simplify = TRUE)

# Clean the raw data -------------------------------------------------

# Split the hurr_tracks into meta and observation lists
hurr_lengths <- sapply(hurr_tracks, length)
hurr_meta <- hurr_tracks[hurr_lengths == 4]
hurr_obs <- hurr_tracks[hurr_lengths == 21]

rm(hurr_tracks) # Clean memory

# Create and clean meta data frame
hurr_meta <- lapply(hurr_meta, tibble::as_tibble)
hurr_meta <- bind_rows(hurr_meta)

hurr_meta <- hurr_meta %>%
	select(-V4) %>%
	rename(storm_id = V1, storm_name = V2, n_obs = V3) %>%
	mutate(storm_name = str_trim(storm_name),
								n_obs = as.numeric(n_obs))

storm_id <- rep(hurr_meta$storm_id, times = hurr_meta$n_obs)

# Create and clean obs data frame
hurr_obs <- lapply(hurr_obs, tibble::as_tibble)
hurr_obs <- bind_rows(hurr_obs) %>%
	mutate(storm_id = storm_id) %>%
	select(storm_id, V1, V2, V4:V7) %>%
	rename(date = V1, time = V2, status = V4, latitude = V5, longitude = V6, wind = V7)

# Change date and time & unite them
hurr_obs <- hurr_obs %>%
	unite(date_time, date, time) %>%
	mutate(date_time = ymd_hm(date_time)) %>%
	mutate(decade = substring(year(date_time), 1, 3),
								decade = paste0(decade, "0s"))

# Meaningful status names
storm_levels <- c("TD", "TS", "HU", "EX", "SD", "SS", "LO", "WV", "DB")
storm_labels <- c("Tropical depression", "Tropical storm", "Hurricane",
									"Extratropical cyclone", "Subtropical depression", "Subtropical storm",
									"Other low", "Tropical wave", "Disturbance")
hurr_obs <- hurr_obs %>%
	mutate(status = factor(str_trim(status),
																levels = storm_levels,
																labels = storm_labels))

# Split the numeric latitude from the direction of that latitude
hurr_obs <- hurr_obs %>%
	mutate(lat_dir = str_extract(latitude, "[A-Z]"),
				 latitude = as.numeric(str_extract(latitude, "[^A-Z]+")),
				 lon_dir = str_extract(longitude, "[A-Z]"),
				 longitude = as.numeric(str_extract(longitude, "[^A-Z]+")))

# Clean up wind column -------------------------------------

# Clean and reformat the wind column
hurr_obs <- hurr_obs %>%
	mutate(wind = ifelse(wind == " -99", NA, as.numeric(wind)))

# Manually change odd middle value for AL191976
hurr_obs <- hurr_obs %>%
	mutate(wind = ifelse(storm_id == "AL191976" & is.na(wind), 20, wind)) %>%
	filter(is.na(wind) != TRUE)

# Add useful info to hurr_obs data frame --------------------

# Add category 5 hurricanes boolean
# hurr_obs <- hurr_obs %>%
# 	group_by(storm_id) %>%
# 	mutate(cat_5 = max(wind) >= 137) %>%
# 	ungroup()

# Add storm_name and storm_year to hurr_obs
hurr_obs <- hurr_obs %>%
	left_join(hurr_meta, by = "storm_id") %>%
	mutate(storm_year = year(date_time))

# Ignore data outside the delta_t = 6 hours
hurr_obs <- hurr_obs %>%
	filter(hour(date_time) == 00 |
									hour(date_time) == 06 |
									hour(date_time) == 12 |
									hour(date_time) == 18) %>%
	filter(minute(date_time) == 00)

# Recalculate n_obs
hurr_obs <- hurr_obs %>%
	group_by(storm_id) %>%
	mutate(n_obs = length(wind))

# Rearrange hurr_obs data frame columns
# 	"delta_t" after "date_time"
# 	"cat_5" after "wind"
hurr_obs <- hurr_obs[c("storm_id", "storm_name", "n_obs", "date_time", "status",
											 "latitude", "lat_dir", "longitude", "lon_dir",
											 "wind", "storm_year", "decade")]
