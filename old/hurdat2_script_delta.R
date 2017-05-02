# This is code to replicate the analyses and figures from a paper.
# Code developed by Alfredo Hernández

# rm(list = ls())
library(tidyverse)
library(stringr) # To split lines
library(lubridate) # Use dates
library(measurements) # Convert units
library(scales) # To show exponents

# Read and split raw data --------------------------------------------

# tracks_url <- paste0("http://www.nhc.noaa.gov/data/hurdat/", "hurdat2-1851-2015-070616.txt")
tracks_file <- paste0("hurdat2-1851-2015-070616.txt")
# tracks_file <- paste0("hurdat2_small.txt")
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

# Clean meta dataframe
hurr_meta <- lapply(hurr_meta, tibble::as_tibble)
hurr_meta <- bind_rows(hurr_meta)

hurr_meta <- hurr_meta %>%
	dplyr::select(-V4) %>%
	dplyr::rename(storm_id = V1, storm_name = V2, n_obs = V3) %>%
	dplyr::mutate(storm_name = str_trim(storm_name),
				  n_obs = as.numeric(n_obs))

storm_id <- rep(hurr_meta$storm_id, times = hurr_meta$n_obs)

# Clean obs dataframe
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

# Add category 5 hurricanes boolean
hurr_obs <- hurr_obs %>%
	dplyr::group_by(storm_id) %>%
	dplyr::mutate(cat_5 = max(wind) >= 137) %>%
	dplyr::ungroup()

# Add storm_name and storm_year to hurr_obs
hurr_obs <- hurr_obs %>%
	dplyr::left_join(hurr_meta, by = "storm_id") %>%
	dplyr::mutate(storm_year = year(date_time))

# Calculate delta of time of hurr_obs
hurr_obs <- hurr_obs %>%
	group_by(storm_id) %>%
	arrange(date_time) %>%
	mutate(delta_t = date_time - lag(date_time, default=first(date_time)))

# Rearrange hurr_obs dataframe columns
# 	"delta_t" would go after "date_time"
hurr_obs <- hurr_obs[c("storm_id", "storm_name", "n_obs", 
											 "date_time", "delta_t", "status",
											 "latitude", "lat_dir", "longitude", "lon_dir",
											 "wind", "cat_5", "storm_year", "decade")]

# Create new useful dataframes -----------------------------

# Dataframe with named hurricanes only
# hurr_obs_named <- hurr_obs %>%
# 	dplyr::left_join(hurr_meta, by = "storm_id") %>%
# 	dplyr::filter(storm_name != "UNNAMED") %>%
# 	dplyr::mutate(storm_year = year(date_time)) %>%

# Dataframe with with hurricanes from a certain year range
# hurr_obs_2k <- hurr_obs %>%
# 	dplyr::left_join(hurr_meta, by = "storm_id") %>%
# 	dplyr::filter(year(date_time) >= 2000 & year(date_time) <= 2020) %>%
# 	dplyr::mutate(storm_year = year(date_time))

# Calculate the PDI --------------------------------------------------

# Create dataframe with PDI and year of the storm
hurr_obs_pdi <- hurr_obs %>% 
	group_by(storm_id, storm_name) %>% 
	summarize(storm_pdi = sum(conv_unit(wind, "knot", "m_per_sec")^3 * delta_t)) %>%
	dplyr::mutate(storm_year = substring(storm_id, 5, 9)) %>%
	dplyr::filter(storm_pdi != "NA") %>%
	dplyr::filter(storm_pdi != 0)

# Function to get PDI of a single storm
get_pdi <- function(storm, year){
	wanted_pdi_df <-hurr_obs_pdi %>% 
		dplyr::filter(storm_name == toupper(storm)) %>%
		dplyr::filter(storm_year == year)
	wanted_pdi_df$storm_pdi
}

# Create DPDI from a certain year range


# Data visualisation -------------------------------------------------

# Wind histogram
# ggplot(hurr_obs, aes(x = wind)) +
# 	geom_histogram(binwidth = 5)

# Storm PDI histogram
# ggplot(hurr_obs_pdi %>% 
# 			 	dplyr::filter(storm_year >= 1950 & storm_year <= 2020), aes(x = storm_pdi)) +
# 	geom_histogram(binwidth = 2.5e+8)

# Track of a single storm
track_storm <- function(storm, year){
	ggplot(hurr_obs %>% 
				 	dplyr::filter(storm_name == toupper(storm)) %>%
				 	dplyr::filter(storm_year == year)) + 
		geom_point(mapping = aes(x = date_time, y = wind, colour = status)) +
		labs(title = paste0(storm, " profile ", "(", year, ")", 
												", PDI = ", scientific(get_pdi(storm, year), digits = 3), " m^3/s^2"), 
				 x = "Time (days)", 
				 y = "Velocity (kt)",
				 colour = "Status")
}

track_storm("Katrina", "2005")
