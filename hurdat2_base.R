# Base code to study the PDI of hurricane data from the National Hurricane Center (HURDAT)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(tidyverse)
library(stringr) # To split lines
library(lubridate) # Use dates

# Read and split raw data --------------------------------------------

# tracks.url <- paste0("http://www.nhc.noaa.gov/data/hurdat/", "hurdat2-1851-2015-070616.txt")
tracks.file <- paste0("hurdat2-1851-2015-070616.txt")
hurr.tracks <- readLines(tracks.file)

hurr.tracks <- lapply(hurr.tracks, str_split, pattern = ",", simplify = TRUE)

# Clean the raw data -------------------------------------------------

# Split the hurr.tracks into meta and observation lists
hurr.lengths <- sapply(hurr.tracks, length)
hurr.meta <- hurr.tracks[hurr.lengths == 4]
hurr.obs <- hurr.tracks[hurr.lengths == 21]

rm(hurr.tracks) # Clean memory

# Create and clean meta data frame
hurr.meta <- lapply(hurr.meta, tibble::as_tibble)
hurr.meta <- bind_rows(hurr.meta)

hurr.meta <- hurr.meta %>%
	dplyr::select(-V4) %>%
	rename(storm.id = V1, storm.name = V2, n.obs = V3) %>%
	mutate(storm.name = str_trim(storm.name),
				 n.obs = as.numeric(n.obs))

storm.id <- rep(hurr.meta$storm.id, times = hurr.meta$n.obs)

# Create and clean obs data frame
hurr.obs <- lapply(hurr.obs, tibble::as_tibble)
hurr.obs <- bind_rows(hurr.obs) %>%
	mutate(storm.id = storm.id) %>%
	dplyr::select(storm.id, V1, V2, V4:V7) %>%
	rename(date = V1, time = V2, status = V4, latitude = V5, longitude = V6, wind = V7)

# Change date and time & unite them
hurr.obs <- hurr.obs %>%
	unite(date.time, date, time) %>%
	mutate(date.time = ymd_hm(date.time)) %>%
	mutate(decade = substring(year(date.time), 1, 3),
				 decade = paste0(decade, "0s"))

# Meaningful status names
storm.levels <- c("TD", "TS", "HU", "EX", "SD", "SS", "LO", "WV", "DB")
storm.labels <- c("Tropical depression", "Tropical storm", "Hurricane",
									"Extratropical cyclone", "Subtropical depression", "Subtropical storm",
									"Other low", "Tropical wave", "Disturbance")
hurr.obs <- hurr.obs %>%
	mutate(status = factor(str_trim(status),
												 levels = storm.levels,
												 labels = storm.labels))

# Split the numeric latitude from the direction of that latitude
hurr.obs <- hurr.obs %>%
	mutate(lat.dir = str_extract(latitude, "[A-Z]"),
				 latitude = as.numeric(str_extract(latitude, "[^A-Z]+")),
				 lon.dir = str_extract(longitude, "[A-Z]"),
				 longitude = as.numeric(str_extract(longitude, "[^A-Z]+")))

# Clean up wind column -------------------------------------

# Clean and reformat the wind column
hurr.obs <- hurr.obs %>%
	mutate(wind = ifelse(wind == " -99", NA, as.numeric(wind)))

# Manually change odd middle value for AL191976
hurr.obs <- hurr.obs %>%
	mutate(wind = ifelse(storm.id == "AL191976" & is.na(wind), 20, wind)) %>%
	filter(is.na(wind) != TRUE)

# Add useful info to hurr.obs data frame --------------------

# Add category 5 hurricanes boolean
# hurr.obs <- hurr.obs %>%
# 	group_by(storm.id) %>%
# 	mutate(cat.5 = max(wind) >= 137) %>%
# 	ungroup()

# Add storm.name and storm.year to hurr.obs
hurr.obs <- hurr.obs %>%
	left_join(hurr.meta, by = "storm.id") %>%
	mutate(storm.year = year(date.time))

# Ignore data outside the delta.t = 6 hours
hurr.obs <- hurr.obs %>%
	filter(hour(date.time) == 00 |
				 hour(date.time) == 06 |
				 hour(date.time) == 12 |
				 hour(date.time) == 18) %>%
	filter(minute(date.time) == 00)

# Recalculate n.obs
hurr.obs <- hurr.obs %>%
	group_by(storm.id) %>%
	mutate(n.obs = length(wind))

# Rearrange hurr.obs data frame columns
# 	"delta.t" after "date.time"
# 	"cat.5" after "wind"
hurr.obs <- hurr.obs[c("storm.id", "storm.name", "n.obs", "date.time", "status",
											 "latitude", "lat.dir", "longitude", "lon.dir",
											 "wind", "storm.year", "decade")]

# Remove unneeded variables
rm(hurr.meta)
rm(hurr.lengths)
rm(storm.id)
rm(storm.labels)
rm(storm.levels)
rm(tracks.file)
