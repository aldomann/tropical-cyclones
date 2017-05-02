# Code to study the PDI probability density (DPDI)
# Author: Alfredo Hernández

source("hurdat2_base.R")
library(measurements) # Convert units
library(scales) # To show exponents

# Calculate the PDI ----------------------------------------

# Create data frame with PDI and year of the storm
hurr_obs_pdi <- hurr_obs %>%
	group_by(storm_id, storm_name, n_obs) %>%
	summarise(storm_pdi = sum(conv_unit(wind, "knot", "m_per_sec")^3 * conv_unit(6, "hr", "sec"))) %>%
	mutate(storm_year = substring(storm_id, 5, 9)) %>%
	filter(storm_pdi != "NA") %>%
	filter(storm_pdi != 0)

# Get PDI of a single storm by name
get_pdi <- function(storm, year){
	wanted_pdi_df <-hurr_obs_pdi %>%
		filter(storm_name == toupper(storm)) %>%
		filter(storm_year == year)
	wanted_pdi_df$storm_pdi
}

# Get PDI of a single storm by id
get_pdi_by_id <- function(id){
	wanted_pdi_df <-hurr_obs_pdi %>%
		filter(storm_id == id)
	wanted_pdi_df$storm_pdi
}

# Calculate the DPDI ---------------------------------------

# Function to calculate the DPDI in a range of years
get_dpdi <- function(min_year, max_year){
	hurr_obs_pdi <- hurr_obs_pdi %>%
		filter(storm_year %in% min_year:max_year)

	c <- 10^(1/4)
	m <- min(hurr_obs_pdi$storm_pdi)

	dpdi_df <- data.frame()
	for (i in 1:20) {
		dpdi_df <- rbind(dpdi_df, c(c^(i-1)*m, c^(i)*m))
	}
	colnames(dpdi_df) <- c("pdi_min", "pdi_max")

	dpdi_df <- dpdi_df %>%
		mutate(pdi_star = sqrt(pdi_min * pdi_max)) %>%
		mutate(pdi_bin = pdi_max - pdi_min) %>%
		rowwise() %>%
		mutate(ndpdi = sum(hurr_obs_pdi$storm_pdi >= pdi_min & hurr_obs_pdi$storm_pdi <= pdi_max)) %>%
		mutate(dpdi = ndpdi/(length(hurr_obs_pdi$storm_pdi)*pdi_bin)) %>%
		filter(dpdi != 0)

	return(dpdi_df)
}

# Function to plot the DPDI data frame
plot_dpdi <- function(min_year, max_year){
	dpdi_df <- get_dpdi(min_year, max_year)
	ggplot(dpdi_df) +
		geom_line(aes(x = pdi_star, y = dpdi), linetype="dotted") +
		geom_point(aes(x = pdi_star, y = dpdi)) +
		scale_x_log10() +
		scale_y_log10() +
		labs(title = paste0("PDI probability density for ", min_year, "-", max_year),
				 x = "PDI (m^3/s^2)",
				 y = "D(PDI) (s^2/m^3)")
}

# Track a storm --------------------------------------------

# Track of a single storm by name
track_storm <- function(storm, year){
	ggplot(hurr_obs %>%
				 	filter(storm_name == toupper(storm)) %>%
				 	filter(storm_year == year)) +
		geom_line(aes(x = date_time, y = wind), linetype="dotted") +
		geom_point(aes(x = date_time, y = wind, colour = status)) +
		labs(title = paste0(storm, " profile ", "(", year, ")",
												", PDI = ", scientific(get_pdi(storm, year), digits = 3), " m^3/s^2"),
				 x = "Time (days)",
				 y = "Velocity (kt)",
				 colour = "Status")
}

# Track of a single storm by id
track_storm_by_id <- function(id){
	wanted_storm <-hurr_obs %>%
		filter(storm_id == id)
	storm <- wanted_storm$storm_name
	year <- wanted_storm$storm_year
	ggplot(wanted_storm)+
		geom_line(aes(x = date_time, y = wind), linetype="dotted") +
		geom_point(aes(x = date_time, y = wind, colour = status)) +
		labs(title = paste0(storm, " profile ", "(", year, ")",
												", PDI = ", scientific(get_pdi_by_id(id), digits = 3), " m^3/s^2"),
				 x = "Time (days)",
				 y = "Velocity (kt)",
				 colour = "Status")
}

# Data visualisation -------------------------------------------------

# dpdi_54_07 <- get_dpdi(1954, 2007)
plot_dpdi(1954, 2007)

get_pdi("katrina", "2005")
track_storm("Katrina", "2005")
# track_storm("Betsy", "1956")
# track_storm_by_id("AL191976")
