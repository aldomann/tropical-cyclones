# Base code to study the PDI probability density (DPDI)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

source("hurdat2_base.R")
library(measurements) # Convert units
library(scales) # To show exponents

# Calculate the PDI ----------------------------------------

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

# Calculate and plot the DPDI ------------------------------

# Function to calculate the DPDI in a range of years
get_dpdi <- function(years){
	hurr_obs_pdi <- hurr_obs_pdi %>%
		filter(storm_year %in% years)

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
plot_dpdi <- function(years){
	dpdi_df <- get_dpdi(years)
	ggplot(dpdi_df) +
		geom_line(aes(x = pdi_star, y = dpdi), linetype="dotted") +
		geom_point(aes(x = pdi_star, y = dpdi)) +
		scale_x_log10() +
		scale_y_log10() +
		labs(title = paste0("PDI probability density for ", years[1], "-", years[length(years)]),
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
