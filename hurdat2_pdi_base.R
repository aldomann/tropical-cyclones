# Base code to study the PDI probability density (DPDI)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

source("hurdat2_base.R")
library(measurements) # Convert units
library(scales) # To show exponents

# Calculate the PDI ----------------------------------------

# Get PDI of a single storm by name
get_pdi <- function(storm, year){
	wanted.pdi.df <- hurr.obs.pdi %>%
		filter(storm.name == toupper(storm)) %>%
		filter(storm.year == year)
	wanted.pdi.df$storm.pdi
}

# Get PDI of a single storm by id
get_pdi_by_id <- function(id){
	wanted.pdi.df <-hurr.obs.pdi %>%
		filter(storm.id == id)
	wanted.pdi.df$storm.pdi
}

# Calculate and plot the DPDI ------------------------------

# Function to calculate the DPDI in a range of years
get_dpdi <- function(years){
	hurr.obs.pdi <- hurr.obs.pdi %>%
		filter(storm.year %in% years)

	c <- 10^(1/4)
	m <- min(hurr.obs.pdi$storm.pdi)

	dpdi.df <- data.frame()
	for (i in 1:20) {
		dpdi.df <- rbind(dpdi.df, c(c^(i-1)*m, c^(i)*m))
	}
	colnames(dpdi.df) <- c("pdi.min", "pdi.max")

	dpdi.df <- dpdi.df %>%
		mutate(pdi.star = sqrt(pdi.min * pdi.max),
					 pdi.bin = pdi.max - pdi.min) %>%
		rowwise() %>%
		mutate(ndpdi = sum(hurr.obs.pdi$storm.pdi >= pdi.min & hurr.obs.pdi$storm.pdi <= pdi.max),
					 dpdi = ndpdi/(length(hurr.obs.pdi$storm.pdi)*pdi.bin)) %>%
		filter(dpdi != 0)

	return(dpdi.df)
}

# Function to plot the DPDI data frame
plot_dpdi <- function(years){
	dpdi.df <- get_dpdi(years)
	ggplot(dpdi.df) +
		geom_line(aes(x = pdi.star, y = dpdi), linetype = "dotted") +
		geom_point(aes(x = pdi.star, y = dpdi)) +
		scale_x_log10() +
		scale_y_log10() +
		labs(title = paste0("PDI probability density for ", years[1], "-", years[length(years)]),
				 x = "PDI (m^3/s^2)",
				 y = "D(PDI) (s^2/m^3)")
}

# Track a storm --------------------------------------------

# Track of a single storm by name
track_storm <- function(storm, year){
	ggplot(hurr.obs %>%
				 	filter(storm.name == toupper(storm)) %>%
				 	filter(storm.year == year)) +
		geom_line(aes(x = date.time, y = wind), linetype = "dotted") +
		geom_point(aes(x = date.time, y = wind, colour = status)) +
		labs(title = paste0(storm, " profile ", "(", year, ")",
												", PDI = ", scientific(get_pdi(storm, year), digits = 3), " m^3/s^2"),
				 x = "Time (days)", y = "Velocity (kt)", colour = "Status")
}

# Track of a single storm by id
track_storm_by_id <- function(id){
	wanted.storm <- hurr.obs %>%
		filter(storm.id == id)
	storm <- wanted.storm$storm.name
	year <- wanted.storm$storm.year
	ggplot(wanted.storm)+
		geom_line(aes(x = date.time, y = wind), linetype="dotted") +
		geom_point(aes(x = date.time, y = wind, colour = status)) +
		labs(title = paste0(storm, " profile ", "(", year, ")",
												", PDI = ", scientific(get_pdi_by_id(id), digits = 3), " m^3/s^2"),
				 x = "Time (days)", y = "Velocity (kt)", colour = "Status")
}