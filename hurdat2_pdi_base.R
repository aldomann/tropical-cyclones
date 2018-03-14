# Base code to study the PDI probability density (DPDI)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

source("hurdat2_base.R")

library(measurements) # Convert units
library(scales) # To show exponents

# Calculate the PDI ----------------------------------------

# Create data frame with PDI and year of the storm
get_pdis <- function(hurr.obs){
	hurr.obs.pdi <- hurr.obs %>%
		group_by(storm.id, storm.name, n.obs) %>%
		summarise(storm.pdi = sum(conv_unit(wind, "knot", "m_per_sec")^3 * conv_unit(6, "hr", "sec")),
							max.wind = max(wind),
							mean.wind = mean(wind)) %>%
		mutate(storm.duration = n.obs * conv_unit(6, "hr", "sec")) %>%
		mutate(storm.year = substring(storm.id, 5, 9)) %>%
		filter(storm.pdi != "NA") %>%
		filter(storm.pdi != 0)
	hurr.obs.pdi <- hurr.obs.pdi[c("storm.id", "storm.name", "n.obs", "storm.duration", "storm.pdi", "max.wind", "mean.wind", "storm.year")]
	return(hurr.obs.pdi)
}

# Get PDI of a single storm by name
get_pdi <- function(hurr.obs.pdi, storm, year){
	wanted.pdi.df <- hurr.obs.pdi %>%
		filter(storm.name == toupper(storm)) %>%
		filter(storm.year == year)
	wanted.pdi.df$storm.pdi
}

# Get PDI of a single storm by id
get_pdi_by_id <- function(hurr.obs.pdi, id){
	wanted.pdi.df <- hurr.obs.pdi %>%
		filter(storm.id == id)
	wanted.pdi.df$storm.pdi
}

# Calculate and plot the DPDI ------------------------------

# Function to calculate the DPDI in a range of years
get_dpdi <- function(hurr.obs.pdi, years){
	hurr.obs.pdi <- hurr.obs.pdi %>%
		filter(storm.year %in% years)

	c <- 10^(1/5)
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
					 dpdi = ndpdi/(length(hurr.obs.pdi$storm.pdi)*pdi.bin),
					 pdi.error = dpdi / sqrt(ndpdi) ) %>%
		filter(dpdi != 0)

	return(dpdi.df)
}

# Function to plot the DPDI data frame
plot_dpdi <- function(hurr.obs.pdi, years){
	dpdi.df <- get_dpdi(hurr.obs.pdi, years)
	ggplot(dpdi.df, aes(x = pdi.star, y = dpdi, ymin = dpdi-pdi.error, ymax = dpdi+pdi.error)) +
		geom_line(linetype = "dotted") +
		geom_point() +
		geom_errorbar(width = 0.1) +
		scale_x_log10() +
		scale_y_log10() +
		labs(title = paste0("PDI probability density for ", years[1], "-", years[length(years)],
												" (", attr(hurr.obs.pdi, "title"), ")"),
				 x = bquote(PDI~ (m^3 ~s^-2)), y = bquote(D(PDI)~(s^2~m^-3)))
}

# Track a storm --------------------------------------------

# Track of a single storm by name
track_storm <- function(hurr.obs = hurr.all.obs, storm, year){
	ggplot(hurr.obs %>%
				 	filter(storm.name == toupper(storm)) %>%
				 	filter(storm.year == year),
				 aes(x = date.time, y = wind)) +
		geom_line(linetype = "dotted") +
		geom_point(aes(colour = status)) +
		labs(title = bquote(.(storm) ~ profile ~ .(paste0("(", year, "),") ) ~ PDI ==
												.(scientific(get_pdi(hurr.all.pdi, storm, year), digits = 3)) ~ m^3 ~s^-2),
				 x = "Time (days)", y = "Wind speed (kt)", colour = "Status")
}

# Track of a single storm by id
track_storm_by_id <- function(hurr.obs = hurr.all.obs, id){
	wanted.storm <- hurr.obs %>%
		filter(storm.id == id)
	storm <- wanted.storm$storm.name
	year <- wanted.storm$storm.year

	ggplot(wanted.storm, aes(x = date.time, y = wind)) +
		geom_line(linetype="dotted") +
		geom_point(aes(colour = status)) +
		labs(title = paste0(storm, " profile ", "(", year, ")",
												", PDI = ", scientific(get_pdi_by_id(hurr.all.pdi, id), digits = 3), " m^3/s^2"),
				 x = "Time (days)", y = "Wind speed (kt)", colour = "Status")
}

