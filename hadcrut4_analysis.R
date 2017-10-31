# Code to study the global average temperature (from HadCRUT4)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(tidyverse)
library(lubridate) # Use dates

# Read and clean raw data ----------------------------------

# Read the raw data
temps.data <- read.table("data/HadCRUT.4.5.0.0.annual_ns_avg_smooth.txt")

# Clean the raw data
temps.data <- temps.data %>%
	dplyr::select(V1, V2) %>%
	rename(year = V1, temp = V2) %>%
	mutate(year = ymd(paste(year, "01", "01", sep = "-")))
	# mutate(temp.class = ifelse(year(year) >= 1975, "high", "low"))

# Data visualisation ---------------------------------------

# Find local absolute minimum
filter(temps.data, temp == min((temps.data %>% filter(year(year) %in% 1966:2016))$temp))

# Plot global mean temperature anomalies
plot_global_temperature <- function(data.df){
	years.str <- paste0(year(data.df$year[1]), "-", year(data.df$year[length(data.df$year)]))

	ggplot(data.df) +
		aes(x = year, y = temp) +
		geom_hline(aes(yintercept = 0, linetype = "1961-90 mean"), colour = "blueviolet") +
		geom_line(aes(linetype = "Annual"), colour = "black") +
		geom_point(data=temps.data[125, ], aes(x = year, y = temp), colour="red", size=2) +
		scale_linetype_manual(values = c("twodash", "solid")) +
		labs(title = paste0("Global mean temperature between ", years.str),
				 x = "Time (year)", y = "Temperature anomaly (°C)", linetype = "Temperature") +
		guides(linetype = guide_legend(override.aes = list(colour = c("blueviolet", "black"))))
}

plot_global_temperature(temps.data) #+ theme_bw() + theme(legend.position = c(0.13, 0.82)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "global-temps.pdf", width = 6.3, height = 3.5, dpi = 96, device = cairo_pdf)
