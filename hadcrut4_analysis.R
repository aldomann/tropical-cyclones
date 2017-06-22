# Code to study the global average temperature (from HadCRUT4)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(tidyverse)
library(lubridate) # Use dates

# Read and clean raw data ----------------------------------
temps.file <- paste0("data/", "HadCRUT.4.5.0.0.annual_ns_avg_smooth.txt")

temps.data <- read.table(temps.file)

temps.data <- temps.data %>%
	dplyr::select(V1, V2) %>%
	rename(year = V1, temp = V2) %>%
	mutate(year = ymd(paste(year, "01", "01", sep = "-")))
				 # temp.class = ifelse(year(year) >= 1975, "high", "low"))

# Data visualisation ---------------------------------------

templ.plot <- ggplot(temps.data) +
	aes(x = year, y = temp) +
	geom_hline(aes(yintercept = 0, linetype = "1961-1990 mean"), colour = "blueviolet") +
	geom_line(aes(linetype = "Annual"), colour = "black") +
	geom_point(data=temps.data[126, ], aes(x = year, y = temp), colour="red", size=2) +
	scale_linetype_manual(values = c("twodash", "solid")) +
	labs(title = paste0("Global average temperature 1850-2016"),
			 x = "Time (year)", y = "Temperature anomaly (°C)", linetype = "Temperature") +
	guides(linetype = guide_legend(override.aes = list(colour = c("blueviolet", "black"))))

templ.plot #+ theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "global-temps.pdf", width = 6.5, height = 2.5, dpi = 96, device = cairo_pdf)
