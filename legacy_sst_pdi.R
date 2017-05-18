library(tidyverse)
library(stringr) # To split lines
library(lubridate) # Use dates


# Read raw data --------------------------------------------

tracks.file.high <- paste0("data/", "sst_large_$9_NAtl_1966_2007.dat")
tracks.file.low <- paste0("data/", "sst_low_$9_NAtl_1966_2007.dat")

legacy.dat.high <- read.table(tracks.file.high)
legacy.dat.high <- legacy.dat.high %>%
	dplyr::select(V3,V5) %>%
	rename(storm.duration = V3, storm.pdi = V5)

legacy.dat.low <- read.table(tracks.file.low)
legacy.dat.low <- legacy.dat.low %>%
	dplyr::select(V3,V5) %>%
	rename(storm.duration = V3, storm.pdi = V5)


# Data manipulation ----------------------------------------

lm.high.y <- lm(log10(storm.pdi) ~ log10(storm.duration), data = legacy.dat.high)
lm.low.y <- lm(log10(storm.pdi) ~ log10(storm.duration), data = legacy.dat.low)
lm.high.x <- lm(log10(storm.duration) ~ log10(storm.pdi), data = legacy.dat.high)
lm.low.x <- lm(log10(storm.duration) ~ log10(storm.pdi), data = legacy.dat.low)

coef(lm.high.y)[[2]]
coef(lm.low.y)[[2]]
1/coef(lm.high.x)[[2]]
1/coef(lm.low.x)[[2]]

ggplot() +
	aes(x = storm.duration, y = storm.pdi) +
	geom_point(data = legacy.dat.high, aes(colour = "high"), size = 0.3) +
	geom_point(data = legacy.dat.low, aes(colour = "low"), size = 0.3) +
	geom_abline(aes(slope = coef(lm.high.y)[[2]], intercept = coef(lm.high.y)[[1]],
									colour = "high.y~x"), linetype = "twodash") +
	geom_abline(aes(slope = coef(lm.low.y)[[2]], intercept = coef(lm.low.y)[[1]],
									colour = "low.y~x"), linetype = "twodash") +
	geom_abline(aes(slope = 1/coef(lm.high.x)[[2]], intercept = -coef(lm.high.x)[[1]]/coef(lm.high.x)[[2]],
									colour = "high.x~y"), linetype = "twodash") +
	geom_abline(aes(slope = 1/coef(lm.low.x)[[2]], intercept = -coef(lm.low.x)[[1]]/coef(lm.low.x)[[2]],
									colour = "low.x~y"), linetype = "twodash") +
	scale_colour_manual(values = c("high" = "brown1", "low" = "dodgerblue1",
																 "high.y~x" = "red", "low.y~x" = "blue",
																 "high.x~y" = "darkviolet", "low.x~y" = "green")) +
	guides(color=guide_legend(override.aes = list(linetype = c(0,4,4,0,4,4)))) +
	scale_x_log10() +
	scale_y_log10() +
	labs(title = paste0("PDI Scatterplot", " (N. ATl; 1966-2007)"),
			 x = "Storm duration (h)", y = "PDI (m^3/s^2)", colour = "SST Class")
