# Base code with misc functions needed in main_analysis
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(maps)
library(ggalt)

# Basins maps functions ------------------------------------

# Map showing the hurricanes in specified window
# SRC: http://stackoverflow.com/questions/33302424/format-latitude-and-longitude-axis-labels-in-ggplot
scale_x_longitude <- function(xmin = -180, xmax = 180, step = 1, xtra.lim = 1.5, ...) {
	xbreaks <- seq(xmin,xmax,step)
	xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(-x,"^o", "*W")),
																											 ifelse(x > 0, parse(text=paste0(x,"^o", "*E")), x))))
	return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels,
														expand = c(0, 0), limits = c(xmin-xtra.lim, xmax+xtra.lim), ...))
}

scale_y_latitude <- function(ymin = -90, ymax = 90, step = 0.5, xtra.lim = 1.5, ...) {
	ybreaks <- seq(ymin,ymax,step)
	ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(-x,"^o", "*S")),
																											 ifelse(x > 0, parse(text=paste0(x,"^o", "*N")), x))))
	return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels,
														expand = c(0, 0), limits = c(ymin-xtra.lim, ymax+xtra.lim), ...))
}

# Install legacy version of ggalt (see https://github.com/hrbrmstr/ggalt/issues/33)
# devtools::install_github("rplzzz/ggalt", ref = "ggp221")
map_region_hurrs <- function(hurr.obs, years, coords, rect.coords, steps = c(5,5), xtra.lims = c(1.5,1.5)){
	coords <- morph_coords(coords)
	rect.coords <- morph_coords(rect.coords)
	hurr.obs <- hurr.obs %>%
		filter(storm.year %in% years)
	world_map <- map_data("world")
	world_map <- subset(world_map, region!="Antarctica")

	ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
		geom_cartogram(map = world_map, aes(map_id = region), colour = "white", fill = "grey50") +
		scale_x_longitude(xmin = as.numeric(coords[1]), xmax = as.numeric(coords[2]),
											step = steps[1], xtra.lim = xtra.lims[1]) +
		scale_y_latitude(ymin = as.numeric(coords[3]), ymax = as.numeric(coords[4]),
										 step = steps[2], xtra.lim = xtra.lims[2]) +
		coord_proj("+proj=merc") +
		geom_path(data = hurr.obs, aes(x = long, y = lat, group = storm.id),
							colour = "red", alpha = 0.2, size = 0.2) +
		annotate("rect", xmin = as.integer(rect.coords[1]), xmax = as.integer(rect.coords[2]),
						 ymin = as.integer(rect.coords[3]), ymax = as.integer(rect.coords[4]),
						 colour = "green", alpha = 0.2)
}

map_region_hurrs_small <- function(hurr.obs, years, coords, steps = c(5,5), xtra.lims = c(1.5,1.5)){
	coords <- morph_coords(coords)
	hurr.obs <- hurr.obs %>%
		filter(storm.year %in% years)
	world_map <- map_data("world")
	world_map <- subset(world_map, region!="Antarctica")

	ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
		geom_cartogram(map = world_map, aes(map_id = region)) +
		scale_x_longitude(xmin = as.numeric(coords[1]), xmax = as.numeric(coords[2]),
											step = steps[1], xtra.lim = xtra.lims[1]) +
		scale_y_latitude(ymin = as.numeric(coords[3]), ymax = as.numeric(coords[4]),
										 step = steps[2], xtra.lim = xtra.lims[2]) +
		coord_proj("+proj=merc") +
		geom_path(data = hurr.obs, aes(x = long, y = lat, group = storm.id),
							colour = "red", alpha = 0.2, size = 0.2)
}

# PDI visualisation functions ------------------------------

# Plot the DPDI data frame
plot_dpdi_by_sst_class <- function(hurr.obs.pdi, ssts.df){
	years <- year(ssts.df$year[1]):year(ssts.df$year[length(ssts.df$year)])

	high.years <- get_high_years(ssts.df)
	low.years <- get_low_years(ssts.df)
	dpdi.high.df <- get_dpdi(hurr.obs.pdi, high.years)
	dpdi.low.df <- get_dpdi(hurr.obs.pdi, low.years)

	ggplot() +
		aes(x = pdi.star, y = dpdi, ymin = dpdi-pdi.error, ymax = dpdi+pdi.error) +
		geom_line(data = dpdi.high.df, aes(colour = "high"), linetype = "dotted") +
		geom_point(data = dpdi.high.df, aes(colour = "high")) +
		geom_errorbar(data = dpdi.high.df, aes(colour = "high"), width = 0.1) +
		geom_line(data = dpdi.low.df, aes(colour = "low"), linetype = "dotted") +
		geom_point(data = dpdi.low.df, aes(colour = "low")) +
		geom_errorbar(data = dpdi.low.df, aes(colour = "low"), width = 0.1) +
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		scale_x_log10() +
		scale_y_log10() +
		labs(title = paste0("PDI probability density for ",
												years[1], "-", years[length(years)],
												" (", attr(ssts.df, "title"), ")"),
				 x = bquote(PDI~ (m^3 ~s^-2)), y = bquote(D(PDI)~(s^2~m^-3)), colour = "SST class")
}

# Plot PDI time series
plot_pdi_tempseries <- function(hurr.pdi, ssts){
	hurr.high.pdi <- hurr.pdi %>% filter(storm.year %in% get_high_years(ssts))
	hurr.low.pdi <- hurr.pdi %>% filter(storm.year %in% get_low_years(ssts))
	years.str <- paste0(year(ssts$year[1]), "-", year(ssts$year[length(ssts$year)]))

	ggplot() +
		aes(x = as.Date(paste(storm.year, "01", "01", sep = "-")), y = storm.pdi, group = 1) +
		geom_point(data = hurr.high.pdi, aes(colour = "high"), size = 0.5)+
		geom_point(data = hurr.low.pdi, aes(colour = "low"), size = 0.5)+
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		scale_y_log10() +
		labs(title = paste0("PDI Time series", " (", attr(ssts, "title"), "; ", years.str, ")"),
				 x = "Time (year)", y = bquote(PDI~ (m^3 ~s^-2)), colour = "SST class" )
}

# Scatterplot functions ------------------------------------

# PDI scatterplots
plot_pdi_scatter_by_class <- function(hurr.pdi, ssts, class){
	if(class == "high"){
		hurr.pdi <- hurr.pdi %>%
			filter(storm.year %in% get_high_years(ssts))
	} else if(class == "low"){
		hurr.pdi <- hurr.pdi %>%
			filter(storm.year %in% get_low_years(ssts))
	}
	hurr.ds.pdi <- hurr.pdi %>%
		filter(max.wind > 33)
	hurr.nds.pdi <- hurr.pdi %>%
		filter(max.wind <= 33)

	lm.ds.y <- lm(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.ds.pdi)
	lm.nds.y <- lm(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.nds.pdi)
	lm.ds.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(storm.pdi), data = hurr.ds.pdi)
	lm.nds.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(storm.pdi), data = hurr.nds.pdi)

	years.str <- paste0(year(ssts$year[1]), "-", year(ssts$year[length(ssts$year)]))

	gg <- ggplot() +
		aes(x = conv_unit(storm.duration, "sec", "hr"), y = storm.pdi) +
		geom_point(data = hurr.ds.pdi,
							 aes(colour = "developing"), shape = 1, size = 1) +
		geom_point(data = hurr.nds.pdi,
							 aes(colour = "non-developing"), shape = 5, size = 1) +
		geom_abline(aes(slope = coef(lm.nds.y)[[2]],
										intercept = coef(lm.nds.y)[[1]],
										colour = "non-developing", linetype = "y(x)")) +
		geom_abline(aes(slope = 1/coef(lm.nds.x)[[2]],
										intercept = -coef(lm.nds.x)[[1]]/coef(lm.nds.x)[[2]],
										colour = "non-developing", linetype = "x(y)")) +
		geom_abline(aes(slope = coef(lm.ds.y)[[2]],
										intercept = coef(lm.ds.y)[[1]],
										colour = "developing", linetype = "y(x)")) +
		geom_abline(aes(slope = 1/coef(lm.ds.x)[[2]],
										intercept = -coef(lm.ds.x)[[1]]/coef(lm.ds.x)[[2]],
										colour = "developing", linetype = "x(y)")) +
		scale_x_log10(breaks = c(25, 50, 100, 200, 400, 800)) +
		scale_y_log10() +
		labs(title = paste0("PDI vs duration scatterplot", " (", attr(ssts, "title"), "; ", years.str ,")") ,
				 x = "Storm duration (h)", y = bquote(PDI~ (m^3 ~s^-2)),
				 colour = "System status", linetype = "Regression") +
		guides(colour = guide_legend(order = 1, override.aes = list(linetype = c(0,0), shape = c(1,5))),
					 linetype = guide_legend(order = 2, override.aes = list(colour = c("black", "black"))))

	if(class == "high"){
		gg +
			scale_colour_manual(labels = c(bquote(.(paste0("developing; ")) ~ r^2 ~
																						.(paste0("= ", format(summary(lm.ds.y)$r.squared, digits = 2)))),
																		 bquote(.(paste0("non-developing; ")) ~ r^2 ~
																						.(paste0("= ", format(summary(lm.nds.y)$r.squared, digits = 2))))),
													values = c("developing" = "brown1", "non-developing" = "darkviolet")) +
			scale_linetype_manual(values = c("x(y)" = "longdash", "y(x)" = "solid"))
	} else if(class == "low"){
		gg +
			scale_colour_manual(labels = c(bquote(.(paste0("developing; ")) ~ r^2 ~
																						.(paste0("= ", format(summary(lm.ds.y)$r.squared, digits = 2)))),
																		 bquote(.(paste0("non-developing; ")) ~ r^2 ~
																						.(paste0("= ", format(summary(lm.nds.y)$r.squared, digits = 2))))),
													values = c("developing" = "dodgerblue1", "non-developing" = "purple")) +
			scale_linetype_manual(values = c("x(y)" = "longdash", "y(x)" = "solid"))
	}
}

plot_pdi_scatter_by_status <- function(hurr.pdi, ssts, status){
	if(status == "ds"){
		hurr.pdi <- hurr.pdi %>%
			filter(max.wind > 33)

	} else if(status == "nds"){
		hurr.pdi <- hurr.pdi %>%
			filter(max.wind <= 33)
	}
	hurr.high.pdi <- hurr.pdi %>%
		filter(storm.year %in% get_high_years(ssts))
	hurr.low.pdi <- hurr.pdi %>%
		filter(storm.year %in% get_low_years(ssts))

	lm.high.y <- lm(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.high.pdi)
	lm.low.y <- lm(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.low.pdi)
	lm.high.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(storm.pdi), data = hurr.high.pdi)
	lm.low.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(storm.pdi), data = hurr.low.pdi)

	years.str <- paste0(year(ssts$year[1]), "-", year(ssts$year[length(ssts$year)]))

	gg <- ggplot() +
		aes(x = conv_unit(storm.duration, "sec", "hr"), y = storm.pdi) +
		geom_point(data = hurr.low.pdi, aes(colour = "low"), shape = 5, size = 1) +
		geom_point(data = hurr.high.pdi, aes(colour = "high"), shape = 1, size = 1) +
		geom_abline(aes(slope = coef(lm.low.y)[[2]],
										intercept = coef(lm.low.y)[[1]],
										colour = "low", linetype = "y(x)")) +
		geom_abline(aes(slope = 1/coef(lm.low.x)[[2]],
										intercept = -coef(lm.low.x)[[1]]/coef(lm.low.x)[[2]],
										colour = "low", linetype = "x(y)")) +
		geom_abline(aes(slope = coef(lm.high.y)[[2]],
										intercept = coef(lm.high.y)[[1]],
										colour = "high", linetype = "y(x)")) +
		geom_abline(aes(slope = 1/coef(lm.high.x)[[2]],
										intercept = -coef(lm.high.x)[[1]]/coef(lm.high.x)[[2]],
										colour = "high", linetype = "x(y)")) +
		scale_x_log10(breaks = c(25, 50, 100, 200, 400, 800)) +
		scale_y_log10() +
		guides(colour = guide_legend(order = 1, override.aes = list(linetype = c(0,0), shape = c(1,5))),
					 linetype = guide_legend(order = 2, override.aes = list(colour = c("black", "black")))) +
		scale_colour_manual(labels = c(bquote(.(paste0("high; ")) ~ r^2 ~
																						.(paste0("= ", format(summary(lm.high.y)$r.squared, digits = 2)))),
																	 bquote(.(paste0("low;  ")) ~ r^2 ~
																	 			 	.(paste0("= ", format(summary(lm.low.y)$r.squared, digits = 2))))),
												values = c("high" = "brown1", "low" = "dodgerblue1")) +
		scale_linetype_manual(values = c("x(y)" = "longdash", "y(x)" = "solid")) +
		labs(title = paste0("PDI vs duration scatterplot", " (", attr(ssts, "title"), "; ", years.str ,")") ,
				 x = "Storm duration (h)", y = bquote(PDI~ (m^3 ~s^-2)),
				 colour = "SST class", linetype = "Regression")

	if(status == "ds"){
		gg
	} else if(status == "nds"){
		gg +
			scale_y_log10(breaks = c(0.25*10^9, 0.5*10^9, 10^9, 0.25*10^10, 0.5*10^10))
	}
}

plot_pdi_scatter_wind <- function(hurr.pdi, ssts){
	hurr.pdi <- hurr.pdi %>%
		filter(max.wind > 33)
	hurr.high.pdi <- hurr.pdi %>%
		filter(storm.year %in% get_high_years(ssts))
	hurr.low.pdi <- hurr.pdi %>%
		filter(storm.year %in% get_low_years(ssts))

	lm.high.y <- lm(log10(storm.pdi) ~ log10(conv_unit(max.wind, "knot", "m_per_sec")), data = hurr.high.pdi)
	lm.low.y <- lm(log10(storm.pdi) ~ log10(conv_unit(max.wind, "knot", "m_per_sec")), data = hurr.low.pdi)
	lm.high.x <- lm(log10(conv_unit(max.wind, "knot", "m_per_sec")) ~ log10(storm.pdi), data = hurr.high.pdi)
	lm.low.x <- lm(log10(conv_unit(max.wind, "knot", "m_per_sec")) ~ log10(storm.pdi), data = hurr.low.pdi)

	years.str <- paste0(year(ssts$year[1]), "-", year(ssts$year[length(ssts$year)]))

	ggplot() +
		aes(x = conv_unit(max.wind, "knot", "m_per_sec"), y = storm.pdi) +
		geom_point(data = hurr.low.pdi, aes(colour = "low"), shape = 5, size = 1, position = "jitter") +
		geom_point(data = hurr.high.pdi, aes(colour = "high"), shape = 1, size = 1, position = "jitter") +
		geom_abline(aes(slope = coef(lm.low.y)[[2]],
										intercept = coef(lm.low.y)[[1]],
										colour = "low", linetype = "y(x)")) +
		geom_abline(aes(slope = 1/coef(lm.low.x)[[2]],
										intercept = -coef(lm.low.x)[[1]]/coef(lm.low.x)[[2]],
										colour = "low", linetype = "x(y)")) +
		geom_abline(aes(slope = coef(lm.high.y)[[2]],
										intercept = coef(lm.high.y)[[1]],
										colour = "high", linetype = "y(x)")) +
		geom_abline(aes(slope = 1/coef(lm.high.x)[[2]],
										intercept = -coef(lm.high.x)[[1]]/coef(lm.high.x)[[2]],
										colour = "high", linetype = "x(y)")) +
		scale_x_log10(breaks = c(25, 50, 100, 200, 400, 800)) +
		scale_y_log10() +
		guides(colour = guide_legend(order = 1, override.aes = list(linetype = c(0,0), shape = c(1,5))),
					 linetype = guide_legend(order = 2, override.aes = list(colour = c("black", "black")))) +
		scale_colour_manual(labels = c(bquote(.(paste0("high; ")) ~ r^2 ~
																						.(paste0("= ", format(summary(lm.high.y)$r.squared, digits = 2)))),
																	 bquote(.(paste0("low;  ")) ~ r^2 ~
																	 			 	.(paste0("= ", format(summary(lm.low.y)$r.squared, digits = 2))))),
												values = c("high" = "brown1", "low" = "dodgerblue1")) +
		scale_linetype_manual(values = c("x(y)" = "longdash", "y(x)" = "solid")) +
		labs(title = paste0("PDI vs wind speed scatterplot", " (", attr(ssts, "title"), "; ", years.str ,")") ,
				 x = "Maximum wind speed (kt)", y = bquote(PDI~ (m^3 ~s^-2)),
				 colour = "SST class", linetype = "Regression")
}
