# Code to study the PDI dependence with the SST (presentation)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("./hadisst_base.R")
source("./hurdat2_pdi_base.R")
source("./analysis_base.R")

# Create PDI data frame ------------------------------------

# Get hurricanes data frames
hurr.natl.obs <- get_hurr_obs("./hurdat2-1851-2016-apr2017.txt")
hurr.epac.obs <- get_hurr_obs("./hurdat2-nepac-1949-2016-apr2017.txt")
hurr.all.obs <- rbind(hurr.natl.obs, hurr.epac.obs)

# Create data frame with PDI and year of the storm
hurr.natl.pdi <- get_pdis(hurr.natl.obs) %>%
	filter(storm.id != "AL171988")
attr(hurr.natl.pdi, "title") <- "N. Atl."
hurr.epac.pdi <- get_pdis(hurr.epac.obs) %>%
	filter(storm.id != "EP231989")
attr(hurr.epac.pdi, "title") <- "E. Pac."
hurr.all.pdi <- get_pdis(hurr.all.obs)

# Create SST data frames -----------------------------------

hadsst.raster <- load_hadsst(file = "./data/HadISST_sst.nc")

# Windows of activity
years.natl <- 1966:2016
coords.natl <- c("90W", "20E", "5N", "25N")
coords.natl.map <- c("100W", "20E", "0N", "60N")

years.epac <- 1966:2016
coords.epac <- c("120W", "90W", "5N", "20N")
coords.epac.map <- c("160W", "90W", "5N", "35N")

# Construct SST data frames
ssts.natl <- get_mean_ssts(years = years.natl, coords = coords.natl)
attr(ssts.natl, "title") <- "N. Atl."

ssts.epac <- get_mean_ssts(years = years.epac, coords = coords.epac)
attr(ssts.epac, "title") <- "E. Pac."

# Basins maps ----------------------------------------------

# Maps of the basins (full)
# map_region_hurrs(hurr.natl.obs, years.natl, coords.natl.map, coords.natl, steps = c(20, 10), xtra.lims = c(3,2)) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "map-natl.pdf", width = 5.75, height = 3.75, dpi = 96, device = cairo_pdf)
# map_region_hurrs(hurr.epac.obs, years.epac, coords.epac.map, coords.epac, steps = c(10, 10), xtra.lims = c(3,2)) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "map-epac.pdf", width = 6, height = 3.15, dpi = 96, device = cairo_pdf)

# SST map of a raster layer
# map_global_sst(hadsst.raster, 12, 2015) #+ theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "map-sst.pdf", width = 6, height = 3, dpi = 96, device = cairo_pdf)

map_region_hurrs2 <- function(hurr.obs, years, coords, rect.coords, steps = c(5,5), xtra.lims = c(1.5,1.5)){
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
							colour = "red", alpha = 0.2, size = 0.2) #+
		# annotate("rect", xmin = as.integer(rect.coords[1]), xmax = as.integer(rect.coords[2]),
		# 				 ymin = as.integer(rect.coords[3]), ymax = as.integer(rect.coords[4]),
		# 				 colour = "green", alpha = 0.2)
}

coords.all.map <- c("160W", "0E", "0N", "60N")
map_region_hurrs2(hurr.all.obs, years.natl, coords.all.map, coords.natl, steps = c(20, 10), xtra.lims = c(3,2)) + theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "complete-map.pdf", width = 6.75, height = 3.75, dpi = 96, device = cairo_pdf)

# Plot SSTs and PDIs ---------------------------------------

# Plot annual SSTs

plot_annual_sst(ssts.natl) + theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "sst-analysis-natl.pdf", width = 6.5, height = 3.75, dpi = 96, device = cairo_pdf)
# plot_annual_sst(ssts.epac) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "sst-analysis-epac.pdf", width = 6.5, height = 3.5, dpi = 96, device = cairo_pdf)

# PDI time series
# plot_pdi_tempseries(hurr.natl.pdi, ssts.natl) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "time-series-natl.pdf", width = 6.5, height = 3, dpi = 96, device = cairo_pdf)
# plot_pdi_tempseries(hurr.epac.pdi, ssts.epac) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "time-series-epac.pdf", width = 6.5, height = 3, dpi = 96, device = cairo_pdf)

# DPDI plots
plot_dpdi(hurr.natl.pdi, years.natl) + theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "dpdi-natl.pdf", width = 6.5, height = 3.75, dpi = 96, device = cairo_pdf)
# plot_dpdi(hurr.epac.pdi, years.epac) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "dpdi-epac.pdf", width = 6.5, height = 3, dpi = 96, device = cairo_pdf)

# DPDI plots by SST class
plot_dpdi_by_sst_class(hurr.natl.pdi, ssts.natl) + theme_bw() + theme(legend.position = c(0.92, 0.80)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "dpdi-by-class-natl.pdf", width = 6.5, height = 3.75, dpi = 96, device = cairo_pdf)
# plot_dpdi_by_sst_class(hurr.epac.pdi, ssts.epac) #+ theme_bw() + theme(legend.position = c(0.92, 0.80)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "dpdi-by-class-epac.pdf", width = 6.5, height = 3.25, dpi = 96, device = cairo_pdf)

# Get summary of cyclone status
# table((hurr.natl.obs%>% filter(storm.year %in% years.natl))$status)

# Scatterplots analysis ------------------------------------

plot_pdi_scatter_pres1 <- function(hurr.pdi, ssts){
	hurr.pdi.nds <- hurr.pdi %>%
		filter(max.wind <= 33)
	hurr.pdi <- hurr.pdi %>%
		filter(max.wind > 33)

	hurr.high.pdi <- hurr.pdi %>%
		filter(storm.year %in% get_high_years(ssts))
	hurr.low.pdi <- hurr.pdi %>%
		filter(storm.year %in% get_low_years(ssts))

	hurr.high.pdi.nds <- hurr.pdi.nds %>%
		filter(storm.year %in% get_high_years(ssts))
	hurr.low.pdi.nds <- hurr.pdi.nds %>%
		filter(storm.year %in% get_low_years(ssts))

	# lm.high.y <- lm(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.high.pdi)
	# lm.low.y <- lm(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.low.pdi)
	# lm.high.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(storm.pdi), data = hurr.high.pdi)
	# lm.low.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(storm.pdi), data = hurr.low.pdi)

	years.str <- paste0(year(ssts$year[1]), "-", year(ssts$year[length(ssts$year)]))

	gg <- ggplot() +
		aes(x = conv_unit(storm.duration, "sec", "hr"), y = storm.pdi) +
		geom_point(data = hurr.low.pdi, aes(colour = "low"), shape = 5, size = 1) +
		geom_point(data = hurr.high.pdi, aes(colour = "high"), shape = 1, size = 1) +
		geom_smooth(data = hurr.low.pdi, aes(colour = "low"),
								method = "glm", formula = y ~ x , level = 0.99,
								size = 0.5) +
		geom_smooth(data = hurr.high.pdi, aes(colour = "high"),
								method = "glm", formula = y ~ x , level = 0.99,
								size = 0.5) +
		geom_point(data = hurr.low.pdi.nds, aes(colour = "low"), shape = 5, size = 1) +
		geom_point(data = hurr.high.pdi.nds, aes(colour = "high"), shape = 1, size = 1) +
		geom_smooth(data = hurr.high.pdi.nds, aes(colour = "high"),
								method = "glm", formula = y ~ x , level = 0.99,
								size = 0.5) +
		geom_smooth(data = hurr.low.pdi.nds, aes(colour = "low"),
								method = "glm", formula = y ~ x , level = 0.99,
								size = 0.5) +
		scale_x_log10(breaks = c(25, 50, 100, 200, 400, 800)) +
		scale_y_log10() +
		guides(colour = guide_legend(order = 1, override.aes = list(fill = NA, linetype = c(0,0), shape = c(1,5))),
					 linetype = guide_legend(order = 2, override.aes = list(colour = c("black", "black","black", "black")))) +
		scale_colour_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		scale_linetype_manual(values = c("x(y)" = "longdash", "y(x)" = "solid")) +
		labs(title = paste0("PDI vs duration scatterplot", " (", attr(ssts, "title"), "; ", years.str ,")") ,
				 x = "Storm duration (h)", y = bquote(PDI~ (m^3 ~s^-2)),
				 colour = "SST class", linetype = "Regression")
	return(gg)
}

# PDI scatterplots (duration)
# plot_pdi_scatter_pres1(hurr.natl.pdi, ssts.natl, "ds") + theme_bw() #+ theme(legend.position = c(0.13, 0.66)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "scatter-natl-ds.pdf", width = 6.5, height = 3.8, dpi = 96, device = cairo_pdf)
# plot_pdi_scatter_pres1(hurr.natl.pdi, ssts.natl, "nds") + theme_bw() #+ theme(legend.position = c(0.13, 0.66)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "scatter-natl-nds.pdf", width = 6.5, height = 3.8, dpi = 96, device = cairo_pdf)
plot_pdi_scatter_pres1(hurr.epac.pdi, ssts.epac) #+ theme_bw() + theme(legend.position = c(0.1, 0.8)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "scatter-epac-ds.pdf", width = 6.5, height = 3.75, dpi = 96, device = cairo_pdf)
# plot_pdi_scatter_pres1(hurr.epac.pdi, ssts.epac, "nds") + theme_bw() #+ theme(legend.position = c(0.13, 0.67)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "scatter-epac-nds.pdf", width = 6.5, height = 4, dpi = 96, device = cairo_pdf)

# SST Map --------------------------------------------------

hadsst.raster.new <- load_hadsst(file = "data/HadISST1_SST_update.nc")
hadsst.raster.new[hadsst.raster.new == -1000] <- -5

# SST map of a single time layer
map_global_sst_cont <- function(x = hadsst.raster, month, year){
	time.layer = month
	gplot(raster(x, layer = time.layer)) +
		geom_tile(aes(fill = value)) +
		# scale_fill_continuous(guide="legend", na.value = "white") +
		scale_fill_gradientn(colours = c("#760200", "#b10e00", "#ec3b00", "#f9a100", "#d3ed0a", "#88ec6a", "#6cd2a8", "#4ca6e8", "#3276fb", "#214FBB", "#010546"),
												 values = rescale(c(35, 30, 25, 20, 15, 10, 7.5, 5, 2.5, 0, -5)),
												 breaks=c(30, 15, 0, -5),
												 labels=c(30, 15, 0, "Ice"),
												 guide="colourbar", na.value = "white") +
		labs(fill = "SST") +
		theme(plot.margin=margin(c(0,10,0,0)),
					legend.margin=margin(c(1,1,5,-15)),
					axis.line=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank())
}

map_global_sst_cont(hadsst.raster.new, 5, 2017) #+ theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "map-sst-cont.pdf", width = 6, height = 3, dpi = 96, device = cairo_pdf)

track_storm(hurr.natl.obs, "Katrina", 2005) #+ theme_bw() + theme(legend.position = c(0.2, 0.75)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "track-storm-katrina.pdf", width = 4.75, height = 3.5, dpi = 96, device = cairo_pdf)
