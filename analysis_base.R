# Base code with misc functions needed in main_analysis
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Data visualisation functions -----------------------------

# Function to plot the DPDI data frame
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
				 x = "PDI (m^3/s^2)", y = "D(PDI) (s^2/m^3)", colour = "SST Class")
}

map_region_hurrs <- function(hurr.obs, years, coords){
	library(maps)
	library(ggalt)

	coords <- morph_coords(coords)

	hurr.obs <- hurr.obs %>%
		filter(storm.year %in% years)

	world_map <- map_data("world")
	world_map <- subset(world_map, region!="Antarctica")

	map <- ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
		geom_cartogram(map = world_map, aes(map_id = region)) +
		# xlim(c(as.numeric(coords[1]), as.numeric(coords[2]))) +
		# ylim(c(as.numeric(coords[3]), as.numeric(coords[4]))) +
		scale_x_continuous(expand = c(0,0), limits = c(as.numeric(coords[1]), as.numeric(coords[2]))) +
		scale_y_continuous(expand = c(0,0), limits = c(as.numeric(coords[3]),as.numeric(coords[4]))) +
		coord_trans() +
		geom_path(data = hurr.obs, aes(x = long.num, y = lat.num, group = storm.id),
							color = "red", alpha = 0.2, size = 0.2)

	return(map)
}
