# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("hadisst_base.R")
source("hurdat2_pdi_base.R")
source("analysis_base.R")

# Save in PDF + Latin Modern Roman -------------------------

# + theme(text = element_text(family = "LM Roman 10"))
# + ggsave(filename = "asd.pdf", width = 7.813, height = 4.33, dpi = 96, device = cairo_pdf)

# Create PDI data frame ------------------------------------

# Get hurricanes data frames
hurr.natl.obs <- get_hurr_obs("hurdat2-1851-2016-apr2017.txt")
hurr.epac.obs <- get_hurr_obs("hurdat2-nepac-1949-2016-apr2017.txt")
hurr.all.obs <- rbind(hurr.natl.obs, hurr.epac.obs)

# Create data frame with PDI and year of the storm
hurr.natl.pdi <- get_pdis(hurr.natl.obs)
attr(hurr.natl.pdi, "title") <- "N. Atl."
hurr.epac.pdi <- get_pdis(hurr.epac.obs)
attr(hurr.epac.pdi, "title") <- "E. Pac."
hurr.all.pdi <- get_pdis(hurr.all.obs)

# Create SST data frames -----------------------------------

hadsst.raster <- load_hadsst(file = "data/HadISST_sst.nc")

# Windows of activity
years.natl <- 1966:2016
coords.natl <- c("90W", "20E", "5N", "25N")
coords.natl.map <- c("100W", "20E", "5N", "60N")

years.epac <- 1966:2016
coords.epac <- c("120W", "90W", "5N", "20N")
coords.epac.map <- c("160W", "90W", "5N", "35N")

# Construct SST data frames
ssts.natl <- get_mean_ssts(years = years.natl, coords = coords.natl)
attr(ssts.natl, "title") <- "N. Atl."

ssts.epac <- get_mean_ssts(years = years.epac, coords = coords.epac)
attr(ssts.epac, "title") <- "E. Pac."

# Create vector of low & high SST years
# years.low.natl <- get_low_years(ssts.natl)
# years.high.natl <- get_high_years(ssts.natl)
# years.low.epac <- get_low_years(ssts.natl)
# years.high.epac <- get_high_years(ssts.natl)

# Get number of years per SST class
table(ssts.natl$sst.class)
table(ssts.epac$sst.class)

# Basins maps ----------------------------------------------

# Maps of the basins
# map_region_hurrs(hurr.natl.obs, years.natl, coords.natl.map, steps = c(20, 10), xtra.lims = c(3,2))
# map_region_hurrs(hurr.epac.obs, years.epac, coords.epac)

map_region_hurrs_full(hurr.natl.obs, years.natl, coords.natl.map, coords.natl, steps = c(20, 10), xtra.lims = c(3,2))# + ggsave(filename = "map-natl.pdf", width = 7.2, height = 4.5, dpi = 96, device = cairo_pdf)
map_region_hurrs_full(hurr.epac.obs, years.epac, coords.epac.map, coords.epac, steps = c(10, 10), xtra.lims = c(3,2))#  + ggsave(filename = "map-epac.pdf", width = 8.8, height = 4.5, dpi = 96, device = cairo_pdf)

# Plot SSTs and PDIs ---------------------------------------

# Plot annual SSTs
plot_annual_sst(ssts.natl)
plot_annual_sst(ssts.epac)

plot_annual_sst_alt(ssts.natl)

# DPDI plots by SST class
plot_dpdi_by_sst_class(hurr.natl.pdi, ssts.natl)
plot_dpdi_by_sst_class(hurr.epac.pdi, ssts.epac)

# PDFI time series
plot_pdi_tempseries(hurr.natl.pdi, ssts.natl)
plot_pdi_tempseries(hurr.epac.pdi, ssts.epac)

# Get summary of cyclone status
# table((hurr.natl.obs%>% filter(storm.year %in% years.natl))$status)

# Scatterplots analysis ------------------------------------

# Write PDIs data frames to files
# hurr.natl.high.pdi <- hurr.natl.pdi %>% filter(storm.year %in% get_high_years(ssts.natl))
# hurr.natl.low.pdi <- hurr.natl.pdi %>% filter(storm.year %in% get_low_years(ssts.natl))
# hurr.epac.high.pdi <- hurr.epac.pdi %>% filter(storm.year %in% get_high_years(ssts.epac))
# hurr.epac.low.pdi <- hurr.epac.pdi %>% filter(storm.year %in% get_low_years(ssts.epac))
# write.table(hurr.natl.high.pdi, file = "natl-high.csv", row.names = FALSE, sep = "  ", quote = FALSE)
# write.table(hurr.epac.high.pdi, file = "epac-high.csv", row.names = FALSE, sep = "  ", quote = FALSE)
# write.table(hurr.natl.low.pdi, file = "natl-low.csv", row.names = FALSE, sep = "  ", quote = FALSE)
# write.table(hurr.epac.low.pdi, file = "epac-low.csv", row.names = FALSE, sep = "  ", quote = FALSE)

# PDI scatterplots
plot_pdi_scatter(hurr.natl.pdi, ssts.natl)
plot_pdi_scatter(hurr.epac.pdi, ssts.epac)

# Filter out tropical depressions
get_pdis_no_td <- function(hurr.obs){
	hurr.obs.pdi <- hurr.obs %>%
		group_by(storm.id, storm.name, n.obs) %>%
		summarise(storm.pdi = sum(conv_unit(wind, "knot", "m_per_sec")^3 * conv_unit(6, "hr", "sec")),
							max.wind = max(wind)) %>%
		mutate(storm.duration = n.obs * conv_unit(6, "hr", "sec")) %>%
		mutate(storm.year = substring(storm.id, 5, 9)) %>%
		filter(max.wind > 30) %>%
		filter(storm.pdi != "NA") %>%
		filter(storm.pdi != 0)
	hurr.obs.pdi <- hurr.obs.pdi[c("storm.id", "storm.name", "n.obs", "storm.duration",
																 "storm.pdi", "max.wind", "storm.year")]
	return(hurr.obs.pdi)
}

hurr.natl.pdi.no.td <- get_pdis_no_td(hurr.natl.obs)
hurr.epac.pdi.no.td <- get_pdis_no_td(hurr.epac.obs)

plot_pdi_scatter(hurr.natl.pdi.no.td, ssts.natl)
plot_pdi_scatter(hurr.epac.pdi.no.td, ssts.epac)


# Tests  ---------------------------------------------------

plot_test_scatter <- function(hurr.pdi, ssts){
	hurr.high.pdi <- hurr.pdi %>% filter(storm.year %in% get_high_years(ssts)) %>% filter(max.wind > 34)
	hurr.low.pdi <- hurr.pdi %>% filter(storm.year %in% get_low_years(ssts)) %>% filter(max.wind > 34)

	lm.high.y <- lm(log10(max.wind) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.high.pdi)
	lm.low.y <- lm(log10(max.wind) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.low.pdi)
	lm.high.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(max.wind), data = hurr.high.pdi)
	lm.low.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(max.wind), data = hurr.low.pdi)

	years.str <- paste0(year(ssts$year[1]), "-", year(ssts$year[length(ssts$year)]))

	ggplot() +
		aes(x = conv_unit(storm.duration, "sec", "hr"), y = max.wind) +
		geom_point(data = hurr.high.pdi, aes(colour = "high"), size = 0.3) +
		geom_point(data = hurr.low.pdi, aes(colour = "low"), size = 0.3) +
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
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
		# annotate("text", x = 50, y = 3*10^11, label=paste0("r^2 = ", summary(lm.high.y)$r.squared), colour="brown1", size=4) +
		# annotate("text", x = 50, y = 2*10^11, label=paste0("r^2 = ", summary(lm.low.y)$r.squared), colour="dodgerblue1", size=4) +
		labs(title = paste0("PDI Scatterplot", " (", attr(ssts, "title"), "; ", years.str ,")", "; excl. v_max < 34") ,
				 x = "Storm duration (h)", y = "v_max (knot)", colour = "SST Class")
}

plot_test_scatter(hurr.natl.pdi.no.td, ssts.natl)
plot_test_scatter(hurr.epac.pdi.no.td, ssts.epac)
