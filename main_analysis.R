# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("hadisst_base.R")
source("hurdat2_pdi_base.R")
source("analysis_base.R")

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

# Data visualisation ---------------------------------------

# Plot annual SSTs
plot_annual_sst(ssts.natl)
plot_annual_sst(ssts.epac)
# plot_annual_sst(ssts.epac, save = T, pdf = T, lmodern = T)

# DPDI plots by SST class
plot_dpdi_by_sst_class(hurr.natl.pdi, ssts.natl)
plot_dpdi_by_sst_class(hurr.epac.pdi, ssts.epac)

# Maps of the basins
# map_region_hurrs(hurr.natl.obs, years.natl, coords.natl.map, steps = c(20, 10), xtra.lims = c(3,2))
# map_region_hurrs(hurr.epac.obs, years.epac, coords.epac)

# map_region_hurrs2(hurr.natl.obs, years.natl, coords.natl.map, coords.natl, steps = c(20, 10), xtra.lims = c(3,2))
# map_region_hurrs2(hurr.epac.obs, years.epac, coords.epac.map, coords.epac, steps = c(10, 10), xtra.lims = c(3,2))


# PDI Time series ------------------------------------------

plot_pdi_tempseries <- function(hurr.pdi, ssts){
	hurr.high.pdi <- hurr.pdi %>% filter(storm.year %in% get_high_years(ssts))
	hurr.low.pdi <- hurr.pdi %>% filter(storm.year %in% get_low_years(ssts))
	years.str <- paste0(year(ssts$year[1]), "-", year(ssts$year[length(ssts$year)]))

	gg <- ggplot() +
		aes(x = as.Date(paste(storm.year, "01", "01", sep = "-")), y = storm.pdi, group = 1) +
		geom_point(data = hurr.high.pdi, aes(colour = "high"), size = 0.5)+
		geom_point(data = hurr.low.pdi, aes(colour = "low"), size = 0.5)+
		scale_colour_manual(values = c("brown1", "dodgerblue1")) +
		scale_y_log10() +
		labs(title = paste0("PDI Time series", " (", attr(ssts, "title"), "; ", years.str, ")"),
				 x = "Time (year)", y = "PDI (m^3/s^2)", colour = "SST Class" )
	# ggsave(filename = "asd.pdf",
	# 			 width = 7.813, height = 4.33, dpi = 96, device = cairo_pdf)
	gg
}

plot_pdi_tempseries(hurr.natl.pdi, ssts.natl)
plot_pdi_tempseries(hurr.epac.pdi, ssts.epac)

# Get summary of cyclone status
table((hurr.natl.obs%>% filter(storm.year %in% years.natl))$status)

# New analysis ---------------------------------------------

library(RVAideMemoire)

# PDI scatterplots
plot_pdi_scatter <- function(hurr.pdi, ssts){
	hurr.high.pdi <- hurr.pdi %>% filter(storm.year %in% get_high_years(ssts))
	hurr.low.pdi <- hurr.pdi %>% filter(storm.year %in% get_low_years(ssts))

	lm.high.y <- lm(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.high.pdi)
	lm.low.y <- lm(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.low.pdi)
	lm.high.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(storm.pdi), data = hurr.high.pdi)
	lm.low.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(storm.pdi), data = hurr.low.pdi)
	# lm.lr.high <- least.rect(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.high.pdi)
	# lm.lr.low <- least.rect(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.low.pdi)

	years.str <- paste0(year(ssts$year[1]), "-", year(ssts$year[length(ssts$year)]))

	ggplot() +
		aes(x = conv_unit(storm.duration, "sec", "hr"), y = storm.pdi) +
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
		# geom_abline(aes(slope = coef(lm.lr.high)[[2]], intercept = coef(lm.lr.high)[[1]],
										# colour = "high.lr"), linetype = "twodash") +
		# geom_abline(aes(slope = coef(lm.lr.low)[[2]], intercept = coef(lm.lr.low)[[1]],
										# colour = "low.lr"), linetype = "twodash") +
		scale_colour_manual(values = c("high" = "brown1", "low" = "dodgerblue1",
																	 "high.y~x" = "red", "low.y~x" = "blue",
																	 "high.x~y" = "darkviolet", "low.x~y" = "green")) +
																	 # "high.lr" = "orange", "low.lr" = "darkblue")) +
		guides(color=guide_legend(override.aes = list(linetype = c(0,4,4,0,4,4)))) +
		# guides(color=guide_legend(override.aes = list(linetype = c(0,4,4,4,0,4,4,4)))) +
		scale_x_log10() +
		scale_y_log10() +
		annotate("text", x = 50, y = 3*10^11, label=paste0("r^2 = ", summary(lm.high.y)$r.squared), colour="brown1", size=4) +
		annotate("text", x = 50, y = 2*10^11, label=paste0("r^2 = ", summary(lm.low.y)$r.squared), colour="dodgerblue1", size=4) +
		labs(title = paste0("PDI Scatterplot", " (", attr(ssts, "title"), "; ", years.str ,")") ,# "; excl. non-dev"),
				 x = "Storm duration (h)", y = "PDI (m^3/s^2)", colour = "SST Class")
}

plot_pdi_scatter(hurr.natl.pdi, ssts.natl)
plot_pdi_scatter(hurr.epac.pdi, ssts.epac)

# Filter out tropical depressions --------------------------

# Create data frame with PDI and year of the storm
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
	hurr.obs.pdi <- hurr.obs.pdi[c("storm.id", "storm.name", "n.obs", "storm.duration", "storm.pdi", "max.wind", "storm.year")]
	return(hurr.obs.pdi)
}

hurr.natl.pdi.no.td <- get_pdis_no_td(hurr.natl.obs)
hurr.epac.pdi.no.td <- get_pdis_no_td(hurr.epac.obs)

plot_pdi_scatter(hurr.natl.pdi.no.td, ssts.natl)
plot_pdi_scatter(hurr.epac.pdi.no.td, ssts.epac)
