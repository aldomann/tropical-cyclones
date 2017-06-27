# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("hadisst_base.R")
source("hurdat2_pdi_base.R")
source("analysis_base.R")

# Save in PDF + Latin Modern Roman -------------------------

# library(extrafont)
# font_import(paths = "~/TTF") # Only once
# + theme(text = element_text(family = "LM Roman 10"))
# + ggsave(filename = "asd.pdf", width = 7.813, height = 4.33, dpi = 96, device = cairo_pdf)

# Create PDI data frame ------------------------------------

# Get hurricanes data frames
hurr.natl.obs <- get_hurr_obs("hurdat2-1851-2016-apr2017.txt")
hurr.epac.obs <- get_hurr_obs("hurdat2-nepac-1949-2016-apr2017.txt")
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

hadsst.raster <- load_hadsst(file = "data/HadISST_sst.nc")

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

# Get list of low & high SST years
# get_low_years(ssts.natl)
# get_high_years(ssts.natl)
# get_low_years(ssts.epac)
# get_high_years(ssts.epac)

# Get number of years per SST class
# table(ssts.natl$sst.class)
# table(ssts.epac$sst.class)

# Get number of storms per SST class
# length((hurr.natl.pdi %>% filter(storm.year %in% get_high_years(ssts.natl)))$storm.pdi)
# length((hurr.natl.pdi %>% filter(storm.year %in% get_low_years(ssts.natl)))$storm.pdi)
# length((hurr.epac.pdi %>% filter(storm.year %in% get_high_years(ssts.epac)))$storm.pdi)
# length((hurr.epac.pdi %>% filter(storm.year %in% get_low_years(ssts.epac)))$storm.pdi)

# Basins maps ----------------------------------------------

# Maps of the basins
# map_region_hurrs_small(hurr.natl.obs, years.natl, coords.natl.map, steps = c(20, 10), xtra.lims = c(3,2))
# map_region_hurrs_small(hurr.epac.obs, years.epac, coords.epac)

# Maps of the basins (full)
map_region_hurrs(hurr.natl.obs, years.natl, coords.natl.map, coords.natl, steps = c(20, 10), xtra.lims = c(3,2)) #+ theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "map-natl.pdf", width = 5.75, height = 3.75, dpi = 96, device = cairo_pdf)
map_region_hurrs(hurr.epac.obs, years.epac, coords.epac.map, coords.epac, steps = c(10, 10), xtra.lims = c(3,2)) #+ theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "map-epac.pdf", width = 6, height = 3.15, dpi = 96, device = cairo_pdf)

# SST map of a raster layer
map_global_sst(hadsst.raster, 12, 2015) #+ theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "map-sst.pdf", width = 6, height = 3, dpi = 96, device = cairo_pdf)

# Plot SSTs and PDIs ---------------------------------------

# Plot annual SSTs
# plot_annual_sst_norm(ssts.natl)
# plot_annual_sst_norm(ssts.epac)
plot_annual_sst(ssts.natl) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "sst-analysis-natl.pdf", width = 6.5, height = 3.5, dpi = 96, device = cairo_pdf)
plot_annual_sst(ssts.epac) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "sst-analysis-epac.pdf", width = 6.5, height = 3.5, dpi = 96, device = cairo_pdf)

# PDI time series
plot_pdi_tempseries(hurr.natl.pdi, ssts.natl) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "time-series-natl.pdf", width = 6.5, height = 3, dpi = 96, device = cairo_pdf)
plot_pdi_tempseries(hurr.epac.pdi, ssts.epac) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "time-series-epac.pdf", width = 6.5, height = 3, dpi = 96, device = cairo_pdf)

# DPDI plots
plot_dpdi(hurr.natl.pdi, years.natl) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "dpdi-natl.pdf", width = 6.5, height = 3, dpi = 96, device = cairo_pdf)
plot_dpdi(hurr.epac.pdi, years.epac) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "dpdi-epac.pdf", width = 6.5, height = 3, dpi = 96, device = cairo_pdf)

# DPDI plots by SST class
plot_dpdi_by_sst_class(hurr.natl.pdi, ssts.natl) #+ theme_bw() + theme(legend.position = c(0.92, 0.80)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "dpdi-by-class-natl.pdf", width = 6.5, height = 3.25, dpi = 96, device = cairo_pdf)
plot_dpdi_by_sst_class(hurr.epac.pdi, ssts.epac) #+ theme_bw() + theme(legend.position = c(0.92, 0.80)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "dpdi-by-class-epac.pdf", width = 6.5, height = 3.25, dpi = 96, device = cairo_pdf)

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

# PDI scatterplots (duration)
plot_pdi_scatter_by_status(hurr.natl.pdi, ssts.natl, "ds") #+ theme_bw() + theme(legend.position = c(0.13, 0.65)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "scatter-natl-ds.pdf", width = 6.5, height = 3.5, dpi = 96, device = cairo_pdf)
plot_pdi_scatter_by_status(hurr.natl.pdi, ssts.natl, "nds") #+ theme_bw() + theme(legend.position = c(0.13, 0.65)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "scatter-natl-nds.pdf", width = 6.5, height = 3.5, dpi = 96, device = cairo_pdf)
plot_pdi_scatter_by_status(hurr.epac.pdi, ssts.epac, "ds") #+ theme_bw() + theme(legend.position = c(0.13, 0.65)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "scatter-epac-ds.pdf", width = 6.5, height = 3.5, dpi = 96, device = cairo_pdf)
plot_pdi_scatter_by_status(hurr.epac.pdi, ssts.epac, "nds") #+ theme_bw() + theme(legend.position = c(0.13, 0.65)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "scatter-epac-nds.pdf", width = 6.5, height = 3.5, dpi = 96, device = cairo_pdf)

# PDI scatterplots (wind)
plot_pdi_scatter_wind(hurr.natl.pdi, ssts.natl) #+ theme_bw() + theme(legend.background = element_blank(), legend.position = c(0.13, 0.65)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "scatter-natl-wind.pdf", width = 6.5, height = 3.5, dpi = 96, device = cairo_pdf)
plot_pdi_scatter_wind(hurr.epac.pdi, ssts.epac) #+ theme_bw() + theme(legend.background = element_blank(), legend.position = c(0.13, 0.65)) + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "scatter-epac-wind.pdf", width = 6.5, height = 3.5, dpi = 96, device = cairo_pdf)



