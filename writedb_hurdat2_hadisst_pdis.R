# Code to build a PDI (from HURDAT2) data set with SST data (from HADISST)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("hadisst_base.R")
source("hurdat2_pdi_base.R")

# Create PDI data frame ------------------------------------

# Get hurricanes data frames
hurr.natl.obs <- get_hurr_obs("hurdat2-1851-2016-apr2017.txt")
hurr.epac.obs <- get_hurr_obs("hurdat2-nepac-1949-2016-apr2017.txt")

# Create data frame with PDI and year of the storm
hurr.natl.pdi <- get_pdis(hurr.natl.obs) %>%
	dplyr::filter(storm.id != "AL171988") %>%
	mutate(basin = "NATL") %>%
	dplyr::filter(storm.year %in% 1966:2016)

hurr.epac.pdi <- get_pdis(hurr.epac.obs) %>%
	dplyr::filter(storm.id != "EP231989") %>%
	mutate(basin = "EPAC") %>%
	dplyr::filter(storm.year %in% 1966:2016)

# hurr.all.pdi <- rbind(hurr.natl.pdi, hurr.epac.pdi)

# Create SST data frames -----------------------------------

hadsst.raster <- load_hadsst(file = "data/HadISST_sst.nc")

# Windows of activity
years.natl <- 1966:2016
coords.natl <- c("90W", "20W", "5N", "25N")

years.epac <- 1966:2016
coords.epac <- c("120W", "90W", "5N", "20N")

# Construct SST data frames
ssts.natl <- get_mean_ssts(years = years.natl, coords = coords.natl)
ssts.epac <- get_mean_ssts(years = years.epac, coords = coords.epac)

# Build PDI + SST data set ---------------------------------

ssts.natl <- ssts.natl %>%
	mutate(storm.year = as.character(year(year))) %>%
	dplyr::select(-year)

ssts.epac <- ssts.epac %>%
	mutate(storm.year = as.character(year(year))) %>%
	dplyr::select(-year)

# Add SST data into HURDAT2 data
hurr.sst.pdi.natl <- full_join(hurr.natl.pdi, ssts.natl)
hurr.sst.pdi.epac <- full_join(hurr.epac.pdi, ssts.epac)

# Sort by year
hurr.sst.pdi.natl <- arrange(hurr.sst.pdi.natl, storm.year)
hurr.sst.pdi.natl <- arrange(hurr.sst.pdi.natl, storm.year)

hurr.sst.all.pdi <- rbind(hurr.sst.pdi.natl, hurr.sst.pdi.epac)

# Write into CSV -------------------------------------------

if (!file.exists("data/hurdat2-hadisst-1966-2016.csv")) {
	write_csv(hurr.sst.all.pdi, "data/hurdat2-hadisst-1966-2016.csv")
}
