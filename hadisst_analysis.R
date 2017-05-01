# library(devtools)
# install_github("jebyrnes/hadsstR")
library(hadsstr)

# Read raw data --------------------------------------------

# Open file
hadsst.raster <- load_hadsst(file = "/home/aldomann/Downloads/Hadley/HadISST_sst.nc")

# Get annual and average SSTs:
startYear <- 2001
endYear <- 2007
annual.ssts <- get_annual_ssts(hadsst.raster, years = startYear:endYear)
avg.sst <- get_average_sst(hadsst.raster, years = startYear:endYear)

# Crop SST data to a certain region:
startLat <- 0; endLat <- 60;
startLon <- -40; endLon <- 40;
annual.ssts <- crop(annual_ssts, extent(startLat, endLat, startLon, endLon))

# Data visualisation ---------------------------------------

# Plot the result using ggplot:
# ggplot() +
# 	labs(title = paste0("HadISST between ", startYear, "-", endYear ),
# 			 x = "Time (year)", y = "SST") +
# 	geom_line(tserie.df, mapping = aes(x = date, y = data),
# 						colour = "grey") +
# 	geom_line(tserie.red.df, mapping = aes(x = date, y = data),
# 						colour = "red")
