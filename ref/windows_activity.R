# Windows of activity of the different basins
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Unneeded functions in main analysis ----------------------

# Get mean SSTs (Alt) for ranges within two years
get_mean_ssts2 <- function(x = hadsst.raster, years, first.range, second.range,
													 coords = c("180W", "180E", "90S", "90N")){
	coords <- morph_coords(coords)
	aoi <- extent(as.numeric(coords))
	nms <- names(x)
	x <- crop(x, aoi)

	months <- c("01","02","03","04","05","06","07","08","09","10","11","12")
	xMeans <- vector(length = length(years)-1, mode = 'list')
	for (ix in 2:length(years)){
		xMeans[[ix-1]] <- mean(x[[c(sapply(first.range,function(x) grep(paste0(years[ix-1],'.',months[x]),nms)),
																sapply(1:4,function(x) grep(paste0(years[ix],'.',months[x]),nms)))]], na.rm = T)
	}
	mean.brick <- do.call(brick,xMeans)
	mean.brick <- lapply(1:nlayers(mean.brick),function(ix) mean(as.matrix(mean.brick[[ix]]), na.rm = T))

	mean.df <- unlist(mean.brick)
	mean.df <- data.frame(sst = mean.df)
	mean.df <- normalise_ssts(mean.df, years)
	return(mean.df)
}

# Create SST data frames -----------------------------------

# Windows of activity
natl.years <- 1966:2007
natl.range <- 6:10
natl.coords <- c("90W", "20E", "5N", "25N")

wpac.years <- 1966:2007
wpac.range <- 5:12
wpac.coords <- c("120E", "180E", "5N", "20N")

epac.years <- 1966:2007
epac.range <- 6:10
epac.coords <- c("120W", "90W", "5N", "20N")

spac.years <- 1966:2007
spac.first.range <- 12
spac.second.range <- 1:4
spac.coords <- c("155E", "180E", "20S", "5S")

nio.years <- 1966:2007
nio.range <- as.integer(c(4,5,9,10,11))
nio.coords <- c("55E", "90E", "5N", "20N")

sio.years <- 1966:2007
sio.first.range <- 11:12
sio.second.range <- 1:4
sio.coords <- c("50E", "115E", "20S", "5S")

# Construct SST data frames
natl.ssts.df <- get_mean_ssts(years = natl.years, range = natl.range, coords = natl.coords)
attr(natl.ssts.df, "title") <- "N. Atl."

wpac.ssts.df <- get_mean_ssts(years = wpac.years, range = wpac.range, coords = wpac.coords)
attr(wpac.ssts.df, "title") <- "W. Pac."

epac.ssts.df <- get_mean_ssts(years = epac.years, range = epac.range, coords = epac.coords)
attr(epac.ssts.df, "title") <- "E. Pac."

spac.ssts.df <- get_mean_ssts2(years = spac.years, first.range = spac.first.range,
															 second.range = spac.second.range, coords = spac.coords)
attr(spac.ssts.df, "title") <- "S.W. Pac."

nio.ssts.df <- get_mean_ssts(years = nio.years, range = nio.range, coords = nio.coords)
attr(nio.ssts.df, "title") <- "N. Ind."

sio.ssts.df <- get_mean_ssts2(years = sio.years, first.range = sio.first.range,
															second.range = sio.second.range, coords = sio.coords)
attr(sio.ssts.df, "title") <- "S. Ind."
