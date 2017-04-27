library(raster)

read.things <- function(f) {
	# f is the file path of your ascii data
	d <- readLines(f)
	d <- split(d, rep(1:12, each=181))
	d <- lapply(d, function(x) read.fwf(textConnection(x), rep(6, 360), 
																			skip=1, stringsAsFactors=FALSE,
																			na.strings=c(-1000, -32768)))
	d <- lapply(d, function(x) sapply(x, as.numeric))
	out <- stack(lapply(d, raster))
	names(out) <- month.abb
	extent(out) <- c(-180, 180, -90, 90)
	out/100
}


# download.file(
# 	'http://www.metoffice.gov.uk/hadobs/hadisst/data/HadISST1_SST_2004.txt.gz',
# 	destfile= {f <- tempfile()})

hadley.file <- paste0("/home/aldomann/Downloads/Hadley/HadISST1_SST_2004.txt.gz")

s <- read.things(hadley.file)
# s

library(rasterVis)
levelplot(s, at=seq(min(s[], na.rm=T), max(s[], na.rm=T), len=100),
					col.regions=colorRampPalette(c('#2c7bb6', '#abd9e9', '#ffffbf',
																				 '#fdae61', '#d7191c')))
