library(raster)
library(xts)
library(caTools)


# Data parameters ------------------------------------------

# Some time definitions:
startYear <- 1931   # start of the period
endYear <- 2007     # end of the period
subp <- '1931-01-16/2007-12-16'   # period for the climatology calculation

# Open the file:
sst <- brick('/home/aldomann/Downloads/Hadley/HadISST_sst.nc')
Date <- substr(names(sst),2,11)
Date <- gsub('\\.', '\\-', Date)
Date <- as.Date(Date)
dstart <- paste(startYear,'01','16',sep='-'); dstart <- grep(dstart, Date)
dend <- paste(endYear,'12','16',sep='-'); dend <- grep(dend, Date)
sst <- subset(sst, dstart:dend)
Date <- Date[dstart:dend]


# Extract the data -----------------------------------------

# Extract the time serie for a specific point (lat=35, lon=120):
tserie <- as.vector(extract(sst, cbind(120, -35)))
tserie <- xts(tserie, order.by=Date)


# Calculate the climatology for the subp period:
clim <- as.numeric()
for(ii in 1:12){
	clim[ii] <- mean(tserie[subp][(.indexmon(tserie[subp])+1) == ii])
}
clim <- xts(rep(clim, length(tserie)/12), order.by=Date)

# Calculate anomalies:
tserie <- tserie - clim

# Red line:
tserie.red <- xts(runmean(tserie, 12), order.by=Date)

# Convert rownames to data column:
tserie.df <- data.frame(date=index(tserie), data = coredata(tserie))
tserie.red.df <- data.frame(date=index(tserie.red), data = coredata(tserie.red))

# Data visualisation ---------------------------------------

# Plot the result:
# par(las=1)
# plot(tserie, t='n', main='HadISST')
# lines(tserie, col='grey')
# lines(xts(runmean(tserie, 12), order.by=Date), col='red', lwd=2)
# legend('bottomleft', c('Monthly anomaly','12-month moving avg'), lty=c(1,1), lwd=c(1,2), col=c('grey','red'))

# Plot the result using ggplot:
ggplot() +
	labs(title = paste0("HadISST between ", startYear, "-", endYear ),
			 x = "Time (year)", y = "SST") +
	geom_line(tserie.df, mapping = aes(x = date, y = data),
						colour = "grey") +
	geom_line(tserie.red.df, mapping = aes(x = date, y = data),
						colour = "red")
