library(raster)
library(xts)
library(caTools) 

# Some time definitions:
startYear <- 1930   # start of the period
endYear <- 2010     # end of the period
subp <- '2005-01-16/2007-12-16'   # period for the climatology calculation

# Open the file:
sst <- brick('/home/aldomann/Downloads/Hadley/HadISST_sst.nc')
Date <- substr(names(sst),2,11) 
Date <- gsub('\\.', '\\-', Date)
Date <- as.Date(Date)
dstart <- paste(startYear,'01','16',sep='-'); dstart <- grep(dstart, Date)
dend <- paste(endYear,'12','16',sep='-'); dend <- grep(dend, Date)
sst <- subset(sst, dstart:dend)
Date <- Date[dstart:dend]

# Extract the time serie for a specific point (lat=35, lon=120):
tserie <- as.vector(extract(sst, cbind(116, -35)))
tserie <- xts(tserie, order.by=Date)

# Calculate the climatology for the subp period:
clim <- as.numeric()
for(ii in 1:12){
	clim[ii] <- mean(tserie[subp][(.indexmon(tserie[subp])+1) == ii])
}
clim <- xts(rep(clim, length(tserie)/12), order.by=Date)

# Calculate anomalies:
tserie <- tserie - clim

# Plot the result:
par(las=1)
plot(tserie, t='n', main='HadISST')
lines(tserie, col='grey')
lines(xts(runmean(tserie, 12), order.by=Date), col='red', lwd=2)
legend('bottomleft', c('Monthly anomaly','12-month moving avg'), lty=c(1,1), lwd=c(1,2), col=c('grey','red'))