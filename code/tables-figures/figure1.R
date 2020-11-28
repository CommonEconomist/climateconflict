#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figure 1: precipitation anomalies
# Data taken from: http://datadryad.org/resource/doi:10.5061/dryad.nk780
# Last update 2018 12 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Settings
par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2,cex.axis=2,cex.main=2)
options(scipen=4)

# Libraries
library(ncdf4)
library(raster)
library(rgdal)
library(maptools)

# 1) Aggregate precipitation data: district level
load("raw-data/gaulk_adm2.RData")                # boundaries
belg<-brick("raw-data/CenTrends_v1_MarJun.nc")   # long rains

# Subset districts to Ethiopia and Kenya: transform projection
districts<-gaul_k[gaul_k$ADM0_NAME=="Ethiopia" | gaul_k$ADM0_NAME=="Kenya",]
districts<-spTransform(districts,proj4string(belg))

# Create one large polygon for national borders
districts$id<-1
border<-unionSpatialPolygons(districts,districts$id) 
plot(belg[[115]]);plot(border,add=TRUE) # check orientation

# Aggregate data (~4 sec.)
system.time(belg.a<-extract(belg,border,fun=mean,na.rm=TRUE,
                small=TRUE,weights=TRUE,normalizeWeights=TRUE,sp=TRUE))

# 2) Calculate anomalies
ma<-function(x,n=15)filter(x,rep(1/n,n),sides=1) # Moving average function

belg.df<-data.frame(belg.a)
belg_long<-t(belg.df[1,]) # Transpose
belg.s<-(belg_long-mean(belg_long))/sd(belg_long) # Calculate anomalies
belg.ma<-ma(belg.s) # 15 year running averages

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3) Figure: anomalies 15-y MA
year<-1900:2014

plot(year,belg.ma,type="h",axes=FALSE,xlab="",xlim=c(1915,2014),
     ylab="Precipitation (|Z|)",lwd=5)
axis(1,tick=FALSE,at=seq(1900,2020,10))
axis(2,tick=FALSE,line=-1)
abline(v=1998,lty=2)

## FIN