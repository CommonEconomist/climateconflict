#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figure 2: Average rainfall anomaly
# Last update 2018 12 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Libraries
library(RColorBrewer)
library(maptools)

# Data
load('data/rainfall.RData')
load("raw-data/gaulk_adm2.RData")

# Subset districts
districts<-gaul_k[gaul_k$ADM0_NAME=="Ethiopia" | gaul_k$ADM0_NAME=="Kenya",]
districts<-merge(districts,rain)
districts<-merge(districts,rain)

# Create breaks for map
brks<-c(-1,-.75,-.5,-.25,0,.25,.5,.75,1)
col.regions=brewer.pal(9,"RdYlGn")

# Plot figure
par(mfrow=c(1,2),oma=c(2,0,0,0),mar=c(0,0,4,.5),mgp=c(0,0,0),xpd=NA,
    cex.main=2)

plot(districts,
     col=col.regions[findInterval(districts$belg.a0,brks,all.inside=TRUE)],
     axes=F,main="Rainfall anomalies (standardised) \n 1981-1998")
plot(districts,
     col=col.regions[findInterval(districts$belg.a,brks,all.inside=TRUE)],
     axes=F,main="Rainfall anomalies (standardised) \n 1999-2014")
legend(42,5.4, legend=leglabs(brks), fill=col.regions, bty="n",cex=1.3,
       y.intersp=.5)

## FIN