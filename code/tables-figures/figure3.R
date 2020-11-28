#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figure 3: Conflict events
# Last update 2018 12 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(plyr)
library(spdep)

# 1) Prepare conflict data
load("raw-data/ged50.Rdata")

# Include only Ethiopia and Kenya
# Include events that can be geolocated at district level
# Include only true events
ged<-ged50[ged50$country=="Ethiopia" | ged50$country=="Kenya",]
ged<-ged[ged$where_prec<3 & ged$event_clar==1,]

load("raw-data/gaulk_adm2.RData") # Includes neighbours
gaul_k$nrow<-1:nrow(gaul_k)

# Assign row numbers
proj4string(ged)<-proj4string(gaul_k)
ged<-spTransform(ged,proj4string(gaul_k))
ged$nrow=over(SpatialPoints(ged),
              SpatialPolygons(gaul_k@polygons),returnlist=TRUE)
# There are 28 missing values all in Eritrea, these can be ignored.

# Exclude irrelevant conflicts
# In the data there are some conflicts that are not relevant to testing the 
# climate-conflict mechanism. 
# These include events where a government pursues a rebel group on foreign
# territory for instance. 
# Therefore the following conflicts are excluded from the data:
# Egypt:Government; Government of Uganda - Civilians; Somalia:Government
ged<-ged[!(ged$conflict_1 %in% c("Egypt:Government",
                                 "Government of Uganda - Civilians",
                                 "Somalia:Government")),]

# Aggregate data to district level.
ged$com<-as.numeric(ged$type_of_vi==2) # communal violence
ged$period<-ifelse(ged$year>1998,1,0)  # period indicator

# Aggregate data for each period
ged_a<-data.frame(ged)
ged_a<-ddply(ged_a[ged_a$year!=2015,],.(nrow,period),summarise,
             communal=max(com))


# Plot figure
load("data/lzones.RData")
par(mar=c(0,0,3,0),mfrow=c(1,2),cex.main=2)

# 1989-1998
plot(hoa_lz,main="Communal violence 1989-1998",border="white")
plot(hoa_lz[hoa_lz$lztype=="Pastoral",],
     col="lightgoldenrod1",border="lightgoldenrod1",add=TRUE)
plot(hoa_lz[hoa_lz$lztype=="Agropastoral",],
     col="orange1",border="orange1",add=TRUE)
plot(hoa_lz[hoa_lz$lztype=="Farming",],
     col="olivedrab4",border="olivedrab4",add=TRUE)
plot(hoa_lz[hoa_lz$lztype=="Urban",],
     col="black",border="black",add=TRUE)
plot(gaul_k[gaul_k$ADM0_NAME=="Ethiopia" | gaul_k$ADM0_NAME=="Kenya",],
     lty=2,add=TRUE)
points(ged[ged$type_of_vi==2 & ged$year<1999,],cex=1.5,pch=19,col="firebrick3")

# 1999-2014
plot(hoa_lz,main="Communal violence 1999-2014",border="white")
plot(hoa_lz[hoa_lz$lztype=="Pastoral",],
     col="lightgoldenrod1",border="lightgoldenrod1",add=TRUE)
plot(hoa_lz[hoa_lz$lztype=="Agropastoral",],
     col="orange1",border="orange1",add=TRUE)
plot(hoa_lz[hoa_lz$lztype=="Farming",],
     col="olivedrab4",border="olivedrab4",add=TRUE)
plot(hoa_lz[hoa_lz$lztype=="Urban",],
     col="black",border="black",add=TRUE)
plot(gaul_k[gaul_k$ADM0_NAME=="Ethiopia" | gaul_k$ADM0_NAME=="Kenya",],
     lty=2,add=TRUE)
points(ged[ged$type_of_vi==2 & ged$year>1998,],cex=1.5,pch=19,col="firebrick3")

legend(40,3.5,c("Farming","Agropastoral","Pastoral","Violence"),
       cex=1.7,pch=c(15,15,15,19),pt.cex=c(4,4,4,2),
       col=c("olivedrab4","orange1","lightgoldenrod1","firebrick3"),
       bty="n",y.intersp=c(0.25))

## FIN