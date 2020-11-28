#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figure A3: Conflicts over time
# Request by R3
# Data: http://ucdp.uu.se/downloads/ (used v.5)
# Last update 2018 10 24
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(plyr)
load("~/Dropbox/submitted/horn-of-africa/raw_data/ged50.Rdata")

# Subset data to events included in analysis
ged<-ged50[ged50$country=="Ethiopia" | ged50$country=="Kenya",]
ged<-ged[ged$where_prec<3 & ged$event_clar==1,]

# Aggregate
d<-ddply(data.frame(ged),.(country,year,type_of_vi),summarise,
         n=length(year))

# Plot settings
par(mar=c(5,5,2,1),las=1,bty="n",cex.axis=2,cex.lab=2,cex.main=2,mfrow=c(2,1))

# Plot: Ethiopia
plot(n~year,d[d$country=='Ethiopia' & d$type_of_vi==2,],
     ylim=c(1,100),xlim=c(1989,2015),log='y',
     type='b', pch=19,
     xlab='',ylab='Frequency',main='Ethiopia',axes=F)
lines(n~year,d[d$country=='Ethiopia' & d$type_of_vi==1,],type='b',
      pch=5,lty=3,col='grey50')
lines(n~year,d[d$country=='Ethiopia' & d$type_of_vi==3,],type='b',pch=4,lty=3,
      col='grey50')
axis(2,tick=F,line=-2,at=c(1,5,10,50,100),label=c(1,5,10,50,100))
abline(v=1999,lty=2)
legend(1990,300,c("Civil conflict","Communal conflict","One-sided violence"),
       cex=1.5,pch=c(5,19,4),pt.cex=c(2,2,2),col=c("grey50","black","grey50"),
       bty="n",y.intersp=c(0.25))

# Plot: Kenya
plot(n~year,d[d$country=='Kenya' & d$type_of_vi==2,],
     ylim=c(1,100),xlim=c(1989,2015),log='y',
     type='b',pch=19,
     xlab='',ylab='Frequency',main='Kenya',axes=F)
lines(n~year,d[d$country=='Kenya' & d$type_of_vi==1,],type='b',
      pch=5,lty=3,col='grey50')
lines(n~year,d[d$country=='Kenya' & d$type_of_vi==3,],type='b',pch=4,lty=3,
      col='grey50')

legend(1990,300,c("Civil conflict","Communal conflict","One-sided violence"),
       cex=1.5,pch=c(5,19,4),pt.cex=c(2,2,2),col=c("grey50","black","grey50"),
       bty="n",y.intersp=c(0.25))

axis(2,tick=F,line=-2,at=c(1,5,10,50,100),label=c(1,5,10,50,100))
axis(1,tick=F)
abline(v=1999,lty=2)

## FIN