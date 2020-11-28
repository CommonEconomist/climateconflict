#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figure A1: Precipitation distribution
# Analysis on drought frequency included in text
# Last update 2018 12 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
load("data/CenTrend.RData")

# 1) Benchmark period
# 1980-1998
x<-dat[dat$year>1980 &dat$year<1999,]
mean(x$belg.d);sd(x$belg.d) # 0.4;1.0

drought<-ifelse(x$belg.d<0,1,0)
large_drought<-ifelse(x$belg.d<=-1,1,0)
sum(drought)/length(drought)   # 0.52
sum(large_drought)/length(large_drought) # 0.14

# Compare with whole distribution before 1999
x<-dat[dat$year<1999,]
mean(x$belg.d);sd(x$belg.d) # 0.3;1.0

drought<-ifelse(x$belg.d<0,1,0)
large_drought<-ifelse(x$belg.d<=-1,1,0)
sum(drought)/length(drought)   # 0.52
sum(large_drought)/length(large_drought) # 0.14

# Very similar

# 2) post-1998
x<-dat[dat$year>1998 & dat$year<2015,]

mean(x$belg.d);sd(x$belg.d) # -0.2;1.1
drought<-ifelse(x$belg.d<0,1,0)
large_drought<-ifelse(x$belg.d<=-1,1,0)
sum(drought)/length(drought)   # 0.60
sum(large_drought)/length(large_drought) # 0.25

# Increase for both drought and severe drought

# 3) Figure: plot density
par(mar=c(5,5,1,1),las=1,bty="n",cex.lab=2,cex.axis=2)

# Distribution to vectors
a0<-dat$belg.d
a<-dat$belg.d[dat$year>1980 & dat$year<1999]
b<-dat$belg.d[dat$year>1998 & dat$year<2015]

# Plot
plot(density(a),xlim=c(-4,4),ylim=c(0,.5),col='black',lwd=1.2,
     axes=FALSE,xlab="Precipitation anomaly |Z|",ylab="Density",main="")
lines(density(b),col="firebrick3",lwd=2)
lines(density(a0),lty=2,lwd=1)

axis(1,tick=FALSE,line=-1)
axis(2,tick=FALSE,line=-2)

legend(-2,.5,legend=c("1900-2014","1981-1998","1999-2014"),
       col=c("black","black","firebrick3"),
       lty=c(2,1,1),text.width=.4,bty="n",lwd=5,
       y.intersp=c(0.4),x.intersp=c(.1),xjust=1,cex=1.5)

## FIN