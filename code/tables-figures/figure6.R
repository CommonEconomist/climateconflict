#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figure 6: Marginal calibration diagram
# Based on: "Forecasting Civil Conflict with Zero-Inflated Count Models"
# https://www.tandfonline.com/doi/abs/10.1080/13698249.2015.1059564
# Last update 2018 12 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
load("output/cross_validation.RData")

# 1) Function to calculate observations per bin
hfreq<-function(x){
  c(length(x[x==0]),length(x[x==1]),length(x[x==2]),length(x[x==3]),
    length(x[x==4]),length(x[x==5]),length(x[x>=6 & x<=10]),
    length(x[x>=11 & x<=25]),length(x[x>=26 & x<=50]),length(x[x>50]))
}

# 2) Prepare data
fitted<-exp(colMeans(yhatM))
Xobs<-hfreq(Y)
Xfit<-hfreq(floor(fitted))

as<-c(.1,.5,1,5,10,50)

# 3) Figure: count of observed vs. fitted values
par(mar=c(5,5,3,1),bty="n", las=1,cex.axis=1.5,cex.lab=2,cex.main=3,pty="m")
plot(1:10,Xobs,type="b",ylim=c(0,max(Xobs,Xfit)),pch=15,
     main="",
     xlab="",ylab="",axes=FALSE,cex=1.5,lwd=2) 
lines(Xfit,lty=2,type="b",lwd=2,cex=1.5) 
axis(2,tick=FALSE,line=-2);mtext("Frequency",side=2,cex=2,las=0,line=1.5)
axis(1,tick=FALSE,at=1:10,label=c(0,1,2,3,4,5,"6-10","11-25","25-50","50+"),
     line=-1)
mtext("Number of events",side=1,cex=2,las=0,line=2)
text(1.8,120,"Observed",cex=2)
text(2.6,29,"Fitted",cex=2)

## FIN