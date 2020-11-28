#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Analysis results data aggregated to cell level
# Request by R1 + R2
# Creates figure A4
# Last update 2018 05 16
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
options(scipen=1)

# Libraries
library(R2jags)
library(rjags)
library(jagstools)

# Run analysis (this will take about 6 hours)
source('code/rr/load-raster.R') 
source('code/rr/fit-raster.R')

# 1.1) Table regression results
print(m1$BUGSoutput$summary[1:5,c(1,2,3,7)],digits.summary=3) # Col. 2
print(m2$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) # Col. 3

print(m7$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) # Col. 7


# 1.2) Estimated effects
# P(negative effect rainfall)
beta<-as.vector(m1$BUGSoutput$sims.list$b3)
sum(beta<0)/length(beta) # 0.88
exp(mean(m1$BUGSoutput$sims.list$b3)) # 0.74

beta<-as.vector(m2$BUGSoutput$sims.list$b3)
sum(beta<0)/length(beta) # 0.73
exp(mean(m2$BUGSoutput$sims.list$b3)) # 0.85

beta<-as.vector(m7$BUGSoutput$sims.list$b3)
sum(beta<0)/length(beta) # 0.92
exp(mean(m7$BUGSoutput$sims.list$b3)) # 0.56

# 3.1) Marginal calibration diagram
# Function to calculate observations per bin
hfreq<-function(x){
  c(length(x[x==0]),length(x[x==1]),length(x[x==2]),length(x[x==3]),
    length(x[x==4]),length(x[x==5]),length(x[x>=6 & x<=10]),
    length(x[x>=11 & x<=25]),length(x[x>=26 & x<=50]),length(x[x>50]))
}

fitted<-exp(colMeans(yhatM))
Xobs<-hfreq(X$events.com)
Xfit<-hfreq(floor(fitted))

as<-c(.1,.5,1,5,10,20,50)

# 3.2) Figure: count of observed vs. fitted values
par(mar=c(5,5,3,.5),bty="n", las=1,mfrow=c(1,1),
    cex.axis=2,cex.lab=2,cex.main=3,pty="m")
plot(1:10,Xobs,type="b",ylim=c(0,max(Xobs,Xfit)),pch=15,
     main="",xlab="",ylab="",axes=FALSE,cex=1.5,lwd=2) 
lines(Xfit,lty=2,type="b",lwd=2,cex=1.5) 
axis(2,tick=FALSE,line=-2);mtext("Frequency",side=2,cex=2,las=0,line=1.5)
axis(1,tick=FALSE,at=1:10,label=c(0,1,2,3,4,5,"6-10","11-25","25-50","50+"),
     line=-1)
mtext("Number of events",side=1,cex=2,las=0,line=2)
text(1.7,120,"Observed",cex=2)
text(2.5,29,"Fitted",cex=2)

# 4.1) Calculate prediction error
error<-c()
for(i in 1:J){
  error[i]=exp(mean(yhat[[i]]))-Y[i]
}

# For plotting
i<-ifelse(X$events.com!=0,1,0)
r<-X$belg.g

# 4.2) Figure: prediction error
par(mar=c(5,5,3,1),bty="n",mfrow=c(1,2),
    las=1,cex.axis=2,cex.lab=2,cex.main=2,pty="s")

# Change in rainfall
plot(r,fitted,log="y",axes=FALSE,cex=1.5,col='grey50',
     main="(a)",xlab="Change in rainfall anomaly",ylab="Fitted value")
points(r[i==1],fitted[i==1],pch=19,cex=1.5)

abline(v=0,lty=2)
abline(h=1,lty=2)
axis(1,tick=FALSE,line=-1)
axis(2,tick=FALSE,line=-1,labels=c(0.01,0.1,1,10,100,1000),
     at=c(0.01,0.1,1,10,100,1000))
text(-.15,65,"Conflict observed",cex=1.5)

# Prediction error
plot(Y,abs(error),cex=1.5,axes=FALSE,xlim=c(.01,21),ylim=c(.01,1200),
     xlab="Observed conflict events",ylab="Mean error",log="y",main="(b)")
axis(2,tick=FALSE,at=c(.01,.1,1,10,100,1000),label=c(.01,.1,1,10,100,1000),line=-1)
axis(1,tick=FALSE,line=-1)
abline(v=1,lty=2)

## FIN