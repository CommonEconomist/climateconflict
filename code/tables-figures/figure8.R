#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figure 8: Forecast error
# Last update 2018 12 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
load("output/cross_validation.RData")

# 1.1) Plot settings
par(mar=c(4.5,4.5,3,1),bty="n", las=1,mfrow=c(1,2),
    cex.axis=1.5,cex.lab=1.5,cex.main=2,pty="s")

# 1.2) Panel a: mean squared error
as<-c(.1,.5,1,5,10,50)
plot(Y,abs(error),cex=1.5,axes=FALSE,xlim=c(.05,60),ylim=c(.05,140),
     xlab="Observed conflict events",ylab="Mean error",log="y",main="(a)")
axis(2,tick=FALSE,at=as,labels=as,line=-1)
axis(1,tick=FALSE,line=-1)
abline(v=1,lty=2)

# 1.3) Panel b: baseline model versus full model
as<-c(.1,1,10,100)
plot(abs(error0),abs(error),log="xy",axes=FALSE,
     xlim=c(.05,160),ylim=c(.05,160),
     xlab="Excluding precipitation",ylab="Including precipitation",main="(b)",
     cex=1.5)
points(abs(error0[observed!=0]),abs(error[observed!=0]),pch=19,cex=1.5)
axis(1,tick=FALSE,at=as,labels=as,line=-1)
axis(2,tick=FALSE,at=as,labels=as,line=-1)
abline(a=0,b=1,untf=1,lty=2)
rug(abs(error0),side=1,line=-.5)
rug(abs(error),side=2,line=-.5)

## FIN