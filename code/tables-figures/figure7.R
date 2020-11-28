#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figure 7: Scatterplot fitted values
# Last update 2018 12 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
load("data/rainfall.RData")
load("output/ttest.RData")
load("output/cross_validation.RData")

# 1) Prepare data
fitted<-exp(colMeans(yhatM))
df<-merge(ttest_result,rain[,c("row_name","belg.g")])
df$fitted<-fitted
df$observed<-observed

# 2) Plot results

# 2.1) Settings
par(mar=c(4.5,4.5,3,1),bty="n", las=1,mfrow=c(1,2),
    cex.axis=2,cex.lab=2,cex.main=2,pty="s")

# 2.2) Panel a: Change in rainfall
plot(df$belg.g,df$fitted,log="y",axes=FALSE,cex=1.5,
     main="(a)",xlab="Change in rainfall anomaly",ylab="Fitted value")
points(df[df$observed!=0,]$belg.g,df[df$observed!=0,]$fitted,pch=19,cex=1.5)
abline(v=0,lty=2)
abline(h=1,lty=2)
axis(1,tick=FALSE,line=-1)
axis(2,tick=FALSE,line=-1,labels=c(0.01,0.1,1,10,100),at=c(0.01,0.1,1,10,100))
text(-.15,72,"Conflict observed",cex=1.5)

# 2.3) Panal b: BEST
plot(df$BEST,df$fitted,log="y",axes=FALSE,cex=1.5,xlim=c(0,1),
     main="(b)",xlab="BEST",ylab="")
points(df[df$observed!=0,]$BEST,df[df$observed!=0,]$fitted,pch=19,cex=1.5)
abline(v=.5,lty=2)
abline(h=1,lty=2)
axis(1,tick=FALSE,line=-1)

## FIN