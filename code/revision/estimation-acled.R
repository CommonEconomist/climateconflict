#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Robustness check with ACLED data
# Request from R1 
# Last update 2018 05 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
source('code/load.R') 

# 1) Prepare data
load('data/acled.RData')
acled<-acled[acled$period==1 & acled$row_name %in% X$row_name,] # Subset
x<-acled[,c('row_name','events.com','W.events.com','events.com.l')]

# Standardise
stan<-function(x){(x-mean(x,na.rm=TRUE))/(2*sd(x,na.rm=TRUE))}
x[,3:4]<-data.frame(lapply(x[,3:4],stan))

# Merge
x<-merge(x,X[,c('row_name','belg.g','BEST','p00.km2','pastoral')])
rm(X);X<-x

# 2) Fit models to data
# Baseline model
set.seed(42);ac0<-jags(data=list(conflict=X$events.com,
                                 conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,N=N),
                      parameters.to.save=c("a0","b1","b2","fitted"),
                      model.file=M0,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# General model
set.seed(42);ac1<-jags(data=list(conflict=X$events.com,
                                 conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","fitted"),
                      model.file=M1,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Additional controls
set.seed(42);ac2<-jags(data=list(conflict=X$events.com,conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# t-test result rather than change 
set.seed(42);ac3<-jags(data=list(conflict=X$events.com,
                                 conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$BEST,
                                pop=X$p00.km2,lz=X$pastoral,N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5",
                                           "fitted"),
                      model.file=M2,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# 3) Analyse results
print(ac0$BUGSoutput$summary[1:4,c(1,2,3,7)],digits.summary=3) # Col. 1
print(ac1$BUGSoutput$summary[1:5,c(1,2,3,7)],digits.summary=3) # Col. 2
print(ac2$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) # Col. 3
print(ac3$BUGSoutput$summary[1:7,c(1,2,3,7)],digits.summary=3) # Col. 4

save.image('output/acled_estimation.RData')

# 4) Create figure
# Posterior density to vector
acled2<-as.vector(ac2$BUGSoutput$sims.list$b3)
acled3<-as.vector(ac3$BUGSoutput$sims.list$b3)

rm(list=setdiff(ls(),c('acled2','acled3')))

# Pr
mean(acled2);sum(acled2<0)/length(acled2)
mean(acled3);sum(acled3>0)/length(acled3)

# Load original results
load('output/fitted_models.RData')
ucdp2<-as.vector(m2$BUGSoutput$sims.list$b3)
ucdp3<-as.vector(m3$BUGSoutput$sims.list$b3)

# Plot results
library(vioplot)
par(mgp=c(5,1,0),mar=c(4,6,3,1),cex.main=2,cex.axis=2,cex.lab=2,las=1,bty="n",
    mfrow=c(1,2))

plot(0:1,0:1,type="n",ylim=c(.5,2.5),xlim=c(-2.3,1.1),xlab='',ylab='',
     axes=FALSE,main='Change in rain')
vioplot(acled2,ucdp2,horizontal=TRUE,add=TRUE,col="grey60",
        drawRect=FALSE,border="white")
axis(1,tick=F)
axis(2,at=1:2,label=c('ACLED','UCDP'),tick=F,line=-2)

plot(0:1,0:1,type="n",ylim=c(.5,2.5),xlim=c(-2.8,5.7),xlab='',ylab='',
     axes=FALSE,main='BEST')
vioplot(acled3,ucdp3,horizontal=TRUE,add=TRUE,col="grey60",
        drawRect=FALSE,border="white")
axis(1,tick=F)

## FIN