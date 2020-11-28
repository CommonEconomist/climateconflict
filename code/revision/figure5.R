#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figure 5: estimated coefficients
# Precipitation estimate for number of different model specifications
# Last update 2018 05 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd("~/Dropbox/submitted/horn-of-africa/")
par(mar=c(5,3,3,10),bty='n',las=1,cex.lab=2,cex.axis=2,cex.main=2,pty='s')

# 1) Prepare data
# Posterior distributions to vectors

# Main results
load("output/fitted_models.RData")
a1<-as.vector(m1$BUGSoutput$sims.list$b3)
a2<-as.vector(m2$BUGSoutput$sims.list$b3)
a3<-as.vector(m7$BUGSoutput$sims.list$b3)
rm(list=setdiff(ls(),c('a1','a2','a3')))

# Raster aggregation
load('output/raster_results.RData')
a4<-as.vector(m2$BUGSoutput$sims.list$b3)
rm(list=setdiff(ls(),c('a1','a2','a3','a4')))

# Including night light emissions
load('output/accounting_local_wealth.RData')
a5<-as.vector(nl$BUGSoutput$sims.list$b3)
rm(list=setdiff(ls(),c('a1','a2','a3','a4','a5')))

# Including temperature
load('output/including_temperature.RData')
a6<-as.vector(m$BUGSoutput$sims.list$b3)
rm(list=setdiff(ls(),c('a1','a2','a3','a4','a5','a6')))

# Different types of conflict
load('output/conflict_types.RData')
a7<-as.vector(c0$BUGSoutput$sims.list$b3)
a8<-as.vector(c1$BUGSoutput$sims.list$b3)
a9<-as.vector(c2$BUGSoutput$sims.list$b3)
rm(list=setdiff(ls(),c('a1','a2','a3','a4','a5','a6','a7','a8','a9')))

# ACLED data
load('output/acled_estimation.RData')
a10<-as.vector(ac2$BUGSoutput$sims.list$b3)
rm(list=setdiff(ls(),c('a1','a2','a3','a4','a5','a6','a7','a8','a9','a10')))

# 2) Calculate means, errors, intervals
# Mean and standard error
m<-rev(sapply(list(a1,a2,a4,a5,a6,a3,a7,a8,a9,a10),mean))
e<-rev(sapply(list(a1,a2,a4,a5,a6,a3,a7,a8,a9,a10),sd))
lwr<-m-2*e
upr<-m+2*e

# 50% interval
q1<-rev(sapply(list(a1,a2,a4,a5,a6,a3,a7,a8,a9,a10),quantile,probs=.25))
q2<-rev(sapply(list(a1,a2,a4,a5,a6,a3,a7,a8,a9,a10),quantile,probs=.75))

n<-rev(c('Parsimonious model','Full model',
         'Grid-cells as unit-of-analysis',
         'Controlling for \n night light emissions',
         'Controlling for temperature',
         'Fatalities as outcome','All conflict types',
         'Civil conflict','Violence against civilians',
         'ACLED conflict data'))

# 3) Plot results
plot(m,1:length(n),xlim=c(-2.1,2),cex=2,xlab='',ylab='',axes=F,type='n',
     main='Parameter estimate precipitation')
abline(v=0,lwd=1.2,col='black')
abline(h=1:length(n),lwd=.75,lty=2,col='grey50')

# Estimates
segments(lwr,1:length(n),upr,1:length(n),lwd=4,lend=1,col='grey50') #+/- 2 s.e.
segments(q1,1:length(n),q2,1:length(n),lwd=5,lend=1,col='black')# 50% interval
points(m,1:length(n),pch=19,cex=2)                          # mean estimate

# Axes
axis(1,tick=F)
axis(2,tick=F,at=1:length(n),label=rev(c('i.','ii.','iii.','iv.','v.',
                                     'vi.','vii.','viii.','ix.','x.')))

axis(4,tick=F,at=1:length(n),label=n,cex.axis=1.5,line=-1.2)

## FIN